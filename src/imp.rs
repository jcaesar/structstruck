use heck::ToPascalCase;
use proc_macro2::Delimiter;
use proc_macro2::Group;
use proc_macro2::Ident;
use proc_macro2::Punct;
use proc_macro2::Spacing;
use proc_macro2::Span;
use proc_macro2::TokenStream;
use proc_macro2::TokenTree;
use quote::quote;
use quote::quote_spanned;
use quote::ToTokens;
use std::iter::once;
use std::mem;
use std::ops::Deref;
use venial::parse_declaration;
use venial::Attribute;
use venial::AttributeValue;
use venial::Declaration;
use venial::GenericParam;
use venial::GenericParamList;
use venial::StructFields;

fn stream_span(input: impl Iterator<Item = impl Deref<Target = TokenTree>>) -> Option<Span> {
    let mut ret = None;
    for tok in input {
        let tok = tok.deref();
        match ret {
            None => ret = Some(tok.span()),
            Some(span) => match span.join(tok.span()) {
                Some(span) => ret = Some(span),
                None => return ret,
            },
        }
    }
    ret
}

pub(crate) fn recurse_through_definition(
    input: TokenStream,
    mut strike_attrs: Vec<Attribute>,
    make_pub: bool,
    ret: &mut TokenStream,
) -> Option<GenericParamList> {
    let input_vec = input.into_iter().collect::<Vec<TokenTree>>();
    let span = stream_span(input_vec.iter());
    let input = hack_append_type_decl_semicolon(input_vec);
    let input = move_out_inner_attrs(input);
    let mut parsed = match parse_declaration(input) {
        Ok(parsed) => parsed,
        Err(e) => {
            // Sadly, venial still panics on invalid syntax
            report_error(span, ret, &format!("{}", e));
            return None;
        }
    };
    match &mut parsed {
        Declaration::Struct(s) => {
            strike_through_attributes(&mut s.attributes, &mut strike_attrs, ret);
            recurse_through_struct_fields(&mut s.fields, &strike_attrs, ret, &None, false);
            if make_pub {
                s.vis_marker.get_or_insert_with(make_pub_marker);
            }
        }
        Declaration::Enum(e) => {
            strike_through_attributes(&mut e.attributes, &mut strike_attrs, ret);
            for (v, _) in &mut e.variants.iter_mut() {
                recurse_through_struct_fields(
                    &mut v.contents,
                    &strike_attrs,
                    ret,
                    &Some(v.name.clone()),
                    is_plain_pub(&e.vis_marker),
                );
            }
            if make_pub {
                e.vis_marker.get_or_insert_with(make_pub_marker);
            }
        }
        Declaration::Union(u) => {
            strike_through_attributes(&mut u.attributes, &mut strike_attrs, ret);
            named_struct_fields(&mut u.fields, &strike_attrs, ret, false);
            if make_pub {
                u.vis_marker.get_or_insert_with(make_pub_marker);
            }
        }
        Declaration::TyDefinition(t) => {
            strike_through_attributes(&mut t.attributes, &mut strike_attrs, ret);
            let ttok = mem::take(&mut t.initializer_ty.tokens);
            recurse_through_type_list(
                &type_tree(&ttok, ret),
                &strike_attrs,
                ret,
                &None,
                false,
                &mut t.initializer_ty.tokens,
            );
            if make_pub {
                t.vis_marker.get_or_insert_with(make_pub_marker);
            }
        }
        _ => {
            report_error(
                span,
                ret,
                "Unsupported declaration (only struct, enum, and union are allowed)",
            );
            return None;
        }
    }
    if let Declaration::Struct(s) = &mut parsed {
        if let StructFields::Tuple(_) = s.fields {
            if s.tk_semicolon.is_none() {
                s.tk_semicolon = Some(Punct::new(';', Spacing::Alone))
            }
        }
    }
    parsed.to_tokens(ret);
    parsed.generic_params().cloned()
}

fn hack_append_type_decl_semicolon(input_vec: Vec<TokenTree>) -> TokenStream {
    let is_type_decl = input_vec
        .iter()
        .any(|t| matches!(t, TokenTree::Ident(kw) if kw == "type"))
        && input_vec.iter().all(|t| {
            matches!(t, TokenTree::Ident(kw) if kw == "type")
                || !matches!(t, TokenTree::Ident(kw) if is_decl_kw(kw))
        });
    let input = match is_type_decl {
        true => input_vec
            .into_iter()
            .chain(once(TokenTree::Punct(Punct::new(';', Spacing::Alone))))
            .collect(),
        false => input_vec.into_iter().collect(),
    };
    input
}

pub(crate) fn make_pub_marker() -> venial::VisMarker {
    venial::VisMarker {
        tk_token1: TokenTree::Ident(Ident::new("pub", Span::mixed_site())),
        tk_token2: None,
    }
}

pub(crate) fn is_plain_pub(vis_marker: &Option<venial::VisMarker>) -> bool {
    match vis_marker {
        Some(venial::VisMarker {
            tk_token1: TokenTree::Ident(i),
            tk_token2: None,
        }) if i.to_string() == "pub" => true,
        _ => false,
    }
}

fn move_out_inner_attrs(input: TokenStream) -> TokenStream {
    let mut prefix = vec![];
    let mut ret = vec![];
    for e in input {
        match e {
            TokenTree::Group(g) if g.delimiter() == Delimiter::Brace => {
                let mut tt: Vec<TokenTree> = vec![];
                let gt = g.stream().into_iter().collect::<Vec<_>>();
                let mut gt = &gt[..];
                loop {
                    match gt {
                        [TokenTree::Punct(hash), TokenTree::Punct(bang), TokenTree::Group(tree), rest @ ..]
                            if hash.as_char() == '#' && bang.as_char() == '!' =>
                        {
                            gt = rest;
                            prefix.extend_from_slice(&[
                                TokenTree::Punct(hash.to_owned()),
                                TokenTree::Group(tree.to_owned()),
                            ]);
                        }
                        [rest @ ..] => {
                            for t in rest {
                                tt.push(t.to_owned());
                            }
                            break;
                        }
                    }
                }
                let mut gr = Group::new(g.delimiter(), tt.into_iter().collect());
                gr.set_span(g.span());
                ret.push(TokenTree::Group(gr));
            }
            e => ret.push(e),
        }
    }
    prefix.into_iter().chain(ret.into_iter()).collect()
}

fn recurse_through_struct_fields(
    fields: &mut venial::StructFields,
    strike_attrs: &[Attribute],
    ret: &mut TokenStream,
    name_hint: &Option<Ident>,
    in_pub_enum: bool,
) {
    match fields {
        StructFields::Unit => (),
        StructFields::Named(n) => named_struct_fields(n, strike_attrs, ret, in_pub_enum),
        StructFields::Tuple(t) => tuple_struct_fields(t, strike_attrs, ret, name_hint, in_pub_enum),
    }
}

fn named_struct_fields(
    n: &mut venial::NamedStructFields,
    strike_attrs: &[Attribute],
    ret: &mut TokenStream,
    in_pub_enum: bool,
) {
    for (field, _) in &mut n.fields.iter_mut() {
        let name_hint = field.name.to_string();
        let name_hint = match name_hint.starts_with("r#") {
            true => &name_hint[2..],
            false => &name_hint,
        };
        let name_hint = Ident::new(&name_hint.to_pascal_case(), field.name.span());
        let ttok = mem::take(&mut field.ty.tokens);
        recurse_through_type_list(
            &type_tree(&ttok, ret),
            strike_attrs,
            ret,
            &Some(name_hint),
            is_plain_pub(&field.vis_marker) || in_pub_enum,
            &mut field.ty.tokens,
        );
    }
}

fn tuple_struct_fields(
    t: &mut venial::TupleStructFields,
    strike_attrs: &[Attribute],
    ret: &mut TokenStream,
    name_hint: &Option<Ident>,
    in_pub_enum: bool,
) {
    for (field, _) in &mut t.fields.iter_mut() {
        let ttok = mem::take(&mut field.ty.tokens);
        let ttok = type_tree(&ttok, ret);

        // Slight hack for tuple structs:
        // struct Foo(pub struct Bar()); is ambigous:
        // Which does the pub belong to, Bar or Foo::0?
        // I'd say Bar, but venial parses the pub as the visibility specifier of the current struct field
        // So, transfer the visibility specifier to the declaration token stream, but only if there isn't already one:
        // I also don't want to break struct Foo(pub pub struct Bar()); (both Bar and Foo::0 public)
        let vtok;
        let ttok = match ttok
            .iter()
            .any(|t| matches!(t, TypeTree::Token(TokenTree::Ident(kw)) if kw == "pub"))
        {
            true => ttok,
            false => match mem::take(&mut field.vis_marker) {
                Some(vis) => {
                    vtok = vis.into_token_stream().into_iter().collect::<Vec<_>>();
                    vtok.iter()
                        .map(TypeTree::Token)
                        .chain(ttok.into_iter())
                        .collect()
                }
                None => ttok,
            },
        };

        recurse_through_type_list(
            &ttok,
            strike_attrs,
            ret,
            name_hint,
            is_plain_pub(&field.vis_marker) || in_pub_enum,
            &mut field.ty.tokens,
        );
    }
}

fn strike_through_attributes(
    dec_attrs: &mut Vec<Attribute>,
    strike_attrs: &mut Vec<Attribute>,
    ret: &mut TokenStream,
) {
    let mut nostrike = false;

    dec_attrs.retain(|attr| {
        if matches!(&attr.path[..], [TokenTree::Ident(kw)] if kw == "nostrike") {
            match &attr.value {
                AttributeValue::Empty => {
                    nostrike = true;
                },
                _ => {
                    report_error(
                        stream_span(attr.get_value_tokens().iter()),
                        ret,
                        "#[nostrike] does not take parameters",
                    );
                }
            }
            false
        } else if matches!(&attr.path[..], [TokenTree::Ident(kw)] if kw == "strikethrough") {
            match &attr.value {
                AttributeValue::Group(brackets, value) => {
                    strike_attrs.push(Attribute {
                        tk_bang: attr.tk_bang.clone(),
                        tk_hash: attr.tk_hash.clone(),
                        tk_brackets: brackets.clone(),
                        // Hack a bit: Put all the tokens into the path.
                        path: value.to_vec(),
                        value: AttributeValue::Empty,
                    });
                }
                _ => {
                    report_error(
                        stream_span(attr.get_value_tokens().iter()),
                        ret,
                        "#[strikethrough …]: … must be a [group]",
                    );
                }
            };
            false
        } else {
            true
        }
    });

    if !nostrike {
        dec_attrs.splice(0..0, strike_attrs.iter().cloned());
    }
}

fn get_tt_punct<'t>(t: &'t TypeTree<'t>, c: char) -> Option<&'t Punct> {
    match t {
        TypeTree::Token(TokenTree::Punct(p)) if p.as_char() == c => Some(p),
        _ => None,
    }
}

fn recurse_through_type_list(
    tok: &[TypeTree],
    strike_attrs: &[Attribute],
    ret: &mut TokenStream,
    name_hint: &Option<Ident>,
    pub_hint: bool,
    type_ret: &mut Vec<TokenTree>,
) {
    let mut tok = tok;
    loop {
        let end = tok.iter().position(|t| get_tt_punct(t, ',').is_some());
        let current = &tok[..end.unwrap_or(tok.len())];
        recurse_through_type(current, strike_attrs, ret, name_hint, pub_hint, type_ret);
        if let Some(comma) = end {
            type_ret.push(match tok[comma] {
                TypeTree::Token(comma) => comma.clone(),
                _ => unreachable!(),
            });
            tok = &tok[comma + 1..];
        } else {
            return;
        }
    }
}
fn recurse_through_type(
    tok: &[TypeTree],
    strike_attrs: &[Attribute],
    ret: &mut TokenStream,
    name_hint: &Option<Ident>,
    pub_hint: bool,
    type_ret: &mut Vec<TokenTree>,
) {
    if let Some(c) = tok.windows(3).find_map(|t| {
        get_tt_punct(&t[0], ':')
            .or(get_tt_punct(&t[2], ':'))
            .is_none()
            .then(|| get_tt_punct(&t[1], ':'))
            .flatten()
    }) {
        report_error(
            Some(c.span()),
            ret,
            "Colon in top level of type expression. Did you forget a comma somewhere?",
        );
    }
    let kw = tok.iter().position(|t| get_decl_ident(t).is_some());
    if let Some(kw) = kw {
        if let Some(dup) = tok[kw + 1..].iter().find_map(get_decl_ident) {
            report_error(
                Some(dup.span()),
                ret,
                "More than one struct/enum/.. declaration found",
            );
        }
        let mut decl = Vec::new();
        un_tree_type(tok, &mut decl);
        let pos = decl
            .iter()
            .position(|t| matches!(t, TokenTree::Ident(kw) if is_decl_kw(kw)))
            .unwrap();
        let generics = if let Some(name @ TokenTree::Ident(_)) = decl.get(pos + 1) {
            type_ret.push(name.clone());
            recurse_through_definition(
                decl.into_iter().collect(),
                strike_attrs.to_vec(),
                pub_hint,
                ret,
            )
        } else {
            let name = match name_hint {
                Some(name) => TokenTree::Ident(name.clone()),
                None => {
                    report_error(
                        stream_span(decl.iter()),
                        ret,
                        "No context for naming substructure",
                    );
                    TokenTree::Punct(Punct::new('!', Spacing::Alone))
                }
            };
            let tail = decl.drain((pos + 1)..).collect::<TokenStream>();
            let head = decl.into_iter().collect::<TokenStream>();
            let newthing = quote! {#head #name #tail};
            let generics =
                recurse_through_definition(newthing, strike_attrs.to_vec(), pub_hint, ret);

            type_ret.push(name);
            generics
        };
        if let Some(generics) = generics {
            type_ret.push(generics.tk_l_bracket.into());
            let mut gp = generics.params.clone();
            gp.iter_mut().for_each(|(gp, _)| {
                *gp = GenericParam {
                    name: gp.name.clone(),
                    tk_prefix: gp
                        .tk_prefix
                        .clone()
                        .filter(|pfx| matches!(pfx, TokenTree::Punct(_))),
                    bound: None,
                }
            });
            type_ret.extend(gp.into_token_stream());
            type_ret.push(generics.tk_r_bracket.into());
        }
    } else {
        un_type_tree(tok, type_ret, |g, type_ret| {
            recurse_through_type_list(g, strike_attrs, ret, name_hint, false, type_ret)
        });
    }
}

fn get_decl_ident<'a>(t: &'a TypeTree) -> Option<&'a Ident> {
    match t {
        TypeTree::Token(TokenTree::Ident(ref kw)) if is_decl_kw(kw) => Some(kw),
        _ => None,
    }
}

fn un_tree_type(tok: &[TypeTree], type_ret: &mut Vec<TokenTree>) {
    un_type_tree(tok, type_ret, un_tree_type)
}

fn un_type_tree(
    tok: &[TypeTree],
    type_ret: &mut Vec<TokenTree>,
    mut f: impl FnMut(&[TypeTree], &mut Vec<TokenTree>),
) {
    for tt in tok.iter() {
        match tt {
            TypeTree::Group(o, g, c) => {
                type_ret.push(TokenTree::Punct((*o).clone()));
                f(g, type_ret);
                if let Some(c) = c {
                    type_ret.push(TokenTree::Punct((*c).clone()));
                }
            }
            TypeTree::Token(t) => type_ret.push((*t).clone()),
        }
    }
}

#[cfg_attr(test, derive(Debug))]
pub(crate) enum TypeTree<'a> {
    Group(&'a Punct, Vec<TypeTree<'a>>, Option<&'a Punct>),
    Token(&'a TokenTree),
}

pub(crate) fn type_tree<'a>(args: &'a [TokenTree], ret: &'_ mut TokenStream) -> Vec<TypeTree<'a>> {
    let mut stac = vec![];
    let mut current = vec![];
    for tt in args {
        match tt {
            TokenTree::Punct(open) if open.as_char() == '<' => {
                stac.push((open, mem::take(&mut current)));
            }
            TokenTree::Punct(close) if close.as_char() == '>' => {
                if let Some((open, parent)) = stac.pop() {
                    let child = mem::replace(&mut current, parent);
                    current.push(TypeTree::Group(open, child, Some(close)));
                } else {
                    report_error(Some(close.span()), ret, "Unexpected >");
                    current.push(TypeTree::Token(tt));
                }
            }
            tt => current.push(TypeTree::Token(tt)),
        }
    }
    while let Some((open, parent)) = stac.pop() {
        report_error(Some(open.span()), ret, "Unclosed group");
        let child = mem::replace(&mut current, parent);
        current.push(TypeTree::Group(open, child, None));
    }
    current
}

fn is_decl_kw(kw: &Ident) -> bool {
    kw == "struct"
        || kw == "enum"
        || kw == "union"
        || kw == "type"
        || kw == "fn"
        || kw == "mod"
        || kw == "trait"
}

fn report_error(span: Option<Span>, ret: &mut TokenStream, error: &str) {
    let error = format!(
        "{} error: {} - starting from:",
        env!("CARGO_PKG_NAME"),
        error
    );
    match span {
        Some(span) => {
            quote_spanned! {
                span => compile_error!(#error);
            }
            .to_tokens(ret);
        }
        None => panic!("{}", error),
    }
}

pub fn flatten_empty_groups(ts: proc_macro2::TokenStream) -> proc_macro2::TokenStream {
    ts.into_iter()
        .flat_map(|tt| match tt {
            proc_macro2::TokenTree::Group(g) if g.delimiter() == proc_macro2::Delimiter::None => {
                flatten_empty_groups(g.stream())
            }
            proc_macro2::TokenTree::Group(group) => {
                let inner = flatten_empty_groups(group.stream());
                let mut ngroup = proc_macro2::Group::new(group.delimiter(), inner);
                ngroup.set_span(group.span());
                once(proc_macro2::TokenTree::Group(ngroup)).collect()
            }
            x => once(x).collect(),
        })
        .collect()
}
