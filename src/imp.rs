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
use std::borrow::Cow;
use std::mem;
use std::ops::Deref;
use venial::parse_declaration;
use venial::Attribute;
use venial::Declaration;
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
    ret: &mut TokenStream,
) {
    let span = stream_span(input.clone().into_iter().map(Cow::Owned));
    let input = move_out_inner_attrs(input);
    let mut parsed = parse_declaration(input);
    match &mut parsed {
        Declaration::Struct(s) => {
            strike_through_attributes(&mut s.attributes, &mut strike_attrs);
            recurse_through_struct_fields(&mut s.fields, &strike_attrs, ret, &None);
        }
        Declaration::Enum(e) => {
            strike_through_attributes(&mut e.attributes, &mut strike_attrs);
            for (v, _) in &mut e.variants.iter_mut() {
                recurse_through_struct_fields(
                    &mut v.contents,
                    &strike_attrs,
                    ret,
                    &Some(v.name.clone()),
                );
            }
        }
        _ => {
            return report_error(
                span,
                ret,
                "Unsupported declaration (only struct and enum are allowed)",
            );
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
) {
    match fields {
        StructFields::Named(n) => {
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
                    &mut field.ty.tokens,
                );
            }
        }
        StructFields::Unit => (),
        StructFields::Tuple(t) => {
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
                    &mut field.ty.tokens,
                );
            }
        }
    }
}

fn strike_through_attributes(dec_attrs: &mut Vec<Attribute>, strike_attrs: &mut Vec<Attribute>) {
    dec_attrs.retain(|attr| {
        if matches!(&attr.path[..], [TokenTree::Ident(kw)] if kw == "strikethrough") {
            strike_attrs.push(Attribute {
                tk_hashbang: attr.tk_hashbang.clone(),
                tk_braces: attr
                    .tk_group
                    .clone()
                    .unwrap_or_else(|| attr.tk_braces.clone()),
                // Hack a bit: Put all the tokens into the path.
                path: attr.value.clone().unwrap_or_default(),
                tk_equals: None,
                tk_group: None,
                value: None,
            });
            false
        } else {
            true
        }
    });
    dec_attrs.extend_from_slice(&strike_attrs[..]);
}

fn recurse_through_type_list(
    tok: &[TypeTree],
    strike_attrs: &[Attribute],
    ret: &mut TokenStream,
    name_hint: &Option<Ident>,
    type_ret: &mut Vec<TokenTree>,
) {
    let mut tok = tok;
    loop {
        let end = tok.iter().position(
            |t| matches!(t, TypeTree::Token(TokenTree::Punct(comma)) if comma.as_char() == ','),
        );
        let current = &tok[..end.unwrap_or(tok.len())];
        recurse_through_type(current, strike_attrs, ret, name_hint, type_ret);
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
    type_ret: &mut Vec<TokenTree>,
) {
    let kw = tok
        .iter()
        .any(|t| matches!(t, TypeTree::Token(TokenTree::Ident(kw)) if is_decl_kw(kw)));
    if kw {
        let mut decl = Vec::new();
        un_tree_type(tok, &mut decl);
        let pos = decl
            .iter()
            .position(|t| matches!(t, TokenTree::Ident(kw) if is_decl_kw(kw)))
            .unwrap();
        if let Some(name @ TokenTree::Ident(_)) = decl.get(pos + 1) {
            type_ret.push(name.clone());
            recurse_through_definition(decl.into_iter().collect(), strike_attrs.to_vec(), ret);
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
            recurse_through_definition(newthing, strike_attrs.to_vec(), ret);
            type_ret.push(name);
        }
    } else {
        un_type_tree(tok, type_ret, |g, type_ret| {
            recurse_through_type_list(g, strike_attrs, ret, name_hint, type_ret)
        });
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
