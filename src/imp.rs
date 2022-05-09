use convert_case::Case;
use convert_case::Casing;
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

fn recurse_through_struct_fields(
    fields: &mut venial::StructFields,
    strike_attrs: &Vec<Attribute>,
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
                let name_hint = Ident::new(&name_hint.to_case(Case::Pascal), field.name.span());
                let ttok =
                    recurse_through_type(&field.ty.tokens[..], strike_attrs, ret, &Some(name_hint));
                field.ty.tokens = ttok;
            }
        }
        StructFields::Unit => (),
        StructFields::Tuple(t) => {
            for (field, _) in &mut t.fields.iter_mut() {
                let ttok = recurse_through_type(&field.ty.tokens[..], strike_attrs, ret, name_hint);
                field.ty.tokens = ttok;
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

fn recurse_through_type(
    tok: &[TokenTree],
    strike_attrs: &Vec<Attribute>,
    ret: &mut TokenStream,
    name_hint: &Option<Ident>,
) -> Vec<TokenTree> {
    match tok {
        // Named substruct
        tt @ [.., TokenTree::Ident(kw), name @ TokenTree::Ident(_), TokenTree::Group(_)]
            if is_decl_kw(kw) =>
        {
            let name = name.clone();
            recurse_through_definition(tt.iter().cloned().collect(), strike_attrs.clone(), ret);
            vec![name]
        }
        // Unnamed substruct
        [head @ .., TokenTree::Ident(kw), body @ TokenTree::Group(_)] if is_decl_kw(kw) => {
            let name = match name_hint {
                Some(name) => name.clone(),
                None => {
                    report_error(
                        stream_span(tok.iter()),
                        ret,
                        "No context for naming substructure",
                    );
                    return vec![TokenTree::Punct(Punct::new('!', Spacing::Alone))];
                }
            };
            let head = head.iter().cloned().collect::<TokenStream>();
            let newthing = quote! {#head #kw #name #body};
            recurse_through_definition(newthing, strike_attrs.clone(), ret);
            vec![TokenTree::Ident(name)]
        }
        // Curse into generics
        [typ @ TokenTree::Ident(_), TokenTree::Punct(open), inner @ .., TokenTree::Punct(close)]
            if open.as_char() == '<' && close.as_char() == '>' =>
        {
            [
                &[typ.clone(), TokenTree::Punct(open.clone())][..],
                &for_each_generic_parameter(inner, ret, |group, ret| {
                    recurse_through_type(group, strike_attrs, ret, name_hint)
                }),
                &[TokenTree::Punct(close.clone())],
            ]
            .concat()
        }
        _ => tok.into(),
    }
}

// Groups in token trees aren't used for Generics<Asdf<Bsdf, Csdf>> /ö\
// Instead, that's just a flat array of Puncts and Idents.
// But I need to process that tree-like
fn for_each_generic_parameter(
    args_in: &[TokenTree],
    output: &mut TokenStream,
    mut f: impl FnMut(&[TokenTree], &mut TokenStream) -> Vec<TokenTree>,
) -> Vec<TokenTree> {
    // Yeah, this is quadratic... But I don't want to introduce another type
    let punct = |punct| TokenTree::Punct(Punct::new(punct, Spacing::Alone));
    let mut depth = 0;
    let mut group = vec![];
    let mut ret = vec![];
    let mut args = args_in.to_vec();
    if !matches!(args.last(), Some(TokenTree::Punct(comma)) if comma.as_char() == ',') {
        // The last thing always being a comma makes the next loop easier to write
        args.push(punct(','));
    }
    for tt in args {
        match tt {
            TokenTree::Punct(comma) if comma.as_char() == ',' && depth == 0 => {
                ret.extend_from_slice(&f(&group, output));
                ret.push(TokenTree::Punct(comma));
                group.clear();
            }
            tt => {
                match &tt {
                    TokenTree::Punct(opening) if opening.as_char() == '<' => {
                        depth += 1;
                    }
                    TokenTree::Punct(closing) if closing.as_char() == '>' => {
                        if depth == 0 {
                            report_error(Some(closing.span()), output, "Too many >");
                            return ret;
                        }
                        depth -= 1;
                    }
                    _ => (),
                }
                group.push(tt);
            }
        };
    }
    if !group.is_empty() {
        panic!(
            "Internal type parsing: We forced a comma as the last token, yet we did not find it..."
        );
    }
    if depth != 0 {
        report_error(stream_span(args_in.iter()), output, "Too few >");
    }
    ret
}

fn is_decl_kw(kw: &Ident) -> bool {
    kw == "struct" || kw == "enum" || kw == "union"
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
