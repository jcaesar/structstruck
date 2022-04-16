use crate::unvenial::modify_punctuated;
use crate::unvenial::UpdateTokens;
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
    let mut parsed = parse_declaration(input);
    match &mut parsed {
        Declaration::Struct(s) => {
            strike_through_attributes(&mut s.attributes, &mut strike_attrs);
            recurse_through_struct_fields(&mut s.fields, &strike_attrs, ret, &None);
        }
        Declaration::Enum(e) => {
            strike_through_attributes(&mut e.attributes, &mut strike_attrs);
            modify_punctuated(&mut e.variants, |v| {
                recurse_through_struct_fields(
                    &mut v.contents,
                    &strike_attrs,
                    ret,
                    &Some(v.name.clone()),
                );
            });
            e.update_tokens();
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
            if s._semicolon.is_none() {
                s._semicolon = Some(Punct::new(';', Spacing::Alone))
            }
        }
    }
    parsed.to_tokens(ret);
}

fn recurse_through_struct_fields(
    fields: &mut venial::StructFields,
    strike_attrs: &[Attribute],
    ret: &mut TokenStream,
    name_hint: &Option<Ident>,
) {
    match fields {
        StructFields::Named(n) => {
            modify_punctuated(&mut n.fields, |field| {
                let name_hint = field.name.to_string();
                let name_hint = match name_hint.starts_with("r#") {
                    true => &name_hint[2..],
                    false => &name_hint,
                };
                let name_hint = Ident::new(&name_hint.to_case(Case::Pascal), field.name.span());
                let ttok = mem::take(&mut field.ty.tokens);
                recurse_through_type_list(
                    &type_tree(&ttok, ret),
                    strike_attrs,
                    ret,
                    &Some(name_hint),
                    &mut field.ty.tokens,
                );
            });
            n.update_tokens();
        }
        StructFields::Unit => (),
        StructFields::Tuple(t) => {
            modify_punctuated(&mut t.fields, |field| {
                let ttok = mem::take(&mut field.ty.tokens);
                recurse_through_type_list(
                    &type_tree(&ttok, ret),
                    strike_attrs,
                    ret,
                    name_hint,
                    &mut field.ty.tokens,
                );
            });
            t.update_tokens();
        }
    }
}

fn strike_through_attributes(dec_attrs: &mut Vec<Attribute>, strike_attrs: &mut Vec<Attribute>) {
    dec_attrs.retain(|attr| match &attr.child_tokens[..] {
        [TokenTree::Ident(kw), TokenTree::Group(body)] if kw == "strikethrough" => {
            strike_attrs.push(Attribute {
                child_tokens: body.stream().into_iter().collect(),
                _braces: body.clone(),
                ..attr.clone()
            });
            false
        }
        _ => true,
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
