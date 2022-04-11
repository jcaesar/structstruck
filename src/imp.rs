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
use venial::parse_declaration;
use venial::Attribute;
use venial::Declaration;
use venial::StructFields;

fn stream_span(input: impl Iterator<Item = TokenTree>) -> Option<Span> {
    let mut ret = None;
    for tok in input {
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
    let span = stream_span(input.clone().into_iter());
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
    strike_attrs: &Vec<Attribute>,
    ret: &mut TokenStream,
    name_hint: &Option<Ident>,
) {
    match fields {
        StructFields::Named(n) => {
            modify_punctuated(&mut n.fields, |field| {
                let name_hint = field.name.to_string().to_case(Case::Pascal);
                let name_hint = Ident::new(&name_hint, field.name.span());
                let ttok =
                    recurse_through_type(&field.ty.tokens[..], strike_attrs, ret, &Some(name_hint));
                field.ty.tokens = ttok;
            });
            n.update_tokens();
        }
        StructFields::Unit => (),
        StructFields::Tuple(t) => {
            modify_punctuated(&mut t.fields, |field| {
                let ttok = recurse_through_type(&field.ty.tokens[..], strike_attrs, ret, name_hint);
                field.ty.tokens = ttok;
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
                        stream_span(tok.iter().cloned()),
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
            let subs = inner
                .split(|tt| matches!(tt, TokenTree::Punct(comma) if comma.as_char() == ','))
                .map(|sub| recurse_through_type(sub, strike_attrs, ret, name_hint));
            [
                &[typ.clone(), TokenTree::Punct(open.clone())][..],
                &subs.collect::<Vec<_>>()[..]
                    .join(&[TokenTree::Punct(Punct::new(',', Spacing::Alone))][..]),
                &[TokenTree::Punct(close.clone())],
            ]
            .concat()
        }
        _ => tok.into(),
    }
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
