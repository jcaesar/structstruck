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
use venial::Attribute;
use venial::EnumVariant;
use venial::NamedField;
use venial::Punctuated;
use venial::{parse_declaration, Declaration};

fn stream_span(input: &TokenStream) -> Option<Span> {
    let mut ret = None;
    for tok in input.clone().into_iter() {
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
    let span = stream_span(&input);
    let mut parsed = parse_declaration(input);
    match &mut parsed {
        Declaration::Struct(s) => {
            strike_through_attributes(&mut s.attributes, &mut strike_attrs);
            recurse_through_struct_fields(&mut s.fields, &strike_attrs, ret);
        }
        Declaration::Enum(e) => {
            strike_through_attributes(&mut e.attributes, &mut strike_attrs);
            let vs = e.variants.iter().cloned().map(|(mut v, p)| {
                recurse_through_struct_fields(&mut v.contents, &strike_attrs, ret);
                (v, p)
            });
            let mut new_variants: Punctuated<EnumVariant> = Default::default();
            for (v, p) in vs {
                new_variants.push(v, Some(p));
            }
            e.variants = new_variants;
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
    parsed.to_tokens(ret);
}

fn recurse_through_struct_fields(
    fields: &mut venial::StructFields,
    strike_attrs: &Vec<Attribute>,
    ret: &mut TokenStream,
) {
    match fields {
        venial::StructFields::Named(n) => {
            let fields = n.fields.iter().cloned().map(|(mut field, punct)| {
                let ttok = recurse_through_type(&field.ty.tokens[..], &strike_attrs, ret, &field);
                field.ty.tokens = ttok;
                (field, punct)
            });
            let mut new_fields: Punctuated<NamedField> = Default::default();
            for (field, punct) in fields {
                new_fields.push(field, Some(punct));
            }
            n.fields = new_fields;
            n.update_tokens();
        }
        _ => unreachable!(),
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
    field: &NamedField,
) -> Vec<TokenTree> {
    match tok {
        // Named substruct
        tt @ [.., TokenTree::Ident(kw), name @ TokenTree::Ident(_), TokenTree::Group(_)]
            if kw == "struct" || kw == "enum" =>
        {
            let name = name.clone();
            recurse_through_definition(tt.iter().cloned().collect(), strike_attrs.clone(), ret);
            vec![name]
        }
        // Unnamed substruct
        [head @ .., TokenTree::Ident(kw), body @ TokenTree::Group(_)]
            if kw == "struct" || kw == "enum" =>
        {
            let name = field.name.to_string().to_case(Case::Pascal);
            let name = Ident::new(&name, field.name.span());
            let head = head.into_iter().cloned().collect::<TokenStream>();
            recurse_through_definition(quote! {#head #kw #name #body}, strike_attrs.clone(), ret);
            vec![TokenTree::Ident(name)]
        }
        // Curse into generics
        [typ @ TokenTree::Ident(_), TokenTree::Punct(open), inner @ .., TokenTree::Punct(close)]
            if open.as_char() == '<' && close.as_char() == '>' =>
        {
            let subs = inner
                .split(|tt| match tt {
                    TokenTree::Punct(comma) if comma.as_char() == ',' => true,
                    _ => false,
                })
                .map(|sub| recurse_through_type(sub, strike_attrs, ret, field));
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
