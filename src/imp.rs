use convert_case::Case;
use convert_case::Casing;
use proc_macro2::Delimiter;
use proc_macro2::Group;
use proc_macro2::Ident;
use proc_macro2::Punct;
use proc_macro2::Spacing;
use proc_macro2::TokenStream;
use proc_macro2::TokenTree;
use quote::quote;
use quote::ToTokens;
use venial::Attribute;
use venial::NamedField;
use venial::Punctuated;
use venial::{parse_declaration, Declaration};

pub(crate) fn recurse_through_definition(
    quot: TokenStream,
    mut strike_attrs: Vec<Attribute>,
    ret: &mut TokenStream,
) {
    let mut quot = parse_declaration(quot);
    match &mut quot {
        Declaration::Struct(s) => {
            s.attributes.retain(|attr| match &attr.child_tokens[..] {
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
            s.attributes.extend_from_slice(&strike_attrs[..]);
            match &mut s.fields {
                venial::StructFields::Named(n) => {
                    let fields = n.fields.iter().cloned().map(|(mut field, punct)| {
                        let ttok =
                            recurse_through_type(&field.ty.tokens[..], &strike_attrs, ret, &field);
                        field.ty.tokens = ttok;
                        (field, punct)
                    });
                    let mut new_fields: Punctuated<NamedField> = Default::default();
                    let mut group = TokenStream::new();
                    for (field, punct) in fields {
                        field.to_tokens(&mut group);
                        punct.to_tokens(&mut group);
                        new_fields.push(field, Some(punct));
                    }
                    n.fields = new_fields;
                    n.tk_braces = Group::new(Delimiter::Brace, group);
                }
                _ => unreachable!(),
            }
        }
        _ => unreachable!(),
    }
    quot.to_tokens(ret);
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
            if kw == "struct" =>
        {
            let name = name.clone();
            recurse_through_definition(tt.iter().cloned().collect(), strike_attrs.clone(), ret);
            vec![name]
        }
        // Unnamed substruct
        [head @ .., TokenTree::Ident(kw), body @ TokenTree::Group(_)] if kw == "struct" => {
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
