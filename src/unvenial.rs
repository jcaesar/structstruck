use proc_macro2::{Group, TokenStream};
use quote::ToTokens;
use venial::{Enum, NamedStructFields, TupleStructFields};

pub(crate) trait UpdateTokens {
    fn update_tokens(&mut self);
}

impl UpdateTokens for NamedStructFields {
    fn update_tokens(&mut self) {
        punctuated_to_group(&self.fields, &mut self.tk_braces);
    }
}

impl UpdateTokens for TupleStructFields {
    fn update_tokens(&mut self) {
        punctuated_to_group(&self.fields, &mut self.tk_parens);
    }
}

impl UpdateTokens for Enum {
    fn update_tokens(&mut self) {
        punctuated_to_group(&self.variants, &mut self.tk_braces);
    }
}

fn punctuated_to_group<T: ToTokens>(from: &venial::Punctuated<T>, to: &mut Group) {
    let mut group = TokenStream::new();
    for (field, punct) in from.iter() {
        field.to_tokens(&mut group);
        punct.to_tokens(&mut group);
    }
    *to = Group::new(to.delimiter(), group)
}
