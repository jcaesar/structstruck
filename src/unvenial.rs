use proc_macro2::{Delimiter, Group, TokenStream};
use quote::ToTokens;
use venial::{Enum, NamedStructFields};

pub(crate) trait UpdateTokens {
    fn update_tokens(&mut self);
}

impl UpdateTokens for NamedStructFields {
    fn update_tokens(&mut self) {
        self.tk_braces = punctuated_to_group(&self.fields);
    }
}

impl UpdateTokens for Enum {
    fn update_tokens(&mut self) {
        self.tk_braces = punctuated_to_group(&self.variants);
    }
}

fn punctuated_to_group<T: ToTokens>(from: &venial::Punctuated<T>) -> Group {
    let mut group = TokenStream::new();
    for (field, punct) in from.iter() {
        field.to_tokens(&mut group);
        punct.to_tokens(&mut group);
    }
    Group::new(Delimiter::Brace, group)
}
