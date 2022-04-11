use proc_macro2::{Group, TokenStream};
use quote::ToTokens;
use venial::{Enum, NamedStructFields, Punctuated, TupleStructFields};

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
    let mut group = Group::new(to.delimiter(), group);
    group.set_span(to.span());
    *to = group;
}

pub(crate) fn modify_punctuated<T: Clone>(modify: &mut Punctuated<T>, mut f: impl FnMut(&mut T)) {
    let mut new: Punctuated<T> = Default::default();
    for (v, p) in modify.iter() {
        let mut v: T = v.clone();
        f(&mut v);
        new.push(v, Some(p.clone()));
    }
    *modify = new;
}
