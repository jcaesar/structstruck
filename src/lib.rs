use proc_macro::TokenStream;

mod imp;
#[cfg(test)]
mod test;
mod unvenial;

#[proc_macro]
pub fn strike(item: TokenStream) -> TokenStream {
    let mut ret = Default::default();
    imp::recurse_through_definition(item.into(), vec![], &mut ret);
    ret.into()
}
