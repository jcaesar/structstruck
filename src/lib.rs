mod imp;
#[cfg(test)]
mod test;

#[proc_macro]
pub fn strike(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let mut ret = Default::default();
    imp::recurse_through_definition(item.into(), vec![], &mut ret);
    ret.into()
}
