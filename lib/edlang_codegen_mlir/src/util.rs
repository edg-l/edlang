use melior::{ir::Location, Context};
use mlir_sys::mlirLocationCallSiteGet;

pub fn call_site<'c>(callee: Location<'c>, caller: Location<'c>) -> Location<'c> {
    unsafe { Location::from_raw(mlirLocationCallSiteGet(callee.to_raw(), caller.to_raw())) }
}
