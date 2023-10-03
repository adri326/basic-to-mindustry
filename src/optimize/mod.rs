mod constant;
pub use constant::*;

mod jump;
pub use jump::*;

mod dead;
pub(crate) use dead::optimize_dead_code;

mod lookaround;
pub(crate) use lookaround::*;

mod replace;
pub(crate) use replace::*;

// TODO:
// - optimize jump(1)-label(2)-...instr-label(1) into ...instr-jump(2)
// - shorten temporary variable names
// - jump normalization
// - variable normalization

/// Returns true if the label was automatically generated, and can be removed by dead code elimination.
pub(crate) fn is_automatic_label(name: &str) -> bool {
    name.contains("__label") && !name.contains("__phantom")
}

/// Returns true if `name` refers to a temporary variable.
/// A temporary variable is a variable whose compuation results aren't needed outside of the program.
pub(crate) fn is_temporary_variable(name: &str) -> bool {
    name.contains("__tmp")
}
