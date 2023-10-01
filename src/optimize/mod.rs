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
