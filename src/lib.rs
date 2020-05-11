//! Utilities macro/function to ease function chain to be less verbose.
//! 
//! This crate provide two usage style, a macro `chain` and a function `chain`.
//! 
//! # Sample showcase
//! ## Case 1 chain by using a helper macro
//! ```rust
//! use fn_chain::chain;
//! 
//! fn simple_add(a : i32, b : i32, c : i32) -> i32 {
//!     a + b + c
//! }
//! 
//! fn pass_through(v : f64) -> f64 {
//!     v
//! }
//! 
//! assert_eq!(
//!     6f64, 
//!     chain!(
//!         simple_add(1, 2, 3), 
//!         |result: i32| {(result as f64).powi(2)}, 
//!         |sqr: f64| {sqr.powf(0.5)},
//!         pass_through,
//!         pass_through
//!     )
//! );
//! // This macro will expand to:
//! assert_eq!(
//!     6f64, 
//!     pass_through(
//!         pass_through(
//!             (|sqr: f64| {sqr.powf(0.5)})(
//!                 (|result: i32| {(result as f64).powi(2)})(
//!                     simple_add(1, 2, 3)
//!                 )
//!             )
//!         )
//!     )
//! );
//! ```
//! ## Case 2 chain by using helper function
//! ```rust
//! use fn_chain::chain;
//! 
//! fn simple_add(a : i32, b : i32, c : i32) -> i32 {
//!     a + b + c
//! }
//! 
//! fn pass_through(v : f64) -> f64 {
//!     v
//! }
//! 
//! assert_eq!(6f64, *chain(simple_add(1, 2, 3))
//!                         .chain(|result| {(result as f64).powi(2)})
//!                         .chain(|sqr| sqr.powf(0.5))
//!                         .chain(pass_through)
//!                         .chain(pass_through));
//! ```

/// A macro counterpart of function [chain](fn.chain.html) which reduce verbosity
/// and defer the execution until entire chain is formed.
/// 
/// This macro will expanded to a nested function call, not a (Chainable)[struct.Chainable.html] struct.
/// 
/// For example: 
/// ```rust
/// use fn_chain::chain;
/// let result = chain!((|a, b, c| a + b + c)(1i32, 2, 3), |d| d * 2);
/// assert_eq!(12, result);
/// // The macro will expand into something like this
/// assert_eq!(
///     12,
///     (|d| d * 2)((|a, b, c| a + b + c)(1i32, 2, 3))
/// );
/// ```
/// Another example using function:
/// ```rust
/// use fn_chain::chain;
/// fn function(a : i32, b : i32, c : i32) -> i32{
///     a + b + c
/// }
/// fn subsequence_function(d: i32) -> i32 {
///     d * 2
/// }
/// let outer = 2;
/// let result = chain!(
///                 function(1, 2, 3), 
///                 |e| e + 1, 
///                 move |f| f * outer, 
///                 subsequence_function, 
///                 subsequence_function);
/// assert_eq!(56, result);
/// // This macro will be expanded to:
/// assert_eq!(56, 
///     subsequence_function(
///         subsequence_function(
///             (move |f| f * outer)(
///                 (|e| e + 1)(
///                     function(1, 2, 3)
///                 )
///             )
///         )
///     )
/// )
/// ```
#[macro_export]
macro_rules! chain {
    // call chain with single function
    ($function: ident($($params: expr),*)) => {
        $function($($params),*)
    };
    // call chain with single closure block stmt with immediate execution
    (|$($c_params: ident),*| $body: block($($expr: expr),*)) => {
        (|$($c_params),*| $body)($($expr),*)
    };
    // call chain with single any kind of expression closure wrap inside parenthesis with immediate execution
    ((|$($c_params: ident),*| $body: expr)($($expr: expr),*)) => {
        (|$($c_params),*| $body)($($expr),*)
    };
    // call chain with single moved closure block stmt with immediate execution
    (move |$($c_params: ident),*| $body: block($($expr: expr),*)) => {
        (move |$($c_params),*| $body)($($expr),*)
    };
    // call chain with single any kind of expression moved closure wrap inside parenthesis with immediate execution
    ((move |$($c_params: ident),*| $body: expr)($($expr: expr),*)) => {
        (move |$($c_params),*| $body)($($expr),*)
    };
    // call chain with single borrow closure block stmt with immediate execution
    (&|$($c_params: ident),*| $body: block($(*$expr: expr),*)) => {
        (&|$($c_params),*| $body)($($expr),*)
    };
    // call chain with single any kind of expression borrow closure wrap inside parenthesis with immediate execution
    ((&|$($c_params: ident),*| $body: expr)($($expr: expr),*)) => {
        (&|$($c_params),*| $body)($($expr),*)
    };
    // call chain with single borrow muit closure block stmt with immediate execution
    (& mut |$($c_params: ident),*| $body: block($($expr: expr),*)) => {
        (& mut |$($c_params),*| $body)($($expr),*)
    };
    // call chain with single any kind of expression borrow mut closure wrap inside parenthesis with immediate execution
    ((& mut |$($c_params: ident),*| $body: expr)($($expr: expr),*)) => {
        (|$($c_params),*| $body)($($expr),*)
    };
    // chain of two functions
    ($function: ident($($params: expr),*), $function2: ident) => {
        $function2($function($($params),*))
    };
    // chain of a function before closure
    ($function: ident($($params: expr),*), |$c_params: ident| $body: expr) => {
        (|$c_params| $body)($function($($params),*))
    };
    // chain of a function before closure specifying type of params in a closure
    ($function: ident($($params: expr),*), |$c_params: ident: $ty: ident| $body: expr) => {
        (|$c_params: $ty| $body)($function($($params),*))
    };
    // chain of a function before closure specifying borrow type of params in a closure
    ($function: ident($($params: expr),*), |$c_params: ident: & $ty: ident| $body: expr) => {
        (|$c_params: $ty| $body)($function($($params),*))
    };
    // chain of a function before closure specifying borrow mut type of params in a closure
    ($function: ident($($params: expr),*), |$c_params: ident: & mut $ty: ident| $body: tt) => {
        (|$c_params: $ty| $body)($function($($params),*))
    };
    // chain of a function before moved closure
    ($function: ident($($params: expr),*), move |$c_params: ident| $body: tt) => {
        (move |$c_params| $body)($function($($params),*))
    };
    // chain of a function before moved closure specifying type of params in a closure
    ($function: ident($($params: expr),*), move |$c_params: ident: $ty: ident| $body: tt) => {
        (move |$c_params: $ty| $body)($function($($params),*))
    };
    // chain of a function before moved closure specifying borrow type of params in a closure
    ($function: ident($($params: expr),*), move |$c_params: ident: & $ty: ident| $body: tt) => {
        (move |$c_params: $ty| $body)($function($($params),*))
    };
    // chain of a function before moved closure specifying borrow mut type of params in a closure
    ($function: ident($($params: expr),*), move |$c_params: ident: & mut $ty: ident| $body: tt) => {
        (move |$c_params: $ty| $body)($function($($params),*))
    };
    // chain of multiple functions
    ($function: ident($($params: expr),*), $function2: ident, $($more_func: ident),+) => {
        chain!($function2($function($($params),*)), $($more_func),*)
    };
    // call chain of closure execution follow by function
    ((|$($c_params: ident),*| $body: expr)($($expr: expr),*), $func: ident) => {
        $func((|$($c_params),*| $body)($($expr),*))
    };
    // call chain of moved closure execution follow by function
    ((move |$($c_params: ident),*| $body: expr)($($expr: expr),*), $func: ident) => {
        $func((move |$($c_params),*| $body)($($expr),*))
    };
    // call chain of closure execution follow by another closure
    ((|$($c_params: ident),*| $body: expr)($($expr: expr),*), |$($c2_params: ident),*| $body2: expr) => {
        (|$($c2_params),*| $body2)((|$($c_params),*| $body)($($expr),*))
    };
    // call chain of moved closure execution follow by another closure
    ((move |$($c_params: ident),*| $body: expr)($($expr: expr),*), |$c2_params: ident| $body2: expr) => {
        (|$c2_params| $body2)((move |$($c_params),*| $body)($($expr),*))
    };
    // call chain of closure execution with typed parameter follow by function
    ((|$($c_params: ident: $ty: ty),*| $body: expr)($($expr: expr),*), $func: ident) => {
        $func((|$($c_params: $ty),*| $body)($($expr),*))
    };
    // call chain of moved closure execution with typed parameter follow by function
    ((move |$($c_params: ident: $ty: ty),*| $body: expr)($expr: expr), $func: ident) => {
        $func((move |$($c_params: $ty),*| $body)($($expr),*))
    };
    // call chain of closure execution follow by closure
    ((|$($c_params: ident),*| $body: expr)($($expr: expr),*), |$c2_params: ident| $body2: expr) => {
        (|$c2_params| $body2)((|$($c_params),*| $body)($($expr),*))
    };
    // call chain of closure execution follow by moved closure
    ((|$($c_params: ident),*| $body: expr)($($expr: expr),*), move |$c2_params: ident| $body2: expr) => {
        (move |$c2_params| $body2)((|$($c_params),*| $body)($($expr),*))
    };
    // call chain of moved closure execution follow by another moved closure
    ((move |$($c_params: ident),*| $body: expr)($($expr: expr),*), move |$c2_params: ident| $body2: expr) => {
        (move |$c2_params| $body2)((move |$($c_params),*| $body)($($expr),*))
    };
    // call chain of closure execution with typed parameter follow by another closure
    ((|$($c_params: ident: $ty: ty),*| $body: expr)($($expr: expr),*), |$c2_params: ident| $body2: expr) => {
        (|$c2_params| $body2)((|$($c_params: $ty),*| $body)($($expr),*))
    };
    // call chain of moved closure execution with typed parameter follow by another closure
    ((move |$($c_params: ident: $ty: ty),*| $body: expr)($($expr: expr),*), |$c2_params: ident| $body2: expr) => {
        (|$c2_params| $body2)((move |$($c_params: $ty),*| $body)($($expr),*))
    };
    // call chain of closure execution with typed parameter follow by moved closure
    ((|$($c_params: ident: $ty: ty),*| $body: expr)($($expr: expr),*), move |$c2_params: ident| $body2: expr) => {
        (move |$c2_params| $body2)((|$($c_params: $ty),*| $body)($($expr),*))
    };
    // call chain of moved closure execution with typed parameter follow by another moved closure
    ((move |$($c_params: ident: $ty: ty),*| $body: expr)($($expr: expr),*), move |$c2_params: ident| $body2: expr) => {
        (move |$c2_params| $body2)((move |$($c_params: $ty),*| $body)($($expr),*))
    };
    // call chain of closure execution follow by another closure
    ((|$($c_params: ident),*| $body: expr)($($expr: expr),*), |$c2_params: ident: $ty: ty| $body2: expr) => {
        (|$c2_params: $ty| $body2)((|$($c_params),*| $body)($($expr),*))
    };
    // call chain of moved closure execution follow by another closure
    ((move |$($c_params: ident),*| $body: expr)($($expr: expr),*), |$c2_params: ident: $ty: ty| $body2: expr) => {
        (|$c2_params: $ty| $body2)((move |$($c_params),*| $body)($($expr),*))
    };
    // call chain of closure execution follow by moved closure
    ((|$($c_params: ident),*| $body: expr)($($expr: expr),*), move |$c2_params: ident: $ty: ty| $body2: expr) => {
        (move |$c2_params: $ty| $body2)((|$($c_params),*| $body)($($expr),*))
    };
    // call chain of moved closure execution follow by another moved closure
    ((move |$($c_params: ident),*| $body: expr)($($expr: expr),*), move |$c2_params: ident: $ty: ty| $body2: expr) => {
        (move |$c2_params: $ty| $body2)((move |$($c_params),*| $body)($($expr),*))
    };
    // call chain of closure with typed parameter follow by another closure with typed parameter
    ((|$($c_params: ident: $ty: ty),*| $body: expr)($($expr: expr),*), |$c2_params: ident: $ty2: ty| $body2: expr) => {
        (|$c2_params: $ty2| $body2)((|$($c_params: $ty),*| $body)($($expr),*))
    };
    // call chain of moved closure with typed parameter follow by another closure with typed parameter
    ((move |$($c_params: ident: $ty: ty),*| $body: expr)($($expr: expr),*), |$c2_params: ident: $ty2: ty| $body2: expr) => {
        (|$c2_params: $ty2| $body2)((move |$($c_params: $ty),*| $body)($($expr),*))
    };
    // call chain of closure with typed parameter follow by move closure with typed parameter
    ((|$($c_params: ident: $ty: ty),*| $body: expr)($($expr: expr),*), move |$c2_params: ident: $ty2: ty| $body2: expr) => {
        (move |$c2_params: $ty2| $body2)((|$($c_params: $ty),*| $body)($($expr),*))
    };
    // call chain of moved closure with typed parameter follow by another moved closure with typed parameter
    ((move |$($c_params: ident: $ty: ty),*| $body: expr)($($expr: expr),*), move |$c2_params: ident: $ty2: ty| $body2: expr) => {
        (move |$c2_params: $ty2| $body2)((move |$($c_params: $ty),*| $body)($($expr),*))
    };
    // multiple chain begin by function then closure then anything else
    ($function: ident($($params: expr),*), |$c_params: ident| $body: expr, $($other: tt)*) => {
        chain!((|$c_params| $body)($function($($params),*)), $($other)*)
    };
    // multiple chain begin by function then move closure then anything else
    ($function: ident($($params: expr),*), move |$c_params: ident| $body: expr, $($other: tt)*) => {
        chain!((move |$c_params| $body)($function($($params),*)), $($other)*)
    };
    // multiple chain begin by closure then function then anything else
    ((|$($c_params: ident),*| $body: expr)($($expr: expr),*), $function: ident, $($other: tt)*) => {
        chain!($function((|$c_params| $body)($($expr),*)), $($other)*)
    };
    // multiple chain begin by moved closure then function then anything else
    ((move |$($c_params: ident),*| $body: expr)($($expr: expr),*), $function: ident, $($other: tt)*) => {
        chain!($function((move |$($c_params),*| $body)($($expr),*)), $($other)*)
    };
    // multiple chain begin by closure then closure then anything else
    ((|$($c_params: ident),*| $body: expr)($($expr: expr),*), |$($c2_params: ident),*| $body2: expr, $($other: tt)*) => {
        chain!((|$($c_params),*| $body)($($expr),*), |$($c2_params),*| $body2)
    };
    // multiple chain begin by moved closure then closure then anything else
    ((move |$($c_params: ident),*| $body: expr)($($expr: expr),*), |$($c2_params: ident),*| $body2: expr, $($other: tt)*) => {
        chain!((move |$($c_params),*| $body)($($expr),*), |$($c2_params),*| $body2)
    };
    // multiple chain begin by closure then move closure then anything else
    ((|$($c_params: ident),*| $body: expr)($($expr: expr),*), move |$c2_params: ident| $body2: expr, $($other: tt)*) => {
        chain!((move |$c2_params| $body2)((|$($c_params),*| $body)($($expr),*)), $($other)*)
    };
    // multiple chain begin by move closure then move closure then anything else
    ((move |$($c_params: ident),*| $body: expr)($($expr: expr),*), move |$($c2_params: ident),*| $body2: expr, $($other: tt)*) => {
        chain!((move |$($c_params),*| $body)($($expr),*), move |$($c2_params),*| $body2)
    };
    // multiple chain begin by function then closure with typed params then anything else
    ($function: ident($($params: expr),*), |$c_params: ident: $ty: ty| $body: expr, $($other: tt)*) => {
        chain!((|$c_params: $ty| $body)($function($($params),*)), $($other)*)
    };
    // multiple chain begin by function then moved closure with typed params then anything else
    ($function: ident($($params: expr),*), move |$c_params: ident: $ty: ty| $body: expr, $($other: tt)*) => {
        chain!((move |$c_params: $ty| $body)($function($($params),*)), $($other)*)
    };
    // multiple chain begin by closure with typed param then function then anything else
    ((|$($c_params: ident: $ty: ty),*| $body: expr)($($expr: expr),*), $function: ident, $($other: tt)*) => {
        chain!($function((|$($c_params: $ty),*| $body)($($expr),*)), $($other)*)
    };
    // multiple chain begin by moved closure with typed param then function then anything else
    ((move |$($c_params: ident: $ty: ty),*| $body: expr)($($expr: expr),*), $function: ident, $($other: tt)*) => {
        chain!($function((|$c_params: $ty| $body)($($expr),*)), $($other)*)
    };
    // multiple chain begin by closure then closure then anything else
    ((|$($c_params: ident),*| $body: expr)($($expr: expr),*), |$c2_params: ident| $body2: expr, $($other: tt)*) => {
        chain!((|$c2_params| $body2)((|$($c_params),*| $body)($($expr),*)), $(other)*)
    };
    // multiple chain begin by closure then closure then anything else
    ((move |$($c_params: ident),*| $body: expr)($($expr: expr),*), |$c2_params: ident| $body2: expr, $($other: tt)*) => {
        chain!((|$c2_params| $body2)((move |$($c_params),*| $body)($($expr),*)), $(other)*)
    };
    // multiple chain begin by closure with typed params then closure then anything else
    ((|$($c_params: ident: $ty: ty),*| $body: expr)($($expr: expr),*), |$c2_params: ident| $body2: expr, $($other: tt)*) => {
        chain!((|$c2_params| $body2)((|$($c_params: $ty),*| $body)($($expr),*)), $($other)*)
    };
    // multiple chain begin by moved closure with typed params then closure then anything else
    ((move |$($c_params: ident: $ty: ty),*| $body: expr)($($expr: expr),*), |$c2_params: ident| $body2: expr, $($other: tt)*) => {
        chain!((|$c2_params| $body2)((move |$($c_params: $ty),*| $body)($($expr),*)), $($other)*)
    };
    // multiple chain begin by closure with typed params then moved closure then anything else
    ((|$($c_params: ident: $ty: ty),*| $body: expr)($($expr: expr),*), move |$c2_params: ident| $body2: expr, $($other: tt)*) => {
        chain!((move |$c2_params| $body2)((|$($c_params: $ty),*| $body)($($expr),*)), $($other)*)
    };
    // multiple chain begin by moved closure with typed params then moved closure then anything else
    ((move |$($c_params: ident: $ty: ty),*| $body: expr)($($expr: expr),*), move |$c2_params: ident| $body2: expr, $($other: tt)*) => {
        chain!((move |$c2_params| $body2)((move |$($c_params: $ty),*| $body)($($expr),*)), $($other)*)
    };
    // multiple chain begin by closure then closure with typed params then anything else
    ((|$($c_params: ident),*| $body: expr)($($expr: expr),*), |$c2_param: ident: $ty2: ty| $body2: expr, $($other: tt)*) => {
        chain!((|$sc2_param : $ty2| $body2)((|$($c_params),*| $body)($($expr),*)), $($other)*)
    };
    // multiple chain begin by moved closure then closure with typed params then anything else
    ((move |$($c_params: ident),*| $body: expr)($($expr: expr),*), |$c2_param: ident: $ty2: ty| $body2: expr, $($other: tt)*) => {
        chain!((|$sc2_param : $ty2| $body2)((move |$($c_params),*| $body)($($expr),*)), $($other)*)
    };
    // multiple chain begin by closure then moved closure with typed params then anything else
    ((|$($c_params: ident),*| $body: expr)($($expr: expr),*), move |$c2_param: ident: $ty2: ty| $body2: expr, $($other: tt)*) => {
        chain!((move |$sc2_param : $ty2| $body2)((|$($c_params),*| $body)($($expr),*)), $($other)*)
    };
    // multiple chain begin by moved closure then moved closure with typed params then anything else
    ((move |$($c_params: ident),*| $body: expr)($($expr: expr),*), move |$c2_param: ident: $ty2: ty| $body2: expr, $($other: tt)*) => {
        chain!((move |$sc2_param : $ty2| $body2)((move |$($c_params),*| $body)($($expr),*)), $($other)*)
    };
    // multiple chain begin by closure then closure, both are typed params, then anything else
    ((|$($c_params: ident: $ty: ty),*| $body: expr)($($expr: expr),*), |$c2_param: ident: $ty2: ty| $body2: expr, $($other: tt)*) => {
        chain!((|$c2_param : $ty2| $body2)((|$($c_params: $ty),*| $body)($($expr),*)), $($other)*)
    };
    // multiple chain begin by moved closure then closure, both are typed params, then anything else
    ((move |$($c_params: ident: $ty: ty),*| $body: expr)($($expr: expr),*), |$c2_param: ident: $ty2: ty| $body2: expr, $($other: tt)*) => {
        chain!((|$sc2_param : $ty2| $body2)(move |$($c_params: $ty),*| $body)($($expr),*), $($other)*)
    };
    // multiple chain begin by closure then moved closure, both are typed params, then anything else
    ((|$($c_params: ident: $ty: ty),*| $body: expr)($($expr: expr),*), move |$c2_param: ident: $ty2: ty| $body2: expr, $($other: tt)*) => {
        chain!((move |$sc2_param : $ty2| $body2)(|$($c_params: $ty),*| $body)($($expr),*), $($other)*)
    };
    // multiple chain begin by moved closure then moved closure, both are typed params, then anything else
    ((move |$($c_params: ident: $ty: ty),*| $body: expr)($($expr: expr),*), move |$c2_param: ident: $ty2: ty| $body2: expr, $($other: tt)*) => {
        chain!((move |$sc2_param : $ty2| $body2)(move |$($c_params: $ty),*| $body)($($expr),*), $($other)*)
    };
}

/// Chainable struct that permit user to rapidly call [chain method](struct.Chainable.html#method.chain)
/// to chain multiple function together using previous function returned value as input
/// to the next function in chain.
/// 
/// It is usually construct by using function [chain](fn.chain.html).
pub struct Chainable<R> {
    val: R
}

impl<R> Chainable<R> {
    /// Consume current chain and produce a new [Chainable](struct.Chainable.html) 
    /// that can further [chain](struct.Chainable.html#method.chain) as needed.
    pub fn chain<CR, F>(self, func: F) -> Chainable<CR> where F: FnOnce(R) -> CR {
        Chainable {
            val: func(self.val)
        }
    }

    /// Consume the chain and release it result
    pub fn end(self) -> R {
        self.val
    }
}

impl<R> std::ops::Deref for Chainable<R> {
    type Target = R;

    fn deref(&self) -> &R {
        &self.val
    }
}

/// A function that take an expression which return a single value or object then
/// return a [Chainable](struct.Chainable.html) then it can be chain with other function/closure
/// with method [chain](struct.Chainable.html#method.chain) of [Chainable.](struct.Chainable.html)
/// 
/// ## Example
/// ```rust
/// use fn_chain::chain;
/// 
/// fn function1(a : i32, b : i32, c : i32) -> i32 {
///     a + b + c
/// }
/// 
/// fn function2(d : i32) -> i32 {
///     d + 1
/// }
/// 
/// let var = 2;
/// let chained = chain(function1(1, 2, 3))
///                     .chain(function2)
///                     .chain(|param| param + 2)
///                         .chain(move |param| param * var);
/// assert_eq!(18, *chained);
/// // Chainable struct implement Deref to obtain final result.
/// // There's also `end` method that consume Chainable and return result.
/// assert_eq!(18, chained.end());
/// // chained no longer exist as it was consumed by end()
/// ```
pub fn chain<R>(result: R) -> Chainable<R> {
    Chainable { val: result }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn func_chain() {
        fn some_func(a: i64) -> i64 {
            a
        }
        fn add_one(b: i64) -> i64 {
            b + 1
        }
        assert_eq!(1.5f64, *chain(some_func(1)).chain(add_one).chain(add_one).chain(|c| (c as f64) / 2f64));
        assert_eq!(10, *chain(some_func(1)).chain(add_one).chain(|c| c * 2).chain(|c| c * 2).chain(add_one).chain(add_one));
    }

    #[test]
    fn func_chain_moved() {
        fn some_func(a: i64) -> i64 {
            a
        }
        fn add_one(b: i64) -> i64 {
            b + 1
        }
        assert_eq!(10, *chain(some_func(1)).chain(add_one).chain(move |c| c * 2).chain(move |c| c * 2).chain(add_one).chain(add_one));
        assert_eq!(10, *chain(some_func(1)).chain(add_one).chain(|c| c * 2).chain(move |c| c * 2).chain(add_one).chain(add_one));
        assert_eq!(10, *chain(some_func(1)).chain(add_one).chain(move |c| c * 2).chain(|c| c * 2).chain(add_one).chain(add_one));
        assert_eq!(14, *chain::<i64>((|a, b| a + b)(1i64, 1)).chain(add_one).chain(move |c| c * 2).chain(|c| c * 2).chain(add_one).chain(add_one));
        assert_eq!(6.5f64, *chain::<i64>((|a, b| a + b)(1i64, 1)).chain(add_one).chain(move |c| c * 2).chain(|c| c * 2).chain(add_one).chain(|d| {d as f64 / 2f64}));
    }

    #[test]
    fn macro_chain() {
        fn some_func(a: i64) -> i64 {
            a
        }
        fn another_func(b: i64) -> i64 {
            b + 1
        }
        assert_eq!(3, chain!(some_func(1), another_func, another_func));
    }

    #[test]
    fn macro_chain_closure() {
        fn some_func(a: i64) -> i64 {
            a
        }
        fn some_add(a: i64, b: i64, c: i64) -> i64 {
            a + b + c
        }
        fn pass_through(v: f64) -> f64 {
            v
        }
        assert_eq!(2, chain!(some_func(1), |a| {return a + 1}, |b| {return b * 1}, some_func, some_func));
        assert_eq!(6f64, chain!(some_add(1, 2, 3), |result: i64| {(result as f64).powi(2)}, |sqr: f64| {sqr.powf(0.5)}, pass_through, pass_through));
    }

    #[test]
    fn macro_chain_moved_closure() {
        fn some_func(a: i64) -> i64 {
            a
        }
        let value = 2;
        let one = 1;
        assert_eq!(3, chain!(some_func(1), move |a| {return a + value}, move |b| {return b * one}, some_func, some_func));
        assert_eq!(3, chain!(some_func(1), |a| {return a + value}, move |b| {return b * one}, some_func, some_func));
        assert_eq!(3, chain!(some_func(1), move |a| {return a + value}, |b| {return b * one}, some_func, some_func));
    }
}
