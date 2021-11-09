// Copyright 2021 Sergey Mechtaev

// This file is part of Modus.

// Modus is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// Modus is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with Modus.  If not, see <https://www.gnu.org/licenses/>.

use crate::logic::{IRTerm, Literal, Predicate};

pub trait BuiltinPredicate {
    fn name(&self) -> &'static str;
    fn arg_groundness(&self) -> &'static [bool];

    fn select(&self, lit: &Literal) -> bool {
        let Literal { ref atom, ref args } = lit;
        if &atom.0 != self.name() {
            return false;
        }
        args.iter()
            .zip(self.arg_groundness().into_iter())
            .all(|pair| matches!(pair, (_, true) | (IRTerm::Constant(_), false)))
    }

    /// Return a new literal specifically constructed to unify with the input
    /// literal. The returned literal will essentially be used as the head of a
    /// new "hidden" rule, which will hopefully unify with the input literal.
    /// The rule will contain no body literals.
    ///
    /// For example, the implementation of run should simply return the input
    /// literal, after checking that it only contains a constant. (Returning any
    /// unresolved variables can make the actual generation of build
    /// instructions impossible)
    ///
    /// Renaming will not be done on this literal, so if variables are needed
    /// they must all be either auxillary or some existing variables from the
    /// input.
    fn apply(&self, lit: &Literal) -> Option<Literal>;
}

trait MaybeStringConst {
    fn as_str_const(&self) -> Option<String>;
}

impl MaybeStringConst for IRTerm {
    fn as_str_const(&self) -> Option<String> {
        match &self {
            IRTerm::Constant(c) => Some(c.to_string()),
            _ => None,
        }
    }
}

fn string_concat_result(a: String, b: String, c: String) -> Option<Literal> {
    Some(Literal {
        atom: Predicate("string_concat".to_owned()),
        args: vec![
            IRTerm::Constant(a),
            IRTerm::Constant(b),
            IRTerm::Constant(c),
        ],
    })
}

pub struct StringConcat1;
impl BuiltinPredicate for StringConcat1 {
    fn name(&self) -> &'static str {
        "string_concat"
    }

    fn arg_groundness(&self) -> &'static [bool] {
        &[false, false, true]
    }

    fn apply(&self, lit: &Literal) -> Option<Literal> {
        let a = lit.args[0].as_str_const()?;
        let b = lit.args[1].as_str_const()?;
        let c = a.clone() + &b;
        string_concat_result(a, b, c)
    }
}

pub struct StringConcat2;
impl BuiltinPredicate for StringConcat2 {
    fn name(&self) -> &'static str {
        "string_concat"
    }

    fn arg_groundness(&self) -> &'static [bool] {
        &[true, false, false]
    }

    fn apply(&self, lit: &Literal) -> Option<Literal> {
        let b = lit.args[1].as_str_const()?;
        let c = lit.args[2].as_str_const()?;
        if let Some(a) = c.strip_suffix(&b) {
            string_concat_result(a.to_string(), b, c)
        } else {
            None
        }
    }
}

pub struct StringConcat3;
impl BuiltinPredicate for StringConcat3 {
    fn name(&self) -> &'static str {
        "string_concat"
    }

    fn arg_groundness(&self) -> &'static [bool] {
        &[false, true, false]
    }

    fn apply(&self, lit: &Literal) -> Option<Literal> {
        let a = lit.args[0].as_str_const()?;
        let c = lit.args[2].as_str_const()?;
        if let Some(b) = c.strip_prefix(&a) {
            string_concat_result(a, b.to_string(), c)
        } else {
            None
        }
    }
}

mod run {
    use crate::logic::{IRTerm, Literal};

    use super::BuiltinPredicate;

    pub struct Run;
    impl BuiltinPredicate for Run {
        fn name(&self) -> &'static str {
            "run"
        }

        fn arg_groundness(&self) -> &'static [bool] {
            &[false]
        }

        fn apply(&self, lit: &Literal) -> Option<Literal> {
            let l = (*lit).clone();
            match lit.args[0] {
                // it's valid as long as we're given a constant
                IRTerm::Constant(_) => Some(l),
                _ => None,
            }
        }
    }
}

mod from {
    use crate::logic::{IRTerm, Literal};

    use super::BuiltinPredicate;

    pub struct From;
    impl BuiltinPredicate for From {
        fn name(&self) -> &'static str {
            "from"
        }

        fn arg_groundness(&self) -> &'static [bool] {
            &[false]
        }

        fn apply(&self, lit: &Literal) -> Option<Literal> {
            let l = (*lit).clone();
            match lit.args[0] {
                // it's valid as long as we're given a constant
                IRTerm::Constant(_) => Some(l),
                _ => None,
            }
        }
    }
}

/// Convenience macro that returns Some(b) for the first b that can be selected.
macro_rules! select_builtins {
    ( $lit:expr, $x1:expr, $( $x:expr ),* ) => {
        if $x1.select($lit) {
            return Some(&$x1);
        }
        $(
            else if $x.select($lit) {
                return Some(&$x);
            }
        )*
    };
}

pub fn select_builtin<'a>(lit: &Literal) -> Option<&'a dyn BuiltinPredicate> {
    select_builtins!(
        lit,
        StringConcat1,
        StringConcat2,
        StringConcat3,
        run::Run,
        from::From
    );
    None
}
