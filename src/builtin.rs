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

use crate::logic::{Literal, Term};

pub trait BuiltinPredicate<C, V> {
    fn name(&self) -> &'static str;
    fn arg_groundness(&self) -> &'static [bool];

    fn select(&self, lit: &Literal<C, V>) -> bool {
        let Literal { ref atom, ref args } = lit;
        if &atom.0 != self.name() {
            return false;
        }
        args.iter()
            .zip(self.arg_groundness().into_iter())
            .all(|pair| !matches!(pair, (Term::Variable(_), false)))
    }
}

pub struct StringConcat1;
impl<C: ToString, V> BuiltinPredicate<C, V> for StringConcat1 {
    fn name(&self) -> &'static str {
        "string_concat"
    }

    fn arg_groundness(&self) -> &'static [bool] {
        &[false, false, true]
    }
}

pub struct StringConcat2;
impl<C: ToString, V> BuiltinPredicate<C, V> for StringConcat2 {
    fn name(&self) -> &'static str {
        "string_concat"
    }

    fn arg_groundness(&self) -> &'static [bool] {
        &[true, false, false]
    }
}

pub struct StringConcat3;
impl<C: ToString, V> BuiltinPredicate<C, V> for StringConcat3 {
    fn name(&self) -> &'static str {
        "string_concat"
    }

    fn arg_groundness(&self) -> &'static [bool] {
        &[false, true, false]
    }
}

pub fn select_builtin<'a, C: 'a + ToString, V: 'a>(
    lit: &Literal<C, V>,
) -> Option<&'a dyn BuiltinPredicate<C, V>> {
    if StringConcat1.select(lit) {
        return Some(&StringConcat1);
    } else if StringConcat2.select(lit) {
        return Some(&StringConcat2);
    } else if StringConcat3.select(lit) {
        return Some(&StringConcat3);
    }
    None
}
