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

use crate::logic::{Atom, Term, Literal, Rule};


/// checks if the rules have recursion
/// outputs predicates with recursive dependencies if any
pub fn check_recursion<C, V>(rules: Vec<Rule<C,V>>) -> Option<Vec<Atom>> {
    None //TODO use dependency graphs to find loops
}

/// checks if image predicates transitively depend on image/1 in each disjunct
/// outputs those that do not depend on image/1 in some disjuncts
pub fn check_image_predicates<C, V>(rules: Vec<Rule<C,V>>) -> Option<Vec<Atom>> {
    None
}

