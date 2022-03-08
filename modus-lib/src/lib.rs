// Modus, a language for building container images
// Copyright (C) 2022 University College London

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.

// You should have received a copy of the GNU Affero General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

pub mod analysis;
// pub mod buildkit;
pub mod builtin;
pub mod dockerfile;
pub mod imagegen;
pub mod logic;
pub mod modusfile;
// pub mod reporting;
pub mod sld;
pub mod translate;
pub mod transpiler;
pub mod unification;
pub mod wellformed;

#[macro_use]
extern crate lazy_static;
