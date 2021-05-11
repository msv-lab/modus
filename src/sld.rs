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


use crate::datalog;
use datalog::{ Rule };

pub struct Renamed<V> {
    index: u32,
    variable: V,
}

static mut availableIndex: u32 = 0;

impl Renamed<V> {
    pub fn new(variable: V) -> Renamed<V> {
        let index = availableIndex;
        availableIndex += 1;
        Renaming{ index, variable }
    }
    pub fn rename(&self) -> Renamed<V> {
        let index = availableIndex;
        availableIndex += 1;
        Renaming{ index, ..self }       
    }
}

type Substitution = i32;