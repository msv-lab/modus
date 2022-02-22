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

//! This module exist because the buildkit_llb crate doesn't provide a way to
//! say something like Arc<dyn Trait> where Trait is implemented for any T
//! such that Arc<T>: SingleOwnedOutput or (Arc<T>, u32): MultiOwnedOutput.

use std::sync::Arc;

use buildkit_llb::{
    prelude::{
        fs::SequenceOperation,
        source::{ImageSource, LocalSource},
        Command, MultiOwnedLastOutput, MultiOwnedOutput, SingleOwnedOutput,
    },
    utils::OperationOutput,
};

#[derive(Clone)]
pub enum OwnedOutput {
    ImageSource(Arc<ImageSource>),
    LocalSource(Arc<LocalSource>),
    Command(Arc<Command<'static>>, u32),
    Sequence(Arc<SequenceOperation<'static>>),
}

impl OwnedOutput {
    pub fn output(&self) -> OperationOutput<'static> {
        match self {
            OwnedOutput::ImageSource(s) => s.output(),
            OwnedOutput::LocalSource(s) => s.output(),
            OwnedOutput::Command(s, i) => s.output(*i),
            OwnedOutput::Sequence(s) => s.last_output().unwrap(),
        }
    }
}

impl From<Arc<ImageSource>> for OwnedOutput {
    fn from(s: Arc<ImageSource>) -> Self {
        OwnedOutput::ImageSource(s)
    }
}
impl From<Arc<LocalSource>> for OwnedOutput {
    fn from(s: Arc<LocalSource>) -> Self {
        OwnedOutput::LocalSource(s)
    }
}
impl From<Arc<SequenceOperation<'static>>> for OwnedOutput {
    fn from(s: Arc<SequenceOperation<'static>>) -> Self {
        OwnedOutput::Sequence(s)
    }
}
impl OwnedOutput {
    pub fn from_command(s: Arc<Command<'static>>, i: u32) -> Self {
        OwnedOutput::Command(s, i)
    }
}
