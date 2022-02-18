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
