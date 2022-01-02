use std::path::Path;

use serde::Serialize;

use crate::{
    imagegen::BuildPlan,
    logic::{IRTerm, Literal},
};

pub type BuildResult = Vec<Image>;

#[derive(Serialize, Debug, Clone)]
pub struct ConstantLiteral {
    pub predicate: String,
    pub args: Vec<String>,
}

impl ConstantLiteral {
    pub fn from_literal(lit: Literal) -> Self {
        Self {
            predicate: lit.predicate.0,
            args: lit
                .args
                .into_iter()
                .map(|x| match x {
                    IRTerm::Constant(x) => x,
                    _ => panic!("Expected constant"),
                })
                .collect::<Vec<_>>(),
        }
    }
}

#[derive(Serialize, Debug, Clone)]
pub struct Image {
    pub source_literal: ConstantLiteral,
    pub digest: String,
}

pub fn write_build_result<P: AsRef<Path>>(
    json_out: P,
    build_plan: &BuildPlan,
    image_ids: &[String],
) -> Result<(), String> {
    debug_assert_eq!(build_plan.outputs.len(), image_ids.len());
    debug_assert!(build_plan
        .outputs
        .iter()
        .all(|x| x.source_literal.is_some()));

    let res = build_plan
        .outputs
        .iter()
        .zip(image_ids)
        .map(|(o, i)| Image {
            source_literal: ConstantLiteral::from_literal(
                o.source_literal.as_ref().unwrap().clone(),
            ),
            digest: i.clone(),
        })
        .collect::<Vec<_>>();

    std::fs::write(
        json_out.as_ref(),
        serde_json::to_vec_pretty(&res).map_err(|e| format!("Serialization error: {}", e))?,
    )
    .map_err(|e| format!("Error writing to {}: {}", json_out.as_ref().display(), e))?;

    Ok(())
}
