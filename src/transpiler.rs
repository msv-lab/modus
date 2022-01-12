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

use std::{io::Write, str::FromStr};

use codespan_reporting::diagnostic::Diagnostic;

use crate::{
    dockerfile::{Dockerfile, Image, Instruction, ResolvedDockerfile, ResolvedParent, Run},
    imagegen::{self, BuildPlan, MergeNode, NodeId},
    logic::{self, Clause, IRTerm, Literal, Predicate},
    modusfile::Modusfile,
    sld::{self, ClauseId, ResolutionError, SLDResult, Tree},
};

use crate::imagegen::BuildNode;

/// Renders the entire SLD tree as a DOT graph, to the writer.
pub fn render_tree<W: Write>(clauses: &Vec<Clause>, sld_result: SLDResult, output: &mut W) {
    // TODO: we could figure out the corresponding error for each failed path
    let g = sld_result.full_tree.to_graph(clauses);
    dot::render(&g, output).unwrap()
}

pub fn transpile(
    mf: Modusfile,
    query: logic::Literal,
) -> Result<Dockerfile<ResolvedParent>, Vec<Diagnostic<()>>> {
    let build_plan = imagegen::plan_from_modusfile(mf, query)?;
    Ok(plan_to_docker(&build_plan))
}

fn plan_to_docker(plan: &BuildPlan) -> ResolvedDockerfile {
    let topological_order = plan.topological_order();

    let mut instructions = topological_order
        .into_iter()
        .map(|node_id| {
            use crate::dockerfile::*;
            let node = &plan.nodes[node_id];
            let str_id = format!("n_{}", node_id);
            match node {
                BuildNode::From { image_ref } => vec![Instruction::From(From {
                    parent: ResolvedParent::Image(Image::from_str(image_ref).unwrap()),
                    alias: Some(str_id),
                })],
                BuildNode::Run {
                    parent,
                    command,
                    cwd,
                } => vec![
                    Instruction::From(From {
                        parent: ResolvedParent::Stage(format!("n_{}", parent)),
                        alias: Some(str_id),
                    }),
                    Instruction::Run(Run(if cwd.is_empty() {
                        command.to_owned()
                    } else {
                        format!("cd {:?} || exit 1; {}", cwd, command)
                    })),
                ],
                BuildNode::CopyFromImage {
                    parent,
                    src_image,
                    src_path,
                    dst_path,
                } => vec![
                    Instruction::From(From {
                        parent: ResolvedParent::Stage(format!("n_{}", parent)),
                        alias: Some(str_id),
                    }),
                    Instruction::Copy(Copy(format!(
                        "--from=n_{} {:?} {:?}", // TODO: is this really correct?
                        src_image, src_path, dst_path
                    ))),
                ],
                BuildNode::CopyFromLocal {
                    parent,
                    src_path,
                    dst_path,
                } => vec![
                    Instruction::From(From {
                        parent: ResolvedParent::Stage(format!("n_{}", parent)),
                        alias: Some(str_id),
                    }),
                    Instruction::Copy(Copy(format!("{:?} {:?}", src_path, dst_path))),
                ],
                BuildNode::SetWorkdir {
                    parent,
                    new_workdir,
                } => vec![
                    Instruction::From(From {
                        parent: ResolvedParent::Stage(format!("n_{}", parent)),
                        alias: Some(str_id),
                    }),
                    Instruction::Workdir(Workdir(new_workdir.to_string())),
                ],
                BuildNode::SetEntrypoint {
                    parent,
                    new_entrypoint,
                } => vec![
                    Instruction::From(From {
                        parent: ResolvedParent::Stage(format!("n_{}", parent)),
                        alias: Some(str_id),
                    }),
                    Instruction::Entrypoint(format!("{:?}", new_entrypoint)),
                ],
                BuildNode::SetLabel {
                    parent,
                    label,
                    value,
                } => vec![
                    Instruction::From(From {
                        parent: ResolvedParent::Stage(format!("n_{}", parent)),
                        alias: Some(str_id),
                    }),
                    Instruction::Label(label.to_owned(), value.to_owned()),
                ],
                BuildNode::Merge(MergeNode { parent, operations }) => {
                    let mut insts = Vec::new();
                    insts.push(Instruction::From(From {
                        parent: ResolvedParent::Stage(format!("n_{}", parent)),
                        alias: Some(str_id),
                    }));
                    for op in operations {
                        use imagegen::MergeOperation;
                        match op {
                            MergeOperation::Run { command, cwd } => {
                                insts.push(Instruction::Run(Run(if cwd.is_empty() {
                                    command.to_owned()
                                } else {
                                    format!("cd {:?} || exit 1; {}", cwd, command)
                                })));
                            }
                            MergeOperation::CopyFromLocal { src_path, dst_path } => {
                                insts.push(Instruction::Copy(Copy(format!(
                                    "{:?} {:?}",
                                    src_path, dst_path
                                ))));
                            }
                            MergeOperation::CopyFromImage {
                                src_image,
                                src_path,
                                dst_path,
                            } => {
                                insts.push(Instruction::Copy(Copy(format!(
                                    "--from=n_{} {:?} {:?}",
                                    src_image, src_path, dst_path
                                ))));
                            }
                        }
                    }
                    insts
                }
            }
        })
        .flatten()
        .collect::<Vec<_>>();

    if plan.outputs.len() > 1 {
        use crate::dockerfile::{From, Run};
        instructions.push(Instruction::From(From {
            parent: ResolvedParent::Stage("busybox".to_owned()),
            alias: Some("force_multioutput".to_owned()),
        }));

        for o in plan.outputs.iter() {
            let k = format!("n_{}", o.node);
            instructions.push(Instruction::Run(Run(format!(
                "--mount=type=bind,from={},source=/,target=/mnt true",
                k,
            ))));
        }
    }

    Dockerfile(instructions)
}
