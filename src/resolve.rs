// Copyright 2026 Oxide Computer Company

use std::{borrow::Cow, collections::HashSet};

use anyhow::{anyhow, bail};
use openapiv3::ReferenceOr;
use serde::{Deserialize, de::DeserializeOwned};

use crate::{context::Context, path::RefTargetPath};

pub trait ReferenceOrResolver<'a, 'context, T>
where
    T: Clone,
{
    fn resolve(
        &'a self,
        context: &Context<'context>,
    ) -> anyhow::Result<(Cow<'a, T>, Context<'context>)>;
}

impl<'a, 'context, T> ReferenceOrResolver<'a, 'context, T> for ReferenceOr<T>
where
    T: DeserializeOwned + Clone,
{
    fn resolve(
        &'a self,
        context: &Context<'context>,
    ) -> anyhow::Result<(Cow<'a, T>, Context<'context>)> {
        let mut context = context.clone();
        let mut target = match self {
            ReferenceOr::Item(item) => return Ok((Cow::Borrowed(item), context)),
            ReferenceOr::Reference { reference } => Cow::Borrowed(reference),
        };

        let mut follower = ReferenceFollower::default();
        loop {
            let (item_or_reference, next_context) = follower.follow(&context, &target)?;
            context = next_context;

            match item_or_reference {
                ReferenceOr::Item(item) => return Ok((Cow::Owned(item), context)),

                ReferenceOr::Reference { reference } => {
                    target = Cow::Owned(reference);
                }
            }
        }
    }
}

/// `$ref` chain follower and cycle detector.
///
/// Unlike `Compare::visit_state`, which tracks schemas which have been
/// resolved, this tracks a chain of `$ref`s followed before reaching a schema
/// at all. For example, a chain like:
///
/// * `A: { "$ref": "#/components/schemas/B" }`
/// * `B: { "$ref": "#/components/schemas/C" }`
///
/// and so on.
///
/// This also produces an error if there are any loops in the `$ref` chain.
/// (`Compare::visit_state` can't handle this because in case of a loop we never
/// reach a schema.) For example:
///
/// * `A: { "$ref": "#/components/schemas/B" }`
/// * `B: { "$ref": "#/components/schemas/C" }`
/// * `C: { "$ref": "#/components/schemas/A" }`
#[derive(Debug, Default)]
pub(crate) struct ReferenceFollower {
    followed: HashSet<RefTargetPath>,
}

impl ReferenceFollower {
    pub(crate) fn follow<'context, T>(
        &mut self,
        context: &Context<'context>,
        reference: &str,
    ) -> anyhow::Result<(ReferenceOr<T>, Context<'context>)>
    where
        T: DeserializeOwned,
    {
        let target: RefTargetPath = reference.parse()?;
        let next_context = context.push(&target);

        let subtree = next_context
            .raw_openapi
            .pointer(target.json_pointer())
            .ok_or(anyhow!("invalid reference {reference}"))?;

        if !self.followed.insert(target) {
            bail!(
                "`$ref` {reference} leads back to itself without reaching a \
                 definition (references followed: {})",
                context.stack(),
            );
        }

        let item_or_reference = ReferenceOr::<T>::deserialize(subtree)?;
        Ok((item_or_reference, next_context))
    }
}
