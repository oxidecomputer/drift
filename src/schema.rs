// Copyright 2025 Oxide Computer Company

use std::fmt;

use openapiv3::{AdditionalProperties, ArrayType, ObjectType, ReferenceOr, Schema, SchemaData};

use crate::{
    ChangeClass, ChangeComparison, ChangeDetails,
    compare::Compare,
    context::{Contextual, ToContext},
    setops::SetCompare,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum SchemaComparison {
    Input,
    Output,
}

impl From<SchemaComparison> for ChangeComparison {
    fn from(value: SchemaComparison) -> Self {
        match value {
            SchemaComparison::Input => ChangeComparison::Input,
            SchemaComparison::Output => ChangeComparison::Output,
        }
    }
}

impl Compare {
    pub fn compare_schema_ref(
        &mut self,
        comparison: SchemaComparison,
        old_schema: Contextual<'_, &ReferenceOr<Schema>>,
        new_schema: Contextual<'_, &ReferenceOr<Schema>>,
    ) -> anyhow::Result<()> {
        let _ = self.compare_schema_ref_helper(false, comparison, old_schema, new_schema)?;
        Ok(())
    }

    fn compare_schema_ref_helper(
        &mut self,
        dry_run: bool,
        comparison: SchemaComparison,
        old_schema: Contextual<'_, &ReferenceOr<Schema>>,
        new_schema: Contextual<'_, &ReferenceOr<Schema>>,
    ) -> anyhow::Result<bool> {
        // Handle single-element wrappers: allOf/anyOf/oneOf with one item.
        // These are semantically equivalent to their inner type.
        //
        // An allOf wrapper is commonly added to include additional metadata
        // such as an additional description field.
        if let Some(result) =
            self.try_compare_flattened(dry_run, comparison, &old_schema, &new_schema)?
        {
            Ok(result)
        } else {
            // General path: resolve and compare.
            let (old_schema, old_context) = old_schema.contextual_resolve()?;
            let (new_schema, new_context) = new_schema.contextual_resolve()?;

            let old_schema = Contextual::new(old_context, old_schema.as_ref());
            let new_schema = Contextual::new(new_context, new_schema.as_ref());

            self.compare_schema(comparison, dry_run, old_schema, new_schema)
        }
    }

    /// Try to compare schemas by flattening single-element wrappers.
    ///
    /// Returns `Some(result)` if flattening was applicable, `None` to fall
    /// through to the general path.
    fn try_compare_flattened(
        &mut self,
        dry_run: bool,
        comparison: SchemaComparison,
        old_schema: &Contextual<'_, &ReferenceOr<Schema>>,
        new_schema: &Contextual<'_, &ReferenceOr<Schema>>,
    ) -> anyhow::Result<Option<bool>> {
        use SchemaRefKind::*;

        let old_kind = classify_schema_ref(old_schema.as_ref());
        let new_kind = classify_schema_ref(new_schema.as_ref());

        match (old_kind, new_kind) {
            (
                SingleElement {
                    inner: old_inner,
                    metadata: old_meta,
                },
                SingleElement {
                    inner: new_inner,
                    metadata: new_meta,
                },
            ) => {
                // Both old and new are single-element wrappers.
                //
                // Compare effective nullable (wrapper || inner) since nullable
                // is a semantic property, not metadata.
                let old_nullable = get_effective_nullable(old_schema)?;
                let new_nullable = get_effective_nullable(new_schema)?;
                let _ = self.compare_wrapper_nullable(
                    dry_run,
                    comparison,
                    old_schema,
                    new_schema,
                    old_nullable,
                    new_nullable,
                )?;
                if has_non_nullable_metadata_diff(old_meta, new_meta) {
                    self.push_change(
                        "schema metadata changed",
                        old_schema,
                        new_schema,
                        comparison.into(),
                        ChangeClass::Trivial,
                        ChangeDetails::Metadata,
                    );
                }
                let old_inner = old_schema.append_deref(old_inner, "0");
                let new_inner = new_schema.append_deref(new_inner, "0");
                Ok(Some(self.compare_schema_ref_helper(
                    dry_run, comparison, old_inner, new_inner,
                )?))
            }
            (
                SingleElement {
                    inner: old_inner,
                    metadata: old_meta,
                },
                BareRef | InlineType,
            ) => {
                // Old is a single-element wrapper, new is a bare ref or inline
                // type.
                //
                // Compare effective nullable (wrapper || inner) since nullable
                // is a semantic property, not metadata.
                let old_nullable = get_effective_nullable(old_schema)?;
                let new_nullable = get_effective_nullable(new_schema)?;
                let _ = self.compare_wrapper_nullable(
                    dry_run,
                    comparison,
                    old_schema,
                    new_schema,
                    old_nullable,
                    new_nullable,
                )?;
                if has_meaningful_non_nullable_metadata(old_meta) {
                    self.push_change(
                        "schema metadata removed",
                        old_schema,
                        new_schema,
                        comparison.into(),
                        ChangeClass::Trivial,
                        ChangeDetails::Metadata,
                    );
                }
                let old_inner = old_schema.append_deref(old_inner, "0");
                Ok(Some(self.compare_schema_ref_helper(
                    dry_run,
                    comparison,
                    old_inner,
                    new_schema.clone(),
                )?))
            }
            (
                BareRef | InlineType,
                SingleElement {
                    inner: new_inner,
                    metadata: new_meta,
                },
            ) => {
                // Old is a bare ref or inline type, new is a single-element
                // wrapper.
                //
                // Compare effective nullable (wrapper || inner) since nullable
                // is a semantic property, not metadata.
                let old_nullable = get_effective_nullable(old_schema)?;
                let new_nullable = get_effective_nullable(new_schema)?;
                let _ = self.compare_wrapper_nullable(
                    dry_run,
                    comparison,
                    old_schema,
                    new_schema,
                    old_nullable,
                    new_nullable,
                )?;
                if has_meaningful_non_nullable_metadata(new_meta) {
                    self.push_change(
                        "schema metadata added",
                        old_schema,
                        new_schema,
                        comparison.into(),
                        ChangeClass::Trivial,
                        ChangeDetails::Metadata,
                    );
                }
                let new_inner = new_schema.append_deref(new_inner, "0");
                Ok(Some(self.compare_schema_ref_helper(
                    dry_run,
                    comparison,
                    old_schema.clone(),
                    new_inner,
                )?))
            }
            (BareRef | InlineType | MultiElement, BareRef | InlineType | MultiElement)
            | (SingleElement { .. }, MultiElement)
            | (MultiElement, SingleElement { .. }) => {
                // No flattening applicable, so fall through to the general
                // comparison path.
                Ok(None)
            }
        }
    }

    /// Compare nullable flags from wrapper metadata, reporting appropriate
    /// compatibility changes.
    ///
    /// This is used during wrapper flattening to ensure nullable changes at the
    /// wrapper level are properly classified (not treated as trivial metadata).
    fn compare_wrapper_nullable<T, U>(
        &mut self,
        dry_run: bool,
        comparison: SchemaComparison,
        old_schema: &Contextual<'_, T>,
        new_schema: &Contextual<'_, U>,
        old_nullable: bool,
        new_nullable: bool,
    ) -> anyhow::Result<bool> {
        if old_nullable == new_nullable {
            return Ok(true);
        }

        let (message, class, details) = if new_nullable {
            // non-nullable → nullable
            let class = match comparison {
                SchemaComparison::Input => ChangeClass::ForwardIncompatible,
                SchemaComparison::Output => ChangeClass::BackwardIncompatible,
            };
            ("schema became nullable", class, ChangeDetails::LessStrict)
        } else {
            // nullable → non-nullable
            let class = match comparison {
                SchemaComparison::Input => ChangeClass::BackwardIncompatible,
                SchemaComparison::Output => ChangeClass::ForwardIncompatible,
            };
            (
                "schema became non-nullable",
                class,
                ChangeDetails::MoreStrict,
            )
        };

        self.schema_push_change(
            dry_run,
            message,
            old_schema,
            new_schema,
            comparison,
            class,
            details,
        )
    }

    fn compare_schema(
        &mut self,
        comparison: SchemaComparison,
        dry_run: bool,
        old_schema: Contextual<'_, &Schema>,
        new_schema: Contextual<'_, &Schema>,
    ) -> anyhow::Result<bool> {
        // We wait for both new and old to contain a cycle; this ensures that
        // we consider "unrolled" cycles properly. There is a possibility of
        // getting stuck in an A->B->A / B->A->B cycle... we can address that
        // should that construction arise.
        if old_schema.context().stack().contains_cycle()
            && new_schema.context().stack().contains_cycle()
        {
            return Ok(true);
        }

        // Return the cached compatibility of these schemas so that we don't
        // generate redundant notes.
        if let Some(equal) = self.visited.get(&(
            comparison,
            old_schema.context().stack().top.clone(),
            new_schema.context().stack().top.clone(),
        )) {
            return Ok(*equal);
        }

        // We expand structures to ensure we don't accidentally fail to examine
        // a field.
        let Schema {
            schema_data:
                SchemaData {
                    nullable: old_nullable,
                    read_only: old_read_only,
                    write_only: old_write_only,
                    deprecated: old_deprecated,
                    external_docs: old_external_docs,
                    example: old_example,
                    title: old_title,
                    description: old_description,
                    discriminator: old_discriminator,
                    default: old_default,
                    extensions: old_extensions,
                },
            schema_kind: old_schema_kind,
        } = old_schema.as_ref();
        let Schema {
            schema_data:
                SchemaData {
                    nullable: new_nullable,
                    read_only: new_read_only,
                    write_only: new_write_only,
                    deprecated: new_deprecated,
                    external_docs: new_external_docs,
                    example: new_example,
                    title: new_title,
                    description: new_description,
                    discriminator: new_discriminator,
                    default: new_default,
                    extensions: new_extensions,
                },
            schema_kind: new_schema_kind,
        } = new_schema.as_ref();

        // Examine properties that don't affect the serialized form.
        let metadata_equal = old_read_only == new_read_only
            && old_write_only == new_write_only
            && old_deprecated == new_deprecated
            && old_external_docs == new_external_docs
            && old_example == new_example
            && old_title == new_title
            && old_description == new_description
            && old_discriminator == new_discriminator
            && old_default == new_default
            && old_extensions == new_extensions;

        if !metadata_equal {
            let _ = self.schema_push_change(
                dry_run,
                "schema metadata changed".to_string(),
                &old_schema,
                &new_schema,
                comparison,
                ChangeClass::Trivial,
                ChangeDetails::Metadata,
            );
        }

        let nullable_equal = if old_nullable == new_nullable {
            true
        } else if *new_nullable {
            // non-nullable → nullable: the schema now accepts/returns null.
            //
            // For outputs: old clients may receive null from new servers and
            // break. New clients can handle null from old servers (which never
            // send it). This is backward incompatible.
            //
            // For inputs: old clients send non-null to new servers, which
            // accept it. New clients may send null to old servers, which
            // reject it. This is forward incompatible.
            let class = match comparison {
                SchemaComparison::Input => ChangeClass::ForwardIncompatible,
                SchemaComparison::Output => ChangeClass::BackwardIncompatible,
            };
            self.schema_push_change(
                dry_run,
                "schema became nullable",
                &old_schema,
                &new_schema,
                comparison,
                class,
                ChangeDetails::LessStrict,
            )?
        } else {
            // nullable → non-nullable: the schema no longer accepts/returns null.
            //
            // For outputs: old clients can handle non-null from new servers.
            // New clients expect non-null but old servers may return null.
            // This is forward incompatible.
            //
            // For inputs: old clients may send null to new servers, which
            // reject it. New clients send non-null to old servers, which
            // accept it. This is backward incompatible.
            let class = match comparison {
                SchemaComparison::Input => ChangeClass::BackwardIncompatible,
                SchemaComparison::Output => ChangeClass::ForwardIncompatible,
            };
            self.schema_push_change(
                dry_run,
                "schema became non-nullable",
                &old_schema,
                &new_schema,
                comparison,
                class,
                ChangeDetails::MoreStrict,
            )?
        };

        let schema_equal = self.compare_schema_kind(
            comparison,
            dry_run,
            Contextual::new(old_schema.context().clone(), old_schema_kind),
            Contextual::new(new_schema.context().clone(), new_schema_kind),
        )?;

        // Cache the result.
        self.visited.insert(
            (
                comparison,
                old_schema.context().stack().top.clone(),
                new_schema.context().stack().top.clone(),
            ),
            nullable_equal && schema_equal,
        );

        Ok(nullable_equal && schema_equal)
    }

    pub(crate) fn compare_schema_kind(
        &mut self,
        comparison: SchemaComparison,
        dry_run: bool,
        old_schema_kind: Contextual<'_, &openapiv3::SchemaKind>,
        new_schema_kind: Contextual<'_, &openapiv3::SchemaKind>,
    ) -> anyhow::Result<bool> {
        match (old_schema_kind.as_ref(), new_schema_kind.as_ref()) {
            (&openapiv3::SchemaKind::Type(old_type), &openapiv3::SchemaKind::Type(new_type)) => {
                self.compare_schema_type(
                    comparison,
                    dry_run,
                    old_schema_kind.subcomponent(old_type),
                    new_schema_kind.subcomponent(new_type),
                )
            }
            (
                openapiv3::SchemaKind::OneOf { one_of: old_one_of },
                openapiv3::SchemaKind::OneOf { one_of: new_one_of },
            ) => {
                let old_one_of = old_schema_kind.append_deref(old_one_of, "oneOf");
                let new_one_of = new_schema_kind.append_deref(new_one_of, "oneOf");
                self.compare_schema_type_one_of(comparison, dry_run, old_one_of, new_one_of)
            }
            (
                openapiv3::SchemaKind::AllOf { all_of: old_all_of },
                openapiv3::SchemaKind::AllOf { all_of: new_all_of },
            ) => {
                let old_all_of = old_schema_kind.append_deref(old_all_of, "allOf");
                let new_all_of = new_schema_kind.append_deref(new_all_of, "allOf");
                self.compare_schema_type_all_of(comparison, dry_run, old_all_of, new_all_of)
            }

            (
                openapiv3::SchemaKind::AnyOf { any_of: old_any_of },
                openapiv3::SchemaKind::AnyOf { any_of: new_any_of },
            ) => {
                if old_any_of != new_any_of {
                    self.schema_push_change(
                        dry_run,
                        "unhandled, 'anyOf' schema",
                        &old_schema_kind,
                        &new_schema_kind,
                        comparison,
                        ChangeClass::Unhandled,
                        ChangeDetails::UnknownDifference,
                    )
                } else {
                    Ok(true)
                }
            }
            (
                openapiv3::SchemaKind::Not { not: old_not },
                openapiv3::SchemaKind::Not { not: new_not },
            ) => {
                let old_not = old_schema_kind.append_deref(old_not.as_ref(), "not");
                let new_not = new_schema_kind.append_deref(new_not.as_ref(), "not");
                self.compare_schema_ref_helper(dry_run, comparison, old_not, new_not)
            }
            (&openapiv3::SchemaKind::Any(old_any), &openapiv3::SchemaKind::Any(new_any)) => {
                if old_any == new_any {
                    Ok(true)
                } else {
                    self.schema_push_change(
                        dry_run,
                        "schema kind 'any' changed",
                        &old_schema_kind,
                        &new_schema_kind,
                        comparison,
                        ChangeClass::Unhandled,
                        ChangeDetails::UnknownDifference,
                    )
                }
            }
            _ => {
                let old_tag = SchemaKindTag::new(&old_schema_kind);
                let new_tag = SchemaKindTag::new(&new_schema_kind);
                self.schema_push_change(
                    dry_run,
                    format!("schema kind changed from {} to {}", old_tag, new_tag),
                    &old_schema_kind,
                    &new_schema_kind,
                    comparison,
                    ChangeClass::Incompatible,
                    ChangeDetails::Datatype,
                )
            }
        }
    }

    pub(crate) fn compare_schema_type(
        &mut self,
        comparison: SchemaComparison,
        dry_run: bool,
        old_schema_type: Contextual<'_, &openapiv3::Type>,
        new_schema_type: Contextual<'_, &openapiv3::Type>,
    ) -> anyhow::Result<bool> {
        match (old_schema_type.as_ref(), new_schema_type.as_ref()) {
            (openapiv3::Type::String(old_string), openapiv3::Type::String(new_string)) => {
                if old_string != new_string {
                    self.schema_push_change(
                        dry_run,
                        "string schema changed",
                        &old_schema_type,
                        &new_schema_type,
                        comparison,
                        ChangeClass::Unhandled,
                        ChangeDetails::UnknownDifference,
                    )
                } else {
                    Ok(true)
                }
            }
            (openapiv3::Type::Number(old_number), openapiv3::Type::Number(new_number)) => {
                if old_number != new_number {
                    self.schema_push_change(
                        dry_run,
                        "number schema changed",
                        &old_schema_type,
                        &new_schema_type,
                        comparison,
                        ChangeClass::Unhandled,
                        ChangeDetails::UnknownDifference,
                    )
                } else {
                    Ok(true)
                }
            }
            (openapiv3::Type::Integer(old_integer), openapiv3::Type::Integer(new_integer)) => {
                if old_integer != new_integer {
                    self.schema_push_change(
                        dry_run,
                        "integer schema changed",
                        &old_schema_type,
                        &new_schema_type,
                        comparison,
                        ChangeClass::Unhandled,
                        ChangeDetails::UnknownDifference,
                    )
                } else {
                    Ok(true)
                }
            }
            (openapiv3::Type::Boolean(old_boolean), openapiv3::Type::Boolean(new_boolean)) => {
                if old_boolean != new_boolean {
                    self.schema_push_change(
                        dry_run,
                        "boolean schema changed",
                        &old_schema_type,
                        &new_schema_type,
                        comparison,
                        ChangeClass::Unhandled,
                        ChangeDetails::UnknownDifference,
                    )
                } else {
                    Ok(true)
                }
            }
            (openapiv3::Type::Array(old_array), openapiv3::Type::Array(new_array)) => self
                .compare_schema_type_array(
                    comparison,
                    dry_run,
                    old_schema_type.subcomponent(old_array),
                    new_schema_type.subcomponent(new_array),
                ),
            (openapiv3::Type::Object(old_object), openapiv3::Type::Object(new_object)) => self
                .compare_schema_type_object(
                    comparison,
                    dry_run,
                    old_schema_type.subcomponent(old_object),
                    new_schema_type.subcomponent(new_object),
                ),
            _ => self.schema_push_change(
                dry_run,
                "schema types changed",
                &old_schema_type,
                &new_schema_type,
                comparison,
                ChangeClass::Incompatible,
                ChangeDetails::UnknownDifference,
            ),
        }
    }

    fn compare_schema_type_array(
        &mut self,
        comparison: SchemaComparison,
        dry_run: bool,
        old_array: Contextual<'_, &openapiv3::ArrayType>,
        new_array: Contextual<'_, &openapiv3::ArrayType>,
    ) -> anyhow::Result<bool> {
        let ArrayType {
            items: old_items,
            min_items: old_min_items,
            max_items: old_max_items,
            unique_items: old_unique_items,
        } = old_array.as_ref();
        let ArrayType {
            items: new_items,
            min_items: new_min_items,
            max_items: new_max_items,
            unique_items: new_unique_items,
        } = new_array.as_ref();

        let mut ret = true;

        if old_min_items != new_min_items {
            ret = false;
            let _ = self.schema_push_change(
                dry_run,
                "array minItems changed",
                &old_array,
                &new_array,
                comparison,
                ChangeClass::Unhandled,
                ChangeDetails::UnknownDifference,
            );
        }

        if old_max_items != new_max_items {
            ret = false;
            let _ = self.schema_push_change(
                dry_run,
                "array maxItems changed",
                &old_array,
                &new_array,
                comparison,
                ChangeClass::Unhandled,
                ChangeDetails::UnknownDifference,
            );
        }

        if old_unique_items != new_unique_items {
            ret = false;
            let _ = self.schema_push_change(
                dry_run,
                "array uniqueItems changed",
                &old_array,
                &new_array,
                comparison,
                ChangeClass::Unhandled,
                ChangeDetails::UnknownDifference,
            );
        }

        match (old_items, new_items) {
            (Some(old_items), Some(new_items)) => {
                let old_item_schema = old_items.clone().unbox();
                let old_items = old_array.append_deref(&old_item_schema, "items");
                let new_item_schema = new_items.clone().unbox();
                let new_items = new_array.append_deref(&new_item_schema, "items");

                ret &= self.compare_schema_ref_helper(dry_run, comparison, old_items, new_items)?;
            }
            (None, None) => {}
            _ => {
                ret = false;
                let _ = self.schema_push_change(
                    dry_run,
                    "array items changed",
                    &old_array,
                    &new_array,
                    comparison,
                    ChangeClass::Unhandled,
                    ChangeDetails::UnknownDifference,
                );
            }
        }

        Ok(ret)
    }

    fn compare_schema_type_object(
        &mut self,
        comparison: SchemaComparison,
        dry_run: bool,
        old_object: Contextual<'_, &openapiv3::ObjectType>,
        new_object: Contextual<'_, &openapiv3::ObjectType>,
    ) -> anyhow::Result<bool> {
        let ObjectType {
            properties: old_properties,
            required: old_required,
            additional_properties: old_additional_properties,
            min_properties: old_min_properties,
            max_properties: old_max_properties,
        } = old_object.as_ref();
        let ObjectType {
            properties: new_properties,
            required: new_required,
            additional_properties: new_additional_properties,
            min_properties: new_min_properties,
            max_properties: new_max_properties,
        } = new_object.as_ref();

        let mut ret = true;

        if old_required != new_required {
            ret = false;
            let _ = self.schema_push_change(
                dry_run,
                "object required properties changed",
                &old_object,
                &new_object,
                comparison,
                ChangeClass::Unhandled,
                ChangeDetails::UnknownDifference,
            );
        }

        if old_min_properties != new_min_properties {
            ret = false;
            let _ = self.schema_push_change(
                dry_run,
                "object minProperties changed",
                &old_object,
                &new_object,
                comparison,
                ChangeClass::Unhandled,
                ChangeDetails::UnknownDifference,
            );
        }

        if old_max_properties != new_max_properties {
            ret = false;
            let _ = self.schema_push_change(
                dry_run,
                "object maxProperties changed",
                &old_object,
                &new_object,
                comparison,
                ChangeClass::Unhandled,
                ChangeDetails::UnknownDifference,
            );
        }

        match (old_additional_properties, new_additional_properties) {
            (
                Some(AdditionalProperties::Schema(old_ap_schema)),
                Some(AdditionalProperties::Schema(new_ap_schema)),
            ) => {
                let old_ap_schema =
                    old_object.append_deref(old_ap_schema.as_ref(), "additionalProperties");
                let new_ap_schema =
                    new_object.append_deref(new_ap_schema.as_ref(), "additionalProperties");

                ret &= self.compare_schema_ref_helper(
                    dry_run,
                    comparison,
                    old_ap_schema,
                    new_ap_schema,
                )?;
            }

            (Some(AdditionalProperties::Any(false)), Some(AdditionalProperties::Any(false))) => {}

            // Equivalent and not worth reporting any change (i.e. absent is
            // effectively equal to `true`)
            (
                None | Some(AdditionalProperties::Any(true)),
                None | Some(AdditionalProperties::Any(true)),
            ) => {}

            _ => {
                ret = false;
                let _ = self.schema_push_change(
                    dry_run,
                    "object additionalProperties changed",
                    &old_object,
                    &new_object,
                    comparison,
                    ChangeClass::Unhandled,
                    ChangeDetails::UnknownDifference,
                );
            }
        }

        let SetCompare {
            a_unique,
            common,
            b_unique,
        } = SetCompare::new(old_properties, new_properties);

        if !a_unique.is_empty() || !b_unique.is_empty() {
            ret = false;
            let _ = self.schema_push_change(
                dry_run,
                "object properties changed",
                &old_object,
                &new_object,
                comparison,
                ChangeClass::Unhandled,
                ChangeDetails::UnknownDifference,
            );
        }

        let old_prop_context = old_object.context().append("properties");
        let new_prop_context = new_object.context().append("properties");
        for (prop_name, (old_prop, new_prop)) in common {
            let old_prop_schema = old_prop.clone().unbox();
            let old_prop = Contextual::new(old_prop_context.append(prop_name), &old_prop_schema);
            let new_prop_schema = new_prop.clone().unbox();
            let new_prop = Contextual::new(new_prop_context.append(prop_name), &new_prop_schema);

            ret &= self.compare_schema_ref_helper(dry_run, comparison, old_prop, new_prop)?;
        }

        Ok(ret)
    }

    fn compare_schema_type_one_of(
        &mut self,
        comparison: SchemaComparison,
        dry_run: bool,
        old_one_of: Contextual<'_, &Vec<ReferenceOr<Schema>>>,
        new_one_of: Contextual<'_, &Vec<ReferenceOr<Schema>>>,
    ) -> anyhow::Result<bool> {
        let old_schemas = old_one_of.as_ref();
        let new_schemas = new_one_of.as_ref();
        if old_schemas.len() != new_schemas.len() {
            return self.schema_push_change(
                dry_run,
                "oneOf schema count changed",
                &old_one_of,
                &new_one_of,
                comparison,
                ChangeClass::Unhandled,
                ChangeDetails::UnknownDifference,
            );
        }

        // TODO we could be resilient to reordering... but aren't for now.

        let mut ret = true;

        for (idx, (old_schema, new_schema)) in
            old_schemas.iter().zip(new_schemas.iter()).enumerate()
        {
            let old_schema = old_one_of.append_deref(old_schema, &idx.to_string());
            let new_schema = new_one_of.append_deref(new_schema, &idx.to_string());
            ret &= self.compare_schema_ref_helper(dry_run, comparison, old_schema, new_schema)?;
        }

        Ok(ret)
    }

    // NOTE: Single-element allOf schemas are flattened by `try_compare_flattened`
    // before reaching this function. Multi-element allOf would require semantic
    // merging for proper comparison, so we just do an equality check.
    fn compare_schema_type_all_of(
        &mut self,
        comparison: SchemaComparison,
        dry_run: bool,
        old_all_of: Contextual<'_, &Vec<ReferenceOr<Schema>>>,
        new_all_of: Contextual<'_, &Vec<ReferenceOr<Schema>>>,
    ) -> anyhow::Result<bool> {
        if old_all_of.as_ref() != new_all_of.as_ref() {
            self.schema_push_change(
                dry_run,
                "unhandled, 'allOf' schema",
                &old_all_of,
                &new_all_of,
                comparison,
                ChangeClass::Unhandled,
                ChangeDetails::UnknownDifference,
            )
        } else {
            Ok(true)
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn schema_push_change(
        &mut self,
        dry_run: bool,
        message: impl ToString,
        old: &dyn ToContext<'_>,
        new: &dyn ToContext<'_>,
        comparison: impl Into<ChangeComparison>,
        class: ChangeClass,
        details: ChangeDetails,
    ) -> anyhow::Result<bool> {
        if !dry_run {
            self.push_change(message, old, new, comparison.into(), class, details);
        }
        Ok(false)
    }
}

#[derive(Clone, Debug)]
enum SchemaKindTag {
    Type,
    OneOf,
    AllOf,
    AnyOf,
    Not,
    Any,
}

impl fmt::Display for SchemaKindTag {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Type => write!(f, "regular type"),
            Self::OneOf => write!(f, "oneOf"),
            Self::AllOf => write!(f, "allOf"),
            Self::AnyOf => write!(f, "anyOf"),
            Self::Not => write!(f, "not"),
            Self::Any => write!(f, "any"),
        }
    }
}

impl SchemaKindTag {
    fn new(kind: &openapiv3::SchemaKind) -> Self {
        match kind {
            openapiv3::SchemaKind::Type(_) => Self::Type,
            openapiv3::SchemaKind::OneOf { .. } => Self::OneOf,
            openapiv3::SchemaKind::AllOf { .. } => Self::AllOf,
            openapiv3::SchemaKind::AnyOf { .. } => Self::AnyOf,
            openapiv3::SchemaKind::Not { .. } => Self::Not,
            openapiv3::SchemaKind::Any { .. } => Self::Any,
        }
    }
}

/// Classification of a schema reference for flattening purposes.
enum SchemaRefKind<'a> {
    /// A bare $ref.
    BareRef,
    /// An inline type (Type, Any, Not).
    ///
    /// It is okay to compare something like Not with single-element wrappers.
    /// When recursing, we'll ensure that the child is also Not.
    InlineType,
    /// A single-element allOf/anyOf/oneOf wrapper that can be flattened to its
    /// inner type.
    SingleElement {
        inner: &'a ReferenceOr<Schema>,
        metadata: &'a SchemaData,
    },
    /// Multi (or, less commonly, zero) element allOf/anyOf/oneOf: cannot be
    /// flattened.
    MultiElement,
}

/// Classify a schema reference for flattening purposes.
fn classify_schema_ref(schema_ref: &ReferenceOr<Schema>) -> SchemaRefKind<'_> {
    match schema_ref {
        ReferenceOr::Reference { .. } => SchemaRefKind::BareRef,
        ReferenceOr::Item(schema) => match &schema.schema_kind {
            openapiv3::SchemaKind::Type(_)
            | openapiv3::SchemaKind::Not { .. }
            | openapiv3::SchemaKind::Any(_) => SchemaRefKind::InlineType,
            openapiv3::SchemaKind::AllOf { all_of } if all_of.len() == 1 => {
                SchemaRefKind::SingleElement {
                    inner: all_of.first().unwrap(),
                    metadata: &schema.schema_data,
                }
            }
            openapiv3::SchemaKind::AnyOf { any_of } if any_of.len() == 1 => {
                SchemaRefKind::SingleElement {
                    inner: any_of.first().unwrap(),
                    metadata: &schema.schema_data,
                }
            }
            openapiv3::SchemaKind::OneOf { one_of } if one_of.len() == 1 => {
                SchemaRefKind::SingleElement {
                    inner: one_of.first().unwrap(),
                    metadata: &schema.schema_data,
                }
            }
            // Multi-element wrappers - not semantically equivalent to single schemas.
            openapiv3::SchemaKind::AllOf { .. }
            | openapiv3::SchemaKind::AnyOf { .. }
            | openapiv3::SchemaKind::OneOf { .. } => SchemaRefKind::MultiElement,
        },
    }
}

/// Compute the effective nullability of a schema reference.
///
/// For single-element wrappers, this is `wrapper.nullable || inner.nullable`.
/// For bare refs and inline types, this is the schema's own nullable flag.
fn get_effective_nullable(
    schema_ref: &Contextual<'_, &ReferenceOr<Schema>>,
) -> anyhow::Result<bool> {
    match schema_ref.as_ref() {
        ReferenceOr::Reference { .. } => {
            // Bare ref: resolve and get the referenced schema's nullable.
            let (resolved, _) = schema_ref.contextual_resolve()?;
            Ok(resolved.schema_data.nullable)
        }
        ReferenceOr::Item(schema) => {
            // For single-element wrappers, effective nullable is wrapper || inner.
            // For everything else, use the schema's own nullable.
            match &schema.schema_kind {
                openapiv3::SchemaKind::AllOf { all_of } if all_of.len() == 1 => {
                    let inner = all_of.first().expect("checked length is 1");
                    let inner_ctx = schema_ref.append_deref(inner, "0");
                    let inner_nullable = get_effective_nullable(&inner_ctx)?;
                    Ok(schema.schema_data.nullable || inner_nullable)
                }
                openapiv3::SchemaKind::AnyOf { any_of } if any_of.len() == 1 => {
                    let inner = any_of.first().expect("checked length is 1");
                    let inner_ctx = schema_ref.append_deref(inner, "0");
                    let inner_nullable = get_effective_nullable(&inner_ctx)?;
                    Ok(schema.schema_data.nullable || inner_nullable)
                }
                openapiv3::SchemaKind::OneOf { one_of } if one_of.len() == 1 => {
                    let inner = one_of.first().expect("checked length is 1");
                    let inner_ctx = schema_ref.append_deref(inner, "0");
                    let inner_nullable = get_effective_nullable(&inner_ctx)?;
                    Ok(schema.schema_data.nullable || inner_nullable)
                }
                _ => Ok(schema.schema_data.nullable),
            }
        }
    }
}

/// Check if schema_data has any non-default values other than nullable.
///
/// Nullable is a semantic property, not metadata, so it should be compared
/// separately with proper compatibility classification.
fn has_meaningful_non_nullable_metadata(data: &SchemaData) -> bool {
    let SchemaData {
        // Nullable is semantic, not metadata.
        nullable: _,
        read_only,
        write_only,
        deprecated,
        external_docs,
        example,
        title,
        description,
        discriminator,
        default,
        extensions,
    } = data;

    *read_only
        || *write_only
        || *deprecated
        || external_docs.is_some()
        || example.is_some()
        || title.is_some()
        || description.is_some()
        || discriminator.is_some()
        || default.is_some()
        || !extensions.is_empty()
}

/// Check if two schema_data values differ in any field other than nullable.
fn has_non_nullable_metadata_diff(old: &SchemaData, new: &SchemaData) -> bool {
    let SchemaData {
        // Nullable is semantic, not metadata.
        nullable: _,
        read_only: old_read_only,
        write_only: old_write_only,
        deprecated: old_deprecated,
        external_docs: old_external_docs,
        example: old_example,
        title: old_title,
        description: old_description,
        discriminator: old_discriminator,
        default: old_default,
        extensions: old_extensions,
    } = old;

    let SchemaData {
        nullable: _,
        read_only: new_read_only,
        write_only: new_write_only,
        deprecated: new_deprecated,
        external_docs: new_external_docs,
        example: new_example,
        title: new_title,
        description: new_description,
        discriminator: new_discriminator,
        default: new_default,
        extensions: new_extensions,
    } = new;

    old_read_only != new_read_only
        || old_write_only != new_write_only
        || old_deprecated != new_deprecated
        || old_external_docs != new_external_docs
        || old_example != new_example
        || old_title != new_title
        || old_description != new_description
        || old_discriminator != new_discriminator
        || old_default != new_default
        || old_extensions != new_extensions
}
