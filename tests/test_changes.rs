// Copyright 2025 Oxide Computer Company

use std::fmt::Write;

use drift::compare;
use similar::TextDiff;

#[test]
fn test_change() {
    // Iterate through the directory named "cases" and find each subdirectory.
    let cases_dir = std::path::Path::new("tests/cases");
    for entry in std::fs::read_dir(cases_dir).unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();
        if path.is_dir() {
            let case_name = path.file_name().unwrap().to_str().unwrap();
            println!("Running case: {}", case_name);

            // Load the directory named "base.json"
            let base_path = path.join("base.json");
            let base_content = std::fs::read_to_string(&base_path).unwrap();
            let base_value: serde_json::Value = serde_json::from_str(&base_content).unwrap();

            // Start by making sure that the base compares cleanly against
            // itself.
            let result = compare(&base_value, &base_value).expect("fatal failure in comparison");
            assert!(result.is_empty());

            // Iterate through every file in the "patch" subdirectory.
            let patch_dir = path.join("patch");
            let output_dir = path.join("output");
            for patch_entry in std::fs::read_dir(&patch_dir).unwrap() {
                let patch_entry = patch_entry.unwrap();
                println!("  Considering patch {:?}", patch_entry.file_name());
                let patch_path = patch_entry.path();
                if patch_path.is_file() {
                    let patch_content = std::fs::read_to_string(&patch_path).unwrap();
                    let patch_value: Vec<_> = serde_json::from_str(&patch_content).unwrap();

                    let mut patched = base_value.clone();
                    json_patch::patch(&mut patched, &patch_value).unwrap();

                    let udiff = {
                        let mut base_pretty = serde_json::to_string_pretty(&base_value).unwrap();
                        base_pretty.push('\n');
                        let mut patched_pretty = serde_json::to_string_pretty(&patched).unwrap();
                        patched_pretty.push('\n');
                        let diff = TextDiff::from_lines(&base_pretty, &patched_pretty);
                        let patch_name = patch_entry.file_name();

                        // Format the unified diff manually, replacing `@@ -N,M
                        // +N,M @@` hunk headers with bare `@@` to avoid churn
                        // when base.json changes shift line positions.
                        let mut out = String::new();
                        let mut first = true;
                        for hunk in diff.unified_diff().iter_hunks() {
                            if first {
                                writeln!(out, "--- {}", patch_name.to_string_lossy()).unwrap();
                                writeln!(out, "+++ patched").unwrap();
                                first = false;
                            }
                            writeln!(out, "@@").unwrap();
                            for change in hunk.iter_changes() {
                                write!(out, "{}{}", change.tag(), change.to_string_lossy())
                                    .unwrap();
                            }
                        }
                        out
                    };

                    let result =
                        compare(&base_value, &patched).expect("fatal failure in comparison");

                    let output = format!("{udiff}\n\nResult for patch:\n{:#?}\n", result);
                    let mut output_path = output_dir.join(patch_entry.file_name());
                    output_path.set_extension("out");
                    expectorate::assert_contents(output_path, &output);
                }
            }
        }
    }
}
