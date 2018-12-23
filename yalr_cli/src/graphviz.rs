use std::error::Error;
use std::fs;
use std::io::Write;
use tempfile::NamedTempFile;

use yalr_codegen::{Nonterminal, Terminal};

use crate::parse;

pub fn write_graphviz_graph(
    input_filename: &str,
    impl_type: Option<&str>,
    output_filename: &str,
) -> Result<(), Box<dyn Error>> {
    let parse_table = parse::generate_parse_table(input_filename, impl_type)?;
    let graphviz_string = render_graphviz_graph(&parse_table);
    fs::write(output_filename, graphviz_string)?;
    Ok(())
}

pub fn show_graphviz_graph(filename: &str, impl_type: Option<&str>) -> Result<(), Box<dyn Error>> {
    let parse_table = parse::generate_parse_table(filename, impl_type)?;
    let graphviz_string = render_graphviz_graph(&parse_table);
    // We need the tempfile filename in order to open it with an associated application
    let mut temp_file = NamedTempFile::new()?;
    let path: String = temp_file.path().to_str().unwrap().to_owned() + ".dot";
    write!(temp_file, "{}", graphviz_string)?;
    temp_file.persist(&path)?;
    open::that(&path)?;
    Ok(())
}

fn render_graphviz_graph(parse_table: &yalr_core::ParseTable<Terminal, Nonterminal>) -> String {
    let mut lines = Vec::new();
    lines.push("digraph lalr_states {".to_owned());
    for (state_idx, state) in parse_table.states.iter().by_ref().enumerate() {
        // Create graphviz box with table for the state
        let table_rows: Vec<String> = state
            .item_closure
            .iter()
            .by_ref()
            .map(|item| {
                let mut la_string = item.lookahead_string();
                if item.is_pos_at_end() {
                    // Use underline to mark a reduce action
                    la_string = format!("<U>{}</U>", la_string);
                } else if la_string == "" {
                    // No lookahead, there may as well be unicorns, okay?!
                    la_string = "🦄".to_owned();
                }
                format!(
                    "      <TR><TD>{}</TD><TD>{}</TD></TR>",
                    item.augmented_rule_string(),
                    la_string
                )
            })
            .collect();
        let table_row_string = table_rows.join("\n");
        let table_head = format!(
            "      <TR><TD><B>State #{}</B></TD><TD><B>Lookahead</B></TD></TR>",
            state_idx
        );
        let line = format!(
            r#"  State{} [shape=plain label=<
    <TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0">
{}
{}
    </TABLE>
  >];"#,
            state_idx, table_head, table_row_string
        );
        lines.push(line);
        // Create state transitions
        for (nonterminal, other_state_idx) in state.goto_map.iter() {
            lines.push(format!(
                r#"  State{} -> State{} [label="{}"];"#,
                state_idx, other_state_idx, nonterminal
            ))
        }
        for (terminal, action) in state.action_map.iter() {
            if let yalr_core::Action::Shift(other_state_idx) = action {
                lines.push(format!(
                    r#"  State{} -> State{} [label="{}"];"#,
                    state_idx, other_state_idx, terminal
                ))
            }
        }
    }
    lines.push("}".to_owned());
    lines.join("\n")
}
