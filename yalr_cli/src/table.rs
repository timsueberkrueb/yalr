use std::error::Error;
use std::fs::File;

use prettytable as pt;
use prettytable::cell;
use prettytable::row;

use crate::parse;
use yalr_codegen::EnumVariant;

pub fn print_table(input_filename: &str, impl_type: Option<&str>) -> Result<(), Box<dyn Error>> {
    let parse_table = parse::generate_parse_table(input_filename, impl_type)?;
    let pretty_table = generate_pretty_table(&parse_table);
    println!("{}", pretty_table);
    Ok(())
}

pub fn write_table_csv(
    input_filename: &str,
    impl_type: Option<&str>,
    csv_filename: &str,
) -> Result<(), Box<dyn Error>> {
    let parse_table = parse::generate_parse_table(input_filename, impl_type)?;
    let pretty_table = generate_pretty_table(&parse_table);
    let csv_file = File::create(csv_filename)?;
    pretty_table.to_csv(csv_file)?;
    Ok(())
}

fn generate_pretty_table(
    parse_table: &yalr_core::ParseTable<EnumVariant, EnumVariant>,
) -> pt::Table {
    let mut table = pt::Table::new();

    let mut title_row = row!["#", "LALR item closure", "Lookahead"];

    for t in parse_table
        .grammar
        .terminals
        .iter()
        .map(|t| format!("{}", t))
    {
        title_row.add_cell(cell!(t));
    }

    for n in parse_table
        .grammar
        .nonterminals
        .iter()
        .map(|n| format!("{}", n))
    {
        title_row.add_cell(cell!(n));
    }

    table.add_row(title_row);

    for (i, state) in parse_table.states.iter().enumerate() {
        let (items, lookaheads): (Vec<_>, Vec<_>) = state
            .item_closure
            .iter()
            .map(|item| (item.augmented_rule_string(), item.lookahead_string()))
            .unzip();

        let mut row = row![i, items.join("\n"), lookaheads.join("\n")];

        for t in parse_table.grammar.terminals.iter() {
            row.add_cell(cell![state
                .action_map
                .get(t)
                .map_or("".to_owned(), |a| format!("{:?}", a))]);
        }

        for n in parse_table.grammar.nonterminals.iter() {
            row.add_cell(cell![state
                .goto_map
                .get(n)
                .map_or("".to_owned(), |idx| format!("Goto({})", idx))]);
        }

        table.add_row(row);
    }

    table
}
