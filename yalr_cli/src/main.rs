extern crate proc_macro;

use std::error::Error;
use std::process;

use clap::{App, AppSettings, Arg, SubCommand};

mod graphviz;
mod parse;
mod table;

fn main() {
    if let Err(err) = cli() {
        eprintln!("Error: {}", err);
        process::exit(1);
    }
}

fn cli() -> Result<(), Box<dyn Error>> {
    let matches = App::new("yalr_cli")
        .about("Tool for inspecting LALR parsers made with YALR")
        .version(env!("CARGO_PKG_VERSION"))
        .subcommand(
            SubCommand::with_name("table")
                .arg(
                    Arg::with_name("file")
                        .help("Rust source file containing YALR impl block")
                        .required(true),
                ).arg(
                    Arg::with_name("impl")
                        .long("--impl")
                        .takes_value(true)
                        .help("Rust type with a YALR impl block to inspect"),
                ).arg(
                    Arg::with_name("csv")
                        .long("--csv")
                        .takes_value(true)
                        .help("Write the parse table to a specified CSV file"),
                ).about("Prints the LALR parse table of a YALR parser"),
        ).subcommand(
            SubCommand::with_name("graph")
                .arg(
                    Arg::with_name("file")
                        .help("Rust source file containing YALR impl block")
                        .required(true),
                ).arg(
                    Arg::with_name("impl")
                        .long("--impl")
                        .takes_value(true)
                        .help("Rust type with a YALR impl block to inspect"),
                ).arg(
                    Arg::with_name("output")
                        .long("--output")
                        .short("-o")
                        .takes_value(true)
                        .help("Write the generated graphviz graph to a file (*.dot)"),
                ).about("Outputs a graphviz graph showing the LALR states of a YALR parser"),
        ).setting(AppSettings::ArgRequiredElseHelp)
        .get_matches();

    if let Some(table_opts) = matches.subcommand_matches("table") {
        let filename = table_opts.value_of("file").unwrap();
        let impl_type: Option<&str> = table_opts.value_of("impl");
        let csv_file: Option<&str> = table_opts.value_of("csv");

        if let Some(csv_filename) = csv_file {
            table::write_table_csv(filename, impl_type, csv_filename)?;
        } else {
            table::print_table(filename, impl_type)?;
        }
    }

    if let Some(graph_opts) = matches.subcommand_matches("graph") {
        let filename = graph_opts.value_of("file").unwrap();
        let impl_type: Option<&str> = graph_opts.value_of("impl");
        let output_file: Option<&str> = graph_opts.value_of("output");

        if let Some(output_filename) = output_file {
            graphviz::write_graphviz_graph(filename, impl_type, output_filename)?;
        } else {
            graphviz::show_graphviz_graph(filename, impl_type)?;
        }
    }

    Ok(())
}
