use std::error::Error;

use character_comp::ids::parse_ids_str;

fn main() -> Result<(), Box<dyn Error>> {
    for line_result in std::io::stdin().lines() {
        let line = line_result?;
        if line.starts_with(";;") {
        } else {
            let toks = line.split("\t").collect::<Vec<_>>();
            let (_code, _root, ids_str) = match &toks[0..3] {
                [code, root, ids_str] => (code, root, ids_str),
                _ => {
                    eprintln!("LINE Can't parse: '{}'", line);
                    continue;
                }
            };
            if let None = parse_ids_str(ids_str) {
                eprintln!("Can't parse: '{:?}'", toks);
                eprintln!("\tIn IDS: '{}'", ids_str);
            }
        }
    }

    Ok(())
}
