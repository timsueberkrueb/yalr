use yalrjson::Parser;

fn main() {
    let json = r#"{
        "a": [true, false, null, [1, 2, 3], -1.3e+7],
        "b": "Test\t"
    }"#;
    println!("{:#?}", Parser::parse_str(json) )

}