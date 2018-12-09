import os


print("""
/*
Uses Testcases from https://github.com/nst/JSONTestSuite/

MIT License

Copyright (c) 2016 Nicolas Seriot

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/
""")

print("use yalrjson::Parser;")

for file in os.listdir("test_parsing"):
    if not file[0] in ['y', 'n', 'i']:
        continue

    data = ""

    try:
        with open("test_parsing/" + file, 'r') as myfile:
            data=myfile.read().replace('\n', '')
    except UnicodeDecodeError:
        # Ignore unicode decode errors as those are out of scope for our project
        continue

    file = file[:-1] \
        .replace("-", "dash") \
        .replace("+", "plus") \
        .replace(".", "dot") \
        .replace("#", "hashtag")

    if file.startswith("y"):
        print("""#[test]
fn {}() {{
    assert!(Parser::parse_str(r#"{}"#).is_ok());
}}
        """.format(file[:-5], data))
    elif file.startswith("n"):
        print("""#[test]
fn {}() {{
    assert!(Parser::parse_str(r#"{}"#).is_err());
}}
        """.format(file, data))
    elif file.startswith("i"):
        print("""#[test]
fn {}() {{
    Parser::parse_str(r#"{}"#);
}}
        """.format(file, data))

