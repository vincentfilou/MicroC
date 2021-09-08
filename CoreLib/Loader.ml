let load input =
  let lexbuf = (Lexing.from_channel input) in
      (Parser.instruction Lexer.token lexbuf)

let load_file f = load (open_in f)