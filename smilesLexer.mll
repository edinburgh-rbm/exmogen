{
  open SmilesParser
}

let digit = ['0'-'9']
let letter = ['a'-'z'] | ['A'-'Z']
let upper_case_letter = ['A'-'Z']
let lower_case_letter = ['a'-'z']

rule token = parse
  | [' ' '\t'] { token lexbuf }
  | "//" [^ '\n']* '\n' { token lexbuf }
  | digit+ as num { INTEGER(num) }
  | "." digit+
  | digit+ "." digit* as num
      { FLOAT (num) }
  | "." { DOT }
  | "(" { OPENP }
  | ")" { CLOSEP }
  | "[" { OPENBRACKET }
  | "]" { CLOSEBRACKET }
  | "{" { OPENBRACE }
  | "}" { CLOSEBRACE }
  | ";" { SEMICOLON }
  | "-" { SINGLE }
  | "=" { DOUBLE }
  | "#" { TRIPLE }
  | "->" { ARROW }
  | "<->" { REVARROW }
  | "+" { PLUS }
  | (upper_case_letter)(letter | upper_case_letter | digit | '_')* as id { IDENT(id)  }
  | (letter | upper_case_letter | digit | '_')* as id { VAR(id)  }
  | _ { token lexbuf }
  | eof { EOF }
