{
  open Parser

  exception Error of string

}

(*
    # TODO: Error if whitespace includes newline
    NO_NEWLINE ::= ;
    # TODO: quasiliterals aka template literals
    QUASI_ALL ::= ${() => FAIL};
    QUASI_HEAD ::= ${() => FAIL};
    QUASI_MID ::= ${() => FAIL};
    QUASI_TAIL ::= ${() => FAIL};
 *)

    (* Omit "async", "arguments", and "eval" from IDENT in TinySES even
       though ES2017 considers them in IDENT. *)
let KEYWORD =
      "break"
    | "case" | "catch" | "const" | "continue"
    | "debugger" | "default"
    | "else" | "export"
    | "finally" | "for" | "function"
    | "if" | "import"
    | "return"
    | "switch"
    | "throw" | "try" | "typeof"
    | "void"
    | "while"

    (* Unused by TinySES but enumerated here, in order to omit them
       from the IDENT token. *)
let RESERVED_KEYWORD =
      "class"
    | "delete" | "do"
    | "extends"
    | "in" | "instanceof"
    | "new"
    | "super"
    | "this"
    | "var"
    | "with"
    | "yield"

let FUTURE_RESERVED_WORD =
      "await" | "enum"
    | "implements" | "package" | "protected"
    | "interface" | "private" | "public"

let RESERVED_WORD =
      KEYWORD | RESERVED_KEYWORD | FUTURE_RESERVED_WORD
    | "null" | "false" | "true"
    | "async" | "arguments" | "eval"

(* This rule looks for a single line, terminated with '\n' or eof.
   It returns a pair of an optional string (the line that was found)
   and a Boolean flag (false if eof was reached). *)
rule line = parse
| ([^'\n']* '\n') as line
    (* Normal case: one line, no eof. *)
    { Some line, true }
| eof
    (* Normal case: no data, eof. *)
    { None, false }
| ([^'\n']+ as line) eof
    (* Special case: some data but missing '\n', then eof.
       Consider this as the last line, and add the missing '\n'. *)
    { Some (line ^ "\n"), false }

and token = parse
| "null" { NULL }
| "true" { TRUE }
| "false" { FALSE }
| eof
    { EOF }
| _
    { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }
