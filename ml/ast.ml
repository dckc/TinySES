
type expression =
  Literal of value
| Use of string
| ArrayExpr of expression list
| ObjectExpr of property list
| FunctionExpr of pattern option * pattern list * block
| ExprHole (* ISSUE: really? *)
| Quasi of (string, expression) either list
| Get of expression * string
| Index of expression * expression
| Call of expression * expression list
| Tag of expression * expression
| GetLater of expression * string
| IndexLater of expression * expression
| CallLater of expression * expression list
| Assign of op * expression * expression
| Arrow of pattern list * block
| Lambda of pattern list * expression
| Unary of unOp * expression
| Binary of op * expression * expression (* TODO: sum type for BinaryOp *)
| SpreadExpr of expression (* ISSUE: really? *)
and op = string (* ISSUE *)
and unOp = Void | TypeOf | Positive | Negative | Not
and value =
  Null
| Bool of bool
| Number of float (* double? *)
| String of string
and ('a, 'b) either = Left of 'a | Right of 'b
and property =
  Prop of propName * expression
| SpreadObj of expression
| MethodDef of propName * pattern list * block
| Getter of propName * pattern list * block
| Setter of propName * pattern list * block
and propName =
  PropKey of string
| PropIx of float
and propParam =
RestObj of pattern
| MatchProp of propName * pattern
| OptionalProp of string * string * expression (* ISSUE: k2? types? *)
and pattern =
Def of string
| MatchData of value
| MatchArray of pattern list
| Rest of pattern (* ISSUE: really *)
| Optional of pattern * expression (* ISSUE: really?? *)
| MatchObj of propParam list
| PatternHole

and statement =
  ExprStmt of expression
| Block of block
| Return of expression option
| Break of string option
| Continue of string option
| Throw of expression
and block = (statement, declaration) either list

and declaration =
Const of (pattern * expression) list
| Let of (pattern * expression) list
| FunctionDecl of pattern * pattern list * block
