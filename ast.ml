open Core.Std
open Async.Std

type pos = int * int with sexp

type 'a program =
  | Program of 'a * 'a use list * 'a callable list

and 'a use =
  | Use of 'a * 'a id

and 'a callable =
  | Func of 'a * 'a id * 'a annnotated_var list * 'a typ list * 'a stmt
  | Proc of 'a * 'a id * 'a annnotated_var list * 'a stmt

and 'a id =
  | Id of 'a * string

and 'a annnotated_var =
  | AnnotatedId of 'a * 'a id * 'a typ
  | AnnotatedUnderscore of 'a * 'a typ

and 'a var =
  | AnnotatedVar of 'a annnotated_var
  | Underscore of 'a

and 'a call =
  | Call of 'a id * 'a expr list

and 'a stmt =
  | Decl of 'a * 'a var list
  | DeclAsgn of 'a * 'a var list * 'a expr
  | Asgn of 'a * 'a expr * 'a expr
  | Block of 'a * 'a stmt list * 'a expr list option
  | If of 'a * 'a expr * 'a stmt
  | IfElse of 'a * 'a expr * 'a stmt * 'a stmt
  | While of 'a * 'a expr * 'a stmt
  | StmtCall of 'a call

and binop_code =
  | MINUS
  | STAR
  | HIGHMULT
  | DIV
  | MOD
  | PLUS
  | LT
  | LTE
  | GTE
  | GT
  | EQEQ
  | NEQ
  | AMP
  | BAR

and unop_code =
  | UMINUS
  | BANG

and 'a literal =
  | NumLiteral of 'a * int
  | BoolLiteral of 'a * bool
  | StringLiteral of 'a * string
  | CharLiteral of 'a * char
  | ArrayLiteral of 'a * 'a expr list

and 'a expr =
  | Literal of 'a literal
  | ExprId of 'a id
  | BinOp of 'a * binop_code * 'a expr * 'a expr
  | UnOp of unop_code * 'a expr
  | Index of 'a * 'a expr * 'a expr
  | Length of 'a * 'a expr
  | ExprCall of 'a call

and 'a typ =
  | Int of 'a
  | Bool of 'a
  | Array of 'a * 'a typ * 'a expr option
with sexp

let main filenames () : unit Deferred.t =
  Deferred.List.iter filenames ~f:(fun filename ->
    Reader.load_sexp_exn filename (program_of_sexp pos_of_sexp) >>| fun p ->
    print_endline (Sexp.to_string (sexp_of_program sexp_of_pos p))
  )

let () =
  Command.async
    ~summary:""
    Command.Spec.(
      empty
      +> anon (sequence ("FILE" %: file))
    )
    main
  |> Command.run
