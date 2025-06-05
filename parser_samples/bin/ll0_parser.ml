module LL0Lang = 
  struct
    type token =
    | NUM of int
    | PLUS
    | TIMES
    | EOF
    type nt =
    | Start
    | Expr
    type cst =
    | NT of (nt * cst list)
    | T of token

    let eof = EOF

    (* 2 * 3 + 4 * 5 *)
    let sample_code = [NUM 2; TIMES; NUM 3; PLUS; NUM 4; TIMES; NUM 5; EOF]

    let token_match t1 t2 =
      match t1,t2 with
      | NUM _, NUM _ -> true
      | _ -> t1 = t2
    let string_of_token : token -> string = function
    | NUM n -> "NUM(" ^ string_of_int n ^ ")"
    | PLUS -> "PLUS"
    | TIMES -> "TIMES"
    | EOF -> "EOF"
    let string_of_nt : nt -> string = function
    | Start -> "Start"
    | Expr -> "Expr"
    let rec string_of_cst : cst -> string = function
    | NT (nt, csts) 
      -> "(" ^ string_of_nt nt ^ ", [" ^ 
        (List.map (string_of_cst) csts |> String.concat ";") ^
        "])"
    | T t -> string_of_token t
  end

module LL0Parser = 
  struct
    open LL0Lang
    open Lexer.Lexer(LL0Lang)
    let rec start () =
      let res = [] in
      let res = expr()::res in
      let res = pop(PLUS)::res in
      let res = expr()::res in
      let res = pop(EOF)::res in
      let res = List.rev res in
      NT(Start,res)
    and expr () =
      let res = [] in
      let res = pop(NUM 0)::res in
      let res = pop(TIMES)::res in
      let res = pop(NUM 0)::res in
      let res = List.rev res in
      NT(Expr,res)

    let main (ts:token list) = 
      reset ts;
      start ()
  end