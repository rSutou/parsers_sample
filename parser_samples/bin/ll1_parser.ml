module LL1Lang = 
  struct
    type token =
    | NUM of int
    | PLUS
    | TIMES
    | EOF
    type nt =
    | Start
    | Expr
    | Arith
    | Term
    type cst =
    | NT of (nt * cst list)
    | T of token

    let eof = EOF

    (* (+ ( * ( * 1 2 ) 3 ) ( * 4 5 )) *)
    let sample_code = [PLUS; TIMES; TIMES; NUM 1; NUM 2; NUM 3; TIMES; NUM 4; NUM 5; EOF]
    (* ( * 1 ( * 2 3 )) *)
    let sample_code2 = [TIMES; NUM 1; TIMES; NUM 2; NUM 3; EOF]
    (* empty EOF *)
    let sample_code3 = [EOF]

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
    | Arith -> "Arith"
    | Term -> "Term"
    let rec string_of_cst : cst -> string = function
    | NT (nt, csts) 
      -> "(" ^ string_of_nt nt ^ ", [" ^ 
        (List.map (string_of_cst) csts |> String.concat ";") ^
        "])"
    | T t -> string_of_token t
  end

module LL1Parser = 
  struct
  exception Parse_error of string
    open LL1Lang
    open Lexer.Lexer(LL1Lang)
    let rec start () =
      match top () with
      | PLUS | TIMES | NUM _ | EOF -> 
        let res = [] in
        let res = expr()::res in
        let res = pop(EOF)::res in
        let res = List.rev res in
        NT(Start,res)
    and expr () =
      match top () with
      | PLUS -> 
        let res = [] in
        let res = arith()::res in
        let res = List.rev res in
        NT(Expr,res)
      | TIMES | NUM _ -> 
        let res = [] in
        let res = term()::res in
        let res = List.rev res in
        NT(Expr,res)
      | EOF -> 
        let res = [] in
        let res = List.rev res in
        NT(Expr,res)
    and arith () =
      match top () with
      | PLUS -> 
        let res = [] in
        let res = pop(PLUS)::res in
        let res = term()::res in
        let res = term()::res in
        let res = List.rev res in
        NT(Arith,res)
      | t -> raise (Parse_error ("Parse_error : unexpexted token "^ string_of_token t))
    and term () =
      match top () with
      | TIMES -> 
        let res = [] in
        let res = pop(TIMES)::res in
        let res = term()::res in
        let res = term()::res in
        let res = List.rev res in
        NT(Arith,res)
      | NUM _ -> 
        let res = [] in
        let res = pop(NUM 0)::res in
        let res = List.rev res in
        NT(Arith,res)
      | t -> raise (Parse_error ("Parse_error : unexpexted token "^ string_of_token t))

    let main (ts:token list) = 
      reset ts;
      start ()

  end