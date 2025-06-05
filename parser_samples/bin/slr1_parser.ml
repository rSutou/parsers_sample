module LR1Lang = 
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

    (* 2 + 3 * 4 * 5 + 6 *)
    let sample_code = [NUM 2; PLUS; NUM 3; TIMES; NUM 4; TIMES; NUM 5; PLUS; NUM 6; EOF]

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

module LR1Parser =
  struct
    exception Parse_error of string
    open LR1Lang
    open Lexer.Lexer(LR1Lang)

    let rec back (state:int list) (reads:cst list) (len:int): int list * cst list * cst list =
      if len = 0 then (state,reads,[])
      else match state,reads with
      | _::st,cst::rt -> let (st',rt',csts') = back st rt (len-1) in (st',rt',cst::csts')
      | _ -> raise (Parse_error "Parse error: over reduce")



    let reduce (state:int list) (reads:cst list) (rule:int) : int list * cst list =
      let (st,rt,cst) =
        (match rule with
        | 1 -> let (st,rt,csts) = back state reads 2 in (st,rt,NT(Start,List.rev csts))
        | 2 -> let (st,rt,csts) = back state reads 1 in (st,rt,NT(Expr,List.rev csts))
        | 3 -> let (st,rt,csts) = back state reads 3 in (st,rt,NT(Arith,List.rev csts))
        | 4 -> let (st,rt,csts) = back state reads 1 in (st,rt,NT(Arith,List.rev csts))
        | 5 -> let (st,rt,csts) = back state reads 3 in (st,rt,NT(Term,List.rev csts))
        | 6 -> let (st,rt,csts) = back state reads 1 in (st,rt,NT(Term,List.rev csts))
        | _ -> raise (Parse_error "Parse error: reduce by unexpected rule_id"))
      in
      match st with
      | [] -> raise (Parse_error "Parse error:lost state")
      | s::_ -> 
      match s, cst with
      | 1, NT(Expr,_) -> (3::st,cst::rt)
      | 1, NT(Arith,_) -> (4::st,cst::rt)
      | 1, NT(Term,_) -> (5::st,cst::rt)
      | 8, NT(Term,_) -> (9::st,cst::rt)
      | _ -> raise (Parse_error "Parse error: fail to shift after redeced")

    let rec step (state:int list) (reads:cst list) =
      match state with
      | [] -> raise (Parse_error "Parse error:lost state")
      | s::_ -> 
      match s, top() with
      | 3,EOF -> NT(Start,List.rev (T EOF:: reads))
      | 2,PLUS | 2,TIMES | 2,EOF -> let (st,rt) = reduce state reads 6 in step st rt
      | 4,EOF -> let (st,rt) = reduce state reads 2 in step st rt
      | 5,PLUS | 5,EOF -> let (st,rt) = reduce state reads 4 in step st rt
      | 7,PLUS | 7,TIMES | 7,EOF -> let (st,rt) = reduce state reads 5 in step st rt
      | 9,PLUS | 9,EOF -> let (st,rt) = reduce state reads 3 in step st rt
      | 1,NUM _ -> step (2::state) (pop(NUM 0)::reads)
      | 4,PLUS -> step (8::state) (pop(PLUS)::reads)
      | 5,TIMES -> step (6::state) (pop(TIMES)::reads)
      | 6,NUM _ -> step (7::state) (pop(NUM 0)::reads)
      | 8,NUM _ -> step (2::state) (pop(NUM 0)::reads)
      | 9,TIMES -> step (6::state) (pop(TIMES)::reads)
      | _,t -> raise (Parse_error ("Parse_error : unexpexted token "^ string_of_token t))

    let main (ts:token list) = 
      reset ts;
      step [1] []

  end