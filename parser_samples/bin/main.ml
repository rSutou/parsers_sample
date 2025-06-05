open Ll0_parser
let _ = LL0Parser.main LL0Lang.sample_code |> LL0Lang.string_of_cst |> print_endline

open Ll1_parser
let _ = LL1Parser.main LL1Lang.sample_code |> LL1Lang.string_of_cst |> print_endline
let _ = LL1Parser.main LL1Lang.sample_code2 |> LL1Lang.string_of_cst |> print_endline
let _ = LL1Parser.main LL1Lang.sample_code3 |> LL1Lang.string_of_cst |> print_endline

open Lr0_parser
let _ = LR0Parser.main LR0Lang.sample_code |> LR0Lang.string_of_cst |> print_endline


open Slr1_parser
let _ = SLR1Parser.main SLR1Lang.sample_code |> SLR1Lang.string_of_cst |> print_endline

