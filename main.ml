open Parsing;;
open Lexing;;

open Lambda;;
open Parser;;
open Lexer;;


let check_line line = 
  try
    String.rindex_from line ((String.rindex line ';')-1)  ';'; true
  with
    Not_found -> false 
    
  ;;
let rec read_line_loop string =
  let line = read_line() in 
    if check_line line then String.sub (string ^ " " ^ line) 0 (String.length(string ^ " " ^ line)-2)  else string ^ " " ^ read_line_loop (line)
  ;;

let top_level_loop () =
  print_endline "Evaluator of lambda expressions...";
  let rec loop (vctx, tctx) =
    print_string ">> ";
    flush stdout; 
    try
      let c = s token (from_string (read_line_loop "")) in
      loop (execute (vctx, tctx) c)
    with
       Lexical_error ->
         print_endline "lexical error";
         loop (vctx, tctx)
     | Parse_error ->
         print_endline "syntax error";
         loop (vctx, tctx)
     | Type_error e ->
         print_endline ("type error: " ^ e);
         loop (vctx, tctx)
     | End_of_file ->
         print_endline "...bye!!!"
  in
    loop (emptyctx, emptyctx)
  ;;
top_level_loop ()
;;

