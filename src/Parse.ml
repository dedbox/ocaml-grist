open Angstrom

type tree = Atom of string | List of tree list [@@deriving show]

let symbol : string t =
  take_while1 (function
    | '!'
    | '$' .. '&'
    | '*'
    | '+' .. '/'
    | '0' .. '9'
    | ':'
    | '<' .. '@'
    | 'A' .. 'Z'
    | '^' | '_'
    | 'a' .. 'z'
    | '~' ->
        true
    | _ -> false)

let space : string t = take_while1 (function ' ' | '\t' -> true | _ -> false)

let break : string t = end_of_line *> space <|> space

let eol : unit t = end_of_line <|> end_of_input

let atom : tree t =
  fix (fun atom ->
      (let+ x = symbol in
       Atom x)
      <|> let+ xs = char '(' *> sep_by break atom <* char ')' in
          List xs)

let tree : tree t =
  (let* x = atom <* break in
   let+ xs = sep_by1 break atom in
   List (x :: xs))
  <|> atom

let exps : tree list t =
  (let+ _ = end_of_input in
   [])
  <|> let* x = tree <* end_of_line in
      let+ xs = sep_by end_of_line tree in
      x :: xs

(* Parsers *)

let line (str : string) : (tree, string) result =
  parse_string ~consume:Prefix tree str

let lines (str : string) : (tree list, string) result =
  parse_string ~consume:All exps str

(* Unit Tests *)

let check_line (str : string) (want : tree) : bool =
  match line str with
  | Ok got ->
      (* "Got: " ^ show_exp got |> print_endline; *)
      got = want
  | Error str ->
      "Error" ^ str |> print_endline;
      false

let check_lines (str : string) (want : tree list) : bool =
  match lines str with
  | Ok got ->
      (* "Got: " ^ (List.map show_exp got |> String.concat "; ") |> print_endline; *)
      got = want
  | Error str ->
      "Error" ^ str |> print_endline;
      false

let%test "atom word" = Atom "abc" |> check_line "abc"

let%test "atom number" = Atom "123" |> check_line "123"

let%test "list atoms" = List [ Atom "abc"; Atom "123" ] |> check_line "abc 123"

let%test "list break" =
  List [ Atom "abc"; Atom "123" ] |> check_line "abc\n 123"

let%test "lists" =
  List [ Atom "ab"; List [ Atom "cd"; Atom "ef" ]; Atom "gh" ]
  |> check_line "ab (cd ef) gh"

let%test "lines" =
  [ List [ Atom "ab"; Atom "cd" ]; List [ Atom "ef"; Atom "gh" ] ]
  |> check_lines "ab cd\nef gh"

let%test "lines break" =
  [ List [ Atom "ab"; Atom "cd" ]; List [ Atom "ef"; Atom "gh" ] ]
  |> check_lines "ab\n cd\nef\n gh"
