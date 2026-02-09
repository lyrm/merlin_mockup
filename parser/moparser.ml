(*
  let a = 1 + 2 - 1
  let b = a * 10 - a
  let c = b + (4 * a)
*)

let get_source source =
  let file = Stdlib.open_in source in
  let rec loop () =
    match Stdlib.input_line file with
    | line -> line :: loop ()
    | exception End_of_file -> []
  in
  let r = loop () in
  Stdlib.close_in file;
  r

let add_space_before_after char str =
  String.split_on_char char str |> String.concat (" " ^ Char.escaped char ^ " ")

let buffer_to_words str =
  add_space_before_after '(' str
  |> add_space_before_after ')' |> add_space_before_after '='
  |> add_space_before_after '+' |> add_space_before_after '*'
  |> add_space_before_after '-' |> add_space_before_after '/'
  |> String.split_on_char ' '
  |> List.filter (fun elt -> elt <> "")

let split_on_def words =
  let rec loop before acc = function
    | [] -> List.rev before :: acc |> List.rev
    | "let" :: xs -> loop [ "let" ] (List.rev before :: acc) xs
    | hd :: xs -> loop (hd :: before) acc xs
  in
  match words with
  | "let" :: xs -> loop [ "let" ] [] xs
  | _ -> failwith "Bad def"

let buffer_to_words buffer =
  get_source buffer |> String.concat " " |> buffer_to_words |> split_on_def

type token =
  | TLet
  | TAdd
  | TSub
  | TMul
  | TDiv
  | TVar of string
  | TInt of int
  | TEq
  | TParen of token list

let in_par l =
  let rec loop count before = function
    | ")" :: after when count = 0 -> (List.rev before, after)
    | hd :: xs ->
        loop
          (if hd = "(" then count + 1 else if hd = ")" then count - 1 else count)
          (hd :: before) xs
    | [] -> raise Not_found
  in
  loop 0 [] l

let rec lex = function
  | "let" :: xs -> TLet :: lex xs
  | "=" :: xs -> TEq :: lex xs
  | "+" :: xs -> TAdd :: lex xs
  | "*" :: xs -> TMul :: lex xs
  | "-" :: xs -> TSub :: lex xs
  | "/" :: xs -> TDiv :: lex xs
  | "(" :: xs ->
      let inparen, after_paren = in_par xs in
      TParen (lex inparen) :: lex after_paren
  | "\n" :: xs -> lex xs
  | ")" :: _ -> failwith "Unmatched )"
  | " " :: xs -> lex xs
  | str :: xs -> (
      match int_of_string_opt str with
      | Some i -> TInt i :: lex xs
      | None -> TVar str :: lex xs)
  | [] -> []

type typedtree = typed_item list
and typed_item = string * int

type parsedtree = parsed_item list
and parsed_item = string * expr
and expr = Var of string | Int of int | Binop of (op * expr * expr)
and op = Div | Add | Sub | Mul

type env = (string * int) list ref

module Parser = struct
  let op_from_token = function
    | TAdd -> Add
    | TSub -> Sub
    | TDiv -> Div
    | TMul -> Mul
    | _ -> failwith "no binop"

  let rec term = function
    | TInt i -> Int i
    | TVar v -> Var v
    | TParen inparen -> expr inparen
    | _ -> failwith "Not a  valid term"

  and expr = function
    | left :: ((TAdd | TSub) as op) :: right ->
        let right = expr right in
        let left = term left in
        Binop (op_from_token op, left, right)
    | [ left; ((TMul | TDiv) as op); right ] ->
        Binop (op_from_token op, term left, term right)
    | left :: ((TMul | TDiv) as op) :: right :: xs ->
        expr (TParen [ left; op; right ] :: xs)
    | TVar s :: [] -> Var s
    | TInt i :: [] -> Int i
    | TParen inparen :: [] -> expr inparen
    | _ -> failwith "Not a valid expression"

  let def def =
    match def with
    | TLet :: TVar v :: TEq :: e -> (v, expr e)
    | _ -> failwith "Not a valid definition"
end

let eval_item env (name, expr) =
  let rec eval env = function
    | Int i -> i
    | Var v -> (
        match List.assoc_opt v env with
        | None -> failwith ("Var " ^ v ^ " unknown.")
        | Some ev -> ev)
    | Binop (op, l, r) ->
        let op_int =
          match op with
          | Add -> Int.add
          | Mul -> Int.mul
          | Sub -> Int.sub
          | Div -> Int.div
        in
        let lv = eval env l in
        let rv = eval env r in
        op_int lv rv
  in
  (name, eval env expr)

let name count =
  let power_26 = [| 1; 26; 676; 17576; 456976 |] in
  let rec loop pow =
    if count / power_26.(pow + 1) = 0 then pow else loop (pow + 1)
  in
  let max_pow = loop 0 in
  let rec loop curr_pow curr_count =
    if curr_pow < 0 then []
    else
      let m = curr_count / power_26.(curr_pow) in
      m :: loop (curr_pow - 1) (curr_count - (26 * m))
  in
  loop max_pow count
  |> List.map (fun c -> c + 97 |> Char.chr |> Char.escaped)
  |> String.concat ""

let rename defs =
  defs := List.mapi (fun i (_, v) -> (name i, v)) (List.rev !defs)

let pp_def ppf (x, y) = Format.fprintf ppf "Value of \"%s\" is %d.\n" x y

(** {[
      "test/test2" |> buffer_to_words |> List.map lexer |> List.map parse_def
      |> List.fold_left (fun env (name, def) -> eval env (name, def) :: env) []
    ]} *)

let domain_name () =
  if Domain.is_main_domain () then "[Main]" else "      [Typer]"

let log lvl (fmt : ('a, Format.formatter, unit, unit) format4) : 'a =
  if lvl > 0 then (
    Format.eprintf "%s " (domain_name ());
    Format.eprintf (fmt ^^ "\n%!"))
  else Format.ifprintf Format.std_formatter fmt

let print env =
  log 0 "@[<hov>%a@]@."
    Format.(pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf "") pp_def)
    !env

let to_string env =
  Format.asprintf "@[<hov>%a@]@."
    Format.(pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf "") pp_def)
    !env

let parse buf = buffer_to_words buf |> List.map lex |> List.map Parser.def
