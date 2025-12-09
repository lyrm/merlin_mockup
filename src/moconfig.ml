type completion = All | Part of int
type config = { source : string; completion : completion }

let available_files = List.init 3 (fun i -> "test/test" ^ string_of_int i)
let test_one = [ { source = "test/test1"; completion = All } ]

let test_cancel =
  [
    { source = "test/test1"; completion = Part 4 };
    { source = "test/test2"; completion = All };
  ]

let test_partial =
  [
    { source = "test/test1"; completion = Part 4 };
    { source = "test/test1"; completion = Part 15 };
  ]

let test_cache =
  [
    { source = "test/test1"; completion = Part 1 };
    { source = "test/test1"; completion = Part 3 };
    { source = "test/test1"; completion = Part 12 };
    { source = "test/test1"; completion = Part 20 };
    { source = "test/test1"; completion = Part 10 };
  ]

(** Previous request is cancelled is the new request uses a different source
    file. *)
let prev_is_cancelled prev curr =
  match (prev, curr) with
  | None, _ -> false
  | Some { source = s1; _ }, { source = s2; _ } when s1 = s2 -> false
  | _ -> true
