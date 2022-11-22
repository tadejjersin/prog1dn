(* Pomožni tip, ki predstavlja mrežo *)

type 'a grid = 'a Array.t Array.t

(* Funkcije za prikaz mreže.
   Te definiramo najprej, da si lahko z njimi pomagamo pri iskanju napak. *)

(* Razbije seznam [lst] v seznam seznamov dolžine [size] *)
let chunkify size lst =
  let rec aux chunk chunks n lst =
    match (n, lst) with
    | _, [] when chunk = [] -> List.rev chunks
    | _, [] -> List.rev (List.rev chunk :: chunks)
    | 0, _ :: _ -> aux [] (List.rev chunk :: chunks) size lst
    | _, x :: xs -> aux (x :: chunk) chunks (n - 1) xs
  in
  aux [] [] size lst

let string_of_list string_of_element sep lst =
  lst |> List.map string_of_element |> String.concat sep

let string_of_nested_list string_of_element inner_sep outer_sep =
  string_of_list (string_of_list string_of_element inner_sep) outer_sep

let string_of_row string_of_cell row =
  let string_of_cells =
    row |> Array.to_list |> chunkify 3
    |> string_of_nested_list string_of_cell "" "│"
  in
  "┃" ^ string_of_cells ^ "┃\n"

let print_grid string_of_cell grid =
  let ln = "───" in
  let big = "━━━" in
  let divider = "┠" ^ ln ^ "┼" ^ ln ^ "┼" ^ ln ^ "┨\n" in
  let row_blocks =
    grid |> Array.to_list |> chunkify 3
    |> string_of_nested_list (string_of_row string_of_cell) "" divider
  in
  Printf.printf "┏%s┯%s┯%s┓\n" big big big;
  Printf.printf "%s" row_blocks;
  Printf.printf "┗%s┷%s┷%s┛\n" big big big

(* Funkcije za dostopanje do elementov mreže *)

let get_row (grid : 'a grid) (row_ind : int) = 
  Array.copy grid.(row_ind)

let rows grid = List.init 9 (get_row grid)

let get_column (grid : 'a grid) (col_ind : int) =
  Array.init 9 (fun row_ind -> grid.(row_ind).(col_ind))

let columns grid = List.init 9 (get_column grid)

let get_box (grid : 'a grid) (box_ind : int) = 
  Array.init 9 (fun x -> grid.(3 * (box_ind / 3) + (x / 3)).(3 * (box_ind mod 3) + (x mod 3)))

let boxes grid = List.init 9 (get_box grid)

(* Funkcije za ustvarjanje novih mrež *)

let map_grid (f : 'a -> 'b) (grid : 'a grid) : 'b grid = 
  Array.init 9 (fun row_ind -> Array.init 9 (fun col_ind -> f (grid.(row_ind).(col_ind))))

let copy_grid (grid : 'a grid) : 'a grid = map_grid (fun x -> x) grid

let foldi_grid (f : int -> int -> 'a -> 'acc -> 'acc) (grid : 'a grid)
    (acc : 'acc) : 'acc =
  let acc, _ =
    Array.fold_left
      (fun (acc, row_ind) row ->
        let acc, _ =
          Array.fold_left
            (fun (acc, col_ind) cell ->
              (f row_ind col_ind cell acc, col_ind + 1))
            (acc, 0) row
        in
        (acc, row_ind + 1))
      (acc, 0) grid
  in
  acc

let row_of_string cell_of_char str =
  List.init (String.length str) (String.get str) |> List.filter_map cell_of_char

let grid_of_string cell_of_char str =
  let grid =
    str |> String.split_on_char '\n'
    |> List.map (row_of_string cell_of_char)
    |> List.filter (function [] -> false | _ -> true)
    |> List.map Array.of_list |> Array.of_list
  in
  if Array.length grid <> 9 then failwith "Nepravilno število vrstic";
  if Array.exists (fun x -> x <> 9) (Array.map Array.length grid) then
    failwith "Nepravilno število stolpcev";
  grid

(* Funkcije za branje razširitev *)

let get_string_of_grid_from_string str = 
  String.sub str 0 357

(* Funkcija za branje posameznih vrstic termometrov, puščic in kletk. Vrne seznam
  stringov le-teh. *)
let get_expansion_from_string (str : string) (c : char) = 
  let rec aux acc i =
    let starting_index = String.index_from_opt str i c in 
    if starting_index = None then acc 
    else if str.[(Option.get starting_index) + 1] = ':' then 
      let end_index = String.index_from str (Option.get starting_index) '\n' in
      let str_len = end_index - (Option.get starting_index + 3) in
      aux ((String.sub str ((Option.get starting_index) + 3) str_len) :: acc) end_index
    else  
      aux acc ((Option.get starting_index) + 1)  
  in 
  aux [] 0 

(* Vrne seznam termometrov, ki so seznami parov int*int. *)
let get_t (st : string) = 
  let helper str =
    str |> String.split_on_char ';' |>
    List.map (fun s -> (int_of_char s.[1] - 48, int_of_char s.[3] - 48))
  in 
  List.map helper (get_expansion_from_string st 'T')

(* Vrne seznam puščic *)
let get_a (st : string) = 
  let helper str =
    let a_head = (int_of_char str.[1] - 48, int_of_char str.[3] - 48) in 
    let a_tail = String.sub str 9 ((String.length str) - 9) in 
    let a_tail' = a_tail |> String.split_on_char ';' |>
    List.map (fun s -> (int_of_char s.[1] - 48, int_of_char s.[3] - 48)) in 
    (a_head, a_tail')
  in 
  List.map helper (get_expansion_from_string st 'A')

(* Vrne seznam kletk *)
let get_k (st : string) = 
  let helper str = 
    let border_index = String.index_from str 0 ' ' in 
    let k_value = int_of_string (String.sub str 0 border_index) in 
    let k_tail = String.sub str (border_index + 1) (String.length str - border_index - 1) in 
    let k_tail' = k_tail |> String.split_on_char ';' |>
    List.map (fun s -> (int_of_char s.[1] - 48, int_of_char s.[3] - 48)) in 
    (k_value, k_tail')
  in 
  List.map helper (get_expansion_from_string st 'K')


(* Model za vhodne probleme *)

type problem = { initial_grid : int option grid; 
  t : (int * int) list list; 
  a : ((int * int) * (int * int) list) list;
  k : (int * (int * int) list) list }

let print_problem problem : unit = 
  let string_of_element = function
    | None -> "?"
    | Some x -> string_of_int x
  in
  print_grid string_of_element problem

let problem_of_string str =
  let cell_of_char = function
    | ' ' -> Some None
    | c when '1' <= c && c <= '9' -> Some (Some (Char.code c - Char.code '0'))
    | _ -> None
  in
  { initial_grid = grid_of_string cell_of_char (get_string_of_grid_from_string str);
    t = get_t str;
    a = get_a str;
    k = get_k str }

(* Model za izhodne rešitve *)

type solution = int grid

let print_solution solution = print_grid string_of_int solution

let is_valid_solution (problem : problem) (solution : solution) = 
  let nums = [1;2;3;4;5;6;7;8;9] in
  let valid_part f = 
    List.for_all (
      fun box -> List.for_all (
        fun num -> Array.mem num box
        ) 
        nums
        ) 
        (f solution)
  in
  (valid_part rows) && (valid_part columns) && (valid_part boxes)
