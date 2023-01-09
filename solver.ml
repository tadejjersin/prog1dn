type available = { loc : int; possible : int list }


(* TODO: tip stanja ustrezno popravite, saj boste med reševanjem zaradi učinkovitosti
   želeli imeti še kakšno dodatno informacijo *)
type state = { problem : Model.problem; 
              current_grid : int option Model.grid; 
              starting_empty_squares : (int * int) array;
              available : available}

let print_state (state : state) : unit =
  Model.print_grid
    (function None -> "?" | Some digit -> string_of_int digit)
    state.current_grid

type response = Solved of Model.solution | Unsolved of state | Fail of state

(* Zamenja element na mestu (a, b) v sudokuju z elementom x *)
let replace_element_in_grid (grid : int option Model.grid) (loc : int * int) (x : int) = 
 let row_ind = fst loc in
  let col_ind = snd loc in
  grid.(row_ind).(col_ind) <- Some x;
  grid
      

(* Funkcija za določanje naslednjega mesta v mreži, če se premikamo najprej po prvi vrstici, potem po drugi... *)
(* let next_loc (loc : int * int) = 
  match fst loc, snd loc with
  | 8, 8 -> (8, 8)
  | x, 8 -> ((x + 1), 0)
  | x, y -> (x, (y+1)) *)
      

(* Vrne state, s trenutno lokacijo na naslednjem praznem mestu *)
(*
let rec find_next_empty (state : state) : state = 
  let row_ind = fst state.available.loc in
  let col_ind = snd state.available.loc in
  match state.current_grid.(row_ind).(col_ind) with 
    | None -> state
    | _ -> find_next_empty ( { current_grid = state.current_grid; 
      problem = state.problem; 
      starting_empty_squares = state.starting_empty_squares;
      available = { 
      loc = next_loc state.available.loc; 
      possible = [1;2;3;4;5;6;7;8;9] } } ) *)


let get_t_values (state : state) (x: int) (t : (int * int) list) = 
  let current_loc = state.starting_empty_squares.(state.available.loc) in 
  List.map (fun y -> if current_loc = y then (Some x) else state.current_grid.(fst y).(snd y)) t

let check_if_x_fits_in_t (state : state) (x : int) = 
  let current_loc = state.starting_empty_squares.(state.available.loc) in 
  let rec is_sorted = function 
      | [] -> true
      | y :: [] -> true
      | y :: y' :: ys -> if y < y' then is_sorted (y' :: ys) else false
  in
  (List.filter (List.mem current_loc) state.problem.t) |>
  List.map (get_t_values state x) |>
  List.map (List.filter Option.is_some) |>
  List.map is_sorted

let get_k_values (state : state) (k : int * (int * int) list) = 
  (fst k, List.map (fun y -> state.current_grid.(fst y).(snd y)) (snd k))

let check_if_x_fits_in_k (state : state) (x : int) =
  let current_loc = state.starting_empty_squares.(state.available.loc) in 
  let myfun k = 
    if List.mem (Some x) (snd k) then false 
    else 
      let k_sum = List.filter Option.is_some (snd k) |> List.map Option.get |> List.fold_left (+) 0 in 
      let k_full_len = List.length (snd k) in 
      let k_len = (List.filter Option.is_some (snd k) |> List.length) + 1 in 
      if (k_sum + x) > fst k then false
      else if k_len < k_full_len && (k_sum + x) < (fst k) then true 
      else if (k_sum + x) = fst k then true 
      else false
  in 
  List.filter (fun y -> List.mem current_loc (snd y)) state.problem.k |>
  List.map (get_k_values state) |>
  List.map (myfun)

let get_a_values (state : state) (a : (int * int) * (int * int) list) = 
  (state.current_grid.(fst (fst a)).(snd (fst a)),
  List.map (fun y -> state.current_grid.(fst y).(snd y)) (snd a))

let check_if_x_fits_in_a (state : state) (x : int) =
  let current_loc = state.starting_empty_squares.(state.available.loc) in 
  let myfun a = 
    let a_values = get_a_values state a in
    let right_sum = List.filter Option.is_some (snd a_values) |> List.map Option.get |> List.fold_left (+) 0 in 
    let left_num = if Option.is_none (fst a_values) then 0 else Option.get (fst a_values) in 
    let a_full_len = List.length (snd a) in 
    let a_len = (List.filter Option.is_some (snd a_values) |> List.length) + 1 in 
    if current_loc = (fst a) then (if a_full_len = a_len - 1 && x != right_sum then false else true) 
    else if (a_full_len = a_len && (right_sum + x) != left_num && left_num != 0) || (right_sum + x > 9) then false else true
  in   
  List.filter (fun y -> List.mem current_loc (snd y) || fst y = current_loc) state.problem.a |>
  List.map (myfun) 

(* Preveri, če x lahko vstavimo na trenutno mesto v sudokuju *)
let check_if_ok (state : state) (x : int) : bool =
  let current_loc = state.starting_empty_squares.(state.available.loc) in 
  let row_ind = fst current_loc in
  let col_ind = snd current_loc in 
  let box_ind = 3 * (row_ind / 3) + (col_ind / 3) in
  if 
    Array.mem (Some x) (Model.get_row state.current_grid (row_ind)) ||
    Array.mem (Some x) (Model.get_column state.current_grid (col_ind)) ||
    Array.mem (Some x) (Model.get_box state.current_grid (box_ind)) ||
    List.mem false (check_if_x_fits_in_t state x) ||
    List.mem false (check_if_x_fits_in_k state x) ||
    List.mem false (check_if_x_fits_in_a state x)
  then false
  else true

let replace_element_and_go_to_next_loc (state : state) (x : int): state = 
  let current_loc = state.starting_empty_squares.(state.available.loc) in 
  { current_grid = replace_element_in_grid state.current_grid current_loc x; 
        problem = state.problem; 
        starting_empty_squares = state.starting_empty_squares;
        available = { 
        loc = state.available.loc + 1; 
        possible = [1;2;3;4;5;6;7;8;9] } }

let get_empty_squares (problem : Model.problem) = 
  let rec get_empty_squares_from_row acc row row_ind i=
    if i = 9 then List.rev acc
    else if row.(i) = None then get_empty_squares_from_row ((row_ind, i) :: acc) row row_ind (i+1) 
    else get_empty_squares_from_row acc row row_ind (i+1) 
  in 
  let rec aux acc grid i = 
    if i = 9 then acc 
    else aux ((get_empty_squares_from_row [] grid.(i) i 0) @ acc) grid (i+1)
  in 
  aux [] problem.initial_grid 0 |> Array.of_list 
  (* let sqaure_list = aux [] problem.initial_grid 0 in 
  let get_square_availabilty (x, y) = 
    let box_ind = 3 * (x / 3) + (y / 3) in
    [1;2;3;4;5;6;7;8;9] 
    |> List.filter (fun a -> (Array.mem (Some a) (Model.get_row problem.initial_grid x) = false))
    |> List.filter (fun a -> (Array.mem (Some a) (Model.get_column problem.initial_grid y) = false))
    |> List.filter (fun a -> (Array.mem (Some a) (Model.get_box problem.initial_grid box_ind) = false))
    |> List.length
  in 
  let rec aux' acc lst = 
    match lst with
    | [] -> acc
    | x :: xs -> aux' (((get_square_availabilty x), x) :: acc) xs
  in 
  let max_ind = aux' [] sqaure_list |> List.sort compare |> List.hd |> snd in 
  let rec aux'' acc lst x = 
    match lst with 
    | [] -> List.rev acc
    | y :: ys when y = x -> y :: ys @ aux'' acc [] x 
    | y :: ys -> aux'' (y :: acc) ys x
  in 
  aux'' [] sqaure_list max_ind |> Array.of_list *)


(* slovarji *)

module IntPairs =
       struct
         type t = int * int
         let compare (x0,y0) (x1,y1) =
           match Stdlib.compare x0 x1 with
             | 0 -> Stdlib.compare y0 y1
             | c -> c
       end

module PairsDict = Map.Make(IntPairs)

let get_optimal_path (problem : Model.problem) = 
  let empty_squares = get_empty_squares problem |> Array.to_list in 
  let basic_value = 10 in 
  let term_value = 25 in
  let arrow_value = 30 in 
  let get_square_availabilty (x, y) = 
    let box_ind = 3 * (x / 3) + (y / 3) in
    let basic = 9 - ([1;2;3;4;5;6;7;8;9] 
    |> List.filter (fun a -> (Array.mem (Some a) (Model.get_row problem.initial_grid x) = false))
    |> List.filter (fun a -> (Array.mem (Some a) (Model.get_column problem.initial_grid y) = false))
    |> List.filter (fun a -> (Array.mem (Some a) (Model.get_box problem.initial_grid box_ind) = false))
    |> List.length) in 
    let term = problem.t |> List.filter (List.mem (x, y)) |> List.length in 
    let arrow = problem.a |> List.filter (fun arrow -> (x, y) = fst arrow || List.mem (x, y) (snd arrow)) |> List.length in
    basic_value * basic + term_value * term + arrow_value * arrow
  in 
  let rec aux acc = function
    | [] -> acc
    | x :: xs -> aux (PairsDict.add x (get_square_availabilty x) acc) xs
  in
  let availability_dict = aux PairsDict.(empty) empty_squares in 
  let remove_duplicates_from_list lst = 
    let remove_el xs x = if List.mem x xs then xs else x :: xs in 
    List.fold_left remove_el [] lst
  in 
  let rec get_key_with_largest_availability dict = 
    let compare' (a, b) (a', b') = 
      if b > b' || (b = b' && a > a') then 1 
      else if a = a' && b = b' then 0
      else -1
    in 
    PairsDict.bindings dict |> List.sort compare' |> List.rev |> List.hd |> fst
  in 
  let update_value dict key value = 
    if PairsDict.find_opt key dict = None then dict
    else PairsDict.add key (PairsDict.find key dict + value) dict 
  in 
  let keys_to_update (x, y) = 
    List.init 9 (fun a -> (a, y)) 
    @ List.init 9 (fun a -> (x, a))
    @ List.init 9 (fun a -> (3 * (x / 3) + a / 3, y - y mod 3 + a mod 3)) 
    |> remove_duplicates_from_list
  in 
  let keys_to_update_term (x, y) = 
    problem.t |> List.filter (List.mem (x, y)) |> List.flatten |> remove_duplicates_from_list
  in 
  let keys_to_update_arrow (x, y) = 
    problem.a |> List.filter (fun arrow -> (x, y) = fst arrow || List.mem (x, y) (snd arrow)) 
    |> List.map (fun arrow -> fst arrow :: (snd arrow)) |> List.flatten |> remove_duplicates_from_list 
  in
  let rec update_keys dict key_list value = 
    match key_list with
      | [] -> dict
      | x :: xs -> update_keys (update_value dict x value) xs value
  in 
  let rec optimal_path acc dict = 
    if dict = PairsDict.(empty) then List.rev acc
    else
      let k = get_key_with_largest_availability dict in 
      let k_list = keys_to_update k in 
      let k_list_term = keys_to_update_term k in
      let k_list_arrow = keys_to_update_arrow k in
      optimal_path (k :: acc) (PairsDict.(
        update_keys dict k_list basic_value 
        |> (fun d -> update_keys d k_list_term term_value) 
        |> (fun d-> update_keys d k_list_arrow arrow_value)
        |> remove k)) 
  in
  optimal_path [] availability_dict |> Array.of_list



let initialize_state (problem : Model.problem) : state = 
  let path = get_optimal_path problem in 
  {
  current_grid = Model.copy_grid problem.initial_grid; 
  problem = problem; 
  starting_empty_squares = path;
  available = { 
    loc = 0; 
    possible = [1;2;3;4;5;6;7;8;9] }
  } 

let validate_state (state : state) : response =
  let unsolved =
    Array.exists (Array.exists Option.is_none) state.current_grid
  in
  if unsolved then Unsolved state
  else
    (* Option.get ne bo sprožil izjeme, ker so vse vrednosti v mreži oblike Some x *)
    let solution = Model.map_grid Option.get state.current_grid in
    if Model.is_valid_solution state.problem solution then Solved solution
    else Fail state

let branch_state (state : state) : (state * state) option =
  (* TODO: Pripravite funkcijo, ki v trenutnem stanju poišče hipotezo, glede katere
     se je treba odločiti. Če ta obstaja, stanje razveji na dve stanji:
     v prvem predpostavi, da hipoteza velja, v drugem pa ravno obratno.
     Če bo vaš algoritem najprej poizkusil prvo možnost, vam morda pri drugi
     za začetek ni treba zapravljati preveč časa, saj ne bo nujno prišla v poštev. *)  
  (* let state' = find_next_empty state in *)
  (* Poišče prvo število od 1 do 9, ki ga lahko vstavimo na trenutno mesto v sudokuju 
    in ga vrne skupaj s preostankom seznama *)
  let rec first_ok_element (list : int list) : (int * (int list)) option = 
    if list = [] then None else (
      let head = List.hd list in 
      let tail = List.tl list in 
      match check_if_ok state head with
       | false -> first_ok_element tail
       | true -> Some (head, tail)
    )
  in 
  (* Prva možnost na trenutno mesto vstavi število, ki ga dobimo iz first_ok_element in 
     se premakne na naslednje prazno mesto, druga pa na trenutnem mestu od možnosti za 
     možne elemente odstrani zgoraj omenjeno število. Če je seznam možnih števil prazen 
     vrne None. *)
  match first_ok_element state.available.possible with 
    | None -> None
    | Some (x, xs) -> Some (
      replace_element_and_go_to_next_loc state x,
      { current_grid = (Model.copy_grid state.current_grid); 
        problem = state.problem; 
        starting_empty_squares = state.starting_empty_squares;
        available = { 
        loc = state.available.loc; 
        possible = xs } }
    )

(* pogledamo, če trenutno stanje vodi do rešitve *)
let rec solve_state (state : state) =
  (* uveljavimo trenutne omejitve in pogledamo, kam smo prišli *)
  (* TODO: na tej točki je stanje smiselno počistiti in zožiti možne rešitve *)

  match validate_state state with
  | Solved solution ->
      (* če smo našli rešitev, končamo *)
      Some solution
  | Fail fail ->
      (* prav tako končamo, če smo odkrili, da rešitev ni *)
      None
  | Unsolved state' ->
      (* če še nismo končali, raziščemo stanje, v katerem smo končali *)
      explore_state state'

and explore_state (state : state) =
  (* pri raziskovanju najprej pogledamo, ali lahko trenutno stanje razvejimo *)
  match branch_state state with
  | None ->
      (* če stanja ne moremo razvejiti, ga ne moremo raziskati *)
      None
  | Some (st1, st2) -> (
      (* če stanje lahko razvejimo na dve možnosti, poizkusimo prvo *)
      match solve_state st1 with
      | Some solution ->
          (* če prva možnost vodi do rešitve, do nje vodi tudi prvotno stanje *)
          Some solution
      | None ->
          (* če prva možnost ne vodi do rešitve, raziščemo še drugo možnost *)
          solve_state st2 )

let solve_problem (problem : Model.problem) =
  problem |> initialize_state |> solve_state

