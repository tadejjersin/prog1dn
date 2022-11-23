type available = { loc : int * int; possible : int list }


(* TODO: tip stanja ustrezno popravite, saj boste med reševanjem zaradi učinkovitosti
   želeli imeti še kakšno dodatno informacijo *)
type state = { problem : Model.problem; current_grid : int option Model.grid; available : available}

let print_state (state : state) : unit =
  Model.print_grid
    (function None -> "?" | Some digit -> string_of_int digit)
    state.current_grid

type response = Solved of Model.solution | Unsolved of state | Fail of state

(* Zamenja element na mestu (x, y) v sudokuju z elementom x *)
let replace_element_in_grid (grid : int option Model.grid) (loc : int * int) (x : int) = 
 let row_ind = fst loc in
  let col_ind = snd loc in
  let _ = grid.(row_ind).(col_ind) <- Some x in 
  grid
      

(* Funkcija za določanje naslednjega mesta v mreži, če se premikamo najprej po prvi vrstici, potem po drugi... *)
let next_loc (loc : int * int) = 
  match fst loc, snd loc with
  | 8, 8 -> (8, 8)
  | x, 8 -> ((x + 1), 0)
  | x, y -> (x, (y+1))
      

(* Vrne state, s trenutno lokacijo na naslednjem praznem mestu *)
let rec find_next_empty (state : state) : state = 
  let row_ind = fst state.available.loc in
  let col_ind = snd state.available.loc in
  match state.current_grid.(row_ind).(col_ind) with 
    | None -> state
    | _ -> find_next_empty ( { current_grid = state.current_grid; 
      problem = state.problem; 
      available = { 
      loc = next_loc state.available.loc; 
      possible = [1;2;3;4;5;6;7;8;9] } } )


let get_t_values (state : state) (x: int) (t : (int * int) list) = 
  List.map (fun y -> if state.available.loc = y then (Some x) else state.current_grid.(fst y).(snd y)) t

let check_if_x_fits_in_t (state : state) (x : int) = 
  let rec is_sorted = function 
      | [] -> true
      | y :: [] -> true
      | y :: y' :: ys -> if y < y' then is_sorted (y' :: ys) else false
  in
  (List.filter (List.mem state.available.loc) state.problem.t) |>
  List.map (get_t_values state x) |>
  List.map (List.filter Option.is_some) |>
  List.map is_sorted


(* Preveri, če x lahko vstavimo na (a, b) mesto v sudokuju *)
let check_if_ok (state : state) (x : int) : bool =
  let row_ind = fst state.available.loc in
  let col_ind = snd state.available.loc in 
  let box_ind = 3 * (row_ind / 3) + (col_ind / 3) in
  if 
    Array.mem (Some x) (Model.get_row state.current_grid (row_ind)) ||
    Array.mem (Some x) (Model.get_column state.current_grid (col_ind)) ||
    Array.mem (Some x) (Model.get_box state.current_grid (box_ind)) ||
    List.mem false (check_if_x_fits_in_t state x)
  then false
  else true

let initialize_state (problem : Model.problem) : state = 
  {
  current_grid = Model.copy_grid problem.initial_grid; 
  problem = problem; 
  available = { 
    loc = (0, 0); 
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
  let state' = find_next_empty state in
  (* Poišče prvo število od 1 do 9, ki ga lahko vstavimo na trenutno mesto v sudokuju 
    in ga vrne skupaj s preostankom seznama *)
  let rec first_ok_element (list : int list) : (int * (int list)) option = 
    if list = [] then None else (
      let head = List.hd list in 
      let tail = List.tl list in 
      match check_if_ok state' head with
       | false -> first_ok_element tail
       | true -> Some (head, tail)
    )
  in 
  (* Prva možnost na trenutno mesto vstavi število, ki ga dobimo iz first_ok_element in 
     se premakne na naslednje prazno mesto, druga pa na trenutnem mestu od možnosti za 
     možne elemente odstrani zgoraj omenjeno število. Če je seznam možnih števil prazen 
     vrne None. *)
  match first_ok_element state'.available.possible with 
    | None -> None
    | Some (x, xs) -> Some (
      { current_grid = replace_element_in_grid state'.current_grid state'.available.loc x; 
        problem = state'.problem; 
        available = { 
        loc = next_loc state'.available.loc; 
        possible = [1;2;3;4;5;6;7;8;9] } },
      { current_grid = (Model.copy_grid state'.current_grid); 
        problem = state'.problem; 
        available = { 
        loc = state'.available.loc; 
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
