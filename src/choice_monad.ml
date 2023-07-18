open Choice_monad_intf

module List = struct
  type vbool = Sym_value.S.vbool

  type thread = Thread.t

  type 'a t = thread -> ('a * thread) list

  let return (v : 'a) : 'a t = fun t -> [ (v, t) ]

  let bind (v : 'a t) (f : 'a -> 'b t) : 'b t =
   fun t ->
    let lst = v t in
    match lst with
    | [] -> []
    | [ (r, t) ] -> (f r) t
    | _ -> List.flatten @@ List.map (fun (r, t) -> (f r) t) lst

  let select (sym_bool : vbool) : bool t =
   fun state ->
    let solver = Thread.solver state in
    let pc = Thread.pc state in
    let mem = Thread.mem state in
    let sym_bool = Encoding.Expression.simplify sym_bool in
    match sym_bool with
    | Val (Bool b) -> [ (b, state) ]
    | Val (Num (I32 _)) -> assert false
    | _ -> (
      let no = Sym_value.S.Bool.not sym_bool in
      let sat_true = Thread.Solver.check_sat solver (sym_bool :: pc) in
      let sat_false = Thread.Solver.check_sat solver (no :: pc) in
      match (sat_true, sat_false) with
      | false, false -> []
      | true, false -> [ (true, state) ]
      | false, true -> [ (false, state) ]
      | true, true ->
        Format.printf "CHOICE: %s@." (Encoding.Expression.to_string sym_bool);
        let state1 =
          { state with pc = sym_bool :: pc; mem = Sym_memory.M.clone mem }
        in
        let state2 =
          { state with pc = no :: pc; mem = Sym_memory.M.clone mem }
        in
        [ (true, state1); (false, state2) ] )

  let select_i32 _sym_int = assert false

  let trap : Trap.t -> 'a t = function
    | Out_of_bounds_table_access -> assert false
    | Out_of_bounds_memory_access -> assert false
    | Integer_overflow -> assert false
    | Integer_divide_by_zero -> assert false
    | Unreachable -> fun _ -> []

  (* raise (Types.Trap "out of bounds memory access") *)

  let with_thread (f : thread -> 'b) : 'b t = fun t -> [ (f t, t) ]

  let add_pc (c : Sym_value.S.vbool) : unit t =
   fun t -> [ ((), { t with pc = c :: t.pc }) ]

  let run (v : 'a t) (thread : thread) =
    v thread
end

module Test_list :
  Complete with type thread := Thread.t and module V := Sym_value.S =
  List
