open Syntax
module Expr = Encoding.Expr
module Value = Symbolic_value
module Choice = Symbolic.P.Choice
open Hc

let symbolic_extern_module :
  Symbolic.P.Extern_func.extern_func Link.extern_module =
  let sym_cnt = Atomic.make 0 in
  let symbol ty () : Value.int32 Choice.t =
    let id = Atomic.fetch_and_add sym_cnt 1 in
    let sym = Format.kasprintf (Encoding.Symbol.mk_symbol ty) "symbol_%d" id in
    let sym_expr = Expr.mk_symbol sym in
    Choice.with_thread (fun thread ->
        thread.symbol_set <- sym :: thread.symbol_set;
        match ty with
        | Ty_bitv S8 -> Expr.(Cvtop (ExtU 24, sym_expr) @: Ty_bitv S32)
        | _ -> sym_expr )
  in
  let assume_i32 (i : Value.int32) : unit Choice.t =
    let c = Value.I32.to_bool i in
    Choice.add_pc c
  in
  let assume_positive_i32 (i : Value.int32) : unit Choice.t =
    let c = Value.I32.ge i Value.I32.zero in
    Choice.add_pc c
  in
  let assert_i32 (i : Value.int32) : unit Choice.t =
    let c = Value.I32.to_bool i in
    Choice.assertion c
  in
  (* we need to describe their types *)
  let functions =
    [ ( "i8_symbol"
      , Symbolic.P.Extern_func.Extern_func
          (Func (UArg Res, R1 I32), symbol (Ty_bitv S8)) )
    ; ( "i32_symbol"
      , Symbolic.P.Extern_func.Extern_func
          (Func (UArg Res, R1 I32), symbol (Ty_bitv S32)) )
    ; ( "i64_symbol"
      , Symbolic.P.Extern_func.Extern_func
          (Func (UArg Res, R1 I64), symbol (Ty_bitv S64)) )
    ; ( "f32_symbol"
      , Symbolic.P.Extern_func.Extern_func
          (Func (UArg Res, R1 F32), symbol (Ty_fp S32)) )
    ; ( "f64_symbol"
      , Symbolic.P.Extern_func.Extern_func
          (Func (UArg Res, R1 F64), symbol (Ty_fp S64)) )
    ; ( "assume"
      , Symbolic.P.Extern_func.Extern_func
          (Func (Arg (I32, Res), R0), assume_i32) )
    ; ( "assume_positive_i32"
      , Symbolic.P.Extern_func.Extern_func
          (Func (Arg (I32, Res), R0), assume_positive_i32) )
    ; ( "assert"
      , Symbolic.P.Extern_func.Extern_func
          (Func (Arg (I32, Res), R0), assert_i32) )
    ]
  in
  { functions }

let summaries_extern_module :
  Symbolic.P.Extern_func.extern_func Link.extern_module =
  let open Expr in
  let i32 v =
    match v.node.e with
    | Val (Num (I32 v)) -> v
    | _ -> Log.err {|alloc: cannot allocate base pointer "%a"|} Expr.pp v
  in
  let ptr v =
    match v.node.e with
    | Ptr (b, _) -> b
    | _ -> Log.err {|free: cannot fetch pointer base of "%a"|} Expr.pp v
  in
  let abort () : unit Choice.t = Choice.add_pc @@ Value.Bool.const false in
  let alloc (base : Value.int32) (size : Value.int32) : Value.int32 Choice.t =
    let base : int32 = i32 base in
    Choice.with_thread (fun t ->
        let memories = Thread.memories t in
        Symbolic_memory.iter
          (fun tbl ->
            Symbolic_memory.ITbl.iter
              (fun _ (m : Symbolic_memory.t) ->
                Symbolic_memory.replace_size m base size )
              tbl )
          memories;
        Ptr (base, Value.const_i32 0l) @: Ty_bitv S32 )
  in
  let free (p : Value.int32) : unit Choice.t =
    let base = ptr p in
    Choice.with_thread (fun t ->
        let memories = Thread.memories t in
        Symbolic_memory.iter
          (fun tbl ->
            Symbolic_memory.ITbl.iter
              (fun _ (m : Symbolic_memory.t) -> Symbolic_memory.free m base)
              tbl )
          memories )
  in
  let functions =
    [ ( "alloc"
      , Symbolic.P.Extern_func.Extern_func
          (Func (Arg (I32, Arg (I32, Res)), R1 I32), alloc) )
    ; ( "dealloc"
      , Symbolic.P.Extern_func.Extern_func (Func (Arg (I32, Res), R0), free) )
    ; ("abort", Symbolic.P.Extern_func.Extern_func (Func (UArg Res, R0), abort))
    ]
  in
  { functions }

let ( let*/ ) (t : 'a Result.t) (f : 'a -> 'b Result.t Choice.t) :
  'b Result.t Choice.t =
  match t with Error e -> Choice.return (Error e) | Ok x -> f x

let simplify_then_link_then_run ~unsafe ~optimize (pc : unit Result.t Choice.t)
  file =
  let link_state = Link.empty_state in
  let link_state =
    Link.extern_module' link_state ~name:"symbolic"
      ~func_typ:Symbolic.P.Extern_func.extern_type symbolic_extern_module
  in
  let link_state =
    Link.extern_module' link_state ~name:"summaries"
      ~func_typ:Symbolic.P.Extern_func.extern_type summaries_extern_module
  in
  let*/ to_run, link_state =
    list_fold_left
      (fun ((to_run, state) as acc) instruction ->
        match instruction with
        | Text.Module m ->
          let has_start =
            List.exists (function Text.MStart _ -> true | _ -> false) m.fields
          in
          let has_start_id_function =
            List.exists
              (function
                | Text.MFunc { id = Some "_start"; _ } -> true | _ -> false )
              m.fields
          in
          let fields =
            if has_start || not has_start_id_function then m.fields
            else MStart (Text "_start") :: m.fields
          in
          let m = { m with fields } in
          let+ m, state =
            Compile.until_link ~unsafe state ~optimize ~name:None m
          in
          let m = Symbolic.convert_module_to_run m in
          (m :: to_run, state)
        | Text.Register (name, id) ->
          let+ state = Link.register_module state ~name ~id in
          (to_run, state)
        | _ -> Ok acc )
      ([], link_state) file
  in
  let f (pc : (unit, _) result Choice.t) to_run =
    let c = (Interpret.SymbolicP.modul link_state.envs) to_run in
    let results =
      Choice.bind pc (fun r ->
          match r with Error _ -> Choice.return r | Ok () -> c )
    in
    results
  in
  List.fold_left f pc (List.rev to_run)

let run_file ~unsafe ~optimize pc filename =
  let*/ script = Parse.Script.from_file filename in
  simplify_then_link_then_run ~unsafe ~optimize pc script

let get_model ~symbols solver pc =
  assert (Solver.Z3Batch.check solver pc);
  match Solver.Z3Batch.model ~symbols solver with
  | None -> assert false
  | Some model -> model

let out_testcase ~dst ~err testcase =
  let o = Xmlm.make_output ~nl:true ~indent:(Some 2) dst in
  let tag ?(atts = []) name = (("", name), atts) in
  let atts = if err then Some [ (("", "coversError"), "true") ] else None in
  let to_string v = Format.asprintf "%a" Encoding.Value.pp_num v in
  let input v = `El (tag "input", [ `Data (to_string v) ]) in
  let testcase = `El (tag ?atts "testcase", List.map input testcase) in
  let dtd =
    {|<!DOCTYPE testcase PUBLIC "+//IDN sosy-lab.org//DTD test-format testcase 1.1//EN" "https://sosy-lab.org/test-format/testcase-1.1.dtd">|}
  in
  Xmlm.output o (`Dtd (Some dtd));
  Xmlm.output_tree Fun.id o testcase

let write_testcase =
  let cnt = ref 0 in
  fun ~dir ~err testcase ->
    incr cnt;
    let name = Format.ksprintf Fpath.v "testcase-%d.xml" !cnt in
    let path = Fpath.append dir name in
    let* res =
      Bos.OS.File.with_oc path
        (fun chan () -> Ok (out_testcase ~dst:(`Channel chan) ~err testcase))
        ()
    in
    res

let cmd profiling debug unsafe optimize workers no_stop_at_failure no_values
  (workspace : Fpath.t) files =
  if profiling then Log.profiling_on := true;
  if debug then Log.debug_on := true;
  let* () =
    match Bos.OS.Dir.create ~path:true ~mode:0o755 workspace with
    | Ok true | Ok false -> Ok ()
    | Error (`Msg msg) -> Error (`Msg msg)
  in
  let pc = Choice.return (Ok ()) in
  let solver = Solver.Z3Batch.create () in
  let result = List.fold_left (run_file ~unsafe ~optimize) pc files in
  let thread : Thread.t = Thread.create () in
  let results = Choice.run ~workers result thread in
  let* failing =
    list_fold_left
      (fun failing (result, thread) ->
        let pc = Thread.pc thread in
        let symbols = thread.symbol_set in
        let model = get_model ~symbols solver pc in
        let* result, err =
          match result with
          | Symbolic_choice.Multicore.EVal (Ok ()) -> Ok (failing, false)
          | EAssert assertion ->
            Ok (`EAssert (assertion, model) :: failing, true)
          | ETrap tr -> Ok (`ETrap (tr, model) :: failing, true)
          | EVal (Error e) -> Error e
        in
        if not no_values then
          let testcase =
            List.sort compare (Encoding.Model.get_bindings model)
            |> List.map snd
          in
          let+ () = write_testcase ~dir:workspace ~err testcase in
          result
        else Ok result )
      [] (List.of_seq results)
  in
  let print_bug = function
    | `ETrap (tr, model) ->
      Format.pp_std "Trap: %s@\n" (Trap.to_string tr);
      Format.pp_std "Model:@\n  @[<v>%a@]@\n"
        (Encoding.Model.pp ~no_values)
        model
    | `EAssert (assertion, model) ->
      Format.pp_std "Assert failure: %a@\n" Expr.pp assertion;
      Format.pp_std "Model:@\n  @[<v>%a@]@\n"
        (Encoding.Model.pp ~no_values)
        model
  in
  let rec print_and_count_failures count = function
    | [] -> count
    | hd :: tl ->
      print_bug hd;
      let count = succ count in
      if no_stop_at_failure then print_and_count_failures count tl else count
  in
  let count = print_and_count_failures 0 failing in
  if count > 0 then Error (`Found_bug count)
  else begin
    Format.pp_std "All OK";
    Ok ()
  end
