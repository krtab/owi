open Owi

let () = Random.self_init ()

(* an extern module that will be linked with a wasm module *)
let extern_module : Concrete_value.Func.extern_func Link.extern_module =
  (* some custom functions *)
  let str_buffer = Buffer.create 16 in
  let height (_ : int32) : int32 = 45l in
  let width (_ : int32) : int32 = 30l in
  let sleep (_ : int32) = Unix.sleepf 0.2 in
  let newline (_ : int32) = Buffer.add_string str_buffer "|\n" in
  let cell_print (i : int32) =
    Buffer.add_string str_buffer (if i = 1l then "| X " else "|   ")
  in
  let clear_screen (_ : int32) =
    print_endline "\027[2J";
    print_endline (Buffer.contents str_buffer);
    Buffer.clear str_buffer
  in
  let rand_val (bound : int32) : int32 =
    let r = Random.int32 bound in
    if r = 0l then 1l else 0l
  in
  let init_window (_ : int32) = () in
  (* we need to describe their types *)
  let functions =
    [ ( "height"
      , Concrete_value.Func.Extern_func (Func (Arg (I32, Res), R1 I32), height)
      )
    ; ( "width"
      , Concrete_value.Func.Extern_func (Func (Arg (I32, Res), R1 I32), width)
      )
    ; ( "sleep"
      , Concrete_value.Func.Extern_func (Func (Arg (I32, Res), R0), sleep) )
    ; ( "newline"
      , Concrete_value.Func.Extern_func (Func (Arg (I32, Res), R0), newline) )
    ; ( "cell_print"
      , Concrete_value.Func.Extern_func (Func (Arg (I32, Res), R0), cell_print)
      )
    ; ( "clear_screen"
      , Concrete_value.Func.Extern_func (Func (Arg (I32, Res), R0), clear_screen)
      )
    ; ( "rand_val"
      , Concrete_value.Func.Extern_func (Func (Arg (I32, Res), R1 I32), rand_val)
      )
    ; ( "init_window"
      , Concrete_value.Func.Extern_func (Func (Arg (I32, Res), R0), init_window)
      )
    ]
  in
  { functions }

(* a link state that contains our custom module, available under the name `life_ext` *)
let link_state =
  Link.extern_module Link.empty_state ~name:"life_ext" extern_module

(* first pure wasm module refering to `life_ext` *)
let pure_wasm_module_1 =
  match Parse.Text.Module.from_file (Fpath.v "life.wat") with
  | Error e -> Result.failwith e
  | Ok modul -> modul

(* our first pure wasm module, linked with `life_ext` *)
let module_to_run, link_state =
  match
    Compile.until_link link_state ~unsafe:false ~optimize:true
      ~name:(Some "life") pure_wasm_module_1
  with
  | Error e -> Result.failwith e
  | Ok (m, state) -> (m, state)

(* let's run it ! First module to be interpreted *)
let () =
  match Interpret.Concrete.modul link_state.envs module_to_run with
  | Error e -> Result.failwith e
  | Ok () -> ()

(* second pure wasm module refering to `life_ext` *)
let pure_wasm_module_2 =
  match Parse.Text.Module.from_file (Fpath.v "life_loop.wat") with
  | Error e -> Result.failwith e
  | Ok modul -> modul

(* our second pure wasm module, linked with `life_ext` and `life` interpreted before *)
let module_to_run, link_state =
  match
    Compile.until_link link_state ~unsafe:false ~optimize:true ~name:None
      pure_wasm_module_2
  with
  | Error e -> Result.failwith e
  | Ok (m, state) -> (m, state)

(* let's run it ! it will animate the game of life in console *)
let () =
  match Interpret.Concrete.modul link_state.envs module_to_run with
  | Error e -> Result.failwith e
  | Ok () -> ()
