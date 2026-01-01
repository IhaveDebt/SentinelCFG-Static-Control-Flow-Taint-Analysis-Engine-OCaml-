module StringMap = Map.Make(String)
module IntSet = Set.Make(Int)

type expr =
  | Const of int
  | Var of string
  | Add of expr * expr
  | Input

type stmt =
  | Assign of string * expr
  | Print of expr
  | If of expr * stmt list * stmt list
  | While of expr * stmt list

type block_id = int

type block = {
  id : block_id;
  stmts : stmt list;
  mutable succs : block_id list;
}

type cfg = {
  blocks : (block_id, block) Hashtbl.t;
  entry : block_id;
}

type taint =
  | Clean
  | Tainted

type env = taint StringMap.t

let fresh_id =
  let c = ref 0 in
  fun () -> incr c; !c

let rec taint_expr env = function
  | Const _ -> Clean
  | Var v -> StringMap.find_opt v env |> Option.value ~default:Clean
  | Add (a, b) ->
      if taint_expr env a = Tainted || taint_expr env b = Tainted
      then Tainted else Clean
  | Input -> Tainted

let taint_stmt env = function
  | Assign (v, e) ->
      StringMap.add v (taint_expr env e) env
  | Print _ -> env
  | If _ | While _ -> env

let build_block stmts =
  { id = fresh_id (); stmts; succs = [] }

let rec build_cfg stmts =
  let entry = build_block stmts in
  let tbl = Hashtbl.create 16 in
  Hashtbl.add tbl entry.id entry;
  { blocks = tbl; entry = entry.id }

let analyze_block block env =
  List.fold_left taint_stmt env block.stmts

let rec analyze_cfg cfg =
  let worklist = Queue.create () in
  let envs = Hashtbl.create 16 in

  Queue.add cfg.entry worklist;
  Hashtbl.add envs cfg.entry StringMap.empty;

  while not (Queue.is_empty worklist) do
    let bid = Queue.pop worklist in
    let block = Hashtbl.find cfg.blocks bid in
    let in_env = Hashtbl.find envs bid in
    let out_env = analyze_block block in_env in

    List.iter (fun succ ->
      let old =
        Hashtbl.find_opt envs succ |> Option.value ~default:StringMap.empty
      in
      if old <> out_env then begin
        Hashtbl.replace envs succ out_env;
        Queue.add succ worklist
      end
    ) block.succs
  done;
  envs

let report envs =
  print_endline "=== SentinelCFG Taint Report ===";
  Hashtbl.iter (fun bid env ->
    print_endline ("Block " ^ string_of_int bid);
    StringMap.iter (fun v t ->
      Printf.printf "  %s -> %s\n" v
        (match t with Clean -> "Clean" | Tainted -> "Tainted")
    ) env
  ) envs

(* Demo Program *)
let program =
  [
    Assign ("x", Input);
    Assign ("y", Const 5);
    Assign ("z", Add (Var "x", Var "y"));
    Print (Var "z");
  ]

let () =
  let cfg = build_cfg program in
  let envs = analyze_cfg cfg in
  report envs
