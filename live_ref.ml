open Javalib_pack
open Javalib
open JBasics
open JCode
open Sawja_pack
open JBir
open JControlFlow
module Stack = BatStack
module Array = BatArray
module List = BatList
module JL = JClassLow
module Enum = BatEnum

let usage_msg = "Usage: live_ref class-path class-name
		 [Note
		 1.) Class-name should be given without the .class extension
		 2.) Should be a fully qualified name, .e.g,: java.lang.Object
		 3.) Should have the main function]";;

(* module JCodePPSet = Set.Make(struct *)
(*                               type t = JCodePP.t  *)
(*                               let compare = compare end) *)

(* The global list of new bytecodes program points to replace *)
let global_replace = ref []


let class_header = 5

(* Object handle offsets *)
let handle_size = 9
let mtab_off = 1 		(* Only this word is used!! *)
let size_off = 3
let type_off = 4
let ptr_to_next_off = 5
let gray_list_off = 6
let space_off = 7
let waste_1_off = 8
let waste_2_off = 9

let hADDRESS = ref (57536+8000)

exception Internal of string
exception NARGS
exception Not_supported of string
exception Uninitialized of string
exception Cant_handle of string

let signal_class_name = make_cn "systemj.lib.Signal"
let array_bound_class_name = make_cn "java.lang.ArrayBound"
let signal_set_value_ms = "setValue"

(* This is the piping operator *)
let (|>) x f = f x

(* This is the compose operator *)
let (>>) f g x = (f(g x))

let find_alli f l = 
  let ret = Array.mapi (fun c x -> if (f x) then Some c else None) l in
  let ret = Array.filter (function Some x -> true | None -> false) ret in
  Array.map (function Some x -> x | None -> raise (Internal "")) ret

let du t pc v = ReachDef.Lat.get (ReachDef.run t pc) v |> Ptset.elements
let duf t pc v = 
  try
    MyReachDef.Lat.get (MyReachDef.run t pc) v |> Ptset.elements
  with
  | _ -> []


let liveness t pc = Live_bir.run t pc |> Live_bir.Env.elements 

let rec isSignalSetValue ms = function
  | StaticField (cn,fs) -> 
     (* This happens when setting the variable argument*)
     let cn = (match (fs_type fs) with
               | TObject (TClass x) -> x
               | _ -> make_cn "") in
     (ms_name ms = signal_set_value_ms) && (cn = signal_class_name)
  | Field (e,cn,fs) -> 
     (* This happens when setting the field argument *)
     (ms_name ms = signal_set_value_ms) && (cn = signal_class_name)
  | Var (vt,v) -> (match vt with
		   | TObject (TClass cn) -> (ms_name ms = signal_set_value_ms) && (cn = signal_class_name)
		   | _ -> false)
  | Binop (_,e1,e2) -> isSignalSetValue ms e1 || isSignalSetValue ms e2
  | Unop (_,e1) -> isSignalSetValue ms e1 
  | Const _ -> false

let rec isCorrectField2 varrs vvt = function
  | Var (vt,v) -> List.exists ((=) v) varrs
  | Field (e,_,_) -> isCorrectField2 varrs vvt e
  | Unop (_,e) -> isCorrectField2 varrs vvt e
  | _ as s -> false

let isCorrectField varrs tfs le cn fs cnfs = 
  let ifs = FieldMap.value_elements cnfs in
  let cnfs = make_cfs cn fs in
  let ifs = List.map (function 
		       | InterfaceField ii -> ii.if_class_signature 
		       | ClassField ic -> ic.cf_class_signature) ifs in
  let r1 = List.exists (cfs_equal cnfs) ifs in
  let r2 = isCorrectField2 varrs tfs le in
  r2 

let isArrayEqual varrs = function
  | Var (_,vv) -> List.exists ((=) vv) varrs
  | _ -> false

let rec getargs = function
  | Unop (_,x) -> getargs x
  | Binop (_,x,y) -> (getargs x) @ (getargs y)
  | _ as s -> [s]

let rec scommon cnfs sors2 instrs setValuepcs pp_stack prta pbir mstack ms_stack this_ms mbir =
  let nmap =
    Array.fold_left 
      (fun elist (x,pc) ->
       (* Get the argument of the setValue method *)
       let arg = (match x with 
		  | InvokeVirtual (None,e1,vk,ms,el) -> el 
		  | AffectField (_,_,_,e) -> [e]
		  | AffectArray (_,_,e) -> [e]
		  | _ as s -> raise (Internal ("Set value not of type InvokeVirtual!: " ^ (print_instr s)))) in
       (* If arg is a local variable *)
       let arg = if List.length arg = 1 then List.hd arg else raise (Internal "") in
       let arg = getargs arg in
       let rr= List.fold_left 
		 (fun elist y -> 
		  (match y with
		   | StaticField (cn,fs) -> 
		      let fs_stack = Stack.create () in
		      let () = Stack.push fs fs_stack in
		      let ret = fielddefpcs prta (fs_type fs) pbir pp_stack fs_stack elist this_ms mstack ms_stack mbir pc cn fs x in
		      ignore(Stack.pop fs_stack); 
		      ret
		   | Field (e,cn,fs) -> 
		      let fs_stack = Stack.create () in
		      let () = Stack.push fs fs_stack in
		      let ret = fielddefpcs prta (fs_type fs) pbir pp_stack fs_stack elist this_ms mstack ms_stack mbir pc cn fs x in
		      ignore(Stack.pop fs_stack); 
		      ret
		   | Var (vt,v) -> 
		      let fs_stack = Stack.create () in
		      vardefpcs prta vt pbir pp_stack fs_stack elist this_ms mstack ms_stack mbir pc v x
		   | Const x -> elist
		   | _ as s -> 
		      raise (Internal ("Setting a non-field, var type value in setValue: " ^ (print_expr s)))
		  )) ClassMethodMap.empty arg in
       ClassMethodMap.merge (@) elist rr
      ) ClassMethodMap.empty setValuepcs in

  global_replace := nmap :: !global_replace;

  (* Invoke each method call separately for each invoke bytecode *)
  Array.iteri (fun i xx ->
	       match xx with
	       | InvokeStatic (_,cn,ms,_) -> 
		  invoke_method cnfs sors2 i pp_stack prta pbir cn ms mbir mstack ms_stack this_ms
	       | InvokeVirtual (_,_,VirtualCall (TClass cn),ms,el) -> 
		  invoke_method cnfs sors2 i pp_stack prta pbir cn ms mbir mstack ms_stack this_ms
	       | InvokeVirtual (_,_,VirtualCall (TArray cn),ms,el) -> raise (Internal "")
	       | InvokeVirtual (_,_,InterfaceCall cn,ms,el) -> 
		  invoke_method cnfs sors2 i pp_stack prta pbir cn ms mbir mstack ms_stack this_ms
	       | InvokeNonVirtual(_,_,cn,ms,_) ->
		  invoke_method cnfs sors2 i pp_stack prta pbir cn ms mbir mstack ms_stack this_ms
	       | _ -> ()) instrs

and fvardefpcs prta pbir pp_stack fs_stack map cms mstack ms_stack mbir pc v x =
  let defpcs = du mbir pc v in
  let pp = List.map (others prta pbir pp_stack fs_stack map cms mstack ms_stack mbir x) defpcs in
  let cns = List.map (fun (_,x,_) -> x) pp in
  let varrs = List.map (fun(x,_,_) ->x) pp in
  let varrs = varrs |> List.flatten in 
  let pp = List.map (fun (_,_,y) -> y) pp in
  let rpp = ((List.map (fun (x,_) -> x)) (pp |> List.flatten)) |> (List.sort_unique compare) in
  let pp = pp |> List.flatten |> List.unique in
  let cns = List.filter (function | Some x -> true | None -> false) cns in
  let cns = List.map (function | Some x -> x | None -> raise (Internal "No class name for new!")) cns in
  let cn = List.hd cns in
  if List.for_all ((=) cn) cns then
    if (List.length (List.map (fun (_,x)->x) pp) > 1) then 
      if not (reachable pbir (cms_split cms) rpp) then 
	(varrs, Some cn, pp)
      else
	let () = print_endline ("Check program points in class file method<" ^JPrint.class_method_signature cms^">: ") in
	List.iter (print_endline >> string_of_int) (List.map (fun (_,x)->x) pp);
	raise (Not_supported "Bad code with excess memory usage and excess calls to \"new\"")
    else
      (varrs, Some cn,pp)
  else
    if (List.length (List.map (fun (_,x)->x) pp) > 1) && (not (reachable pbir (cms_split cms) rpp)) then
      let sizes = List.map2 (fun x (pp,_) -> gets mbir pbir x pp) cns pp in
      let mmax = List.max sizes in
      let (index,_) = List.findi (fun i x -> mmax = x) sizes in
      (varrs, Some (List.nth cns index), pp)
    else
      let () = print_endline ("Check program points in class file method<" ^JPrint.class_method_signature cms^">: ") in
      List.iter (print_endline >> string_of_int) (List.map (fun (_,x)->x) pp);
      raise (Not_supported "Bad code with excess memory usage and excess calls to \"new\"")
	    
(* This only works for new bytecode not newarray! *)
and get_bpir pc cn mbir = 
  let fa = Array.filteri (fun i _ -> i<pc) (code mbir) in
  fa 
  |> Array.rev
  |> Array.findi (function 
		   | MayInit cnn -> 
		      cn_equal cn cnn
		   | _ -> false)
  |> ((-) ((Array.length fa)-1))


and others prta pbir pp_stack fs_stack map cms mstack ms_stack mbir x pc = 
  if pc >= 0 then
    match (code mbir).(pc) with
    | New (rv,cn,_,_) -> 
       let bpir = get_bpir pc cn mbir in
       ([rv], Some ((TObject (TClass cn))),[(pc,(pc_ir2bc mbir).(bpir))])
    | NewArray (rv,vt,els) -> 
       (* FIXME: This needs to be looked at later on!! *)
       ([rv], Some vt, [(pc,(pc_ir2bc mbir).(pc - 1))])
    | AffectVar (_,e) as s -> 
       hexpr prta pbir pp_stack fs_stack map cms mstack ms_stack mbir pc s e
    | _ as s -> 
       (* (print_endline >> JPrint.class_method_signature) cms; *)
       raise (Cant_handle ("Can't handle: " ^ (print_instr s)))
  else
    raise (Internal ("New outside the current method" ^ (print_instr x)))

and start2 (cnfs as ffs) pp_stack prta pbir mstack ms_stack this_ms mbir =
  let (varrs,tfs,cnfs) = (match cnfs with 
			  | Some (varrs,tfs,cnfs) -> (varrs,tfs,cnfs) 
			  | None -> raise (Internal "")) in
  let instrs = code mbir in
  (* VIMP: This has to be flow insensitive to be conservative *)
  (* What this means is that I am not checking just below this program
  points, but also above it, for conservative estimates. *)
  let setSigs = Array.mapi (fun pc x ->
			    (match x with
			     | AffectField (le,cn,fs,re) as s -> 
				if (isCorrectField varrs tfs le cn fs cnfs) then Some (s,pc) else None
			     | AffectArray (el,_,_) as s -> 
				if (isArrayEqual varrs el) then Some (s,pc) else None
			     | _ -> None)
			   ) instrs in
  let setValuepcs = Array.filter (function | Some _ -> true | None -> false) setSigs in
  let setValuepcs = Array.map (function | Some (x,y) -> (x,y) | _ -> raise (Internal "")) setValuepcs in
  scommon ffs false instrs setValuepcs pp_stack prta pbir mstack ms_stack this_ms mbir 

and start _ pp_stack prta pbir mstack ms_stack this_ms mbir =
  let instrs = code mbir in
  let setSigs = Array.mapi (fun pc x ->
			    (match x with
			     | InvokeVirtual (None,e1,vk,ms,el) as s -> 
				if (isSignalSetValue ms e1) then Some (s,pc) else None
			     | _ -> None)) instrs in
  let setValuepcs = Array.filter (function | Some _ -> true | None -> false) setSigs in
  let setValuepcs = Array.map (function | Some (x,y) -> (x,y) | _ -> raise (Internal "")) setValuepcs in
  scommon None true instrs setValuepcs pp_stack prta pbir mstack ms_stack this_ms mbir 

and get_object_size pbir = function
  | TObject (TClass cn) ->
     let pnode = JProgram.get_node pbir cn in
     let fields = JProgram.get_fields pnode in
     let fields = FieldMap.filter (not >> is_static_field) fields in
     let fields = FieldMap.elements fields in
     let vts = List.map (fun (x,y) -> (fs_type x,y)) fields in
     List.fold_left (fun v x -> (get_size pbir x + v)) 0 vts 
  | TObject(TArray vt) -> raise (Not_supported "Array as signal values, use objects wrapping arrays instead") (* FIXME *)
  | _ -> raise (Internal "Got a basic value type for size!")

and get_object_size_array = function
  | TBasic (`Bool) -> 1
  | TBasic (`Int) -> 1
  | TBasic (`Byte) -> 1
  | TBasic (`Char) -> 1
  | TBasic (`Float) -> 1
  | TBasic (`Short) -> 1
  | TBasic (`Long) -> 2
  | TBasic (`Double) -> 2 
  | TObject (TClass _) -> 1
  | _ -> raise (Internal "Array inside an array??")

and get_size pbir (vt,f) = 
  match vt with
  | TBasic (`Bool) -> 1
  | TBasic (`Int) -> 1
  | TBasic (`Byte) -> 1
  | TBasic (`Char) -> 1
  | TBasic (`Float) -> 1
  | TBasic (`Short) -> 1
  | TBasic (`Long) -> 2
  | TBasic (`Double) -> 2 
  | TObject (TClass _) -> 1
  | TObject (TArray _) -> 1

and ifields pbir = function
  | TObject (TClass cn) -> 
     let pnode = JProgram.get_node pbir cn in
     let fields = JProgram.get_fields pnode in
     (* let fields = FieldMap.filter (not >> is_static_field) fields in *)
     FieldMap.filter (function
		       | InterfaceField x -> 
			  (match fs_type (x.if_signature) with 
			   | TObject (TClass _) -> true 
			   | TObject (TArray vt) -> true
			   | _ -> false) 
		       | ClassField x -> 
			  (match fs_type (x.cf_signature) with 
			   | TObject (TClass _) -> true 
			   | TObject (TArray vt) -> true
			   | _ -> false)) fields
  | _ -> FieldMap.empty

and var_escape mbir v = 
  Array.iter 
    (function
      | Return (Some e) -> 
	 (match e with
	  | Var (_,v1) -> if (var_equal v v1) 
			  then 
			    raise (Not_supported ("Return of variable: " ^ (var_name v)))
	  | _ -> ())
      | _ -> ()
    )(code mbir)

and gets mbir pbir vvt pc = 
  match (code mbir).(pc) with
  | NewArray(_,vt,els) -> 
     (* Check that all els have constant dimensions *)
     let dims = List.map (function 
			   | Const (`Int x) -> 
			      let ret = Int32.to_int x in
			      if ret >= 0 then ret else raise (Not_supported ("Negative dimension arrays: " ^ (string_of_int ret)))
			   | _ as s -> raise (Not_supported ("Non constant array dimensions" ^ (print_expr s)))
			 ) els in
     let dim = List.fold_left ( * ) 1 dims in
     dim * (get_object_size_array vt)
  | New _ -> (get_object_size pbir vvt) + (super_size pbir vvt)
  | _ as s -> raise (Internal ("Wrong instruction!: "^ (print_instr s)))


(* FIXME:
   1.) Need to consider if the var is an argument and not a local var
   -- Can handle primitive argument var, but not Object type argument var.
 *)
and vardefpcs prta vt pbir pp_stack fs_stack map cms mstack ms_stack mbir pc v x = 
  let (vaars,rrcn,rrp) = fvardefpcs prta pbir pp_stack fs_stack map cms mstack ms_stack mbir pc v x in
  let rrcn = (match rrcn with 
	      | Some x -> x 
	      | None -> raise (Internal ("Variable v's new type is unknown: " ^ (var_name v)))) in
  let rr = [rrp] in
  let sizes = List.map (fun (pp,_) -> gets mbir pbir rrcn pp) rrp in
  let size = List.max sizes in
  let rr = List.map (fun x -> List.map (fun x -> (x,size)) x) rr in
  (* Check that v does not escape from this method *)
  ignore(
      let ifs = ifields pbir rrcn in
      if not (FieldMap.is_empty ifs) 
      then 
	start2 (Some (vaars,vt,ifs)) pp_stack
	       prta pbir mstack
	       ms_stack cms mbir
      else ());
  let () = var_escape mbir v in 
  (* Return if everything is A-OK *)
  ClassMethodMap.add cms rr map

and hexpr prta pbir pp_stack fs_stack map cms mstack ms_stack mbir pc x = function
  | Const c -> ([],None,[])
  | Var (vt, v) -> 
     fvardefpcs prta pbir pp_stack fs_stack map cms mstack ms_stack mbir pc v x
  | Unop (_,e) -> 
     hexpr prta pbir pp_stack fs_stack map cms mstack ms_stack mbir pc x e
  | Binop (_,e1,e2) -> 
     let (vaars1,cn,ppl) = 
       hexpr prta pbir pp_stack fs_stack map cms mstack ms_stack mbir pc x e1 in
     let (vaars2,cn2,ppl2) = 
       hexpr prta pbir pp_stack fs_stack map cms mstack ms_stack mbir pc x e2 in
     if (cn = cn2) then (vaars1@vaars2,cn, ppl @ ppl2)
     else raise (Internal ("BinOp: class not the same: " ))
  | Field (e,cn,fs) -> 
     if not (Enum.exists (fs_equal fs) (Stack.enum fs_stack)) then 
       let () = Stack.push fs fs_stack in
       let (vaars,cn,ret) = 
	 vfielddefpcs prta pbir pp_stack fs_stack map cms mstack ms_stack mbir pc cn fs x in
       let ret = ret |> List.flatten |> List.unique in
       ignore(Stack.pop fs_stack);
       (vaars,cn,ret)
     else ([],None,[])
  | StaticField (cn,fs) -> 
     if not (Enum.exists (fs_equal fs) (Stack.enum fs_stack)) then 
       let () = Stack.push fs fs_stack in
       let (vaars,cn,ret) =
	 vfielddefpcs prta pbir pp_stack fs_stack map cms mstack ms_stack mbir pc cn fs x in
       let ret = ret |> List.flatten |> List.unique in
       ignore(Stack.pop fs_stack);
       (vaars,cn,ret)
     else ([],None,[])

and reachable pbir (cn,ms) = function
  | [] -> false
  | h::t ->
     (reachable2 pbir h t (cn,ms))
     || (reachable pbir (cn,ms) t)

and reachable2 pbir h pps (cn,ms) = 
  let pp = JBirPP.get_first_pp pbir cn ms in
  let pp = JBirPP.goto_absolute pp h in
  let rpp = JBirPP.reachable_pp pp in
  let rpp = List.map JBirPP.get_pc rpp in
  List.fold_left (fun v x -> v || (List.exists ((=) x) rpp)) false pps

and getliveness = function
  | Var (_,v) -> v
  | _ as s -> raise (Internal ("Currently not supported: " ^ print_expr s))

and super_size pbir cnn = 
  match cnn with
  | TObject (TClass cnn) -> 
     let nnode = JProgram.to_ioc (JProgram.get_node pbir cnn) in
     (match nnode with
      | JInterface x -> 0
      | JClass x -> 
	 (match x.c_super_class with
	  | Some x -> 
	     (get_object_size pbir (TObject (TClass x))) + 
	       super_size pbir (TObject (TClass x))
	  | None -> 0))
  | _ -> 0

and vfielddefpcs prta pbir pp_stack fs_stack map cms mstack ms_stack mbir pc cn fs x =
  let fslv = Array.mapi 
	       (fun pc' x ->
		match x with
		| AffectField (e,cn',fs',e')
		     when not (Enum.exists (fs_equal fs') (Stack.enum fs_stack)) -> 
		   let vars = liveness mbir pc' in
		   if List.length vars = 1 then Some ((List.hd vars), pc')
		   else None
		| AffectStaticField (cn',fs',e')
		     when not (Enum.exists (fs_equal fs') (Stack.enum fs_stack)) -> 
		   let vars = liveness mbir pc' in
		   if List.length vars = 1 then Some ((List.hd vars), pc')
		   else None
		| _ -> None) (code mbir) in
  let fslv = Array.filter (function | Some _ -> true | None -> false) fslv in
  let fslv = Array.map (function | Some x -> x 
			| None -> raise (Internal "")) fslv in
  let pcs = duf mbir pc (make_cfs cn fs) in
  if List.length pcs <> 0 then
    let rescn = ref None in
    let varrscn = ref [] in
    let res = 
      Array.fold_left 
	(fun res pc'->
	 match (code mbir).(pc') with
	 | AffectField (e,cn',fs',e') as s -> 
            let vars = [getliveness e'] in
	    let (varrs,vrescn, vres) = fvardefpcs prta pbir pp_stack fs_stack map cms mstack ms_stack mbir pc' (List.hd vars) x in
	    let vress = List.map (fun (_,x) -> x) vres in
	    rescn := vrescn;
	    varrscn := varrs;
            if List.length vars = 1 then
	      (if Array.exists (fun (fs'', _) -> (List.hd vars) = fs'') fslv then
		 let fslvs = Array.filter (fun (fs'', _) -> fs'' = (List.hd vars)) fslv in
		 let opps = Array.map 
			      (fun (fs'', pc'') -> 
			       let (_,_,r) = 
				 try 
				   fvardefpcs prta pbir pp_stack fs_stack map cms mstack ms_stack mbir pc'' fs'' x 
				 with
				 | Cant_handle _ -> ([],None,[])
			       in 
			       List.map (fun (_,x) -> x) r) fslvs in
		 let opps = opps |> Array.fold_left (@) [] |> List.fold_left (fun s x -> Ptset.add x s) Ptset.empty in
		 let vvres = List.fold_left (fun s x -> Ptset.add x s) Ptset.empty vress in
		 if not (Ptset.equal opps vvres) then
		   (List.fold_left (fun r x -> [x] :: r) [] vres) @ res
		 else
		   [vres] @ res
	       else
		 [vres] @ res)
            else
              raise (Internal ("Field being set with more than one var!: " ^ (print_instr s)))
	 | AffectStaticField (cn',fs',e') as s -> 
            let vars = [getliveness e'] in
	    let (varrs,vrescn,vres) = fvardefpcs prta pbir pp_stack fs_stack map cms mstack ms_stack mbir pc' (List.hd vars) x in
	    let vress = List.map (fun (_,x) -> x) vres in
	    rescn := vrescn;
	    varrscn := varrs;
            if List.length vars = 1 then
	      (if Array.exists (fun (fs'', _) -> (List.hd vars) = fs'') fslv then
		 let fslvs = Array.filter (fun (fs'', _) -> fs'' = (List.hd vars)) fslv in
		 let opps = Array.map 
			      (fun (fs'', pc'') -> 
			       let (_,_,r) = 
				 try 
				   fvardefpcs prta pbir pp_stack fs_stack map cms mstack ms_stack mbir pc'' fs'' x 
				 with
				 | Cant_handle _ -> ([],None,[])
			       in 
			       List.map (fun (_,x) -> x) r) fslvs 
			    |> Array.fold_left (@) [] 
			    |> List.fold_left (fun s x -> Ptset.add x s) Ptset.empty in
		 let vvres = List.fold_left (fun s x -> Ptset.add x s) Ptset.empty vress in
		 if not (Ptset.equal opps vvres) then
		   (List.fold_left (fun r x -> [x] :: r) [] vres) @ res
		 else
		   [vres] @ res
	       else
		 [vres] @ res)
            else
              raise (Internal ("Field being set with more than one var!: " ^ (print_instr s)))
	 | _ as s -> raise (Internal (print_instr s))
	) [] (Array.of_list pcs) in
    (!varrscn,!rescn,res)
  else
    if not (Stack.is_empty pp_stack) then
      let pp_stack_c = Stack.copy pp_stack in
      let mstack_c = Stack.copy mstack in
      let ms_stack_c = Stack.copy ms_stack in
      let pcc = Stack.pop pp_stack_c in
      let mbirc = Stack.pop mstack_c in
      let cmsc = Stack.pop ms_stack_c in
      let (varrs,_,rr) = vfielddefpcs prta pbir pp_stack_c (Stack.create ()) map cmsc mstack_c ms_stack_c mbirc pcc cn fs x in
      let () = (match (fs_type fs) with 
		| (TObject (TClass x)) -> () 
		| _ -> raise (Not_supported "Fields of non Class type")) in
      let size = get_object_size pbir (fs_type fs) in
      let size = size + super_size pbir (fs_type fs) in
      let rr = List.map (fun x -> List.map (fun x -> (x,size)) x) rr in
      ignore(
	  let ifs = ifields pbir (fs_type fs) in
	  if not (FieldMap.is_empty ifs) 
	  then 
	    start2_others varrs ifs prta pbir (Stack.copy pp_stack_c) cmsc 
			  (Stack.copy mstack_c) 
			  (Stack.copy ms_stack_c) mbirc pcc cn fs
	  else ());
      global_replace := (ClassMethodMap.add cmsc rr map) :: !global_replace;
      ([],None,[])
    else
      raise (Uninitialized ("Field: " ^ (fs_name fs))) 

and start2_others varrs ifs prta pbir pp_stack cms mstack ms_stack mbir pc cn fs =
  let () = start2 (Some (varrs,(fs_type fs),ifs)) pp_stack
		  prta pbir mstack
		  ms_stack cms mbir in
  if not (Stack.is_empty pp_stack) then
    let cmsc = Stack.pop ms_stack in
    let mbirc = Stack.pop mstack in
    ignore(Stack.pop pp_stack);
    start2_others varrs ifs prta pbir pp_stack cmsc mstack ms_stack mbirc pc cn fs

(* XXX: We do not do not check that class name of the new vs the field,
because this is handled at the variable level not at the field level.*)
and fielddefpcs prta vt pbir pp_stack fs_stack map cms mstack ms_stack mbir pc cn fs x =
  let (varrs,_,rr) = vfielddefpcs prta pbir pp_stack fs_stack map cms mstack ms_stack mbir pc cn fs x in
  let () = (match vt with | 
		     (TObject (TClass x)) -> () 
		     | _ -> raise (Not_supported "Fields of non Class type")) in
  let size = get_object_size pbir vt in
  let size = size + super_size pbir vt in
  let rr = List.map (fun x -> List.map (fun x -> (x,size)) x) rr in
  (* let () = List.iter (fun x -> List.iter (fun (x,_) -> print_int x; print_string " ") x; print_endline "\n") rr in *)
  ignore(
      let ifs = ifields pbir (fs_type fs) in
      if not (FieldMap.is_empty ifs) 
      then 
	start2_others varrs ifs prta pbir 
		      (Stack.copy pp_stack) cms 
		      (Stack.copy mstack) 
		      (Stack.copy ms_stack) mbir pc cn fs
      else ());
  ClassMethodMap.add cms rr map

and invoke_method cnfs sors2 pp pp_stack prta pbir cn ms mbir mstack ms_stack this_ms = 
  let cmi = JProgram.get_concrete_method (JProgram.get_node pbir cn) ms in
  let () = Stack.push pp pp_stack in
  let () = Stack.push mbir mstack in
  let () = Stack.push this_ms ms_stack in
  let _ = 
    try
      if sors2 then
	ignore(map_concrete_method ~force:true (start cnfs pp_stack prta pbir mstack ms_stack (cmi.cm_class_method_signature)) cmi) 
      else
	ignore(map_concrete_method ~force:true (start2 cnfs pp_stack prta pbir mstack ms_stack (cmi.cm_class_method_signature)) cmi) 
    with Not_found -> () in
  let _ = Stack.pop pp_stack in 
  let _ = Stack.pop mstack in 
  ignore(Stack.pop ms_stack)

let rec signals prta pbir mstack ms_stack this_ms mbir =
  (* These are all the new statements for some objects *)
  let snpcs = find_alli (function New (_,cn,_,_) -> cn_equal cn signal_class_name | _ -> false) (code mbir) in
  let bcn = pc_ir2bc mbir in
  let snpcs = Array.map (fun x -> [((x,bcn.(x-1)),(get_object_size pbir (TObject (TClass signal_class_name))))]) snpcs |> Array.rev in
  let nmap = Array.map (fun x -> ClassMethodMap.add this_ms [x] ClassMethodMap.empty) snpcs in
  let nmap = Array.fold_left (fun r x -> ClassMethodMap.merge (@) x r) ClassMethodMap.empty nmap in
  global_replace := nmap :: !global_replace;
  (* Now iterate throw the rest of the calls made from this method *)
  Array.iter (function
	       | InvokeStatic (_,cn,ms,_) -> 
		  sinvoke_method prta pbir mstack ms_stack cn ms this_ms mbir 
	       | InvokeVirtual (_,_,VirtualCall (TClass cn),ms,el) -> 
		  sinvoke_method prta pbir mstack ms_stack cn ms this_ms mbir 
	       | InvokeVirtual (_,_,VirtualCall (TArray cn),ms,el) -> raise (Internal "")
	       | InvokeVirtual (_,_,InterfaceCall cn,ms,el) -> 
		  sinvoke_method prta pbir mstack ms_stack cn ms this_ms mbir 
	       | InvokeNonVirtual(_,_,cn,ms,_) ->
		  sinvoke_method prta pbir mstack ms_stack cn ms this_ms mbir 
	       | _ -> ()) (code mbir)

and sinvoke_method prta pbir mstack ms_stack cn ms this_ms mbir =  
  let cmi = JProgram.get_concrete_method (JProgram.get_node pbir cn) ms in
  let () = Stack.push mbir mstack in
  let () = Stack.push this_ms ms_stack in
  let _ = 
    try
      ignore(map_concrete_method ~force:true (signals prta pbir mstack ms_stack (cmi.cm_class_method_signature)) cmi)
    with
    | Not_found -> () in

  let _ = Stack.pop mstack in 
  ignore(Stack.pop ms_stack)

let main =
  try
    let args = Sys.argv in
    let (cp, cn) =
      if Array.length args <> 3 then let () = print_endline usage_msg in raise NARGS
      else (args.(1),args.(2)) in
    (* Need to build all the other entry points so that other classes are also parsed!! *)
    let (prta,_) = JRTA.parse_program ~instantiated:[] ~other_entrypoints:[make_cms (make_cn "com.jopdesign.sys.Startup")
										    (make_ms "boot" [] None)]
				      cp (make_cms (make_cn cn) JProgram.main_signature) in
    (* Convert it into JBIR format *)
    let pbir = JProgram.map_program2
		 (fun _ -> JBir.transform ~bcv:false ~ch_link:false ~formula:false ~formula_cmd:[]) 
		 (Some (fun code pp -> (JBir.pc_ir2bc code).(pp)))
		 prta in

    let obj = JProgram.get_node pbir (make_cn cn) in
    let mobj = JProgram.get_concrete_method obj JProgram.main_signature in
    let ss = Stack.create () in
    let pp_stack = Stack.create () in
    let ms_ss = Stack.create () in

    (* This function is used to get new opcodes for the signal objects *)
    ignore(map_concrete_method ~force:true (signals prta pbir ss ms_ss (mobj.cm_class_method_signature)) mobj);


    (* From here on we use dataflow analysis to replace new opcodes being passed to signal object's setValue method *)
    JPrint.print_class (JProgram.to_ioc obj) JBir.print stdout;
    ignore(map_concrete_method ~force:true (start None pp_stack prta pbir ss ms_ss (mobj.cm_class_method_signature)) mobj);

    (* JPrint.print_class (JProgram.to_ioc (JProgram.get_node prta (make_cn cn))) JPrint.jcode stdout; *)
    (* Now we are ready to replace the bytecodes!! *)
    let global_replace = !global_replace in
    let global_replace = List.fold_left (fun r x -> ClassMethodMap.merge (@) x r) ClassMethodMap.empty global_replace in
    (* Replace them bytecodes *)
    let prta = ClassMethodMap.fold 
		 (fun k v prta ->
		  JProgram.map_program2 
		    (fun pnode cm javacode -> 
                     if (cms_equal k cm.cm_class_method_signature) 
                     then
		       (* let () = print_endline (JPrint.class_method_signature k) in *)
		       let ndone = ref [] in
                       (* Changing the new instruction here!! *)
                       List.fold_left
			 (fun jt rl ->
			  (* let () = List.iter (fun ((bb,x),_) -> print_int x; print_string " "; print_int bb; print_endline " ") rl in *)
			  (* let () = print_endline "\n" in *)
			  let lnt = match jt.JCode.c_line_number_table with 
                            | Some x -> x 
                            | None -> failwith  
					("Could not find the line number table of "^(JPrint.class_method_signature cm.Javalib.cm_class_method_signature))
			  in
			  let doonce = ref true in
			  let (r,lnt) =
                            List.fold_left
                              (fun (r,lnt) ((bpp,x),size) ->
			       if not (List.exists ((=) x) !ndone) then
				 (* Extend the constant pool!! *)
				 let pc = (match JProgram.to_ioc pnode with | JClass x -> x | _ -> raise (Internal "")) in
				 let () = 
				   if(!doonce) then
				     let () = hADDRESS := !hADDRESS - (size+handle_size) in
				     let pool = Array.append pc.c_consts [|ConstValue (ConstInt (Int32.of_int (!hADDRESS+9)))|] in
				     let pool = Array.append pool [|ConstValue (ConstInt (Int32.of_int !hADDRESS))|] in
				     pc.c_consts <- pool;
				     doonce := false; 
				 in

				 let (cname,mms) = cms_split k in
				 let birc = (match (JProgram.get_concrete_method (JProgram.get_node pbir cname) mms).cm_implementation with
					     | Native -> raise (Internal "Native method in NewArray")
					     | Java x -> Lazy.force x) in

				 let gremove mbir pp = 
				   match (code mbir).(pp) with
				   | NewArray (_,(TBasic _),_) -> 2
				   | NewArray (_,(TObject (TClass _)),_) -> 3
				   | New _ -> 3
				   | _ as s -> raise (Internal ("Expected new/(a)newarray got: " ^ (print_instr s))) in

				 let remove = gremove birc bpp in

				 let pipi = 22 in

				 let ox = x in
				 let x = List.fold_left (fun x t -> if ox > t then x + pipi else x) x !ndone in
				 (* let () = (print_endline >> string_of_int) x in *)
				 ndone := ox :: !ndone;
				 let newinstr = r.(x) in

				 (* Increasing line numbers *)
				 let lnt = List.map (fun ((bll,sll) as y) -> if bll > x then (bll+pipi,sll) else y ) lnt in
				 (* ------ done *)

				 let fa = Array.filteri (fun i _ -> (i<x)) r in
				 let fa = Array.mapi 
					    (fun rr ff ->
					     (match ff with
					      | OpIfCmp (xx,target) as s -> if (rr + target) = x then s 
									    else if (rr+target) > x then OpIfCmp (xx,(target+pipi))
									    else s
					      | OpIf (xx,target) as s -> if (rr+target) = x then s 
									 else if (rr+target) > x then OpIf (xx,(target+pipi))
									 else s
					      | OpGoto target as s -> if (rr+target) = x then s 
								      else if (rr+target) > x then OpGoto (target + pipi)
								      else s
					      | OpTableSwitch _ | OpLookupSwitch _ -> raise (Internal "Analysis with switch stmt not supported")
					      | _ as s -> s
					     )) fa in
				 let sa = Array.filteri (fun i _ -> (i>x+(remove-1))) r in
				 let sa = Array.mapi 
					    (fun rr ff ->
					     let mindex = x + 3 + rr in
					     (match ff with
					      | OpIfCmp (xx,target) as s -> if (mindex + target) = x then s 
									    else if (mindex + target) < x then OpIfCmp (xx,(target-pipi))
									    else s
					      | OpIf (xx,target) as s -> if (mindex + target) = x then s 
									 else if (mindex + target) < x then OpIf (xx,(target-pipi))
									 else s
					      | OpGoto target as s -> 
						 if (mindex + target) = x then s 
						 else if (mindex + target) < x then OpGoto (target - pipi)
						 else s
					      | OpTableSwitch _ | OpLookupSwitch _ -> raise (Internal "Analysis with switch stmt not supported")
					      | _ as s -> s
					     )) sa in

				 (* Change to low level format to get the index in the constant pool *)
				 (* Should be encoded in 3 bytes max *)
				 let cpool1 = DynArray.init (Array.length pc.c_consts) (fun i -> pc.c_consts.(i)) in
				 let newinstrlow = JInstruction.instruction2opcode cpool1 remove newinstr in
				 match newinstrlow with 
				 | JClassLow.OpNew nx -> 
				    let poolindex = nx in
				    let xx = [|
					(* This is the instruction sequence that replaces new after deleting it *)
					JInstruction.opcode2instruction
					  pc.c_consts (JClassLow.OpLdc1w ((Array.length pc.c_consts) - 1));
					OpInvalid; OpInvalid; (* 3 bytes *)

					OpDup; (* 1 byte *)

					JInstruction.opcode2instruction
					  pc.c_consts (JClassLow.OpLdc1w ((Array.length pc.c_consts) - 2));
					OpInvalid; OpInvalid; (* 3 bytes *)

					OpSwap; (* 1 byte *)

					(* Write the pointer to start of data *)
					OpInvoke ((`Static (make_cn "com.jopdesign.sys.Native")),
						  (make_ms "wr" [(TBasic `Int);(TBasic `Int)] None));
					OpInvalid; OpInvalid; (* 3 bytes *)

					OpDup; (* 1 byte *)

					OpConst (`Byte mtab_off); OpInvalid; (* 2 byte *)

					OpAdd `Int2Bool; (* 1 byte *)

					JInstruction.opcode2instruction pc.c_consts (JClassLow.OpLdc1w poolindex); 
					OpInvalid; OpInvalid; (* 3 bytes *)

					OpConst (`Byte class_header); OpInvalid; (* 2 bytes *)

					OpAdd `Int2Bool; (* 1 byte *)

					OpSwap; (* 1 byte *)

					(* Write the pointer to start of method table structure *)
					OpInvoke ((`Static (make_cn "com.jopdesign.sys.Native")),
						  (make_ms "wr" [(TBasic `Int);(TBasic `Int)] None));
					OpInvalid; OpInvalid (* 3 bytes *)

				       |] in
				    (Array.append (Array.append fa xx) sa,lnt)

				 (* Allocating 1-D primitive arrays with const dimensions *)
				 | JClassLow.OpNewArray _ 
				 | JClassLow.OpANewArray _ -> 
				    let dim = (match (code birc).(bpp) with
					       | NewArray (_,_,els) -> 
						  if List.length els = 1 then
						    match (List.hd els) with
						    | Const (`Int x) -> x
						    | _ as s -> raise (Not_supported ("Non constant array dimensions" ^ (print_expr s)))
						  else raise (Not_supported ("Arrays with more than 1-dimension"))
					       | _ as s -> raise (Internal (print_instr s))) in
				    let xx = [|
					(* This is the instruction sequence that replaces new after deleting it *)
					(* OpNop; (\* 1 byte *\) *)
					OpPop; (* 1 byte *)

					JInstruction.opcode2instruction
					  pc.c_consts (JClassLow.OpLdc1w ((Array.length pc.c_consts) - 1));
					OpInvalid; OpInvalid; (* 3 bytes *)

					OpDup; (* 1 byte *)

					JInstruction.opcode2instruction
					  pc.c_consts (JClassLow.OpLdc1w ((Array.length pc.c_consts) - 2));
					OpInvalid; OpInvalid; (* 3 bytes *)

					OpSwap; (* 1 byte *)

					(* Write the pointer to start of data *)
					OpInvoke ((`Static (make_cn "com.jopdesign.sys.Native")),
						  (make_ms "wr" [(TBasic `Int);(TBasic `Int)] None));
					OpInvalid; OpInvalid; (* 3 bytes *)

					OpDup; (* 1 byte *)

					OpConst (`Byte mtab_off); OpInvalid; (* 2 byte *)

					OpAdd `Int2Bool; (* 1 byte *)

					OpConst (`Int dim); OpInvalid; (* 2 bytes *)

					OpSwap; (* 1 byte *)

					(* Write the pointer to start of method table structure *)
					OpInvoke ((`Static (make_cn "com.jopdesign.sys.Native")),
						  (make_ms "wr" [(TBasic `Int);(TBasic `Int)] None));
					OpInvalid; OpInvalid; (* 3 bytes *)
					
				       |] in
				    let xx = (match newinstrlow with
					      | JClassLow.OpNewArray _ -> Array.append xx [|OpNop;OpNop|]
					      | _ -> Array.append xx [|OpNop;OpNop;OpNop|]) in
				    (Array.append (Array.append fa xx) sa,lnt)
				      
				 | _ as op -> 
				    (print_endline >> JPrint.class_method_signature) k;
				    print_endline ("Looking for new/newarray opcode, found: " ^ (JDumpLow.opcode op));
				    raise (Internal ("Encode incorrectly as byte: " ^ (string_of_int x)))
			       else (r,lnt)
                              )(jt.c_code,lnt) rl in
			  {jt with c_code = r; c_line_number_table = Some lnt}
			 ) javacode v
                     else javacode) None prta
		 ) global_replace prta in
    (* JPrint.print_class (JProgram.to_ioc (JProgram.get_node prta (make_cn cn))) JPrint.jcode stdout; *)
    unparse_class (JProgram.to_ioc (JProgram.get_node prta (make_cn cn))) (open_out_bin (cn^".class"));
  with 
  | NARGS -> ()
