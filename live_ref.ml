open Javalib_pack
open Javalib
open JBasics
open JCode
open Sawja_pack
open JBir
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

(* The global list of new bytecodes program points to replace *)
let global_replace = ref []

let pipi = 12 + 9

let mtab_len = 5
let hADDRESS = ref (57536+8000)

exception Internal of string
exception NARGS
exception Not_supported of string
exception Uninitialized of string

let signal_class_name = make_cn "systemj.lib.Signal"
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

let isCorrectField2 vvt = function
  | Var (vt,v) -> 
     (* FIXME: This is not good enough. It allocates memory in the
     permanent-heap even though it should not go there. So we have
     pointers from normal heap to permanent heap space. It is sound, but
     excessive memory consumption can happen. We need to get the actual
     field that this variable is pointing to.*)
     vvt = vt
  | _ -> false

let isCorrectField tfs le cn fs cnfs = 
  let ifs = FieldMap.value_elements cnfs in
  let cnfs = make_cfs cn fs in
  let ifs = List.map (function 
		       | InterfaceField ii -> ii.if_class_signature 
		       | ClassField ic -> ic.cf_class_signature) ifs in
  let r1 = List.exists (cfs_equal cnfs) ifs in
  let r2 = isCorrectField2 tfs le in
  r1 && r2

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

and start2 (cnfs as ffs) pp_stack prta pbir mstack ms_stack this_ms mbir =
  let (tfs,cnfs) = (match cnfs with 
		   | Some (tfs,cnfs) -> (tfs,cnfs) 
		   | None -> raise (Internal "")) in
  let instrs = code mbir in
  (* VIMP: This has to be flow insensitive to be conservative *)
  (* That this means is that I am not checking just below this program
  points, but also above it, for conservative estimates. *)
  let setSigs = Array.mapi (fun pc x ->
			    (match x with
			     | AffectField (le,cn,fs,re) as s -> 
				if (isCorrectField tfs le cn fs cnfs) then Some (s,pc) else None
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
     let fields = FieldMap.key_elements fields in
     let vts = List.map fs_type fields in
     List.fold_left (fun v x -> (get_size pbir x + v)) 0 vts 
  | TObject(TArray vt) -> raise (Not_supported "Array as signal values") (* FIXME *)
  | _ -> raise (Internal "Got a basic valutype for size!")

and get_size pbir = function
  | TBasic (`Bool) -> 1
  | TBasic (`Int) -> 1
  | TBasic (`Byte) -> 1
  | TBasic (`Char) -> 1
  | TBasic (`Float) -> 1
  | TBasic (`Short) -> 1
  | TBasic (`Long) -> 2
  | TBasic (`Double) -> 2 
  | TObject (TClass _) -> 1
  | TObject (TArray _) -> raise (Not_supported "Array as signal values") (* FIXME *)

and ifields pbir = function
  | TObject (TClass cn) -> 
     let pnode = JProgram.get_node pbir cn in
     let fields = JProgram.get_fields pnode in
     let fields = FieldMap.filter (not >> is_static_field) fields in
     FieldMap.filter (function 
		       | InterfaceField x -> 
			  (match fs_type (x.if_signature) with 
			   | TObject (TClass _) -> true 
			   | _ -> false) 
		       | ClassField x -> 
			  (match fs_type (x.cf_signature) with 
			   | TObject (TClass _) -> true 
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

(* FIXME:
   1.) Need to consider if the var is an argument and not a local var
   -- Can handle primitive argument var, but not Object type argument var.
 *)
and vardefpcs prta vt pbir pp_stack fs_stack map cms mstack ms_stack mbir pc v x = 
  let (rrcn,rrp) = fvardefpcs prta pbir pp_stack fs_stack map cms mstack ms_stack mbir pc v x in
  let rrcn = (match rrcn with 
	      | Some x -> x 
	      | None -> raise (Internal ("Variable v's new type is unknown: " ^ (var_name v)))) in
  let rr = [rrp] in
  let size = get_object_size pbir (TObject (TClass rrcn)) in
  let rr = List.map (fun x -> List.map (fun x -> (x,size)) x) rr in
  (* Check that v does not escape from this method *)
  ignore(
      let ifs = ifields pbir vt in
      if not (FieldMap.is_empty ifs) 
      then 
	start2 (Some (vt,ifs)) pp_stack
	       prta pbir mstack
	       ms_stack cms mbir
      else ());
  let () = var_escape mbir v in 
  (* Return if everything is A-OK *)
  ClassMethodMap.add cms rr map

and others prta pbir pp_stack fs_stack map cms mstack ms_stack mbir x pc = 
  if pc >= 0 then
    match (code mbir).(pc) with
    | New (_,cn,_,_) -> (Some cn,[(pc_ir2bc mbir).(pc - 1)])
    | AffectVar (_,e) as s -> 
       hexpr prta pbir pp_stack fs_stack map cms mstack ms_stack mbir pc s e
    | _ as s -> raise (Internal ("Can't handle: " ^ (print_instr s)))
  else
    raise (Internal ("New outside the current method" ^ (print_instr x)))

and hexpr prta pbir pp_stack fs_stack map cms mstack ms_stack mbir pc x = function
  | Const c -> (None,[])
  | Var (vt, v) -> 
     fvardefpcs prta pbir pp_stack fs_stack map cms mstack ms_stack mbir pc v x
  | Unop (_,e) -> 
     hexpr prta pbir pp_stack fs_stack map cms mstack ms_stack mbir pc x e
  | Binop (_,e1,e2) -> 
     let (cn,ppl) = 
       hexpr prta pbir pp_stack fs_stack map cms mstack ms_stack mbir pc x e1 in
     let (cn2,ppl2) = 
       hexpr prta pbir pp_stack fs_stack map cms mstack ms_stack mbir pc x e2 in
     if (cn = cn2) then (cn, ppl @ ppl2)
     else raise (Internal ("BinOp: class not the same: " ))
  | Field (e,cn,fs) -> 
     if not (Enum.exists (fs_equal fs) (Stack.enum fs_stack)) then 
       let () = Stack.push fs fs_stack in
       let (cn,ret) = 
	 vfielddefpcs prta pbir pp_stack fs_stack map cms mstack ms_stack mbir pc cn fs x in
       let ret = ret |> List.flatten |> List.unique in
       ignore(Stack.pop fs_stack);
       (cn,ret)
     else (None,[])
  | StaticField (cn,fs) -> 
     if not (Enum.exists (fs_equal fs) (Stack.enum fs_stack)) then 
       let () = Stack.push fs fs_stack in
       let (cn,ret) =
	 vfielddefpcs prta pbir pp_stack fs_stack map cms mstack ms_stack mbir pc cn fs x in
       let ret = ret |> List.flatten |> List.unique in
       ignore(Stack.pop fs_stack);
       (cn,ret)
     else (None,[])

and fvardefpcs prta pbir pp_stack fs_stack map cms mstack ms_stack mbir pc v x =
  let defpcs = du mbir pc v in
  let pp = List.map (others prta pbir pp_stack fs_stack map cms mstack ms_stack mbir x) defpcs in
  let cns = List.map (fun (x,_) -> x) pp in
  let pp = List.map (fun (_,y) -> y) pp in
  let cn = List.hd cns in
  if List.for_all ((=) cn) cns then
    (cn, pp |> List.flatten |> List.unique)
  else raise (Internal ("Class types in new not the same"))

and getliveness = function
  | Var (_,v) -> v
  | _ as s -> raise (Internal ("Currently not supported: " ^ print_expr s))

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
    let res = 
      Array.fold_left 
	(fun res pc'->
	 match (code mbir).(pc') with
	 | AffectField (e,cn',fs',e') as s -> 
            let vars = [getliveness e'] in
	    let (vrescn, vres) = fvardefpcs prta pbir pp_stack fs_stack map cms mstack ms_stack mbir pc' (List.hd vars) x in
	    rescn := vrescn;
            if List.length vars = 1 then
	      (if Array.exists (fun (fs'', _) -> (List.hd vars) = fs'') fslv then
		 let fslvs = Array.filter (fun (fs'', _) -> fs'' = (List.hd vars)) fslv in
		 let opps = Array.map 
			      (fun (fs'', pc'') -> 
			       let (_,r) = fvardefpcs prta pbir pp_stack fs_stack map cms mstack ms_stack mbir pc'' fs'' x in r) fslvs in
		 let opps = opps |> Array.fold_left (@) [] |> List.fold_left (fun s x -> Ptset.add x s) Ptset.empty in
		 let vvres = List.fold_left (fun s x -> Ptset.add x s) Ptset.empty vres in
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
	    let (vrescn,vres) = fvardefpcs prta pbir pp_stack fs_stack map cms mstack ms_stack mbir pc' (List.hd vars) x in
	    rescn := vrescn;
            if List.length vars = 1 then
	      (if Array.exists (fun (fs'', _) -> (List.hd vars) = fs'') fslv then
		 let fslvs = Array.filter (fun (fs'', _) -> fs'' = (List.hd vars)) fslv in
		 let opps = Array.map 
			      (fun (fs'', pc'') -> 
			       let (_,r) = fvardefpcs prta pbir pp_stack fs_stack map cms mstack ms_stack mbir pc'' fs'' x in r) fslvs 
			    |> Array.fold_left (@) [] 
			    |> List.fold_left (fun s x -> Ptset.add x s) Ptset.empty in
		 let vvres = List.fold_left (fun s x -> Ptset.add x s) Ptset.empty vres in
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
    (!rescn,res)
  else
    if not (Stack.is_empty pp_stack) then
      let pp_stack_c = Stack.copy pp_stack in
      let mstack_c = Stack.copy mstack in
      let ms_stack_c = Stack.copy ms_stack in
      let pcc = Stack.pop pp_stack_c in
      let mbirc = Stack.pop mstack_c in
      let cmsc = Stack.pop ms_stack_c in
      let (rrcn,rr) = vfielddefpcs prta pbir pp_stack_c (Stack.create ()) map cmsc mstack_c ms_stack_c mbirc pcc cn fs x in
      let size = get_object_size pbir (fs_type fs) in
      let rr = List.map (fun x -> List.map (fun x -> (x,size)) x) rr in
      ignore(
	  let ifs = ifields pbir (fs_type fs) in
	  if not (FieldMap.is_empty ifs) 
	  then 
	    start2_others ifs prta pbir (Stack.copy pp_stack_c) cmsc 
			  (Stack.copy mstack_c) 
			  (Stack.copy ms_stack_c) mbirc pcc cn fs
	  else ());
      global_replace := (ClassMethodMap.add cmsc rr map) :: !global_replace;
      (None,[])
    else
      raise (Uninitialized ("Field: " ^ (fs_name fs))) 

and start2_others ifs prta pbir pp_stack cms mstack ms_stack mbir pc cn fs =
  let () = start2 (Some ((fs_type fs),ifs)) pp_stack
		  prta pbir mstack
		  ms_stack cms mbir in
  if not (Stack.is_empty pp_stack) then
    let cmsc = Stack.pop ms_stack in
    let mbirc = Stack.pop mstack in
    ignore(Stack.pop pp_stack);
    start2_others ifs prta pbir pp_stack cmsc mstack ms_stack mbirc pc cn fs

(* XXX: We do not do not check that class name of the new vs the field,
because this is handled at the variable level not at the field level.*)
and fielddefpcs prta vt pbir pp_stack fs_stack map cms mstack ms_stack mbir pc cn fs x =
  let (_,rr) = vfielddefpcs prta pbir pp_stack fs_stack map cms mstack ms_stack mbir pc cn fs x in
  let size = get_object_size pbir vt in
  let rr = List.map (fun x -> List.map (fun x -> (x,size)) x) rr in
  ignore(
      let ifs = ifields pbir (fs_type fs) in
      if not (FieldMap.is_empty ifs) 
      then 
	start2_others ifs prta pbir 
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
  let snpcs = Array.map (fun x -> [(bcn.(x-1),2)]) snpcs |> Array.rev in
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
		       let ndone = ref [] in
                       (* Changing the new instruction here!! *)
                       List.fold_left
			 (fun jt rl ->
			  let lnt = match jt.JCode.c_line_number_table with 
                            | Some x -> x 
                            | None -> failwith  
					("Could not find the line number table of "^(JPrint.class_method_signature cm.Javalib.cm_class_method_signature))
			  in
			  let (r,lnt) =
                            List.fold_left
                              (fun (r,lnt) (x,size) ->
			       if not (List.exists ((=) x) !ndone) then
				 (* Extend the constant pool!! *)
				 let pc = (match JProgram.to_ioc pnode with | JClass x -> x | _ -> raise (Internal "")) in
				 hADDRESS := !hADDRESS - (size+2);
				 let pool = Array.append pc.c_consts [|ConstValue (ConstInt (Int32.of_int !hADDRESS))|] in
				 pc.c_consts <- pool;
				 let cpool1 = DynArray.init (Array.length pool) (fun i -> pool.(i)) in
				 let ox = x in
				 let x = List.fold_left (fun x t -> if ox > t then x + pipi else x) x !ndone in
				 ndone := ox :: !ndone;
				 (* hADDRESS := !hADDRESS - (size+mtab_len); *)

				 (* Increasing line numbers *)
				 let lnt = List.map (fun ((bll,sll) as y) -> if bll > x then (bll+pipi,sll) else y ) lnt in
				 (* ------ done *)

				 let newinstr = r.(x) in
				 (* Change to low level format to get the index in the constant pool *)
				 (* Should be encoded in 3 bytes max *)
				 let newinstrlow = JInstruction.instruction2opcode cpool1 3 newinstr in
				 let poolindex = (match newinstrlow with 
						  | JClassLow.OpNew x -> x
						  | _ as op -> 
						     print_endline ("Looking for new opcode, found: " ^ (JDumpLow.opcode op));
						     raise (Internal ("Encode incorrectly as byte: " ^ (string_of_int x)))) in
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
				 let sa = Array.filteri (fun i _ -> (i>x+2)) r in
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
				 let xx = [|
                                     (* This is the instruction sequence that replaces new after deleting it *)
                                     JInstruction.opcode2instruction
                                       pc.c_consts (JClassLow.OpLdc1 ((Array.length pc.c_consts) - 1));
                                     OpInvalid; (* 2 bytes *)

                                     OpDup; (* 1 byte *)

                                     OpDup; (* 1 byte *)

                                     OpConst (`Byte 2); OpInvalid; (* 2 byte *)

                                     OpAdd `Int2Bool; (* 1 byte *)

                                     OpSwap; (* 1 byte *)

                                     OpInvoke ((`Static (make_cn "com.jopdesign.sys.Native")),
                                               (make_ms "wr" [(TBasic `Int);(TBasic `Int)] None));
                                     OpInvalid; OpInvalid; (* 3 bytes *)

                                     OpDup; (* 1 byte *)

                                     OpConst (`Byte 1); OpInvalid; (* 2 byte *)

                                     OpAdd `Int2Bool; (* 1 byte *)

                                     JInstruction.opcode2instruction pc.c_consts (JClassLow.OpLdc1 poolindex); 
                                     OpInvalid; (* 2 bytes *)

                                     (* OpConst (`Byte (size+mtab_len)); OpInvalid; (\* 2 bytes *\) *)
                                     OpConst (`Byte mtab_len); OpInvalid; (* 2 bytes *)

                                     OpAdd `Int2Bool; (* 1 byte *)

                                     OpSwap; (* 1 byte *)

                                     OpInvoke ((`Static (make_cn "com.jopdesign.sys.Native")),
                                               (make_ms "wr" [(TBasic `Int);(TBasic `Int)] None));
                                     OpInvalid; OpInvalid (* 3 bytes *)

				    |] in
				 (Array.append (Array.append fa xx) sa,lnt)
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
