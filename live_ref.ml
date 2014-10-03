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
		 2.) Should be a fully qualified name, .e.g,: java.lang.Object";;

exception Internal of string
exception Not_supported of string

let signal_class_name = make_cn "systemj.lib.Signal"
let signal_set_value_ms = "setValue"

(* This is the piping operator *)
let (|>) x f = f x

(* This is the compose operator *)
let (>>) f g x = f(g x)

let du t pc v = ReachDef.Lat.get (ReachDef.run t pc) v |> Ptset.elements

let liveness t pc = Live_bir.run t pc

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


let rec getargs = function
  | Unop (_,x) -> getargs x
  | Binop (_,x,y) -> (getargs x) @ (getargs y)
  | _ as s -> [s]

let isnew mbir pc = 
  (match (code mbir).(pc) with
   | New _ 
   | NewArray _ -> true
   | _ -> false)

(* FIXME:
 1.) Need to consider if the var is an argument and not a local var
 2.) Need to consider if the var itself if being set from a field or another var.
 *)
let vardefpcs map cms mstack ms_stack mbir pc v x = 
  let defpcs = du mbir pc v in
  let lm = List.map (isnew mbir) defpcs in
  if List.fold_left (&&) true lm then 
    ClassMethodMap.add cms (List.map (fun x -> x - 1) defpcs) map
  else raise (Not_supported ("new outside of current method: " ^ (print_instr x)))

let rec start prta pbir mstack ms_stack this_ms mbir =
  (* First check if there are any setValue signal class calls in this method using the bir representation! *)
  let instrs = code mbir in
  let setSigs = Array.mapi (fun pc x ->
			       (match x with
				| InvokeVirtual (None,e1,vk,ms,el) as s -> 
				   if (isSignalSetValue ms e1) then Some (s,pc) else None
				| _ -> None)) instrs in
  let setValuepcs = Array.filter (function | Some _ -> true | None -> false) setSigs in
  let setValuepcs = Array.map (function | Some (x,y) -> (x,y) | _ -> raise (Internal "")) setValuepcs in
  let nmap =
    Array.fold_left 
      (fun elist (x,pc) ->
       (* Get the argument of the setValue method *)
       let (e1,k,ms,arg) = (match x with 
			    | InvokeVirtual (None,e1,vk,ms,el) -> (e1,vk,ms,el) 
			    | _ as s -> raise (Internal ("Set value not of type InvokeVirtual!: " ^ (print_instr s)))
			   ) in
       (* If arg is a local variable *)
       let arg = if List.length arg = 1 then List.hd arg else raise (Internal "") in
       let arg = getargs arg in
       List.fold_left (fun _ y -> 
		       (match y with
			| StaticField (cn,fs) -> 
			   let () = print_endline "it be a static field" in
			   elist
			| Field (e,cn,fs) -> 
			   let () = print_endline "It be a non-static field!" in
			   elist
			| Var (vt,v) -> 
			   let ret = vardefpcs elist this_ms mstack ms_stack mbir pc v x in
			   let () = List.iter (print_endline >> string_of_int) 
					      (List.flatten (ClassMethodMap.value_elements ret)) in 
			   ret
			| Const x -> elist
			| _ as s -> 
			   raise (Internal ("Setting a non-field, var type value in setValue: " ^ (print_expr s)))
		      )) ClassMethodMap.empty arg) ClassMethodMap.empty setValuepcs in

  (* TODO: simulate heap here! *)
  (* REMEMBER when replacing new with loads, make sure that they have
  not already been replaced *)

  
  (* Invoke each method call separately for each invoke bytecode *)
  Array.iter (function
	       | InvokeStatic (_,cn,ms,_) -> 
		  invoke_method prta pbir cn ms mbir mstack ms_stack this_ms
	       | InvokeVirtual (_,_,VirtualCall (TClass cn),ms,el) -> 
		  invoke_method prta pbir cn ms mbir mstack ms_stack this_ms
	       | InvokeVirtual (_,_,VirtualCall (TArray cn),ms,el) -> raise (Internal "")
	       | InvokeVirtual (_,_,InterfaceCall cn,ms,el) -> 
		  invoke_method prta pbir cn ms mbir mstack ms_stack this_ms
	       | InvokeNonVirtual(_,_,cn,ms,_) ->
		  invoke_method prta pbir cn ms mbir mstack ms_stack this_ms
	       | _ -> ()) instrs

and invoke_method prta pbir cn ms mbir mstack ms_stack this_ms = 
  let cmi = JProgram.get_concrete_method (JProgram.get_node pbir cn) ms in
  let () = Stack.push mbir mstack in
  let () = Stack.push this_ms ms_stack in
  let _ = map_concrete_method ~force:true (start prta pbir mstack ms_stack (cmi.cm_class_method_signature)) cmi in
  let _ = Stack.pop mstack in 
  ignore(Stack.pop ms_stack)

let main =
  try
    let args = Sys.argv in
    let (cp, cn) =
      if Array.length args <> 3 then let () = print_endline usage_msg in raise (Internal "")
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
    (* let mobj = JProgram.get_concrete_method obj (make_ms "MethodCall1_0" [] (Some (TBasic `Bool)))in *)
    (* Try doing liveness analysis *)
    let ss = Stack.create () in
    let ms_ss = Stack.create () in
    ignore(map_concrete_method ~force:true (start prta pbir ss ms_ss (mobj.cm_class_method_signature)) mobj);
    (* let _ = map_concrete_method ~force:true liveness mobj in *)
    (* let () = print_endline "----------------------" in *)
    (* Try doing possible definition analysis *)
    (* let _ = map_concrete_method ~force:true du mobj in *)
    
    JPrint.print_class (JProgram.to_ioc obj) JBir.print stdout
  with 
  | Internal _ -> ()
