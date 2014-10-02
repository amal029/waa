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

exception Internal

let signal_class_name = make_cn "systemj.lib.Signal";;
let signal_set_value_ms = "setValue";;

(* let du t =  *)
(*   (\* let bc2irn = Ptmap.elements (JBir.pc_bc2ir t) in *\) *)
(*   (\* let () = List.iter (fun (x,y) -> print_endline ((string_of_int x) ^ " bc --- ir " ^ (string_of_int y))) bc2irn in *\) *)
(*   let lnums = JBir.pc_ir2bc t in *)
(*   let () = Array.iteri (fun i x -> print_endline ((string_of_int i) ^ "--" ^ (string_of_int x))) lnums in *)
(*   let instrs = JBir.code t in *)
(*   let () = Array.iter (function  *)
(* 			| JBir.InvokeVirtual (None,e,k,ms,args) ->  *)
(* 			| _ -> ()) instrs in *)
(*   let instrs = Array.length instrs in *)
(*   let lives = Array.init instrs (ReachDef.run t) in *)
(*   let lives = Array.map (ReachDef.Lat.to_string t) lives in *)
(*   Array.iteri (fun i x -> print_endline ((string_of_int i) ^ ":"^ x)) lives *)

(* let liveness t =  *)
(*   let instrs = Array.length (JBir.code t) in *)
(*   let lives = Array.init instrs (Live_bir.run t) in *)
(*   let lives = Array.map Live_bir.to_string lives in *)
(*   Array.iteri (fun i x -> print_endline ((string_of_int i) ^ ":"^ x)) lives *)

let rec isSignalSetValue ms = function
  | StaticField (cn,fs) -> (ms_name ms = signal_set_value_ms) && (cn = signal_class_name)
  | Field (e,cn,fs) -> (ms_name ms = signal_set_value_ms) && (cn = signal_class_name)
  | Var (vt,v) -> (match vt with
		   | TObject (TClass cn) -> (ms_name ms = signal_set_value_ms) && (cn = signal_class_name)
		   | _ -> false)
  | Binop (_,e1,e2) -> isSignalSetValue ms e1 || isSignalSetValue ms e2
  | Unop (_,e1) -> isSignalSetValue ms e1 
  | Const _ -> false


let rec start prta pbir mstack mbir =
  (* First check if there are any setValue signal class calls in this method using the bir representation! *)
  let instrs = code mbir in
  let setSigs = Array.filter (function
			 | InvokeVirtual (None,e1,vk,ms,el) -> isSignalSetValue ms e1 
			 | _ -> false) instrs in
  (* Now just print this *)
  let () = Array.iter (fun x -> print_endline (print_instr x)) setSigs in
  (* Invoke each method call separately for each invoke bytecode *)
  let () = Array.iter (function
			| InvokeStatic (_,cn,ms,_) ->
			   ignore(map_concrete_method ~force:true (start prta pbir mstack) (JProgram.get_concrete_method (JProgram.get_node pbir cn) ms))
			| InvokeVirtual (_,_,VirtualCall (TClass cn),ms,el) -> 
			   ignore(map_concrete_method ~force:true (start prta pbir mstack) (JProgram.get_concrete_method (JProgram.get_node pbir cn) ms))
			| InvokeVirtual (_,_,VirtualCall (TArray cn),ms,el) -> raise Internal
			| InvokeVirtual (_,_,InterfaceCall cn,ms,el) -> 
			   ignore(map_concrete_method ~force:true (start prta pbir mstack) (JProgram.get_concrete_method (JProgram.get_node pbir cn) ms))
			| InvokeNonVirtual(_,_,cn,ms,_) ->
			   ignore(map_concrete_method ~force:true (start prta pbir mstack) (JProgram.get_concrete_method (JProgram.get_node pbir cn) ms))
			| _ -> ()) instrs in
  ()

let main =
  try
    let args = Sys.argv in
    let (cp, cn) =
      if Array.length args <> 3 then let () = print_endline usage_msg in raise Internal
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
    ignore(map_concrete_method ~force:true (start prta pbir (Stack.push mobj)) mobj);
    (* let _ = map_concrete_method ~force:true liveness mobj in *)
    (* let () = print_endline "----------------------" in *)
    (* Try doing possible definition analysis *)
    (* let _ = map_concrete_method ~force:true du mobj in *)
    
    JPrint.print_class (JProgram.to_ioc obj) JBir.print stdout
  with 
  | Internal -> ()
