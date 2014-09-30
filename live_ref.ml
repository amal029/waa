open Javalib_pack
open Javalib
open JBasics
open JCode
open Sawja_pack
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

let du t = 
  let instrs = Array.length (JBir.code t) in
  let lives = Array.init instrs (ReachDef.run t) in
  let lives = Array.map (ReachDef.Lat.to_string t) lives in
  Array.iteri (fun i x -> print_endline ((string_of_int i) ^ ":"^ x)) lives

let liveness t = 
  let instrs = Array.length (JBir.code t) in
  let lives = Array.init instrs (Live_bir.run t) in
  let lives = Array.map Live_bir.to_string lives in
  Array.iteri (fun i x -> print_endline ((string_of_int i) ^ ":"^ x)) lives

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
    (* let mobj = JProgram.get_concrete_method obj JProgram.main_signature in *)
    let mobj = JProgram.get_concrete_method obj (make_ms "MethodCall1_0" [] (Some (TBasic `Bool)))in
    (* Try doing liveness analysis *)
    let _ = map_concrete_method ~force:true liveness mobj in
    let () = print_endline "----------------------" in
    (* Try doing possible definition analysis *)
    let _ = map_concrete_method ~force:true du mobj in
    
    JPrint.print_class (JProgram.to_ioc obj) JBir.print stdout
  with 
  | Internal -> ()
