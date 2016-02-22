module Stack = BatStack
module Array = BatArray
module Hashtbl = BatHashtbl
open Javalib_pack
open ExtLib
open BatPervasives
open LibWcma
open Javalib
open JBasics

let addms = ref false;;
let usage_msg = "Usage: wcma [-sourcepath <filename>] [OPTION] class-path class-name
Note:
1.) Class-name should be given without the .class extension
2.) Should be a fully qualified name, .e.g,: java.lang.Object";;

let main = 
  let args = DynArray.make 2 in
  let ff = Sys.getcwd () in
  let sourcep = ref "" in
  let speclist = [
    ("-sourcepath", Arg.String (fun x -> sourcep := x), "Source path for parsing loop count");
    ("-m", Arg.Set addms, "Add execution times of Java implemented bytecodes")
  ] in
  let () = Arg.parse speclist (fun x -> DynArray.add args x) (usage_msg^"\n[OPTION]:") in
  let l = parsewca !sourcep in 
  let (cp, cn) = 
    if DynArray.length args <> 2 then let () = print_endline usage_msg; Arg.usage speclist "[OPTION]:" in exit 1
    else (DynArray.get args 0,DynArray.get args 1) in
  let mm = internal_main cp cn l !addms in
  let () = Sys.chdir ff in
  let fd = open_out (cn^".ini") in
  DynArray.iter (fun (x,(i,m)) -> 
      let () = output_string fd ((JPrint.class_method_signature x) ^ "\n") in
      output_string fd ("[" ^ (BatNum.to_string i) ^","^ (BatNum.to_string m) ^ "]\n")) mm;
  print_endline (Sys.getcwd ());
  close_out fd;
