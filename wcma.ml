module Stack = BatStack
module Array = BatArray
module Hashtbl = BatHashtbl
open Javalib_pack
open ExtLib
open BatPervasives
open LibWcma
open Javalib
open JBasics

let bj3 = ref "";;
let addmethods = ref false;;
let cpp = ref ""
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
    ("-m", Arg.Set addmethods, "Add execution times of Java implemented bytecodes")
  ] in
  let () = Arg.parse speclist (fun x -> DynArray.add args x) (usage_msg^"\n[OPTION]:") in
  let l = parsewca !sourcep in 
  let (cp, cn) = 
    if DynArray.length args <> 2 then let () = print_endline usage_msg; Arg.usage speclist "[OPTION]:" in raise (Internal "")
    else (DynArray.get args 0,DynArray.get args 1) in
  cpp := cp;
  let marray = DynArray.make 100 in
  let jfk = Stack.create () in
  bj3 := cn;
  let () = generate_microcode_clazz marray (JFile.class_path cp) (make_cn cn) l in 
  let marray = DynArray.map (fun (mn,vals) -> (mn,Array.map(fun(x,y,z) -> (x,BatNum.of_int y,z))vals)) marray in
  let mm = DynArray.map (fun (mn,vals) -> (JPrint.class_method_signature mn,
                                           calc_exec_time jfk (mn,vals) marray (Hashtbl.create 50)) ) marray in
  let () = Sys.chdir ff in
  let fd = open_out (cn^".ini") in
  DynArray.iter (fun (x,(i,m)) -> 
      let () = output_string fd (x ^ "\n") in
      output_string fd ("[" ^ (BatNum.to_string i) ^","^ (BatNum.to_string m) ^ "]\n")) mm;
  print_endline (Sys.getcwd ());
  close_out fd;
