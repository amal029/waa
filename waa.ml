open Javalib_pack
open Javalib
open JBasics
open JCode
module Array = BatArray
module List = BatList

let usage_msg = "Usage: waa class-path class-name [Note class-name should be given without the .class extension]"

let (|>) x f = f x

exception Internal

let get_counts (class_path : class_path)
			(cn : class_name) =
  (* We first recover the interface or class associated to the
    class name cn. *)
  let c = get_class class_path cn in
  (* Then, we get all the methods of c. *)
  let methods = get_methods c in
  (* For each method of c, we associate a field set containing
    all the accessed fields. *)
  MethodMap.map
    (fun m ->
     match m with
     (* A method can be abstract or concrete. *)
     | AbstractMethod _ -> (0,0)
     | ConcreteMethod cm ->
        (match cm.cm_implementation with
         | Native -> (0,0) 	(* Native method is not supported and assumed to not have any memory access instructions*)
	 (* Increment memory access counter if memory access instruction happens, else increment instruction counter *)
         | Java code -> Array.fold_left (fun (i,m) op -> 
					 (match op with
					  | OpLoad _ | OpStore _ | OpGetField _ | OpPutField _ 
					  | OpArrayLength | OpArrayStore _ | OpArrayLoad _ | OpInvoke _
					  | OpReturn _ | OpGetStatic _ 
					  | OpPutStatic _ ->  (i,m+1)
					  | _ -> (i+1,m))
					) (0,0)  (Lazy.force code).c_code)) methods

let main = 
  try 
    let args = Sys.argv in
    let (cp, cn) = 
      if Array.length args <> 3 then let () = print_endline usage_msg in raise Internal
      else (args.(1),args.(2)) in
    let mm = get_counts (class_path cp) (make_cn cn) in
    let fd = open_out (cn^".ini") in
    (* Print the (i,m) from the MethodMap *)
    MethodMap.iter (fun x (i,m) -> 
		    let mss = JPrint.method_signature x in
		    let ims = "[" ^ (string_of_int i) ^","^ (string_of_int m) ^ "]\n" in
		    let () = output_string fd (mss ^ "\n") in
		    output_string fd ims) mm
  with
  | Internal -> ()
