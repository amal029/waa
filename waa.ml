open Javalib_pack
open Javalib
open JBasics
open JCode
open Joplang
module Array = BatArray
module List = BatList

let usage_msg = "Usage: waa class-path class-name [Note class-name should be given without the .class extension]"

let (|>) x f = f x

exception Internal
exception Opcode_Not_Implemented of string
exception Opcode_Java_Implemented of string

let lcmp = [|Stm;Stm;Stm;Stm;|]
let lcmp_chk_ov1 = 
  [|
    Ldm;
    Ldi;
    Shr;
    Ldm;
    Ldi;
    Shr;
    Ldi;
    Xor;
    Or;
    Nop;
    Bnz;
   |]
let lcmp_chk_ov2 = 
  [|
    Ldm;
    Ldi;
    Shr;
    Ldi;
    Xor;
    Ldm;
    Ldi;
    Shr;
    Or;
    Nop;
   |]
let lcmp_chk_ov3 = 
  [|
    Ldm;
    Ldi;
    Xor;
    Stm;
    Ldm;
    Ldi;
    Xor;
    Stm;
    Ldm;
    Ldi;
    Ushr;
    Ldm;
    Ldi;
    Ushr;
    Add;
    Ldm;
    Ldi;
    And;
    Ldm;
    Ldi;
    And;
    Add;
    Ldi;
    Add;
    Ldi;
    Shr;
    Add;
    Ldi;
    Ushr;
    Ldm;
    Add;
    Ldm;
    Add;
    Stm;
    Ldm;
    Ldm;
    Add;
    Ldi;
    Add;
    Stm;
    Ldm;
    Ldm;
    Or;
    Nop;
    Bnz;
    
    Ldm;
    Ldi;
    Shr;
    Nop;
    Bnz;
    Nop;
    Nop;
    Ldi;
   |]
let lshl = [|Ldi;And;Dup;Bnz;Nop;Nop;Nop;Pop|]
let lshl_not0 = [|Dup;Ldi;Sub;Ldi;Ushr;Nop;Bnz;Stm;Stm|]
let lshl_le31= 
  [|Stm;
    Ldm;
    Ldm;
    Shl;
    Ldm;
    Ldi;
    Ldm;
    Sub;
    Ushr;
    Add;
    Ldm;
    Ldm;
    Shl|]
let long_lcmp = Array.append lcmp lcmp_chk_ov1 |> Array.append lcmp_chk_ov2 |> Array.append lcmp_chk_ov3
let lushr = [|Ldi;And;Dup;Bnz;Nop;Nop;Pop|]
let lushr_not0 = [|Dup;Ldi;Sub;Ldi;Ushr;Nop;Bnz;Stm;Stm|]
let lushr_le31 = 
  [|
    Stm;
    Ldm;
    Ldm;
    Ushr;
    Ldm;
    Ldm;
    Ushr;
    Ldm;
    Ldi;
    Ldm;
    Sub;
    Shl;
    Add
   |]

let lshr = [|Ldi;And;Dup;Bnz;Nop;Nop;Pop;Dup;Ldi;Sub;Ldi;Ushr;Nop;Bnz;Stm;Stm|]
let lshr_le31 = 
  [|
    
    Stm;
    Ldm;
    Ldm;
    Shr;
    Ldm;
    Ldm;
    Ushr;
    Ldm;
    Ldi;
    Ldm;
    Sub;
    Shl;
    Add;
   |]

let long_add = [|
    Stm;
    Stm;
    Stm;
    Stm;
    Ldm;
    Ldi;
    Ushr;
    Ldm;
    Ldi;
    Ushr;
    Add;
    Ldm;
    Ldm;
    And;
    Ldi;
    And;
    Add;
    Ldi;
    Ushr;
    Ldm;
    Add;
    Ldm;
    Add;
    Ldm;
    Ldm;
    Add
   |]
    let generate_microcode (class_path : class_path)
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
	 | AbstractMethod _ -> None
	 | ConcreteMethod cm ->
            (match cm.cm_implementation with
             | Native -> None 	(* Native method is not supported and assumed to not have any memory access instructions*)
	     (* Increment memory access counter if memory access instruction happens, else increment instruction counter *)
             | Java code -> Some (Array.map (fun op -> 
					     let () = IFDEF DEBUG THEN if op <> OpInvalid then JPrint.jopcode op |> print_endline ELSE () ENDIF in
					     (match op with
					      | OpNew _ | OpNewArray _ (* OpNewArray == anewarray not newarray in jvm.asm *)
					      | OpCheckCast _ 
					      | OpInstanceOf _ -> 
						 [|
						   Ldjpc;
						   Ldi;
						   Stjpc;
						   Nop;
						   Nop;
						   Ldm;
						   Nop;
						   Ld_opd_8u;
						   Ldi;
						   And;
						   Dup;
						   Add;
						   Add;
						   Stm;
						   Ldm;
						   Nop;
						   Ld_opd_16u;
						   Add;
						   Stmrac;
						   Wait;
						   Wait;
						   Ldmrd;
						   Ldm;
						   Jmp;
						   Nop;
						   Nop
						  |]
					      | OpPutField _ -> 
						 [|
						   Ldjpc;
						   Ldi;
						   Sub;
						   Stjpc;
						   Nop;
						   Nop;
						   Ldm;
						   Nop;
						   Ld_opd_8u;
						   Ldi;
						   And;
						   Dup;
						   Add;
						   Add;
						   Stm;
						   Nop;
						   Nop;
						   Ld_opd_16u;
						   Ldm;
						   Jmp;
						   Nop;
						   Nop
						  |]
					      | OpPutStatic _ ->
						 [|
						   Ldjpc;
						   Ldi;
						   Sub;
						   Stjpc;
						   Nop;
						   Nop;
						   Ldm;
						   Nop;
						   Ld_opd_8u;
						   Ldi;
						   And;
						   Dup;
						   Add;
						   Add;
						   Stm;
						   Nop;
						   Nop;	
						   Ld_opd_16u;
						   Ldm;
						   Jmp;
						   Nop;
						   Nop
						  |]
					      | OpNop -> [|Nop|]
					      | OpConst x -> (match x with | `Long _ | `Double _ -> [|Ldi; Ldi|] | _ -> [|Ldi|])
					      | OpSwap -> [|Stm;Stm;Ldm;Ldm|]
					      | OpLoad (x,_) -> (match x with 
								 (* Just considers the worst possible scenario! *)
								 | `Long | `Double -> [|Ldvp;Dup;Ld_opd_8u;Add;Stvp;Stm;Ld0;Ld1;Ldm;Stvp;Nop|]
								 | _ -> [|Stald;Pop;Wait;Wait;Ldmrd|])
					      | OpStore (x,_) ->(match x with 
								 (* Just considers the worst possible scenario! *)
								 | `Long | `Double -> [|Ldvp;Dup;Ld_opd_8u;Add;Stvp;Stm;St1;St0;Ldm;Stvp;Nop|]
								 | _ -> [|Nop;St|])
					      | OpIInc _ -> [|Ldvp;Ld_opd_8u;Add;Star;Ld_opd_8u;Ldmi;Stmi|]
					      | OpPop -> [|Pop|]
					      | OpPop2 -> [|Pop;Pop|]
					      | OpDup -> [|Dup|]
					      | OpDupX1 -> [|Stm;Stm;Ldm;Ldm;Ldm;|]
					      | OpDupX2 -> [|Stm;Stm;Stm;Ldm;Ldm;Ldm;Ldm|]
					      | OpDup2 -> [|Stm;Stm;Ldm;Ldm;Ldm;Ldm|]
					      | OpDup2X1 -> [|Stm;Stm;Stm;Ldm;Ldm;Ldm;Ldm;Ldm|]
					      | OpDup2X2 -> [|Stm;Stm;Stm;Stm;Ldm;Ldm;Ldm;Ldm;Ldm;Ldm|]
					      | OpAdd x -> (match x with 
							    | `Double -> raise (Opcode_Not_Implemented (JPrint.jopcode op))
							    | `Float -> raise (Opcode_Java_Implemented (JPrint.jopcode op))
							    | `Long -> long_add
							    | _ -> [|Add|])
					      | OpSub x -> (match x with 
							    | `Double -> raise (Opcode_Not_Implemented (JPrint.jopcode op))
							    | `Float -> raise (Opcode_Java_Implemented (JPrint.jopcode op))
							    | `Long -> 
							       [|
								 Ldi;
								 Xor;
								 Stm;
								 Ldi;
								 Xor;
								 Stm;
								 Stm;
								 Stm;
								 Ldm;
								 Ldi;
								 Ushr;
								 Ldm;
								 Ldi;
								 Ushr;
								 Add;
								 Ldm;
								 Ldi;
								 And;
								 Ldm;
								 Ldi;
								 And;
								 Add;
								 Ldi;
								 Add;
								 Ldi;
								 Shr;
								 Add;
								 Ldi;
								 Ushr;
								 Ldm;
								 Add;
								 Ldm;
								 Add;
								 Ldm;
								 Ldm;
								 Add;
								 Ldi;
								 Add;
								|]
							    | _ -> [|Sub|])
					      | OpMult x -> (match x with
							     | `Double -> raise (Opcode_Not_Implemented (JPrint.jopcode op))
							     | `Float
							     | `Double -> raise (Opcode_Java_Implemented (JPrint.jopcode op))
							     | _ -> [|Stmul;Pop;Ldi|])
					      | OpDiv x | OpRem x -> (match x with
								      | `Double -> raise (Opcode_Not_Implemented (JPrint.jopcode op))
								      | _ -> raise (Opcode_Java_Implemented (JPrint.jopcode op)))
					      | OpNeg x -> (match x with 
							    | `Long -> Array.append [|Ldi;Xor;Stm;Ldi;Xor;Ldm;Ldi;Ldi|] long_add
							    | `Double -> raise (Opcode_Not_Implemented (JPrint.jopcode op))
							    | `Float -> raise (Opcode_Java_Implemented (JPrint.jopcode op))
							    | _ -> [|Ldi;Xor;Ldi;Add|])
					      | OpIShl -> [|Shl|]
					      | OpIShr -> [|Shr|]
					      | OpIUShr -> [|Ushr|]
					      | OpLShl -> Array.append (Array.append lshl lshl_not0) lshl_le31
					      | OpLShr -> (Array.append lshr lshr_le31)
					      | OpLUShr -> Array.append (Array.append lushr lushr_le31) lushr_not0
					      | OpIAnd -> [|And|]
					      | OpIOr -> [|Or|]
					      | OpIXor -> [|Xor|]
					      | OpLAnd -> [|Stm;Stm;Stm;Ldm;And;Ldm;Ldm;And|]
					      | OpLOr -> [|Stm;Stm;Stm;Ldm;Or;Ldm;Ldm;Or|]
					      | OpLXor -> [|Stm;Stm;Stm;Ldm;Xor;Ldm;Ldm;Xor|]
					      | OpI2L -> [|Dup;Stm;Ldi;Shr;Ldm|]
					      | OpI2C -> [|Ldi;And|]
					      | OpL2I -> [|Stm;Pop;Ldm|]
					      | OpGoto _ -> [|Nop;Jbr;Nop;Nop|]
					      | OpL2F -> raise (Opcode_Not_Implemented (JPrint.jopcode op))
					      | OpL2D -> raise (Opcode_Not_Implemented (JPrint.jopcode op))
					      | OpF2I -> raise (Opcode_Java_Implemented (JPrint.jopcode op))
					      | OpF2L -> raise (Opcode_Not_Implemented (JPrint.jopcode op))
					      | OpF2D -> raise (Opcode_Not_Implemented (JPrint.jopcode op))
					      | OpD2I -> raise (Opcode_Not_Implemented (JPrint.jopcode op))
					      | OpD2F -> raise (Opcode_Not_Implemented (JPrint.jopcode op))
					      | OpD2L -> raise (Opcode_Java_Implemented (JPrint.jopcode op))
					      | OpI2B -> raise (Opcode_Java_Implemented (JPrint.jopcode op))
					      | OpI2F -> raise (Opcode_Java_Implemented (JPrint.jopcode op))
					      | OpI2S -> raise (Opcode_Java_Implemented (JPrint.jopcode op))
					      | OpI2D -> raise (Opcode_Not_Implemented (JPrint.jopcode op))
					      | OpIf _ -> [|Nop;Jbr;Pop;Nop|]
					      | OpIfCmp _ -> [|Nop;Jbr;Pop;Pop|]
					      | OpCmp x -> (match x with
							    | `DL | `DG -> raise (Opcode_Not_Implemented (JPrint.jopcode op))
							    | `FL | `FG -> raise (Opcode_Java_Implemented (JPrint.jopcode op))
							    | `L -> long_lcmp
							   )
					      | OpJsr _ | OpRet _ -> [||]
					      | OpTableSwitch _ | OpLookupSwitch _ -> raise (Opcode_Java_Implemented (JPrint.jopcode op))
					      | OpGetField (_,x) -> (match (fs_type x) with
								     | TBasic x -> (match x with
										    | `Long ->
										       [|
											 Dup;
											 Nop;
											 Bz;
											 Nop;
											 Nop;
											 Stmraf;
											 Wait;
											 Wait;
											 Ldmrd;
											 Nop;
											 Nop;
											 Ld_opd_16u;
											 Add;
											 Dup;
											 Stmraf;
											 Ldi;
											 Add;
											 Stm;
											 Wait;
											 Wait;
											 Ldmrd;
											 Ldm;
											 Stmraf;
											 Wait;
											 Wait;
											 Ldmrd;
											|]
										    | _ -> [|Stgf;Nop;Wait;Wait;Ldmrd;|])
								     | _ -> [|Stgf;Nop;Wait;Wait;Ldmrd;|])
						 
					      | OpGetStatic (_,x) -> (match (fs_type x) with
								      | TBasic x -> (match x with 
										     | `Long -> [|Nop;Nop;Ld_opd_16u;Dup;
												 Stmra;Ldi;Add;Stm;Wait;Wait;
												 Ldmrd;Ldm;Stmra;Wait;Wait;Ldmrd|] 
										     | _ -> [|Stgs;Nop;Wait;Wait;Ldmrd|])
								      | _ -> [|Stgs;Nop;Wait;Wait;Ldmrd|])
					      | OpArrayLength -> [|Ldi;Add;Stmraf;Wait;Wait;Ldmrd;|]
					      | OpMonitorEnter -> [|Pop;Ldi;Stmwa;Ldi;Stmwd;Ldm;Ldm;Ldi;Add;Wait;Wait;Cinval;Stm;Ldi;
								   Stmwa;Ldi;Stmwd;Wait;Wait;Nop|]
					      | OpMonitorExit -> [|Pop;Ldm;Ldi;Sub;Dup;Stm;Bnz;Ldi;Stmwa;Ldi;Stmwd;Wait;Wait;Ldi;Stmwa;
								  Ldi;Stmwd;Wait;Wait;Nop|]
					      | OpReturn x -> (match x with
							       | `Double | `Long -> [|Stm;Stm;Dup;Stmrac;Stm;Stm;Stvp;Wait;Wait;
										     Ldmrd; Stbcrd;Stm;Nop;Stsp;Pop;Pop;Ldbcstart;Ldm;Add;
										     Stjpc;Ldm;Ldm;Wait;Wait;Nop|]
							       | `Void -> [|Dup;Stmrac;Stm;Stm;Stvp;Wait;Wait;Ldmrd;Stbcrd;Stm;Nop;
									   Stsp;Ldbcstart;Ldm;Add;Stjpc;Pop;Pop;Wait;Wait;Nop|]
							       | _ -> [|Stm;Dup;Stmrac;Stm;Stm;Stvp;Wait;Wait;Ldmrd;Stbcrd;Stm;Nop;Stsp;
								       Pop;Pop;Ldbcstart;Ldm;Add;Stjpc;Ldm;Wait;Wait;Nop|]
							      )
					      | OpAMultiNewArray _ -> raise (Opcode_Java_Implemented (JPrint.jopcode op))
					      | OpThrow -> ignore (print_endline "Throw found!!"); raise (Opcode_Java_Implemented (JPrint.jopcode op))
					      | OpBreakpoint -> raise (Opcode_Not_Implemented (JPrint.jopcode op))
					      | OpInvalid -> [||]
					    )) (Lazy.force code).c_code))) methods

    let main = 
      try 
	let args = Sys.argv in
	let (cp, cn) = 
	  if Array.length args <> 3 then let () = print_endline usage_msg in raise Internal
	  else (args.(1),args.(2)) in
	let mm = generate_microcode (class_path cp) (make_cn cn) in
	let fd = open_out (cn^".ini") in
	(* Print the (i,m) from the MethodMap *)
	MethodMap.iter (fun x (i,m) -> 
			let mss = JPrint.method_signature x in
			let ims = "[" ^ (string_of_int i) ^","^ (string_of_int m) ^ "]\n" in
			let () = output_string fd (mss ^ "\n") in
			output_string fd ims) mm
      with
      | Internal -> ()
