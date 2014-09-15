open Javalib_pack
open Javalib
open JBasics
open JCode
open Joplang
open Sawja_pack
module Array = BatArray
module Enum = BatEnum

let usage_msg = "Usage: waa class-path class-name
[Note
 1.) Class-name should be given without the .class extension
 2.) Should be a fully qualified name, .e.g,: java.lang.Object
 3.) Should contain the main method]";;

let (|>) x f = f x;;

set_permissive true;;


exception Internal
exception Opcode_Not_Implemented of string
exception Opcode_Java_Implemented of string
exception Opcode_Not_Bounded of string

(* Classes that implement bytecodes in Java *)
let bj1 = "com.jopdesign.sys.JVM";;
let bj2 = "com.jopdesign.sys.GC";;

let monitorexit = [|Pop;Ldm;Ldi;Sub;Dup;Stm;Bnz;Ldi;Stmwa;Ldi;Stmwd;Wait;Wait;Ldi;Stmwa;
							       Ldi;Stmwd;Wait;Wait;Nop|];;

let monitorenter = [|Pop;Ldi;Stmwa;Ldi;Stmwd;Ldm;Ldm;Ldi;Add;Wait;Wait;Cinval;Stm;Ldi;
								Stmwa;Ldi;Stmwd;Wait;Wait;Nop|];;
let iaload = [|Stald;Pop;Wait;Wait;Ldmrd|];;
let iastore = [|Stast;Pop;Pop;Wait;Wait;Nop|];;
let arraylength = [|Ldi;Add;Stmraf;Wait;Wait;Ldmrd;|];;

let jopsys_putstatic = [|Stidx;Stps;Wait;Wait;Nop|];;
let jopsys_getstatic = [|Stidx;Stgs;Wait;Wait;Ldmrd|];;
let jopsys_getfield = [|Stidx;Stgf;Wait;Wait;Ldmrd|];;
let jopsys_putfield = [|Stm;Stidx;Ldm;Stpf;Wait;Wait;Pop|];;

let jopsys_rd = [|Stmra;Wait;Wait;Ldmrd|];;
let jopsys_rdint = [|Star;Nop;Ldmi|];;
let jopsys_wr = [|Stmwa;Stmwd;Wait;Wait;Nop|];;
let jopsys_wrint = [|Star;Nop;Stmi|];;
let jopsys_getsp = [|Ldsp;Ldi;Add|];;
let jopsys_setsp = [|Nop;Stsp;Pop;Pop|];;
let jopsys_getvp = [|Ldvp|];;
let jopsys_setvp = [|Stvp;Nop|];;

(* This has a loop, which is not bounded!! *)
let jopsys_int2ext op = raise (Opcode_Not_Bounded (JPrint.jopcode op));;
let jopsys_ext2int op = jopsys_int2ext op;;

let jopsys_memcpy = [|Stcp;Pop;Wait;Wait;Pop|]
let jopsys_nop = [|Nop|];;
let jopsys_cond_move = [|Nop;Bz;Stm;Stm;Ldm;Ldm|];;
let jopsys_inval = [|Cinval;Nop;Nop;Nop|];;
let jopsys_count = [|Cyc;Pop|];;
let jopsys_hc = [|Ldcr;Stm;Stm;Ldm;Ldm;Pop|];;

let invoke_vpsave =
  [|
    Dup; Ldi;Add;Stmrac; Ldm; Stm; Stm;Wait;Wait;Ldmrd;Ldjpc;
    Ldbcstart; Sub; Stm; Ldm;Stmrac; Ldm; Stm;Wait;Wait;Ldmrd;Stbcrd; Dup; Ldi; And; Stm; Ldi;Ushr; Dup; Ldi; And; Stm; Ldi; Ushr; Stm; Ldsp; Ldi;
    Add; Dup; Ldm; Sub; Stm; Ldm; Ldi; Add; Stvp; Ldm; Add; Nop; Stsp; Pop; Pop; Ldm; Ldm; Ldbcstart; Stjpc; Ldm; Ldm; Ldm; Wait; Wait; Nop
   |];;

let invoke_ok = 
  Array.append [|
      Ldm; 			(* invoke_addoffset *)
      Add
     |] invoke_vpsave ;;

let invokestatic_mc = 
  Array.append [|
      Ldm; Nop;Ld_opd_16u; Add;Stmrac;Wait;Wait;Ldmrd;Jmp
     |] invoke_vpsave;;

let invokevirtual_mc = 
  Array.append [|
      Ldm;Nop;
      Ld_opd_16u;Add;Stmrac;Wait;Wait;Ldmrd;Dup;Ldi;And;Stm;Ldi;Ushr;Stm;Ldsp;Ldi;Add;Ldm;Sub;Star;Nop;Ldmi;Dup;Nop;Bnz;Ldi;Add;Stmraf;Wait;Wait;Ldmrd
     |] invoke_ok;;

let invokeinterface_mc = 
  Array.append [|
      Ldm;Nop;Ld_opd_16u;Add;Stmrac;Wait;
      Wait;Ldmrd;Dup;Ldi;And;Stm;Ldi;Ushr;Stm;Ldsp;Ldi;Add;Ldm;Sub;Star;Nop;Ldmi;Dup;Nop;Bnz;Ldi;Add;Stmraf;Wait;Wait;Ldmrd;Ldi;Sub;Stmrac;Wait;Wait;Ldmrd;Ldm;Add;
      Stmrac;Wait;Wait;Ldmrd;Jmp;Nop;Nop;Ldm;Dup;Ld_opd_16u;Add;Stmrac;Wait;Wait;Ldmrd;Dup;Ldi;And;Stm;Ldi;Ushr;Stm;Ldsp;Ldi;Add;Ldm;
      Sub;Star;Nop;Ldmi;Nop;Nop;Bz;Ldvp;Stm;Ldi;Sub;Stmraf;Wait;Wait;Ldmrd;Ldi;Add;Stmrac;Wait;Wait;Ldmrd;Ldi;Add;Jmp
     |] invoke_ok;;

let lcmp = [|Stm;Stm;Stm;Stm;|];;
let lcmp_chk_ov1 = 
  [|
    Ldm;Ldi;Shr;Ldm;Ldi;Shr;Ldi;Xor;Or;Nop;Bnz;
   |]
let lcmp_chk_ov2 = 
  [|
    Ldm;Ldi;Shr;Ldi;Xor;Ldm;Ldi;Shr;Or;Nop;
   |]
let lcmp_chk_ov3 = 
  [|
    Ldm;Ldi;Xor;Stm;Ldm;Ldi;Xor;Stm;Ldm;Ldi;Ushr;Ldm;Ldi;Ushr;Add;Ldm;Ldi;And;Ldm;Ldi;And;Add;
    Ldi;Add;Ldi;Shr;Add;Ldi;Ushr;Ldm;Add;Ldm;Add;Stm;Ldm;Ldm;Add;Ldi;Add;Stm;Ldm;Ldm;Or;Nop;Bnz;Ldm;Ldi;Shr;Nop;Bnz;Nop;Nop;Ldi;
   |];;
let lshl = [|Ldi;And;Dup;Bnz;Nop;Nop;Nop;Pop|];;
let lshl_not0 = [|Dup;Ldi;Sub;Ldi;Ushr;Nop;Bnz;Stm;Stm|];;
let lshl_le31= 
  [|Stm;Ldm;Ldm;Shl;Ldm;Ldi;Ldm;Sub;Ushr;Add;Ldm;Ldm;Shl|]
let long_lcmp = Array.append lcmp lcmp_chk_ov1 |> Array.append lcmp_chk_ov2 |> Array.append lcmp_chk_ov3
let lushr = [|Ldi;And;Dup;Bnz;Nop;Nop;Pop|]
let lushr_not0 = [|Dup;Ldi;Sub;Ldi;Ushr;Nop;Bnz;Stm;Stm|]
let lushr_le31 = 
  [|Stm;Ldm;Ldm;Ushr;Ldm;Ldm;Ushr;Ldm;Ldi;Ldm;Sub;Shl;Add|];;

let lshr = [|Ldi;And;Dup;Bnz;Nop;Nop;Pop;Dup;Ldi;Sub;Ldi;Ushr;Nop;Bnz;Stm;Stm|];;
let lshr_le31 = 
  [|Stm;Ldm;Ldm;Shr;Ldm;Ldm;Ushr;Ldm;Ldi;Ldm;Sub;Shl;Add;|];;

let long_add = [|Stm;Stm;Stm;Stm;Ldm;Ldi;Ushr;Ldm;Ldi;Ushr;Add;Ldm;Ldm;And;Ldi;And;Add;Ldi;Ushr;Ldm;Add;Ldm;Add;Ldm;Ldm;Add|];;

let rec opcode2microcode clazz cp ((program,_) as ss) = function
   | OpNop -> [|Nop|] 		(* This is just Nop *)

   | OpConst x as op -> 
      let oop = JInstruction.code2opcodes cp [|op|] in
      let () = Array.iter (fun x -> JDumpLow.opcode x |> print_endline) oop in
      (match x with 
       | `ANull -> [|Ldi|]
       | `Int _ |`Float _ -> [|Ldi|]
       | `Long _ | `Double _ -> [|Ldi; Ldi|] 
       | `Byte _ -> [|Nop;Ld_opd_8u|]
       | `Short _ -> [|Nop;Nop;Ld_opd_8u|]
       | `String _ -> [|Ldm;Ld_opd_8u;Add;Stmrac;Wait;Wait;Ldmrd|]
       | `Class _ -> raise (Opcode_Not_Implemented (JPrint.jopcode op)))


   | OpNew _ | OpNewArray _ (* OpNewArray == anewarray not newarray in jvm.asm *)
   | OpCheckCast _ 
   | OpInstanceOf _ -> 
      [|
	Ldjpc; Ldi;
	Stjpc; Nop; Nop; Ldm;
	Nop; Ld_opd_8u; Ldi; And; Dup; Add; Add; Stm; Ldm; Nop; Ld_opd_16u; Add; Stmrac; Wait; Wait; Ldmrd; Ldm; Jmp; Nop;Nop
       |]
   | OpPutField (_,x) -> (match (fs_type x) with
			  | TBasic x -> (match x with
					 | `Long -> [|Stm;Stm;Dup;Nop;Bz;Nop;Nop;Stmraf;
						      Wait;Wait;Ldmrd;Nop;Nop;Ld_opd_16u;
						      Add;Dup;Stmraf;Ldi;Add;Stm;Wait;Wait;Ldmrd;
						      Ldm;Stmraf;Wait;Wait;Ldmrd|]
					 | _ ->  [|
						Ldjpc; Ldi; Sub; Stjpc; Nop; Nop; Ldm; Nop;
						Ld_opd_8u; Ldi; And; Dup; Add; Add; Stm; Nop;
						Nop; Ld_opd_16u; Ldm; Jmp; Nop; Nop
					       |])
			  | _ -> 
			     [|Ldjpc;
			       Ldi;Sub;Stjpc;Nop;Nop;Ldm;Nop;Ld_opd_8u;Ldi;And;Dup;Add;Add;Stm;Nop;Nop;Ld_opd_16u;Ldm;Jmp;Nop;Nop
			      |])
   | OpPutStatic (_,x) ->
      (match (fs_type x) with
       | TBasic x -> (match x with
		      | `Long -> [|Stm;Stm;Ld_opd_16u;Dup;Stmwa;Ldm;Stmwd;Ldi;Add;Wait;Wait;
				   Stmwa;Ldm;Stmwd;Wait;Wait;Nop|]
		      | _ -> [|
			     Ldjpc;Ldi;Sub;Stjpc;Nop;Nop;Ldm;Nop;Ld_opd_8u;Ldi;And;Dup;Add;Add;Stm;Nop;Nop;
			     Ld_opd_16u;Ldm;Jmp;Nop;Nop
			    |])
       | _ -> [|Ldjpc;Ldi;Sub;
		Stjpc;Nop;Nop;Ldm;Nop;Ld_opd_8u;Ldi;And;Dup;Add;Add;Stm;Nop;Nop;	Ld_opd_16u;Ldm;Jmp;Nop;Nop
	       |])
   | OpSwap -> [|Stm;Stm;Ldm;Ldm|]
   | OpLoad (x,_) -> 
      (match x with 
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
   | OpAdd x as op -> (match x with 
		 | `Double -> raise (Opcode_Not_Implemented (JPrint.jopcode op))
		 | `Float -> raise (Opcode_Java_Implemented (JPrint.jopcode op))
		 | `Long -> long_add
		 | _ -> [|Add|])
   | OpSub x as op -> (match x with 
		 | `Double -> raise (Opcode_Not_Implemented (JPrint.jopcode op))
		 | `Float -> raise (Opcode_Java_Implemented (JPrint.jopcode op))
		 | `Long -> 
		    [|Ldi;Xor;Stm;Ldi;Xor;Stm;Stm;Stm;Ldm;Ldi;Ushr;Ldm;
		      Ldi;Ushr;Add;Ldm;Ldi;And;Ldm;Ldi;And;Add;Ldi;Add;Ldi;Shr;Add;Ldi;Ushr;Ldm;Add;Ldm;Add;Ldm;Ldm;Add;Ldi;Add|]
		 | _ -> [|Sub|])
   | OpMult x as op-> (match x with
		  | `Double -> raise (Opcode_Not_Implemented (JPrint.jopcode op))
		  | `Long | `Float -> raise (Opcode_Java_Implemented (JPrint.jopcode op))
		  | _ -> Array.init 35 (fun _ -> Nop)) (* Note that imul never access external memory!! *)
   | OpDiv x as op -> (match x with
		 | `Double -> raise (Opcode_Not_Implemented (JPrint.jopcode op))
		 | _ -> let mn = make_ms "f_idiv" [(TBasic `Int);(TBasic `Int)] (Some (TBasic `Int)) in
			(* let cn = JProgram.get_node program (make_cn bj1) in *)
			let cn = try JProgram.get_node program (make_cn bj1) with 
				 | Not_found -> print_endline (JPrint.jopcode op); raise Not_found in
			let m = JProgram.get_method cn mn in
			(* This is a crappy hack, and leads to significant over-approximation *)
			(* FIXME *)
			(* The value 32 comes from the fact that idiv has a loop bound of 32 *)
			let tt = lazy (get_microcode clazz cp ss m) in
			let r = Array.init 32 (fun _ -> match Lazy.force tt with 
							| Some x -> Array.fold_left Array.append [||] x
							| None -> raise (Opcode_Not_Implemented (JPrint.jopcode op))) in
			Array.fold_left Array.append [||] r)
   | OpRem x as op -> (match x with
		 | `Double -> raise (Opcode_Not_Implemented (JPrint.jopcode op))
		 | _ -> let mn = make_ms "f_idiv" [(TBasic `Int);(TBasic `Int)] (Some (TBasic `Int)) in
			(* let cn = JProgram.get_node program (make_cn bj1) in *)
			let cn = try JProgram.get_node program (make_cn bj1) with 
				 | Not_found -> print_endline (JPrint.jopcode op); raise Not_found in
			let m = JProgram.get_method cn mn in
			match (get_microcode clazz cp ss m) with
			| Some x -> Array.fold_left Array.append [||] x
			| None -> raise (Opcode_Not_Implemented (JPrint.jopcode op)))
   | OpNeg x as op -> (match x with 
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
   | OpL2F as op -> raise (Opcode_Not_Implemented (JPrint.jopcode op))
   | OpL2D as op -> raise (Opcode_Not_Implemented (JPrint.jopcode op))
   | OpF2I as op -> raise (Opcode_Java_Implemented (JPrint.jopcode op))
   | OpF2L as op -> raise (Opcode_Not_Implemented (JPrint.jopcode op))
   | OpF2D as op -> raise (Opcode_Not_Implemented (JPrint.jopcode op))
   | OpD2I as op -> raise (Opcode_Not_Implemented (JPrint.jopcode op))
   | OpD2F as op -> raise (Opcode_Not_Implemented (JPrint.jopcode op))
   | OpD2L as op -> raise (Opcode_Java_Implemented (JPrint.jopcode op))
   | OpI2B as op -> raise (Opcode_Java_Implemented (JPrint.jopcode op))
   | OpI2F as op -> raise (Opcode_Java_Implemented (JPrint.jopcode op))
   | OpI2S as op -> raise (Opcode_Java_Implemented (JPrint.jopcode op))
   | OpI2D as op -> raise (Opcode_Not_Implemented (JPrint.jopcode op))
   | OpIf _ -> [|Nop;Jbr;Pop;Nop|]
   | OpIfCmp _ -> [|Nop;Jbr;Pop;Pop|]
   | OpCmp x as op -> (match x with
		 | `DL | `DG -> raise (Opcode_Not_Implemented (JPrint.jopcode op))
		 | `FL | `FG -> raise (Opcode_Java_Implemented (JPrint.jopcode op))
		 | `L -> long_lcmp)
   | OpJsr _ | OpRet _ -> [||]
   | OpTableSwitch _ as op -> raise (Opcode_Not_Implemented (JPrint.jopcode op))
   (* TODO: When the Java method is called we need to add the load time for the method cache *)
   | OpLookupSwitch _ as op -> let mn = make_ms "f_lookupswitch" [(TBasic `Int)] None in
			 let cn = try JProgram.get_node program (make_cn bj1) with 
				  | Not_found -> print_endline (JPrint.jopcode op); 
						 print_endline ("class: " ^ bj1 ^ " Not_found");
						 raise Not_found in
			 let m = JProgram.get_method cn mn in
			 (match (get_microcode clazz cp ss m) with
			  | Some x -> Array.fold_left Array.append [||] x
			  | None -> raise (Opcode_Not_Implemented (JPrint.jopcode op)))
   | OpGetField (_,x) -> (match (fs_type x) with
			  | TBasic x -> (match x with
					 | `Long ->
					    [|Dup;Nop;Bz;Nop;Nop;Stmraf;Wait;Wait;Ldmrd;Nop;
					      Nop;Ld_opd_16u;Add;Dup;Stmraf;Ldi;Add;Stm;Wait;Wait;Ldmrd;Ldm;Stmraf;Wait;Wait;Ldmrd;
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
   | OpArrayLength -> arraylength 
   | OpMonitorEnter -> monitorenter
   | OpMonitorExit -> monitorexit
   | OpReturn x -> (match x with
		    | `Double | `Long -> [|Stm;Stm;Dup;Stmrac;Stm;Stm;Stvp;Wait;Wait;
					   Ldmrd; Stbcrd;Stm;Nop;Stsp;Pop;Pop;Ldbcstart;Ldm;Add;
					   Stjpc;Ldm;Ldm;Wait;Wait;Nop|]
		    | `Void -> [|Dup;Stmrac;Stm;Stm;Stvp;Wait;Wait;Ldmrd;Stbcrd;Stm;Nop;
				 Stsp;Ldbcstart;Ldm;Add;Stjpc;Pop;Pop;Wait;Wait;Nop|]
		    | _ -> [|Stm;Dup;Stmrac;Stm;Stm;Stvp;Wait;Wait;Ldmrd;Stbcrd;Stm;Nop;Stsp;
			     Pop;Pop;Ldbcstart;Ldm;Add;Stjpc;Ldm;Wait;Wait;Nop|])
   (* TODO: Need to find out the size of the method and add that as memory cache fill time *)
   | OpInvoke (x,y) as op -> (match x with
			| `Interface _ -> invokeinterface_mc
			| `Virtual _ -> invokevirtual_mc
			| `Special _ -> invokestatic_mc
			| `Static cn -> 
			   let cn = cn_simple_name cn in
			   let mn = ms_name y in
			   (* let () = IFDEF DEBUG THEN print_endline cn ELSE () ENDIF in *)
			   (* let () = IFDEF DEBUG THEN print_endline mn ELSE () ENDIF in *)
			   if cn = "Native" then
			     (match mn with
			      | "rd" -> jopsys_rd
			      | "wr" -> jopsys_wr
			      | "wrMem" -> jopsys_wr
			      | "rdMem" -> jopsys_rd
			      | "rdIntMem" -> jopsys_rdint
			      | "wrIntMem" -> jopsys_wrint
			      | "getSP" -> jopsys_getsp
			      | "getVP" -> jopsys_getvp
			      | "setSP" -> jopsys_setsp
			      | "setVP" -> jopsys_setvp
			      | "int2extMem" -> jopsys_int2ext op
			      | "ext2intMem" -> jopsys_ext2int op
			      | "makeLong" -> jopsys_nop
			      | "invoke" -> invoke_vpsave
			      | "toInt" -> jopsys_nop
			      | "toFloat" -> jopsys_nop
			      | "toObject" -> jopsys_nop
			      | "toIntArray" -> jopsys_nop
			      | "toLong" -> jopsys_nop
			      | "toDouble" -> jopsys_nop
			      | "monitorExit" -> monitorexit
			      | "monitorEnter" -> monitorenter
			      | "condMove" -> jopsys_cond_move
			      | "condMoreRef" -> jopsys_cond_move
			      | "invalidate" -> jopsys_inval
			      | "memCopy" -> jopsys_memcpy
			      | "putStatic" -> jopsys_putstatic
			      | "getStatic" -> jopsys_getstatic
			      | "putField" -> jopsys_putfield
			      | "getField" -> jopsys_getfield
			      | "arrayLoad" -> iaload
			      | "arrayStore" -> iastore
			      | "arrayLength" -> arraylength
			      | "count" -> jopsys_count
			      | "hc" -> jopsys_hc
			      | _ -> invokestatic_mc)
			   else invokestatic_mc)
   | OpAMultiNewArray _ as op -> raise (Opcode_Java_Implemented (JPrint.jopcode op))
   | OpThrow as op -> 
      let mn = make_ms "f_athrow" [TObject (TClass (make_cn "java.lang.Throwable"))]
		       (Some (TObject (TClass (make_cn "java.lang.Throwable")))) in
      let cn = try JProgram.get_node program (make_cn bj1) with 
	       | Not_found -> print_endline (JPrint.jopcode op); raise Not_found in
      let m = JProgram.get_method cn mn in
      (match (get_microcode clazz cp ss m) with
       | Some x -> Array.fold_left Array.append [||] x
       | None -> raise (Opcode_Not_Implemented (JPrint.jopcode op)))
   (* MethodMap.find mn (match jvm_map with | Some x -> x | None -> raise Internal) *)
   | OpBreakpoint as op -> raise (Opcode_Not_Implemented (JPrint.jopcode op))
   | OpArrayLoad x -> (match x with
		       | `Double | `Long -> [||] (* TODO *)
		       | _ -> iaload)
   | OpArrayStore x -> (match x with
			| `Double | `Long -> [||] (* TODO *)
			| _ -> iastore)
   | OpInvalid -> [||]

and lop2micro clazz cp (program,_) op = 
  let () = JPrint.class_name (get_name clazz) |> print_endline in
  let () = JPrint.jopcode op |> print_endline in
  (match (JInstruction.code2opcodes cp [|op|]).(0) with
       | _ as s -> JDumpLow.opcode s |> print_endline)

and get_microcode clazz cp ((program,_) as ss) = function
  | AbstractMethod _ -> None
  | ConcreteMethod cm ->
     (match cm.cm_implementation with
      | Native -> None 	(* Native method is not supported and assumed to not have any memory access instructions*)
      (* Increment memory access counter if memory access instruction happens, else increment instruction counter *)
      | Java code -> 
	 let () = (Array.iter (lop2micro clazz cp ss) (Lazy.force code).c_code) in
	 Some (Array.map (opcode2microcode clazz cp ss) (Lazy.force code).c_code))

let generate_microcode (class_path : class_path)
		       (cn : class_name) program =
  (* We first recover the interface or class associated to the
    class name cn. *)
  let c = get_class class_path cn in
  let cp = (match c with | JClass c -> c.c_consts | JInterface _ -> raise Internal) in 
  let cp = DynArray.init (Array.length cp) (fun i -> cp.(i)) in
  (* Then, we get all the methods of c. *)
  let methods = get_methods c in
  (* For each method of c, we associate a field set containing
    all the accessed fields. *)
  MethodMap.map (get_microcode c cp program) methods

let main = 
  try
    let args = Sys.argv in
    let (cp, cn) = 
      if Array.length args <> 3 then let () = print_endline usage_msg in raise Internal
      else (args.(1),args.(2)) in
    (* TEST *)
    (* let lcf = IO.input_channel (open_in (cp^"/"^tt^".class")) in *)
    let llc = JFile.get_class_low (JFile.class_path cp) (make_cn cn) in
    let () = List.iter (fun x -> JDumpLow.dump_method (IO.output_channel stdout) llc.JClassLow.j_consts x) (llc.JClassLow.j_methods) in
    (* Need to build all the other entry points so that other classes are also parsed!! *)
    let program = JRTA.parse_program ~instantiated:[] ~other_entrypoints:[make_cms (make_cn "com.jopdesign.sys.Startup")
									 (make_ms "boot" [] None)] 
				     cp (make_cms (make_cn cn) JProgram.main_signature) in
    let mm = generate_microcode (class_path cp) (make_cn cn) program in
    let mm = MethodMap.filter (function Some _ -> true | None -> false) mm in
    let mm = MethodMap.map (function Some x -> x | _ -> raise Internal) mm in
    let mm = MethodMap.map (fun x -> Array.fold_left (fun t x -> Array.append t x) [||] x) mm in
    (* What happens with WAIT microcode???? *)
    let mm = MethodMap.map (fun x -> Array.fold_left (fun (i,m) x ->  
						      if (Array.exists (fun y -> x = y) mem_instr) then 
							(i,m+1)
						      else (i+1,m)) (0,0) x) mm in
    let fd = open_out (cn^".ini") in
    (* Print the (i,m) from the MethodMap *)
    MethodMap.iter (fun x (i,m) -> 
		    let mss = JPrint.method_signature x in
		    let ims = "[" ^ (string_of_int i) ^","^ (string_of_int m) ^ "]\n" in
		    let () = output_string fd (mss ^ "\n") in
		    output_string fd ims) mm
  with
  | Internal -> ()
