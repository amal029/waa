open Javalib_pack
open Javalib
open JBasics
open JCode
open Joplang
open Sawja_pack
module Array = BatArray
module Enum = BatEnum
module JL = JClassLow

let (|>) x f = f x;;
exception Internal
exception Opcode_Not_Implemented of string
exception Opcode_Java_Implemented of string
exception Opcode_Not_Bounded of string
(* Classes that implement bytecodes in Java *)
let bj1 = "com.jopdesign.sys.JVM";;
let bj2 = "com.jopdesign.sys.GC";;

let newb = [|
    Ldjpc; Ldi;
    Stjpc; Nop; Nop; Ldm;
    Nop; Ld_opd_8u; Ldi; And; Dup; Add; Add; Stm; Ldm; Nop; Ld_opd_16u; Add; Stmrac; Wait; Wait; Ldmrd; Ldm; Jmp; Nop;Nop
   |]

let laload = 
  [|Stm;Dup;Dup;Bz;Ldi;Add;Stmraf;Wait;Wait;Ldmrd;Ldi;Sub;Ldm;
    Sub;Ldm;Or;Ldi;And;Nop;Bnz;Nop;Nop;Stmraf;Wait;Wait;Ldmrd;Ldm;
    Ldi;Shl;Add;Dup;Stm;Stmra;Wait;Wait;Ldmrd;Ldm;Ldi;Add;Stmra;Wait;Wait;Ldmrd|]

let lastore = 
  [|Stm;Stm;Stm;Dup;Dup;Bz;Ldi;Add;Stmraf;Wait;Wait;Ldmrd;Ldi;
    Sub;Ldm;Sub;Ldm;Or;Ldi;And;Nop;Bnz;Nop;Nop;Stmraf;Wait;
    Wait;Ldmrd;Ldm;Ldi;Shl;Add;Stm;Ldm;Stmwa;Ldm;Stmwd;Ldm;Ldi;Add;Wait;Wait;Stmwa;Ldm;Stmwd;Wait;Wait;Nop;
   |]

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

let usage_msg = "Usage: wcma class-path class-name
[Note
 1.) Class-name should be given without the .class extension
 2.) Should be a fully qualified name, .e.g,: java.lang.Object";;


set_permissive true;;

(* Method size in bytes *)
let get_method_size = function
  | JL.OpNop -> 1
  | JL.OpAConstNull -> 1
  | JL.OpLConst _ -> 1
  | JL.OpIConst _ -> 1
  | JL.OpFConst _ -> 1
  | JL.OpDConst _ -> 1
  | JL.OpBIPush _ -> 2
  | JL.OpSIPush _ -> 3
  | JL.OpLdc1 _ -> 2
  | JL.OpLdc1w _ -> 3
  | JL.OpLdc2w _ -> 3 
  | JL.OpLoad (t,v) -> 
     (match t with
      | `Double 
      | `Long -> if (v <= 3) then 1 else 2
      | _ -> if (v <= 3) then 1 else 2)
  | JL.OpALoad v -> if v<=3 then 1 else 2
  | JL.OpArrayLoad _ -> 1
  | JL.OpAALoad | JL.OpBALoad
  | JL.OpCALoad | JL.OpSALoad -> 1
  | JL.OpAStore _ -> 2
  | JL.OpStore (_,v) -> if v<=3 then 1 else 2
  | JL.OpArrayStore _ -> 1
  | JL.OpAAStore | JL.OpBAStore
  | JL.OpCAStore | JL.OpSAStore -> 1
  | JL.OpPop -> 1
  | JL.OpPop2 -> 1
  | JL.OpDup -> 1
  | JL.OpDupX1 -> 1
  | JL.OpDupX2 -> 1
  | JL.OpDup2 -> 1
  | JL.OpDup2X1 -> 1
  | JL.OpDup2X2 -> 1
  | JL.OpSwap -> 1
  | JL.OpAdd _ -> 1 
  | JL.OpSub _ -> 1 
  | JL.OpMult _ -> 1 
  | JL.OpDiv _ ->  1
  | JL.OpRem _ -> 1
  | JL.OpNeg _ -> 1
  | JL.OpIShl -> 1
  | JL.OpIShr -> 1
  | JL.OpIUShr -> 1
  | JL.OpLShl -> 1
  | JL.OpLShr -> 1
  | JL.OpLUShr -> 1
  | JL.OpIAnd -> 1
  | JL.OpIOr -> 1
  | JL.OpIXor -> 1
  | JL.OpLAnd -> 1
  | JL.OpLOr -> 1
  | JL.OpLXor -> 1
  | JL.OpI2L -> 1
  | JL.OpI2C -> 1
  | JL.OpL2I -> 1
  | JL.OpL2F -> 1
  | JL.OpL2D -> 1
  | JL.OpF2I -> 1
  | JL.OpF2L -> 1
  | JL.OpF2D -> 1
  | JL.OpD2I -> 1
  | JL.OpD2F -> 1
  | JL.OpD2L -> 1
  | JL.OpI2B -> 1
  | JL.OpI2F -> 1 
  | JL.OpI2S -> 1
  | JL.OpI2D -> 1
  | JL.OpIInc _ -> 3
  | JL.OpLCmp -> 1
  | JL.OpFCmpL -> 1
  | JL.OpFCmpG -> 1
  | JL.OpDCmpL -> 1
  | JL.OpIfEq _ | JL.OpIfNe _ 
  | JL.OpIfLt _ | JL.OpIfGe _
  | JL.OpIfLe _ | JL.OpIfGt _ -> 3
  | JL.OpICmpEq _
  | JL.OpICmpNe _
  | JL.OpICmpLt _
  | JL.OpICmpGe _
  | JL.OpICmpGt _
  | JL.OpICmpLe _
  | JL.OpACmpEq _
  | JL.OpACmpNe _ -> 3
  | JL.OpGoto _ -> 3
  | JL.OpJsr _ -> 3
  | JL.OpRet _ -> 2
  | JL.OpTableSwitch _ as op -> raise (Opcode_Not_Implemented (JDumpLow.opcode op))
  | JL.OpLookupSwitch _ -> 0 (* FIXME *)
  | JL.OpReturn x -> 1
  | JL.OpAReturn -> 1 
  | JL.OpReturnVoid -> 1 
  | JL.OpGetStatic _ -> 3
  | JL.OpPutStatic _ -> 3
  | JL.OpGetField _ -> 3
  | JL.OpPutField _ -> 3
  | JL.OpInvokeVirtual _ -> 3 
  | JL.OpInvokeNonVirtual _ -> 3
  | JL.OpInvokeStatic _ -> 3
  | JL.OpInvokeInterface _ -> 5
  | JL.OpNew _ -> 3
  | JL.OpNewArray _ -> 2
  | JL.OpANewArray _ -> 3
  | JL.OpArrayLength -> 1
  | JL.OpThrow -> 1
  | JL.OpCheckCast _ -> 3
  | JL.OpInstanceOf _ -> 3
  | JL.OpMonitorEnter  -> 1
  | JL.OpMonitorExit -> 1
  | JL.OpAMultiNewArray _ -> 4
  | JL.OpIfNull _ -> 3
  | JL.OpIfNonNull _ -> 3 
  | JL.OpGotoW _ -> 5
  | JL.OpJsrW _ -> 5
  | JL.OpBreakpoint -> 1
  | JL.OpInvalid -> 0
  | JL.OpDCmpG -> 1

(* This is the main function that generates the micro-codes *)
let rec generate_microcode_bc const_pool cp = function
  | JL.OpNop -> [|Nop|]
  | JL.OpAConstNull -> [|Ldi|]
  | JL.OpLConst _ -> [|Ldi;Ldi|]
  | JL.OpIConst _ -> [|Ldi|]
  | JL.OpFConst _ -> [|Ldi|] 	(* Only fconst_0 is supported *)
  | JL.OpDConst _ -> [|Ldi;Ldi|] (* Only dconst_0 is supported *)
  | JL.OpBIPush _ -> [|Nop;Ld_opd_8u|]
  | JL.OpSIPush _ -> [|Nop;Nop;Ld_opd_16s|]
  | JL.OpLdc1 _ -> [|Ldm;Ld_opd_8u;Add;Stmrac|]
  | JL.OpLdc1w _ -> [|Ldm;Nop;Ld_opd_16u;Add;Stmrac;Wait;Wait;Ldmrd|]
  | JL.OpLdc2w _ -> [|Ldm;Nop;Ld_opd_16u;Add;Dup;Stmrac;Ldi;Add;Wait;
		      Wait;Ldmrd;Stm;Stmrac;Ldm;Wait;Wait;Ldmrd|]
  (* We can make this more accurate by checking for the second arg, but it is OK for now! *)
  | JL.OpLoad (t,v) -> (match t with
			| `Double | `Long ->  
				     if (v <= 2) then [|Ld;Ld|]
				     else if (v = 3) then 
				       [|Ldvp;Dup;Ldi;Add;Stvp;Stm;Ld2;Ld3;Ldm;Stvp;Nop|]
				     else
				       [|Ldvp;Dup;Ld_opd_8u;Add;Stvp;Stm;Ld0;Ld1;Ldm;Stvp;Nop|]
			| _ -> 
			   if (v <= 3) then
			     [|Ld|]
			   else 
			     [|Nop;Ld|])
  | JL.OpALoad v -> if v <=3 then [|Ld|] else [|Nop;Ld|]
  | JL.OpArrayLoad t -> (match t with
		       | `Double | `Long ->laload 
		       | _ -> iaload)
  | JL.OpAALoad | JL.OpBALoad
  | JL.OpCALoad | JL.OpSALoad -> [|Stald;Pop;Wait;Wait;Ldmrd|]
  | JL.OpAStore _ -> [|Nop;St|]
  | JL.OpStore (x,v) ->
     (match x with
      | `Long | `Double -> if v<=2 then [|St;St|]
			   else if v = 3 then 
			     [|Ldvp;Dup;Ldi;Add;Stvp;Stm;St3;St2;Ldm;Stvp;Nop|]
			   else [|Ldvp;Dup;Ld_opd_8u;Add;Stvp;Stm;St1;St0;Ldm;Stvp;Nop|] 
      | _ -> if v <=3 then [|St|] else [|Nop;St|])
  | JL.OpArrayStore x -> 
     (match x with
      | `Double | `Long -> lastore
      | _ -> iastore)
  | JL.OpAAStore | JL.OpBAStore
  | JL.OpCAStore | JL.OpSAStore -> [|Stast;Pop;Pop;Wait;Wait;Nop|]
  | JL.OpPop -> [|Pop|]
  | JL.OpPop2 -> [|Pop;Pop|]
  | JL.OpDup -> [|Dup|]
  | JL.OpDupX1 -> [|Stm;Stm;Ldm;Ldm;Ldm;|]
  | JL.OpDupX2 -> [|Stm;Stm;Stm;Ldm;Ldm;Ldm;Ldm|]
  | JL.OpDup2 -> [|Stm;Stm;Ldm;Ldm;Ldm;Ldm|]
  | JL.OpDup2X1 -> [|Stm;Stm;Stm;Ldm;Ldm;Ldm;Ldm;Ldm|]
  | JL.OpDup2X2 -> [|Stm;Stm;Stm;Stm;Ldm;Ldm;Ldm;Ldm;Ldm;Ldm|]
  | JL.OpSwap -> [|Stm;Stm;Ldm;Ldm|]
  | JL.OpAdd x as op -> (match x with
		      | `Double -> raise (Opcode_Not_Implemented (JDumpLow.opcode op))
		      | `Float -> raise (Opcode_Java_Implemented (JDumpLow.opcode op))
		      | `Long -> long_add
		      | _ -> [|Add|])
  | JL.OpSub x as op -> (match x with 
		      | `Double -> raise (Opcode_Not_Implemented (JDumpLow.opcode op))
		      | `Float -> raise (Opcode_Java_Implemented (JDumpLow.opcode op))
		      | `Long -> 
			 [|Ldi;Xor;Stm;Ldi;Xor;Stm;Stm;Stm;Ldm;Ldi;Ushr;Ldm;
			   Ldi;Ushr;Add;Ldm;Ldi;And;Ldm;Ldi;And;Add;Ldi;Add;Ldi;Shr;Add;Ldi;Ushr;Ldm;Add;Ldm;Add;Ldm;Ldm;Add;Ldi;Add|]
		      | _ -> [|Sub|])
   | JL.OpMult x as op-> (match x with
		  | `Double -> raise (Opcode_Not_Implemented (JDumpLow.opcode op))
		  | `Long | `Float -> raise (Opcode_Java_Implemented (JDumpLow.opcode op))
		  | _ -> Array.init 35 (fun _ -> Nop)) (* Note that imul never access external memory!! *)
   | JL.OpDiv x as op ->  
      (match x with
       | `Double -> raise (Opcode_Not_Implemented (JDumpLow.opcode op))
       | _ -> let mn = make_ms "f_idiv" [(TBasic `Int);(TBasic `Int)] (Some (TBasic `Int)) in
	      let cn = try JFile.get_class cp (make_cn bj1) with 
		       |  No_class_found _ -> print_endline (JDumpLow.opcode op); raise Not_found 
		       |  Class_structure_error _ -> print_endline (JDumpLow.opcode op); raise Not_found in
	      let m = JClass.get_method cn mn in
	      let cn = (match cn with | JClass.JClass x -> x | _ -> raise Internal) in
	      let cpool = cn.JClass.c_consts in
	      let cpool1 = DynArray.init (Array.length cpool) (fun i -> cpool.(i)) in
	      let m = JHigh2Low.h2l_acmethod cpool1 m in
	      (* FIXME: need to do dataflow analysis of the method itself! *)
	      (* The value 32 comes from the fact that idiv has a loop bound of 32 *)
	      let tt = lazy (generate_microcode_method cpool cp m) in
	      let tt = Array.init 32 (fun _ -> Lazy.force tt) in
	      Array.fold_left Array.append [||] tt)
   | JL.OpRem x as op ->
      (match x with
       | `Double -> raise (Opcode_Not_Implemented (JDumpLow.opcode op))
       | _ -> let mn = make_ms "f_irem" [(TBasic `Int);(TBasic `Int)] (Some (TBasic `Int)) in
	      let cn = try JFile.get_class cp (make_cn bj1) with 
		       |  No_class_found _ -> print_endline (JDumpLow.opcode op); raise Not_found 
		       |  Class_structure_error _ -> print_endline (JDumpLow.opcode op); raise Not_found in
	      let m = JClass.get_method cn mn in
	      let cn = (match cn with | JClass.JClass x -> x | _ -> raise Internal) in
	      let cpool = cn.JClass.c_consts in
	      let cpool1 = DynArray.init (Array.length cpool) (fun i -> cpool.(i)) in
	      let m = JHigh2Low.h2l_acmethod cpool1 m in
	      (* FIXME: need to do dataflow analysis of the method itself! *)
	      (* The value 32 comes from the fact that idiv has a loop bound of 32 *)
	      let tt = lazy (generate_microcode_method cpool cp m) in
	      let tt = Array.init 32 (fun _ -> Lazy.force tt) in
	      Array.fold_left Array.append [||] tt)
   | JL.OpNeg x as op ->
      (match x with 
       | `Long -> Array.append [|Ldi;Xor;Stm;Ldi;Xor;Ldm;Ldi;Ldi|] long_add
       | `Double -> raise (Opcode_Not_Implemented (JDumpLow.opcode op))
       | `Float -> raise (Opcode_Java_Implemented (JDumpLow.opcode op))
       | _ -> [|Ldi;Xor;Ldi;Add|]) 
   | JL.OpIShl -> [|Shl|]
   | JL.OpIShr -> [|Shr|]
   | JL.OpIUShr -> [|Ushr|]
   | JL.OpLShl -> Array.append (Array.append lshl lshl_not0) lshl_le31
   | JL.OpLShr -> (Array.append lshr lshr_le31)
   | JL.OpLUShr -> Array.append (Array.append lushr lushr_le31) lushr_not0
   | JL.OpIAnd -> [|And|]
   | JL.OpIOr -> [|Or|]
   | JL.OpIXor -> [|Xor|]
   | JL.OpLAnd -> [|Stm;Stm;Stm;Ldm;And;Ldm;Ldm;And|]
   | JL.OpLOr -> [|Stm;Stm;Stm;Ldm;Or;Ldm;Ldm;Or|]
   | JL.OpLXor -> [|Stm;Stm;Stm;Ldm;Xor;Ldm;Ldm;Xor|]
   | JL.OpI2L -> [|Dup;Stm;Ldi;Shr;Ldm|]
   | JL.OpI2C -> [|Ldi;And|]
   | JL.OpL2I -> [|Stm;Pop;Ldm|]
   | JL.OpL2F as op -> raise (Opcode_Not_Implemented (JDumpLow.opcode op))
   | JL.OpL2D as op -> raise (Opcode_Not_Implemented (JDumpLow.opcode op))
   | JL.OpF2I as op -> raise (Opcode_Java_Implemented (JDumpLow.opcode op))
   | JL.OpF2L as op -> raise (Opcode_Not_Implemented (JDumpLow.opcode op))
   | JL.OpF2D as op -> raise (Opcode_Not_Implemented (JDumpLow.opcode op))
   | JL.OpD2I as op -> raise (Opcode_Not_Implemented (JDumpLow.opcode op))
   | JL.OpD2F as op -> raise (Opcode_Not_Implemented (JDumpLow.opcode op))
   | JL.OpD2L as op -> raise (Opcode_Java_Implemented (JDumpLow.opcode op))
   | JL.OpI2B as op -> raise (Opcode_Java_Implemented (JDumpLow.opcode op))
   | JL.OpI2F as op -> raise (Opcode_Java_Implemented (JDumpLow.opcode op))
   | JL.OpI2S as op -> raise (Opcode_Java_Implemented (JDumpLow.opcode op))
   | JL.OpI2D as op -> raise (Opcode_Not_Implemented (JDumpLow.opcode op))
   | JL.OpIInc _ -> [|Ldvp;Ld_opd_8u;Add;Star;Ld_opd_8u;Ldmi;Stmi|]
   | JL.OpLCmp as op -> 
      let mn = make_ms "f_lcmp" [(TBasic `Int);(TBasic `Int);(TBasic `Int);(TBasic `Int)] (Some (TBasic `Int)) in
      let cn = try JFile.get_class cp (make_cn bj1) with
	       |  No_class_found _ -> print_endline (JDumpLow.opcode op); raise Not_found 
	       |  Class_structure_error _ -> print_endline (JDumpLow.opcode op); raise Not_found in
      let m = JClass.get_method cn mn in
      let cn = (match cn with | JClass.JClass x -> x | _ -> raise Internal) in
      let cpool = cn.JClass.c_consts in
      let cpool1 = DynArray.init (Array.length cpool) (fun i -> cpool.(i)) in
      let m = JHigh2Low.h2l_acmethod cpool1 m in
      generate_microcode_method cpool cp m
   | JL.OpFCmpL as op -> raise (Opcode_Java_Implemented (JDumpLow.opcode op))
   | JL.OpFCmpG as op -> raise (Opcode_Java_Implemented (JDumpLow.opcode op))
   | JL.OpDCmpL as op -> raise (Opcode_Not_Implemented (JDumpLow.opcode op))
   | JL.OpDCmpG as op -> raise (Opcode_Not_Implemented (JDumpLow.opcode op))
   | JL.OpIfEq _ | JL.OpIfNe _ 
   | JL.OpIfLt _ | JL.OpIfGe _
   | JL.OpIfLe _ | JL.OpIfGt _ -> [|Nop;Jbr;Pop;Nop|]
   | JL.OpICmpEq _
   | JL.OpICmpNe _
   | JL.OpICmpLt _
   | JL.OpICmpGe _
   | JL.OpICmpGt _
   | JL.OpICmpLe _
   | JL.OpACmpEq _
   | JL.OpACmpNe _ -> [|Nop;Jbr;Pop;Pop|]
   | JL.OpGoto _ -> [|Nop;Jbr;Nop;Nop|]
   | JL.OpJsr _ | JL.OpRet _ -> [||]
   | JL.OpTableSwitch _ as op -> raise (Opcode_Not_Implemented (JDumpLow.opcode op))
   | JL.OpLookupSwitch (_,x) as op -> 
      let mn = make_ms "f_lookupswitch" [(TBasic `Int)] None in
      let cn = try JFile.get_class cp (make_cn bj1) with
	       |  No_class_found _ -> print_endline (JDumpLow.opcode op); raise Not_found 
	       |  Class_structure_error _ -> print_endline (JDumpLow.opcode op); raise Not_found in
      let m = JClass.get_method cn mn in
      let cn = (match cn with | JClass.JClass x -> x | _ -> raise Internal) in
      let cpool = cn.JClass.c_consts in
      let cpool1 = DynArray.init (Array.length cpool) (fun i -> cpool.(i)) in
      let m = JHigh2Low.h2l_acmethod cpool1 m in
      let tt = lazy(generate_microcode_method cpool cp m) in
      let r = Array.init (List.length x) (fun _ -> Lazy.force tt) in
      Array.fold_left Array.append [||] r
   | JL.OpReturn x ->
      (match x with
       | `Double | `Long -> [|Stm;Stm;Dup;Stmrac;Stm;Stm;Stvp;Wait;Wait;
			      Ldmrd; Stbcrd;Stm;Nop;Stsp;Pop;Pop;Ldbcstart;Ldm;Add;
			      Stjpc;Ldm;Ldm;Wait;Wait;Nop|]
       | _ -> [|Stm;Dup;Stmrac;Stm;Stm;Stvp;Wait;Wait;Ldmrd;Stbcrd;Stm;Nop;Stsp;
		Pop;Pop;Ldbcstart;Ldm;Add;Stjpc;Ldm;Wait;Wait;Nop|])
   | JL.OpAReturn -> [|Stm;Dup;Stmrac;Stm;Stm;Stvp;Wait;Wait;Ldmrd;Stbcrd;Stm;Nop;Stsp;
		       Pop;Pop;Ldbcstart;Ldm;Add;Stjpc;Ldm;Wait;Wait;Nop|] 
   | JL.OpReturnVoid -> [|Dup;Stmrac;Stm;Stm;Stvp;Wait;Wait;Ldmrd;Stbcrd;Stm;Nop;
			Stsp;Ldbcstart;Ldm;Add;Stjpc;Pop;Pop;Wait;Wait;Nop|]

   | JL.OpGetStatic _ 
   | JL.OpGetField _ 
   | JL.OpPutField _ 
   | JL.OpInvokeStatic _
   | JL.OpInvokeVirtual _
   | JL.OpInvokeNonVirtual _
   | JL.OpInvokeInterface _
   | JL.OpPutStatic _ as op -> 
      (* Better to convert it into high-level instruction and solve it there!! *)
      let hopcode = JInstruction.opcode2instruction const_pool op in
      (match hopcode with
       | OpGetStatic (_,x) -> 
	  (match (fs_type x) with
	   | TBasic x -> (match x with
      			  | `Long -> [|Nop;Nop;Ld_opd_16u;Dup;
      				       Stmra;Ldi;Add;Stm;Wait;Wait;
      				       Ldmrd;Ldm;Stmra;Wait;Wait;Ldmrd|]
      			  | _ -> [|Stgs;Nop;Wait;Wait;Ldmrd|])
	   | _ -> [|Stgs;Nop;Wait;Wait;Ldmrd|])
       | OpGetField (_,x) ->
	  (match (fs_type x) with
	   | TBasic x -> (match x with
			  | `Long ->
			     [|Dup;Nop;Bz;Nop;Nop;Stmraf;Wait;Wait;Ldmrd;Nop;
			       Nop;Ld_opd_16u;Add;Dup;Stmraf;Ldi;Add;Stm;Wait;Wait;Ldmrd;Ldm;Stmraf;Wait;Wait;Ldmrd;
			      |]
			  | _ -> [|Stgf;Nop;Wait;Wait;Ldmrd;|])
	   | _ -> [|Stgf;Nop;Wait;Wait;Ldmrd;|])
       | OpPutField (_,x) ->
	  (match (fs_type x) with
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
	   | _ -> [|Ldjpc;
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
       | OpInvoke (x,y) as op ->
	  (match x with
	   | `Interface _ -> invokeinterface_mc
	   | `Virtual _ -> invokevirtual_mc
	   | `Special _ -> invokestatic_mc
	   | `Static cn -> 
	      let cn = cn_simple_name cn in
	      let mn = ms_name y in
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
       | _ -> raise Internal)
   | JL.OpMonitorExit -> monitorexit
   | JL.OpMonitorEnter -> monitorenter
   | JL.OpBreakpoint -> [||]
   | JL.OpInvalid -> [||]
   | JL.OpGotoW _ as op -> raise (Opcode_Not_Implemented (JDumpLow.opcode op))
   | JL.OpJsrW _ as op -> raise (Opcode_Not_Implemented (JDumpLow.opcode op))
   | JL.OpIfNull _ 
   | JL.OpIfNonNull _ -> [|Nop;Jbr;Pop;Nop|]
   | JL.OpArrayLength -> arraylength
   | JL.OpThrow as op -> 
      let mn = make_ms "f_athrow" [TObject (TClass (make_cn "java.lang.Throwable"))]
		       (Some (TObject (TClass (make_cn "java.lang.Throwable")))) in
      let cn = try JFile.get_class cp (make_cn bj1) with
	       |  No_class_found _ -> print_endline (JDumpLow.opcode op); raise Not_found 
	       |  Class_structure_error _ -> print_endline (JDumpLow.opcode op); raise Not_found in
      let m = JClass.get_method cn mn in
      let cn = (match cn with | JClass.JClass x -> x | _ -> raise Internal) in
      let cpool = cn.JClass.c_consts in
      let cpool1 = DynArray.init (Array.length cpool) (fun i -> cpool.(i)) in
      let m = JHigh2Low.h2l_acmethod cpool1 m in
      generate_microcode_method cpool cp m
   (* The new bytecodes!! *)
   | JL.OpNew _ as op -> 
      let mn = make_ms "f_new" [(TBasic `Int)] (Some (TBasic `Int)) in
      let cn = try JFile.get_class cp (make_cn bj1) with
	       |  No_class_found _ -> print_endline (JDumpLow.opcode op); raise Not_found 
	       |  Class_structure_error _ -> print_endline (JDumpLow.opcode op); raise Not_found in
      let m = JClass.get_method cn mn in
      let cn = (match cn with | JClass.JClass x -> x | _ -> raise Internal) in
      let cpool = cn.JClass.c_consts in
      let cpool1 = DynArray.init (Array.length cpool) (fun i -> cpool.(i)) in
      let m = JHigh2Low.h2l_acmethod cpool1 m in
      generate_microcode_method cpool cp m

(* Generate micro-code for a given method *)
and generate_microcode_method cpool cp m = 
  let bcs = m.JClassLow.m_attributes in
  let bcs = List.filter (fun t -> match t with | JClassLow.AttributeCode _ -> true | _ -> false) bcs in
  let bcs = match (List.hd bcs) with | JClassLow.AttributeCode x -> x | _ -> raise Internal in
  let bcs = (Lazy.force bcs).JClassLow.c_code in
  (* Count the number of bytecodes to find the size of the method in bytes *)
  let msize = Array.fold_left (fun v x -> v + (get_method_size x)) 0 bcs in
  let waits = Array.init (msize / 4) (fun _ -> Wait) in
  Array.fold_left (fun t x -> Array.append t (generate_microcode_bc cpool cp x)) waits bcs 

(* Generating micro-code for a given class *)
let generate_microcode_clazz cp clazz = 
  let llc = JFile.get_class_low cp clazz in
  let cpool = llc.JClassLow.j_consts in
  let lms = llc.JClassLow.j_methods in
  List.map (fun x -> ((JDumpBasics.method_signature x.JClassLow.m_name x.JClassLow.m_descriptor)),
		      (generate_microcode_method cpool cp x)) lms

let main = 
  try
    let args = Sys.argv in
    let (cp, cn) = 
      if Array.length args <> 3 then let () = print_endline usage_msg in raise Internal
      else (args.(1),args.(2)) in
    let mm = generate_microcode_clazz (JFile.class_path cp) (make_cn cn) in 
    let mm = List.map (fun (mn,vals) -> 
		       (mn, Array.fold_left (fun (i,m) x -> if (Array.exists (fun y -> x = y) mem_instr) 
							    then 
							      (i,m+1) 
							    else 
							      (i+1,m)) (0,0) vals)) mm in
    let fd = open_out (cn^".ini") in
    List.iter (fun (x,(i,m)) -> 
	      let () = output_string fd (x ^ "\n") in
	      output_string fd ("[" ^ (string_of_int i) ^","^ (string_of_int m) ^ "]\n")) mm
  with
  | Internal -> ()
