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

let laload = 
  [|Stm;Dup;Dup;Bz;Ldi;Add;Stmraf;Wait;Wait;Ldmrd;Ldi;Sub;Ldm;
    Sub;Ldm;Or;Ldi;And;Nop;Bnz;Nop;Nop;Stmraf;Wait;Wait;Ldmrd;Ldm;
    Ldi;Shl;Add;Dup;Stm;Stmra;Wait;Wait;Ldmrd;Ldm;Ldi;Add;Stmra;Wait;Wait;Ldmrd|]

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


(* This is the main function that generates the micro-codes *)
let rec generate_microcode_bc cp = function
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
  | JL.OpLoad (t,_) -> (match t with
			| `Double | `Long -> [|Ldvp;Dup;Ld_opd_8u;Add;Stvp;Stm;Ld0;Ld1;Ldm;Stvp;Nop|]
			| _ -> [|Nop;Ld|])
  | JL.OpALoad _ -> [|Nop;Ld|]
  | JL.OpArrayLoad t -> (match t with
		       | `Double | `Long ->laload 
		       | _ -> iaload)
  | JL.OpAALoad | JL.OpBALoad
  | JL.OpCALoad | JL.OpSALoad -> [|Stald;Pop;Wait;Wait;Ldmrd|]
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
   | JL.OpDiv x as op -> (match x with
			  | `Double -> raise (Opcode_Not_Implemented (JDumpLow.opcode op))
			  | _ -> let mn = make_ms "f_idiv" [(TBasic `Int);(TBasic `Int)] (Some (TBasic `Int)) in
				 let cn = try JFile.get_class cp (make_cn bj1) with 
					  |  No_class_found _ -> print_endline (JDumpLow.opcode op); raise Not_found 
					  |  Class_structure_error _ -> print_endline (JDumpLow.opcode op); raise Not_found in
				 let m = JClass.get_method cn mn in
				 let cn = (match cn with | JClass.JClass x -> x | _ -> raise Internal) in
				 let cpool = cn.JClass.c_consts in
				 let cpool = DynArray.init (Array.length cpool) (fun i -> cpool.(i)) in
				 let m = JHigh2Low.h2l_acmethod cpool m in
				 (* FIXME: need to do dataflow analysis of the method itself! *)
				 (* The value 32 comes from the fact that idiv has a loop bound of 32 *)
				 let tt = lazy (generate_microcode_method cp m) in
				 let tt = Array.init 32 (fun _ -> Lazy.force tt) in
				 Array.fold_left Array.append [||] tt)
  | _ -> [||]

(* Generate micro-code for a given method *)
and generate_microcode_method cp m = 
  let bcs = m.JClassLow.m_attributes in
  let bcs = List.filter (fun t -> match t with | JClassLow.AttributeCode _ -> true | _ -> false) bcs in
  let bcs = match (List.hd bcs) with | JClassLow.AttributeCode x -> x | _ -> raise Internal in
  let bcs = (Lazy.force bcs).JClassLow.c_code in
  Array.fold_left (fun t x -> Array.append t (generate_microcode_bc cp x)) [||] bcs 

(* Generating micro-code for a given class *)
let generate_microcode_clazz cp clazz = 
  let llc = JFile.get_class_low cp clazz in
  let lms = llc.JClassLow.j_methods in
  List.map (fun x -> ((JDumpBasics.method_signature x.JClassLow.m_name x.JClassLow.m_descriptor)),
		      (generate_microcode_method cp x)) lms

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
