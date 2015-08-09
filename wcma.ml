open Javalib_pack
open Javalib
open JBasics
open JCode
open Joplang
open Sawja_pack
module Stack = BatStack
module Array = BatArray
module List = BatList
module JL = JClassLow
module Enum = BatEnum
module File = BatFile
module Hashtbl = BatHashtbl
open BatPervasives

let (>>) f g x = (f(g x))


exception Internal
exception Opcode_Not_Implemented of string
exception Opcode_Java_Implemented of string
exception Opcode_Not_Bounded of string
(* Classes that implement bytecodes in Java *)
let bj1 = "com.jopdesign.sys.JVM";;
let bj2 = "com.jopdesign.sys.GC";;
let bj3 = ref "";;
let addmethods = ref false;;
let cpp = ref ""

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
    Ldbcstart; Sub; Stm; Ldm;Stmrac; Ldm;
    Stm;Wait;Wait;Ldmrd;Stbcrd; Dup; Ldi; And; Stm; Ldi;Ushr; Dup; Ldi; And; Stm; Ldi; Ushr; Stm; Ldsp; Ldi;
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
    Ld_opd_16u;Add;Stmrac;Wait;
    Wait;Ldmrd;Dup;Ldi;And;Stm;Ldi;Ushr;Stm;Ldsp;Ldi;Add;Ldm;Sub;Star;Nop;Ldmi;Dup;Nop;Bnz;Ldi;Add;Stmraf;Wait;Wait;Ldmrd
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

let usage_msg = "Usage: wcma [-sourcepath <filename>] [OPTION] class-path class-name
		 Note:
		 1.) Class-name should be given without the .class extension
		 2.) Should be a fully qualified name, .e.g,: java.lang.Object";;


  set_permissive true;;


let exists_in_marray marray ms = 
  List.exists (fun (i,_) -> (i = ms)) (DynArray.to_list  marray)

let exists_in_clzms_array clzms x = 
  let a = Stack.enum x in
  Enum.exists (fun x -> x = clzms) a

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
    | JL.OpTableSwitch (_,_,_,x) -> 4+4+4+(4* Array.length x)
    | JL.OpLookupSwitch (_,x) -> 4+4+(8* List.length x)
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

  let getloopcount lr opline opcode = 
    (* Getting number of executions for this byte code *)
    let num = List.fold_left (fun x (sl,el,lc) -> if (sl <= opline) && (el >= opline) then x*lc else x ) 1 lr in
    match num with | 0 -> 1 | _ -> num

  let rec filteri f a =
    Array.of_list (filteri2 f 0 a [])
  and filteri2 f i a l =
    match f i (Array.get a i) with
    | true -> (i, Array.get a i) :: if (Array.length a) <> (i+1) then filteri2 f (i+1) a l else l
    | false -> if (Array.length a) <> (i+1) then filteri2 f (i+1) a l else l

  let getloopranges wca_list lnt code cname = 
    (* lnt (x,y) --> x : bytecode line number, y : line number *)
    (* wca_list (x,y) --> x : line number, y : loop count *)
    let opcodes = code.JClassLow.c_code in
    let wca_list = BatHashtbl.find_option wca_list (JBasics.cn_name cname) in
    let wca_list = match wca_list with | Some x -> DynArray.to_list x | None -> [] in
    let rec findr = fun i x -> match x.(i) with | JClassLow.OpInvalid -> findr (i-1) x | _ -> i in
    let nxt = List.map (fun (ln,lc) -> 
			let ((_,l1),(_,l2)) = (List.hd lnt, List.nth lnt ((List.length lnt)-1)) in
			if ((l1 <= (int_of_string ln)) && ((int_of_string ln) < l2)) then
			  begin
			    (* This is one line after @WCA *)
			    (* TODO: Also need to include inf loop which may not generate line number from the @WCA *)
			    let (s_bcln,s_ln2) = List.find (fun (bcln, ln2) -> (int_of_string ln) <= ln2 ) lnt in
			    let (n_bcln,e_ln2) = List.find (fun (bcln, ln2) -> s_ln2 < ln2 ) lnt in (* it was (int_of_string ln) + 1 <= ln2 *)
			    print_endline ("Branch to search : start_bytecode "^(string_of_int s_bcln)^" end_bytecode "^(string_of_int n_bcln)^" "^ln);
			    (* let tlines = Array.filteri (fun i x -> i >= s_bcln && i < n_bcln) opcodes in *)
			    let tlines = Array.fold_lefti (fun z i x -> 
							   if i >= s_bcln && i < n_bcln then
							     Array.append z [|(i,x)|]
							   else
							     z
							  ) [||] opcodes 
			    in
			    Array.iter (fun (i,x) -> print_endline ("Searching bytecode : "^(string_of_int i)^" "^(JDumpLow.opcode x)) ) tlines;
			    (* This will generate exp for inf loop *)
			    try
			      let jumpop = Array.find (fun x -> 
						       match x with
						       | (_,(JClassLow.OpICmpGe _ | JClassLow.OpACmpEq _ | JClassLow.OpACmpNe _ | JClassLow.OpICmpEq _ 
							     | JClassLow.OpICmpGt _ | JClassLow.OpICmpLe _ | JClassLow.OpICmpLt _ | JClassLow.OpICmpNe _ 
							     | JClassLow.OpIfEq _ | JClassLow.OpIfGe _ | JClassLow.OpIfGt _ | JClassLow.OpIfLe _ 
							     | JClassLow.OpIfLt _ | JClassLow.OpIfNe _ | JClassLow.OpIfNonNull _ | JClassLow.OpIfNull _ ) ) -> true
						       | _ -> false
						      ) tlines in
			      let (s_bcln,e_bcln) = match jumpop with 
				| (i, (JClassLow.OpICmpGe x | JClassLow.OpACmpEq x | JClassLow.OpACmpNe x | JClassLow.OpICmpEq x 
				       | JClassLow.OpICmpGt x | JClassLow.OpICmpLe x | JClassLow.OpICmpLt x | JClassLow.OpICmpNe x 
				       | JClassLow.OpIfEq x | JClassLow.OpIfGe x | JClassLow.OpIfGt x | JClassLow.OpIfLe x 
				       | JClassLow.OpIfLt x | JClassLow.OpIfNe x | JClassLow.OpIfNonNull x | JClassLow.OpIfNull x )) -> 
				   let sss = i in
				   let i = findr (i+x-1) opcodes in
				   let goto = (Array.get opcodes i) in
				   (match goto with
				    | JClassLow.OpGotoW n | JClassLow.OpGoto n -> ((i+n) ,i)
				    | _ -> print_endline ("jump line "^(string_of_int sss)^" |  "^(string_of_int i)^" "^(JDumpLow.opcode goto)); raise Not_found)
				| _ -> failwith "Could not find end of loop (general)"
			      in
			      Some (s_bcln,e_bcln,int_of_string lc)
			    with 
			    | Not_found -> 
			       let goto = filteri (fun i x -> 
						   if (i >= n_bcln) then
						     match x with
						     | JClassLow.OpGotoW _ | JClassLow.OpGoto _ | JClassLow.OpIfNe _ -> true
						     | _ -> false
						   else
						     false
						  ) opcodes in
			       let goto = Array.fold_left (fun x y -> 
							   (function (i,JClassLow.OpGotoW n) | (i,JClassLow.OpGoto n)
								     | (i,JClassLow.OpIfNe n) -> if (i+n) = s_bcln then Some i else x | _ -> failwith "This must be goto bytecode") y
							  ) None goto 
			       in
			       match goto with
			       | Some x -> Some (s_bcln,x,int_of_string lc)
			       | None -> print_endline ((string_of_int s_bcln)^" loopc "^lc); failwith "Could not find the end of the loop (inf)"
			  end
			else
			  None
		       ) wca_list
    in
    List.filter_map (fun x -> x) nxt

  let tttt = ref true


  (* This is the main function that generates the micro-codes *)
  (* TODO: Dataflow analysis is absolutely necessary!! *)
  let rec generate_microcode_bc mstack marray pms cms pcname cname bcs const_pool cp l bd level ss2 = function
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
			   | `Long -> 
			      let mn = make_ms "f_lmul" [(TBasic `Long);(TBasic `Long)] (Some (TBasic `Long)) in
			      let () = byte_code_method_invoke op mn mstack marray pms cms pcname cname bcs const_pool cp l bd level ss2 in
			      invokestatic_mc
			   | `Float -> raise (Opcode_Java_Implemented (JDumpLow.opcode op))
			   | _ -> Array.init 35 (fun _ -> Nop)) (* Note that imul never access external memory!! *)
    | JL.OpDiv x as op ->  
       (match x with
	| `Double -> raise (Opcode_Not_Implemented (JDumpLow.opcode op))
	| _ -> 
	   let mn = make_ms "f_idiv" [(TBasic `Int);(TBasic `Int)] (Some (TBasic `Int)) in
	   let () = byte_code_method_invoke op mn mstack marray pms cms pcname cname bcs const_pool cp l bd level ss2 in
	   invokestatic_mc)
    | JL.OpRem x as op ->
       (match x with
	| `Double -> raise (Opcode_Not_Implemented (JDumpLow.opcode op))
	| _ -> 
	   let mn = make_ms "f_irem" [(TBasic `Int);(TBasic `Int)] (Some (TBasic `Int)) in
	   let () = byte_code_method_invoke op mn mstack marray pms cms pcname cname bcs const_pool cp l bd level ss2 in
	   invokestatic_mc)
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
    | JL.OpD2F as op -> 
       let mn = make_ms "f_d2f" [(TBasic `Long)] (Some (TBasic `Int)) in
       let () = byte_code_method_invoke op mn mstack marray pms cms pcname cname bcs const_pool cp l bd level ss2 in
       invokestatic_mc
    | JL.OpD2L as op -> raise (Opcode_Java_Implemented (JDumpLow.opcode op))
    | JL.OpI2B as op -> 
       let mn = make_ms "f_i2b" [(TBasic `Int)] (Some (TBasic `Int)) in
       let () = byte_code_method_invoke op mn mstack marray pms cms pcname cname bcs const_pool cp l bd level ss2 in
       invokestatic_mc
    (* raise (Opcode_Java_Implemented (JDumpLow.opcode op)) *)
    | JL.OpI2F as op -> raise (Opcode_Java_Implemented (JDumpLow.opcode op))
    | JL.OpI2S as op -> raise (Opcode_Java_Implemented (JDumpLow.opcode op))
    | JL.OpI2D as op -> raise (Opcode_Not_Implemented (JDumpLow.opcode op))
    | JL.OpIInc _ -> [|Ldvp;Ld_opd_8u;Add;Star;Ld_opd_8u;Ldmi;Stmi|]
    | JL.OpLCmp as op -> 
       let mn = make_ms "f_lcmp" [(TBasic `Int);(TBasic `Int);(TBasic `Int);(TBasic `Int)] (Some (TBasic `Int)) in
       let () = byte_code_method_invoke op mn mstack marray pms cms pcname cname bcs const_pool cp l bd level ss2 in
       invokestatic_mc
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
    | JL.OpTableSwitch _ as op -> 
       let mn = make_ms "f_tableswitch" [(TBasic `Int)] None in
       let () = byte_code_method_invoke op mn mstack marray pms cms pcname cname bcs const_pool cp l bd level ss2 in
       invokestatic_mc
    | JL.OpLookupSwitch (_,x) as op -> 
       let mn = make_ms "f_lookupswitch" [(TBasic `Int)] None in
       let () = byte_code_method_invoke op mn mstack marray pms cms pcname cname bcs const_pool cp l bd level ss2 in
       invokestatic_mc
    | JL.OpReturn x ->
       (match x with
	| `Double | `Long -> 
		     [|Stm;Stm;Dup;Stmrac;Stm;Stm;Stvp;Wait;Wait;
                       Ldmrd; Stbcrd;Stm;Nop;Stsp;Pop;Pop;Ldbcstart;Ldm;Add;
                       Stjpc;Ldm;Ldm;Wait;Wait;Nop|]
	| _ ->
	   [|Stm;Dup;Stmrac;Stm;Stm;Stvp;Wait;Wait;Ldmrd;Stbcrd;Stm;Nop;Stsp;
             Pop;Pop;Ldbcstart;Ldm;Add;Stjpc;Ldm;Wait;Wait;Nop|])
    | JL.OpAReturn -> 
       [|Stm;Dup;Stmrac;Stm;Stm;Stvp;Wait;Wait;Ldmrd;Stbcrd;Stm;Nop;Stsp;
	 Pop;Pop;Ldbcstart;Ldm;Add;Stjpc;Ldm;Wait;Wait;Nop|] 
    | JL.OpReturnVoid -> 
       [|Dup;Stmrac;Stm;Stm;Stvp;Wait;Wait;Ldmrd;Stbcrd;Stm;Nop;
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
	| OpInvoke (x,mn) as op ->
	   (match x with
            | `Interface cn -> 
               (* Invoke the method itself!! *)
               let mstacke = Stack.enum mstack in
               let (ms, _) = pipi cn cp op mn in
               let clzms = JClass.get_class_method_signature ms in
               let exists = Enum.exists (fun x -> x = clzms) mstacke in
               if (not exists) then
		 let () = Stack.push clzms mstack in
		 let () = if not (exists_in_clzms_array clzms ss2) then 
			    let () = print_string "This is the level: " in
			    let () = (print_endline >> string_of_int) level in
			    let () = if (!addmethods) && (level > 1) then DynArray.add bd clzms in
			    invoke_method mstack cn mn const_pool cp marray cms cname op l level ss2 clzms in
		 ignore(Stack.pop mstack)
               else ();
               let iwaits = get_invoke_msize true cp cn op mn in
	       let iiwaits = get_invoke_msize false cp cname op cms in
	       let iwaits = Array.append iwaits iiwaits in
               Array.append iwaits invokeinterface_mc
            (* Array.append iwaits invokeinterface_mc *)
            | `Virtual ot -> 
               let cn = (match ot with | TClass x -> x | TArray _ -> raise Internal) in
               let mstacke = Stack.enum mstack in
               let (ms, _) = pipi cn cp op mn in
               let clzms = JClass.get_class_method_signature ms in
               let exists = Enum.exists (fun x -> x = clzms) mstacke in
               if (not exists) then
		 let () = Stack.push clzms mstack in
		 let () = if not (exists_in_clzms_array clzms ss2) then 
			    let () = print_string "This is the level: " in
			    let () = (print_endline >> string_of_int) level in
			    let () = if (!addmethods) && (level > 1) then DynArray.add bd clzms in
			    invoke_method mstack cn mn const_pool cp marray cms cname op l level ss2 clzms in
		 ignore(Stack.pop mstack)
               else ();
               let iwaits = get_invoke_msize true cp cn op mn in
	       let iiwaits = get_invoke_msize false cp cname op cms in
	       let iwaits = Array.append iwaits iiwaits in
               Array.append iwaits invokevirtual_mc
            (* Array.append waits invokevirtual_mc *)
            | `Special cn -> 
               let mstacke = Stack.enum mstack in
               let (ms, _) = pipi cn cp op mn in
               let clzms = JClass.get_class_method_signature ms in
               let exists = Enum.exists (fun x -> x = clzms) mstacke in
               if (not exists) then
		 let () = Stack.push clzms mstack in
		 let () = print_string "This is the level before: " in
		 let () = if not (exists_in_clzms_array clzms ss2) then 
			    let () = print_string "This is the level: " in
			    let () = (print_endline >> string_of_int) level in
			    let () = if (!addmethods) && (level > 1) then DynArray.add bd clzms in
			    invoke_method mstack cn mn const_pool cp marray cms cname op l level ss2 clzms in
		 ignore(Stack.pop mstack)
               else ();
               let iwaits = get_invoke_msize true cp cn op mn in
	       let iiwaits = get_invoke_msize false cp cname op cms in
	       let iwaits = Array.append iwaits iiwaits in
               Array.append iwaits invokestatic_mc
            (* Array.append waits invokestatic_mc *)
            | `Static cn ->
               let cn1 = cn_simple_name cn in
               let mn1 = ms_name mn in
               let mstacke = Stack.enum mstack in
               let (ms, _) = pipi cn cp op mn in
               let clzms = JClass.get_class_method_signature ms in
               let exists = Enum.exists (fun x -> x = clzms) mstacke in
               if cn1 = "Native" then
		 (match mn1 with
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
		  | _ -> 
		     if (Stack.top mstack <> clzms) then
                       let () = Stack.push clzms mstack in
                       let () = if not (exists_in_clzms_array clzms ss2) then 
				  let () = print_string "This is the level: " in
				  let () = (print_endline >> string_of_int) level in
				  let () = if (!addmethods) && (level > 1) then DynArray.add bd clzms in
				  invoke_method mstack cn mn const_pool cp marray cms cname op l level ss2 clzms in
                       ignore(Stack.pop mstack)
		     else ();
		     let iwaits = get_invoke_msize true cp cn op mn in
		     let iiwaits = get_invoke_msize false cp cname op cms in
		     let iwaits = Array.append iwaits iiwaits in
		     Array.append iwaits invokestatic_mc
		 (* Array.append waits invokeinterface_mc *)
		 )
               else if (not exists) then
		 let () = Stack.push clzms mstack in
		 let () = if not (exists_in_clzms_array clzms ss2) then 
			    let () = print_string "This is the level: " in
			    let () = (print_endline >> string_of_int) level in
			    let () = if (!addmethods) && (level > 1) then DynArray.add bd clzms in
			    invoke_method mstack cn mn const_pool cp marray cms cname op l level ss2 clzms in
		 ignore(Stack.pop mstack);
		 let iwaits = get_invoke_msize true cp cn op mn in
		 let iiwaits = get_invoke_msize false cp cname op cms in
		 let iwaits = Array.append iwaits iiwaits in
		 Array.append iwaits invokestatic_mc
               else
		 let iwaits = get_invoke_msize true cp cn op mn in
		 let iiwaits = get_invoke_msize false cp cname op cms in
		 let iwaits = Array.append iwaits iiwaits in
		 Array.append iwaits invokestatic_mc
           (* Array.append waits invokeinterface_mc *)
	   )
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
       let () = byte_code_method_invoke op mn mstack marray pms cms pcname cname bcs const_pool cp l bd level ss2 in
       invokestatic_mc
    (* The new bytecodes!! *)
    | JL.OpNew _ as op -> 
       let mn = make_ms "f_new" [(TBasic `Int)] (Some (TBasic `Int)) in
       let () = byte_code_method_invoke op mn mstack marray pms cms pcname cname bcs const_pool cp l bd level ss2 in
       Array.append newb invokestatic_mc
    | JL.OpNewArray _ 
    | JL.OpANewArray _ as op -> 
       let () = JDumpLow.opcode op |> print_endline in
       let mn = make_ms "f_newarray" [(TBasic `Int);(TBasic `Int)] (Some (TBasic `Int)) in
       let () = byte_code_method_invoke op mn mstack marray pms cms pcname cname bcs const_pool cp l bd level ss2 in
       Array.append newb invokestatic_mc
    | JL.OpAMultiNewArray _ as op -> raise (Opcode_Java_Implemented (JDumpLow.opcode op))
    | JL.OpCheckCast _ as op -> 
       let mn = make_ms "f_checkcast" [(TBasic `Int);(TBasic `Int)] (Some (TBasic `Int)) in
       let () = byte_code_method_invoke op mn mstack marray pms cms pcname cname bcs const_pool cp l bd level ss2 in
       Array.append newb invokestatic_mc
    | JL.OpInstanceOf _ as op ->
       let mn = make_ms "f_instanceof" [(TBasic `Int);(TBasic `Int)] (Some (TBasic `Int)) in
       let () = byte_code_method_invoke op mn mstack marray pms cms pcname cname bcs const_pool cp l bd level ss2 in
       Array.append newb invokestatic_mc

  and byte_code_method_invoke op mn mstack marray pms cms pcname cname bcs const_pool cp l bd level ss2 = 
    let cn = try JFile.get_class cp (make_cn bj1) with
	     |  No_class_found _ -> print_endline (JDumpLow.opcode op); raise Not_found 
	     |  Class_structure_error _ -> print_endline (JDumpLow.opcode op); raise Not_found in
    let m = JClass.get_method cn mn in
    let cn = (match cn with | JClass.JClass x -> x | _ -> raise Internal) in
    let clzms = (JBasics.make_cms cn.JClass.c_name mn) in
    (* let () = if (!addmethods) then DynArray.add bd clzms in *)
    if not (exists_in_clzms_array clzms ss2) then 
      (* let () = (if !addmethods then DynArray.add bd clzms) in *)
      (if not (exists_in_marray marray clzms) then
	 let () = Stack.push clzms ss2 in
	 let cpool = cn.JClass.c_consts in
	 let cpool1 = DynArray.init (Array.length cpool) (fun i -> cpool.(i)) in
	 let m = JHigh2Low.h2l_acmethod cpool1 m in
	 let () = generate_microcode_method mstack marray (Some cms) mn (Some cname) cn.JClass.c_name cpool cp m l (succ level) ss2 in
	 let () = if !addmethods then DynArray.add bd clzms in
	 ignore(Stack.pop ss2)
       else
	 if !addmethods then DynArray.add bd clzms)
    else
      if !addmethods then DynArray.add bd clzms

  (* Generate micro-code for a given method *)
  and generate_microcode_method mstack marray pms cms pcname cname cpool cp m l level ss2 = 
    let () = print_endline m.JClassLow.m_name in
    let bcs = m.JClassLow.m_attributes in
    let bcs = List.filter (fun t -> match t with | JClassLow.AttributeCode _ -> true | _ -> false) bcs in
    if bcs <> [] then
      let bcs = match (List.hd bcs) with | JClassLow.AttributeCode x -> x | _ -> raise Internal in
      let code = (Lazy.force bcs) in
      let attr_l = code.JClassLow.c_attributes in
      let lnt = List.find (fun x -> match x with | JClassLow.AttributeLineNumberTable _ -> true | _ -> false) attr_l in
      let lnt = match lnt with | JClassLow.AttributeLineNumberTable x -> x | _ -> [] in
      let bcs = (Lazy.force bcs).JClassLow.c_code in
      print_endline ("CURRR "^(JBasics.ms_name cms)^"  "^m.JClassLow.m_name);
      let lr = getloopranges l lnt code cname in
      let () =
	print_endline ("===== Method : "^(JPrint.class_method_signature (JBasics.make_cms cname cms)));
	List.iter (function
		    | (x,y,lc) ->
		       print_endline ("start "^(string_of_int x)^" end "^(string_of_int y)^" loop "^(string_of_int lc))
		  ) lr;
	if lr = [] then print_endline ("\n--------------------------------") else print_endline ("--------------------------------")
      in
      let res = Array.fold_lefti (fun t i x -> 
				  let lc = getloopcount lr i x in
				  (match x with | JClassLow.OpInvalid -> () | _ -> print_endline ((string_of_int i)^" "^(JDumpLow.opcode x)^"\t"^(string_of_int lc)));
				  Array.append t 
					       (
						 let bd = DynArray.make 30 in
						 let bcary = generate_microcode_bc mstack marray pms cms pcname cname bcs cpool cp l bd level ss2 x in
						 let bd = (DynArray.to_array bd) in
						 [|(bcary,lc,bd)|]
					       )

				 ) [||] bcs in
      print_endline ("----- done "^(JBasics.ms_name cms));
      DynArray.add marray ((JBasics.make_cms cname cms),res)
    else
      DynArray.add marray ((JBasics.make_cms cname cms),[||])

  and search_super op cp cn ms cnn =
    let cn = 
      match cn with 
      | Some x -> 
	 (try JFile.get_class cp x with
	  |  No_class_found _ -> print_endline (JPrint.jopcode op); raise Not_found 
	  |  Class_structure_error _ -> print_endline (JPrint.jopcode op); raise Not_found)
      | None -> 
	 failwith ("method: " ^ (ms_name ms) ^ " not found in class: " ^ (cn_name cnn)) in
    (* raise (II ("method: " ^ (ms_name ms) ^ " not found in class: " ^ (cn_name cnn))) in *)
    try
      (JClass.get_method cn ms, cn)
    with
      Not_found -> 
      let cn = (match cn with | JClass.JClass x -> x | _ -> raise Internal) in
      search_super op cp cn.JClass.c_super_class ms cnn

  and pipi cn cp op mn =
    let cnn = cn in
    let cn = try JFile.get_class cp cn with
	     |  No_class_found _ -> print_endline (JPrint.jopcode op); raise Not_found 
	     |  Class_structure_error _ -> print_endline (JPrint.jopcode op); raise Not_found in
    try 
      (JClass.get_method cn mn, cn)
    with
      Not_found -> 
      let cn = (match cn with | JClass.JClass x -> x | _ -> raise Internal) in
      search_super op cp cn.JClass.c_super_class mn cnn

  and invoke_method mstack cn mn cpool cp marray cms cname op l level ss2 clzms = 
    if not (exists_in_marray marray clzms) then
      let () = Stack.push clzms ss2 in
      let (m,cn) = pipi cn cp op mn in 
      let cn = (match cn with | JClass.JClass x -> x | _ -> raise Internal) in
      print_endline ("jlkjdlfjlsjf: " ^ (cn_name cn.JClass.c_name));
      let cpool = cn.JClass.c_consts in
      let cpool1 = DynArray.init (Array.length cpool) (fun i -> cpool.(i)) in
      let m = JHigh2Low.h2l_acmethod cpool1 m in
      let () = generate_microcode_method mstack marray (Some cms) mn (Some cname) cn.JClass.c_name cpool cp m l level ss2 in
      ignore(Stack.pop ss2)

  and get_invoke_msize retorinv cp cn op ms = 
    let cnn = cn in
    let cn = try JFile.get_class cp cn with
	     |  No_class_found _ -> print_endline (JPrint.jopcode op); raise Not_found 
	     |  Class_structure_error _ -> print_endline (JPrint.jopcode op); raise Not_found in
    let (m, cn) = 
      try 
	(JClass.get_method cn ms, cn)
      with
	Not_found -> 
	if retorinv then
          let cn = (match cn with | JClass.JClass x -> x | _ -> raise Internal) in
          search_super op cp cn.JClass.c_super_class ms cnn
	else raise Not_found in
    let cn = (match cn with | JClass.JClass x -> x | _ -> raise Internal) in
    let cpool = cn.JClass.c_consts in
    let cpool1 = DynArray.init (Array.length cpool) (fun i -> cpool.(i)) in
    let m = JHigh2Low.h2l_acmethod cpool1 m in
    let bcs = m.JClassLow.m_attributes in
    let bcs = List.filter (fun t -> match t with | JClassLow.AttributeCode _ -> true | _ -> false) bcs in
    if bcs <> [] then
      let bcs = match (List.hd bcs) with | JClassLow.AttributeCode x -> x | _ -> raise Internal in
      let bcs = (Lazy.force bcs).JClassLow.c_code in
      let msize = Array.fold_left (fun v x -> v + (get_method_size x)) 0 bcs in
      let msize = if msize mod 4 = 0 then msize / 4 else msize / 4 + 1 in
      Array.init msize (fun _ -> Wait)
    else
      [||]


  (* Generating micro-code for a given class *)
  let generate_microcode_clazz marray cp clazz l = 
    let llc = JFile.get_class_low cp clazz in
    let () = JDumpLow.dump (IO.output_channel Pervasives.stdout) llc in
    let cn = JLow2High.low2high_class llc in
    let m = JClass.get_method cn JProgram.main_signature in
    let mclzms = JClass.get_class_method_signature m in
    let cn = (match cn with JClass.JClass x -> x | _ -> raise Internal) in
    let cpool = cn.JClass.c_consts in
    let cpool1 = DynArray.init (Array.length cpool) (fun i -> cpool.(i)) in
    let m = JHigh2Low.h2l_acmethod cpool1 m in
    (* The stack to hold the method call sequence *)
    let ss = Stack.create () in
    let () = Stack.push mclzms ss in
    let ss2 = Stack.create () in
    let () = Stack.push (JBasics.make_cms clazz JProgram.main_signature) ss2 in
    generate_microcode_method ss marray None JProgram.main_signature None llc.JClassLow.j_name cpool cp m l 1 ss2

  let wca = Str.regexp ".*\\(//[ ]*@WCA[ ]*loop[ ]*\\(<\\)?=[ ]*\\([0-9]+\\)[ ]*\\)$"

  let pgex = Str.regexp "[ ]*package[ ]+\\(.*\\)[ ]*;[ ]*"

  let cgex = Str.regexp "[ ]*\\(\\(public\\|private\\|protected\\)[ ]+\\)?\\(final[ ]+\\)?class[ ]+\\([a-zA-Z]+[a-zA-Z_0-9]*\\).*"

  let rec getds h cdir =
    if Sys.is_directory cdir then
      let () = Sys.chdir cdir in
      let fl = Sys.readdir "." in
      let () = Array.iter (fun x -> getds h x) fl in
      Sys.chdir ".."
    else if Filename.check_suffix cdir "java" then
      let wl = DynArray.make 50 in
      let ic = open_in cdir in
      let () =
	try
          let ln = ref 1 in
          let rp = ref "." in
          let cn_name = ref "" in
          while true do
            let line = input_line ic in
            let () = if Str.string_match wca line 0 then
		       let ss = Str.matched_group 3 line in
		       DynArray.add wl (!rp, !cn_name, ((string_of_int !ln),ss))
		     else if Str.string_match pgex line 0 then
		       rp := Str.matched_group 1 line
		     else if Str.string_match cgex line 0 then
		       match !cn_name with 
		       | "" -> cn_name := (Str.matched_group 4 line);
		       | _ -> failwith 
				("Currently only single (top-most) class per file is supported : " ^ !cn_name)
            in
            ln := succ !ln
          done
	with
          End_of_file -> close_in ic
      in
      let wl = DynArray.map (fun (x,y,z) -> match x with "." -> (y,z) | _ -> (x^"."^y,z) ) wl in
      let clname = DynArray.fold_left (fun x (b,_) -> match x with "" -> b | _ as s -> if x <> s then failwith "Multiple class defs" else x) "" wl in
      let ll = DynArray.map (fun (_,x) -> x ) wl in
      match DynArray.empty ll with
      | true -> ()
      | false -> 
	 match Hashtbl.find_option h clname with
	 | None -> Hashtbl.add h clname ll
	 | Some _ -> failwith ("Source dir "^(Sys.getcwd ())^" has multiple class definitions : "^clname)


  let parsewca x =
    (* First parsing file *) 
    let x = Str.split (Str.regexp ";") x in
    let x = Array.of_list x in
    let x = Array.map String.trim x in
    Array.fold_left (fun h x -> getds h x ;h ) (Hashtbl.create 50) x

  let rec calc_exec_time jfk (mn,vals) marray tbl =
    let open BatNum in
    if not (exists_in_clzms_array mn jfk) then
      let jkf = mn in
      let _ = Stack.push mn jfk in
      let bb = Array.map (fun (_,_,x) -> x) vals in
      (* let () = print_endline "^^^^^^^^^^^^^^^^^^^^^^^^" in *)
      (* let () = (print_endline >> JPrint.class_method_signature) mn in *)
      (* let () = Array.iter (fun x -> Array.iter (print_endline >> JPrint.class_method_signature) x) bb in *)
      (* let () = print_endline "^^^^^^^^^^^^^^^^^^^^^^^^" in *)
      let vals = Array.fold_left 
		   (fun (i,m) (micro,lc,bd) -> 
		    let (ii,mm) = Array.fold_left (fun (i,m) x -> 
						   if (Array.exists (fun y -> x = y) mem_instr) then 
						     (i,m+lc) 
						   else 
						     (i+lc,m)) (of_int 0, of_int 0) micro in
		    let (ii,mm) = Array.fold_left 
				    (fun (ii,mm) x -> 
				     let indx = DynArray.index_of (fun (y,_) -> x = y) marray in
				     let (mn,vals) = DynArray.get marray indx in 
				     let n = Hashtbl.find_option tbl mn in
				     let (ri,rm) = 
				       match n with 
				       | Some n -> n 
				       | None -> calc_exec_time jfk (mn,vals) marray tbl
				     in
				     (* print_endline "tutu start"; *)
				     (* (print_endline >> JPrint.class_method_signature) jkf; *)
				     (* (print_endline >> JPrint.class_method_signature) mn; *)
				     (* (print_string >> to_string) ri; *)
				     (* print_string " "; *)
				     (* (print_endline >> to_string) rm; *)
				     (* print_endline "tutu end"; *)
				     (ii+(ri*lc),mm+(rm*lc))
				    ) (ii,mm) bd in
		    (i+ii,mm+m)
		   ) (of_int 0, of_int 0) vals in
      let _ = Stack.pop jfk in
      (* let () = Hashtbl.add tbl mn vals in *)
      vals
    else (of_int 0, of_int 0)

  let main = 
    try
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
	if DynArray.length args <> 2 then let () = print_endline usage_msg; Arg.usage speclist "[OPTION]:" in raise Internal
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
    with
    | Internal -> ()
