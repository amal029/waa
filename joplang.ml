(* The micro-codes implemented in JOP *)
(* Each micro-code takes 1 cpu cc to execute. *)
(* Author: Avinash *)
(* Date: Fri Sep 12 13:52:29 NZST 2014 *)


type jop_microcode =
  | Pop
  | And
  | Or
  | Xor
  | Add
  | Sub
  | Stmul
  | Stmwa
  | Stmra 			(* Mem *)
  | Stmrac			(* Mem *)
  | Stmraf			(* Mem *)
  | Stmwd			(* Mem *)
  | Stald			(* Mem *)
  | Stast			(* Mem *)
  | Stgf			(* Mem *)
  | Stgs			(* Mem *)
  | Stpf			(* Mem *)
  | Stcp			(* Mem *)
  | Stbcrd			(* Mem *)
  | St0
  | St1
  | St2
  | St3
  | St4
  | St
  | Stmi
  | Stvp
  | Stjpc
  | Star
  | Stsp
  | Ushr
  | Shl
  | Shr
  | Stm
  | Bz
  | Bnz
  | Nop
  | Wait
  | Jbr
  | Ldm
  | Ldi
  | Ldmrd
  | Ldmul
  | Ldbcstart
  | Ld0
  | Ld1
  | Ld2
  | Ld3
  | Ld4
  | Ld
  | Ldmi
  | Ldsp
  | Ldvp
  | Ldjpc
  | Ld_opd_8u
  | Ld_opd_8s
  | Ld_opd_16u
  | Ld_opd_16s
  | Dup
  | Jmp
  | Cinval 			(* Cache invalidate instruction, is this mem-instruction?? *)


let mem_instr = [|Stmra; Stmrac; Stmwd; Stald; Stast; Stgf; Stpf; Stcp; Stbcrd; Stgs; Stmraf|]
