(* The micro-codes implemented in JOP *)
(* Each micro-code takes 1 cpu cc to execute. *)
(* Author: Avinash *)
(* Date: Fri Sep 12 13:52:29 NZST 2014 *)
open Sexplib.Std

type annot = {avgp : float}
with sexp

(* type jop_microcode = .. *)

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
   | Stidx
   | Stps
   | Cyc 			(* Harware counter *)
   | Ldcr			(* Load hardware counter value *)
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
with sexp

type jop_pmicrocode =
   | PPop of annot
   | PAnd of annot
   | POr of annot
   | PXor of annot
   | PAdd of annot
   | PSub of annot
   | PStmul of annot
   | PStmwa of annot
   | PStmra of annot
   | PStmrac of annot
   | PStmraf of annot
   | PStmwd of annot
   | PStald of annot
   | PStast of annot
   | PStgf of annot
   | PStgs of annot
   | PStpf of annot
   | PStcp of annot
   | PStbcrd of annot
   | PStidx of annot
   | PStps of annot
   | PCyc  of annot
   | PLdcr of annot
   | PSt0 of annot
   | PSt1 of annot
   | PSt2 of annot
   | PSt3 of annot
   | PSt4 of annot
   | PSt of annot
   | PStmi of annot
   | PStvp of annot
   | PStjpc of annot
   | PStar of annot
   | PStsp of annot
   | PUshr of annot
   | PShl of annot
   | PShr of annot
   | PStm of annot
   | PBz of annot
   | PBnz of annot
   | PNop of annot
   | PWait of annot
   | PJbr of annot
   | PLdm of annot
   | PLdi of annot
   | PLdmrd of annot
   | PLdmul of annot
   | PLdbcstart of annot
   | PLd0 of annot
   | PLd1 of annot
   | PLd2 of annot
   | PLd3 of annot
   | PLd4 of annot
   | PLd of annot
   | PLdmi of annot
   | PLdsp of annot
   | PLdvp of annot
   | PLdjpc of annot
   | PLd_opd_8u of annot
   | PLd_opd_8s of annot
   | PLd_opd_16u of annot
   | PLd_opd_16s of annot
   | PDup of annot
   | PJmp of annot
   | PCinval of annot
with sexp

let mem_instr =
  [|
    PLdm {avgp = 292.0};
    PStps {avgp = 290.0};
    PStmra {avgp = 290.0};
    PStmwa {avgp = 290.0};
    PStmrac {avgp = 290.0};
    PLdmrd {avgp = 291.0};
    PLdbcstart {avgp = 290.0};
    PStmwd {avgp = 290.0};
    PStald {avgp = 290.0};
    PStast {avgp = 290.0};
    PStgf {avgp = 209.0};
    PStpf {avgp = 290.0};
    PStcp {avgp = 290.0};
    PStbcrd {avgp = 290.0};
    PStgs {avgp = 290.0};
    PStmraf {avgp = 290.0};
    PWait {avgp = 290.0}
   |]



let get_power = function
  | PPop x -> x.avgp
  | PAnd x -> x.avgp
  | POr x -> x.avgp
  | PXor x -> x.avgp
  | PAdd x -> x.avgp
  | PSub x -> x.avgp
  | PStmul x -> x.avgp
  | PStmwa x -> x.avgp
  | PStmra x -> x.avgp
  | PStmrac x -> x.avgp
  | PStmraf x -> x.avgp
  | PStmwd x -> x.avgp
  | PStald x -> x.avgp
  | PStast x -> x.avgp
  | PStgf x -> x.avgp
  | PStgs x -> x.avgp
  | PStpf x -> x.avgp
  | PStcp x -> x.avgp
  | PStbcrd x -> x.avgp
  | PStidx x -> x.avgp
  | PStps x -> x.avgp
  | PCyc  x -> x.avgp
  | PLdcr x -> x.avgp
  | PSt0 x -> x.avgp
  | PSt1 x -> x.avgp
  | PSt2 x -> x.avgp
  | PSt3 x -> x.avgp
  | PSt4 x -> x.avgp
  | PSt x -> x.avgp
  | PStmi x -> x.avgp
  | PStvp x -> x.avgp
  | PStjpc x -> x.avgp
  | PStar x -> x.avgp
  | PStsp x -> x.avgp
  | PUshr x -> x.avgp
  | PShl x -> x.avgp
  | PShr x -> x.avgp
  | PStm x -> x.avgp
  | PBz x -> x.avgp
  | PBnz x -> x.avgp
  | PNop x -> x.avgp
  | PWait x -> x.avgp
  | PJbr x -> x.avgp
  | PLdm x -> x.avgp
  | PLdi x -> x.avgp
  | PLdmrd x -> x.avgp
  | PLdmul x -> x.avgp
  | PLdbcstart x -> x.avgp
  | PLd0 x -> x.avgp
  | PLd1 x -> x.avgp
  | PLd2 x -> x.avgp
  | PLd3 x -> x.avgp
  | PLd4 x -> x.avgp
  | PLd x -> x.avgp
  | PLdmi x -> x.avgp
  | PLdsp x -> x.avgp
  | PLdvp x -> x.avgp
  | PLdjpc x -> x.avgp
  | PLd_opd_8u x -> x.avgp
  | PLd_opd_8s x -> x.avgp
  | PLd_opd_16u x -> x.avgp
  | PLd_opd_16s x -> x.avgp
  | PDup x -> x.avgp
  | PJmp x -> x.avgp
  | PCinval x -> x.avgp
