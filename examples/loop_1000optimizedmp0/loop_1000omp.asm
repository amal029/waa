start NOOP ;starting the program
  LDR R0 #0;
  SSVOP R0;
  LDR R1 #0 ; Setting all the output signals statuses to zero
  STR R1 $1 ;Storing the statuses of all the output signals in to the dedicated mem space for outputsignals
  LDR R1 #0
  STR R1 $2 ; Setting internal signals to zero
  LDR R1 #0 ; storing zero to pre-insigs
  STR R1 $3
  LDR R1 #0 ; storing zero to pre-osigs
  STR R1 $4
  LDR R1 #0 ;storing zero to pre-dsigs
  STR R1 $5 ;Storing them into the mem
  LDR R0 #0
  STR R0 $6; Terminate Node stored 0
  LDR R0 #case8 ;
  STR R0 $7; Switch Node 

BEGIN0 NOOP; loading the num which have to be init
  LDR R7 #0;
  LDR R8 #0;previous clock-domain num
SEOT9 CLFZ;
  LDR R0 #0;clearing 
  LDR R1 #0;clearing 
  LDR R11 #0;clearing (This register will always contain zeroes !)
LERR9  LER R0;loading the EREADY bit which is set from ENV
  PRESENT R0 LERR9;
FER9 SEOT; This is basically the SEOT tick
  CER;clear the EREADY bit
  LDR R0 $0001; loading the output signals
  AND R1 R0 #$ffff;clearing output sig fields
  STR R1 $1
  AND R0 R0 #$0;
  SSOP R0;throwing output signals to env
; Updating pre sigs - Delayed semantics 
  STR R0 $4 ;store it to pre-osig of this CD 
  LDR R0 $2; loading signals
  STR R0 $5; storing to delayed
;Setting the declared signals and terminate node to 0
  STR R11 $2 ; DSigs
  LSIP R0;getting input signals from SIP
  AND R0 R0 #$0;
  LDR R1 $0000;
  AND R2 R1 #$0;
  STR R2 $3; storing insigs to delayed
  AND R1 R1 #$ffff;
  OR R0 R0 R1;
  STR R0 $0000;storing SIP signals in mem
  LDR R0 #$8000
  DCALLBL R0 ; casenumber 0
  CEOT;now start processing
RUN0 NOOP; the locks need to be inside the memory since if they are here then I am just eating up logic
  STR R11 $6; storing zero to this CD's Terminate Node
  LDR R7 #0;
  LDR R8 #0;previous clock-domain num
  LDR R0 $7
  JMP R0 ; SwitchNode unconditional jump
case7 NOOP; Switch Child branch
  LDR R0 #case7
  STR R0 $7; EnterNode storing statecode : 0
  JMP END0; Jumping to END 
  JMP ENDS30
case8 NOOP; Switch Child branch
  LDR R0 #0; COMPILER BUG!!!! accessing 2 when there are only 2 childs
;this only happens to the last Switch Node so if this is not the case I made a mistake
  STR R0 $7; EnterNode storing statecode : 2
  LDR R0 #32769 ; loading case number
  DCALLBL R0 ; casenumber 1
  LDR R0 #case7
  STR R0 $7; EnterNode storing statecode : 0
  LDR R0 #case7
  STR R0 $7; EnterNode storing statecode : 0
  JMP END0; Jumping to END 
  JMP ENDS31
ENDS30 NOOP 
ENDS31 NOOP 
END0 JMP BEGIN0
  ENDPROG
