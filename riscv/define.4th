
MODULE: TCCM


#define CELL_SIZE 4
#define DATA_STACK_MAX_SIZE 32
#define RETURN_STACK_MAX_SIZE 64

#define HEADER_NAME_BUF_SIZE 32
#define HEADER_CODE_BUF_SIZE 32

#define DATA_STACK_MAX_SIZE_BYTES DATA_STACK_MAX_SIZE CELL_SIZE *


#define TOKEN_BUFFER_MAX_SIZE 32
#define LINE_BUFFER_MAX_SIZE 128

#define UART_BASE 0x10000000
#define RAM_END 0x88000000

\ flags

#define COMPILE_BIT 1
#define COMMENT_BIT 2
#define FIND_TOKEN_ERROR_BIT 4
#define NUM_IO_HEX_BIT 8   \ if not set, num io is decimal


: SaveReturnAddress 
    S" addi sp, sp, -16" EVALUATE  \ allocate 16 bytes on stack
    S" sw   ra, 12(sp)" EVALUATE  \ store return address on stack
;

: RestoreReturnAddress
    S" lw   ra, 12(sp)" EVALUATE  \ load return address from stack
    S" addi sp, sp, 16" EVALUATE  \ restore stack pointer
;

: end_word
    S" addi s0, s0, CELL_SIZE" EVALUATE
    S" lw t0, 0(s0)" EVALUATE
    S" jalr ra, t0, 0" EVALUATE
;


MCREATE MACROSTR $101 IALLOT


0 VALUE TLINK
0 VALUE TLAST

: word_header_first
 HERE  \ for TLAST
\ HEX F7_ED
  S" CONSTANT " MACROSTR $!
  C"  ," MTOKEN MACROSTR $+!
   S" _impl" MACROSTR  $+!
  '"' PARSE 2DROP \ TYPE
  POSTPONE S"
  
   DP M@   SWAP   M_CMOVE 
     HEADER_CODE_BUF_SIZE  IALLOT
  HERE TO TLINK
  0 L,
   TLAST
   L,
   TO TLAST
      ',' PARSE 2DROP \  TYPE
   PARSE-NAME   DROP C@   '0' -    L,

   POSTPONE \
  HERE  MACROSTR COUNT EVALUATE
    [ ALSO RISCV_MOD ] -1 TO ?DOADSP  [P] RV_ASM_BIG
;

: word_header
\  F7_ED
  [ ALSO RISCV_MOD ]
  MOREPASS  END-CODE
   IF BREAK
  [P]
  
  HERE  TLINK L!
   word_header_first
;
: END-CODE_
 hex F7_ED
 here
 h.
  [ ALSO RISCV_MOD ]
  MOREPASS
  END-CODE
   IF BREAK
  [P] 
   here
 h.

;

: word_header_
 hex F7_ED
 here
 h.
  [ ALSO RISCV_MOD ]
  MOREPASS
  END-CODE
   IF BREAK
  [P]

  HERE
  TLINK L!
   word_header_first
;


0 VALUE OFFSET_CODE
#define  OFFSET_NEXT HEADER_CODE_BUF_SIZE
#define  OFFSET_PREV OFFSET_NEXT  CELL_SIZE +
#define  OFFSET_IMM OFFSET_PREV  CELL_SIZE +
#define  HEADER_SIZE OFFSET_IMM  CELL_SIZE +

MODULE: RISCV_MOD

: word_next, ( regPtrHeader regPtrOut -- ) SWAP  OFFSET_NEXT addi, ;
: word_code, ( regPtrHeader regPtrOut -- ) SWAP  OFFSET_CODE addi, ;
P: word_next,
P: word_code,

: PopStack { regBase regSize regVal -- }
    S" c.addi " MACROSTR $!
	regSize COUNT \ 2DUP TYPE
	MACROSTR $+! S" ," MACROSTR $+!
	S"  -4" MACROSTR $+!
\	MACROSTR COUNT TYPE
  MACROSTR COUNT EVALUATE
\      addi \regSize, \regSize, -CELL_SIZE
    S" add t0," MACROSTR $!
	regBase COUNT  MACROSTR $+! S" ," MACROSTR $+!
	regSize COUNT  MACROSTR $+!
  MACROSTR COUNT EVALUATE
\    add t0, \regBase, \regSize
    S" lw " MACROSTR $!
	regVal COUNT  MACROSTR $+!
	S" , 0(t0)" MACROSTR $+!
  MACROSTR COUNT EVALUATE
\    lw \regOutVal, 0(t0)
;
 
: PopDataStack ( <regVal> -- )
\  HEX F7_ED
	 C" s1"	 C" s2"	 BL WORD PopStack
;

: PopReturnStack ( <regVal> -- )
\  HEX F7_ED
	 C" s3"	 C" s4"	 BL WORD PopStack
;

: PushStack { regBase regSize regVal -- }
\      add t0, \regBase, \regSize
    S" add t0," MACROSTR $!
	regBase COUNT MACROSTR $+! S" ," MACROSTR $+!
	regSize COUNT MACROSTR $+!
\	MACROSTR COUNT TYPE
  MACROSTR COUNT EVALUATE

\    sw \regVal, 0(t0)
    S" sw " MACROSTR $!
	regVal COUNT  MACROSTR $+!
	S" ,(t0)" MACROSTR $+!
  MACROSTR COUNT EVALUATE
  
\   addi \regSize, \regSize, CELL_SIZE
    S" c.addi " MACROSTR $!
	regSize COUNT  MACROSTR $+!
	S" , CELL_SIZE" MACROSTR $+!
  MACROSTR COUNT EVALUATE
;
 
: PushDataStack ( <regVal> -- )
\  HEX F7_ED
	 C" s1"	 C" s2"	 BL WORD PushStack
;

: PushReturnStack ( <regVal> -- )
\  HEX F7_ED
	 C" s3"	 C" s4"	 BL WORD PushStack
;

: PushReg ( <reg> -- )
   S" c.addi sp, -4" EVALUATE
\     sw \reg, 0(sp)
     S" sw " MACROSTR $!
	 PARSE-NAME MACROSTR $+!
	S" , 0(sp)" MACROSTR $+!
  MACROSTR COUNT EVALUATE
; 

: PopReg ( <reg> -- )
\     lw \reg, 0(sp)
     S" lw " MACROSTR $!
	 PARSE-NAME MACROSTR $+!
	S" , 0(sp)" MACROSTR $+!
  MACROSTR COUNT EVALUATE
   S" c.addi sp, 4" EVALUATE
; 

: secondary_word
    S" PushReturnStack s0" EVALUATE
    S" la s0, 3f" EVALUATE
    S" lw t0, 0(s0)" EVALUATE
    S" jalr ra, t0, 0" EVALUATE
    S" 3:" EVALUATE
	POSTPONE \
;
	
0 VALUE 'branch
0 VALUE 'branchIfZero

: S_AHEAD ( -- orig )  'branch L, HERE 0 L, ;
: S_IF ( -- orig )  'branchIfZero L, HERE 0 L, ;
: S_THEN ( orig -- )  HERE OVER - SWAP  L!  ;
: S_ELSE ( orig -- orig' ) S_AHEAD SWAP S_THEN ;
: S_BEGIN (  -- dest ) HERE ;
: S_UNTIL ( dest --  ) 'branchIfZero L, HERE - L, ;


;MODULE

\ preliminary values

0x80000020 #HEADER initUart
0x80000050 #HEADER puts
0x80000086 #HEADER getc
0x8000007c #HEADER notgotchar
0x800000e0 #HEADER vm_run
0x80000136 #HEADER temit
0x800010fe #HEADER strcmp_fs_cs
0x80001190 #HEADER fatoi

0x80001240 #HEADER itofa
0x80001318 #HEADER itofa_dec
0x80001376 #HEADER forth_string_to_c
0x80002b64 CONSTANT outerInterpreter_impl

0x8000427f CONSTANT title_string_f
0x800042a3 CONSTANT _dataEnd

;MODULE

