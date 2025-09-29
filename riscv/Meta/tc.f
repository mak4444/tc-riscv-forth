
[IFNDEF] #HEADER
: #HEADER ( cfa -- )
  HERE >R
  DP M! HEADER
 R> DP M! ;
[THEN]

<DBG>

: >SEG_DT ( madr tadr -- )
 SEG_DT_END @ ABORT" descriptor table is full"
 SEG_DT
 BEGIN  CELL+ CELL+  2DUP @ U<
 UNTIL  \  madr tadr dtadr
 DUP CELL+ @
  IF ." conflict with $" CELL- CELL- @ h. ." seg" ABORT
  THEN
 DUP
 DUP CELL+ CELL+ SEG_DT_END OVER - CMOVE>
 >R SWAP OVER - SWAP
 R> 2!
;

: T> ( tadr -- madr )
  SEG_DT
 BEGIN CELL+ CELL+ 2DUP @  U<
 UNTIL 
  CELL+ @ + 
;

riscv\Meta\mhead.f

REQUIRE RISCV_MOD riscv\Meta\riscv32.4th
riscv\Meta\LEX.F
riscv\Meta\prefix.f

MODULE: TCCM

: M_CMOVE ( c-addr1 c-addr2 u --- )
\ Copy u bytes starting at c-addr1 to c-addr2, proceeding in ascending
\ order.

   DUP IF  >R
   BEGIN
    OVER 
 MC@
 SWAP DUP >R
 C!
 R> 1+ SWAP 1+ SWAP
    R>  1- DUP  >R 0=
   UNTIL
   R>
   THEN
   2DROP DROP
;

REQUIRE DISARM	riscv\Meta\dist2.f

: [RISCV] ALSO RISCV_MOD ; IMMEDIATE

[IFNDEF] OR!	:  OR! ( N ADDR -- )	DUP @ ROT OR	SWAP ! ; [THEN]
[IFNDEF] AND!	: AND! ( N ADDR -- )	DUP @ ROT AND	SWAP ! ; [THEN]


: CMOVE ( c-addr1 c-addr2 u --- )
\ Copy u bytes starting at c-addr1 to c-addr2, proceeding in ascending
\ order.

   DUP IF  >R
   BEGIN
    OVER 
\ ." C=" DUP H.
 C@
 SWAP DUP >R
 C!
 R> 1+ SWAP 1+ SWAP
    R>  1- DUP  >R 0=
   UNTIL
   R>
   THEN
   2DROP DROP
;

: M_CMOVE ( c-addr1 c-addr2 u --- )
\ Copy u bytes starting at c-addr1 to c-addr2, proceeding in ascending
\ order.

   DUP IF  >R
   BEGIN
    OVER 
 MC@
 SWAP DUP >R
 C!
 R> 1+ SWAP 1+ SWAP
    R>  1- DUP  >R 0=
   UNTIL
   R>
   THEN
   2DROP DROP
;

: ><DP DP M@ T-DP M@
       DP M! T-DP M! ;

: IALLOT ALLOT ;

: TCONSTANT
 ><DP CONSTANT   ><DP ;

: CONSTANT HERE THERE? IF TCONSTANT BREAK CONSTANT ;

: #define  HERE THERE? IF ><DP #define ><DP BREAK #define ;
: ?#define >IN @  POSTPONE [DEFINED]  IF DROP POSTPONE \ BREAK >IN ! #define  ;

: MCREATE CREATE  ;
: ICREATE HERE TCONSTANT ;

: IVARIABLE ICREATE 0 L, ;

: ICELLS 2 << ;

: $, ( addr u -- ) \ 
 DP M@ SWAP DUP IALLOT
   M_CMOVE ;

: >SEG_DT_H    ( len segRUN -- )
  DUP DUP >SEG_DT SWAP \ 2*
  +  HERE SWAP >SEG_DT
;

: CM_SEG2: ( len segLD segRUN  -- )
\  HERE $C + OVER >SEG_DT
  HERE OVER ><DP MCREATE  ML,  M, 1 ML, ><DP \ len segLD segRUN
  SWAP ML,	 \ len segRUN
  OVER 2/ ML, 0 ML,
  OVER IALLOT \ len segRUN
\  SWAP 2/ SWAP
  >SEG_DT_H
 ; 

: SEG2: ( len segLD segRUN  -- ) CM_SEG2: ;

: CM_SEG: ( len seg -- )  DUP  CM_SEG2: ;

: SEG: ( len seg -- )  DUP  CM_SEG2: ;

: SEGINFO
  DUP  ."  LAST ADR="  L@ MH. CR
\  DUP  ."  LAST ="  4+ CELL+  L@ MH. CR
    4+ @
  DUP  ." LOAD ADR =" L@ MH. CR
  DUP  ." MAX SIZE =" 4+ L@ MH. CR
  DUP  ." CUR SIZE =" 4+ 4+ L@ MH. CR

  DROP
; 

: THERE?T DUP T> SWAP <> ;

: ?COMP_ STATE M@ =
	IF  COMPILE,
	ELSE EXECUTE
	THEN ;

EXPORT

0 value ?SOURCETYPE

: TCCM-INTERPRET ( -> )
    SAVEERR? ON
 ?SOURCETYPE IF  CR SOURCE TYPE key drop THEN
\   SOURCE-ID 0= IF  MAIN_S THEN
  BEGIN
    PARSE-NAME
 DUP
  WHILE
    SFIND ?DUP
    IF [']  ?COMP_  CATCH 
        THROW
    ELSE  ?SLITERAL
    THEN
    ?STACK
  REPEAT 2DROP 
\   SOURCE-ID 0= IF  MAIN_S THEN
;

: 8000:
    BASE M@ >R HEX
   PARSE-NAME

	  NUMBER?
	0= THROW  THROW
	>IN @ 
\ DUP H. CR SOURCE DUMP KEY DROP
 $C >
	IF L, ELSE  W,
		PARSE-NAME >IN @ $10 =
		if 	  NUMBER?	0= THROW  THROW W,
			PARSE-NAME >IN @ $15 =
			if 	  NUMBER?	0= THROW  THROW W,
				PARSE-NAME >IN @ $1A =
				if 	  NUMBER?	0= THROW  THROW W,
				else 2drop
				then
			else 2drop
			then
		else 2drop
		then
	 THEN \  7 c,  >in @ c,
(
	 PeekChar BL <>
	 IF
	   PARSE-NAME
	  NUMBER?
	0= THROW  THROW
	  W,
 THEN        )

   R> BASE M!
	POSTPONE \
;
: ####: 8000: ;
: ____: DEPTH IF ABORT THEN $12 >IN M!  ;
: FLD 6 >IN M! 8000: ;
: FLW 6 >IN M! 8000: ;
: FMADD.D FLD ;
: .INSN FLD ;
: FSW FLD ;

: QQ DP M! ;

\ ' TCCM-INTERPRET  &INTERPRET !
: TSAVE ( <filename> -- )
 PARSE-NAME W/O CREATE-FILE THROW  TO T-STDOUT
 MSTR_IMG DUP L@ H.  4+
 BEGIN DUP @ 
 WHILE

 CR
 DUP L@ H.
 DUP 4+ L@  H.
 DUP 8 + L@  H.
\ DUP 8 + L@ 0 ?DO DUP I +  $C + C@ H.  LOOP
 DUP L@ T> OVER 8 + L@ 2* DUP H. T-STDOUT WRITE-FILE THROW

 DUP 4+ L@

	2* + $C +
 REPEAT DROP
;


;MODULE

