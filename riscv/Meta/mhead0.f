REQUIRE WORDS tools/spf_words.f

: MC! C! ;
: MC@ C@ ;
: MW! W! ;
: MW@ W@ ;
: ML@ L@ ;
: ML! L! ;
:  M!  ! ;
:  M@  @ ; 
: M2@ 2@ ; 
: M0! 0! ;
: M_0! 0! ;
: M+! +! ;
:  M+  + ;
: M2+  2+ ;
:  M-  - ;
: M1+! 1+! ;
: M-/ / ;
: M, , ;
: ML, L, ;

: MNEGATE NEGATE ; 
: MOVER OVER ;
: MAND AND ;
: MCR CR ;
\- MH.
 : MH. H. ;
: MHERER H. ;
: M_. . ;
: M.S .S ;
: MDEPTH DEPTH ;
: MHEX HEX ;
: MKEY KEY ;
: MKEY? KEY? ;
: MEXECUTE EXECUTE ;
: MWDS WORDS ;
: MCMOVE CMOVE ;
: MFILL FILL ;
: \ZEOF ;
: MCELLS CELLS ;
: MCELL+ CELL+ ;
: MCELL CELL ;
: MALLOT ALLOT ;

VARIABLE T-DP


[IFNDEF] /*
: /*  ( -- )
  BEGIN
    PARSE-NAME DUP 0=
    IF  NIP  REFILL   0= IF DROP TRUE THEN
    ELSE  S" */" COMPARE 0=  THEN
  UNTIL
; IMMEDIATE
[THEN]
