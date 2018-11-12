         BEGIN NAME=MC5A,VERSION=00,AMODE=31
*-------------------------------------------------------------*
******   TO FETCH THE FILE AND APPEND/DISPLAY THE CONTENTS ****
******   WHERE APPEND WILL BE DONE IN NEXT ENTERED PROG   *****
*-------------------------------------------------------------*
* * WE WILL FETCH THE FILE IN HOLD OR NOT IN HOLD             *
*   WHEN ITS JUST TO DISPLAY, NOT TO UPDATE, SO USED FINWC    *
*   OTHERWISE USED FIWHC                                      *
* * FILE HAS TAKEN INTO THE DATA LEVEL 1                      *
* * EXTERNAL DSECT, CC0CC USED TO MAP WITH FILE CONTENTS      *
*   AND MI0MI TO MAP THE INPUT CONTENTS                       *
* * ADD WILL BE DONE IN NEXT FILE USING ENTDC                 *
* * THIS FILE IS FOR DISPLAY                                  *
***************************************************************
*----------------- EXPECTED INPUT ----------------------------*
*  1.  ZSMC2 D                                                *
*  2.  ZSMC3 A 123456789012345/0216/3000/A                    *
*----------------- EXPETED OUTPUT ----------------------------*
*---- FOR FIRST ONE-------------------------------------------*
* CARD NO         EXP  LIMIT STATUS                           *
* 123456789213243 0216 10000 A                                *
* 770212330493944 0317 04321 A                                *
* ............... .... ..... .                                *
*      LIST OF CARD MEMBER DETAILS                            *
*---- FOR SECOND ONE------------------------------------------*
*  RECORD ADDED SUCCESSFULLY :)                               *
***************************************************************
*------------------OUTPUT FOR INVALIDITY----------------------*
*  ** IF INPUT IS OTHER THAN EXPECTED
*     -INVALID INPUT PLEASE ENTER JUST D FOR DISPLAY          *
*  ** IF FACE SEGMENT FAILS TO GIVE FILE ADDESS               *
*     - ERROR OCCURED IN FACE                                 *
*  ** IF FILE IS NOT FOUND                                    *
*     - COULDNT FIND THE FILE                                 *
*  ** IF CARD NUM GIVEN IS NOT UNIQUE                         *
*     - THIS CARD ALREADY EXISTING                            *
*  ** IF FILE IS FULL WITH RECORDS                            *
*     - FILE IS FULL U CANT ADD ANYTHING                      *
*  ** IF EXPIRY DATE IS NOT PROPER                            *
*     - EXPIRY DATE INVALID                                   *
*  ** IF CARD STATUS IS OTHER THAN A/D/L/S/E                  *
*     - CARD STATUS INVALID                                   *
***************************************************************
* REGISTER USED:
*             R0- ORDINAL NUM PARAMETER FOR FACE SEGMENT      *
*                   AND AS A COUNTER                          *
*             R1- BASE OF MI0MI, THEN AS A COUNTER            *
*             R2- BAS SUBROUTINE ADDRESS                      *
*             R3- SCRATCH REGISTER                            *
*             R4- BASE OF CC0CC                               *
*             R5- TO SHOW THE ERROR DISPLAY, FIXED WITH WTOPC *
*                   AND SOMETIME AS SCRATCH REGISTER ALSO     *
*             R6- AS A PARAMETER OF FACE                      *
*             R7- AS A PARAMETER FOR FACE                     *
*             R14- AS A SCRATCH                               *
*-------------------------------------------------------------*
*-------------------------------------------------------------*
MYAREA   DSECT
AA       DS    CL2                 CHECK ENTRY
CARD     DS    XL15                CARD NUMBER
SLASH1   DS    CL1                 FIRST SLASH
EXP      DS    0CL4                EXPIRATION DATE
MM       DS    CL2                 MONTH
YY       DS    CL2                 YEAR
SLASH2   DS    CL1                 SECOND SLASH
LIM      DS    XL5                 CARD LIMIT
STA      DS    X                   CARD STATUS
LIMZ     DS    XL5                 LIMIT ZONED
*-------------------------------------------------------------*
$IS$     CSECT
         BAS   R2,MC5A0000         GO TO VALIDATION OF ENTRY
         BAS   R2,MC5A0020         FIND THE FILE ADDRESS
         BAS   R2,MC5A0030         FETCH THE FILE
         B     MC5AEXIT            EXIT FROM THE PROGRAM
MC5AEXIT EQU   *
         WTOPC TEXT=' ',CHAIN=YES,ENDOFM=YES,HEADER=NO
         CRUSA S0=1,S1=2           MAKING SURE TO RELEASE BLOCK
         EXITC                     EXIT
*--------------VALIDATION FOR DISPLAY-------------------------*
MC5A0000 EQU   *
         MI0MI REG=R1              R1 AS BASE OF MI0MI
         LA    R1,CR0DTN           POINT R1 TO START OF THE INPUT
         CLC   DPLUS,0(R1)         CHECK IF D ONLY
         BNE   MC5A0010            IF NOT THEN ERROR
         CLC   1(2,R1),3(R1)       IF THERE IS ANOTHER CHAR AFTER D
         BE    MC5A5000            THEN INVALID
         CLC   2(2,R1),4(R1)       IF ITS WELL GO AHEAD
         BE    MC5A5000            ELSE ERROR
         CLC   3(2,R1),5(R1)       IF ITS WELL GO AHEAD
         BE    MC5A5000            ELSE ERROR
         BR    R2                  BACK TO MAIN
*---------------ENTER TO THE ADD PROGRAM----------------------*
MC5A0010 EQU   *
         ENTDC MC5B
*--------------------FINDING TH FILE ADDRESS------------------*
MC5A0020 EQU   *
         LA    R6,#MISCL           TAKE RECORD TYPE IN R6
         LA    R0,1                ORDINAL NUM IN R0
         LA    R7,CE1FA1           FILE ADDRESS IN R7
         ENTRC FACE                GO TO FACE SEGMENT
         LTR   R0,R0               CHECK R0 IS ZERO
         BZ    MC5A5010            IF YES THEN ERROR
         MVC   CE1FA1(2),AONE      ELSE MOVE A1 AS RECORD ID
         WTOPC TEXT='CARD NO         EXP  LIMIT STATUS     ',HEADER=NO,X
               CHAIN=YES,ENDOFM=NO
         LHI   R5,20
         BR    R2                  BACK TO THE MAIN FUNCTION
************************ DISPLAY ******************************
*----------------FETCH THE FILE FOR DISPLAY-------------------*
MC5A0030 EQU   *
         FINWC D1,MC5A5020         FIND AND WAIT FOR FILE
         L     R4,CE1CR1           TAKE CONTENT IN R4
         CC0CC REG=R4              BASE R4 TO CC0CC
         LHI   R1,20               LOAD COUNTER WITH 20
         XR    R1,R1               CLEAR R1
         ICM   R1,B'0011',CC0NAB   LOAD THE NAB VALUE IN REG
         CHI   R1,#CC0HRD          COMPARE IF HIGH THAN HEADER SIZE
         BNH   MC5A5030            IF NOT SHOW EMPTY FILE
         LHI   R3,#CC0HRD          TAKE HEADER LEN IN REG
         SR    R1,R3               SUBTRACT IT FROM NAB VALUE
         LHI   R3,#CC0LEN          TAKE RECORD LEN IN REG
         XR    R0,R0               CLEAR R0
         DR    R0,R3               (NAB-HDR_LEN)/REC_LEN
         LTR   R1,R1               COMPARE THE NUM_OF_REC WITH ZERO
         BZ    MC5A5030            IF THEN SHOW EMPTY RECORD
         CHI   R1,R5               COMPARE IF MORE NUM OF LREC
         BNH   MC5A0040            IF NOT THEN DISPLAY OTHERWISE
         LR    R1,R5               MAKE IT MAX
*-------------------DISPLAY THE FILE DATA---------------------*
MC5A0040 EQU   *
         WTOPC TEXT='............... .... ..... ..',SUB=(CHARA,CC0CARD,X
               CHARA,CC0EXP,CHARA,CC0LIM,CHARA,CC0STA),HEADER=NO,      X
               CHAIN=YES,ENDOFM=NO
         AHI   R4,#CC0LEN          BUMP TO NEXT CARD MEMBER DATA
         BCT   R1,MC5A0040         ITERATE 20 TIMES
         L     R4,CE1CR1           REBASE CC0CC
         CLC   CC0FCH,=XL4'0'      CHECK FWD CHAI
         BER   R2                  IF NO ADDRESS STOP LOOP
         RELCC D1                  ELSE RELEASE D1
         MVC   CE1FA1+4(4),CC0FCH  AND ITIALIZE FOR POOL
         MVC   CE1FA1(2),=C'A1'    FILE IN FARW
         LHI   R5,7                MAKE R5= MAX NO OF LREC IN POOL
         B     MC5A0030            REPEAT THE LOOP
*-------------------DISPLAY FOR INVALID-----------------------*
MC5A5000 EQU   *
         LA    R5,EBW000           TAKE ADDRES OF WORK AREA
         MVI   0(R5),L'INPT_IV     PUT LENGHT IN FIRST BYTE
         MVC   1(L'INPT_IV,R5),INPT_IV THEN MOV CHARACTERS IN NEXT
         B     MC5A9000            THEN GO TO DISPLAY THE ERROR
MC5A5010 EQU   *
         LA    R5,EBW000           TAKE ADDRES OF WORK AREA
         MVI   0(R5),L'FACE_ERR    PUT LENGHT IN FIRST BYTE
         MVC   1(L'FACE_ERR,R5),FACE_ERR THEN MOV CHARACTERS IN NEXT
         B     MC5A9000            THEN GO TO DISPLAY THE ERROR
MC5A5020 EQU   *
         LA    R5,EBW000           TAKE ADDRES OF WORK AREA
         MVI   0(R5),L'NOT_FND     PUT LENGHT IN FIRST BYTE
         MVC   1(L'NOT_FND,R5),NOT_FND THEN MOV CHARACTERS IN NEXT
         B     MC5A9000            THEN GO TO DISPLAY THE ERROR
MC5A5030 EQU   *
         LA    R5,EBW000           TAKE ADDRES OF WORK AREA
         MVI   0(R5),L'NAB_ZER     PUT LENGHT IN FIRST BYTE
         MVC   1(L'NAB_ZER,R5),NAB_ZER THEN MOV CHARACTERS IN NEXT
         B     MC5A9000            THEN GO TO DISPLAY THE ERROR
********************DISPLAY ERROR******************************
MC5A9000 EQU   *
         WTOPC TEXTA=0(R5),HEADER=NO DISPLAY THE ERROR
         B     MC5AEXIT            EXIT
*---------------------DC FIELDS-------------------------------*
NAB_ZER  DC    C'FLE IS EMPTY'
INPT_IV  DC    C'PLEASE GIVE A OR D TO SPECIFY THE ACTION'
FACE_ERR DC    C'ERROR OCCURED IN FACE'
NOT_FND  DC    C'COULDNT FIND THE FILE'
DPLUS    DC    C'D+D+'
ZEROZ    DC    C'00000'
ASPACE   DC    C'A '
AONE     DC    C'A1'
LRECFUL  DC    X'040E'
FULLNAB  DC    X'FFFF'
         EXITC
         FINIS MC5A
         END
