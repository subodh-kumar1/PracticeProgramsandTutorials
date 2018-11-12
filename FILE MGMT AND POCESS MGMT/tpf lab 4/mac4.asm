         BEGIN NAME=MAC4,VERSION=00,AMODE=31
*-------------------------------------------------------------*
******   TO FETCH THE FILE AND APPEND/DISPLAY THE CONTENTS ****
*-------------------------------------------------------------*
* * WE WILL FETCH THE FILE IN HOLD OR NOT IN HOLD             *
*   WHEN ITS JUST TO DISPLAY, NOT TO UPDATE, SO USED FINWC    *
*   OTHERWISE USED FIWHC                                      *
* * FILE HAS TAKEN INTO THE DATA LEVEL 1                      *
* * EXTERNAL DSECT, CC0CC USED TO MAP WITH FILE CONTENTS      *
*   AND MI0MI TO MAP THE INPUT CONTENTS                       *
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
*---------------FOR DISPLAY FUNCTION--------------------------*
MAC40000 EQU   *
         BAS   R2,MAC40020         GO TO VALIDATION OF ENTRY
         BAS   R2,MAC40040         FIND THE FILE ADDRESS
         WTOPC TEXT='CARD NO         EXP  LIMIT STATUS     ',HEADER=NO,X
               CHAIN=YES,ENDOFM=NO
         LHI   R5,20
         BAS   R2,MAC40050         FETCH THE FILE
         WTOPC TEXT=' ',CHAIN=YES,ENDOFM=YES,HEADER=NO
         B     MAC4EXIT            EXIT FROM THE PROGRAM
*------------------FOR ADD FUNCTION---------------------------*
MAC40010 EQU   *
         BAS   R2,MAC40030         GO TO VALIDATION OF ENTRY
         BAS   R2,MAC40040         FIND THE FILE ADDRESS
         BAS   R2,MAC40070         FETCHING THE FILE ON HOLD
         BAS   R2,MAC40110         RELEASING THE FILE
         B     MAC4EXIT            EXIT FROM THE PROGRAM
*-------------------EXIT--------------------------------------*
MAC4EXIT EQU   *
         CRUSA S0=1,S1=2           MAKING SURE TO RELEASE BLOCK
         EXITC                     EXIT
*--------------VALIDATION FOR DISPLAY-------------------------*
MAC40020 EQU   *
         MI0MI REG=R1              R1 AS BASE OF MI0MI
         LA    R1,CR0DTN           POINT R1 TO START OF THE INPUT
         CLC   DPLUS,0(R1)         CHECK IF D ONLY
         BNE   MAC40010            IF NOT THEN ERROR
         CLC   1(2,R1),3(R1)       IF THERE IS ANOTHER CHAR AFTER D
         BE    MAC45090            THEN INVALID
         CLC   2(2,R1),4(R1)       IF ITS WELL GO AHEAD
         BE    MAC45090            ELSE ERROR
         CLC   3(2,R1),5(R1)       IF ITS WELL GO AHEAD
         BE    MAC45090            ELSE ERROR
         BR    R2                  BACK TO MAIN
*---------------------VALIDATION FOR ADD----------------------*
MAC40030 EQU   *
         USING MYAREA,R1           MAP INPUT WITH DSECT
         ST    R2,EBX000           STORE MAIN ROUTINE ADDRESS
         CLC   AA,ASPACE           CHECK FOR A
         BNE   MAC45090            IF NOT INVALID INPUT
         LA    R5,EBW000           TAKE ADDRES OF WORK AREA
         MVI   0(R5),L'CARD_IV     PUT LENGHT IN FIRST BYTE
         MVC   1(L'CARD_IV,R5),CARD_IV THEN MOV CHARACTERS IN NEXT
         LHI   R0,L'CARD           CHECK IF CARD
         LA    R3,CARD             IS NUMERIC
         BAS   R2,MAC45000         VALIDATION OF CARD NUM
         CLI   CARD,C'0'           CHECK IF FIRST DIGT OF CARD ZERO
         BE    MAC49000            IF YES SHOW THE ERROR
         CLI   SLASH1,C'/'         CHECK THE SLASHES
         BNE   MAC45160            AT PROPER PLACE
         LA    R5,EBW000           TAKE ADDRES OF WORK AREA
         MVI   0(R5),L'EXP_IV      PUT LENGHT IN FIRST BYTE
         MVC   1(L'EXP_IV,R5),EXP_IV THEN MOV CHARACTERS IN NEXT
         LHI   R0,L'EXP            CHECK IF EXP DATE
         LA    R3,EXP              ALSO NUMERIC
         BAS   R2,MAC45000
         CLI   SLASH2,C'/'         IF NOT THEN
         BNE   MAC45170            INVALID DELIMITER
         CLI   LIM,C'/'            IF THERE IS NO LIMIT MENTIONED
         BE    MAC45150            THEN SHOW THE ERROR
         LHI   R0,0                INITIALISE THE COUNTER
         LA    R3,LIM              LOAD ADDRESS OF LIM
         BAS   R2,MAC45010         VALIDATE THE LIMIT
         MVC   LIM,LIMZ            MAKE LIM 5 DIGIT NUM
         CLC   LIM,ZEROZ           CHECK IF LIMIT IS ZERO
         BE    MAC45150            IF SO, THEN THROW ERROR
         CLC   MM,ONEZ             CHECK WHETHER MONTH
         BL    MAC45050            IS BETWEEN
         CLC   MM,TWELVEZ          01 TO 12
         BH    MAC45050            AND
         CLC   YY,SIXTEENZ         YEAR AFTER
         BL    MAC45050            16
         L     R2,EBX000           RESTORE MAIN ROUTINE ADDRESS
         BR    R2                  BACK TO MAIN ROUTINE
*--------------------FINDING TH FILE ADDRESS------------------*
MAC40040 EQU   *
         LA    R6,#MISCL           TAKE RECORD TYPE IN R6
         LA    R0,1                ORDINAL NUM IN R0
         LA    R7,CE1FA1           FILE ADDRESS IN R7
         ENTRC FACE                GO TO FACE SEGMENT
         LTR   R0,R0               CHECK R0 IS ZERO
         BZ    MAC45100            IF YES THEN ERROR
         MVC   CE1FA1(2),AONE      ELSE MOVE A1 AS RECORD ID
         BR    R2                  BACK TO THE MAIN FUNCTION
************************ DISPLAY ******************************
*----------------FETCH THE FILE FOR DISPLAY-------------------*
MAC40050 EQU   *
         FINWC D1,MAC45110         FIND AND WAIT FOR FILE
         L     R4,CE1CR1           TAKE CONTENT IN R4
         CC0CC REG=R4              BASE R4 TO CC0CC
         LHI   R1,20               LOAD COUNTER WITH 20
         XR    R1,R1               CLEAR R1
         ICM   R1,B'0011',CC0NAB   LOAD THE NAB VALUE IN REG
         CHI   R1,#CC0HRD          COMPARE IF HIGH THAN HEADER SIZE
         BNH   MAC45130            IF NOT SHOW EMPTY FILE
         LHI   R3,#CC0HRD          TAKE HEADER LEN IN REG
         SR    R1,R3               SUBTRACT IT FROM NAB VALUE
         LHI   R3,#CC0LEN          TAKE RECORD LEN IN REG
         XR    R0,R0               CLEAR R0
         DR    R0,R3               (NAB-HDR_LEN)/REC_LEN
         LTR   R1,R1               COMPARE THE NUM_OF_REC WITH ZERO
         BZ    MAC45130            IF THEN SHOW EMPTY RECORD
         CR    R1,R5               CHECK IF NO OF LREC MORE
         BNH   MAC40060            IF NOT THEN GO FOR DISPLAY
         LR    R1,R5               OTHERWISE MAKE IT TO MAX LRECS
*-------------------DISPLAY THE FILE DATA---------------------*
MAC40060 EQU   *
         WTOPC TEXT='............... .... ..... ..',SUB=(CHARA,CC0CARD,X
               CHARA,CC0EXP,CHARA,CC0LIM,CHARA,CC0STA),HEADER=NO,      X
               CHAIN=YES,ENDOFM=NO
         AHI   R4,#CC0LEN          BUMP TO NEXT CARD MEMBER DATA
         BCT   R1,MAC40060         ITERATE 20 TIMES
         L     R4,CE1CR1           BASE CC0CC AGN TO STARTING OF CORE
         CLC   CC0FCH,=XL4'0'      CHECK FOR FWD CHAIN
         BER   R2                  IF NOT THEN STOP THE LOOP
         RELCC D1                  RELEASE THE CORE
         MVC   CE1FA1+4(4),CC0FCH  POPULATE THE POOL ADDRESS
         MVC   CE1FA1(2),AONE      AND THE RID
         LHI   R5,7                MAKE R5 TO MAX OF NO OF LREC
         B     MAC40050            REPEAT THE LOOP
******************* FETCHING FILE ON HOLD *********************
MAC40070 EQU   *
         FIWHC D1,MAC45110         FIND AND WAIT FOR FILE
         L     R4,CE1CR1           TAKE CONTENT IN R4
         CC0CC REG=R4              BASE R4 TO CC0CC
         XR    R0,R0               CLEAR R0
         ICM   R0,B'0011',CC0NAB   TAKE NAB VALUE IN REG
         CHI   R0,#CC0HRD          COMPARE WITH HEADER LENGTH
         BH    MAC40080            IF GREATER THEN GO AHEAD
         LHI   R0,#CC0HRD          OTHERWISE,MAKE NAB VALUE
         STH   R0,CC0NAB           EQUAL TO HEADER LEN+L'NAB
MAC40080 EQU   *
         LR    R7,R0               LOAD R7 WITH NAB VALUE
         LHI   R3,#CC0HRD          TAKE HEADER LEN IN REG
         SR    R7,R3               SUBTRACT IT FROM NAB VALUE
         LHI   R3,#CC0LEN          TAKE RECORD LEN IN REG
         XR    R6,R6               CLEAR R6
         DR    R6,R3               (NAB-HDR_LEN)/REC_LEN
         LA    R6,CC0NAB           TAKE NAB IN REHG
         LTR   R7,R7               COMPARE THE NUM_OF_REC WITH ZERO
         BZ    MAC40100            IF THEN SHOW EMPTY RECORD
         CHI   R7,20               CHECK IF NO OF LREC GT 20
         BNH   MAC40090            IF NOT THEN CHECK FOR DUPLICATION
         LHI   R7,20               OTHERWISE MAKE IT MAX

*---------------- ADDING THE DATA TO THE FILE-----------------*
MAC40090 EQU   *
         CLC   CC0CARD,CARD        CHECK CARD NUM IS UNIQUE
         BE    MAC45070            IF NOT SHOW ERROR
         AHI   R4,#CC0LEN          OTHERWISE CHECK NEXT RECORD
         BCT   R7,MAC40090         AND ITERATE
         L     R4,CE1CR1           REBSAE CC0CC TO STARTING OF D1
         XR    R5,R5               CLEAR R5
         ICM   R5,B'0011',CC0NAB   LOAD R5, WITH NAB VALUE
         AHI   R5,-18              SUBTRACT HEADER
         CLC   CC0NAB,FULLNAB      COMPARE THE NAB, IF IT IS FULL
         BE    MAC40120            REQUEST FOR POOL FILE
         AR    R4,R5               GO TO CURRENT NAB
MAC40100 EQU   *

         MVC   CC0CARD,CARD        ADD THE RESPECTIVE VALUES
         MVC   CC0EXP,EXP          TO THEIR
         MVC   CC0LIM,LIM          RESPECTIVE
         MVC   CC0STA,STA          PLACE IN THE FILE
         L     R4,CE1CR1           REBASE CC0CC TO D1
         LH    R14,CC0NAB           UPDATEING
         AHI   R14,#CC0LEN         THE NAB VALUE
         STH   R14,CC0NAB           ACCORDINGLY
         CLC   CC0NAB,LRECFUL      IF THE NAB VSALUE REACHES
         BLR   R2                  20, MEANS 20 RECORDS FULL
         MVC   CC0NAB,FULLNAB      THEN MAKE IT 'FFFF'
         BR    R2                  BACK TO MAIN
MAC40110 EQU   *
         FILUC D1                  FILE AND UNHOLD
         WTOPC TEXT='RECORD ADDED SUCCESSFULLY :)',HEADER=NO
         BR    R2
*-------------------MAKE POOL FILE FIRST----------------------*
MAC40120 EQU   *
         CLC   CC0FCH,ZEROX        CHECK IF FILE ADDRESS IN FWD CHN
         BNE   MAC40130            IF THERE GO DIRECTLY TO FIND

         GETFC D2,O,ID=C'OM'       GET THE POOL FILE ADDRES
         MVC   EBCID2,ZEROX        MAKE ID ZERO
         FIWHC D2,MAC45110         FIND THE FILE IN D2
         MVC   CE1FA2(2),AONE      ELSE MOVE A1 AS RECORD ID
         L     R3,CE1CR2           MAP R3 WITH D1
         MVC   0(2,R3),AONE        POPULATE THE RID IN 1ST 2 BYTE
         MVI   2(R3),X'0'          CLEAR THE NEW
         MVC   3(256,R3),2(R3)     NEW OVERFLOW
         MVC   259(122,R3),258(R3) FILE
         MVC   CC0FCH,CE1FA2+4     MOVE THE FILE ADDRES TO FWD CHN
         FILUC D2                  SAVE THE NEW FILE
MAC40130 EQU   *
         MVC   CE1FA2(2),AONE      ELSE MOVE A1 AS RECORD ID
         MVC   CE1FA2+4(4),CC0FCH  TAKE FWD CHAIN ADDRESS TO FARW
         FILUC D1                  RELEASE THE PREV FILE

         FIWHC D2,MAC45110         FIND THE FILE IN D2
         FLIPC D1,D2               SWAP THE 2 LEVEL
         L     R4,CE1CR1           LOAD ADDRESS OF CORE BLOCK IN R4
         MVC   CC0BCH,CE1FA2+4     MOVE THE FILE ADDRES TO FWD CHN
         XR    R0,R0               CLEAR R0
         ICM   R0,B'0011',CC0NAB   TAKE NAB VALUE IN REG
         CHI   R0,#CC0HRD          COMPARE WITH HEADER LENGTH
         BH    MAC40140                IF GREATER THEN GO AHEAD
         LHI   R7,#CC0HRD          OTHERWISE,MAKE NAB VALUE
         STH   R7,CC0NAB           EQUAL TO HEADER LEN+L'NAB
MAC40140 EQU   *
         XR    R7,R7               CLEAR THE R7
         ICM   R7,B'0011',CC0NAB   INSERT NAB VALUE IN R7
         LHI   R3,#CC0HRD          TAKE HEADER LEN IN REG
         SR    R7,R3               SUBTRACT IT FROM NAB VALUE
         LHI   R3,#CC0LEN          TAKE RECORD LEN IN REG
         XR    R6,R6               CLEAR R6
         DR    R6,R3               (NAB-HDR_LEN)/REC_LEN
         LA    R6,CC0NAB           LOAD R6 WITH NAB VALUE
         LTR   R7,R7               COMPARE THE NUM_OF_REC WITH ZERO
         BZ    MAC40160            IF THEN SHOW EMPTY RECORD
         CHI   R7,7                CHECK IF NAB IS MORE THAN 7
         BNH   MAC40150            IF NOT THEN CHECK FOR DUPLICATION
         LHI   R7,7                OTHER WISE MAKE IT SEVEN
MAC40150 EQU   *
         CLC   CC0CARD,CARD        CHECK CARD NUM IS UNIQUE
         BE    MAC45070            IF NOT SHOW ERROR
         AHI   R4,#CC0LEN          OTHERWISE CHECK NEXT RECORD
         BCT   R7,MAC40150         AND ITERATE
         L     R4,CE1CR1           REBASE CC0CC TO START OF D1
         XR    R5,R5               CLEAR R5
         ICM   R5,B'0011',CC0NAB   INSERT NAB VALUE IN REG
         AHI   R5,-18              SUBTRACT HEADER
         CLC   CC0NAB,FULLNAB      COMPARE THE NAB, IF IT IS FULL
         BE    MAC40120            REQUEST FOR NEW OVERFLOW
         AR    R4,R5               GO TO CURRENT PLACE FOR APPEND
MAC40160 EQU   *
         MVC   CC0CARD,CARD        ADD THE RESPECTIVE VALUES
         MVC   CC0EXP,EXP          TO THEIR
         MVC   CC0LIM,LIM          RESPECTIVE
         MVC   CC0STA,STA          PLACE IN THE FILE
         L     R4,CE1CR1           REBASE CC0CC TO D1
         LH    R14,CC0NAB          UPDATEING
         AHI   R14,#CC0LEN         THE NAB VALUE
         STH   R14,CC0NAB          ACCORDINGLY
         CLC   CC0NAB,POOLFUL      IF THE NAB VSALUE REACHES
         BL    MAC40170            20, MEANS 20 RECORDS FULL
         MVC   CC0NAB,FULLNAB      THEN MAKE IT 'FFFF'
MAC40170 EQU   *
         FILUC D1                  SAVE AND UNHOLD THE FILE
         WTOPC TEXT='RECORD ADDED SUCCESSFULLY :)',HEADER=NO
         B     MAC4EXIT
*-------------------CHECKING IF NUMBER OR NOT-----------------*
MAC45000 EQU   *

         CLI   0(R3),C'0'          COMPARE IF IT IS NUMBERS
         BL    MAC49000            IF NOT MATCHED THEN
         CLI   0(R3),C'9'          SHOW A MESSAGE
         BH    MAC49000            'INVALID'
*                                  IF IT MATCHES
         AHI   R3,1                GO TO THE NEXT CHARACTER
         BCT   R0,MAC45000         ITERATE
         BR    R2

*----------------VALIDATION FOR LIMIT-------------------------*
MAC45010 EQU   *
         CLI   0(R3),C'0'          COMPARE WITH NUMBERS
         BL    MAC45140            IF NOT NUMBER
         CLI   0(R3),C'9'          THEN SIZE IS INVALID
         BH    MAC45140            ELSE
         AHI   R0,1                INCREMENT THE COUNTER
         AHI   R3,1                BUMP TO NEXT INPUT CHARACTER
         CHI   R0,5                COMPARE SIZE NOT MORE THAN 99999
         BH    MAC45120            OTHERWISE INVALID INPUT
         CLI   0(R3),C'/'          CHECK SLASH
         BNE   MAC45010            IF NOT REPEAT THE LOOP
MAC45020 EQU   *
         CLC   APLUS,1(R3)         CHECK FOR ACTIVE
         BE    MAC45030            OR
         CLC   DPLUS,1(R3)         DEACTIVE
         BE    MAC45030            IF NOT
         CLC   LPLUS,1(R3)         LOST
         BE    MAC45030            ELSE
         CLC   SPLUS,1(R3)         STOLEN
         BE    MAC45030            OTHERWISE
         CLC   EPLUS,1(R3)         EXPIRED
         BE    MAC45030            IF NOT
         B     MAC45080            THEN INVALID STATUS
MAC45030 EQU   *
         CLC   2(2,R3),4(R3)       IF THERE ANOTHER CHAR AFTER STATUS
         BE    MAC45080            THEN INVALID
         CLC   3(2,R3),5(R3)       IF ITS WELL GO AHEAD
         BE    MAC45080            ELSE ERROR
         CLC   4(2,R3),6(R3)       IF ITS WELL GO AHEAD
         BE    MAC45080            ELSE ERROR
         MVC   STA,1(R3)           STORE STATUS IN STA VARIBLE
         MVC   LIMZ,ZEROZ          INTIALIZE SIZEZ WITH CHAR ZEROES
         LA    R5,LIMZ+4           POINT R3 TO LAST BYTE OF SIZEZ
         AHI   R3,-1               BUMP BACK THE INPUT POINTER
MAC45040 EQU   *
         MVC   0(1,R5),0(R3)       START MOVING THE NUMERIC CHAR
         AHI   R3,-1               IN SIZE, FROM RIGHT TO LEFT
         AHI   R5,-1               MOVE THE POINTER BACKWARD ALSO
         BCT   R0,MAC45040         ITERATE TILL THE LENTH OF SIZE
         BR    R2
*-------------------DISPLAY FOR INVALID-----------------------*
MAC45050 EQU   *
         LA    R5,EBW000           TAKE ADDRES OF WORK AREA
         MVI   0(R5),L'EXP_IV      PUT LENGHT IN FIRST BYTE
         MVC   1(L'EXP_IV,R5),EXP_IV THEN MOV CHARACTERS IN NEXT
         B     MAC49000            THEN GO TO DISPLAY THE ERROR
MAC45060 EQU   *
         LA    R5,EBW000           TAKE ADDRES OF WORK AREA
         UNFRC D1                  UNHOLD FILE RELEASE
         MVI   0(R5),L'FIL_FUL     PUT LENGHT IN FIRST BYTE
         MVC   1(L'FIL_FUL,R5),FIL_FUL THEN MOV CHARACTERS IN NEXT
         B     MAC49000            THEN GO TO DISPLAY THE ERROR
MAC45070 EQU   *
         LA    R5,EBW000           TAKE ADDRES OF WORK AREA
         UNFRC D1                  UNHOLD FILE RELEASE
         MVI   0(R5),L'CARD_EQL     PUT LENGHT IN FIRST BYTE
         MVC   1(L'CARD_EQL,R5),CARD_EQL THEN MOV CHARACTERS IN NEXT
         B     MAC49000            THEN GO TO DISPLAY THE ERROR
MAC45080 EQU   *
         LA    R5,EBW000           TAKE ADDRES OF WORK AREA
         MVI   0(R5),L'STA_IV      PUT LENGHT IN FIRST BYTE
         MVC   1(L'STA_IV,R5),STA_IV THEN MOV CHARACTERS IN NEXT
         B     MAC49000            THEN GO TO DISPLAY THE ERROR
MAC45090 EQU   *
         LA    R5,EBW000           TAKE ADDRES OF WORK AREA
         MVI   0(R5),L'INPT_IV     PUT LENGHT IN FIRST BYTE
         MVC   1(L'INPT_IV,R5),INPT_IV THEN MOV CHARACTERS IN NEXT
         B     MAC49000            THEN GO TO DISPLAY THE ERROR
MAC45100 EQU   *
         LA    R5,EBW000           TAKE ADDRES OF WORK AREA
         MVI   0(R5),L'FACE_ERR    PUT LENGHT IN FIRST BYTE
         MVC   1(L'FACE_ERR,R5),FACE_ERR THEN MOV CHARACTERS IN NEXT
         B     MAC49000            THEN GO TO DISPLAY THE ERROR
MAC45110 EQU   *
         LA    R5,EBW000           TAKE ADDRES OF WORK AREA
         MVI   0(R5),L'NOT_FND     PUT LENGHT IN FIRST BYTE
         MVC   1(L'NOT_FND,R5),NOT_FND THEN MOV CHARACTERS IN NEXT
         B     MAC49000            THEN GO TO DISPLAY THE ERROR
MAC45120 EQU   *
         LA    R5,EBW000           TAKE ADDRES OF WORK AREA
         MVI   0(R5),L'LIM_IV      PUT LENGHT IN FIRST BYTE
         MVC   1(L'LIM_IV,R5),LIM_IV THEN MOV CHARACTERS IN NEXT
         B     MAC49000            THEN GO TO DISPLAY THE ERROR
MAC45130 EQU   *
         LA    R5,EBW000           TAKE ADDRES OF WORK AREA
         MVI   0(R5),L'NAB_ZER     PUT LENGHT IN FIRST BYTE
         MVC   1(L'NAB_ZER,R5),NAB_ZER THEN MOV CHARACTERS IN NEXT
         B     MAC49000            THEN GO TO DISPLAY THE ERROR
MAC45140 EQU   *
         LA    R5,EBW000           TAKE ADDRES OF WORK AREA
         MVI   0(R5),L'DEL_IV      PUT LENGHT IN FIRST BYTE
         MVC   1(L'DEL_IV,R5),DEL_IV THEN MOV CHARACTERS IN NEXT
         B     MAC49000            THEN GO TO DISPLAY THE ERROR
MAC45150 EQU   *
         LA    R5,EBW000           TAKE ADDRES OF WORK AREA
         MVI   0(R5),L'NO_LIM      PUT LENGHT IN FIRST BYTE
         MVC   1(L'NO_LIM,R5),NO_LIM THEN MOV CHARACTERS IN NEXT
         B     MAC49000            THEN GO TO DISPLAY THE ERROR
MAC45160 EQU   *
         LA    R5,EBW000           TAKE ADDRES OF WORK AREA
         MVI   0(R5),L'CARD_IV     PUT LENGHT IN FIRST BYTE
         MVC   1(L'CARD_IV,R5),CARD_IV THEN MOV CHARACTERS IN NEXT
         CLI   SLASH1,C'0'         CHECK IF INSTEAD OF SLASH NUM
         BL    MAC45140            IF SO THEN CARD NUM EXCEED
         CLI   SLASH1,C'9'         15 DIGITS, AND SHOW THE
         BH    MAC45140            SPECIFIC ERROR
         B     MAC49000
MAC45170 EQU   *
         LA    R5,EBW000           TAKE ADDRES OF WORK AREA
         MVI   0(R5),L'EXP_IV      PUT LENGHT IN FIRST BYTE
         MVC   1(L'EXP_IV,R5),EXP_IV THEN MOV CHARACTERS IN NEXT
         CLI   SLASH2,C'0'         CHECK IF INSTEAD OF SLASH NUM
         BL    MAC45140            IF SO THEN CARD NUM EXCEED
         CLI   SLASH2,C'9'         15  DIGITS, AND SHOW THE
         BH    MAC45140            SPECIFIC ERROR
         B     MAC49000
MAC45180 EQU   *
         LA    R5,EBW000           TAKE ADDRES OF WORK AREA
         RELFC D2
         UNFRC D1                  UNHOLD FILE RELEASE
         MVI   0(R5),L'FIL_FUL     PUT LENGHT IN FIRST BYTE
         MVC   1(L'FIL_FUL,R5),FIL_FUL THEN MOV CHARACTERS IN NEXT
         B     MAC49000            THEN GO TO DISPLAY THE ERROR
********************DISPLAY ERROR******************************
MAC49000 EQU   *
         WTOPC TEXTA=0(R5),HEADER=NO DISPLAY THE ERROR
         B     MAC4EXIT            EXIT
*---------------------DC FIELDS-------------------------------*
NO_LIM   DC    C'PLEASE MENTION THE CARD LIM 1-99999'
DEL_IV   DC    C'INVALID DELIMITER,PUT / BETWEEN EACH ENTRY'
CARD_IV  DC    C'PLZ GIV VALID 15 DGT CARD NO. START WITH NON ZERO'
NAB_ZER  DC    C'FLE IS EMPTY'
LIM_IV   DC    C'CARD LIMIT SHOULD BE 1-99999'
STA_IV   DC    C'PLEASE PUT VALID STATUS A,L,S,D,E ONLY THEN ENTER'
CARD_EQL DC    C'THIS CARD IS ALREADY EXISTING'
FIL_FUL  DC    C'FILE IS FULL U CANT ADD LREC'
EXP_IV   DC    C'PLEASE PUT A VALID EXPIRY DATE MM=01-12, YY>15'
INPT_IV  DC    C'PLEASE GIVE A SPACE PARAMETERS OR D TO SPECIFY THE ACTX
               ION'
FACE_ERR DC    C'ERROR OCCURED IN FACE'
NOT_FND  DC    C'COULDNT FIND THE FILE'
DPLUS    DC    C'D+D+'
EPLUS    DC    C'E+E+'
SPLUS    DC    C'S+S+'
LPLUS    DC    C'L+L+'
APLUS    DC    C'A+A+'
ZEROZ    DC    C'00000'
ASPACE   DC    C'A '
AONE     DC    C'A1'
ONEZ     DC    C'01'
TWELVEZ  DC    C'12'
SIXTEENZ DC    C'16'
LRECFUL  DC    X'040E'
FULLNAB  DC    X'FFFF'
ZEROX    DC    X'0000'
POOLFUL  DC    X'0177'
         EXITC
         FINIS MAC4
         END
