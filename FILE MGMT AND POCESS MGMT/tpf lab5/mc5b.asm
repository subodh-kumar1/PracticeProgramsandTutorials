         BEGIN NAME=MC5B,VERSION=00,AMODE=31
*-------------------------------------------------------------*
******   TO FETCH THE FILE AND APPEND THE CONTENTS  ***********
******          (entered from prev prog)         **************
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
         BAS   R2,MC5B0010         GO TO VALIDATION OF ENTRY
         BAS   R2,MC5B0020         FIND THE FILE ADDRESS
         BAS   R2,MC5B0030         FETCHING THE FILE ON HOLD
         BAS   R2,MC5B0070         RELEASING THE FILE
         B     MC5BEXIT            EXIT FROM THE PROGRAM
*-------------------EXIT--------------------------------------*
MC5BEXIT EQU   *
         CRUSA S0=1,S1=2           MAKING SURE TO RELEASE BLOCK
         EXITC                     EXIT
*---------------------VALIDATION FOR ADD----------------------*
MC5B0010 EQU   *
         USING MYAREA,R1           MAP INPUT WITH DSECT
         ST    R2,EBX000           STORE MAIN ROUTINE ADDRESS
         CLC   AA,ASPACE           CHECK FOR A
         BNE   MC5B5080            IF NOT INVALID INPUT
         LA    R5,EBW000           TAKE ADDRES OF WORK AREA
         MVI   0(R5),L'CARD_IV     PUT LENGHT IN FIRST BYTE
         MVC   1(L'CARD_IV,R5),CARD_IV THEN MOV CHARACTERS IN NEXT
         LHI   R0,L'CARD           CHECK IF CARD
         LA    R3,CARD             IS NUMERIC
         BAS   R2,MC5B5000         VALIDATION OF CARD NUM
         CLI   CARD,C'0'           CHECK IF FIRST DIGT OF CARD ZERO
         BE    MC5B9000            IF YES SHOW THE ERROR
         CLI   SLASH1,C'/'         CHECK THE SLASHES
         BNE   MC5B5140            AT PROPER PLACE
         LA    R5,EBW000           TAKE ADDRES OF WORK AREA
         MVI   0(R5),L'EXP_IV      PUT LENGHT IN FIRST BYTE
         MVC   1(L'EXP_IV,R5),EXP_IV THEN MOV CHARACTERS IN NEXT
         LHI   R0,L'EXP            CHECK IF EXP DATE
         LA    R3,EXP              ALSO NUMERIC
         BAS   R2,MC5B5000
         CLI   SLASH2,C'/'         IF NOT THEN
         BNE   MC5B5150            INVALID DELIMITER
         CLI   LIM,C'/'            IF THERE IS NO LIMIT MENTIONED
         BE    MC5B5130            THEN SHOW THE ERROR
         LHI   R0,0                INITIALISE THE COUNTER
         LA    R3,LIM              LOAD ADDRESS OF LIM
         BAS   R2,MC5B5010         VALIDATE THE LIMIT
         MVC   LIM,LIMZ            MAKE LIM 5 DIGIT NUM
         CLC   LIM,ZEROZ           CHECK IF LIMIT IS ZERO
         BE    MC5B5130            IF SO, THEN THROW ERROR
         CLC   MM,ONEZ             CHECK WHETHER MONTH
         BL    MC5B5050            IS BETWEEN
         CLC   MM,TWELVEZ          01 TO 12
         BH    MC5B5050            AND
         CLC   YY,SIXTEENZ         YEAR AFTER
         BL    MC5B5050            16
         L     R2,EBX000           RESTORE MAIN ROUTINE ADDRESS
         BR    R2                  BACK TO MAIN ROUTINE
*--------------------FINDING TH FILE ADDRESS------------------*
MC5B0020 EQU   *
         LA    R6,#MISCL           TAKE RECORD TYPE IN R6
         LA    R0,1                ORDINAL NUM IN R0
         LA    R7,CE1FA1           FILE ADDRESS IN R7
         ENTRC FACE                GO TO FACE SEGMENT
         LTR   R0,R0               CHECK R0 IS ZERO
         BZ    MC5B5090            IF YES THEN ERROR
         MVC   CE1FA1(2),AONE      ELSE MOVE A1 AS RECORD ID
         BR    R2                  BACK TO THE MAIN FUNCTION

******************* FETCHING FILE ON HOLD *********************
MC5B0030 EQU   *
         FIWHC D1,MC5B5100         FIND AND WAIT FOR FILE
         L     R4,CE1CR1           TAKE CONTENT IN R4
         CC0CC REG=R4              BASE R4 TO CC0CC
         XR    R0,R0               CLEAR R0
         ICM   R0,B'0011',CC0NAB   TAKE NAB VALUE IN REG
         CHI   R0,#CC0HRD          COMPARE WITH HEADER LENGTH
         BH    MC5B0040            IF GREATER THEN GO AHEAD
         LHI   R0,#CC0HRD          OTHERWISE,MAKE NAB VALUE
         STH   R0,CC0NAB           EQUAL TO HEADER LEN+L'NAB
MC5B0040 EQU   *
         LR    R7,R0               load r7 with nab value
         LHI   R3,#CC0HRD          TAKE HEADER LEN IN REG
         SR    R7,R3               SUBTRACT IT FROM NAB VALUE
         LHI   R3,#CC0LEN          TAKE RECORD LEN IN REG
         XR    R6,R6               clear r6
         DR    R6,R3               (NAB-HDR_LEN)/REC_LEN
         LTR   R7,R7               COMPARE THE NUM_OF_REC WITH ZERO
         BZ    MC5B0060            IF THEN SHOW EMPTY RECORD
         CHI   R7,20               check if no of lrec gt max
         BNH   MC5B0050            if not then go for add
         LHI   R7,20               otherwise make it max

*---------------- ADDING THE DATA TO THE FILE-----------------*
MC5B0050 EQU   *
         CLC   CC0CARD,CARD        CHECK CARD NUM IS UNIQUE
         BE    MC5B5060            IF NOT SHOW ERROR
         AHI   R4,#CC0LEN          OTHERWISE CHECK NEXT RECORD
         BCT   R7,MC5B0050         AND ITERATE
         L     R4,CE1CR1           rebase cc0cc with d1
         XR    R5,R5               clear r5
         ICM   R5,B'0011',CC0NAB   insert the nab in reg
         AHI   R5,-18              subtract header
         CLC   CC0NAB,FULLNAB      COMPARE THE NAB, IF IT IS FULL
         BE    MC5B0080            IF YES DISPLAY THE WARNING
         AR    R4,R5               base cc0cc to curr data of lrec
MC5B0060 EQU   *

         MVC   CC0CARD,CARD        ADD THE RESPECTIVE VALUES
         MVC   CC0EXP,EXP          TO THEIR
         MVC   CC0LIM,LIM          RESPECTIVE
         MVC   CC0STA,STA          PLACE IN THE FILE
         L     R4,CE1CR1           rebase cc0cc
         LH    R14,CC0NAB           UPDATEING
         AHI   R14,#CC0LEN         THE NAB VALUE
         STH   R14,CC0NAB           ACCORDINGLY
         CLC   CC0NAB,LRECFUL      IF THE NAB VSALUE REACHES
         BLR   R2                  20, MEANS 20 RECORDS FULL
         MVC   CC0NAB,FULLNAB      THEN MAKE IT 'FFFF'
         BR    R2
MC5B0070 EQU   *
         FILUC D1                  FILE AND UNHOLD
         WTOPC TEXT='RECORD ADDED SUCCESSFULLY :)',HEADER=NO
         BR    R2
*-------------------MAKE POOL FILE FIRST----------------------*
MC5B0080 EQU   *
         CLC   CC0FCH,=XL4'0000'   CHECK IF FILE ADDRESS IN FWD CHN
         BNE   MC5B0090                 IF THERE GO DIRECTLY TO FIND

         GETFC D2,O,ID=C'OM'       GET THE POOL FILE addres
         MVC   EBCID2,=X'0000'     MAKE ID ZERO
         FIWHC D2,MC5B5100         FIND THE FILE IN D2
         MVC   CE1FA2(2),=C'A1'    MOVE A1 AS RECORD ID
         L     R3,CE1CR2           take curr file
         MVC   0(2,R3),=C'A1'      populate rid in first 2 bytes
         MVI   2(R3),X'0'          clear all
         MVC   3(256,R3),2(R3)     junk values
         MVC   259(122,R3),258(R3)
         MVC   CC0FCH,CE1FA2+4     MOVE THE FILE ADDRES TO FWD CHN
         FILUC D2
MC5B0090  EQU *
         MVC   CE1FA2(2),=C'A1'    ELSE MOVE A1 AS RECORD ID
         MVC   CE1FA2+4(4),CC0FCH  TAKE FWD CHAIN ADDRESS TO FARW
         FILUC D1                  RELEASE THE PREV FILE

         FIWHC D2,MC5B5100         FIND THE FILE IN D2
         FLIPC D1,D2               SWAP THE 2 LEVEL
         L     R4,CE1CR1           LOAD ADDRESS OF CORE BLOCK IN R4
         MVC   CC0BCH,CE1FA2+4     MOVE THE FILE ADDRES TO FWD CHN
         XR    R0,R0               clear r0
         ICM   R0,B'0011',CC0NAB   insert NAB VALUE IN REG
         CHI   R0,#CC0HRD          COMPARE WITH HEADER LENGTH
         BH    MC5B0100            IF GREATER THEN GO AHEAD
         LHI   R0,#CC0HRD          OTHERWISE,MAKE NAB VALUE
         STH   R0,CC0NAB           EQUAL TO HEADER LEN+L'NAB
MC5B0100 EQU   *
         XR    R7,R7               clear r7
         ICM   R7,B'0011',CC0NAB   insetrt nab in reg
         LHI   R3,#CC0HRD          TAKE HEADER LEN IN REG
         SR    R7,R3               SUBTRACT IT FROM NAB VALUE
         LHI   R3,#CC0LEN          TAKE RECORD LEN IN REG
         XR    R6,R6               clear r6
         DR    R6,R3               (NAB-HDR_LEN)/REC_LEN
         LTR   R7,R7               COMPARE THE NUM_OF_REC WITH ZERO
         BZ    MC5B0120            IF THEN SHOW EMPTY RECORD
         CHI   R7,7                compare no of rec with max
         BNH   MC5B0110            if more
         LHI   R7,7                then make it max
MC5B0110 EQU   *
         CLC   CC0CARD,CARD        CHECK CARD NUM IS UNIQUE
         BE    MC5B5060            IF NOT SHOW ERROR
         AHI   R4,#CC0LEN          OTHERWISE CHECK NEXT RECORD
         BCT   R7,MC5B0110         AND ITERATE
         L     R4,CE1CR1           rebase cc0cc of d1
         XR    R5,R5               r5 clearing
         ICM   R5,B'0011',CC0NAB   insert nab value in r5
         AHI   R5,-18              subtract header
         CLC   CC0NAB,FULLNAB      COMPARE THE NAB, IF IT IS FULL
         BE    MC5B0080            IF YES DISPLAY THE WARNING
         AR    R4,R5               base the cc0cc to curr data
MC5B0120 EQU   *
         MVC   CC0CARD,CARD        ADD THE RESPECTIVE VALUES
         MVC   CC0EXP,EXP          TO THEIR
         MVC   CC0LIM,LIM          RESPECTIVE
         MVC   CC0STA,STA          PLACE IN THE FILE
         L     R4,CE1CR1           rebase cc0cc to d1
         LH    R14,CC0NAB          UPDATEING
         AHI   R14,#CC0LEN         THE NAB VALUE
         STH   R14,CC0NAB          ACCORDINGLY
         CLC   CC0NAB,=X'0177'     IF THE NAB VSALUE REACHES
         BL    MC5B0130            20, MEANS 20 RECORDS FULL
         MVC   CC0NAB,FULLNAB      THEN MAKE IT 'FFFF'
MC5B0130 EQU   *
         FILUC D1
         WTOPC TEXT='RECORD ADDED SUCCESSFULLY :)',HEADER=NO
         B     MC5BEXIT
*-------------------CHECKING IF NUMBER OR NOT-----------------*
MC5B5000 EQU   *

         CLI   0(R3),C'0'          COMPARE IF IT IS NUMBERS
         BL    MC5B9000            IF NOT MATCHED THEN
         CLI   0(R3),C'9'          SHOW A MESSAGE
         BH    MC5B9000            'INVALID'
*                                  IF IT MATCHES
         AHI   R3,1                GO TO THE NEXT CHARACTER
         BCT   R0,MC5B5000         ITERATE
         BR    R2

*----------------VALIDATION FOR LIMIT-------------------------*
MC5B5010 EQU   *
         CLI   0(R3),C'0'          COMPARE WITH NUMBERS
         BL    MC5B5120            IF NOT NUMBER
         CLI   0(R3),C'9'          THEN SIZE IS INVALID
         BH    MC5B5120            ELSE
         AHI   R0,1                INCREMENT THE COUNTER
         AHI   R3,1                BUMP TO NEXT INPUT CHARACTER
         CHI   R0,5                COMPARE SIZE NOT MORE THAN 99999
         BH    MC5B5110            OTHERWISE INVALID INPUT
         CLI   0(R3),C'/'          CHECK SLASH
         BNE   MC5B5010            IF NOT REPEAT THE LOOP
MC5B5020 EQU   *
         CLC   APLUS,1(R3)         CHECK FOR ACTIVE
         BE    MC5B5030            OR
         CLC   DPLUS,1(R3)         DEACTIVE
         BE    MC5B5030            IF NOT
         CLC   LPLUS,1(R3)         LOST
         BE    MC5B5030            ELSE
         CLC   SPLUS,1(R3)         STOLEN
         BE    MC5B5030            OTHERWISE
         CLC   EPLUS,1(R3)         EXPIRED
         BE    MC5B5030            IF NOT
         B     MC5B5070            THEN INVALID STATUS
MC5B5030 EQU   *
         CLC   2(2,R3),4(R3)       IF THERE ANOTHER CHAR AFTER STATUS
         BE    MC5B5070            THEN INVALID
         CLC   3(2,R3),5(R3)       IF ITS WELL GO AHEAD
         BE    MC5B5070            ELSE ERROR
         CLC   4(2,R3),6(R3)       IF ITS WELL GO AHEAD
         BE    MC5B5070            ELSE ERROR
         MVC   STA,1(R3)           STORE STATUS IN STA VARIBLE
         MVC   LIMZ,ZEROZ          INTIALIZE SIZEZ WITH CHAR ZEROES
         LA    R5,LIMZ+4           POINT R3 TO LAST BYTE OF SIZEZ
         AHI   R3,-1               BUMP BACK THE INPUT POINTER
MC5B5040 EQU   *
         MVC   0(1,R5),0(R3)       START MOVING THE NUMERIC CHAR
         AHI   R3,-1               IN SIZE, FROM RIGHT TO LEFT
         AHI   R5,-1               MOVE THE POINTER BACKWARD ALSO
         BCT   R0,MC5B5040         ITERATE TILL THE LENTH OF SIZE
         BR    R2
*-------------------DISPLAY FOR INVALID-----------------------*
MC5B5050 EQU   *
         LA    R5,EBW000           TAKE ADDRES OF WORK AREA
         MVI   0(R5),L'EXP_IV      PUT LENGHT IN FIRST BYTE
         MVC   1(L'EXP_IV,R5),EXP_IV THEN MOV CHARACTERS IN NEXT
         B     MC5B9000            THEN GO TO DISPLAY THE ERROR
MC5B5060 EQU   *
         LA    R5,EBW000           TAKE ADDRES OF WORK AREA
         UNFRC D1                  UNHOLD FILE RELEASE
         MVI   0(R5),L'CARD_EQL     PUT LENGHT IN FIRST BYTE
         MVC   1(L'CARD_EQL,R5),CARD_EQL THEN MOV CHARACTERS IN NEXT
         B     MC5B9000            THEN GO TO DISPLAY THE ERROR
MC5B5070 EQU   *
         LA    R5,EBW000           TAKE ADDRES OF WORK AREA
         MVI   0(R5),L'STA_IV      PUT LENGHT IN FIRST BYTE
         MVC   1(L'STA_IV,R5),STA_IV THEN MOV CHARACTERS IN NEXT
         B     MC5B9000            THEN GO TO DISPLAY THE ERROR
MC5B5080 EQU   *
         LA    R5,EBW000           TAKE ADDRES OF WORK AREA
         MVI   0(R5),L'INPT_IV     PUT LENGHT IN FIRST BYTE
         MVC   1(L'INPT_IV,R5),INPT_IV THEN MOV CHARACTERS IN NEXT
         B     MC5B9000            THEN GO TO DISPLAY THE ERROR
MC5B5090 EQU   *
         LA    R5,EBW000           TAKE ADDRES OF WORK AREA
         MVI   0(R5),L'FACE_ERR    PUT LENGHT IN FIRST BYTE
         MVC   1(L'FACE_ERR,R5),FACE_ERR THEN MOV CHARACTERS IN NEXT
         B     MC5B9000            THEN GO TO DISPLAY THE ERROR
MC5B5100 EQU   *
         LA    R5,EBW000           TAKE ADDRES OF WORK AREA
         MVI   0(R5),L'NOT_FND     PUT LENGHT IN FIRST BYTE
         MVC   1(L'NOT_FND,R5),NOT_FND THEN MOV CHARACTERS IN NEXT
         B     MC5B9000            THEN GO TO DISPLAY THE ERROR
MC5B5110 EQU   *
         LA    R5,EBW000           TAKE ADDRES OF WORK AREA
         MVI   0(R5),L'LIM_IV      PUT LENGHT IN FIRST BYTE
         MVC   1(L'LIM_IV,R5),LIM_IV THEN MOV CHARACTERS IN NEXT
         B     MC5B9000            THEN GO TO DISPLAY THE ERROR
MC5B5120 EQU   *
         LA    R5,EBW000           TAKE ADDRES OF WORK AREA
         MVI   0(R5),L'DEL_IV      PUT LENGHT IN FIRST BYTE
         MVC   1(L'DEL_IV,R5),DEL_IV THEN MOV CHARACTERS IN NEXT
         B     MC5B9000            THEN GO TO DISPLAY THE ERROR
MC5B5130 EQU   *
         LA    R5,EBW000           TAKE ADDRES OF WORK AREA
         MVI   0(R5),L'NO_LIM      PUT LENGHT IN FIRST BYTE
         MVC   1(L'NO_LIM,R5),NO_LIM THEN MOV CHARACTERS IN NEXT
         B     MC5B9000            THEN GO TO DISPLAY THE ERROR
MC5B5140 EQU   *
         LA    R5,EBW000           TAKE ADDRES OF WORK AREA
         MVI   0(R5),L'CARD_IV     PUT LENGHT IN FIRST BYTE
         MVC   1(L'CARD_IV,R5),CARD_IV THEN MOV CHARACTERS IN NEXT
         CLI   SLASH1,C'0'         CHECK IF INSTEAD OF SLASH NUM
         BL    MC5B5120            IF SO THEN CARD NUM EXCEED
         CLI   SLASH1,C'9'         15 DIGITS, AND SHOW THE
         BH    MC5B5120            SPECIFIC ERROR
         B     MC5B9000
MC5B5150 EQU   *
         LA    R5,EBW000           TAKE ADDRES OF WORK AREA
         MVI   0(R5),L'EXP_IV      PUT LENGHT IN FIRST BYTE
         MVC   1(L'EXP_IV,R5),EXP_IV THEN MOV CHARACTERS IN NEXT
         CLI   SLASH2,C'0'         CHECK IF INSTEAD OF SLASH NUM
         BL    MC5B5120            IF SO THEN CARD NUM EXCEED
         CLI   SLASH2,C'9'         15  DIGITS, AND SHOW THE
         BH    MC5B5120            SPECIFIC ERROR
         B     MC5B9000
********************DISPLAY ERROR******************************
MC5B9000 EQU   *
         WTOPC TEXTA=0(R5),HEADER=NO DISPLAY THE ERROR
         B     MC5BEXIT            EXIT
*---------------------DC FIELDS-------------------------------*
NO_LIM   DC    C'PLEASE MENTION THE CARD LIM 1-99999'
DEL_IV   DC    C'INVALID DELIMITER,PUT / BETWEEN EACH ENTRY'
CARD_IV  DC    C'PLZ GIVE VALID 15 DIGIT CARD NO. STARTING WITH ZERO'
LIM_IV   DC    C'CARD LIMIT SHOULD BE 1-99999'
STA_IV   DC    C'PLEASE PUT VALID STATUS A,L,S,D,E ONLY'
CARD_EQL DC    C'THIS CARD IS ALREADY EXISTING'
EXP_IV   DC    C'PLEASE PUT A VALID EXPIRY DATE MM=01-12, YY>15'
INPT_IV  DC    C'PLEASE GIVE A OR D TO SPECIFY THE ACTION'
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
         EXITC
         FINIS MC5B
         END
