         BEGIN NAME=MC6A,VERSION=00,AMODE=31
*-------------------------------------------------------------*
******   TO FETCH THE FILE AND DEACTIVATE THE CARD         ****
*********   DEACTIVATION WILL BE DONE BY CHILD ECB       ******
*-------------------------------------------------------------*
***************************************************************
*----------------- EXPECTED INPUT ----------------------------*
*  1.  ZSMC2 X 1234567890112345                               *
*----------------- EXPETED OUTPUT ----------------------------*
*  CARD DEACTIVATED                                           *
***************************************************************
*------------------OUTPUT FOR INVALIDITY----------------------*
*  ** IF FACE SEGMENT FAILS TO GIVE FILE ADDESS               *
*     - ERROR OCCURED IN FACE                                 *
*  ** IF FILE IS NOT FOUND                                    *
*     - COULDNT FIND THE FILE                                 *
*  ** IF CARD IS ALREADY DEATIVATED                           *
*     - CARD ALREADY DEACTIVE                                 *
*  ** IF CARD IS NOT THERE                                    *
*     - CARD NOT FOUND                                        *
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
ENTERY   DS    CL2
CARD     DS    0XL15               CARD NUMBER
CARD14   DS    XL14
CARD15A  DS    XL1
PLUSA    DS    XL1
CARD15B  DS    XL1
PLUSB    DS    XL1
*-------------------------------------------------------------*
$IS$     CSECT
*---------------FOR DISPLAY FUNCTION--------------------------*
         BAS   R2,MC6A0000         GO TO VALIDATION OF ENTRY
         BAS   R2,MC6A0010         FIND THE FILE ADDRESS
         BAS   R2,MC6A0020         FETCH THE FILE
         WTOPC TEXT='CARD NOT FOUND'
         B     MC6AEXIT            EXIT FROM THE PROGRAM
MC6AEXIT EQU   *
         CRUSA S0=1,S1=2           MAKING SURE TO RELEASE BLOCK
         EXITC                     EXIT
*--------------VALIDATION FOR DISPLAY-------------------------*
MC6A0000 EQU   *
         MI0MI REG=R1              R1 AS BASE OF MI0MI
         LA    R1,CR0DTN           POINT R1 TO START OF THE INPUT
         USING MYAREA,R1           MAP INPUT WITH DSECT
         CLC   ENTERY,=C'X '
         BNE   MC6A5020
         ST    R2,EBX000           STORE MAIN ROUTINE ADDRESS
         LA    R5,EBW000           TAKE ADDRES OF WORK AREA
         MVI   0(R5),L'CARD_IV     PUT LENGHT IN FIRST BYTE
         MVC   1(L'CARD_IV,R5),CARD_IV THEN MOV CHARACTERS IN NEXT
         LHI   R0,L'CARD           CHECK IF CARD
         LA    R3,CARD             IS NUMERIC
         BAS   R2,MC6A5000         VALIDATION OF CARD NUM
         CLI   CARD,C'0'           CHECK IF FIRST DIGT OF CARD ZERO
         BE    MC6A9000            IF YES SHOW THE ERROR
         CLI   PLUSA,C'+'
         BNE   MC6A9000
         CLI   PLUSB,C'+'
         BNE   MC6A9000
         CLC   CARD15A,CARD15B
         BNE   MC6A9000
         LA    R3,PLUSA
         CLC   0(2,R3),2(R3)       IF THERE IS ANOTHER CHAR AFTER D
         BE    MC6A5020            THEN INVALID
         CLC   1(2,R3),3(R3)       IF ITS WELL GO AHEAD
         BE    MC6A5020            ELSE ERROR
         CLC   2(2,R3),4(R3)       IF ITS WELL GO AHEAD
         BE    MC6A5020            ELSE ERROR
         L     R2,EBX000           STORE MAIN ROUTINE ADDRESS
         BR    R2                  BACK TO MAIN
*--------------------FINDING TH FILE ADDRESS------------------*
MC6A0010 EQU   *
         LA    R6,#MISCL           TAKE RECORD TYPE IN R6
         LA    R0,1                ORDINAL NUM IN R0
         LA    R7,CE1FA1           FILE ADDRESS IN R7
         ENTRC FACE                GO TO FACE SEGMENT
         LTR   R0,R0               CHECK R0 IS ZERO
         BZ    MC6A5030            IF YES THEN ERROR
         MVC   CE1FA1(2),AONE      ELSE MOVE A1 AS RECORD ID
         LHI   R5,20
         BR    R2                  BACK TO THE MAIN FUNCTION
************************ DISPLAY ******************************
*----------------FETCH THE FILE FOR DISPLAY-------------------*
MC6A0020 EQU   *
         FINWC D1,MC6A5040         FIND AND WAIT FOR FILE
         L     R4,CE1CR1           TAKE CONTENT IN R4
         CC0CC REG=R4              BASE R4 TO CC0CC
         LHI   R1,20               LOAD COUNTER WITH 20
         XR    R1,R1               CLEAR R1
         ICM   R1,B'0011',CC0NAB   LOAD THE NAB VALUE IN REG
         CHI   R1,#CC0HRD          COMPARE IF HIGH THAN HEADER SIZE
         BNH   MC6A5050            IF NOT SHOW EMPTY FILE
         LHI   R3,#CC0HRD          TAKE HEADER LEN IN REG
         SR    R1,R3               SUBTRACT IT FROM NAB VALUE
         LHI   R3,#CC0LEN          TAKE RECORD LEN IN REG
         XR    R0,R0               CLEAR R0
         DR    R0,R3               (NAB-HDR_LEN)/REC_LEN

         L     R3,CE1CR0
         MI0MI REG=R3
         LA    R3,CR0DTN
         USING MYAREA,R3
         LTR   R1,R1               COMPARE THE NUM_OF_REC WITH ZERO
         BZ    MC6A5050            IF THEN SHOW EMPTY RECORD
         CHI   R1,20
         BNH   MC6A0030
         LR    R1,R5
*-------------------DISPLAY THE FILE DATA---------------------*
MC6A0030 EQU   *
         CLC   CC0CARD,CARD        CHECK CARD NUM IS UNIQUE
         BE    MC6A5010            IF NOT SHOW ERROR
         AHI   R4,#CC0LEN          OTHERWISE CHECK NEXT RECORD
         BCT   R1,MC6A0030         ITERATE 20 TIMES
         L     R4,CE1CR1
         CLC   CC0FCH,=XL4'0'
         BER   R2
         RELCC D1
         MVC   CE1FA1+4(4),CC0FCH
         MVC   CE1FA1(2),=C'A1'
         LHI   R5,7
         B     MC6A0020
*--------------CHECKING IF NUM OR NOT-------------------------*
MC6A5000 EQU   *

         CLI   0(R3),C'0'          COMPARE IF IT IS NUMBERS
         BL    MC6A9000            IF NOT MATCHED THEN
         CLI   0(R3),C'9'          SHOW A MESSAGE
         BH    MC6A9000            'INVALID'
*                                  IF IT MATCHES
         AHI   R3,1                GO TO THE NEXT CHARACTER
         BCT   R0,MC6A5000         ITERATE
         BR    R2
*-------------------DISPLAY FOR INVALID-----------------------*

MC6A5010 EQU   *
         CLI   CC0STA,C'D'
         BE    MC6A5060
         L     R1,CE1FA1+4
         LA    R2,CC0STA
         L     R3,CE1CR1
         SR    R2,R3
         STM   R1,R2,EBW000
         MVC   EBW008(3),EBROUT
         LHI   R14,11
         LA    R15,EBW000
         CREMC MC6B
         WTOPC TEXT='PARENT EXITING'
         EXITC
MC6A5020 EQU   *
         LA    R5,EBW000           TAKE ADDRES OF WORK AREA
         MVI   0(R5),L'INPT_IV     PUT LENGHT IN FIRST BYTE
         MVC   1(L'INPT_IV,R5),INPT_IV THEN MOV CHARACTERS IN NEXT
         B     MC6A9000            THEN GO TO DISPLAY THE ERROR
MC6A5030 EQU   *
         LA    R5,EBW000           TAKE ADDRES OF WORK AREA
         MVI   0(R5),L'FACE_ERR    PUT LENGHT IN FIRST BYTE
         MVC   1(L'FACE_ERR,R5),FACE_ERR THEN MOV CHARACTERS IN NEXT
         B     MC6A9000            THEN GO TO DISPLAY THE ERROR
MC6A5040 EQU   *
         LA    R5,EBW000           TAKE ADDRES OF WORK AREA
         MVI   0(R5),L'NOT_FND     PUT LENGHT IN FIRST BYTE
         MVC   1(L'NOT_FND,R5),NOT_FND THEN MOV CHARACTERS IN NEXT
         B     MC6A9000            THEN GO TO DISPLAY THE ERROR
MC6A5050 EQU   *
         LA    R5,EBW000           TAKE ADDRES OF WORK AREA
         MVI   0(R5),L'NAB_ZER     PUT LENGHT IN FIRST BYTE
         MVC   1(L'NAB_ZER,R5),NAB_ZER THEN MOV CHARACTERS IN NEXT
         B     MC6A9000            THEN GO TO DISPLAY THE ERROR
MC6A5060 EQU   *
         LA    R5,EBW000           TAKE ADDRES OF WORK AREA
         MVI   0(R5),L'DEAC_CRD     PUT LENGHT IN FIRST BYTE
         MVC   1(L'DEAC_CRD,R5),DEAC_CRD THEN MOV CHARACTERS IN NEXT
         B     MC6A9000            THEN GO TO DISPLAY THE ERROR
********************DISPLAY ERROR******************************
MC6A9000 EQU   *
         WTOPC TEXTA=0(R5),HEADER=NO DISPLAY THE ERROR
         B     MC6AEXIT            EXIT
*---------------------DC FIELDS-------------------------------*
DEAC_CRD DC    C'CARD ALREADY DEACTIVE'
CARD_IV  DC    C'PLZ GIV VALID 15 DGT CARD NO. START WITH NON ZERO'
CARD_FND DC    C'CARD FOUND'
NAB_ZER  DC    C'FLE IS EMPTY'
INPT_IV  DC    C'PLEASE GIVE X SPACE CARD NO AS AN INPUT'
FACE_ERR DC    C'ERROR OCCURED IN FACE'
NOT_FND  DC    C'COULDNT FIND THE FILE'
DPLUS    DC    C'D+D+'
ZEROZ    DC    C'00000'
ASPACE   DC    C'A '
AONE     DC    C'A1'
LRECFUL  DC    X'040E'
FULLNAB  DC    X'FFFF'
         EXITC
         FINIS MC6A
         END
