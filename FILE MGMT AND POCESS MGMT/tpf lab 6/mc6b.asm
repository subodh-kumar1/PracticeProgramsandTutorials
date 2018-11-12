         BEGIN NAME=MC6B,VERSION=00,AMODE=31
*-------------------------------------------------------------*
******   TO FETCH THE FILE AND DEACTIVATE THE CARD         ****
*********   DEACTIVATION WILL BE DONE BY CHILD ECB       ******
*-------------------------------------------------------------*
***************************************************************
*----------------- EXPECTED INPUT ----------------------------*
*  1.  ZSMC2 X 1234567890112345                               *
*----------------- EXPETED OUTPUT ----------------------------*
*  CARD DEACTIVATED                                           *
*-------------------------------------------------------------*
* FILE NOT FOUND, IF ERROR IN FIND WAIT HOLDC                 *
***************************************************************
* REGISTER USED:                                              *
*             R2- SCRATCH REGISTER                            *
*             R3- SCRATCH REGISTER                            *
*-------------------------------------------------------------*
*-------------------------------------------------------------*
CHLDAREA DSECT
FIL_ADDR DS    XL4
STA_ADDR DS    XL4
$IS$     CSECT
         USING CHLDAREA,R1         MAP DSECT WITH R1
         LA    R1,EBW000           BASE IT WITH EBW000
         MVC   EBROUT,EBW008       INTERCHANGE MSG B/W PARENT CHILD
         WTOPC TEXT='CHILD CREATED' DISPLAY MSG, CHILD CREATED
         MVC   CE1FA2+4(4),FIL_ADDR POPULATE FILE ADDRESS IN FARW
         MVC   EBCID2,=C'A1'       POPULATE RID N FARW
         FIWHC D2,LAB_ERR          FIND THE FILE
         L     R3,STA_ADDR         TAK DISP OF STATUS FRM STRT OF LEVL
         L     R2,CE1CR2           TAKE ADDRESS OF START OF LEVEL
         LA    R3,0(R3,R2)         ADD DISP TO IT
         MVI   0(R3),C'D'          MOVE 'D' AS STATUS
         FILUC D2                  SAVE THE FILE
         WTOPC TEXT='CARD DECTIVATED' DISPLAY MSG, CARD DEACTIVATED
         EXITC                     EXIT FROM THE CHILD
MC6B9000 EQU   *
         WTOPC TEXT='COULDNT FIND THE FILE' DISPLAY ERROR
         EXITC                     EXIT
         FINIS MC6B
         END
