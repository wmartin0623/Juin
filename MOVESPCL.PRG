*Rechazo de cheques
PRIVATE pan_ant,col_ant
pan_ant=SAVESCREEN(0,0,24,79)
col_ant=SETCOLOR()
*clientes
AREA()
OPEN_DBF("MAECLIEN","CLIENTES","C","MAECLIEN","clie","NOMCLIEN","nomb")

*ZONAS
AREA()
OPEN_DBF("MAEZONAS","ZONAS","C","MAEZONAS","vend","NOMZONAS","nomb")

*CUENTA CORRIENTE
AREA()
IF USAR('CCCLIENT','CTACTE')
  IF !FILE('CCCLIENT.NTX')
   INDEX ON clie+tcom+ltra+srie+nume TO CCCLIENT
  ENDIF
  IF !FILE('CCCLIE01.NTX')
   INDEX ON clie+DTOS(fech) TO CCCLIE01
  ENDIF
  IF !FILE('CCCLIE04.NTX')
   INDEX ON tcom+ltra+srie+nume TO CCCLIE04
  ENDIF
  IF !FILE('CCCLIE03.NTX')
   INDEX ON vend+clie+DTOS(fech) TO CCCLIE03
  ENDIF
  SET INDEX TO CCCLIENT,CCCLIE01,CCCLIE04,CCCLIE03
ELSE
 QUIT
ENDIF
PRIVATE vcli,vnom,vsal,vven,nom_ven,vtip,vltra,vsrie,vnum,vfec,vimp,vadju
PRIVATE pb1,pb2,vExto:="N"
AREA()
IF USAR('PORCINBR')
 pb1=inbr1
 pb2=inbr2
ELSE
 QUIT
ENDIF
*Resolucion 12/02 Percepcion Ing.Bruto
PRIVATE vR122,vPR122,vIR122,v122IMin
vcli=SPACE(6)
STORE 0 TO vsal,vimp
vven=SPACE(3)
STORE SPACE(2) TO vtip
STORE SPACE(30) TO vnom,nom_ven
vltra=' '
vsrie=SPACE(4)
vnum=SPACE(8)
vfec=DATE()
vR122="N"
vPR122=0
vIR122=0
v122IMin=0
SET DECIMAL TO 2
DO WHILE .T.
 SETCOLOR(color7)
 @5,0 CLEAR TO 24,79
 DO MOVESCL
 tecla=65
 DIST_COLOR(05,8,'~W+/R~**** CHEQUES RECHAZADOS o COMPROBANTES SISTEMA ANTERIOR ****')
 SET CURSOR OFF
 DO WHILE tecla#13 .AND. tecla#27
  tecla=INKEY(0)
 ENDDO
 IF tecla=27
  EXIT
 ENDIF
 @20,20 SAY SPACE(40)
 SET CURSOR ON
 XPC=8
 XC=1
 SET KEY -4 TO va_tras
 SETCOLOR(color1)
 DO WHILE XC<=XPC
  XD=1
  SET CURSOR ON
  DO CASE
   CASE XC=1
    @9,19 GET vcli PICTURE '@k 999999' VALID VAL_CLI(@vcli)
    READ
    @9,27 SAY vnom
   CASE XC=2
    @13,20 GET vven PICTURE "@K 999" &&VALID VAL_VEN(@vven)
    READ
    SELECT ZONAS
    @13,27 SAY nom_ven
   CASE XC=3
    @17,18 GET vtip PICTURE '!!' VALID TIPOS(@vtip)
    READ
    @15,45 SAY IIF(vtip='FA','FACTURA ',IIF(vtip='ND','N.DEBITO','CHEQUE  '))
    @17,18 SAY vtip
    IF vtip='CH'
     XC=5
    ENDIF
   CASE XC=4
    *Letra factura "A" o "B"
    @18,22 GET vltra PICTURE "!"
    READ
   CASE XC=5
    *Serie Factura
    @18,24 GET vsrie PICTURE "@K 9999"
    READ
    vsrie=CERO(VAL(vsrie),4)
   CASE XC=6
    @17,22 GET vnum PICTURE "@K 99999999" VALID EXICOD("CTACTE",3,vtip+vltra+vsrie+CERO(VAL(vnum),8),"COMPROBANTE YA EXISTE",18,.T.)
    READ
    IF vtip#'CH'
     vnum=CERO(VAL(vnum),8)
    ENDIF
    @17,22 SAY vnum
   CASE XC=7
    @17,32 GET vfec VALID !EMPTY(vfec)
    READ
   CASE XC=8
    @17,45 GET vimp PICTURE "@K 999999999.99" VALID vimp <> 0
    READ
  ENDCASE
  IF LASTKEY()=27
   CLEAR GETS
   XC=9
  ELSE
   XC=XC+XD
  ENDIF
 ENDDO
 IF LASTKEY()#27
  IF CARTEL("DATOS CORRECTOS  [ SI ]  [ NO ]",20,1,'N',color3)
   IF LASTKEY()#27
    *Gravar datos
    SELECT CTACTE
    ADDREC(0)
    RECLOCK(0)
    REPLACE tcom WITH vtip
    REPLACE tipo WITH IIF(vtip='FA','1',IIF(vtip='ND','3','0'))
    REPLACE clie WITH vcli
    REPLACE ltra WITH vltra
    REPLACE srie WITH vsrie
    REPLACE nume WITH vnum
    REPLACE nint WITH SPACE(8)
    REPLACE fech WITH vfec
    REPLACE paga WITH vimp
    REPLACE fpag WITH CTOD("  /  /    ")
    REPLACE lcom WITH ' '
    REPLACE scom WITH SPACE(4)
    REPLACE ncom WITH SPACE(8)
    REPLACE comt WITH SPACE(2)
    REPLACE vend WITH vven
    REPLACE pib1 WITH 0
    REPLACE pib2 WITH 0
    REPLACE cont WITH .F.
    REPLACE baja WITH .F.
    REPLACE sact WITH vimp
    REPLACE canc WITH .F.
    UNLOCK
  *Actualizo archivo clientes
    SELECT CLIENTES
    SEEK vcli
    vsal=sact + vimp
    salsa=0
    IF FOUND()
     RECLOCK(1)
     *Si es una factura o N.D. Actualizo campo ACRE de MAECLIEN
     *cuando vadju<>0
     IF vadju<>0 .AND. vtip#'CH'
      salsa=vadju - vimp
      IF salsa<0
       salsa=0
       vsal=vsal - vadju
      ELSE
       vsal=vsal - vimp
      ENDIF
     ENDIF
     REPLACE acre WITH salsa
     REPLACE sact WITH vsal
     UNLOCK
    ENDIF
   ENDIF
   COMMIT
  ENDIF
 ENDIF
ENDDO
CLOSE ALL
RESTSCREEN(0,0,24,79,pan_ant)
SETCOLOR(col_ant)
RETURN

FUNCTION TIPOS
PARAMETERS tip
IF LASTKEY()=27
 KEYBOARD CHR(27)
 RETURN .T.
ENDIF
PRIVATE papan,colo_ant,ti
colo_ant = SETCOLOR()
SETCOLOR(color3)
papan=SAVESCREEN(16,50,21,67)
IF tip<>'FA' .AND. tip<>'ND' .AND. tip<>'CH'
 SOMBRA(16,50,20,65,'H1',1,'',color3)
 ti=1
 @17,52 PROMPT "FA- FACTURA "
 @18,52 PROMPT "ND- N.DEBITO"
 @19,52 PROMPT "CH- CHEQUE  "
 MENU TO ti
 SETCOLOR(colo_ant)
 IF LASTKEY()=27
  CLEAR GETS
  KEYBOARD CHR(27)
  RESTSCREEN(16,50,21,67,papan)
  RETURN .T.
 ENDIF
 vtip=IIF(ti=1,'FA',IIF(ti=2,'ND',IIF(ti=3,'CH','  ')))
ELSE
 vtip=tip
ENDIF
SETCOLOR(colo_ant)
RESTSCREEN(16,50,21,67,papan)
RETURN .T.
