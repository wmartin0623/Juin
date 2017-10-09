*Carga de Notas de Creditos a Clientes para un grupo de facturas
PUBLIC iva1,iva2,col_ant,ok
col_ant=SETCOLOR()
PUBLIC iva1,iva2
*Porcentaje de iva
AREA()
IF USAR('PORCEIVA')
 iva1=IVAI
 iva2=IVANI
ELSE
 QUIT
ENDIF

*ARCHIVO AUXILIAR
AREA()
IF USAR('AUXILIAR','AUX')
 GO TOP
 PUBLIC Fac_A,Fac_B,vsrie
 vsrie=srie
 Fac_A=nume
 Fac_B=bnum
ELSE
 QUIT
ENDIF

*clientes
OPEN_DBF("MAECLIEN","CLIENTES","C","MAECLIEN","clie","NOMCLIEN","nomb")

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
 INDEX ON clie+tcom TO CCCLIE05 FOR .NOT.cont.AND.tcom='FA'  &&.AND.paga=sact
 IF !FILE('CCCLIE03.NTX')
  INDEX ON vend+clie+DTOS(fech) TO CCCLIE03
 ENDIF
 SET INDEX TO CCCLIENT,CCCLIE01,CCCLIE04,CCCLIE05,CCCLIE03
ELSE
 QUIT
ENDIF
SET RELATION TO clie INTO CLIENTES

*FACTURAS
AREA()
IF USAR("FACL&periodo","FACTURA")
 IF !FILE('FACL&periodo..NTX')
  INDEX ON clie+tcom+ltra+srie+nume TO FACL&periodo
 ENDIF
 wn=periodo+'1'
 IF !FILE('FAC&wn..NTX')
  INDEX ON DTOS(fech) TO FAC&wn
 ENDIF
 SET INDEX TO FACL&periodo,FAC&wn
ELSE
 QUIT
ENDIF

PRIVATE pan_ant
PRIVATE vcli,vnom,vncfa,vciv,vsal,fedes,fehas,vprime,vultim,vtotal
PRIVATE cbte[1],regh[1],vven,nom_ven,wali,vfec,regcli,conta
PRIVATE vltra,vnume
pan_ant=SAVESCREEN(0,0,24,79)
vcli=SPACE(6)
vnom=SPACE(30)
vven=SPACE(3)
vsal=0
STORE DATE() TO fedes,fehas,vfec
DO WHILE .T.
 vtotal=0
 SETCOLOR(color3)
 SOMBRA(10,15,14,50,'H1',1,'',color3)
 @11,18 SAY "Que Cliente : " GET vcli PICTURE "@K 999999" VALID VAL_CLI2(@vcli,@vnom)
 READ
 IF LASTKEY()=27
  EXIT
 ENDIF
 SELECT CLIENTES
 SEEK vcli
 vciv=civa
 @13,17 SAY vnom
 IF CARTEL("DATOS CORRECTOS  [ SI ]  [ NO ]",18,1,'N',colo3)
  IF LASTKEY()=27
   EXIT
  ENDIF
  RESTSCREEN(0,0,24,79,pan_ant)
 ELSE
  LOOP
 ENDIF
 ok=0
 FECHAS(fedes,fehas)
 IF ok=1
  LOOP
 ENDIF
 SELECT CTACTE
 SET ORDER TO 4
 SET FILTER TO fech>=fe_des.AND.fech<=fe_has.AND.clie=vcli
 GO TOP
 *Cargar comprobantes en CBTE[]
 SELECT CTACTE
 SEEK vcli+'FA'
 IF !FOUND()
  CARTEL(ALLTRIM(vnom)+" NO TIENE NADA para NOTA DE CREDITO  [ OK ]",20,1,'N',color3)
  SET FILTER TO
  LOOP
 ELSE
  p=0
  DO WHILE clie = vcli .AND. tcom='FA'
   IF tcom = 'FA' .AND. !BAJA .AND. .NOT.cont .AND. sact > 0
    IF p>0
     ASIZE(cbte,p+1)
     ASIZE(regh,p+1)
    ENDIF
    p = p+1
    cbte[LEN(cbte)] = "               ³            ³               "
    cbte[LEN(cbte)] = STUFF(cbte[LEN(cbte)],2,13,ltra+srie+nume)
    cbte[LEN(cbte)] = STUFF(cbte[LEN(cbte)],18,10,DTOC(fech))
    cbte[LEN(cbte)] = STUFF(cbte[LEN(cbte)],31,12,TRANSFORM(sact,"999999999.99"))
    regh[LEN(regh)] = RECNO()
   ENDIF
   SKIP
  ENDDO
  IF p = 0
   CARTEL(ALLTRIM(vnom)+" NO TIENE NADA para NOTA DE CREDITO  [ OK ]",20,1,'N',color3)
   SET FILTER TO
   LOOP
  ENDIF
 ENDIF
 *Ordena por numero
 DECLARE cam[1]
 cam[1] = "regh"
 ORDEN(cbte,cam)
 *Obtengo Primer y Ultimo Numero de fa
 vprime=SUBSTR(cbte[1],2,13)
 vultim=SUBSTR(cbte[LEN(cbte)],2,13)

 *Suma total FA
 vtotal=0
 FOR con=1 TO LEN(cbte)
  vtotal=vtotal+VAL(SUBSTR(cbte[con],31,12))
 NEXT
 *Obtengo total de comprobantes
 vcan_fa = LEN(cbte)
 *Extraigo el Importe de iva
 IF vciv='1'
  biva=iva1
 ELSE
  IF vciv='2'
   biva=iva1+iva2
  ELSE
   IF vciv > '2'
    biva=0
   ENDIF
  ENDIF
 ENDIF
 IF biva#0
  vtotal=vtotal/((biva+100)/100)
 ENDIF

 panant=SAVESCREEN(9,25,24,78)
 SETCOLOR(color3)
 SOMBRA(9,25,23,77,'V1',3,'',color3)
 @11,25 SAY 'ÌÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹'
 @12,25 SAY 'º                    ³            ³                 º'
 @13,25 SAY 'º                    ³            ³                 º'
 @14,25 SAY 'º                    ³            ³                 º'
 @15,25 SAY 'º                    ³            ³                 º'
 @16,25 SAY 'º                    ³            ³                 º'
 @17,25 SAY 'º                    ³            ³                 º'
 @18,25 SAY 'º                    ³            ³                 º'
 @19,25 SAY 'ÌÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹'
 @10,26 SAY  "      Numero     Fecha        Importe        "
*            "     12345678 ³ 1234567890 ³ 123456789012
 SETCOLOR("W+/R")
 @9,27 SAY ' FACTURAS  para '+ALLTRIM(vnom)
 @20,42 SAY "Total s/IVA : "+STR(vtotal,12,2)
 @21,26 SAY SPACE(38)
 @21,26 SAY "1er. N§ FA= "+vprime+"  Ult. N§ FA= "+vultim
 SETCOLOR(color3)
 @22,26 SAY CHR(24)+CHR(25)+' PgUp PgDn Mover   '+CHR(17)+'ÄÙ Acepta   [Esc]-Salir'

 *----------------------------------------------------------------
 elemento=SCR(cbte,7,12,31,LEN(cbte),9,25,23,77,1,color1,color3)
 *----------------------------------------------------------------
 tcl=LASTKEY()
 RESTSCREEN(9,25,24,78,panant)
 IF tcl#13
  SET FILTER TO
  LOOP
 ENDIF
 *Pedir Porcentaje para aplicar como Nota de credito
 vdesc=0

 @13,8 SAY "                   ÚRango  FACTURASÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿               "
 @14,8 SAY "                   ³De:                a:               ³               "
 @15,8 SAY "ÚImporteÄÄÄÄÄÂ%ÄÄÄÄÅImporteÄÄÄÄÄÂImportes de IVAÄÄÄÄÄÄÄÄÁÄÂImporte+IVAÄ¿"
 @16,8 SAY "ÃTotalÄÄÄÄÄÄÄÅDesc.ÅAcreditadoÄÄÅRI.ÄÄÄÄÄÄÄÄÄÂRNI.ÄÄÄÄÄÄÄÄ´AcreditadoÄÄ´"
 @17,8 SAY "³            ³     ³            ³            ³            ³            ³"
 @18,8 SAY "ÀÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÙ"
 SETCOLOR("W+/R")
 @14,32 SAY vprime
 @14,50 SAY vultim
 SETCOLOR(color3)
 @17,9 SAY vtotal PICTURE "@Z 999999999.99"
 @17,22 GET vdesc PICTURE "@K 99.99" VALID vdesc > 0
 READ
 IF LASTKEY()=27
  SET FILTER TO
  LOOP
 ENDIF
 SETCOLOR("W+/R")
 vtotal= (vtotal * vdesc) / 100
 @17,28 SAY vtotal PICTURE "@Z 999999999.99"
 STORE 0 TO iva,reciva
 *Control Condicion iva
 IF vciv<= '2'
  iva=ROUND((vtotal*iva1)/100,2)
 ENDIF
 IF vciv='2'
  reciva=ROUND((vtotal*iva2)/100,2)
 ENDIF
 *Sujetos no categorizados
 bRG212 = 0
 IF vciv='7'
  bRG212=(vtotal*iva2)/100
  IF bRG212 < vRG212
   bRG212 = 0
  ENDIF
 ENDIF
 *Total del Credito
 vtgral=ROUND(vtotal+iva+reciva+bRG212,2)
 *Credito para cada factura
 vimp_cu = vtgral / vcan_fa

 @16,46 SAY iva1 PICTURE "@Z 99.99"
 @16,60 SAY iva2 PICTURE "@Z 99.99"
 @17,41 SAY iva PICTURE "999999999.99"
 @17,54 SAY reciva PICTURE "@Z 999999999.99"
 @17,67 SAY vtgral PICTURE "999999999.99"
 @18,63 SAY "c/u"
 @18,67 SAY vimp_cu PICTURE "@Z 999999999.99"
 SETCOLOR(col_ant)

 bien=CARTEL("CONFIRME NOTA DE CREDITO por [ "+STR(vtgral,12,2)+" ]  [ NO ]",20,1,'N',colo3)
 RESTSCREEN(0,0,24,79,pan_ant)
 IF LASTKEY()=27.OR.!bien
  SET FILTER TO
  LOOP
 ENDIF

 *Gravar Nota de Credito
 SELECT FACTURA
 GO BOTTOM
 ADDREC(0)
 RECLOCK(0)
 wali=RECNO()
 IF vciv <= '2'
  Fac_A=CERO(VAL(Fac_A)+1,8)
  REPLACE ltra WITH 'A'
  REPLACE nume WITH Fac_A
  vltra='A'
  vnume=Fac_A
 ELSE
  Fac_B=CERO(VAL(Fac_B)+1,8)
  REPLACE ltra WITH 'B'
  REPLACE nume WITH Fac_B
  vltra='B'
  vnume=Fac_B
 ENDIF
 REPLACE srie WITH vsrie
 REPLACE tcom WITH 'NC'
 REPLACE fcre WITH SPACE(8)
 REPLACE fech WITH DATE()
 REPLACE tipo WITH '2'
 REPLACE lcom WITH LEFT(vprime,1)
 REPLACE scom WITH SUBSTR(vprime,2,4)
 REPLACE ncom WITH "--------"
 REPLACE defa WITH RIGHT(vprime,8)
 REPLACE hafa WITH RIGHT(vultim,8)
 REPLACE vend WITH vven
 REPLACE clie WITH vcli
 REPLACE stot WITH vtotal*(-1)
 REPLACE ngra WITH vtotal*(-1)
 REPLACE prin WITH iva1
 REPLACE prni WITH iva2
 REPLACE pper WITH 0
 REPLACE pmul WITH 0
 REPLACE pib1 WITH 0
 REPLACE pib2 WITH 0
 REPLACE civa WITH vciv
 REPLACE ibr1 WITH 0
 REPLACE ibr2 WITH 0
 REPLACE ides WITH 0
 REPLACE irin WITH iva*(-1)
 REPLACE irni WITH reciva*(-1)
 REPLACE iper WITH bRG212*(-1)
 REPLACE imul WITH 0
 REPLACE paga WITH vtgral*(-1)
 REPLACE cont WITH .F.
 REPLACE nrem WITH SPACE(8)
 REPLACE boni WITH 0
 REPLACE obs1 WITH "N.CREDITO x FA N§"+vprime+" a "+vultim
 REPLACE obs2 WITH ""
 REPLACE canc WITH .F.        &&Factura Contado y SI paga
 REPLACE impr WITH .T.
 REPLACE liva WITH .F.
 REPLACE baja WITH .F.
 REPLACE desc WITH vdesc
 UNLOCK

 *Actualizar FA en CTACTE
 SELECT CTACTE
 CARTEL("ESPERE......",12,1,'E',color3)
 DO WHILE .T.
  resto=0
  FOR con=1 TO LEN(cbte)
   GO regh[con]
   RECLOCK(1)
   IF sact < vimp_cu
    resto = resto + (vimp_cu - sact)
    REPLACE sact WITH 0
   ELSE
    REPLACE sact WITH sact - vimp_cu
   ENDIF
   UNLOCK
  NEXT
  IF resto > 0
   vimp_cu = resto / vcan_fa
   LOOP
  ENDIF
  EXIT
 ENDDO

 *Generar en ctacte el registro de NC
 *----
 *Actualizo Archivo de Ctas.Ctes.
 ok=0
 SELECT CTACTE
 ADDREC(0)
 RECLOCK(0)
 REPLACE tcom WITH 'NC'
 REPLACE tipo WITH '2'
 REPLACE clie WITH vcli
 IF vciv <= '2'
  REPLACE ltra WITH 'A'
  REPLACE nume WITH Fac_A
 ELSE
  REPLACE ltra WITH 'B'
  REPLACE nume WITH Fac_B
 ENDIF
 REPLACE srie WITH vsrie
 REPLACE fcre WITH space(8)
 REPLACE nint WITH SPACE(8)      &&solo para pagos
 REPLACE fech WITH DATE()
 REPLACE paga WITH vtgral*(-1)
 REPLACE sact WITH 0
 REPLACE fpag WITH CTOD("  /  /    ")
 REPLACE lcom WITH LEFT(vprime,1)
 REPLACE scom WITH SUBSTR(vprime,2,4)
 REPLACE ncom WITH "--------"
 REPLACE defa WITH RIGHT(vprime,8)
 REPLACE hafa WITH RIGHT(vultim,8)
 REPLACE comt WITH 'FA'
 REPLACE fcom WITH CTOD("  /  /    ")
 REPLACE vend WITH vven
 REPLACE pib1 WITH 0
 REPLACE pib2 WITH 0
 REPLACE cont WITH .F.
 REPLACE baja WITH .F.
 REPLACE moro WITH 'C'
 REPLACE canc WITH .F.
 COMMIT
 UNLOCK

 *Actualizo archivo maestro clientes
 SELECT CLIENTES
 SEEK vcli
 IF FOUND()
  RECLOCK(1)
  REPLACE sact WITH sact-vtgral
  UNLOCK
 ENDIF

 *Actualizo archivo auxiliar
 *ARCHIVO AUXILIAR
 AREA()
 IF USAR('AUXILIAR','AUX')
  SELECT AUX
  GO TOP
  RECLOCK(1)
  IF vciv <= '2'
   REPLACE nume WITH Fac_A
  ELSE
   REPLACE bnum WITH Fac_B
  ENDIF
  UNLOCK
  COMMIT
  CLOSE
 ENDIF
 vnet=vtotal
 conta=.F.

 *----IMPRIMIR NOTA DE CREDITO------
    IF DUDA('IMPRIME EN PAPEL')
     SET CONSOLE OFF
     IF .NOT.ISPRINTER()
      CARTEL("LA IMPRESORA NO ESTA PREPARADA...",12,1,'E','*'+color3)
     ENDIF
     DO WHILE .NOT.ISPRINTER()
      INKEY(.1)
      TONE(700,1)
     ENDDO
     SET PRINT ON
     IF vciv > '2'
      *Papel para  "B"
      CARTEL("POR FAVOR, INSERTE PAPEL CBTE. {B} [ GRACIAS ]",18,1,'N',color3)
      DO IMPR_B_Gpo
      ok=0
      EXIT
     ELSE
      *Facturacion en "A"
      CARTEL("POR FAVOR, INSERTE PAPEL CBTE. {A} [ GRACIAS ]",18,1,'N',color3)
      DO IMPR_A_Gpo
     ENDIF
     SET PRINT OFF
     SET CONSOLE ON
     SELECT FACTURA
     GO wali
     RECLOCK(1)
     REPLACE impr WITH .T.
     UNLOCK
    ENDIF
 *----************************------


ENDDO
RESTSCREEN(0,0,24,79,pan_ant)
CLOSE ALL
SETCOLOR(col_ant)
RETURN



PROCEDURE impr_A_Gpo
?? CHR(27)+CHR(120)+CHR(0)+CHR(18)
?? CHR(27)+CHR(33)+CHR(5)
?? CHR(27)+CHR(67)+CHR(36)
NORMA()
? CHR(15)+SPACE(51)+CHR(18)+SPACE(34)+'San Rafael, '+DTOC(vfec)
?? CHR(15)
?
w=`  Por Fact.:'+vprime+'/'+vultim
? SPACE(50)+CHR(18)+SPACE(50)+`N.C. '+' Nro.: '
? SPACE(79)+vltra+vsrie+vnume
?
SELECT CLIENTES
GO regcli
vdom=domi
vcui=cuit
wpcia=pcia
wdpto=dpto
AREA()
*PROVINCIAS
AREA()
OPEN_DBF("MAEPROVI","PROVIN","C","MAEPROVI","pcia")
SEEK wpcia
IF FOUND()
 provi=ALLTRIM(nomb)
ELSE
 provi='DESCONOCIDA'
ENDIF
*DEPARTAMENTOS
OPEN_DBF("MAEDEPAR","DEPARTA","C","MAEDEPAR","dpto")
SEEK wdpto
IF FOUND()
 ciu=ALLTRIM(nomb)
ELSE
 ciu='DESCONOCIDA'
ENDIF
CLOSE

? SPACE(8)+vcli+' - '+vnom+SPACE(7)+w
? SPACE(8)+vdom
?? SPACE(5)+ciu+` - '+provi
IF vciv='1'
 es=SPACE(8)
 es1=SPACE(42)
ELSE
 es=SPACE(22)
 es1=SPACE(28)
ENDIF
? es+'X'+es1
?? CHR(15)
?? vcui
?? CHR(18)
?? SPACE(19)+vven
? IIF(conta,SPACE(28),SPACE(51))
?? 'X'
?
?
? SPACE(19)+mens1+space(38)+SUBSTR(vtotal,67,12)
k=1
FOR i=1 TO 12-k
 ?
NEXT i
?
?
? SPACE(5)+STR(vtotal,12,2)+SPACE(14)
?? SPACE(15)+STR(0,12,2)+SPACE(26)+STR(vnet,12,2)+CHR(15)
?
?
? SPACE(4)+STR(iva1,5,2)+` : '+IIF(vciv='3'.OR.vciv='4',STR(0,12,2),STR((vnet*iva1)/100,12,2))+SPACE(29)+STR(iva2,5,2)+;
' : '+IIF(vciv='2',STR((vnet*iva2)/100,12,2),STR(0,12,2))+SPACE(29)+;
' : '+SPACE(35)+CHR(18)+STR(vtgral,12,2)
EJECT
?? CHR(27)+CHR(33)+CHR(8)
RETURN

PROCEDURE impr_B_Gpo
?? CHR(27)+CHR(120)+CHR(0)+CHR(18)
?? CHR(27)+CHR(33)+CHR(5)
?? CHR(27)+CHR(67)+CHR(36)
NORMA()
? CHR(15)+SPACE(51)+CHR(18)+SPACE(34)+'San Rafael, '+DTOC(vfec)
?? CHR(15)
?
w=`  Por Fact.:'+vprime+'/'+vultim
? SPACE(50)+CHR(18)+SPACE(50)+`N.C. '+' Nro.: '
? SPACE(79)+vltra+vsrie+vnume
?
SELECT CLIENTES
GO regcli
vdom=domi
vcui=cuit
wpcia=pcia
wdpto=dpto
AREA()
*PROVINCIAS
AREA()
OPEN_DBF("MAEPROVI","PROVIN","C","MAEPROVI","pcia")
SEEK wpcia
IF FOUND()
 provi=ALLTRIM(nomb)
ELSE
 provi='DESCONOCIDA'
ENDIF
*DEPARTAMENTOS
OPEN_DBF("MAEDEPAR","DEPARTA","C","MAEDEPAR","dpto")
SEEK wdpto
IF FOUND()
 ciu=ALLTRIM(nomb)
ELSE
 ciu='DESCONOCIDA'
ENDIF
CLOSE
? SPACE(8)+vcli+' - '+vnom+SPACE(7)+w
? SPACE(8)+vdom
?? SPACE(5)+ciu+` - '+provi
IF vciv='1'
 contra='Resp.Inscripto   '
ELSE
 IF vciv='2'
  contra='Resp.No Inscripto'
 ELSE
  IF vciv='3'
   contra='Consumidor Final '
  ELSE
   IF vciv='4'
    contra='Exento           '
   ELSE
    IF vciv='5'
     contra='MonoTributo      '
    ELSE
     IF vciv='6'
      contra='No Alcanzado     '
     ELSE
      IF vciv='7'
       contra='No Categorizado  '
      ENDIF
     ENDIF
    ENDIF
   ENDIF
  ENDIF
 ENDIF
ENDIF
? SPACE(8)+contra+SPACE(25)+vcui+SPACE(15)+vven
? IIF(conta,SPACE(28),SPACE(51))
vrem=SPACE(8)
?? 'X'+IIF(conta,SPACE(47),SPACE(24))+vrem
?
?
? SPACE(19)+mens1+SPACE(3)+SUBSTR(0,48,5)+SPACE(27)+SUBSTR(vtotal,67,12)
k=1
FOR i=1 TO 12-k
 ?
NEXT i
?
?
? SPACE(10)+STR(vtotal,12,2)+SPACE(25)+STR(0,12,2)+SPACE(10)
?? IIF(bRG212#0,'Percep.RG.212: '+STR(bRG212,12,2),SPACE(1))
?
?
? SPACE(84)+STR(vtgral,12,2)
EJECT
?? CHR(27)+CHR(33)+CHR(8)
RETURN
