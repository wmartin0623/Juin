*PAGO DE CLIENTES
PUBLIC vdol,iva1,iva2
PRIVATE pan_ant,col_ant
pan_ant = SAVESCREEN(0,0,24,79)
col_ant = SETCOLOR()
*Porcentaje de iva
AREA()
IF USAR('PORCEIVA')
 iva1 = IVAI
 iva2 = IVANI
ELSE
 QUIT
ENDIF
*PAGOS
IF USAR("PCL&periodo","PAGOS")
 IF !FILE('PCL&periodo..NTX')
  INDEX ON nume+nint TO PCL&periodo
 ENDIF
 SET INDEX TO PCL&periodo
ELSE
 QUIT
ENDIF
*clientes
AREA()
OPEN_DBF("MAECLIEN","CLIENTES","C","MAECLIEN","clie","NOMCLIEN","nomb")

*FACTURAS
AREA()
IF USAR("FCC&periodo","FACTURA")
 IF !FILE('FCC&periodo..NTX')
  INDEX ON clie+tcom+ltra+srie+nume TO FCC&periodo
 ENDIF
 wn = periodo+'1'
 IF !FILE('FC&wn..NTX')
  INDEX ON DTOS(fech) TO FC&wn
 ENDIF
 SET INDEX TO FCC&periodo,FC&wn
ELSE
 QUIT
ENDIF

*DETALLES
AREA()
IF USAR("FCD&periodo","DETALLE")
 IF !FILE('FCD&periodo..NTX')
  INDEX ON clie+tcom+ltra+srie+nume TO FCD&periodo
 ENDIF
 SET INDEX TO FCD&Periodo
ELSE
 QUIT
ENDIF

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
 SET INDEX TO CCCLIENT,CCCLIE01,CCCLIE04
ELSE
 QUIT
ENDIF
SET RELATION TO clie INTO CLIENTES

*ARCHIVO AUXILIAR
AREA()
IF USAR('AUXILIAR','AUX')
 GO TOP
 PUBLIC vpago
 vpago=npcl
 vdol = DOLA
ELSE
 QUIT
ENDIF

*Bancos
OPEN_DBF("MAEBANCO","BANCOS","C","MAEBANCO","banc","NOMBANCO","nomb")

*Plazas
AREA()
OPEN_DBF("MAEPLAZA","PLAZA","C","MAEPLAZA","plaz","NOMPLAZA","nomb")

*Cheques
AREA()
IF USAR("MAECHEQU","CHEQUES")
 IF !FILE("MAECHEQ1.NTX")
  INDEX ON seri+nano+nint TO MAECHEQ1
 ENDIF
 IF !FILE("MAECHEQ2.NTX")
  INDEX ON banc+plaz+ntal TO MAECHEQ2
 ENDIF
 IF !FILE("MAECHEQ3.NTX")
  INDEX ON DTOS(fvto) TO MAECHEQ3
 ENDIF
 IF !FILE("MAECHEQ4.NTX")
  INDEX ON ALLTRIM(STR(impo,12,2)) TO MAECHEQ4
 ENDIF
 IF !FILE("MAECHEQ5.NTX")
  INDEX ON prov+bdep+pdep+LEFT(DTOS(fvto),6) TO MAECHEQ5
 ENDIF
 IF !FILE("MAECHEQ6.NTX")
  INDEX ON bdep+pdep+DTOS(fsal) TO MAECHEQ6 FOR !EMPTY(bdep)
 ENDIF
 IF !FILE("MAECHEQ7.NTX")
  INDEX ON clie+DTOS(fvto) TO MAECHEQ7
 ENDIF
 IF !FILE("MAECHEQ8.NTX")
  INDEX ON DTOS(fsal) TO MAECHEQ8
 ENDIF
 SET INDEX TO MAECHEQ1,MAECHEQ2,MAECHEQ3,MAECHEQ4,MAECHEQ5,MAECHEQ6,MAECHEQ7,MAECHEQ8
ELSE
 QUIT
ENDIF

*Numeros de Cheques
PUBLIC vint,vser,vnano
vser=LEFT(periodo,1)
vnano=RIGHT(periodo,2)
AREA()
IF USAR("CHENUMER","NROS")
 IF !FILE("CHENUMER.NTX")
  INDEX ON seri+nano+nint TO CHENUMER
 ENDIF
 SET INDEX TO CHENUMER
 SEEK vser+vnano
 IF FOUND()
  vint=nint
 ELSE
  vint=CERO(0,4)
  ADDREC(1)
  RECLOCK(1)
  REPLACE seri WITH vser
  REPLACE nano WITH vnano
  REPLACE nint WITH vint
  UNLOCK
 ENDIF
ELSE
 QUIT
ENDIF

*Tipo de Comprobantes de pago
PUBLIC tp[8]
CARGA1(tp,"EF~US~CH~RI~RB~RG~AP~AN")
PRIVATE vnum,vcli,vciv,vnom,vsal,bAcre,vAcre,campos[30],wiva,vFech
PRIVATE vDom,vDpto,vPcia,Nom_Dpto,Nom_Pcia,vCPs,vCuit
PUBLIC vtta,vaju,vActa
PUBLIC vtot_pag
PRIVATE Clave_Ok:=.F.
*Variables para cargar Cuentas y proveedor cuando el tipo de pago es Retencion
PRIVATE vNcta,Nom_Ncta,vProv,Nom_Prov

vcli = SPACE(6)
STORE 0 TO vtta,vaju,vpor
vCuit=SPACE(13)
vnum = SPACE(8)
SET DECIMAL TO 2
DO WHILE .T.
 STORE 0 TO vtta,vaju,vAcre
 bAcre=.F.
 AFILL(campos,'    ณ   ณ         ณ   ณ         ณ          ณ            ณ              '+SPACE(20)+SPACE(30)+SPACE(47)+SPACE(24))
 PUBLIC cbte[1],dia[1]
 AFILL(cbte,"")
 AFILL(dia,"")
 vFech=DATE()           &&Se cambia a Pedido de Virginia 27-02-2002 antes era en blanco
 SETCOLOR(color7)
 @1,0 CLEAR TO 24,79
 DO MOVPA1CL
 SETCOLOR(color1)
 SET CURSOR ON
 @2,55 SAY "Pago Ant.: "+ vPago COLOR color9
 @4,7 GET vcli PICTURE "@K 999999" VALID VAL_CLI3(@vcli,4,7,color1,30)
 READ
 IF LASTKEY() = 27
  EXIT
 ENDIF
 @4,59 SAY vsal PICTURE '@Z 999999999.99'
 @6,59 SAY vAcre*(-1) PICTURE '@Z 999999999.99'
 *Confirma si el pago es a Cuenta de un Futura Factura SI NO es cliente Ocasional
 IF VAL(vcli) # 0
  IF vsal = 0
   xp=1
  ELSE
   xp=2
  ENDIF
  vACta = CARTEL("ENTREGA A CUENTA ? [ SI ] [ NO ]",18,xp,'N',color3)
  IF LASTKEY()=27
   EXIT
  ENDIF
  IF !vACta .AND. vsal = 0
   LOOP
  ENDIF
 ELSE
  vACta = .F.
 ENDIF
 *Si NO es a cuenta, Permite la seleccion de Cbtes.
 IF !vACta
  SET CURSOR OFF
  com = 1
  rel = 0
  KEYBOARD CHR(24)+CHR(5)
  DO WHILE .T.
   com = ACHOICE(8,7,17,74,cbte,.T.,"sel_pag",com,rel)
   IF com = 0
    EXIT
   ENDIF
  ENDDO
  inkey(.1)
  IF LASTKEY() = 27 .OR. com=999
   LOOP
  ENDIF
  SET KEY -4 TO
  SET CURSOR ON
 ELSE
  *Si ES a Cuenta
  vtot_pag=0
  @21,58 GET vtot_pag PICTURE "@K 999999999.99" VALID vtot_pag > 0
  READ
 ENDIF
 IF vtot_pag=0
  LOOP
 ENDIF
 @23,0
 IF !Clave_Ok
  IF SELECT("CLA_PAGO")#0
   SELECT CLA_PAGO
   CLOSE
  ENDIF
  IF !CRIPTA01("CLA_PAGO")
   CARTEL('LO LAMENTO, USTED NO CONOCE LA CLAVE [ OK ]',22,1,'N',color3)
   SETCOLOR(col_ant)
   RESTSCREEN(0,0,24,79,pan_ant)
   CLOSE ALL
   RETURN
  ENDIF
  Clave_Ok=.T.
 ENDIF
 SETCOLOR(color3)
 @23,10 SAY " ฑEscฑ-Sale  ฑฑ-Mueve Cursor "
 SETCOLOR(color7)
 DIST_COLOR(23,12,'~W+/R~Esc')
 DIST_COLOR(23,24,'~W+/R~')
 CLEAR GETS
 SETCOLOR(color1)
 *Se anula control de NUmero de Pago a pedido de Virginia 29-07-2003
 @21,15 GET vnum PICTURE "@K 99999999" &&VALID EXICOD("PAGOS",1,CERO(VAL(vnum),8),"COMPROBANTE YA INGRESADO  [ OK ]",19,.T.)
 @22,13 GET vFech PICTURE "@D" VALID !EMPTY(vFech)
 READ
 IF LASTKEY()=27
  LOOP
 ENDIF
 vnum = CERO(VAL(vnum),8)
 SETCOLOR(color7)
 @1,0 CLEAR TO 24,79
 DO MOVPA2CL
 SETCOLOR(color1)
 @3,0 SAY "Sgte.CH: "+vSer+'-'+vNano+'-'+CERO(VAL(vint)+1,4) COLOR color9
 @2,1 SAY vnum
 @2,12 SAY vcli
 @2,19 SAY vnom
 @2,51 SAY vtot_pag  PICTURE "@Z 999999999.99"
 @2,64 SAY vtta PICTURE "@Z 999999999.99"
 @4,59 SAY vdol PICTURE "@Z 99.9999"
 IF vAcre#0 .AND. !vActa
  bAcre=CARTEL("DESEA UTILIZAR EL CREDITO [ SI ] [ NO ]",14,2,'N',color3)
  IF LASTKEY()=27
   LOOP
  ENDIF
 ELSE
  bAcre=.F.
 ENDIF
 IF !bAcre
  com = 1
  rel = 0
  KEYBOARD CHR(24)+CHR(5)
  DO WHILE .T.
   com = ACHOICE(7,5,15,75,campos,.T.,"ing_pag",com,rel)
   IF com = 0
    EXIT
   ENDIF
  ENDDO
  IF LASTKEY() = 27
   LOOP
  ENDIF
 ELSE
  *Utiliza credito pendiente
  IF vAcre>vTot_Pag
   vtta=vTot_Pag
  ELSE
   *vTTa=vTot_pag - vAcre
   vTTa=vAcre
  ENDIF
  campos[1] = STUFF(campos[1],2,2,'AC')
  campos[1] = STUFF(campos[1],59,12,TRANSFORM(vTTa,'@Z 999999999.99'))
  @ 7,5 SAY campos[1]
  @2,64 SAY vtta PICTURE '@Z 999999999.99'
 ENDIF
 IF .NOT.CARTEL("DATOS CORRECTOS   [ SI ]  [ NO ]",19,1,'N',color3)
  IF LASTKEY() = 27
   LOOP
  ENDIF
  LOOP
 ENDIF
 *GRABAR PAGOS
*Actualizo archivo detalles
 *Actualizo archivo AUXILIAR
 AREA()
 IF USAR('AUXILIAR','AUX')
  SELECT AUX
  GO TOP
  RECLOCK(1)
  vPago=CERO(VAL(npcl)+1,8)
  REPLACE npcl WITH vpago
  UNLOCK
  CLOSE
 ENDIF
 TONO(1)
 TONO(2)
 TONO(1)
 TONO(2)
 CARTEL("ANOTE EL NUMERO DEL PAGO POR FAVOR [ "+vPago+" ]",12,1,'N',color3)
 FOR con = 1 TO LEN(campos)
  IF !EMPTY(SUBSTR(campos[con],2,2))
   SELECT PAGOS
   ADDREC(0)
   RECLOCK(0)
   REPLACE tcom WITH 'PA'
   REPLACE nume WITH vnum     &&Numero de pago
   REPLACE nint WITH vpago
   REPLACE clie WITH vcli
   REPLACE fpag WITH SUBSTR(campos[con],2,2)
   REPLACE fech WITH vFech    &&DATE()
   REPLACE ntal WITH SUBSTR(campos[con],35,8)
   REPLACE impo WITH VAL(SUBSTR(campos[con],59,12))
   REPLACE banc WITH SUBSTR(campos[con],7,2)
   REPLACE plaz WITH SUBSTR(campos[con],20,3)
   REPLACE fvto WITH CTOD(SUBSTR(campos[con],46,10))
   REPLACE dola WITH IIF(SUBSTR(campos[con],2,2)='US',vdol,0)
   UNLOCK
  ENDIF
 NEXT con
*Actualizo Archivo de Ctas.Ctes.
 IF !vACta
  sal_ent = vtta
  SELECT CTACTE
  FOR con = 1 TO LEN(cbte)
   IF SUBSTR(cbte[con],2,1) = 'ฑ'
    sal_aux = sal_ent
    sal_ent = sal_ent - VAL(SUBSTR(cbte[con],57,12))
    IF sal_ent >= 0
     vtota = VAL(SUBSTR(cbte[con],57,12))
    ELSE
     vtota = sal_aux
    ENDIF
    ADDREC(0)
    RECLOCK(0)
    REPLACE tcom WITH IIF(bAcre,"AC",'PA')
    REPLACE tipo WITH '0'
    REPLACE clie WITH vcli
    REPLACE ltra WITH ' '
    REPLACE srie WITH SPACE(4)
    REPLACE nume WITH vnum
    REPLACE nint WITH vpago
    REPLACE fech WITH vFech   &&DATE()
    REPLACE paga WITH vtota*(-1)
    REPLACE fpag WITH vFech   &&DATE()
    REPLACE lcom WITH SUBSTR(cbte[con],13,1)  &&Letra comp. pagado
    REPLACE scom WITH SUBSTR(cbte[con],14,4)  &&Serie comp. pagado
    REPLACE ncom WITH SUBSTR(cbte[con],18,8)  &&Nง comp. pagado
    REPLACE comt WITH SUBSTR(cbte[con],7,2)   &&Tipo comp. pagado
    REPLACE fcom WITH CTOD(SUBSTR(cbte[con],29,10))      &&Fecha Comp.Pagado
    REPLACE cont WITH .F.
    REPLACE sact WITH 0
    REPLACE canc WITH .F.
    REPLACE baja WITH .F.
    REPLACE moro WITH 'C'
    REPLACE dias WITH DMA(CTOD(SUBSTR(cbte[con],29,10)),vFech)    &&DATE()) &&Dif.de dias en el pago
    REPLACE myan WITH periodo
    UNLOCK

    *Preparo Condiciones para listar el pago
    SEEK vcli+SUBSTR(cbte[con],7,2)+SUBSTR(cbte[con],13,13)
    IF FOUND()
     RECLOCK(1)
     REPLACE sact WITH sact - vtota
     *Guardo el valor descontado para imprimirlo en la ORDEN DE PAGO
     cbte[con] = STUFF(cbte[con],57,12,TRANSFORM(vtota,"999999999.99"))
     *Cambio el Caracter identificador para poder determinar en el listado
     *el Cbte. a incluir en el listado
     cbte[con] = STUFF(cbte[con],2,1,"@")
     UNLOCK
    ENDIF
   ENDIF
   *Verifico si queda dinero para pagar el sigte.compr.elegido
   IF sal_ent <= 0
    con = LEN(cbte)
   ENDIF
  NEXT con
  IF vtta > vtot_pag
   *Importe Sobrante del Pago
   SELECT CTACTE
   ADDREC(0)
   RECLOCK(0)
   REPLACE tcom WITH 'PA'
   REPLACE tipo WITH '0'
   REPLACE clie WITH vcli
   REPLACE ltra WITH ' '
   REPLACE srie WITH SPACE(4)
   REPLACE nume WITH vnum
   REPLACE nint WITH vpago
   REPLACE fech WITH vFech   &&DATE()
   REPLACE paga WITH (vtta - vtot_pag)*(-1)
   REPLACE fpag WITH vFech   &&DATE()
   REPLACE lcom WITH SPACE(1)                &&Letra comp. pagado
   REPLACE scom WITH SPACE(4)                &&Serie comp. pagado
   REPLACE ncom WITH "********"              &&Nง comp. pagado
   REPLACE comt WITH SPACE(2)                &&Tipo comp. pagado
   REPLACE fcom WITH CTOD("  /  /    ")      &&Fecha Comp.Pagado
   REPLACE cont WITH .F.
   REPLACE sact WITH 0
   REPLACE canc WITH .F.
   REPLACE baja WITH .F.
   REPLACE moro WITH 'C'
   REPLACE dias WITH 0
   REPLACE myan WITH periodo
   UNLOCK
  ENDIF
 ELSE
  *Importe a Cuenta
  SELECT CTACTE
  ADDREC(0)
  RECLOCK(0)
  REPLACE tcom WITH 'PA'
  REPLACE tipo WITH '0'
  REPLACE clie WITH vcli
  REPLACE ltra WITH ' '
  REPLACE srie WITH SPACE(4)
  REPLACE nume WITH vnum
  REPLACE nint WITH vpago
  REPLACE fech WITH vFech   &&DATE()
  REPLACE paga WITH vtta*(-1)
  REPLACE fpag WITH vFech   &&DATE()
  REPLACE lcom WITH SPACE(1)                &&Letra comp. pagado
  REPLACE scom WITH SPACE(4)                &&Serie comp. pagado
  REPLACE ncom WITH "********"              &&Nง comp. pagado
  REPLACE comt WITH SPACE(2)                &&Tipo comp. pagado
  REPLACE fcom WITH CTOD("  /  /    ")      &&Fecha Comp.Pagado
  REPLACE cont WITH .F.
  REPLACE sact WITH 0
  REPLACE canc WITH .F.
  REPLACE baja WITH .F.
  REPLACE moro WITH 'C'
  REPLACE dias WITH 0
  REPLACE myan WITH periodo
  UNLOCK
 ENDIF
 *Actualizo archivo maestro de clientes
 SELECT CLIENTES
 SEEK vcli
 IF FOUND()
  RECLOCK(1)
 *IF bAcre
 * *Si uso credito
 * IF vtta < vTot_Pag
 *  REPLACE acre WITH 0
 *  REPLACE sact WITH sact - vtta
 * ELSE
 *  REPLACE acre WITH acre - vTot_Pag
 *  REPLACE sact WITH sact - vTot_Pag
 * ENDIF
 *ELSE
   *No uso credito
 * IF vtta > vtot_pag
 *  REPLACE acre WITH acre + (vtta-vtot_pag)
 *  REPLACE sact WITH sact - vtot_pag
 * ELSE
    REPLACE sact WITH sact - vtta
 * ENDIF
 *ENDIF
  UNLOCK
 ENDIF
 DBCOMMITALL()

 *Actualizo Archivo FACLmmaa cuando Tipo Pago es RI
 FOR con=1 TO LEN(campos)
   iva_fpag=SUBSTR(campos[con],2,2)
  IF iva_fpag='RI'
   IF SELECT("FACTU")=0
    *FACTURAS
    AREA()
    IF !FILE("FCC&periodo..DBF")
     IF USAR('FCC','FACTU')
      COPY TO FCC&periodo STRU
     ELSE
      CLOSE ALL
      QUIT
     ENDIF
    ENDIF
    IF USAR("FCC&periodo","FACTU")
     IF !FILE('FCC&periodo..NTX')
      INDEX ON clie+tcom+ltra+srie+nume TO FCC&periodo
     ENDIF
      wn=periodo+'1'
      IF !FILE('FC&wn..NTX')
       INDEX ON DTOS(fech) TO FC&wn
      ENDIF
      SET INDEX TO FCC&periodo,FC&wn
    ELSE
     QUIT
    ENDIF
   ENDIF
   ADDREC(1)
   RECLOCK(0)
   REPLACE tcom WITH iva_fpag
   REPLACE ltra WITH SPACE(1)
   REPLACE srie WITH SPACE(5)
   REPLACE nume WITH SUBSTR(campos[con],35,8)
   REPLACE fech WITH CTOD(SUBSTR(campos[con],46,10))
   REPLACE tipo WITH '0'
   REPLACE lcom WITH SPACE(1)
   REPLACE scom WITH SPACE(5)
   REPLACE ncom WITH SPACE(8)
   REPLACE clie WITH vcli
   REPLACE stot WITH 0
   REPLACE ngra WITH 0
   REPLACE prin WITH iva1
   REPLACE prni WITH iva2
   REPLACE civa WITH vciv
   REPLACE ides WITH 0
   REPLACE irin WITH VAL(SUBSTR(campos[con],59,12))
   REPLACE irni WITH 0
   REPLACE paga WITH VAL(SUBSTR(campos[con],59,12))
   REPLACE cont WITH .F.
   REPLACE nrem WITH SPACE(8)
   REPLACE canc WITH .T.
   REPLACE obs1 WITH "RETENCION IVA"
   REPLACE obs2 WITH ""
   REPLACE impr WITH .F.
   REPLACE baja WITH .F.
   REPLACE desc WITH 0
   UNLOCK
  ENDIF
 NEXT
 DBCOMMITALL()
 IF SELECT("FACTU")#0
  SELECT FACTU
  CLOSE
 ENDIF

*            *Actualizo Archivo de CAJA
*            *Actualizo Archivo Temporal de Movimientos de Caja
*            AREA()
*            IF USAR('ACTUALIZ','MOVIDO')
*             *
*            ELSE
*             QUIT
*            ENDIF
*            *CAJA (Se abre y se cierra)
*            wperi=SUBSTR(DTOC(vFech),4,2)+RIGHT(DTOC(vFech),2)
*            IF !FILE("CAJA&wperi..DBF")
*             AREA()
*             IF USAR('CAJA','CAJA')
*              COPY TO CAJA&wperi STRU
*             ELSE
*              CLOSE ALL
*              QUIT
*             ENDIF
*             CLOSE
*            ENDIF
*            AREA()
*            IF USAR("CAJA&wperi","CAJA")
*             IF !FILE('CAJA&wperi..NTX')
*              INDEX ON tcom+ltra+srie+nume+fpag TO CAJA&wperi
*             ENDIF
*             wperio=wperi+'1'
*             IF !FILE('CAJ&wperio..NTX')
*              INDEX ON fdia+tcaj+ncta+fpag TO CAJ&wperio
*             ENDIF
*             SET INDEX TO CAJA&wperi,CAJ&wperio
*            ELSE
*             QUIT
*            ENDIF
*            SELECT CAJA
*            FOR con = 1 TO LEN(campos)
*             IF !EMPTY(SUBSTR(campos[con],2,2))
*              GO BOTTOM
*              ADDREC(1)
*              RECLOCK(1)
*              REPLACE tcom WITH 'PA'
*              REPLACE ltra WITH ' '
*              REPLACE srie WITH SPACE(4)
*              REPLACE nume WITH vnum
*              REPLACE fdia WITH SUBSTR(DTOC(vFech),1,2)
*              REPLACE fpag WITH SUBSTR(campos[con],2,2)
*              REPLACE tcaj WITH 'CC'
*              REPLACE impo WITH VAL(SUBSTR(campos[con],59,12))
*              REPLACE clie WITH vcli
*              REPLACE vend WITH vven
*              REPLACE prov WITH SPACE(4)   &&Se usa Cdo.le pagamos a un Proveedor
*              REPLACE ncta WITH '999'
*              REPLACE baja WITH .F.
*              UNLOCK
*              *Actualizo Archivo del dia para luego usarlo p/actualizar los meses
*              *posteriores si tuvieren movimientos
*              SELECT MOVIDO
*              ADDREC(1)
*              RECLOCK(1)
*              REPLACE fech WITH vFech
*              REPLACE fdia WITH LEFT(DTOC(vFech),2)
*              REPLACE tcaj WITH 'CC'
*              REPLACE fpag WITH SUBSTR(campos[con],2,2)
*              REPLACE ncta WITH '999'
*              REPLACE impo WITH VAL(SUBSTR(campos[con],59,12))
*              REPLACE actu WITH .F.
*              UNLOCK
*             ENDIF
*             SELECT CAJA
*            NEXT con
*            SELECT CAJA
*            CLOSE
*            SELECT MOVIDO
*            CLOSE
 *Actualizo Archivo de CHEQUES
 SELECT CHEQUES
 FOR con = 1 TO LEN(campos)
  IF !EMPTY(SUBSTR(campos[con],2,2)) .AND. SUBSTR(campos[con],2,2) = 'CH'
   GO BOTTOM
   ADDREC(0)
   RECLOCK(0)
   vint = CERO(VAL(vint)+1,4)
   REPLACE seri WITH vser
   REPLACE nano WITH vnano
   REPLACE nint WITH vint
   REPLACE clie WITH vcli
   REPLACE ctas WITH SUBSTR(Campos[con],72,20)
   REPLACE prov WITH SPACE(4)
   REPLACE ntal WITH SUBSTR(campos[con],35,8)
   REPLACE banc WITH SUBSTR(campos[con],7,2)
   REPLACE plaz WITH SUBSTR(campos[con],20,3)
   REPLACE fvto WITH CTOD(SUBSTR(campos[con],46,10))
   REPLACE impo WITH VAL(SUBSTR(campos[con],59,12))
   REPLACE icta WITH 0
   REPLACE rech WITH .F.
   REPLACE fent WITH vFech
   REPLACE fsal WITH CTOD('  /  /    ')
   REPLACE bdep WITH SPACE(2)
   REPLACE pdep WITH SPACE(3)
   REPLACE obse WITH SUBSTR(Campos[con],93,30)
   UNLOCK
  ENDIF
 NEXT con
 *Actualizo archivo NROS.cheques
 SELECT NROS
 SEEK vser+vnano
 IF FOUND()
  RECLOCK(1)
  REPLACE nint WITH vint
  UNLOCK
 ELSE
  ADDREC(1)
  RECLOCK(1)
  REPLACE seri WITH vser
  REPLACE nano WITH vnano
  REPLACE nint WITH vint
  UNLOCK
 ENDIF
 DBCOMMITALL()

 *Impresion en papel
 vMess="IMPRIME ORDEN DE PAGO ? "
 DO WHILE .T.
  IF DUDA(vMess,2)
   SET CONSOLE OFF
   DO WHILE .NOT.ISPRINTER()
    CARTEL("LA IMPRESORA NO ESTA PREPARADA...",12,1,'E','*'+color3)
    INKEY(.1)
    TONE(700,1)
   ENDDO
   DO LIS_DATO2
   vMess="IMPRIME OTRA COPIA ? "
  ELSE
   EXIT
  ENDIF
 ENDDO
ENDDO
RESTSCREEN(0,0,24,79,pan_ant)
SETCOLOR(col_ant)
CLOSE ALL
SET KEY -38 TO
RETURN

FUNCTION sel_pag
PARAMETERS MOD,sub,pos
PRIVATE tcl,fil,pus,oriy,con,ydes
FIL=ROW()
IF LEN(cbte) < 10
 ydes = LEN(cbte)
ELSE
 ydes = 10
ENDIF
IF ydes = 1
 @8,5 SAY ''
ELSE
 pus = 10/LEN(cbte)*sub
 FOR oriy = 1 TO 10
    @7+oriy,5 SAY IF(pus <= oriy .and. pus >= oriy-.99999,IIF(sub=1,'',IIF(sub=LEN(cbte),'','')),'ณ')
 NEXT oriy
ENDIF
tcl = LASTKEY()
*Suma e imprime Sub_total
vtot_pag = 0
FOR con = 1 TO LEN(cbte)
 IF SUBSTR(cbte[con],2,1) = 'ฑ'
  vtot_pag = vtot_pag + VAL(SUBSTR(cbte[con],57,12))
 ENDIF
NEXT con
@21,58 SAY vtot_pag PICTURE '@Z 999999999.99'
@18,46 SAY SUBSTR(Cbte[sub],69,5) COLOR color9
@18,58 SAY SUBSTR(Cbte[sub],74,5) COLOR color9
DO CASE
CASE MOD = 0
 RETURN(2)
CASE MOD = 1 .OR. MOD = 2
 TONO(2)
 RETURN(2)
CASE MOD = 3
 DO CASE
 CASE tcl = 27         &&Presiona [ESC] para terminar Pagos
  KEYBOARD CHR(27)
  RETURN(0)
 CASE tcl = 13
   IF SUBSTR(cbte[sub],2,1) # 'ฑ'
    cbte[sub] = STUFF(cbte[sub],2,1,'ฑ')
   ELSE
    cbte[sub] = STUFF(cbte[sub],2,1,' ')
   ENDIF
   KEYBOARD CHR(24)
   rel = pos
   RETURN(1)
 CASE tcl = 42
  vtot_pag = 0
  FOR CON = 1 TO LEN(cbte)
   IF SUBSTR(cbte[con],2,1) = 'ฑ'
    vtot_pag = vtot_pag + VAL(SUBSTR(cbte[con],57,12))
   ENDIF
  NEXT
  IF vtot_pag = 0
   CARTEL("DEBE MARCAR ALGUN COMPROBANTE  [ OK ]",18,1,'N',color3)
   RETURN(1)
  ELSE
   RETURN(0)
  ENDIF
 OTHERWISE
  RETURN(2)
 ENDCASE
ENDCASE

FUNCTION ing_pag
PARAMETERS MOD,sub,pos
PRIVATE tcl,tpa,ban,nom_ban,pza,nom_pza,fbol,imp,fObse,fCtas
PRIVATE fTasi,fNasi,Nom_Tasi
FIL=ROW()
pus = 9/30*sub
FOR oriy = 1 TO 9
   @6+oriy,4 SAY IF(pus <= oriy .and. pus >= oriy-.99999,IIF(sub=1,'',IIF(sub=30,'','')),'ณ')
NEXT oriy
tcl = LASTKEY()
*Suma e imprime Sub_total
vtta = 0
FOR con = 1 TO LEN(campos)
 IF !EMPTY(SUBSTR(campos[con],2,2))
  vtta = vtta + (VAL(SUBSTR(campos[con],59,12))*IIF(SUBSTR(campos[con],2,2)='US',vdol,1))
 ENDIF
NEXT con
@2,64 SAY vtta PICTURE '@Z 999999999.99'
*Si Completo el Importe a pagar Salimos automaticamente
IF vtta = vtot_pag
 MOD = 3
 tcl = 42
ENDIF
IF EMPTY(SUBSTR(campos[sub],2,2))
 *Si no hay pagos cargados
 tpa = SPACE(2)
 imp = 0
 ban = SPACE(2)
 nom_ban = SPACE(9)
 pza = SPACE(3)
 nom_pza = SPACE(9)
 bol = SPACE(8)
 fbol = DATE()
 fObse = space(30)
 fCtas = SPACE(20)
 fNcta = SPACE(3)
 fNomCta=SPACE(15)
 fprov = SPACE(4)
 fNomPro=SPACE(25)
ELSE
 *si hay pagos cargados
 tpa = SUBSTR(campos[sub],2,2)
 ban = SUBSTR(campos[sub],7,2)
 nom_ban = SUBSTR(campos[sub],10,9)
 pza = SUBSTR(campos[sub],20,3)
 nom_pza = SUBSTR(campos[sub],24,9)
 bol = SUBSTR(campos[sub],35,8)
 fbol = CTOD(SUBSTR(campos[sub],46,10))
 imp = VAL(SUBSTR(campos[sub],59,12))
 fCtas = SUBSTR(campos[sub],72,20)
 fObse = SUBSTR(campos[sub],93,30)
 fNcta = SUBSTR(campos[sub],123,3)
 fNomCta=SUBSTR(campos[sub],126,15)
 fProv = SUBSTR(campos[sub],141,4)
 fNomPro=SUBSTR(campos[sub],145,25)
ENDIF
@17,11 SAY fNcta+" "+fNomCta COLOR color9
@17,40 SAY fProv+" "+fNomPro COLOR color9
@18,48 SAY fObse COLOR color9
@19,48 SAY fCtas COLOR color9
DO CASE
CASE MOD = 0
 RETURN(2)
CASE MOD = 1 .OR. MOD = 2
 TONO(2)
 RETURN(2)
CASE MOD = 3
 DO CASE
 CASE tcl = 27    &&Salir sin grabar
  RETURN(0)
 CASE tcl = 13
  XPC = 10
  XC = 1
  SET KEY -4 TO va_tras
  SETCOLOR(color1)
  DO WHILE XC <= XPC
   XD = 1
   SET ESCAPE ON
   SET CURSOR ON
   DO CASE
    CASE XC = 1
     IF vaju > 0
      tpa = 'AP'
      KEYBOARD CHR(13)
     ENDIF
     IF vaju < 0
      tpa = 'AN'
      KEYBOARD CHR(13)
     ENDIF
     @FIL,6 GET tpa PICTURE '@k !!' VALID TIP_PAG(@tpa)
     READ
     @FIL,6 SAY tpa
     IF (tpa = 'AP' .OR. tpa = 'AN') .AND. XD = 1
      XC = 4
     ENDIF
     IF tpa # 'CH'
      ban = SPACE(2)
      nom_ban = SPACE(9)
      pza = SPACE(3)
      nom_pza = SPACE(9)
      bol = SPACE(8)
      @FIL,11 SAY ban
      @FIL,14 SAY nom_ban
      @FIL,24 SAY pza
      @FIL,28 SAY nom_pza
      @FIL,39 SAY bol
     ENDIF
    CASE XC = 2
     IF tpa = 'CH'
      @FIL,11 GET ban PICTURE "@K 99" VALID VAL_BAN(@ban,@nom_ban)
      READ
      ban=CERO(VAL(ban),2)
      @FIL,11 SAY ban
      @FIL,14 SAY nom_ban
     ENDIF
    CASE XC = 3
     IF tpa = 'CH'
      *VAL_PZA esta en MOVBANC1.PRG
      @FIL,24 GET pza PICTURE "@K 999" VALID VAL_PZA(@pza,@nom_pza)
      READ
      pza=CERO(VAL(pza),3)
      @FIL,24 SAY pza
      @FIL,28 SAY nom_pza
     ENDIF
    CASE XC = 4
     IF tpa # 'EF' .AND. tpa # 'AP' .AND. tpa # 'AN' .AND. tpa # 'US'
      @FIL,39 GET bol PICTURE "@K 99999999"
      READ
      IF tpa="CH"
       SELECT CHEQUES
       IF !EXICOD("CHEQUES",2,ban+pza+bol,"CHEQUE YA INGRESADO  [ OK ]",19,.T.)
        XC=1
       ENDIF
      ENDIF
      @FIL,39 SAY bol
     ENDIF
    CASE XC = 5
     IF tpa # 'EF' .AND. tpa # 'AP' .AND. tpa # 'AN' .AND. tpa # 'US'
      @FIL,50 GET fbol VALID !EMPTY(fbol)
      READ
     ENDIF
    CASE XC = 6
     IF !EMPTY(SUBSTR(campos[sub],2,2))
      vtta = vtta - (VAL(SUBSTR(campos[sub],59,12))*IIF(tpa='US',vdol,1))
     ENDIF
     imp = vtot_pag - vtta
     @FIL,63 GET imp PICTURE "@K 999999999.99" VALID VAL_IMP(@imp,tpa)
     READ
     IF LASTKEY() = 27
      IF !EMPTY(SUBSTR(campos[sub],2,2))
       vtta = vtta + (VAL(SUBSTR(campos[sub],59,12))*IIF(tpa='US',vdol,1))
      ENDIF
     ENDIF

    CASE XC = 7
     *Observaciones para Cheques
     *IF tpa="CH"
      @18,48 GET fObse PICTURE "@!"
      READ
     *ENDIF

    CASE XC = 8
     *Nro.Cuenta del Cliente que Entrega el Cheque
     IF tpa="CH"
      @19,48 GET fCtas PICTURE "@!"
      READ
     ENDIF

    CASE XC = 9
     *Cuenta Contable Retencion
    *IF tpa="RI".OR. tpa="RG".OR. tpa="RB"
    * Cam="ncta~nomb"
    * *Cuentas
    * AREA()
    * OPEN_DBF("MAECUENT","CUENTAS","C","MAECUENT","ncta","NOMCUENT","nomb")
    * @17,11 GET fNcta PICTURE "@K 999" VALID VALIDA(@fNcta,@fNomCta,"CUENTAS",Cam,3,30,"NCTA","NOMB")
    * READ
    * @17,11 SAY fNcta+" "+SUBSTR(fNomCta,1,15) COLOR color9
    * SELECT CUENTAS
    * CLOSE
    *ELSE
      fNcta = SPACE(3)
      fNomCta=SPACE(15)
      fprov = SPACE(4)
      fNomPro=SPACE(25)
    *ENDIF

    CASE XC = 10
     *Proveedor afectado por la retencion
    *IF tpa="RI".OR. tpa="RG".OR. tpa="RB"
    * Cam="prov~nomb"
    * *Proveedor
    * AREA()
    * OPEN_DBF("MAEPROVE","PROVEE","C","MAEPROVE","prov","NOMPROVE","nomb")
    * @17,40 GET fProv PICTURE "@K 9999" VALID VALIDA(@fProv,@fNomPro,"PROVEE",Cam,4,30,"PROV","NOMB")
    * READ
    * @17,40 SAY fProv+" "+SUBSTR(fNomPro,1,25) COLOR color9
    * SELECT PROVEE
    * CLOSE
    *ELSE
      fNcta = SPACE(3)
      fNomCta=SPACE(15)
      fprov = SPACE(4)
      fNomPro=SPACE(25)
    *ENDIF

   ENDCASE
   IF LASTKEY() = 27
    CLEAR GETS
    XC = 11
   ELSE
    IF XD = -1 .AND. tpa # 'CH' .AND. XC = 4
     XD = -2
    ENDIF
    IF XD=-1.AND.(tpa='AP'.OR.tpa='AN'.OR.tpa='EF'.OR.tpa='US').AND.XC=6
     XD = -5
    ENDIF
    XC = XC + XD
   ENDIF
  ENDDO
  SET KEY -4 TO
  SET CURSOR OFF
  IF LASTKEY() = 27
   RETURN(2)
  ENDIF
  campos[sub] = STUFF(campos[sub],2,2,tpa)
  campos[sub] = STUFF(campos[sub],7,2,ban)
  campos[sub] = STUFF(campos[sub],10,9,nom_ban)
  campos[sub] = STUFF(campos[sub],20,3,pza)
  campos[sub] = STUFF(campos[sub],24,9,nom_pza)
  campos[sub] = STUFF(campos[sub],35,8,bol)
  campos[sub] = STUFF(campos[sub],46,10,DTOC(fbol))
  campos[sub] = STUFF(campos[sub],59,12,TRANSFORM(imp,'@Z 999999999.99'))
  campos[sub] = STUFF(campos[sub],72,20,fCtas)
  campos[sub] = STUFF(campos[sub],93,30,fObse)
  campos[sub] = STUFF(campos[sub],123,3,fNcta)
  campos[sub] = STUFF(campos[sub],126,15,fNomCta)
  campos[sub] = STUFF(campos[sub],141,4,fProv)
  campos[sub] = STUFF(campos[sub],145,25,fNomPro)
  IF vaju <> 0
   imp = vaju
   KEYBOARD CHR(24)+CHR(13)
  ELSE
   KEYBOARD CHR(24)
  ENDIF
  rel = pos
  RETURN(1)
 CASE (tcl >= 65 .AND. tcl <= 90) .OR. (tcl >= 97 .AND. tcl <= 122)
  KEYBOARD CHR(13)+CHR(tcl)
  RETURN(2)
 CASE tcl = 42
  vtta = 0
  FOR CON = 1 TO LEN(campos)
   IF !EMPTY(SUBSTR(campos[con],2,2))
    vtta = vtta + (VAL(SUBSTR(campos[con],59,12))*IIF(SUBSTR(campos[con],2,2)='US',vdol,1))
   ENDIF
  NEXT
  IF vtta = 0
   CARTEL("DEBE INGRESAR UN PAGO COMO MINIMO  [ OK ]",12,1,'N',color3)
   RETURN(1)
  ENDIF
  IF vtot_pag <> vtta
   CARTEL("DEBE CUADRAR Saldo factura CON Imp. Pagado  [ OK ]",18,1,'N',color3)
  ENDIF
  RETURN(0)
 CASE tcl = 7
  ADEL(campos,sub)
  campos[LEN(campos)] = '    ณ   ณ         ณ   ณ         ณ          ณ            ณ              '+SPACE(20)+SPACE(30)+SPACE(47)+SPACE(24)
  tpa = SUBSTR(campos[sub],2,2)
  ban = SUBSTR(campos[sub],7,2)
  nom_ban = SUBSTR(campos[sub],10,9)
  pza = SUBSTR(campos[sub],20,3)
  nom_pza = SUBSTR(campos[sub],24,9)
  bol = SUBSTR(campos[sub],35,8)
  fbol = CTOD(SUBSTR(campos[sub],46,10))
  imp = VAL(SUBSTR(campos[sub],59,12))
  fCtas = SUBSTR(campos[sub],72,20)
  fObse = SUBSTR(campos[sub],93,30)
  fNcta = SUBSTR(campos[sub],123,3)
  fNomCta=SUBSTR(campos[sub],126,15)
  fProv = SUBSTR(campos[sub],141,4)
  fNomPro=SUBSTR(campos[sub],145,25)
  @17,11 SAY fNcta+" "+fNomCta COLOR color9
  @17,40 SAY fProv+" "+fNomPro COLOR color9
  @18,48 SAY fObse COLOR color9
  @19,48 SAY fCtas COLOR color9
  rel = pos
  RETURN(1)
 OTHERWISE
  RETURN(2)
 ENDCASE
ENDCASE

*Control de clientes
FUNCTION VAL_CLI3
PARAMETERS cli,fil,col,colo_sug,lon
IF LASTKEY() = 27
 KEYBOARD CHR(27)
 RETURN .T.
ENDIF
PRIVATE colo_a,pan_ant,pipi
IF PCOUNT() = 1
 ok1=0
ELSE
 ok1=1
ENDIF
antsel=ALLTRIM(STR(SELECT()))
colo_a=SETCOLOR()
SELECT CLIENTES
SET ORDER TO 1
cli = CERO(VAL(cli),6)
vcli = cli
IF vcli='000000'
 vnom='CONSUMIDOR OCASIONAL'
 vsal = 0
 vacr = 0
 regcli = 0
 vDpto=""
 vPcia=""
 vCPs="    "
 vDom=""
 vCUit=SPACE(13)
 IF ok1=1
  SETCOLOR(colo_sug)
  @fil,col SAY vcli+'   '+SUBSTR(vnom,1,lon)
  SETCOLOR(colo_a)
 ENDIF
ELSE
 SEEK cli
 IF .NOT. FOUND()
  pan_ant = savescreen(5,2,24,72)
  SETCOLOR(color3)
  SOMBRA(5,2,23,71,'V1',3,'',color3)
  @7,2 say'ฬออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน'
  @20,2 say'วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ'
  @17,2 say'ฬออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน'
  SETCOLOR(INVER)
  @5,38 say' BUSCAR '
  SETCOLOR(color3)
  @18,3 SAY'BUSCAR:'
  @19,3 SAY SPACE(38)
  @21,13 say chr(24)+chr(25)+' PgUp PgDn Mover         '+chr(17)+'ฤู Acepta'
  @22,13 say'F2-Cambia Clave Busqueda  [Esc]-Salir'
  DECLARE cam[3],sep[3],ind[2],lar[2],nom[2]
  carga1(cam,"clie~nomb~domi")
  tit='Numero  Nombre  Cliente                Domicilio                           '
  carga2(sep,'5,2,0')
  ind[1]=1
  lar[1]=6
  nom[1]='Codigo cliente : '
  ind[2]=2
  lar[2]=30
  nom[2]='Nombre del Cliente : '
  ord_ant=str(INDEXORD())
  set order to 2
  GO TOP
  BUSCARE(6,4,16,70,cam,tit,sep,ind,lar,19,4,nom,18,14)
  regis=recno()
  SETCOLOR(colo_a)
  restscreen(5,2,24,72,pan_ant)
  set order to &ord_ant
  IF LASTKEY() <> 27
   vcli=clie
   vnom = nomb
   vciv = civa
   vsal = sact
   vAcre=acre
   regcli = RECNO()
   vDpto=dpto
   vPcia=pcia
   vCPs=cpos
   vDom=domi
   vCuit=LEFT(cuit,2)+"-"+SUBSTR(cuit,3,8)+"-"+RIGHT(cuit,1)
   IF ok1=1
    SETCOLOR(colo_sug)
    @fil,col SAY vcli+'   '+SUBSTR(vnom,1,lon)
    SETCOLOR(colo_a)
   ENDIF
  ELSE
   SELECT &antsel
   RETURN .F.
  ENDIF
 ELSE
  vnom=nomb
  vdom = domi
  vciv = civa
  vsal = sact
  vAcre=acre
  vDpto=dpto
  vPcia=pcia
  vCPs=cpos
  vDom=domi
  vCuit=LEFT(cuit,2)+"-"+SUBSTR(cuit,3,8)+"-"+RIGHT(cuit,1)
  regcli = RECNO()
  IF ok1=1
   SETCOLOR(colo_sug)
   @fil,col SAY vcli+'   '+SUBSTR(vnom,1,lon)
   SETCOLOR(colo_a)
  ENDIF
 ENDIF
ENDIF
*BUsca Cbtes.Pendientes
SELECT CTACTE
SET ORDER TO 1
SEEK vcli
IF !FOUND()
 CARTEL(ALLTRIM(vnom)+" NO TIENE NADA p/PAGAR  [ OK ]",20,1,'N',color3)
 SELECT &antsel
 vsal=0
 RETURN .T.
ELSE
 p=0
 DO WHILE clie = vcli
  IF sact<>0 .AND. tcom<>'NC' .AND. !BAJA && .AND. !cont
   IF p > 0
    ASIZE(cbte,p+1)
    ASIZE(dia,p+1)
   ENDIF
   p = p+1                                                                               &&%Ing.Bruto
   cbte[LEN(cbte)] = "   ณ      ณ               ณ            ณ              ณ              "+SPACE(5)
   IF cont
    cbte[LEN(cbte)] = STUFF(cbte[LEN(cbte)],10,1,CHR(16))
   ENDIF
   cbte[LEN(cbte)] = STUFF(cbte[LEN(cbte)],7,2,tcom)
   cbte[LEN(cbte)] = STUFF(cbte[LEN(cbte)],13,13,ltra+srie+nume)
   cbte[LEN(cbte)] = STUFF(cbte[LEN(cbte)],29,10,DTOC(fech))
   cbte[LEN(cbte)] = STUFF(cbte[LEN(cbte)],42,12,TRANSFORM(paga,"999999999.99"))
   cbte[LEN(cbte)] = STUFF(cbte[LEN(cbte)],57,12,TRANSFORM(sact,"999999999.99"))
   dia[LEN(dia)] = BERRE(DTOC(fech))
  ENDIF
  SKIP
 ENDDO
 IF p = 0
  CARTEL(ALLTRIM(vnom)+" NO TIENE NADA p/PAGAR  [ OK ]",20,1,'N',color3)
  SELECT &antsel
  vsal=0
  RETURN .T.
 ENDIF
 *Ordena por fecha
 DECLARE cam[1]
 cam[1] = "cbte"
 ORDEN(dia,cam)
 *Busco en Archivos FACLmmaa el Porcentaje de IVA Vigente en ese Momento
 FOR Con=1 TO LEN(Cbte)
  bPer=CHR(VAL(SUBSTR(Cbte[con],32,2))+64)+SUBSTR(Cbte[con],35,4)
  vCla=vCli+SUBSTR(Cbte[con],7,2)+SUBSTR(Cbte[con],13,13)
  *FACTURAS
  IF FILE("FCC&bPer..DBF")
   AREA()
   IF USAR("FCC&bPer","FAC_IVA")
    IF !FILE('FCC&bPer..NTX')
     INDEX ON clie+tcom+ltra+srie+nume TO FCC&bPer
    ENDIF
    SET INDEX TO FCC&bPer
   ELSE
    QUIT
   ENDIF
   SEEK vCla
   IF FOUND()
    cbte[Con] = STUFF(cbte[Con],69,5,TRANSFORM(prin,"99.99"))
    cbte[Con] = STUFF(cbte[Con],74,5,TRANSFORM(prni,"99.99"))
    cbte[Con] = STUFF(cbte[Con],79,5,TRANSFORM(0,"99.99"))
   ELSE
    cbte[Con] = STUFF(cbte[Con],69,5,TRANSFORM(iva1,"99.99"))
    cbte[Con] = STUFF(cbte[Con],74,5,TRANSFORM(iva2,"99.99"))
    cbte[Con] = STUFF(cbte[Con],79,5,TRANSFORM(0,"99.99"))
   ENDIF
   CLOSE
  ELSE
   cbte[Con] = STUFF(cbte[Con],69,5,TRANSFORM(iva1,"99.99"))
   cbte[Con] = STUFF(cbte[Con],74,5,TRANSFORM(iva2,"99.99"))
   cbte[Con] = STUFF(cbte[Con],79,5,TRANSFORM(0,"99.99"))
  ENDIF
 NEXT
 SELECT &antsel
 RETURN .T.
ENDIF

FUNCTION tip_pag
PARAMETERS tip
PRIVATE busc
busc = ASCAN(tp,tip)
IF busc = 0
 pan=SAVESCREEN(20,4,22,70)
 PRIV menu1[8],fi_co[8]
 CARGA1(menu1,'`1- EF~`2- US~`3- CH~`4- RI~`5- RB~`6- RG~`7- AP~`8- AN')
 CARGA1(fi_co,'20,05~21,05~22,05~20,21~21,21~22,21~20,41~21,41')
 busc=1
 busc= PROMPT(menu1,fi_co,'I',busc)
 RESTSCREEN(20,4,22,70,pan)
 IF busc=0
  RETURN .F.
 ENDIF
ENDIF
tip = tp[busc]
RETURN .T.

FUNCTION VAL_IMP
PARAMETERS impi,tpa
PRIVATE ele
IF ROUND(((IIF(tpa='US',vdol,1)*impi)+vtta),2) > ROUND(vtot_pag,2)
 ele=CARTEL("!!!! ESTA PAGANDO DE MAS !!!! [ AJUSTA ]  [ A CUENTA ]",18,2,'N',color3)
 IF LASTKEY()=27
  KEYBOARD CHR(27)
  RETURN .T.
 ENDIF
 IF ele
  vaju = ROUND(vtot_pag - ((IIF(tpa='US',vdol,1)*impi)+vtta),2)
 ENDIF
ENDIF
RETURN .T.

PROCEDURE LIS_DATO2
PRIVATE Cbte_Aux:={}
PRIVATE AntSel
AntSel=ALLTRIM(STR(SELECT()))
*Leo Nombre de Provincia y Departamento
*Departamentos
AREA()
OPEN_DBF("MAEDEPAR","DPTOS","C","MAEDEPAR","dpto","NOMDEPAR","nomb")
SELECT DPTOS
SEEK vdpto
IF FOUND()
 nom_dpto=nomb
ELSE
 nom_dpto=SPACE(15)
ENDIF
CLOSE
*Provincias
AREA()
OPEN_DBF("MAEPROVI","PCIAS","C","MAEPROVI","pcia","NOMPROVI","nomb")
SELECT PCIAS
SEEK vpcia
IF FOUND()
 nom_pcia=nomb
ELSE
 nom_pcia=SPACE(15)
ENDIF
CLOSE
SELECT &AntSel
vdia=LEFT(DTOC(DATE()),2)
mes=MESES(DATE())
ano=CERO(YEAR(DATE()),4)
hor=0
pagina=0
SET CONSOLE OFF
SET PRINT ON
?? CHR(27)+CHR(120)+CHR(0)+CHR(18)
?? CHR(27)+CHR(67)+CHR(36)
?? CHR(27)+CHR(33)+CHR(5)
*Numero de Pago y Fecha
NORMA()
? SPACE(50)+"I-"+vPago+SPACE(4)+"O-"+vNum
? SPACE(75)+vdia+' '+mes+' '+ano
?
?
*Datos del Cliente
? SPACE(5)+vNom+SPACE(38)+vCli
? SPACE(7)+vdom+SPACE(2)+nom_dpto+SPACE(2)+nom_pcia
? SPACE(5)+vCuit
?
*Importe en letras
CHICO()
vlet= UPPER(LETTER1(INT(ABS(vTTa))))+' con/'+RIGHT(STR(ABS(vTTa),12,2),2)
IF LEN(vlet) > 100
 vle1 = SUBSTR(vlet,1,100)
 FOR con = 1 TO LEN(vle1)
  IF RIGHT(vle1,1) # ' '
   vle1 = SUBSTR(vle1,1,LEN(vle1)-1)
  ELSE
   con = LEN(vle1)
  ENDIF
 NEXT
 vle2 = SUBSTR(vlet,LEN(vle1)+1)
ELSE
 vle1 = vlet
 vle2 = ''
ENDIF
? SPACE(40) + vle1
? SPACE( 3) + vle2
?
?
vsub=0
*Contar longitud maxima de Array de Cbtes.(xx) y De Formas de Pago (yy)
xx=0
FOR con= 1 TO LEN(cbte)
 IF SUBSTR(cbte[con],2,1) = '@'
  ASIZE(Cbte_Aux,LEN(Cbte_Aux)+1)
  Cbte_Aux[LEN(Cbte_Aux)] = Cbte[con]
  xx++
 ENDIF
NEXT
yy=0
FOR con= 1 TO LEN(campos)
 IF !EMPTY(SUBSTR(campos[con],1,2))
  yy++
 ENDIF
NEXT
vSub_Cb=0   &&Totaliza Cbtes. Pagados
vSub_Pa=0   &&Totaliza Formas de Pago
*Obtener Mayor Longitud para el Bucle de Impresion
IF xx > 17  &&Cbtes. que entran en una boleta
 xx=1
 xRes=.T.
 FOR con=1 TO LEN(Cbte_Aux)
  IF SUBSTR(Cbte_Aux[con],2,1)="@"
   vSub_Cb=vSub_Cb+VAL(SUBSTR(Cbte_Aux[con],57,12))
  ENDIF
 NEXT
ELSE
 xRes=.F.
ENDIF
zz=MAX(xx,yy)
Hor=0
FOR con=1 TO zz
 *Obtiene Cbte.
 IF LEN(Cbte_Aux)>=con
  IF !xRes
   IF SUBSTR(Cbte_Aux[con],2,1)="@"
    vLin=SUBSTR(Cbte_Aux[con],29,10)+SPACE(3)+;
         SUBSTR(Cbte_Aux[con], 7, 2)+' '+;
         SUBSTR(Cbte_Aux[con],13,13)+SPACE(13)+;
         SUBSTR(Cbte_Aux[con],57,12)
    *Obtengo Subtotales
    vSub_Cb=vSub_Cb+VAL(SUBSTR(Cbte_Aux[con],57,12))
   ELSE
    vLin=SPACE(54)
   ENDIF
  ELSE
   *Resumen por Muchos Cbtes.Pagados
   vLin="Segun Resumen"+SPACE(29)+STR(vSub_Cb,12,2)
  ENDIF
 ELSE
  vLin=SPACE(54)
 ENDIF
 *Obtiene Forma de Pago
 IF LEN(Campos)>=Con
  IF !EMPTY(SUBSTR(campos[con],2,2))
   vLin=vLin+SPACE(6)+SUBSTR(Campos[con],2,2)+SPACE(4)
   IF SUBSTR(Campos[con],2,2) # "CH"
    vLin=vLin+SUBSTR(Campos[con],93,30)+SPACE(37)
   ELSE
    vLin=vLin+SUBSTR(campos[con],10,9)+SPACE(6)+SUBSTR(campos[con],24,9)+SPACE(3)
    vLin=vLin+SUBSTR(campos[con],46,10)+SPACE(10)+SUBSTR(campos[con],35,8)+SPACE(12)
   ENDIF
   vLin=vLin+SUBSTR(campos[con],59,12)
   *Obtengo Subtotales
   vSub_Pa=vSub_Pa+VAL(SUBSTR(campos[con],59,12))
  ELSE
   vLin=vLin+SPACE(89)
  ENDIF
 ELSE
  vLin=vLin+SPACE(89)
 ENDIF
 *Imprimo Linea
 IF Hor=15
  *Imprimo Total de Formas de Pago
  vLin=STUFF(vLin,134,12,STR(vSub_Pa,12,2))
 ENDIF
 ? vLin
 *Suma Lineas Impresas
 Hor++
 vLin=""
NEXT
IF Hor<17
 FOR con=Hor TO 17
  IF con=15
   *Imprimo Total de Formas de Pago
   ? SPACE(134)+STR(vSub_Pa,12,2)
  ELSE
   ?
  ENDIF
 NEXT
ENDIF
*Imprimo Total de Cbtes. Pagados
*?
? SPACE(40)+STR(vSub_Cb,12,2)
?
EJECT
?? CHR(27)+CHR(33)+CHR(8)
?? CHR(27)+CHR(120)+CHR(0)+CHR(18)
?? CHR(27)+CHR(67)+CHR(72)
SET PRINT OFF
SET CONSOLE ON
RETURN
