*******************************************************
*Confirmacion de Pago de Facturas de Contado
*cuando No entregan el dinero en el momento de facturar
*******************************************************
PRIVATE pant_ant,col_ant,vdol,vser,vnano,vint,iva1,iva2
pant_ant=SAVESCREEN(0,0,24,79)
col_ant=SETCOLOR()
*ARCHIVO AUXILIAR
AREA()
*Porcentaje de iva
IF USAR('PORCEIVA')
 iva1 = IVAI
 iva2 = IVANI
ELSE
 QUIT
ENDIF
IF USAR('AUXILIAR','AUX')
 GO TOP
 PUBLIC vpago
 wpago=npcl
 vdol=DOLA
 vser=seri
 vnano=nano
 IF vser # CHR(64+VAL(LEFT(periodo,2)))
  vser=CHR(64+VAL(LEFT(periodo,2)))
  vnano=RIGHT(periodo,2)
  vint='0000'
 ELSE
  vint=nint
 ENDIF
ELSE
 QUIT
ENDIF
CLOSE
*clientes
AREA()
OPEN_DBF("MAECLIEN","CLIENTES","C","MAECLIEN","clie","NOMCLIEN","nomb")

*ZONAS
AREA()
OPEN_DBF("MAEZONAS","ZONAS","C","MAEZONAS","vend","NOMZONAS","nomb")

*CAJA
AREA()
IF USAR("CAJA&periodo","CAJA")
 IF !FILE('CAJA&periodo..NTX')
  INDEX ON tcom+ltra+srie+nume+fpag TO CAJA&periodo
 ENDIF
 wperio=periodo+'1'
 IF !FILE('CAJ&wperio..NTX')
  INDEX ON fdia+tcaj+ncta+fpag TO CAJ&wperio
 ENDIF
 SET INDEX TO CAJA&periodo,CAJ&wperio
ELSE
 QUIT
ENDIF
*FACTURAS
AREA()
IF USAR("FACL&periodo","FACTURA")
  IF !FILE('FACL&periodo..NTX')
   INDEX ON clie+tcom+nume TO FACL&periodo
  ENDIF
  wn = periodo+'1'
  IF !FILE('FAC&wn..NTX')
   INDEX ON DTOS(fech) TO FAC&wn
  ENDIF
  SET INDEX TO FACL&periodo,FAC&wn
ELSE
 QUIT
ENDIF
*CTA.CTE.
AREA()
IF USAR("CCCLIENT","CTACTE")
  IF !FILE('CCCLIENT.NTX')
   INDEX ON clie+tcom+nume TO CCCLIENT
  ENDIF
  IF !FILE('CCCLIE01.NTX')
   INDEX ON clie+DTOS(fech) TO CCCLIE01
  ENDIF
  IF !FILE('CCCLIE04.NTX')
   INDEX ON tcom+nume TO CCCLIE04
  ENDIF
  IF !FILE('CCCLIE03.NTX')
   INDEX ON vend+clie+DTOS(fech) TO CCCLIE03
  ENDIF
  SET INDEX TO CCCLIENT,CCCLIE01,CCCLIE04,CCCLIE03
ELSE
 QUIT
ENDIF
*Bancos
AREA()
OPEN_DBF("MAEBANCO","BANCOS","C","MAEBANCO","banc","NOMBANCO","nomb")

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
  INDEX ON vend TO MAECHEQ8
 ENDIF
 IF !FILE("MAECHEQ9.NTX")
  INDEX ON DTOS(fsal) TO MAECHEQ9
 ENDIF
 SET INDEX TO MAECHEQ1,MAECHEQ2,MAECHEQ3,MAECHEQ4,MAECHEQ5,MAECHEQ6,MAECHEQ7,MAECHEQ8,MAECHEQ9
ELSE
 QUIT
ENDIF
SET RELATION TO banc INTO BANCOS

PRIVATE pant_ant,col_ant,vcli,vciv,vven
PUBLIC vnom,vadju,nom_ven,vpor,vti_b,vti_bus,vnum
PRIVATE wocas
PRIVATE chep[10],chev[10],vim_ch,vpago[3],vvue[3],sal_che,resto,fvtgral,vvuelt,sale
vcli=SPACE(6)
vven=SPACE(3)
vnum=SPACE(8)
DECLARE tabla[1]
vti_b=CARTEL("PAGO DE FACTURAS POR  [ CLIENTE ]   [ NงFACTURA ]",22,2,'N',color3)
IF LASTKEY()=27
 RESTSCREEN(0,0,24,79,pant_ant)
 SETCOLOR(col_ant)
 CLOSE ALL
 RETURN
ENDIF
IF vti_b
 vti_bus=0  &&Por cliente
ELSE
 vti_bus=1  &&Por Factura
ENDIF
DO WHILE .T.
 sale=0
 AFILL(vpago,0)
 AFILL(vvue,0)
 STORE 0 TO vim_ch,wocas,resto,sal_che
 SETCOLOR(color3)
 SOMBRA(10,15,14,65,'H1',1,'',color3)
* @12,18 SAY "Cobrador: " GET vven PICTURE "@K 999" VALID VAL_VEN(@vven,11,29,color3,25)
 IF vti_bus=0
  @11,18 SAY "Cliente : " GET vcli PICTURE "@K 999999" VALID CON_CLI(@vcli,11,29,color3,25)
 ELSE
  @11,18 SAY "Factura : " GET vnum PICTURE "@K 99999999"
 ENDIF
 READ
 IF LASTKEY()=27
  RESTSCREEN(0,0,24,79,pant_ant)
  SETCOLOR(col_ant)
  CLOSE ALL
  RETURN
 ENDIF
 IF CARTEL("DATOS CORRECTOS  [ SI ]  [ NO ]",18,1,'N',color3)
  IF LASTKEY() = 27
   RESTSCREEN(0,0,24,79,pant_ant)
   SETCOLOR(col_ant)
   LOOP
  ENDIF
  SELECT CTACTE
  IF vti_bus=0
   *Busqueda por Cliente
   SEEK vcli
   p=0
   IF FOUND()
    DECLARE tabla[1]
    DO WHILE clie = vcli
     IF tcom='FA' .AND. cont .AND. .NOT.canc .AND. clie=vcli .AND. !baja
      carg=.T.
     ELSE
      carg=.F.
     ENDIF
     IF carg
      IF p>0
       ASIZE(tabla,p+1)
      ENDIF
      p=p+1
      tabla[p]=nume+' '+tcom+' '+DTOC(fech)+' '+STR(sact,12,2)+' '+IIF(cont,'Cdo.','C.C.')+' '+IIF(baja,'Anula',SPACE(5))
     ENDIF
     SKIP
    ENDDO
   ENDIF
   IF p=0
    CARTEL("NO HAY FACTURAS DE CONTADO PARA CONFIRMAR  [ OK ]",22,1,'N',color3)
    RESTSCREEN(0,0,24,79,pant_ant)
    SETCOLOR(col_ant)
    LOOP
   ENDIF
   SET ORDER TO 1
   IF p>1
    panant=SAVESCREEN(9,30,24,78)
    SETCOLOR(color3)
    SOMBRA(9,30,23,77,'V1',3,'',color3)
    @11,30 say'ฬออออออออออออออออออออออออออออออออออออออออออออออน'
    @21,30 say'ฬออออออออออออออออออออออออออออออออออออออออออออออน'
    @10,31 SAY" Nฃmero  Tp   Fecha      Importe    Tipo Estad"
    SETCOLOR(INVER)
    @9,38 SAY' BUSCAR '
    SETCOLOR(color3)
    @22,31 SAY CHR(24)+CHR(25)+' PgUp PgDn Mover   '+CHR(17)+'ฤู Acepta   [Esc]-Salir'
    *****************************************************************
    elemento=SCR(tabla,9,12,31,LEN(tabla),9,30,23,77,1,color1,color3)
    *****************************************************************
    RESTSCREEN(9,30,24,78,panant)
   ELSE
    elemento=1
   ENDIF
   IF LASTKEY()=13
     vari=vcli+SUBSTR(tabla[elemento],10,2)+SUBSTR(tabla[elemento],1,8)
     SEEK vari
     IF !FOUND()
      CARTEL("COMPROBANTE NO EXISTE  [ OK ]",22,1,'N',color3)
      RESTSCREEN(0,0,24,79,pant_ant)
      SETCOLOR(col_ant)
      LOOP
     ENDIF
   ELSE
    IF LASTKEY()=27
     RESTSCREEN(0,0,24,79,pant_ant)
     SETCOLOR(col_ant)
     LOOP
    ENDIF
   ENDIF
  ELSE
   *Busqueda por Factura
   SET ORDER TO 3
   vnum=CERO(VAL(vnum),8)
   vari="FA"+vnum
   SEEK vari
   IF !FOUND()
    CARTEL("COMPROBANTE NO EXISTE  [ OK ]",22,1,'N',color3)
    RESTSCREEN(0,0,24,79,pant_ant)
    SETCOLOR(col_ant)
    LOOP
   ELSE
    IF !cont
     CARTEL("NO ES FACTURA DE CONTADO  [ OK ]",22,1,'N',color3)
     RESTSCREEN(0,0,24,79,pant_ant)
     SETCOLOR(col_ant)
     LOOP
    ENDIF
    IF sact=0
     CARTEL("FACTURA YA CANCELADA  [ OK ]",22,1,'N',color3)
     RESTSCREEN(0,0,24,79,pant_ant)
     SETCOLOR(col_ant)
     LOOP
    ENDIF
    IF baja
     CARTEL("FACTURA DADA DE BAJA  [ OK ]",22,1,'N',color3)
     RESTSCREEN(0,0,24,79,pant_ant)
     SETCOLOR(col_ant)
     LOOP
    ENDIF
   ENDIF
   vcli=clie
   IF VAL(vcli)=0
    KEYBOARD(13)
   ENDIF
   CON_CLI(@vcli)
  ENDIF
 ELSE
  RESTSCREEN(0,0,24,79,pant_ant)
  SETCOLOR(col_ant)
  LOOP
 ENDIF
 vnum=nume
 tgral=sact
 vven=vend
 vfec=fech
 reg=RECNO()
 *Confirmar Cobrador Zona
 @12,18 SAY "Zona vta. " GET vven PICTURE "@K 999" &&VALID VAL_VEN(@vven,12,29,color3,25)
 READ
 IF LASTKEY()=27
  RESTSCREEN(0,0,24,79,pant_ant)
  SETCOLOR(col_ant)
  LOOP
 ENDIF

 *Carga del pago
 SETCOLOR(color1)
 STORE 0 TO vtgral,vvuelt,sale
 PAG_VUE(@chep,@vpago,tgral,@vtgral,.T.,@sale)

 scr1=SAVESCREEN(14,10,19,25)
 SOMBRA(14,11,18,23,'H2',1,'',color3)
 ok=0
 SETCOLOR(color3)
 IF ((vpago[2]#0.AND.!EMPTY(SUBSTR(chep[1],2,2))).OR.vpago[2]=0).AND.sale=1
  @ 15,12 PROMPT ' MODIFICAR '
  @ 16,12 PROMPT ' GRABAR    '
  opc1=2
  ok=1
 ENDIF
 @ 17,12 PROMPT ' CANCELAR  '
 MENU TO opc1
 IF ok=0
  opc1=3
 ENDIF
 ok=0
 SETCOLOR(color1)
 RESTSCREEN(14,10,19,25,scr1)
 DO CASE
  CASE opc1=1
   SET CURSOR OFF
   RESTSCREEN(0,0,24,79,pant_ant)
   LOOP
  CASE opc1=2
   *GRAVAR ARCHIVO
   *Determina Importe total pagado(por si paga parcialmente)
   vtot_pag=0
   FOR con=1 TO LEN(vpago)
    vtot_pag=vtot_pag + vpago[con]
   NEXT
   *Si entrega de mas, extraigo importe total que deberia pagar para actualizar
   *CTACTE.
   IF vtot_pag > tgral
    vtot_pag = tgral
   ENDIF

   ok=0
   IF vtgral<0 &&paga de m s
    acredi=CARTEL("Destino del RESTO  [ VUELTO ]  [ ACREDITA ]",12,1,'N',color3)
    IF LASTKEY()=27
     LOOP
    ENDIF
    IF acredi
     PAG_VUE(@chev,@vvue,vtgral*(-1),@vvuelt,.F.,sale)
     resto=0
    ELSE
     resto=vtgral*(-1)    &&guardo el resto en MAECLIEN para adjudicarselo
     *                      && a la proxima factura
    ENDIF
   ENDIF
   *Actualizo Archivo de CAJA
   *Abro archivo de Movimientos diarios para luego actualizar caja
   AREA()
   IF USAR("ACTUALIZ","MOVIDO")
    *
   ELSE
    QUIT
   ENDIF
   SELECT CAJA
   FOR con=1 TO LEN(vpago)
    IF vpago[con]<>0
     GO BOTTOM
     ADDREC(1)
     RECLOCK(1)
     REPLACE tcom WITH 'FA'
     REPLACE nume WITH vnum
     REPLACE fdia WITH SUBSTR(DTOC(DATE()),1,2)
     REPLACE tcaj WITH 'CD'
     REPLACE fpag WITH IIF(con=1,'EF',IIF(con=2,'CH',IIF(con=3,'US','  ')))
     REPLACE impo WITH vpago[con]
     REPLACE clie WITH vcli
     REPLACE ncta WITH '999'
     REPLACE baja WITH .F.
     UNLOCK
     *Actualizo Archivo del dia para luego usarlo p/actualizar los meses
     *posteriores si tuvieren movimientos
     SELECT MOVIDO
     ADDREC(1)
     RECLOCK(1)
     REPLACE fech WITH DATE()
     REPLACE fdia WITH LEFT(DTOC(DATE()),2)
     REPLACE tcaj WITH 'CD'
     REPLACE fpag WITH IIF(con=1,'EF',IIF(con=2,'CH',IIF(con=3,'US','  ')))
     REPLACE ncta WITH '999'
     REPLACE impo WITH vpago[con]
     REPLACE actu WITH .F.
     UNLOCK
     SELECT CAJA
    ENDIF
   NEXT con
   COMMIT
   *Actualizo CAJA si di VUELTO
   FOR con=1 TO LEN(vvue)
    IF vvue[con]<>0
     GO BOTTOM
     ADDREC(1)
     RECLOCK(1)
     REPLACE tcom WITH 'FA'
     REPLACE nume WITH vnum
     REPLACE fdia WITH SUBSTR(DTOC(DATE()),1,2)
     REPLACE tcaj WITH 'VD'
     REPLACE fpag WITH IIF(con=1,'EF',IIF(con=2,'CH',IIF(con=3,'US','  ')))
     REPLACE impo WITH vvue[con]*(-1)
     REPLACE clie WITH vcli
     REPLACE ncta WITH '999'
     REPLACE baja WITH .F.
     UNLOCK
     *Actualizo Archivo del dia para luego usarlo p/actualizar los meses
     *posteriores si tuvieren movimientos
     SELECT MOVIDO
     ADDREC(0)
     RECLOCK(0)
     REPLACE fech WITH DATE()
     REPLACE fdia WITH LEFT(DTOC(DATE()),2)
     REPLACE tcaj WITH 'VD'
     REPLACE fpag WITH IIF(con=1,'EF',IIF(con=2,'CH',IIF(con=3,'US','  ')))
     REPLACE ncta WITH '999'
     REPLACE impo WITH vvue[con]*(-1)
     REPLACE actu WITH .F.
     UNLOCK
     SELECT CAJA
    ENDIF
   NEXT con
   COMMIT
   SELECT MOVIDO
   CLOSE
   *Actualizar Archivo de Cuentas Corrientes
   SELECT CTACTE
   GO reg
   RECLOCK(1)

   REPLACE sact WITH sact - vtot_pag
   IF sact <= 0.1
    REPLACE canc WITH .T.
    cancela= .T.
   ELSE
    cancela= .F.
   ENDIF
   UNLOCK
   *----------------------
   ADDREC(0)
   RECLOCK(0)
   wpago = CERO(VAL(wpago)+1,8)
   REPLACE tcom WITH 'PA'
   REPLACE tipo WITH '0'
   REPLACE clie WITH vcli
   REPLACE nume WITH '99999999'
   REPLACE nint WITH wpago
   REPLACE fech WITH DATE()
   REPLACE paga WITH vtot_pag*(-1)
   REPLACE fpag WITH DATE()
   REPLACE ncom WITH vnum
   REPLACE comt WITH 'FA'
   REPLACE fcom WITH vfec
   REPLACE vend WITH vven
   REPLACE pib1 WITH 0
   REPLACE pib2 WITH 0
   REPLACE cont WITH .T.
   REPLACE sact WITH 0
   REPLACE canc WITH .F.
   REPLACE baja WITH .F.
   REPLACE moro WITH 'C'
   REPLACE dias WITH DMA(vfec,DATE()) &&Dif.de dias en el pago
   UNLOCK
   *----------------------
   *Actualizar Archivo de Facturas Mensuales
   SELECT FACTURA
   SEEK vcli+'FA'+vnum
   IF FOUND()
    RECLOCK(1)
    IF cancela
     REPLACE canc WITH .T.
    ENDIF
    IF wocas#1
     vciv=civa
    ENDIF
    UNLOCK
   ENDIF
   *Actualizo archivo clientes
   IF wocas#1  &&Si no es Cliente Ocasional
    SELECT CLIENTES
    SEEK vcli
    IF FOUND()
     RECLOCK(1)
     REPLACE sact WITH sact - vtot_pag
     IF resto<>0
      REPLACE acre WITH acre + resto
     ENDIF
     UNLOCK
    ENDIF
   ENDIF
   *Actualizo Archivo de CHEQUES
   IF vpago[1]#0
    SELECT CHEQUES
    FOR con=1 TO LEN(chep)
     IF !EMPTY(SUBSTR(chep[con],2,2))
      GO BOTTOM
      ADDREC(1)
      RECLOCK(1)
      *Elige serie de cheques segun el mes de Trabajo A-ENE B-FEB C-MAR
      vser=CHR(64+VAL(LEFT(periodo,2)))
      vnano=RIGHT(periodo,2)
      vint=CERO(VAL(vint)+1,4)
      REPLACE seri WITH vser
      REPLACE nano WITH vnano
      REPLACE nint WITH vint
      REPLACE ntal WITH SUBSTR(chep[con],35,8)
      REPLACE banc WITH SUBSTR(chep[con],2,2)
      REPLACE plaz WITH SUBSTR(chep[con],18,3)
      REPLACE fvto WITH CTOD(SUBSTR(chep[con],46,10))
      REPLACE impo WITH VAL(SUBSTR(chep[con],59,12))
      REPLACE clie WITH vcli
      REPLACE prov WITH SPACE(3)
      REPLACE fent WITH DATE()
      REPLACE fsal WITH CTOD('  /  /    ')
      REPLACE vend WITH vven
      UNLOCK
     ENDIF
    NEXT con
    COMMIT
    *Actualizo archivo AUXILIAR
    IF USAR('AUXILIAR','AUX')
     SELECT AUX
     GO TOP
     RECLOCK(1)
     REPLACE seri WITH vser
     REPLACE nano WITH vnano
     REPLACE nint WITH vint
     REPLACE npcl WITH wpago
     UNLOCK
     COMMIT
     CLOSE
    ENDIF
   ENDIF
  CASE opc1=3 .OR. opc1=0
   IF CARTEL('CONFIRMA CANCELACION [SI] [NO]',22,1,'N',color3)
    SET CURSOR OFF
    IF LASTKEY()=27
     RESTSCREEN(0,0,24,79,pant_ant)
     LOOP
    ENDIF
    CLOSE ALL
    SET ESCAPE ON
    RESTSCREEN(0,0,24,79,pant_ant)
    SETCOLOR(col_ant)
    RETURN
   ELSE
    SET CURSOR OFF
    RESTSCREEN(0,0,24,79,pant_ant)
    LOOP
   ENDIF
 ENDCASE
 IF LASTKEY()=27
  RESTSCREEN(0,0,24,79,pant_ant)
  SETCOLOR(col_ant)
  LOOP
 ENDIF
ENDDO
CLOSE ALL
RESTSCREEN(0,0,24,79,pant_ant)
SETCOLOR(col_ant)
RETURN

*Control de clientes
FUNCTION CON_CLI
PARAMETERS cli,fil,col,colo_sug,lon
IF LASTKEY()=27
 KEYBOARD CHR(27)
 RETURN .T.
ENDIF
PRIVATE ok1
IF PCOUNT()=1
 ok1=0
ELSE
 ok1=1
ENDIF
PRIVATE colo_a,pan_ant
antsel=ALLTRIM(STR(SELECT()))
colo_a=SETCOLOR()
SELECT CLIENTES
SET ORDER TO 1
cli=CERO(VAL(cli),6)
vcli=cli
wocas=0
IF vcli='000000'
 si_no=CARTEL("ES CLIENTE OCASIONAL  [ SI ]  [ NO ]",15,1,'N',color3)
 IF LASTKEY()=27
  SELECT &antsel
  RETURN .F.
 ENDIF
 IF si_no
  wocas=1
 ELSE
  wocas=0
 ENDIF
ENDIF
IF wocas=0
 SEEK cli
 IF .NOT. FOUND()
  pan_ant=savescreen(5,2,24,72)
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
  SETCOLOR(colo_a)
  restscreen(5,2,24,72,pan_ant)
  set order to &ord_ant
  IF LASTKEY()=27
   SELECT &antsel
   RETURN .F.
  ENDIF
 ENDIF
 vcli=clie
 vnom=nomb
 vciv=civa
ELSE
 vnom='CLIENTE OCASIONAL'
 vciv='3'
ENDIF
IF ok1=1
 SETCOLOR(colo_sug)
 @fil,col SAY vcli+' '+SUBSTR(vnom,1,lon)
 SETCOLOR(colo_a)
ENDIF
SELECT &antsel
RETURN .T.

FUNCTION funcus_che
PARAMETERS mod,sub,pos
PRIVATE tcl,vban,nom_ban,vtal,vf_ch,vimp
SET KEY -4 TO
FIL=ROW()
tcl=LASTKEY()
IF !EMPTY(SUBSTR(cheque[sub],2,2))
 vban=SUBSTR(cheque[sub],2,2)
 nom_ban=SUBSTR(cheque[sub],7,9)
 vpza=SUBSTR(cheque[sub],18,3)
 nom_pza=SUBSTR(cheque[sub],24,9)
 vtal=SUBSTR(cheque[sub],35,8)
 vf_ch=CTOD(SUBSTR(cheque[sub],46,10))
 vimp=VAL(SUBSTR(cheque[sub],59,12))
ELSE
 vban=SPACE(2)
 vpza=SPACE(3)
 STORE SPACE(9) TO nom_ban,nom_pza
 vtal=SPACE(8)
 vf_ch=CTOD('  /  /    ')
 vimp=0
ENDIF
vto_che=0
FOR CON=1 TO LEN(cheque)
 IF !EMPTY(SUBSTR(cheque[con],2,2))
  vto_che=vto_che + VAL(SUBSTR(cheque[con],59,12))
 ENDIF
NEXT
@15,61 SAY vto_che PICTURE "@Z 999999999.99"
DO CASE
CASE MOD=0
 RETURN(2)
CASE MOD=1 .OR. MOD=2
 TONE(30,10)
 RETURN(2)
CASE MOD=3
 DO CASE
  CASE tcl=-4
   XD=-1
   RETURN(0)
  CASE tcl=27
   OK=0
   vto_che=0
   FOR CON=1 TO LEN(cheque)
    IF !EMPTY(SUBSTR(cheque[con],2,2))
     vto_che=vto_che + VAL(SUBSTR(cheque[con],59,12))
     OK=6
    ENDIF
   NEXT
   XD=1
   RETURN(0)
  CASE tcl=13
   XPC=5
   XC=1
   SET KEY -4 TO va_tras
   SETCOLOR(color1)
   *al modificar, resto al saldo en cheques el importe del modificado
   IF !EMPTY(SUBSTR(cheque[sub],2,2))
    sal_che=sal_che - VAL(SUBSTR(cheque[sub],59,12))
   ENDIF
   DO WHILE XC<=XPC
    XD=1
    SET ESCAPE ON
    SET CURSOR ON
    DO CASE
     CASE XC=1
      @FIL,4 GET vban PICTURE "@K 99" VALID VAL_BAN(@vban,@nom_ban)
      READ
      @FIL,9 SAY nom_ban
     CASE XC=2
      @FIL,20 GET vpza PICTURE "@K 999" VALID VAL_PZA(@vpza,@nom_pza)
      READ
      @FIL,26 SAY nom_pza
     CASE XC=3
      @FIL,37 GET vtal PICTURE '@K 99999999' VALID VAL_CHE(vban+vpza+vtal,'2',2)
      READ
     CASE XC=4
      @FIL,48 GET vf_ch VALID !EMPTY(vf_ch)
      READ
     CASE XC=5
      @FIL,61 GET vimp PICTURE '@K 999999999.99' VALID IMP_CHE(@vimp)
      READ
    ENDCASE
    IF LASTKEY()=27
     CLEAR GETS
     XC=6
    ELSE
     XC=XC+XD
    ENDIF
   ENDDO
   SET KEY -4 TO
   SET CURSOR OFF
   IF LASTKEY()=27
    *por si modifica y se arrepiente
    IF !EMPTY(SUBSTR(cheque[sub],2,2))
     sal_che=sal_che + VAL(SUBSTR(cheque[sub],59,12))
    ENDIF
    RETURN(2)
   ENDIF
   sal_che=sal_che+vimp
   cheque[sub]=STUFF(cheque[sub],2,2,vban)
   cheque[sub]=STUFF(cheque[sub],7,9,nom_ban)
   cheque[sub]=STUFF(cheque[sub],18,3,vpza)
   cheque[sub]=STUFF(cheque[sub],24,9,nom_pza)
   cheque[sub]=STUFF(cheque[sub],35,8,vtal)
   cheque[sub]=STUFF(cheque[sub],46,10,DTOC(vf_ch))
   cheque[sub]=STUFF(cheque[sub],59,12,TRANSFORM(vimp,'@Z 999999999.99'))
   rel=pos
   auxi=0
   FOR con=1 TO LEN(cheque)
    auxi=auxi+VAL(SUBSTR(cheque[con],59,12))
   NEXT
   IF auxi=vim_ch
    KEYBOARD CHR(27)
   ELSE
    KEYBOARD CHR(24)
   ENDIF
   RETURN(1)
  CASE tcl>=48 .and. tcl<=57
   KEYBOARD CHR(13)+CHR(tcl)
   RETURN(2)
  CASE tcl=7
   sal_che=sal_che - VAL(SUBSTR(cheque[sub],59,12))
   ADEL(cheque,sub)
   cheque[LEN(cheque)] = "    ณ          ณ     ณ          ณ          ณ            ณ              "
   vban=SUBSTR(cheque[sub],2,2)
   nom_ban=SUBSTR(cheque[sub],7,9)
   vpza=SUBSTR(cheque[sub],18,3)
   nom_pza=SUBSTR(cheque[sub],24,9)
   vtal=SUBSTR(cheque[sub],35,8)
   vf_ch=CTOD(SUBSTR(cheque[sub],46,10))
   vimp=VAL(SUBSTR(cheque[sub],59,12))
   rel=pos
   vto_che=0
   FOR CON=1 TO LEN(cheque)
    IF !EMPTY(SUBSTR(cheque[con],2,2))
     vto_che=vto_che + VAL(SUBSTR(cheque[con],59,12))
    ENDIF
   NEXT
   @15,61 SAY vto_che PICTURE "@Z 999999999.99"
   RETURN(1)
 OTHERWISE
  RETURN(2)
 ENDCASE
ENDCASE

FUNCTION IMP_CHE
PARA imp
PRIVATE total
IF imp=0
 RETURN .F.
ENDIF
total=imp+sal_che
IF vim_ch<total
 CARTEL('NO SUPERE IMPORTE EN CHEQUES $ '+STR(vim_ch-sal_che,12,2)+' [OK]',22,1,'N',color3)
 vimp=vim_ch-sal_che
 RETURN .F.
ENDIF
RETURN .T.

*Carga del pago
*cheque=array conteniendo el tamaคo y nombre de cheques de pago ข vuelto
*importe=array conteniendo importes en CH, EF o US
*tgral=sera el total a ingresar
*general=sera el importe total a ingresar y se iran deduciendo los importes
*          ingresados en "importe"
*existe=.T. verifica que el cheque EXISTA en cartera (PAGOS)
*       =.F. verifica que el cheque NO EXISTA en cartera (VUELTO)
*sali=bandera por si se aprieta ESC
FUNCTION PAG_VUE
PARAMETERS cheque,importe,tgral,general,existe,sali
SETCOLOR(color3)
@23,30 SAY " ฑEscฑ-Sale ฑF5ฑ-Campo Anterior "
DIST_COLOR(23,32,'~W+/R~Esc')
DIST_COLOR(23,43,'~W+/R~F5')
PRIVATE vim_ch,sal_che
STORE 0 TO vim_ch,sal_che
AFILL(cheque,"    ณ          ณ     ณ          ณ          ณ            ณ              ")
AFILL(importe,0)
SOMBRA(03,02,14,74,'H2',3,'',color7)
DO MOVCHFAC
DO MOVCONT
XPM=4
XP=1
SET KEY -4 TO VA_TRAS
general=tgral
importe[1]=general
SETCOLOR(color1)
@15,13 SAY vnom
@16,13 SAY SUBSTR(nom_ven,1,25)
@16,51 SAY vnum
DO WHILE XP<=XPM
 XD=1
 SET CURSOR ON
 DO CASE
  CASE XP=1
   sale=0
   *Efectivo en pesos
   general=tgral
   importe[1]=general
   @19,14 GET importe[1] PICTURE '@K 999999999.99'
   READ
   general=general - importe[1]
   importe[2]=general
   IF general<=0 .AND. XD=1
    importe[2]=0
    XP=3
    sale=1
   ENDIF
  CASE XP=2
   sale=0
   *Importe en Cheques
   @19,32 GET importe[2] PICTURE '@K 999999999.99' VALID general >= importe[2]
   READ
   *Importe en dolares
   general=(general - importe[2])
   *Transforma US a PE
   importe[3]=IIF(vdol=0,general,general/vdol)
   IF general<=0 .AND. XD=1
    XP=3
   ENDIF
  CASE XP=3
   IF importe[2]#0
    sale=0
   ELSE
    sale=1
   ENDIF
   @ 19,48 GET importe[3] PICTURE '@Z 999999999'
   READ
   *Transforma PE a US
   general=general - IIF(vdol=0,importe[3],importe[3]*vdol)
  CASE XP=4
   IF importe[2]#0
    vim_ch=importe[2]
    SET CURSOR OFF
    com=1
    rel=0
    sale=0
    KEYBOARD CHR(24)+CHR(5)
    DO WHILE .T.
     com=ACHOICE(6,3,13,73,cheque,.T.,"funcus_che",com,rel)
     SET KEY -4 TO VA_TRAS
     IF LASTKEY()=-4
      XD=-1
      EXIT
     ENDIF
     IF com=0
      EXIT
     ENDIF
    ENDDO
    IF !EMPTY(SUBSTR(cheque[1],2,2))
     sale=1
    ENDIF
   ENDIF
 ENDCASE
 IF LASTKEY()=27 .AND. OK=0
  CLEAR GETS
  sali=0
  XP=5
 ELSE
  XP=XP+XD
 ENDIF
ENDDO
RETURN []

FUNCTION VAL_CHE
PARAMETERS num,vord,opx
IF LASTKEY() = 27
 KEYBOARD CHR(27)
 RETURN .T.
ENDIF
IF opx=2 .AND. !UPDATED()
 RETURN .T.
ENDIF
PRIVATE colo_a,pan_ant,antsel
antsel=ALLTRIM(STR(SELECT()))
colo_a=SETCOLOR()
SELECT CHEQUES
SET ORDER TO &vord
SEEK num
IF FOUND()
 CARTEL("CHEQUE YA EXISTE EN LA CARTERA  [ OK ]",1,21,'N',color3)
 SELECT &antsel
 SETCOLOR(colo_a)
 RETURN .F.
ENDIF
SETCOLOR(colo_a)
SELECT &antsel
RETURN .T.
