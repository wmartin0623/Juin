*Listado MAYOR de Clientes
PARAMETERS p6
PUBLIC iva1,iva2,col_ant,ok,regcli
PRIVATE vmesaj
col_ant=SETCOLOR()
*clientes
AREA()
OPEN_DBF("MAECLIEN","CLIENTES","C","MAECLIEN","clie","NOMCLIEN","nomb")

*-----------------------------------------------------------
IF p6=1
  vmesaj='PENDIENTES'
  *CUENTA CORRIENTE
  AREA()
  IF USAR('CCCLIENT','CTACTE')
   IF !FILE("CCCLIE01.NTX")
    INDEX ON clie+DTOS(fech) TO CCCLIE01
   ENDIF
   SET INDEX TO CCCLIE01
  ELSE
   QUIT
  ENDIF
ELSE
  vmesaj='CANCELADAS'
  *Utilizo Ruta Temporal por si han seleccionado otro directorio desde UTIRUBAK
  Bak = Rut_Tem+"CCCLIBAK.DBF"
  *CUENTA CORRIENTE BACKUP
  AREA()
  IF USAR('&Bak','CTACTE')
   vInd=Rut_Tem + "CCCLIBAK.NTX"
   IF !FILE("&vInd")
    INDEX ON clie+DTOS(fech) TO &vInd
   ENDIF
   SET INDEX TO &vInd
  ELSE
   CLOSE ALL
   RETURN
  ENDIF
ENDIF
*--------------------------------------
PRIVATE pan_ant,vcli,vnom,vsal,vtot,vacr,bien,vMoro
pan_ant=SAVESCREEN(0,0,24,79)
vcli=SPACE(6)
vnom=SPACE(30)
vsal=0
DO WHILE .T.
 vtot=0
 SETCOLOR(color3)
 SOMBRA(04,15,08,51,'H1',1,'',color3)
 @05,18 SAY "Que Cliente : " GET vcli PICTURE "@K 999999" VALID VAL_CLI5(@vcli)
 READ
 IF LASTKEY()=27
  EXIT
 ENDIF
 IF vMoro="I"
  SETCOLOR('*'+color3)
  @4,20 SAY "INACTIVO"
  SETCOLOR(color3)
 ENDIF
 @07,17 SAY vnom
 ok=0
 FECHAS(CTOD("01/01/2000"),DATE())
 IF ok=1
  LOOP
 ENDIF
 @08,17 SAY "Desde: "+DTOC(fe_des)+" Hasta: "+DTOC(fe_has)
 IF CARTEL("DATOS CORRECTOS  [ SI ]  [ NO ]",18,1,'N',colo3)
  IF LASTKEY()=27
   EXIT
  ENDIF
  SOMBRA(18,20,21,33,"H2",1,'',color3)
  IF p6=2
   opc_lis=1
  ELSE
   opc_lis=2
  ENDIF
  @19,22 PROMPT "1-Completo"
  @20,22 PROMPT "2-Depurado"
  MENU TO opc_lis
  SELECT CTACTE
  DO CASE
   CASE LASTKEY()=27 .OR. opc_lis=0
    RESTSCREEN(0,0,24,79,pan_ant)
    LOOP
   CASE opc_lis=1
    *Listado Completo
   CASE opc_lis=2
    *Listado Depurado
  ENDCASE
  vtot=0
  o1=0
  SET ORDER TO 1
  SEEK vcli
  IF !FOUND().AND.vacr=0
   CARTEL("CLIENTE CON SALDO 0(cero)   [ OK ]",14,1,'N',color3)
   RESTSCREEN(0,0,24,79,pan_ant)
   LOOP
  ELSE
   IF !FOUND().AND.vacr#0
    CARTEL("CLIENTE CON SALDO A FAVOR = "+STR(vacr,12,2)+" $  [ OK ]",10,1,'N',colo3)
    RESTSCREEN(0,0,24,79,pan_ant)
    LOOP
   ENDIF
  ENDIF
  bien = CARTEL("Enviamos a un Archivo ? [ SI ] [ NO ]",18,2,'N',colo3)
  IF !bien .OR. LASTKEY() = 27
   IF !IMPRI()
    o1=1
   ENDIF
   IF LASTKEY()#27
    SET ORDER TO 1
    SEEK vcli
    IF o1=1
     DO may_imp
    ELSE
     DO may_pan
    ENDIF
   ENDIF
  ELSE
   IF CREA_ARCH(vcli)
    SELECT CTACTE
    SET ORDER TO 1
    SEEK vcli
    DO May_Arch
   ENDIF
  ENDIF
  RESTSCREEN(0,0,24,79,pan_ant)
 ELSE
  LOOP
 ENDIF
ENDDO
RESTSCREEN(0,0,24,79,pan_ant)
CLOSE ALL
SETCOLOR(col_ant)
RETURN

PROCEDURE may_pan
LOCAL ok,Con1,Hoja,vSalAnt,xSalAnt,hor,Debe,Habe,vTip,vNUm,vFec,vTpa
LOCAL vNpa,vCont,Tecla
ok=0
con1=0
hoja=1
vSalAnt=0
xSalAnt=0
DO WHILE .NOT.EOF().AND.vcli=clie
 SETCOLOR(color7)
 @1,0 CLEAR TO 24,79
 DO PAN_MAYOCL
 SETCOLOR(color3)
 @1,0 SAY "MAYOR DE : "+vcli+' '+ALLTRIM(vnom)+' '+"De: "+DTOC(fe_des)+" a: "+DTOC(fe_has)+'   '
 SETCOLOR('*'+color3)
 @1,70 SAY vmesaj
 IF vMoro="I"
  @2,70 SAY "CL-INACTIVO"
 ENDIF
 SETCOLOR(color1)
 hor=4
 IF p6 # 1
  @hor,0 SAY "Datos Leidos desde: "+Rut_Tem
  hor=hor+1
 ENDIF
 IF hoja=1
  hoja=2
  IF vacr<>0
   vtot=vtot+(vacr*(-1))
   @hor,1 SAY ' AC'
   @hor,6 SAY 'Resto de'
   @hor,15 SAY 'FACTURA'
   @hor,57 SAY vtot PICTURE "@Z 9999999.99"
   @hor,68 SAY vtot PICTURE "9999999.99"
   SETCOLOR(color3)
   @23,68 SAY vtot PICTURE "9999999.99"
   SETCOLOR(color1)
   hor=hor+1
  ENDIF
 ENDIF
 DO WHILE .NOT.EOF() .AND. hor<=21.AND.vcli=clie
  IF fech>=fe_des.AND.fech<=fe_has
   IF opc_lis=2.AND.canc
    SKIP
    LOOP
   ENDIF
   IF baja
    SKIP
    LOOP
   ENDIF
   IF tcom = "RT"
    SKIP
    LOOP
   ENDIF
  ELSE
   IF fech<fe_des .AND. clie=vcli.and.xSalAnt=0
    IF opc_lis=2.AND.canc
     SKIP
     LOOP
    ELSE
     IF baja
      SKIP
      LOOP
     ENDIF
     IF tcom = "RT"
      SKIP
      LOOP
     ENDIF
     *Acumula saldo Anterior
     IF paga>=0
      debe=paga
      habe=0
     ELSE
      debe=0
      habe=paga
     ENDIF
     vSalAnt=vSalAnt+debe+habe
    ENDIF
   ENDIF
   SKIP
   LOOP
  ENDIF
  vtip=tcom
  vnum=ltra+srie+nume
  vfec=fech
  vtpa=comt
  IF ncom='--------'
   vnpa=lcom+scom+defa+lcom+scom+hafa
  ELSE
   vnpa=lcom+scom+ncom
  ENDIF
  IF paga>=0
   debe=paga
   habe=0
  ELSE
   debe=0
   habe=paga
  ENDIF
  IF cont
   con1=1
   vcont='*'
  ELSE
   vcont=' '
  ENDIF
  IF xSalAnt=0
   @hor,40 SAY "Saldo Anterior: "
   @hor,68 SAY vSalAnt PICTURE "9999999.99"
   hor=hor+1
   xSalAnt=1
   vtot=vtot+vSalAnt
  ENDIF
  vtot=vtot+debe+habe
  @hor,0 SAY vcont+vtip
  @hor,4 SAY vnum
  @hor,18 SAY DTOC(vfec)
  @hor,29 SAY vtpa
  IF LEN(vnpa)>13
   SETCOLOR(color9)
   @hor,31 SAY vnpa
   SETCOLOR(color1)
  ELSE
   @hor,32 SAY vnpa
   @hor,46 SAY debe PICTURE "@Z 9999999.99"
  ENDIF
  @hor,57 SAY habe PICTURE "@Z 9999999.99"
  @hor,68 SAY vtot PICTURE "9999999.99"
  SETCOLOR(color3)
  @23,68 SAY vtot PICTURE "9999999.99"
  SETCOLOR(color1)
  SKIP
  hor=hor+1
 ENDDO
 IF .NOT.EOF()
  IF xSalAnt=0.AND.vSalAnt#0
   @hor,40 SAY "Saldo Anterior: "
   @hor,68 SAY vSalAnt PICTURE "9999999.99"
   xSalAnt=1
   hor=hor+1
  ENDIF
  SETCOLOR(color3)
  @23,20 SAY " ฑEscฑ-Sale  ฑฤูฑ-Continua"
  SETCOLOR(color7)
  DIST_COLOR(23,22,'~W+/R~Esc')
  DIST_COLOR(23,34,'~W+/R~ฤู')
  IF con1=1
   DIST_COLOR(24,5,'~W+/R~* - Factura Contado')
  ENDIF
  SET CURSOR OFF
  tecla=65
  DO WHILE tecla#13 .AND. tecla#27
   tecla=INKEY(0)
   IF tecla=-9
    CALCULATOR()
   ENDIF
  ENDDO
  IF tecla=27
   ok=1
   EXIT
  ENDIF
  @23,20 SAY SPACE(30)
  SET CURSOR ON
 ENDIF
ENDDO
IF ok=0
  IF xSalAnt=0.AND.vSalAnt#0
   @hor,40 SAY "Saldo Anterior: "
   @hor,68 SAY vSalAnt PICTURE "9999999.99"
   hor=hor+1
  ENDIF
  SETCOLOR(color3)
  @23,20 SAY " ฑEscฑ-Termina "
  SETCOLOR(color7)
  DIST_COLOR(23,22,'~W+/R~Esc')
  IF con1=1
   DIST_COLOR(24,5,'~W+/R~* - Factura Contado')
  ENDIF
  SET CURSOR OFF
  tecla=65
  DO WHILE tecla#27
   tecla=INKEY(0)
   IF tecla=-9
    CALCULATOR()
   ENDIF
  ENDDO
  @23,20 SAY SPACE(30)
  SET CURSOR ON
ENDIF
RETURN

PROCEDURE may_imp
LOCAL Con1,ok,Pagina,vSalAnt,xSalAnt,hor,Hoja
LOCAL vTip,vNum,vFec,vTpa,vNpa,vCont,Debe,Habe,qTot,qDebe,qHabe
@1,0 CLEAR TO 24,79
@12,15 SAY 'ESPERE..... ESTOY IMPRIMIENDO'
SET CONSOLE OFF
SET PRINT ON
SELECT CTACTE
con1=0
ok=0
pagina=0
hoja=1
vSalAnt=0
xSalAnt=0
?? chr(27)+chr(120)+chr(0)+chr(18)
?? chr(27)+chr(67)+chr(72)
?? CHR(27)+CHR(33)+CHR(5)
DO WHILE !EOF() .AND. INKEY(.1) # 27.AND.vcli=clie
 pagina=pagina+1
 NORMA()
 GRAN_ON()
 ? SUBSTR(nom_emp+SPACE(25),1,25)
 GRAN_OFF()
 NORMA()
 CHICO()
 ?? SPACE(23)+'Pagina Nro. : '+STR(pagina,3)
 NORMA()
 GRAN_ON()
 ?SUBSTR('MAYOR DE '+vnom+SPACE(31),1,31)
 GRAN_OFF()
 NORMA()
 CHICO()
 ??SPACE(3)+'Fecha       : '+STUFF(DTOC(DATE()),3,4,'-'+MESES(DATE())+'-')
 NORMA()
 ? "Codigo Cliente : "+vcli+IIF(vMoro="I"," INACTIVO","")+SPACE(5)+"Desde: "+DTOC(fe_des)+" Hasta: "+DTOC(fe_has)+SPACE(5)+"Comprobantes "+vmesaj
 IF p6 # 1
  ? "Datos Leidos desde: "+Rut_Tem
 ENDIF
 ? '=Comprobante================= =Rec.Pagado======= ==Importes============================'
 ? 'Tipo  Numero         Fecha    Tipo    Numero        Debe         Haber        Saldo    '
 ? '==== ============= ========== ==== ============= ============ ============ ============'
 hor=6+IIF(p6#1,1,0)
 IF hoja=1
  hoja=2
  IF vacr<>0
   vtot=vtot+(vacr*(-1))
   ? ' AC  Resto de FACTURA'+SPACE(41)+STR(vtot,12,2)+' '+STR(vtot,12,2)
   hor=hor+1
  ENDIF
 ENDIF

 DO WHILE .NOT.EOF() .AND. hor<=66 .AND. INKEY(.1) # 27.AND.vcli=clie
  IF fech>=fe_des.AND.fech<=fe_has
   IF opc_lis=2.AND.canc
    SKIP
    LOOP
   ENDIF
   IF baja
    SKIP
    LOOP
   ENDIF
   IF tcom = "RT"
    SKIP
    LOOP
   ENDIF
  ELSE
   IF fech<fe_des .AND. xSalAnt=0
    IF opc_lis=2.AND.canc
     SKIP
     LOOP
    ELSE
     IF baja
      SKIP
      LOOP
     ENDIF
     IF tcom = "RT"
      SKIP
      LOOP
     ENDIF
     *Acumulaa saldo Anterior
     IF paga>=0
      debe=paga
      habe=0
     ELSE
      debe=0
      habe=paga
     ENDIF
     vSalAnt=vSalAnt+debe+habe
    ENDIF
   ENDIF
   SKIP
   LOOP
  ENDIF
  vtip=tcom
  vnum=ltra+srie+nume
  vfec=fech
  vtpa=comt
  IF ncom='--------'
   vnpa=lcom+scom+defa+'/'+lcom+scom+hafa
  ELSE
   vnpa=lcom+scom+ncom
  ENDIF
  IF paga>=0
   debe=paga
   habe=0
  ELSE
   debe=0
   habe=paga
  ENDIF
  IF cont
   con1=1
   vcont='*'
  ELSE
   vcont=' '
  ENDIF
  IF xSalAnt=0
   ? SPACE(58)+'Saldo Anterior : '+STR(vSalAnt,12,2)
   hor=hor+1
   xSalAnt=1
   vtot=vtot+vSalAnt
  ENDIF
  vtot=vtot+debe+habe
  qtot=STR(vtot,12,2)
  *Para NC de varias CP
  IF LEN(vnpa)>13
   qdebe=SPACE(3)
  ELSE
   qdebe=IIF(debe=0,SPACE(12),STR(debe,12,2))
  ENDIF
  qhabe=IIF(habe=0,SPACE(12),STR(habe,12,2))
  ? vcont+vtip+'  '+vnum+' '+DTOC(vfec)+'  '+vtpa+'  '+vnpa+' '+qdebe+' '+qhabe+' '+qtot
  hor=hor+1
  SKIP
 ENDDO
 IF LASTKEY()=27
  EXIT
 ENDIF
 IF vcli=clie
  IF con1=1
   ? SPACE(20)+'[ * ] - Factura Contado No Pagada'
  ENDIF
  EJECT
 ELSE
  IF xSalAnt=0.AND.vSalAnt#0
   ? SPACE(58)+'Saldo Anterior : '+STR(vSalAnt,12,2)
   vtot=vSalAnt
   xSalAnt=1
  ENDIF
  ?
  ? SPACE(58)+'**** SALDO *** : '+STR(vtot,12,2)
  ?
  IF con1=1
   ? SPACE(20)+'[ * ] - Factura Contado No Pagada'
  ENDIF
 ENDIF
ENDDO
EJECT
?? CHR(27)+CHR(33)+CHR(8)
SET PRINT OFF
RETURN

*Control de clientes
FUNCTION VAL_CLI5
PARAMETERS cli
IF LASTKEY() = 27
 KEYBOARD CHR(27)
 RETURN .T.
ENDIF
PRIVATE colo_a,pan_ant
antsel=ALLTRIM(STR(SELECT()))
colo_a=SETCOLOR()
SELECT CLIENTES
SET ORDER TO 1
cli=CERO(VAL(cli),6)
vcli=cli
IF vcli='000000'
 vnom='CONSUMIDOR OCASIONAL'
 vsal=0
 vacr=0
 regcli=0
 SELECT &antsel
 RETURN .T.
ENDIF
SEEK cli
IF .NOT. FOUND()
 pan_ant=savescreen(5,2,24,72)
 SETCOLOR(color3)
 sombra(5,2,23,71,'V1',3,'',color3)
 @7,2 say'ฬออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน'
 @20,2 say'วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ'
 @17,2 say'ฬออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน'
 SETCOLOR("I")
 @5,38 say' BUSCAR '
 SETCOLOR(color3)
 @18,3 SAY'BUSCAR:'
 @19,3 SAY SPACE(38)
 @21,13 say chr(24)+chr(25)+' PgUp PgDn Mover         '+chr(17)+'ฤู Acepta'
 @22,13 say'F2-Cambia Clave Busqueda  [Esc]-Salir'
 DECLARE cam[3],sep[3],ind[2],lar[2],nom[2]
 carga1(cam,"clie~nomb~domi")
 tit='Codigo  Nombre  Cliente                Domicilio                           '
 carga2(sep,'5,2,0')
 ind[1]=1
 lar[1]=6
 nom[1]='Codigo cliente : '
 ind[2]=2
 lar[2]=30
 nom[2]='Nombre del Cliente : '
 ord_ant=str(INDEXORD())
 SET ORDER TO 2
 GO TOP
 BUSCARE(6,4,16,70,cam,tit,sep,ind,lar,19,4,nom,18,14)
 regis=recno()
 SETCOLOR(colo_a)
 restscreen(5,2,24,72,pan_ant)
 set order to &ord_ant
 IF LASTKEY() <> 27
  vcli=clie
  vnom=nomb
  vsal=sact
  vacr=acre
  vMoro=moro
  regcli=RECNO()
 ELSE
  SELECT &antsel
  RETURN .F.
 ENDIF
ELSE
 vnom=nomb
 vMoro=moro
 vsal=sact
 vacr=acre
 regcli=RECNO()
ENDIF
IF vMoro='I'
 CARTEL("ESTE CLIENTE ESTA ฑINACTIVOฑ [ OK ]",12,1,'N',color3)
ENDIF
SELECT &antsel
RETURN .T.

* mayorcli.FMT
PROCEDURE PAN_MAYOCL
@ 2,0 SAY "ษออComprobanteออออออออออออออหRecibo PagadoอออหอออImportesอออออออออออออออออออออป"
@ 3,0 SAY "ฬTpหอNฃmeroออออออหออFechaอออฮTpฮอNฃmeroออออออฮออDebeออออหออHaberอออหออSaldoอออน"
@ 4,0 SAY "บ  บ             บ          บ  บ             บ          บ          บ          บ"
@ 5,0 SAY "บ  บ             บ          บ  บ             บ          บ          บ          บ"
@ 6,0 SAY "บ  บ             บ          บ  บ             บ          บ          บ          บ"
@ 7,0 SAY "บ  บ             บ          บ  บ             บ          บ          บ          บ"
@ 8,0 SAY "บ  บ             บ          บ  บ             บ          บ          บ          บ"
@ 9,0 SAY "บ  บ             บ          บ  บ             บ          บ          บ          บ"
@10,0 SAY "บ  บ             บ          บ  บ             บ          บ          บ          บ"
@11,0 SAY "บ  บ             บ          บ  บ             บ          บ          บ          บ"
@12,0 SAY "บ  บ             บ          บ  บ             บ          บ          บ          บ"
@13,0 SAY "บ  บ             บ          บ  บ             บ          บ          บ          บ"
@14,0 SAY "บ  บ             บ          บ  บ             บ          บ          บ          บ"
@15,0 SAY "บ  บ             บ          บ  บ             บ          บ          บ          บ"
@16,0 SAY "บ  บ             บ          บ  บ             บ          บ          บ          บ"
@17,0 SAY "บ  บ             บ          บ  บ             บ          บ          บ          บ"
@18,0 SAY "บ  บ             บ          บ  บ             บ          บ          บ          บ"
@19,0 SAY "บ  บ             บ          บ  บ             บ          บ          บ          บ"
@20,0 SAY "บ  บ             บ          บ  บ             บ          บ          บ          บ"
@21,0 SAY "บ  บ             บ          บ  บ             บ          บ          บ          บ"
@22,0 SAY "ฬออสอออออออออออออสออออออออออสออสอออออออออออออสออออออออออฮออออออออออสออออออออออน"
@23,0 SAY "บ                                                       บ TOTAL   :           บ"
@24,0 SAY "ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออสอออออออออออออออออออออผ"
RETURN

FUNCTION CREA_ARCH(fClie)
PRIVATE Nom_Arch,Num_Arch,aEstruct
Num_Arch='001'
DO WHILE .T.
 Nom_Arch = Rut_Lis+"MAYOR"+Num_Arch+".DBF"
 IF !FILE("&Nom_Arch")
  EXIT
 ELSE
  Num_Arch = CERO(VAL(Num_Arch)+1,3)
 ENDIF
ENDDO
aEstruct:={}
AADD(aEstruct,{"DFEC","D",8,0})
AADD(aEstruct,{"HFEC","D",8,0})
AADD(aEstruct,{"CLIE","C",6,0})
AADD(aEstruct,{"NOMB","C",30,0})
AADD(aEstruct,{"TCOM","C",2,0})
AADD(aEstruct,{"LTRA","C",1,0})
AADD(aEstruct,{"SRIE","C",4,0})
AADD(aEstruct,{"NUME","C",8,0})
AADD(aEstruct,{"FECH","D",8,0})
AADD(aEstruct,{"COMT","C",2,0})
AADD(aEstruct,{"LCOM","C",1,0})
AADD(aEstruct,{"SCOM","C",4,0})
AADD(aEstruct,{"NCOM","C",8,0})
AADD(aEstruct,{"IDEB","N",12,2})
AADD(aEstruct,{"IHAB","N",12,2})
AADD(aEstruct,{"ISAL","N",12,2})
AADD(aEstruct,{"CONT","L",1,0})
DBCREATE("&Nom_Arch",aEstruct)
AREA()
IF !USAR("&Nom_Arch","ARCHIVO")
 CARTEL("NO PUDE CREAR EL ARCHIVO PARA ALOJAR EL LISTADO [ OK ]",12,1,'N',color3)
 RETURN .F.
ENDIF
RETURN .T.

PROCEDURE May_Arch
ok=0
con1=0
vSalAnt=0
xSalAnt=0
DO WHILE .NOT.EOF().AND.vcli=clie
 *Datos: rango de fechas, codigo y Nombre del Cliente
 SELECT ARCHIVO
 ADDREC(1)
 RECLOCK(1)
 REPLACE dfec WITH fe_des
 REPLACE hfec WITH fe_has
 REPLACE clie WITH vcli
 REPLACE nomb WITH vnom
 UNLOCK
 IF vacr<>0
  vtot=vtot+(vacr*(-1))
  *resto de Facturas
  SELECT ARCHIVO
  ADDREC(1)
  RECLOCK(1)
  REPLACE tcom WITH 'AC'
  REPLACE nomb WITH 'Resto de Facturas'
  REPLACE ihab WITH vtot
  REPLACE isal WITH vtot
  UNLOCK
 ENDIF
 SELECT CTACTE
 DO WHILE .NOT.EOF() .AND.vcli=clie
  IF fech>=fe_des.AND.fech<=fe_has
   IF opc_lis=2.AND.canc
    SKIP
    LOOP
   ENDIF
   IF baja
    SKIP
    LOOP
   ENDIF
   IF tcom = "RT"
    SKIP
    LOOP
   ENDIF
  ELSE
   IF fech<fe_des .AND. clie=vcli.and.xSalAnt=0
    IF opc_lis=2.AND.canc
     SKIP
     LOOP
    ELSE
     IF baja
      SKIP
      LOOP
     ENDIF
     IF tcom = "RT"
      SKIP
      LOOP
     ENDIF
     *Acumula saldo Anterior
     IF paga>=0
      debe=paga
      habe=0
     ELSE
      debe=0
      habe=paga
     ENDIF
     vSalAnt=vSalAnt+debe+habe
    ENDIF
   ENDIF
   SKIP
   LOOP
  ENDIF
  vtcom=tcom
  vnume=nume
  vltra=ltra
  vsrie=srie
  vfech=fech
  vcomt=comt
  vlcom=lcom
  vscom=scom
  vncom=ncom
  IF paga>=0
   debe=paga
   habe=0
  ELSE
   debe=0
   habe=paga
  ENDIF
  IF cont
   vcont=.T.
  ELSE
   vcont=.F.
  ENDIF
  IF xSalAnt=0
   *Saldo Anterior
   SELECT ARCHIVO
   ADDREC(1)
   RECLOCK(1)
   REPLACE tcom WITH 'SA'
   REPLACE nomb WITH 'Saldo Anterior'
   REPLACE isal WITH vSalAnt
   UNLOCK
   xSalAnt=1
   vtot=vtot+vSalAnt
  ENDIF
  vtot=vtot+debe+habe
  SELECT ARCHIVO
  ADDREC(1)
  RECLOCK(1)
  REPLACE tcom WITH vtcom
  REPLACE ltra WITH vltra
  REPLACE srie WITH vsrie
  REPLACE nume WITH vnume
  REPLACE fech WITH vfech
  REPLACE comt WITH vcomt
  REPLACE lcom WITH vlcom
  REPLACE scom WITH vscom
  REPLACE ncom WITH vncom
  REPLACE ideb WITH debe
  REPLACE ihab WITH habe
  REPLACE isal WITH vtot
  REPLACE cont WITH vcont
  UNLOCK
  SELECT CTACTE
  SKIP
 ENDDO
 IF .NOT.EOF()
  IF xSalAnt=0.AND.vSalAnt#0
   *Saldo Anterior
   SELECT ARCHIVO
   ADDREC(1)
   RECLOCK(1)
   REPLACE tcom WITH 'SA'
   REPLACE nomb WITH 'Saldo Anterior'
   REPLACE isal WITH vSalAnt
   UNLOCK
   SELECT CTACTE
   xSalAnt=1
  ENDIF
 ENDIF
ENDDO
IF ok=0
  IF xSalAnt=0.AND.vSalAnt#0
   *Saldo Anterior
   SELECT ARCHIVO
   ADDREC(1)
   RECLOCK(1)
   REPLACE tcom WITH 'SA'
   REPLACE nomb WITH 'Saldo Anterior'
   REPLACE isal WITH vSalAnt
   UNLOCK
   SELECT CTACTE
  ENDIF
ENDIF
RETURN
