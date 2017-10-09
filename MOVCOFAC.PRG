*Consulta y baja de Facturacion
#INCLUDE "INKEY.CH"
#INCLUDE "SETCURS.CH"
SET KEY 306 TO MUES_CBTE
PUBLIC iva1,iva2,pb1,pb2,p1,p2,coli_ant,ok,REJNO,nc,vti_bus
PRIVATE coli_ant,pan_ant,fEstruc[1],Arch_Esp
pan_ant=SAVESCREEN(0,0,24,79)
coli_ant=SETCOLOR()
Arch_Esp=""
*Porcentaje de iva
AREA()
IF USAR('PORCEIVA')
 iva1=IVAI
 iva2=IVANI
ELSE
 QUIT
ENDIF
*clientes
AREA()
OPEN_DBF("MAECLIEN","CLIENTES","C","MAECLIEN","clie","NOMCLIEN","nomb")

*Rubros
AREA()
OPEN_DBF("MAERUBRO","RUBRO","C","MAERUBRO","rubr","NOMRUBRO","nomb")

*Proveedores
AREA()
OPEN_DBF("MAEPROVE","PROVE","C","MAEPROVE","prov","NOMPROVE","nomb")

*marcas
AREA()
OPEN_DBF("MAEMARCA","MARCA","C","MAEMARCA","mrca","NOMMARCA","nomb")

*Articulos
AREA()
IF USAR('MAEARTIC','ARTIC')
 SET RELATION TO prov INTO PROVE, TO rubr INTO RUBRO, TO mrca INTO MARCA
 *Por Rubro,Proveedor
 IF !FILE('MAEARTIC&vRDD.')
  INDEX ON rubr+prov TO MAEARTIC
 ENDIF
 *Por Rubro, Proveedor y Marca
 IF !FILE('MAEARTI1&vRDD.')
  INDEX ON rubr+prov+mrca TO MAEARTI1
 ENDIF
 *Por proveedor, Marca
 IF !FILE('MAEARTI2&vRDD.')
  INDEX ON prov+mrca TO MAEARTI2
 ENDIF
 *Por Marca
 IF !FILE('MAEARTI3&vRDD.')
  INDEX ON mrca TO MAEARTI3
 ENDIF
 *Por rubro, Marca
 IF !FILE('MAEARTI4&vRDD.')
  INDEX ON rubr+mrca TO MAEARTI4
 ENDIF
 *Por rubro y Articulo
 IF !FILE('MAEARTI5&vRDD.')
  INDEX ON rubr+arti TO MAEARTI5
 ENDIF
 IF !FILE('NOMARTIC&vRDD.')
  INDEX ON nomb TO NOMARTIC
 ENDIF
 IF !FILE('MAEARTI6&vRDD.')
  INDEX ON arti TO MAEARTI6
 ENDIF
 SET INDEX TO MAEARTIC,MAEARTI1,MAEARTI2,MAEARTI3,MAEARTI4,MAEARTI5,NOMARTIC,MAEARTI6
ELSE
 QUIT
ENDIF

*-----------------------------------------------------------------------
*Opcion de Canceladas o pendientes
IF opc2=2
 *Consultas
 SOMBRA(14,46,17,61,'V1',1,'',INVER,'N+')
 SETCOLOR(color2)
 @15,48 PROMPT'1- Pendientes'MESS`                Permite Listar Mayor con Comprobantes PENDIENTES                 '
 @16,48 PROMPT'2- Canceladas'MESS`                Permite Listar Mayor con Comprobantes CANCELADOS                 '
 MENU TO p6
 IF LASTKEY()=27.OR.p6=0
  RESTSCREEN(0,0,24,79,PAN_ANT)
  SETCOLOR(coli_ant)
  CLOSE ALL
  SET KEY 306 TO
  RETURN
 ENDIF
ELSE
 *Bajas
 p6=1
ENDIF
DO CASE
 CASE p6=1
  vmesaj='PENDIENTES'
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
   RESTSCREEN(0,0,24,79,PAN_ANT)
   SETCOLOR(coli_ant)
   CLOSE ALL
  ENDIF
 CASE p6=2
  vmesaj='CANCELADAS'
  opc2=2        &&Consultas
  Bak = Rut_Tem+"CCCLIBAK.DBF"
  Ind1= Rut_Tem+"CCCLIBA1.NTX"
  Ind2= Rut_Tem+"CCCLIBAK.NTX"
  Ind3= Rut_Tem+"CCCLIBA2.NTX"
  *CUENTA CORRIENTE BACKUP
  AREA()
  IF USAR('&bak','CTACTE')
   IF !FILE('&Ind1')
    INDEX ON clie+tcom+ltra+srie+nume TO &Ind1
   ENDIF
   IF !FILE("&Ind2")
    INDEX ON clie+DTOS(fech) TO &Ind2
   ENDIF
   IF !FILE('&Ind3.NTX')
    INDEX ON tcom+ltra+srie+nume TO &Ind3
   ENDIF
   SET INDEX TO &Ind1,&Ind2,&Ind3
  ELSE
   RESTSCREEN(0,0,24,79,PAN_ANT)
   SETCOLOR(coli_ant)
   CLOSE ALL
   SET KEY 306 TO
   RETURN
  ENDIF
ENDCASE
*-----------------------------------------------------------------------
SET RELATION TO clie INTO CLIENTES

SET DECIMAL TO 2
PRIVATE vltra,vsrie,vnum
PRIVATE vtip,nom_tip,vcli,vnom,vciv,vcct,wocas,vsal
PRIVATE vfec,pan_trn,com,rel,vbaja,vFpag
PRIVATE vtot,mens1,mens2,vpor,dsto,vpre,vcan,vrem,vRem_Fac
PRIVATE tgral,iva,reciva,vnet,vcta,vreg,tdes,vncom,articu
PRIVATE vOrigen      &&="F"-Facturacion   "E"=Especial
PRIVATE wPeri   &&Mes y aคo Factura
*wocas=1 Cliente Ocasional
*wocas=0 Cliente Normal

*Para remitos
PRIVATE vbul,vdbul,existe

*Objeto TBROWSE
PRIVATE oTAB,ClaveGen

SETCOLOR(color7)
@ 24,0 CLEAR
DO MOVCLFAC
SETCOLOR(color3)
IF opc2=2
 @5,24 SAY "CONSULTAS "+vmesaj
ELSE
 IF opc2=3
  @5,24 SAY "   B A J A S     "
 ENDIF
ENDIF
PAN_TRA=SAVESCREEN(0,0,24,79)
vti_b=CARTEL("BUSQUEDA COMPROBANTES POR  [ CLIENTES ]   [ Nง CBTE. ]",22,1,'N',color3)
IF LASTKEY()=27
 RESTSCREEN(0,0,24,79,PAN_ANT)
 SETCOLOR(coli_ant)
 CLOSE ALL
 SET KEY 306 TO
 RETURN
ENDIF
IF vti_b
 vti_bus=0
ELSE
 vti_bus=1
ENDIF
DO WHILE .T.
 wPeri=SPACE(4)
 vOrigen="F"
 vtip=SPACE(1)
 vart=SPACE(3)
 vcli=SPACE(6)
 vdom=SPACE(25)
 vcui=SPACE(11)
 vltra=' '
 vsrie=SPACE(4)
 vnum=SPACE(8)
 vnom=SPACE(30)
 vciv=SPACE(1)
 vRem=SPACE(8)
 vRem_Fac=SPACE(8)
 vbaja=.F.
 vFpag=" "
 STORE 0 TO wocas,vtot,dsto,vcan,vpre,vtota,tgral,iva,reciva,vnet
 conta=.F.
 articu=.F.
 STORE SPACE(30) TO mens1,mens2
 MItems:={}
 vFVto=CTOD("  /  /    ")
 SETCOLOR(color1)
 XPM=2
 XP=1
 SET KEY -4 TO VA_TRAS
  MItems:={}
 DO WHILE XP<=XPM
  XD=1
  SET CURSOR ON
  DO CASE
   CASE XP=1
    IF vti_bus=0
     @ 2,1 GET vcli PICTURE '@k 999999' VALID BUSCLI(@vcli,2,1,color1,25)
     READ
    ELSE
     @ 3,45 GET vtip PICTURE "9" VALID TIPCOMP(@vtip)
     @ 4,50 GET vltra PICTURE "!"
     @ 4,52 GET vsrie PICTURE "@K 9999"
     @ 3,50 GET vnum PICTURE "@K 99999999" VALID BUSCFAC(nom_tip,vltra,@vsrie,@vnum)
     READ
    ENDIF

   CASE XP=2
    SETCOLOR('+'+color1)
    @20,16 SAY 'Descuento'
    IF vbaja
     SETCOLOR("*"+color3)
     @6,58 SAY "Comprob.ANULADO"
    ENDIF
    SETCOLOR(color9)
    @2,34 SAY IIF(vciv='1','R.I. ',IIF(vciv='2','R.N.I',IIF(vciv='3','C.F. ',IIF(vciv='4','EXEN.',IIF(vciv='5','M.T. ',;
    IIF(vciv='6','N.A.',IIF(vciv='7','N.C.',SPACE(5))))))))
    @2,39 SAY IIF(vcct='1','C.C',IIF(vcct='2','CDO',SPACE(3)))
    @2,1 SAY vcli+' '+SUBSTR(vnom,1,25)
    @3, 9 SAY IIF(vFpag="1",'Contado',IIF(vFpag="2",'30 dias','60 dias'))
    @ 3,45 SAY vtip+' '+nom_tip
    @ 3,50 SAY vnum
    @ 4,50 SAY vltra+'-'+vsrie
    @ 3,71 SAY vrem
    @ 3,59 SAY vfec
    @ 4,59 SAY vFVto
    IF vncom <> '00000000'
     @5,51 SAY "Sobre Fact.Nง "+vncom
    ENDIF

    IF !EMPTY(vRem_Fac)
     @6,51 SAY "Sobre Remito Nง "+vRem_Fac
    ENDIF
    @20,2 SAY vtot PICTURE "@Z 999999999.99"
    @20,15 SAY tdes PICTURE '@Z 999999999.99'
    @20,28 SAY vnet PICTURE '@Z 999999999.99'
    @19,48 SAY STR(iva1,5,2)
    @20,41 SAY iva PICTURE '@Z 999999999.99'
    @19,61 SAY STR(iva2,5,2)
    @20,54 SAY reciva PICTURE '@Z 999999999.99'
    @24, 2 SAY ALLTRIM(mens1)+' / '+mens2
    @22,28 SAY ''
    DISPOUT(STR(vsal,12,2),IIF(vsal>1000,color3,color9))
    SETCOLOR(color3)
    mens='  A Pagar '
    IF vtip='2'
     mens=`Acreditado'
    ENDIF
    @18,68 SAY mens
    @20,67 SAY tgral PICTURE '@Z 999999999.99'
    SETCOLOR(color1)
    SET CURSOR OFF
    com=1
    rel=0
    KEYBOARD CHR(24)+CHR(5)
    DO WHILE .T.
     com=ACHOICE(7,1,14,78,MItems,.T.,"funcus_con",com,rel)
     SET KEY -4 TO va_tras
     IF LASTKEY()=-4
      XD=-1
      EXIT
     ENDIF
     IF com=0
      EXIT
     ENDIF
    ENDDO
  ENDCASE
  IF LASTKEY()=27
   CLEAR GETS
   XP=3
  ELSE
   XP=XP+XD
  ENDIF
 ENDDO
 SET KEY -4 TO
 IF LASTKEY()=27 .OR. XP>5
  CLOSE ALL
  RESTSCREEN(0,0,24,79,pan_ant)
  SETCOLOR(coli_ant)
  FERASE("&Arch_Esp")
  SET KEY 306 TO
  RETURN
 ENDIF
 RESTSCREEN(0,0,24,79,PAN_TRA)
ENDDO
CLOSE ALL
RESTSCREEN(0,0,24,79,PAN_ANT)
SETCOLOR(coli_ant)
FERASE("&Arch_Esp")
SET KEY 306 TO
RETURN

FUNCTION funcus_con
PARAMETERS MOD,sub,pos
PRIVATE tcl
SET KEY -4 TO
tcl=LASTKEY()
*Imprime detalles del Articulo
fSubt = VAL(SUBSTR(MItems[sub],79,8)) * VAL(SUBSTR(MItems[sub],87,9))
@16,2 SAY SUBSTR(MItems[sub],139,4) COLOR color9
@16,7 SAY SUBSTR(MItems[sub],124,15) COLOR color9
@16,23 SAY SUBSTR(MItems[sub],106,15) COLOR color9
@16,39 SAY SUBSTR(MItems[sub],96,10) COLOR color9
@16,50 SAY SUBSTR(MItems[sub],79,8) COLOR color9
@16,59 SAY SUBSTR(MItems[sub],87,9) COLOR color9
@16,70 SAY fSubt PICTURE "@Z 999999.99" COLOR color9
DO CASE
CASE MOD=0
 RETURN(2)
CASE MOD=1 .OR. MOD=2
 TONO(2)
 RETURN(2)
CASE MOD=3
 DO CASE
  CASE tcl=-4
   XD=-1
   RETURN(0)
 CASE tcl=27          &&Presiona [ESC] para terminar
  SETCOLOR(color3)
  scr1=SAVESCREEN(19,10,24,25)
  SOMBRA(19,11,23,23,'H2',1,'',color3)
  ok=0
  IF opc2=3.AND.!vbaja
   @ 20,12 PROMPT '  B A J A  '
   ok=1
  ENDIF
  @ 22,12 PROMPT ' CANCELAR  '
  MENU TO opc1
  IF ok=0
   opc1=2
  ENDIF
  ok=0
  SETCOLOR(color1)
  RESTSCREEN(19,10,24,25,scr1)
  DO CASE
   CASE opc1=1
    IF CARTEL('CONFIRMA BAJA COMPROBANTE [SI] [NO]',22,2,'N',color3)
     SET CURSOR OFF
     IF LASTKEY()=27
      RETURN(2)
     ENDIF
     DO anular
    ELSE
     SET CURSOR OFF
     RETURN(2)
    ENDIF
    RETURN(0)
   CASE opc1=2 .OR. opc1=0
    IF CARTEL('CONFIRMA CANCELACION [SI] [NO]',22,1,'N',color3)
     SET CURSOR OFF
     IF LASTKEY()=27
      RETURN(2)
     ENDIF
     RETURN(0)
    ELSE
     SET CURSOR OFF
     RETURN(2)
    ENDIF
  ENDCASE
 OTHERWISE
  RETURN(2)
 ENDCASE
ENDCASE

*Control de clientes
FUNCTION BUSCLI
PARAMETERS cli,fil,col,colo_sug,lon
IF LASTKEY()=27
 KEYBOARD CHR(27)
 RETURN .T.
ENDIF
PRIVATE colo_a,pan_ant,si_no
IF PCOUNT()=1
 ok1=0
ELSE
 ok1=1
ENDIF
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
SEEK cli
IF wocas=0
 IF .NOT. FOUND()
  pan_ant=savescreen(5,2,24,72)
  SETCOLOR(color3)
  sombra(5,2,23,71,'V1',3,'',color3)
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
   vnom=nomb
   vciv=civa
   vsal=sact
   regcli=RECNO()
   IF ok1=1
    SETCOLOR(colo_sug)
    @fil,col SAY vcli+' '+SUBSTR(vnom,1,lon)
    SETCOLOR(colo_a)
   ENDIF
  ELSE
   SELECT &antsel
   RETURN .F.
  ENDIF
 ELSE
  vnom=nomb
  vciv=civa
  vsal=sact
  regcli=RECNO()
  IF ok1=1
   SETCOLOR(colo_sug)
   @fil,col SAY vcli+' '+SUBSTR(vnom,1,lon)
   SETCOLOR(colo_a)
  ENDIF
 ENDIF
ELSE
 vnom='CLIENTE OCASIONAL'
 vsal=0
ENDIF
SELECT CTACTE
SEEK vcli
IF FOUND()
  IF opc2=2     &&Consultas
   Condi="!baja.AND.(tcom='FA' .OR. tcom='TK' .OR. tcom='ND' .OR. tcom='NC'.OR. tcom='RT'.OR. tcom='CH')"
  ELSE
   IF opc2=3   &&Bajas
    Condi='!baja.AND.(IIF(tcom="FA".OR.tcom="CH".OR.tcom="TK",'
    Condi=condi+         'IIF(!cont,paga=sact,.T.),'
    Condi=Condi+          'IIF(tcom="ND".OR.tcom="NC",'
    Condi=Condi+           'IIF(!canc,.T.,.F.),.F.'
    Condi=condi+             ')'
    Condi=Condi+        ')'
    Condi=Condi+    ')'
   ENDIF
  ENDIF
  De_Fes=DATE()
  Ha_fes=DATE()
  ok=0
  PRIVATE GetList:={}
  FECHAS(De_Fes,Ha_Fes)
  IF ok=1
   SET FILTER TO
   SETCOLOR(colo_a)
   SELECT &antsel
   RETURN .F.
  ENDIF
  Condi=condi+".AND.fech>=Fe_Des.AND.fech<=Fe_Has"
ELSE
 CARTEL("CLIENTE no Tiene Comprobantes p/Consultar o Anular [ OK ]",20,1,'N',color3)
 SET FILTER TO
 SETCOLOR(colo_a)
 SELECT &antsel
 RETURN .F.
ENDIF
SELECT CTACTE
DBSETORDER(2)
SET FILTER TO &Condi
GO TOP
SEEK vcli

IF !FOUND()
 CARTEL("CLIENTE no Tiene Comprobantes p/Consultar o Anular [ OK ]",20,1,'N',color3)
 SET FILTER TO
 SETCOLOR(colo_a)
 SELECT &antsel
 RETURN .F.
ENDIF
Pan_Car=SAVESCREEN(6,6,24,76)

      PRIVATE oTAB
      ClaveGen=vCli
      oTab:=BROWSE2(6,6,23,75,ClaveGen)
      DO WHILE .NOT. oTab:STABILIZE()
      ENDDO
      oTAB:DEHILITE()
      ***** BUCLE PRINCIPAL **********
      SETCOLOR(color3)
      DO WHILE .T.
       DO WHILE NEXTKEY()==0 .AND. .NOT. oTAB:STABILIZE()
       ENDDO
       oTAB:COLORRECT({oTAB:ROWPOS,1,oTAB:ROWPOS,oTAB:COLCOUNT}, {5,1})
       IF oTAB:HITTOP .OR. oTAB:HITBOTTOM
         TONE(2000)
       ENDIF
       TECLA:=INKEY(0)
       IF TECLA==K_ESC
         SET FILTER TO
         SELECT &antsel
         SETCOLOR(colo_a)
         RESTSCREEN(6,6,24,76,Pan_Car)
         RETURN .F.
        ELSE
         IF TECLA==K_ENTER
          EXIT
         ELSE
          CUALTCL2(TECLA)
         ENDIF
       ENDIF
       oTAB:REFRESHCURRENT()
      ENDDO
      oTAB:DEHILITE()
      oTAB:REFRESHCURRENT()

ok=0
DO carga_fac
RESTSCREEN(6,6,24,76,Pan_Car)
SETCOLOR(colo_a)
SELECT &antsel
IF ok=1
 CARTEL("NO ENCUENTRO UNO DE LOS ARCHIVOS MENSUALES  [OK]",17,1,'N',color3)
 RETURN .F.
ELSE
 RETURN .T.
ENDIF

FUNCTION BUSCFAC
PARAMETERS tip,ltr,sri,num
IF LASTKEY()=27
 KEYBOARD CHR(27)
 RETURN .T.
ENDIF
PRIVATE colo_a,pan_ant,antsel
antsel=ALLTRIM(STR(SELECT()))
colo_a=SETCOLOR()
IF VAL(num)=0
 RETURN .F.
ENDIF
SELECT CTACTE
SET ORDER TO 3
sri=CERO(VAL(sri),4)
num=CERO(VAL(num),8)
SEEK tip+ltr+sri+num
IF !FOUND()
 SET ORDER TO 3
 SET FILTER TO tcom=tip
 GO TOP
 pan=SAVESCREEN(5,26,24,78)
 SETCOLOR(color3)
 SOMBRA(5,26,23,77,'H1',1,'',color3)
 SETCOLOR(color3)
 @ 7,26  SAY 'ฬออออออออออออออออออออออออออออออออออออออออออออออออออน'
 @ 21,26 SAY 'ฬออออออออออออออออออออออออออออออออออออออออออออออออออน'
 @ 5,29 SAY ' BUSCAR COMPROBANTES '
 @ 22,27 SAY CHR(24)+CHR(25)+' PgUp PgDn mover          '+CHR(17)+'ฤู-ACEPTA'
 DECLARE cam[6],sep[6]
 carga1(cam,"IIF(baja,'An',SPACE(2))~IIF(cont,'Cdo.','C.C.')~tcom~ltra+srie+nume~DTOC(fech)~TRANSFORM(paga,'999999999.99')")
 carga2(sep,'2,1,1,1,1,0')
 titul1='Es Forma Tp  Nฃmero         Fecha     Importe    '
 ord_ant=STR(INDEXORD())
 SETCOLOR(color3)
 BUSCARE(6,28,20,76,cam,titul1,sep)
 SETCOLOR(colo_a)
 SET ORDER TO &ord_ant
 RESTSCREEN(5,26,24,78,pan)
 SET FILTER TO
 IF LASTKEY()=27
  SELECT &antsel
  RETURN .F.
 ENDIF
ENDIF
IF paga>0.AND.paga#sact.AND.Opc2=3   &&Baja
 CARTEL("COMPROBANTE CON PAGOS PARCIALES [ OK ]",12,1,'N',color3)
 SELECT &AntSel
 RETURN .F.
ENDIF
ok=0
DO carga_fac
SELECT &antsel
IF ok=1
 CARTEL("NO ENCUENTRO UNO DE LOS ARCHIVOS MENSUALES  [OK]",17,1,'N',color3)
 RETURN .F.
ELSE
 SELECT CLIENTES
 IF vcli='000000'
  vsal=0
  vnom='CLIENTE OCASIONAL'
  wocas=1
 ELSE
  SEEK vcli
  IF FOUND()
   vcli=clie
   vnom=nomb
   vciv=civa
   vsal=sact
   regcli=RECNO()
  ENDIF
 ENDIF
 SELECT &antsel
 RETURN .T.
ENDIF
RETURN .T.

PROCEDURE carga_fac
SELECT CTACTE
antsel=ALLTRIM(STR(SELECT()))
vcta=RECNO()
vcli=clie
vltra=ltra
vsrie=srie
vnum=nume    &&Numero del Comprobante
vRem_Fac = nrem
vFVto=fvto
vFpag = cpag
vtip=tipo
nom_tip=tcom
wwfe=DTOC(fech)
vMes1 = MONTH(fech)
vAno1 = YEAR(fech)
wperi=CHR(vmes1+64)+CERO(vano1,4)
IF opc2=3  &&Baja
 *controlar que el libro de iva no este cerrado
 AREA()
 IF USAR("LIBIVACL","LIBCL")
  IF !FILE("LIBIVACL.NTX")
   INDEX ON nmes+nano TO LIBIVACL
  ENDIF
  SET INDEX TO LIBIVACL
  SEEK SUBSTR(wwfe,4,2)+RIGHT(wwfe,4)
  IF FOUND() .AND. cerr
   CARTEL("LIBRO IVA DE ESTE MES YA FUE CERRADO [ OK ]",12,1,'N',color3)
   ok=1
   CLOSE
   SELECT &AntSel
   RETURN
  ENDIF
 ENDIF
 CLOSE
ENDIF

SELECT &AntSel

IF SELECT("FACTURA") <> 0
 SELECT FACTURA
 CLOSE
ENDIF
IF SELECT("DETALLE") <> 0
 SELECT DETALLE
 CLOSE
ENDIF
* IF SELECT("CAJA") <> 0
*  SELECT CAJA
*  CLOSE
* ENDIF

*FACTURAS
AREA()
IF USAR("FCC&wperi","FACTURA")
 IF !FILE('FCC&wperi..NTX')
  INDEX ON clie+tcom+ltra+srie+nume TO FCC&wperi
 ENDIF
 wn=wperi+'1'
 IF !FILE('FC&wn..NTX')
  INDEX ON DTOS(fech) TO FC&wn
 ENDIF
 SET INDEX TO FCC&wperi,FC&wn
ELSE
 ok=1
 RETURN
ENDIF

*DETALLES
AREA()
IF USAR("FCD&wperi","DETALLE")
 IF !FILE('FCD&wperi..NTX')
  INDEX ON clie+tcom+ltra+srie+nume TO FCD&wperi
 ENDIF
 SET INDEX TO FCD&wPeri
ELSE
 ok=1
 RETURN
ENDIF
*SET RELATION TO arti INTO ARTIC

IF opc2#2
 *CAJA
*    AREA()
*    IF USAR("CAJ&wperi","CAJA",'E')
*     IF !FILE('CAJ&wperi..NTX')
*      INDEX ON tcom+ltra+srie+nume+fpag TO CAJ&wperi
*     ENDIF
*     SET INDEX TO CAJ&wperi
*    ELSE
*     ok=1
*     RETURN
*    ENDIF
ENDIF

SELECT FACTURA
SEEK vcli+nom_tip+vltra+vsrie+vnum
IF FOUND()
 vOrigen="F"
 vreg=RECNO()
 vrem=nrem
 vfec=fech
 dsto=desc
 tdes=ides
 iva1=prin
 iva2=prni
 iva=irin
 reciva=irni
 vnet=ngra
 vtot=stot
 tgral=paga
 vciv=civa
 vcct=IIF(cont,'2','1')
 mens1=obs1
 mens2=obs2
 vbaja=baja
 reg_nc=RECNO()
 IF tcom='NC'
  SELECT CTACTE
  vncom=ncom
  SELECT FACTURA
 ELSE
  vncom='00000000'
 ENDIF
 IF tcom='FA'.OR.tcom='NC' .OR. tcom = 'RT' .OR. tcom = 'TK'
  SELECT DETALLE
  SEEK vcli+nom_tip+vltra+vsrie+vnum
  IF FOUND()
   pp=1
   DO WHILE .NOT.EOF().AND.clie=vcli.AND.tcom=nom_tip.AND.nume=vnum;
            .AND.ltra=vltra.AND.srie=vsrie
    AADD(MItems,SPACE(171))
    MItems[LEN(MItems)]=STUFF(MItems[LEN(MItems)],1,78,nomb)
    MItems[LEN(MItems)]=STUFF(MItems[LEN(MItems)],143,5,arti)
    MItems[LEN(MItems)]=STUFF(MItems[LEN(MItems)],139,4,rubr)
    MItems[LEN(MItems)]=STUFF(MItems[LEN(MItems)],79,8,TRANSFORM(cant,'@Z 99999.99'))
    MItems[LEN(MItems)]=STUFF(MItems[LEN(MItems)],87,9,TRANSFORM(puni,'@Z 99999.999'))
    MItems[LEN(MItems)]=STUFF(MItems[LEN(MItems)],148,3,mone)
    MItems[LEN(MItems)]=STUFF(MItems[LEN(MItems)],151,8,TRANSFORM(dola,'@Z 999.9999'))
    MItems[LEN(MItems)]=STUFF(MItems[LEN(MItems)],159,12,TRANSFORM(ptot,'@Z 999999999.99'))
    art=arti
    SELECT ARTIC
    SEEK art
    IF FOUND()
     vMrca = mrca
     MItems[LEN(MItems)]=STUFF(MItems[LEN(MItems)],121,3,mrca)
     MItems[LEN(MItems)]=STUFF(MItems[LEN(MItems)],96,10,unid)
     MItems[LEN(MItems)]=STUFF(MItems[LEN(MItems)],106,15,medi)
     SELECT MARCA
     SEEK vMrca
     IF FOUND()
      MItems[LEN(MItems)]=STUFF(MItems[LEN(MItems)],124,15,nomb)
     ENDIF
    ENDIF
    SELECT DETALLE
    SKIP
    pp=pp+1
   ENDDO
   nc = 0
   articu=.T.   &&Se usa para actualizar Archivo DETAmmaa en la baja
  ELSE
   AADD(MItems,SPACE(171))
   MItems[LEN(MItems)]=STUFF(MItems[LEN(MItems)],1,78,SUBSTR(ALLTRIM(mens1)+' '+mens2+SPACE(78),1,78))
   MItems[LEN(MItems)]=STUFF(MItems[LEN(MItems)],79,8,TRANSFORM(0,'@Z 99999.99'))
   MItems[LEN(MItems)]=STUFF(MItems[LEN(MItems)],87,9,TRANSFORM(vTot,'@Z 99999.999'))
   MItems[LEN(MItems)]=STUFF(MItems[LEN(MItems)],148,3,'$  ')
   MItems[LEN(MItems)]=STUFF(MItems[LEN(MItems)],151,8,TRANSFORM(0,'@Z 999.9999'))
   MItems[LEN(MItems)]=STUFF(MItems[LEN(MItems)],159,12,TRANSFORM(vTot,'@Z 999999999.99'))
   nc=1
  ENDIF
 ELSE
   AADD(MItems,SPACE(171))
   MItems[LEN(MItems)]=STUFF(MItems[LEN(MItems)],1,78,mens1+' '+mens2+SPACE(17))
   MItems[LEN(MItems)]=STUFF(MItems[LEN(MItems)],79,8,TRANSFORM(0,'@Z 99999.99'))
   MItems[LEN(MItems)]=STUFF(MItems[LEN(MItems)],87,9,TRANSFORM(vTot,'@Z 99999.999'))
   MItems[LEN(MItems)]=STUFF(MItems[LEN(MItems)],148,3,'$  ')
   MItems[LEN(MItems)]=STUFF(MItems[LEN(MItems)],151,8,TRANSFORM(0,'@Z 999.9999'))
   MItems[LEN(MItems)]=STUFF(MItems[LEN(MItems)],159,12,TRANSFORM(vTot,'@Z 999999999.99'))

 ENDIF
ELSE
 SELECT CTACTE
 vOrigen="E"
 IF tcom='NC'
  vncom=ncom
 ELSE
  vncom='00000000'
 ENDIF
 vrem=SPACE(8)
 vfec=fech
 dsto=0
 tdes=0
 iva1=0
 iva2=0
 iva=0
 reciva=0
 vnet=0
 vtot=0
 tgral=paga
 vciv=CLIENTES->civa
 vcct=IIF(cont,'2','1')
 mens1="Cbte. Cargado por ESPECIAL    "
 vbaja=baja
 AADD(MItems,SPACE(171))
 MItems[LEN(MItems)]=STUFF(MItems[LEN(MItems)],1,78,mens1+SPACE(48))
 MItems[LEN(MItems)]=STUFF(MItems[LEN(MItems)],79,8,TRANSFORM(0,'@Z 99999.99'))
 MItems[LEN(MItems)]=STUFF(MItems[LEN(MItems)],87,9,TRANSFORM(paga,'@Z 99999.999'))
 MItems[LEN(MItems)]=STUFF(MItems[LEN(MItems)],148,3,'$  ')
 MItems[LEN(MItems)]=STUFF(MItems[LEN(MItems)],151,8,TRANSFORM(0,'@Z 999.9999'))
 MItems[LEN(MItems)]=STUFF(MItems[LEN(MItems)],159,12,TRANSFORM(paga,'@Z 999999999.99'))
 CARTEL("NO ENCUENTRO comprobante EN ARCHIVO MENSUAL  [ OK ]",20,1,'N',color3)
 SETCOLOR(colo_a)
ENDIF
RETURN

PROCEDURE anular
*Actualizo Cta.Cte

SELECT CTACTE
GO vcta
RECLOCK(1)
REPLACE baja WITH .T.
UNLOCK
IF vtip='2'.AND.vOrigen="F"
 wn=comt
 wn1=lcom
 wn2=scom
 wn3=ncom
 SET ORDER TO 1
 SEEK vcli+wn+wn1+wn2+wn3
 IF FOUND()
  RECLOCK(1)
  REPLACE sact WITH  sact+ABS(tgral)
  REPLACE canc WITH .F.
  UNLOCK
 ELSE
  CARTEL("ATENCION... No puedo SUMAR saldo a FACTURA [ OK ]",12,1,'N',color3)
 ENDIF
ENDIF
*Actualizo Archivo Comprobantes Mensuales
IF vOrigen="F"
 SELECT FACTURA
 GO vreg
 RECLOCK(1)
 REPLACE baja WITH .T.
 REPLACE fbaj WITH DATE()
 UNLOCK
ENDIF
*Actualizo Cliente
IF wocas#1      &&Si no es cliente Ocasional
 IF vcct='1'.OR.vcct='2'
  SELECT CLIENTES
  SEEK vcli
  IF FOUND()
   RECLOCK(1)
   REPLACE sact WITH sact - tgral
   UNLOCK
  ENDIF
 ENDIF
ENDIF
*Devolver Stock
IF vOrigen="F"
 IF articu
  SELECT DETALLE
  SEEK vcli+nom_tip+vltra+vsrie+vnum
  IF FOUND()
   wreg=RECNO()
   DO WHILE clie=vcli .AND. tcom=nom_tip .AND. nume=vnum;
            .AND. ltra=vltra .AND. srie=vsrie
    RECLOCK(1)
    REPLACE baja WITH .T.
    UNLOCK
    SKIP
   ENDDO
  ENDIF
  IF wreg<>0
   GO wreg
   DO WHILE clie=vcli .AND. tcom=nom_tip .AND. nume=vnum;
            .AND. ltra=vltra .AND. srie=vsrie
    vart=arti
    vcan=cant
    SELECT ARTIC
    SEEK vart
    IF FOUND()
     RECLOCK(1)
     IF nom_tip='NC'
      REPLACE stck WITH stck - vcan
     ELSE
      REPLACE stck WITH stck + vcan
     ENDIF
     UNLOCK
    ENDIF
    SELECT DETALLE
    SKIP
   ENDDO
  ENDIF
 ENDIF
ENDIF
*Actualizo Archivo de Caja
*SELECT CAJA
*REPLACE ALL baja WITH .T. FOR tcom = nom_tip .AND. nume = vnum;
*           .AND. ltra=vltra .AND. srie=vsrie
RETURN

FUNCTION TIPCOMP
PARAMETERS tip
ok=0
IF LASTKEY()=27
 KEYBOARD CHR(27)
 RETURN .T.
ENDIF
PRIVATE papan,colo_ant,are_ant
are_ant=ALLTRIM(STR(SELECT()))
colo_ant=SETCOLOR()
SETCOLOR(color3)
papan=SAVESCREEN(9,14,14,30)
IF tip<>'1' .AND. tip<>'2' .AND. tip<>'3'.AND.tip#'4'.AND.tip#'5'
 SOMBRA(9,14,14,29,'H1',1,'',color3)
 ti=1
 @10,16 PROMPT "1- FACTURA  "
 @11,16 PROMPT "2- N.CREDITO"
 @12,16 PROMPT "3- N.DEBITO "
 @13,16 PROMPT "4- TICKET   "
 @13,16 PROMPT "5- REMITOS  "
 MENU TO ti
 SETCOLOR(colo_ant)
 IF LASTKEY()=27
  CLEAR GETS
  KEYBOARD CHR(27)
  RESTSCREEN(9,14,14,30,papan)
  RETURN .T.
 ENDIF
 vtip=STR(ti,1)
ELSE
 vtip=tip
ENDIF
SETCOLOR(colo_ant)
RESTSCREEN(9,14,14,30,papan)
nom_tip=IIF(vtip='1','FA',IIF(vtip='2','NC',IIF(vTip="3",'ND',IIF(vTip="4",'TK',"RT"))))
@ 3,45 SAY vtip+' '+nom_tip
RETURN .T.

PROCEDURE MUES_CBTE
PRIVATE F_Clie,F_NCli,F_Fech,F_Pan,F_Col,F_Are,F_Cam,F_Per
*Para dejar activo el GET que llama Genero Uno nuevo
PRIVATE GetList := {}
F_Pan=SAVESCREEN(0,0,24,79)
F_Col=SETCOLOR()
F_Are=ALLTRIM(STR(SELECT()))
F_CLie=SPACE(6)
F_NCli=SPACE(30)
F_Cam="CLIE~NOMB"
F_Per=SPACE(4)
SETCOLOR(colo3)
DO WHILE .T.
 SOMBRA(10,10,13,70,"H1",1,'',colo3)
 IF SELECT("F_DETA")#0
  SELECT F_DETA
  CLOSE
 ENDIF
 @11,11 SAY "Qu Cliente: " GET F_Clie PICTURE "@K 999999" VALID VALIDA(@F_CLie,@F_NCli,"CLIENTES",F_Cam,6,30,"CLIE","NOMB")
 @12,11 SAY "Mes y Aคo  : " GET F_Per PICTURE "@R 99-99"
 READ
 IF LASTKEY()=27
  RESTSCREEN(0,0,24,79,F_Pan)
  SETCOLOR(F_Col)
  SELECT &F_Are
  ULTECLA(42)
  RETURN
 ENDIF
 *FACTURAS
 IF FILE("FACL&F_Per..DBF")
  AREA()
  IF USAR("FACL&F_Per","F_DETA")
   IF !FILE('FACL&F_Per..NTX')
    INDEX ON clie+tcom+ltra+srie+nume TO FACL&F_Per
   ENDIF
   SET INDEX TO FACL&F_Per
  ENDIF
 ELSE
  CARTEL("NO EXISTE ARCHIVO PARA ESTE MES Y AฅO [ OK ]",12,1,'N',color3)
  LOOP
 ENDIF
 SET RELATION TO clie+tcom+ltra+srie+nume INTO CTACTE
 F_CLie=CERO(VAL(F_Clie),6)
 SEEK F_Clie
 IF FOUND()
  SET FILTER TO clie=F_Clie .AND. CTACTE->sact > 0 .AND. !BAJA
  GO TOP
  SETCOLOR(color3)
  SOMBRA(5,1,23,78,'V1',3,'',color3)
  @ 7,1 say'ฬออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน'
  @21,1 say'ฬออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน'
      *      12 12345678 123 123456789012 1234567890123456789012345 123456789012345678901
      *     12345678901234567890123456789012345678901234567890123456789012345678901234567890
  @5,10 say' CBTES. de ('+F_Clie+') - '+ALLTRIM(LOWER(F_NCli))+'  ['+MESES(CTOD('01/'+LEFT(F_Per,2)+'/'+RIGHT(F_Per,2)))+'-'+RIGHT(F_Per,2)+']'
  SETCOLOR(color3)
  @22,13 SAY CHR(24)+CHR(25)+' PgUp PgDn Mover      Esc - Salir   '
  DECLARE cam[6],sep[6]
  CARGA1(cam,"tcom~nume~STRZERO(DAY(fech),2)+' '~STR(CTACTE->sact,12,2)~SUBSTR(obs1,1,25)~SUBSTR(obs2,1,21)")
  tit='Tp Nฃmero   Dกa Importe      Observaciones                                  '
*      12 12345678 123 123456789012 1234567890123456789012345 123456789012345678901
  CARGA2(sep,'1,1,1,1,1,0')
  GO TOP
  BUSCARE(6,2,20,77,cam,tit,sep)
  SET FILTER TO
  SETCOLOR(F_Col)
  RESTSCREEN(0,0,24,79,F_Pan)
  SETCOLOR(F_Col)
 ENDIF
ENDDO

FUNCTION BROWSE2(nLINT,nCOLT,nLINB,nCOLB,ClaveGen)
LOCAL con
*SOMBRA(nLINT-1,nCOLT-1,nLINB+1,nCOLB+1,"H2",1,'',color7)
oTAB:=TBROWSEDB(nLINT,nCOLT,nLINB,nCOLB)     //CREA EL OBJETO oTAB
oTAB:COLORSPEC:=color3
oTAB:HEADSEP:=CHR(196)+CHR(194)+CHR(196)
oTAB:COLSEP:= chr(32) +CHR(179)+chr(32)
oTAB:FOOTSEP:=CHR(196)+CHR(193)+CHR(196)

*Crear Columnas
oTAB:ADDCOLUMN(TBCOLUMNNEW("Tipo",{|| tcom}))
oTAB:ADDCOLUMN(TBCOLUMNNEW("Nฃmero",{|| ltra+srie+nume}))
oTAB:ADDCOLUMN(TBCOLUMNNEW("Fecha",{|| fech}))
oTAB:ADDCOLUMN(TBCOLUMNNEW("Importe",{|| paga}))
oTAB:ADDCOLUMN(TBCOLUMNNEW("Tp ",{|| IIF(cont,"Cdo","CC.")}))
oTAB:ADDCOLUMN(TBCOLUMNNEW("Estado",{|| IIF(baja,"Anulado",SPACE(7))}))
oTAB:GOTOPBLOCK={|| INICFRA2(ClaveGen) }
oTAB:GOBOTTOMBLOCK:={|| FINFRA2(ClaveGen)}
oTAB:SKIPBLOCK:={|nPARAMOVER| MUEVE2(nPARAMOVER,ClaveGen)}
RETURN(oTAB)

FUNCTION INICFRA2(ClaveGen)   //DETERMINA EL PRIMER REGISTRO DE LA FRANJA
 DBSEEK(ClaveGen)
RETURN NIL

FUNCTION FINFRA2(ClaveGen)   //DETERMINA EL ULTIMO
**Liga SOFTSEEK (PARA CLAVE MAYOR DE 1 POSICION) y aumenta el valor de la
**clave aumentando el valor de su ฃltimo byte. Por ejemplo, si la ฃltima letra
**fuera "A", se transformarกa en "B"
IF LEN(ClaveGen)=1
  DBSEEK(ULTIMA2(ClaveGen), .F.)
  DBSKIP(-1)
  RETURN ({|| DBSEEK(ULTIMA2(ClaveGen), .F.), DBSKIP(-1)})
ELSE
  DBSEEK(ULTIMA2(ClaveGen), .T.)
  DBSKIP(-1)
  RETURN ({|| DBSEEK(ULTIMA2(ClaveGen),.T.),DBSKIP(-1)})
ENDIF

FUNCTION ULTIMA2(ClaveGen)
 LOCAL ULTIMOREG
 ULTIMOREG := STUFF(ClaveGen, LEN(ClaveGen), 1, ;
                CHR( ASC(RIGHT(ClaveGen,1)) + 1 ))
 RETURN (ULTIMOREG)

FUNCTION MUEVE2( nPARAMOVER, ClaveGen )
LOCAL nMOVIDOS
nMOVIDOS := 0
IF nPARAMOVER == 0 .OR. LASTREC() == 0
  DBSKIP(0)
 ELSEIF nPARAMOVER > 0 .AND. RECNO() != LASTREC() + 1
   DO WHILE nMOVIDOS <= nPARAMOVER .AND. ! EOF() .AND. ;
    LEFT(&(INDEXKEY(0)),6) == ClaveGen              //AVANZA REGISTROS
    DBSKIP(1)
    nMOVIDOS++
   ENDDO
   DBSKIP(-1)        //SIEMPRE DEVUELVE UN REGISTRO PUES EL DO WHILE TERMINA
   nMOVIDOS--        //CUANDO ESTAMOS UN REGISTRO ADELANTE FUERA DE LA FRANJA
 ELSEIF nPARAMOVER < 0   // MOVER PARA TRAS (PARA INICIO)

   DO WHILE nMOVIDOS >= nPARAMOVER .AND. ! BOF() .AND. ;
     LEFT(&(INDEXKEY(0)),6) == ClaveGen
     DBSKIP(-1)
     nMOVIDOS--
   ENDDO
   IF ! BOF()            //SIEMPRE DEVUELVE UN REGISTRO PUES EL DO WHILE
      DBSKIP(1)          //TERMINA CUANDO ESTAMOS FUERA DE LA FRANJA SALVO
   ENDIF                 //EN EL CASO DE NO SER INICIO DE ARCHIVO
   nMOVIDOS++
ENDIF
RETURN (nMOVIDOS)

FUNCTION CUALTCL2(TECLA)
  DO CASE
        CASE TECLA==K_UP
           oTAB:UP()
        CASE TECLA==K_DOWN
           oTAB:DOWN()
        CASE TECLA==K_LEFT         ;oTAB:LEFT()
        CASE TECLA==K_CTRL_LEFT    ;oTAB:PANLEFT()
        CASE TECLA==K_RIGHT        ;oTAB:RIGHT()
        CASE TECLA==K_CTRL_RIGHT   ;oTAB:PANRIGHT()
        CASE TECLA==K_PGUP         ;oTAB:PAGEUP()
        CASE TECLA==K_CTRL_PGUP    ;oTAB:GOTOP()
        CASE TECLA==K_PGDN         ;oTAB:PAGEDOWN()
        CASE TECLA==K_CTRL_PGDN    ;oTAB:GOBOTTOM()
        CASE TECLA==K_HOME         ;oTAB:HOME()
        CASE TECLA==K_CTRL_HOME    ;oTAB:PANHOME()
        CASE TECLA==K_END          ;oTAB:END()
        CASE TECLA==K_CTRL_END     ;oTAB:PANEND()
        OTHERWISE
         TONE(125) ; TONE(300)
  ENDCASE
