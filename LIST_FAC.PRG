*Listado de Boletas de Facturas, N.Creditos y N.Debitos
PRIVATE pant_ant,col_ant,bmes,bano,vcli,vnom,vFbar,vPci,vDpt
PRIVATE vtip,nom_tip,vnum,vltra,vsrie
PRIVATE vciv,vcct
PRIVATE bru[2],bru1[12],bru2[12],por[2],bru3[2]
PRIVATE vfec,pan_trn,com,rel,campos[12],VDESC[12],vbaja
PRIVATE vtot,mens1,mens2,vven,nom_ven,vpor,dsto,vpre,vcan,vrem
PRIVATE tgral,iva,reciva,vnet,ng1,ng2,tdes,fact_nc,ltra_nc,srie_nc,conta,bRG212
*Factura de credito
PRIVATE vfemi,vfvto,vobse,viant,vnum_fcre,fcre_exi
*Percepcion
PRIVATE vper,vmul,percep,multa
*Resolucion 12/02 Percepcion Ing.Bruto
PRIVATE vPR122,vIR122
PUBLIC regcli,pb1,pb2

*Para Remitos
PRIVATE vbul,vcbul,vigu,vdbul,exi_bul,exi_rem,nro_reparto
PRIVATE vrem,vfec_rem,vimp,vree,vfac1,vfac2,vfac3,vtr1,vtr2,vtr_fl1,vtr_fl2
PRIVATE vde1,vde2,vde_fl1,vde_fl2,vco_pag,wpag,vmens
STORE SPACE(30) TO vtr1,vtr2

pant_ant=SAVESCREEN(0,0,24,79)
col_ant=SETCOLOR()
bmes=SPACE(2)
bano=SPACE(4)
AREA()
IF USAR('PORCINBR')
  pb1=inbr1
  pb2=inbr2
ELSE
 QUIT
ENDIF
*ARCHIVO AUXILIAR
IF USAR('AUXILIAR','AUX')
 GO TOP
 PRIVATE xPtoA,xPtoB
 xPtoA=PtoA
 xPtoB=PtoB
ELSE
 QUIT
ENDIF
*PROVINCIAS
OPEN_DBF("MAEPROVI","PROVIN","C","MAEPROVI","pcia","NOMPROVI","nomb")

*DEPARTAMENTOS
AREA()
OPEN_DBF("MAEDEPAR","DEPARTA","C","MAEDEPAR","dpto","NOMDEPAR","nomb")

*clientes
AREA()
OPEN_DBF("MAECLIEN","CLIENTES","C","MAECLIEN","clie","NOMCLIEN","nomb")
SET RELATION TO pcia INTO PROVIN, TO dpto INTO DEPARTA

*ARTICULOS
AREA()
OPEN_DBF("MAEARTIC","ARTICULO","C","MAEARTIC","codi","NOMARTIC","nomb")

*Archivo Consumidores Finales
AREA()
OPEN_DBF("MAECOFIN","CONS_FIN","C","MAECOFIN","tcom+ltra+srie+nume")

*Zonas
AREA()
OPEN_DBF("MAEZONAS","ZONAS","C","MAEZONAS","vend","NOMZONAS","nomb")

DO WHILE .T.
 SETCOLOR(color3)
 @5,25 SAY "LISTADO DE COMPROBANTES"
 SOMBRA(6,16,16,73,'H1',1,'',color3)
 SET FORMAT TO LISFACTU
 READ
 SET FORMAT TO
 *Solicitud de MES Y Aคo para listar boletas
 SETCOLOR(color3)
 @7,39 GET bmes PICTURE '@K 99' VALID VAL(bmes)>=1 .AND. VAL(bmes)<=12
 @9,38 GET bano PICTURE '@K 9999' VALID VAL(bano)>=1994 .AND. VAL(bano)<=2021
 READ
 IF LASTKEY()=27
  RESTSCREEN(0,0,24,79,pant_ant)
  SETCOLOR(col_ant)
  EXIT
 ENDIF
 bmes=CERO(VAL(bmes),2)
 bano=CERO(VAL(bano),4)
 vperi=bmes+RIGHT(bano,2)
 IF .NOT.FILE("FACL&vperi..DBF")
  CARTEL("PERIODO SIN REGISTRACION DE VENTAS  [ OK ]",18,1,'N',color3)
  LOOP
 ENDIF
 *FACTURAS
 AREA()
 IF USAR("FACL&vperi","FACTURA")
  IF !FILE('FACL&vperi..NTX')
   INDEX ON clie+tcom+ltra+srie+nume TO FACL&vperi
  ENDIF
  SET INDEX TO FACL&vperi
 ELSE
  QUIT
 ENDIF
 *DETALLES
 AREA()
 IF USAR("DECL&vperi","DETALLE")
   IF !FILE('DECL&vperi..NTX')
    INDEX ON clie+tcom+ltra+srie+nume TO DECL&vperi
   ENDIF
   SET INDEX TO DECL&vperi
 ELSE
  QUIT
 ENDIF
 SET RELATION TO codi INTO ARTICULO

 *Remitos (Cabecera del Remito)
 AREA()
 OPEN_DBF("REMCLIEN","CAB_REM","C","REMCLIEN","nrem")

 *Condiciones de Pago Remitos
 AREA()
 OPEN_DBF("MAEREMIT","REMIT","C","MAEREMIT","nopc","NOMREMIT","cpag")

 NO_MES(bmes,.T.,7,43)
 DO WHILE .T.
  SETCOLOR(color3)
  XPM=5
  XP=1
  SET KEY -4 TO VA_TRAS
  vcli=SPACE(6)
  vnom=SPACE(30)
  vFbar='N'
  vtip='1'
  vltra=' '
  vsrie=SPACE(4)
  vnum=SPACE(8)
  DO WHILE XP<=XPM
   XD=1
   SET ESCAPE ON
   SET CURSOR ON
   DO CASE
    CASE XP=1
     @11,37 GET vcli PICTURE "@K 999999" VALID VAL_CLI5(@vcli)
     READ
     vcli=CERO(VAL(vcli),6)
     IF LASTKEY()=27
      EXIT
     ENDIF
     @11,37 SAY vcli+' '+vnom
    CASE XP=2
     @13,41 GET vtip PICTURE `9' VALID TIP_VA(@vtip)
     READ
     IF LASTKEY()<>27
      nom_tip=IIF(vtip='1','FA',IIF(vtip='2','NC',IIF(vTip="3",'ND',"RT")))
      SELECT FACTURA
      SEEK vcli+nom_tip
      IF !FOUND()
       CARTEL("CLIENTE NO TIENE COMPROBANTES PARA LISTAR  [ OK ]",22,1,'N',color3)
       XD=-1
      ENDIF
     ENDIF
    CASE XP=3
     *Letra Cbte.
     @15,37 GET vltra PICTURE "!"
     READ
    CASE XP=4
     *Serie Cbte.
     @15,39 GET vsrie PICTURE "@K 9999"
     READ
     vsrie=CERO(VAL(vsrie),4)
    CASE XP=5
     @15,44 GET vnum PICTURE "@K 99999999" VALID FAC_VA(@vnum,vcli,nom_tip,vltra,vsrie)
     READ
     vnum=CERO(VAL(vnum),8)
     @15,37 SAY vltra+' '+vsrie+' '+vnum
   ENDCASE
   IF LASTKEY()=27
    CLEAR GETS
    XP=6
   ELSE
    XP=XP+XD
   ENDIF
  ENDDO
  IF LASTKEY()=27
   SELECT FACTURA
   USE
   SELECT DETALLE
   USE
   EXIT
  ENDIF
  SELECT FACTURA
  STORE SPACE(30) TO descr,mens1,mens2
  AFILL(campos,'                                                                              '+SPACE(5))
  AFILL(VDESC,SPACE(37))
  vltra=ltra
  vsrie=srie
  vnum=nume    &&Numero del Comprobante
  vnum_fcre=fcre
  vven=vend
  LIS_VEN(vven,@nom_ven)
  vrem=nrem
  vfec=fech
  vFVto=fvto
  dsto=desc
  tdes=ides
  iva1=prin
  iva2=prni
  vper=perc    &&pper
  vmul=pmul
  iva=irin
  reciva=irni
*  percep=iper
  percep=iperc
  bRG212=iper
  multa=imul
  vPR122=p122
  vIR122=i122
  vnet = ngra
  ng1=ibr1
  ng2=ibr2
  vtot=stot
  tgral=paga
  vciv=civa
  conta=cont
  mens1=obs1
  mens2=obs2
  por[1]=pib1
  por[2]=pib2
  vbaja=baja
  reg_nc=RECNO()
  IF tcom='NC'
   ltra_nc=lcom
   srie_nc=scom
   fact_nc=ncom
  ELSE
   fact_nc='00000000'
  ENDIF
  IF tcom='FA'.OR.tcom='NC'.OR.tcom="RT"
   SELECT DETALLE
   SEEK vcli+nom_tip+vltra+vsrie+vnum
   IF FOUND()
    pp=1
    DO WHILE .NOT.EOF().AND.clie=vcli.AND.tcom=nom_tip.AND.nume=vnum;
             .AND.ltra=vltra.AND.srie=vsrie
     campos[pp]=STUFF(campos[pp],1,5,codi)
     art=codi
     SELECT ARTICULO
     SEEK art
     IF FOUND()
      gram=ALLTRIM(LEFT(for2,3))+IIF(VAL(SUBSTR(for2,4,3))<>0,'x'+;
           ALLTRIM(SUBSTR(for2,4,3)),'')+IIF(VAL(RIGHT(for2,3))<>0,'x'+;
           ALLTRIM(RIGHT(for2,3)),'')
      produc=gram+` '+trim(nomb)
      VDESC[pp] = SUBSTR(produc+SPACE(37),1,37)
      IF vFbar='S'
       fCbar=Cbar
      ELSE
       fCbar=SPACE(5)
      ENDIF
      IF nibr=`1'
       ib=pb1
      ELSE
       IF nibr=`2'
        ib=pb2
       ENDIF
      ENDIF
      ib1=nibr
      bru1[pp]=ib
      bru2[pp]=ib1
     ELSE
      vdesc[pp]=space(37)
     ENDIF
     campos[pp]=STUFF(campos[pp],7,30,SUBSTR(VDESC[pp],1,30))
     SELECT DETALLE
     campos[pp]=STUFF(campos[pp],38,1,nibr)
     campos[pp]=STUFF(campos[pp],40,7,TRANSFORM(cant,'@Z 9999.99'))
     campos[pp]=STUFF(campos[pp],48,5,TRANSFORM(desc,'@Z 99.99'))
     campos[pp]=STUFF(campos[pp],56,10,TRANSFORM(puni,'@Z 9999999.99'))
     campos[pp]=STUFF(campos[pp],67,12,TRANSFORM(ptot,'@Z 999999999.99'))
     campos[pp]=STUFF(campos[pp],79,5,fCbar)
     SKIP
     pp=pp+1
    ENDDO
    nc=0
   ELSE
    campos[1]=STUFF(campos[1],1,5,'*****')
    campos[1]=STUFF(campos[1],7,30,mens1)
    campos[1]=STUFF(campos[1],38,1,' ')
    campos[1]=STUFF(campos[1],40,7,TRANSFORM(0,'@Z 9999.99'))
    campos[1]=STUFF(campos[1],48,5,TRANSFORM(dsto,'@Z 99.99'))
    campos[1]=STUFF(campos[1],56,10,TRANSFORM(vtot,'@Z 9999999.99'))
    campos[1]=STUFF(campos[1],67,12,TRANSFORM(vtot,'@Z 999999999.99'))
    campos[2]=STUFF(campos[2],1,5,SPACE(5))
    campos[2]=STUFF(campos[2],7,30,mens2)
    nc=1
   ENDIF
  ENDIF
  IF vTip="4"
    SELECT CAB_REM
    SEEK vNum
    IF FOUND()
      exi_rem=.T.
      vfec_rem=fech
      vtr1=tra1
      vtr2=tra2
      vtr_fl1=ftr1
      vtr_fl2=ftr2
      vde1=des1
      vde2=des2
      vde_fl1=fde1
      vde_fl2=fde2
      vimp=impo
      vree=IIF(reem,'S','N')
      vfac1=SUBSTR(nfac,1,8)
      vfac2=SUBSTR(nfac,9,8)
      vfac3=SUBSTR(nfac,17,8)
      vco_pag=cpag
      wPag=nopc
      vmens=obse
    ELSE
      CARTEL("REMITO SIN CABECERA [ OK ]",12,1,'N',color3)
      LOOP
    ENDIF

  ENDIF


  *Anulamos Factura de credito a pedido de ayaime el dia 05-11-97
* *FACTURAS DE CREDITOS
* AREA()
* IF USAR("FACR&vperi","FACRE")
*  IF !FILE('FACR&vperi..NTX')
*   INDEX ON fcre TO FACR&vperi
*  ENDIF
*  SET INDEX TO FACR&vperi
* ELSE
*  QUIT
* ENDIF
* SEEK vnum_fcre
* fcre_exi=.F.
* IF FOUND()
*  vfvto=fvto
*  vfemi=femi
*  viant=iant
*  fcre_exi=.T.
* ELSE
*  STORE CTOD("  /  /    ") TO vfvto,vfemi
*  viannt=0
* ENDIF
* CLOSE
  IF CARTEL("COMENZAMOS EL LISTADO  [ SI ]   [ NO ]",22,1,'N',colo3)
   IF LASTKEY()#27
    DO WHILE .T.
     SET CONSOLE OFF
     IF vLtra="B"
      SET PRINTER TO &xPtoB
     ELSE
      IF vLtra="A"
       SET PRINTER TO &xPtoA
      ELSE
       IF vLtra="X"
         xPtoRem = "LPT3"
         SET PRINTER TO &xPtoRem
       ELSE
         SET PRINTER TO &vNlpt
       ENDIF
      ENDIF
     ENDIF
     IF ISPRINTER()
      SET CONSOLE OFF
      SET PRINT ON
*Anulamos Factura de credito a pedido de ayaime el dia 05-11-97
*      bien=CARTEL("QUE LISTAMOS [ Recibo Factura ] [ Factura de Credito ]",12,1,'N',color3)
*      IF LASTKEY()#27
*       CARTEL("POR FAVOR INSERTE PAPEL ADECUADO [ OK ]",16,1,'N',colo3)
*       IF bien
         IF vTip#"4"
          IF vciv <= '2'
           *Facturacion en 'A'
           CARTEL("POR FAVOR, INSERTE PAPEL CBTE. {A} [ GRACIAS ]",18,1,'N',color3)
           DO Fact_List
          ELSE
           *Facturacion EN 'B'
           CARTEL("POR FAVOR, INSERTE PAPEL CBTE. {B} [ GRACIAS ]",18,1,'N',color3)
           DO Fact_ListB
          ENDIF
         ELSE
           *remitos
           CARTEL("POR FAVOR, INSERTE PAPEL CBTE. {REMITO} [ GRACIAS ]",18,1,'N',color3)
           DO Impr_Remi

         ENDIF
*       ELSE
*        IF fcre_exi
*         DO LIS_FCRED
*        ELSE
*         CARTEL("NO PUDE ENCONTRAR Factura de Credito [ OK ]",16,1,'N',color3)
*         LOOP
*        ENDIF
*       ENDIF
*      ENDIF
      SET PRINT OFF
      SET CONSOLE ON
      EXIT
     ELSE
      ONLINE()
      IF LASTKEY()=27
       EXIT
      ENDIF
      LOOP
     ENDIF
    ENDDO
   ENDIF
  ENDIF
  IF CARTEL("OTRO LISTADO [ SI ]   [ NO ]",20,1,'N',colo3)
   IF LASTKEY()=27
    CLOSE ALL
    RESTSCREEN(0,0,24,79,pant_ant)
    SETCOLOR(col_ant)
    RETURN
   ENDIF
   LOOP
  ELSE
   CLOSE ALL
   RESTSCREEN(0,0,24,79,pant_ant)
   SETCOLOR(col_ant)
   RETURN
  ENDIF
 ENDDO
ENDDO

FUNCTION TIP_VA
PARAMETERS tip
ok=0
IF LASTKEY()=27
 KEYBOARD CHR(27)
 RETURN .T.
ENDIF
PRIVATE papan,colo_ant
colo_ant=SETCOLOR()
SETCOLOR(color3)
papan=SAVESCREEN(9,14,15,30)
IF tip<>'1' .AND. tip<>'2' .AND. tip<>'3'.AND. tip<>'4'
 SOMBRA(9,14,14,29,'H1',1,'',color3)
 ti=1
 @10,16 PROMPT "1- FACTURA  "
 @11,16 PROMPT "2- N.CREDITO"
 @12,16 PROMPT "3- N.DEBITO "
 @13,16 PROMPT "4- REMITO   "
 MENU TO ti
 SETCOLOR(colo_ant)
 IF LASTKEY() = 27
  CLEAR GETS
  KEYBOARD CHR(27)
  RESTSCREEN(9,14,15,30,papan)
  RETURN .T.
 ENDIF
 vtip=STR(ti,1)
ELSE
 vtip=tip
ENDIF
SETCOLOR(colo_ant)
RESTSCREEN(9,14,14,30,papan)
nom_tip=IIF(vtip='1','FA',IIF(vtip='2','NC',IIF(vtip='3','ND','RT')))
@13,41 SAY vtip+' '+nom_tip
RETURN .T.

*Control Numero de factura
FUNCTION FAC_VA
PARAMETERS num,cli,tip,ltr,sri
PRIVATE colo_ant,are_ant,regre
are_ant=ALLTRIM(STR(SELECT()))
colo_ant=SETCOLOR()
SETCOLOR(color3)
SELECT FACTURA
num=CERO(VAL(num),8)
SEEK cli+tip+ltr+sri+num
IF !FOUND()
 SEEK vcli
 IF FOUND()
  p=0
  DECLARE tabla[1]
  DO WHILE clie=vcli
   IF tcom=nom_tip
    carg=.T.
   ELSE
    carg=.F.
   ENDIF
   IF carg
    IF p>0
     ASIZE(tabla,p+1)
    ENDIF
    p=p+1
    wnum=ltra+srie+nume
    wtco=tcom
    wfec=DTOC(fech)
    wpag=paga
    tabla[p]=ltra+srie+nume+' '+tcom+' '+DTOC(fech)+' '+STR(paga,12,2)+' '+IIF(cont,'Cdo.','C.C.')+' '+IIF(baja,'Anula',SPACE(5))
   ENDIF
   SKIP
  ENDDO
 ENDIF
 IF p=0
  CARTEL("CLIENTE no Tiene Comprobantes para LISTAR [ OK ]",20,1,'N',color3)
  SET FILTER TO
  SETCOLOR(colo_ant)
  SELECT &are_ant
  RETURN .F.
 ENDIF
 SET ORDER TO 1
 panant=savescreen(9,25,24,78)
 SETCOLOR(color3)
 sombra(9,25,23,77,'V1',3,'',color3)
 @11,25 say'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออน'
 @21,25 say'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออน'
 @10,26 SAY" Nฃmero       Tp   Fecha      Importe    Tipo Estad"
 SETCOLOR("I")
 @9,33 say' BUSCAR '
 SETCOLOR(color3)
 @22,26 say chr(24)+chr(25)+' PgUp PgDn Mover   '+chr(17)+'ฤู Acepta   [Esc]-Salir'
 **************
 elemento=SCR1(tabla,9,12,26,LEN(tabla),9,25,23,77,1,color1,color3)
 **************
 IF LASTKEY()=13
   vari=vcli+SUBSTR(tabla[elemento],15,2)+SUBSTR(tabla[elemento],1,13)
   SEEK vari
   vltra=SUBSTR(tabla[elemento],1,1)
   vsrie=SUBSTR(tabla[elemento],2,4)
   vnum=SUBSTR(tabla[elemento],6,8)
   IF !FOUND()
    CARTEL("COMPROBANTE NO EXISTE  [ OK ]",22,1,'N',color3)
    SELECT &are_ant
    SETCOLOR(colo_ant)
    RESTSCREEN(9,30,24,78,panant)
    RETURN .F.
   ENDIF
 ELSE
  IF LASTKEY()=27
   SELECT &are_ant
   SETCOLOR(col_ant)
   RESTSCREEN(9,30,24,78,panant)
   RETURN .F.
  ENDIF
 ENDIF
ELSE
 RETURN .T.
ENDIF
SELECT &are_ant
RESTSCREEN(9,25,24,78,panant)
SETCOLOR(colo_ant)
RETURN .T.

PROCEDURE Fact_List
?? CHR(27)+CHR(120)+CHR(0)+CHR(18)
?? CHR(27)+CHR(33)+CHR(5)
?? CHR(27)+CHR(67)+CHR(36)
NORMA()
? CHR(15)+SPACE(51)+CHR(18)+SPACE(34)+'San Rafael, '+DTOC(vfec)
?? CHR(15)
? CHR(15)+SPACE(51)+CHR(18)+SPACE(46)+"Copia"+CHR(15)   &&+'Conv.Mult. 913-534537-0'
*?? CHR(18)
?
IF vtip=`2'
 w=`  Por Factura: '+ltra_nc+srie_nc+fact_nc
ELSE
 w=SPACE(20)
ENDIF
? SPACE(50)+CHR(18)+SPACE(40)+IIF(vtip=`1',`FACT.',iif(vtip=`2',`N.C. ',`N.D. '))+' Nro.: '+vnum
?
IF vcli="000000"
 SELECT CONS_FIN
 SEEK nom_tip+vltra+vsrie+vnum
 IF FOUND()
  vnom=nomb
  vdom=domi
  vcui=cuit
  ciu=""
  provi=""
 ENDIF
ELSE
 SELECT CLIENTES
 GO regcli
 vdom=domi
 vcui=cuit
 ciu=ALLTRIM(DEPARTA->nomb)
 provi=ALLTRIM(PROVIN->nomb)
ENDIF
?SPACE(8)+vcli+' - '+vnom+SPACE(17)+w
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
IF !conta
 IF vTIp="1".OR.vTip="3"
  *Fecha Vencimiento
  ?? CHR(27)+CHR(69)   &&Negrita ON
  ?? ' Vto.:'+SUBSTR(DTOC(vFVto),1,6)+RIGHT(DTOC(vFVto),2)
  ?? CHR(27)+CHR(70)   &&Negrita OFF
 ENDIF
ENDIF
?
? SPACE(56)+"C.Barras"
IF vtip=`1'.OR. (vtip=`2' .AND. nc=0)
 pa=1
ELSE
 pa=2
ENDIF
k=0
IF pa=1
 FOR i=1 TO LEN(campos)
  IF !EMPTY(SUBSTR(campos[i],1,5))
   ? SUBSTR(campos[i],1,5)+'    '+SUBSTR(campos[i],40,7)+'   '+VDESC[i]+' '+SUBSTR(campos[i],79,5)+'   '+;
     SUBSTR(campos[i],48,5)+SPACE(1)+SUBSTR(campos[i],56,10)+'   '+SUBSTR(campos[i],67,12)
   k=k+1
  ENDIF
 NEXT i
ELSE
 FOR i=1 TO LEN(campos)
  IF SUBSTR(campos[i],1,5)='*****'
   ? SPACE(19)+mens1+SPACE(3)+SUBSTR(campos[i],48,5)+space(27)+SUBSTR(campos[i],67,12)
   ? SPACE(19)+mens2
   k=2
  ENDIF
 NEXT i
ENDIF
FOR i=1 TO 12-k
 ?
NEXT i
?
? SPACE(19)+IIF(SUBSTR(campos[1],1,5)<>'*****',ALLTRIM(mens1)+' '+ALLTRIM(mens2),space(1))
? SPACE(5)+STR(vtot,12,2)+SPACE(14)
?? SPACE(15)+STR(tdes,12,2)+SPACE(26)+STR(vnet,12,2)+CHR(15)
* LINEA VIEJA? space(98)+str(por[1],4,2)+` : '+str(ng1*por[1]/100,12,2)
*? 'Percep. '+STR(vper,5,2)+' : '+STR(percep,12,2)+SPACE(4)+'Art.56ง '+STR(vmul,5,2)+' : '+STR(multa,12,2)+space(38)+STR(por[1],4,2)+` : '+STR(ng1*por[1]/100,12,2)
 *Esta linea se agrega el 28-08-2002 a pedido de Sergio Bernue
 IF vPR122#0
  ? 'Perc.I.Bru.Res.12/02 %'+STR(vPR122,5,2)+' : '+STR(vIR122,12,2)
 ELSE
  ?
 ENDIF
 *Nuevo Item de Percepcion 12-05-2009
 IF  vPer#0 .AND. percep#0
  ? "Percepcion %"+STR(vPer,5,2)+"  $ "+STR(percep,12,2)
 ELSE
  ?
 ENDIF

*Tienen el * para eliminar la impresion de ingresos brutos
*? SPACE(4)+STR(iva1,5,2)+` : '+IIF(vciv='3'.OR.vciv='4',STR(0,12,2),STR((vnet*iva1)/100,12,2))+SPACE(29)+STR(iva2,5,2)+;
*` : '+IIF(vciv=`2',STR((vnet*iva2)/100,12,2),STR(0,12,2))+SPACE(25)+STR(por[2],4,2)+;
*' : '+STR(ng2*por[2]/100,12,2)+SPACE(23)+CHR(18)+STR(tgral,12,2)
? SPACE(4)+STR(iva1,5,2)+` : '+IIF(vciv='3'.OR.vciv='4',STR(0,12,2),STR((vnet*iva1)/100,12,2))+SPACE(29)+STR(iva2,5,2)+;
` : '+IIF(vciv=`2',STR((vnet*iva2)/100,12,2),STR(0,12,2))+SPACE(29)+;
' : '+SPACE(35)+CHR(18)+STR(tgral,12,2)
EJECT
?? CHR(27)+CHR(33)+CHR(8)
RETURN

PROCEDURE Fact_ListB
?? CHR(27)+CHR(120)+CHR(0)+CHR(18)
?? CHR(27)+CHR(33)+CHR(5)
?? CHR(27)+CHR(67)+CHR(36)
NORMA()
? CHR(15)+SPACE(51)+CHR(18)+SPACE(34)+'San Rafael, '+DTOC(vfec)
?? CHR(15)
? CHR(15)+SPACE(51)+CHR(18)+SPACE(46)+"Copia"+CHR(15)   &&+'Conv.Mult. 913-534537-0'
IF vtip=`2'
 w=`  Por Factura: '+ltra_nc+srie_nc+fact_nc
ELSE
 w=SPACE(20)
ENDIF
? SPACE(50)+CHR(18)+SPACE(50)+IIF(vtip=`1',`FACT.',iif(vtip=`2',`N.C. ',`N.D. '))+' Nro.: '
? SPACE(79)+vltra+vsrie+vnum
?
IF vcli="000000"
 *Archivo Consumidores Finales
 AREA()
 OPEN_DBF("MAECOFIN","CONS_FIN","C","MAECOFIN","tcom+ltra+srie+nume")
 SELECT CONS_FIN
 SEEK nom_tip+vltra+vsrie+vnum
 IF FOUND()
  vnom=nomb
  vdom=domi
  vcui=cuit
  ciu=""
  provi=""
 ENDIF
 CLOSE
ELSE
 SELECT CLIENTES
 GO regcli
 vdom=domi
 vcui=cuit
 ciu=ALLTRIM(DEPARTA->nomb)
 provi=ALLTRIM(PROVIN->nomb)
ENDIF
? SPACE(8)+vcli+' - '+vnom+SPACE(17)+w
? SPACE(8)+vdom
?? SPACE(3)+ciu+` - '+provi
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
?? 'X'     &&+IIF(conta,SPACE(47),SPACE(24))+vrem
IF !conta
 IF vTIp="1".OR.vTip="3"
  *Fecha Vencimiento
  ?? CHR(27)+CHR(69)   &&Negrita ON
  ?? ' Vto.:'+SUBSTR(DTOC(vFVto),1,6)+RIGHT(DTOC(vFVto),2)
  ?? CHR(27)+CHR(70)   &&Negrita OFF
  ?? SPACE(10)+vRem
 ELSE
  ?? SPACE(24)+vRem
 ENDIF
ELSE
 ?? SPACE(47)+vRem
ENDIF
?     && Luego poner Codigo Sucursal
? SPACE(56)+"C.Barras"
IF vtip=`1'.OR. (vtip=`2' .AND. nc=0)
 pa=1
ELSE
 pa=2
ENDIF
k=0
IF pa=1
 FOR i=1 TO LEN(campos)
  IF !EMPTY(SUBSTR(campos[i],1,5))
   ? SUBSTR(campos[i],1,5)+'    '+SUBSTR(campos[i],40,7)+'   '+VDESC[i]+' '+SUBSTR(campos[i],79,5)+'   '+;
     SUBSTR(campos[i],48,5)+SPACE(1)+SUBSTR(campos[i],56,10)+'   '+SUBSTR(campos[i],67,12)
   k=k+1
  ENDIF
 NEXT i
ELSE
 FOR i=1 TO LEN(campos)
  IF SUBSTR(campos[i],1,5)='*****'
   ? SPACE(19)+mens1+SPACE(3)+SUBSTR(campos[i],48,5)+space(27)+SUBSTR(campos[i],67,12)
   ? SPACE(19)+mens2
   k=2
  ENDIF
 NEXT i
ENDIF
FOR i=1 TO 12-k
 ?
NEXT i
?
 *Nuevo Item de Percepcion 12-05-2009
 IF vPer#0 .AND. percep#0
  ? "Percepcion %"+STR(vPer,5,2)+"  $ "+STR(percep,12,2)
 ELSE
  ?
 ENDIF

? SPACE(10)+STR(vtot,12,2)+SPACE(25)+STR(tdes,12,2)+SPACE(10)
?? IIF(bRG212#0,'Percep.RG.212: '+STR(bRG212,12,2),SPACE(1))
?
?
? ' '+IIF(SUBSTR(campos[1],1,5)<>'*****',SUBSTR(ALLTRIM(mens1)+' '+ALLTRIM(mens2)+SPACE(61),1,61),SPACE(61))
?? SPACE(22)+STR(tgral,12,2)
EJECT
?? CHR(27)+CHR(33)+CHR(8)
RETURN

*Control de Zona vta.
FUNCTION LIS_VEN
PARAMETERS ven,nom
IF LASTKEY() = 27
 KEYBOARD CHR(27)
 RETURN .T.
ENDIF
PRIVATE colo_a,antsel,ok2
antsel=ALLTRIM(STR(SELECT()))
colo_a=SETCOLOR()
ok2=0
IF SELECT("ZONAS")=0
 *ZONAS
 AREA()
 OPEN_DBF("MAEZONAS","ZONAS","C","MAEZONAS","vend","NOMZONAS","nomb")
 ok2=1
ENDIF
SELECT ZONAS
SET ORDER TO 1
ven = CERO(VAL(ven),3)
SEEK ven
IF .NOT. FOUND()
 *Ayuda de ZONAS
 archivo='ZONAS'
 campi='vend~nomb'
 ntx1=1
 ntx2=2
 l1=3
 l2=30
 camp1='vend'
 IF ayudo(campi,ntx1,ntx2,l1,l2,camp1,archivo) = .T.
  ven=vend
  nom=nomb
  IF ok2=1
   CLOSE
  ENDIF
  SELECT &antsel
  RETURN .T.
 ELSE
  IF ok2=1
   CLOSE
  ENDIF
  SELECT &antsel
  RETURN .F.
 ENDIF
ENDIF
ven=vend
nom=nomb
IF ok2=1
 CLOSE
ENDIF
SELECT &antsel
RETURN .T.
