*Presupuestos para Clietnes
PARAMETERS vOpcion
PRIVATE pan_ant,col_ant,Ntx1:="NADA.NTX"
pan_ant=SAVESCREEN(0,0,24,79)
col_ant=SETCOLOR()

*Abri Archivos
*Porcentaje de iva
PRIVATE vIvaI,vIvaNi
AREA()
IF USAR('PORCEIVA','PIVAS')
 GO TOP
 vIvaI =IVAI
 vIvaNi=IVANI
ELSE
 QUIT
ENDIF
*Numeraciขn de Presupuestos
OPEN_DBF("NUMPRESU","NUMPR","C","NUMPRESU","nano")

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
  *Por Marca y Articulo
  IF !FILE('MAEARTI3&vRDD.')
   INDEX ON mrca TO MAEARTI3
  ENDIF
  *Por rubro, Marca y Articulo
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
  Ntx1=ARCH_EXI("BORRA",RIGHT(vRDD,3))
  INDEX ON PROVE->nomb TO &Ntx1
  SET INDEX TO MAEARTIC,MAEARTI1,MAEARTI2,MAEARTI3,MAEARTI4,MAEARTI5,NOMARTIC,&Ntx1
 ELSE
  QUIT
 ENDIF

*Cabecera Presupuestos
AREA()
IF USAR('MOVPRECB','PRECB')
 IF !FILE('MOVPRECB&vRDD.')
  INDEX ON nano+npre TO MOVPRECB
 ENDIF
 SET INDEX TO MOVPRECB
ELSE
 QUIT
ENDIF

*Detalle Presupuestos
AREA()
IF USAR('MOVPREDE','PREDE')
 IF !FILE('MOVPREDE&vRDD.')
  INDEX ON nano+npre TO MOVPREDE
 ENDIF
 SET INDEX TO MOVPREDE
ELSE
 QUIT
ENDIF

*Variables
PRIVATE vFech,vNpre,vNano,vClie,vCpag,vFvto,Campos[200],agg,baja,vObse,sale:=0

baja=.F.
DO WHILE .T.
 SETCOLOR(colo3)
 DO PRE_ART
 vFech=DATE()
 vNano=CERO(YEAR(DATE()),4)
 vNPre="----"
 vClie=SPACE(30)
 vCpag=SPACE(30)
 vFvto=SPACE(30)
 vObse=SPACE(40)
 AFILL(Campos,'     ณ                                                    ณ        ณ         '+;
              SPACE(91)) &&Unid+fvta+unfv+medi+mrca+cali+rubr
 *Busco Nro. Presupuesto anterior para el Aคo en curso
 SELECT NUMPR
 SEEK vNano
 IF !FOUND()
  vAnte="0000"
 ELSE
  vAnte=CERO(VAL(npre),4)
 ENDIF
 SELECT PRECB
 DO WHILE .T.
  SETCOLOR(colo3)
  XPM=6
  XP=1
  agg=0
  IF vOpcion=2
   *Modificar Presupuesto
   vNpre=SPACE(4)
   @2,48 GET vNano PICTURE "@K 9999"
   @2,53 GET vNpre PICTURE "@K 9999"
   READ
   IF LASTKEY()=27
    IF baja
     DO ELIMINAR
    ENDIF
    CLOSE ALL
    RESTSCREEN(0,0,24,79,pan_ant)
    SETCOLOR(col_ant)
    FERASE("&Ntx1")
    RETURN
   ENDIF
   vNano=CERO(VAL(vNano),4)
   vNpre=CERO(VAL(vNpre),4)
   SELECT PRECB
   SEEK vNano+vNpre
   IF FOUND()
    vFech=fech
    vClie=clie
    vCpag=cpag
    vFvto=fvto
    vObse=obse
    SELECT PREDE
    AFILL(Campos,'     ณ                                                    ณ        ณ         '+;
                 SPACE(91)) &&Unid+fvta+unfv+medi+mrca+cali+rubr
    SEEK vNano+vNpre
    IF FOUND()
     p=1
     DO WHILE !EOF().AND.nano+npre = vNano+vNpre
      campos[p] = STUFF(campos[p],1,5,arti)
      campos[p] = STUFF(campos[p],6,1,"ฑ")
      campos[p] = STUFF(campos[p],8,50,nomb)
      campos[p] = STUFF(campos[p],60,8,TRANSFORM(cant,'@Z 99999.99'))
      campos[p] = STUFF(campos[p],69,9,TRANSFORM(puni,'@Z 99999.999'))
      vArti=arti
      fRubr=rubr
      Nom_Rubr=RUBRO->nomb
      SELECT ARTIC
      SET ORDER TO 6
      SEEK fRubr + vArti
      IF FOUND()
       fUnid=unid
       fFvta=fvta
       fUnVt=unvt
       fMedi=medi
       fMrca=mrca
       Nom_Mrca=MARCA->nomb
       fCali=cali
      ENDIF
      SELECT PREDE
      *Agregar atributos
      campos[p] = STUFF(campos[p],78,4,fRubr)
      campos[p] = STUFF(campos[p],82,20,Nom_Rubr)
      campos[p] = STUFF(campos[p],102,10,fUnid)
      campos[p] = STUFF(campos[p],112,10,fFvta)
      campos[p] = STUFF(campos[p],122,10,fUnVt)
      campos[p] = STUFF(campos[p],132,15,fMedi)
      campos[p] = STUFF(campos[p],147,14,fMrca+' '+Nom_Mrca)
      campos[p] = STUFF(campos[p],161, 8,fCali)
      agg=1
      p=p+1
      SKIP
     ENDDO
    ENDIF
   ENDIF
   @4,31 SAY vClie COLOR colo12
   @5,31 say vCpag COLOR colo12
   @6,31 SAY vFvto COLOR colo12
   @17,39 SAY vObse COLOR colo12
   xx=9
   FOR con=1 TO LEN(Campos)
    @xx,2 SAY SUBSTR(Campos[con],1,77)
    IF xx=14
     EXIT
    ENDIF
    xx++
   NEXT
  ENDIF

  SET KEY -4 TO VA_TRAS
  @2,48 SAY vNano+'/'+vNpre COLOR colo12
  IF vOpcion=1
   @2,60 SAY vNano+'/'+vAnte COLOR "*"+colo12
  ENDIF
  DO WHILE XP<=XPM
   XD=1
   SET CURSOR ON
   DO CASE
    CASE XP=1
     *Fecha
     Sale=0
     @2,26 GET vFech PICTURE "@D" VALID !EMPTY(vFech)
     READ
     @2,26 SAY vFech PICTURE "@D" COLOR colo12

    CASE XP=2
     *Clinte
     Sale=0
     @4,31 GET vClie PICTURE "@!"
     READ
     @4,31 SAY vClie COLOR colo12

    CASE XP=3
     *Forma de Pago
     Sale=0
     @5,31 GET vCpag PICTURE "@!"
     READ
     @5,31 say vCpag COLOR colo12

    CASE XP =4
     *Validez de la Oferta
     Sale=0
     @6,31 GET vFvto PICTURE "@!"
     READ
     @6,31 SAY vFvto COLOR colo12

    CASE XP=5
     SET CURSOR OFF
     com=1
     rel=0
     sale=0
     KEYBOARD CHR(24)+CHR(5)
     DO WHILE .T.
      @23,5 SAY "[* ข Esc] para Terminar" COLOR color3
      com=ACHOICE(9,2,14,78,Campos,.T.,"Art_Pre",com,rel)
      SET KEY -4 TO VA_TRAS
      IF LASTKEY()=-4
       XD=-1
       EXIT
      ENDIF
      IF com=0
       EXIT
      ENDIF
     ENDDO
     IF !EMPTY(SUBSTR(Campos[1],1,5))
      sale=1
     ENDIF

    CASE XP =6
     *Observaciones
     @17,39 GET vObse PICTURE "@!"
     READ
     @17,39 SAY vObse COLOR colo12

   ENDCASE
   IF LASTKEY()=27
    CLEAR GETS
    XP=7
   ELSE
    XP=XP+XD
   ENDIF
  ENDDO
  SETCOLOR(color3)
  scr1=SAVESCREEN(14,10,19,25)
  SOMBRA(14,11,18,23,'H2',1,'',color3)
  ok=0
  IF !EMPTY(SUBSTR(campos[1],1,5)) .AND. sale = 1
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
    LOOP
   CASE opc1=2
    *GRAVAR ARCHIVO
    IF !DUDA("DATOS CORRECTOS")
      SET CURSOR OFF
      LOOP
    ENDIF

    DO GRA_REG

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
      DO IMP_PRESU
     SET PRINT OFF
     SET CONSOLE ON
    ENDIF
    ok=0
    EXIT

   CASE opc1=3 .OR. opc1=0
   IF DUDA("CONFIRMA CANCELACION",1,color3)
     SET CURSOR OFF
     IF LASTKEY()=27
      LOOP
     ENDIF
     IF baja
      DO ELIMINAR
     ENDIF
     CLOSE ALL
     RESTSCREEN(0,0,24,79,PAN_ANT)
     SETCOLOR(col_ant)
     FERASE("&Ntx1")
     RETURN
    ELSE
     SET CURSOR OFF
     LOOP
    ENDIF
  ENDCASE
 ENDDO
ENDDO
IF baja
 DO ELIMINAR
ENDIF
CLOSE ALL
RESTSCREEN(0,0,24,79,pan_ant)
SETCOLOR(coli_ant)
FERASE("&Ntx1")
RETURN

PROCEDURE ELIMINAR
CLOSE ALL
*Cabecera Presupuestos
AREA()
IF USAR('MOVPRECB','PRECB','E')
 IF !FILE('MOVPRECB&vRDD.')
  INDEX ON nano+npre TO MOVPRECB
 ENDIF
 SET INDEX TO MOVPRECB
 PACK
ELSE
 QUIT
ENDIF
CLOSE

*Detalle Presupuestos
AREA()
IF USAR('MOVPREDE','PREDE','E')
 IF !FILE('MOVPREDE&vRDD.')
  INDEX ON nano+npre TO MOVPREDE
 ENDIF
 SET INDEX TO MOVPREDE
 PACK
ELSE
 QUIT
ENDIF
CLOSE
RETURN

PROCEDURE GRA_REG
IF agg = 1
 *Si esta reescribiendo un Presupuesto, borro todo el Presupuesto
 SELECT PRECB
 SET FILTER TO npre=vNpre.AND.vNano=nano
 GO TOP
 IF FilLock(1)
  DELETE ALL
 ELSE
  SET FILTER TO
  GO TOP
  RETURN
 ENDIF
 UNLOCK
 SELECT PREDE
 SET FILTER TO npre=vNpre.AND.vNano=nano
 GO TOP
 IF FilLock(1)
  DELETE ALL
  BAJA = .T.
  UNLOCK
 ELSE
  SELECT PRECB
  RECALL ALL
 ENDIF
 SELECT PREDE
 SET FILTER TO
 GO TOP
 SELECT PRECB
 SET FILTER TO
 GO TOP
ENDIF
IF vOpcion=1
 SELECT NUMPR
 SEEK vNano
 IF !FOUND()
  ADDREC(1)
  RECLOCK(1)
  REPLACE nano WITH vNano
  REPLACE npre WITH "0000"
  UNLOCK
  COMMIT
  vNpre="0001"
 ELSE
  vNpre=CERO(VAL(npre)+1,4)
 ENDIF
ENDIF
SELECT PRECB
SET FILTER TO
*Gravo Cabecera
ADDREC(0)
RECLOCK(0)
REPLACE nano WITH vNano
REPLACE npre WITH vNpre
REPLACE fech WITH vFech
REPLACE clie WITH vClie
REPLACE cpag WITH vCpag
REPLACE fvto WITH vFvto
REPLACE obse WITH vObse
COMMIT
SELECT PREDE
FOR con = 1 TO LEN(campos)
 IF !EMPTY(SUBSTR(campos[con],1,5))
  ADDREC(0)
  RECLOCK(0)
  REPLACE nano WITH vNano
  REPLACE npre WITH vNpre
  REPLACE fech WITH vFech
  REPLACE rubr WITH SUBSTR(campos[con],78,4)
  REPLACE arti WITH SUBSTR(campos[con],1,5)
  REPLACE nomb WITH SUBSTR(campos[con],8,50)
  REPLACE cant WITH VAL(SUBSTR(campos[con],60,8))
  REPLACE puni WITH VAL(SUBSTR(campos[con],69,9))
  UNLOCK
 ENDIF
NEXT con
IF vOpcion=1
 SELECT NUMPR
 SEEK vNano
 IF FOUND()
  RECLOCK(1)
  REPLACE npre WITH vNpre
  UNLOCK
 ENDIF
 CARTEL("NUMERO DE PRESUPUESTO [ "+vNano+"/"+vNpre+" ]",12,1,'N',colo3)
ENDIF
SELECT PRECB
DBCOMMITALL()
RETURN

FUNCTION Art_Pre
PARAMETERS MOD,sub,pos
PRIVATE tcl,FIL,pus,oriy,con
PRIVATE fRubr,Nom_Rubr,fArti,Nom_Arti,fUnid,fFvta,fUnVt,fMedi,fMrca,Nom_Mrca
PRIVATE fCali,fExis,vTTa,vIIva,vGral
FIL=ROW()
pus = 6/LEN(campos)*sub
FOR oriy = 1 TO 6
 @8+oriy,1 SAY IF(pus <= oriy .and. pus >= oriy-.99999,IIF(sub=1,'',IIF(sub=LEN(Campos),'','')),'ณ')
NEXT oriy
tcl = LASTKEY()
*Suma e imprime Sub_total
vTTa=0
FOR con=1 TO LEN(campos)
 vTTa=vTTa + (VAL(SUBSTR(campos[con],60,8))*VAL(SUBSTR(campos[con],69,9)))
NEXT con
vIIva=0   &&(vTTa * vIvai)/100
vGral=vTTa + vIIva
@19,67 SAY  vTTa PICTURE '@Z 999999999.99' COLOR colo12
@20,67 SAY vIiva PICTURE '@Z 999999999.99' COLOR colo12
@21,67 SAY vGral PICTURE '@Z 999999999.99' COLOR colo12

IF EMPTY(SUBSTR(Campos[sub],1,5))
 *Si no hay Articulos cargados
 fRubr=SPACE(4)
 Nom_Rubr=SPACE(20)
 fArti=SPACE(5)
 Nom_Arti=SPACE(50)
 STORE SPACE(10) TO fUnid,fFvta,fUnVt
 fMedi=SPACE(15)
 fMrca=SPACE(3)
 Nom_Mrca=SPACE(10)
 fCali=SPACE(8)
 fCant=0
 fPuni=0
 fExis=.F.
ELSE
 *si hay Articulos cargados
 fRubr = SUBSTR(campos[sub],78,4)
 Nom_Rubr=SUBSTR(campos[sub],82,20)
 fArti = SUBSTR(campos[sub],1,5)
 Nom_Arti=SUBSTR(campos[sub],8,50)
 fCant = VAL(SUBSTR(campos[sub],60,8))
 fPuni = VAL(SUBSTR(campos[sub],69,9))
 fUnid = SUBSTR(Campos[sub],102,10)
 fFVta = SUBSTR(Campos[sub],112,10)
 fUnVt = SUBSTR(Campos[sub],122,10)
 fMedi = SUBSTR(Campos[sub],132,15)
 fMrca = SUBSTR(Campos[sub],147,3)
 Nom_Mrca = SUBSTR(Campos[sub],151,10)
 fCali = SUBSTR(Campos[sub],161,8)
 fExis = .T.
ENDIF
@17,8 SAY fRubr+' '+Nom_Rubr COLOR colo12
@19,10 SAY PADL(ALLTRIM(fUnid),10," ") COLOR colo12
@20,10 SAY PADL(ALLTRIM(fFvta),10," ") COLOR colo12
@21,10 SAY PADL(ALLTRIM(fUnvt),10," ") COLOR colo12
@19,29 SAY PADL(ALLTRIM(fMedi),15," ") COLOR colo12
@20,29 SAY PADL(ALLTRIM(fMrca+' '+Nom_Mrca),15," ") COLOR colo12
@21,36 SAY PADL(ALLTRIM(fCali),8," ") COLOR colo12
DO CASE

 CASE MOD = 0
  RETURN(2)

 CASE MOD = 1 .OR. MOD = 2
  TONO(2)
  RETURN(2)

 CASE MOD = 3
  DO CASE

   CASE tcl = 27 .OR. tcl=42    &&Salir
    RETURN(0)

   CASE tcl = 13
    XPC = 5
    XC = 1
    SET KEY -4 TO va_tras
    SETCOLOR(colo3)
    DO WHILE XC <= XPC
     XD = 1
     SET ESCAPE ON
     SET CURSOR ON
     DO CASE
      CASE XC = 1
       *Rubro
       IF SUBSTR(Campos[sub],6,1) # 'ฑ'.AND. !fExis
        @17,8 GET fRubr PICTURE "@!" VALID VAL_RUB(@fRubr,@Nom_Rubr)
        READ
        @17,8 SAY fRubr+' '+Nom_Rubr
       ENDIF

      CASE XC = 2
       IF SUBSTR(Campos[sub],6,1) # 'ฑ'.AND. !fExis
        @FIL,2 GET fArti PICTURE "@K 99999" VALID VER_ARTI(@fArti,@Nom_Arti,fRubr)
        READ
        @FIL,9 SAY Nom_Arti
       ENDIF

       *Verificar si ya Ingreso el Mismo Articulo solo si no esta modificando
       IF !fExis
        IF ASCAN(Campos,fArti) # 0 .AND. SUBSTR(campos[sub],6,1) # 'ฑ'
         CARTEL("YA INGRESO ESTE ARTICULO [ OK ]",12,1,'N',color3)
         XC=0
        ENDIF
       ENDIF

      CASE XC = 3
       @FIL,9 GET Nom_Arti PICTURE "@!"
       READ

      CASE XC = 4
       @FIL,61 GET fCant PICTURE "@K 99999.99"
       READ

      CASE XC = 5
       @FIL,70 GET fPuni PICTURE "@K 99999.999"
       READ

     ENDCASE
     IF LASTKEY() = 27
      CLEAR GETS
      XC = 6
     ELSE
      XC = XC + XD
     ENDIF
    ENDDO
    SET KEY -4 TO
    SET CURSOR OFF
    IF LASTKEY() = 27
     RETURN(2)
    ENDIF
    campos[sub] = STUFF(campos[sub],1,5,fArti)
    campos[sub] = STUFF(campos[sub],8,50,Nom_Arti)
    campos[sub] = STUFF(campos[sub],60,8,TRANSFORM(fCant,'@Z 99999.99'))
    campos[sub] = STUFF(campos[sub],69,9,TRANSFORM(fPuni,'@Z 99999.999'))
    *Agregar atributos
    campos[sub] = STUFF(campos[sub],78,4,fRubr)
    campos[sub] = STUFF(campos[sub],82,20,Nom_Rubr)
    campos[sub] = STUFF(campos[sub],102,10,fUnid)
    campos[sub] = STUFF(campos[sub],112,10,fFvta)
    campos[sub] = STUFF(campos[sub],122,10,fUnVt)
    campos[sub] = STUFF(campos[sub],132,15,fMedi)
    campos[sub] = STUFF(campos[sub],147,14,fMrca+' '+Nom_Mrca)
    campos[sub] = STUFF(campos[sub],161, 8,fCali)
    KEYBOARD CHR(24)
    rel = pos
    RETURN(1)

   CASE (tcl>=48 .AND. tcl<=57).OR.;
        (tcl>=65 .AND. tcl<=90).OR.;
        (tcl>=97 .AND. tcl<=122)
    IF SUBSTR(campos[sub],6,1) = 'ฑ'
     KEYBOARD CHR(13)
    ELSE
     KEYBOARD CHR(13)+CHR(tcl)
    ENDIF
    RETURN(2)

   CASE tcl = 7
    ADEL(campos,sub)
    campos[LEN(campos)] = "     ณ                                                    ณ        ณ         "+SPACE(91)
    fArti=SPACE(5)
    Nom_Arti=SPACE(50)
    fRubr=SPACE(4)
    Nom_Rubr=SPACE(20)
    fCant=0
    fPuni=0
    rel = pos
    RETURN(1)

 OTHERWISE
    RETURN(2)

 ENDCASE
ENDCASE

*Control de RUBROS
FUNCTION VAL_RUB
PARAMETERS fRubr,fNom_Rubr
PRIVATE antsel
antsel=ALLTRIM(STR(SELECT()))
SELECT RUBRO
SET ORDER TO 1
fRubr=PADL(fRubr,4,' ')
SEEK fRubr
IF .NOT.FOUND()
 *Ayuda de Rubros
 archivo='RUBRO'
 campi='rubr~nomb'
 ntx1=1
 ntx2=2
 l1=4
 l2=30
 camp1='rubr'
 IF AYUD1(campi,ntx1,ntx2,l1,l2,camp1,archivo) = .T.
  fRubr=rubr
  Nom_Rubr=nomb
  SELECT &antsel
  RETURN .T.
 ELSE
  SELECT &antsel
  RETURN .F.
 ENDIF
ENDIF
Nom_Rubr=nomb
SELECT &antsel
RETURN .T.

FUNCTION VER_ARTI
PARAMETERS var1,descri,fRubr
LOCAL sub_pan,area_ant,tcl,VAR,colo_ant
colo_ant=SETCOLOR()
IF LASTKEY()=27         &&tcl=27
 RETURN(.T.)
ENDIF
area_ant=ALLTRIM(STR(SELECT()))
SELECT ARTIC
SET ORDER TO 6
VAR1=CERO(VAL(var1),5)
SEEK fRubr+var1
IF !FOUND()
 *--Filtrar registros a mostrar
 vCount=0
 bBloq:={|| vCount++}

 bCondi:={|| Rubr=fRubr }
 bWhile:={|| vCount=0 }
 GO TOP
 DBEVAL(bBloq,bCondi,bWhile)
 IF vCount=0
  CARTEL("NO HAY REGISTROS EN RUBRO SOLICITADO  [ OK ]",18,1,'N',color3)
  SELECT &area_ant
  RETURN .F.
 ENDIF
 SET FILTER TO rubr=fRubr
 GO TOP
 sub_pan=SAVESCREEN(4,0,24,79)
 SETCOLOR(color3)
 SOMBRA(4,0,22,79,'V1',3,'',color3)
 @5,79 TO 21,79 DOUBLE COLOR color3
 @ 6,0 SAY'ฬออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน'
 @19,0 SAY'บฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤบ'
 @16,0 SAY'ฬออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน'
 SETCOLOR(INVER)
 @4,38 SAY' BUSCAR '
 SETCOLOR(color3)
 @17,1 SAY'BUSCAR:'
 @18,2 SAY SPACE(38)
 @20,12 SAY CHR(24)+CHR(25)+' PgUp PgDn Mover         '+CHR(17)+'ฤู Acepta'
 @21,12 SAY'F2-Cambia Clave Busqueda  [Esc]-Salir'
 DECLARE cam[4],sep[4],ind[3],lar[3],nom[3]
 CARGA1(cam,"rubr+arti~SUBSTR(nomb,1,40)~medi~PROVE->nomb")
*AQUIAQUI
 tit='Rub+Cod   Nombre Artกculo                          Medidas         Proveedor  '
 CARGA2(sep,'1,1,1,0')
 ind[1]=6
 lar[1]=9
 nom[1]='Rubro + Articulo: '
 ind[2]=7
 lar[2]=50
 nom[2]='Nombre del Producto : '
 ind[3]=8
 lar[3]=30
 nom[3]='Nombre Proveedor    : '
 ord_ant=STR(INDEXORD())
 SET ORDER TO 7
 SETCOLOR(color3)
 GO TOP
 BUSCARE(5,1,15,78,cam,tit,sep,ind,lar,18,1,nom,17,10)
 SETCOLOR(colo_ant)
 RESTSCREEN(4,0,24,79,sub_pan)
 SET ORDER TO &ord_ant
 IF LASTKEY()=27
  RETURN(.F.)
 ENDIF
ENDIF
var1=arti
fPuni=pucd
descri=nomb
fUnid=unid
fFvta=fvta
fUnVt=unvt
fMedi=medi
fMrca=mrca
Nom_Mrca=MARCA->nomb
fCali=cali
SETCOLOR(colo_ant)
SELECT &area_ant
RETURN(.T.)

PROCEDURE PRE_ART
SETCOLOR(colo3)
@ 1,1 SAY "                ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤยPresupuestoฤฤฤฤฤฤฤฤฤยAnteriorฤฤฟ          "
@ 2,1 SAY "                ณ Fecha:            ณ  Nฃmero:           ณ          ณ          "
@ 3,1 SAY "        ฺฤฤฤฤฤฤฤมฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤมฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤมฤฤฤฤฤฤฤฤฤฤด          "
@ 4,1 SAY "        ณ     Cliente       :                                       ณ          "
@ 5,1 SAY "        ณ     Forma de Pago :                                       ณ          "
@ 6,1 SAY "        ณ     Validez Oferta:                                       ณ          "
@ 7,1 SAY "        ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู          "
@ 8,1 SAY "ฺArt.ฤยDescripciขnฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤยCantidadยP.Unit.ฤฤฟ"
@ 9,1 SAY "ณ     ณ                                                    ณ        ณ         ณ"
@10,1 SAY "ณ     ณ                                                    ณ        ณ         ณ"
@11,1 SAY "ณ     ณ                                                    ณ        ณ         ณ"
@12,1 SAY "ณ     ณ                                                    ณ        ณ         ณ"
@13,1 SAY "ณ     ณ                                                    ณ        ณ         ณ"
@14,1 SAY "ณ     ณ                                                    ณ        ณ         ณ"
@15,1 SAY "ณ     ณ                                                    ณ        ณ         ณ"
@16,1 SAY "รฤฤฤฤฤมฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤมฤฤฤฤฤฤฤฤมฤฤฤฤฤฤฤฤฤด"
@17,1 SAY "ณRubro:                         ณObs.:                                        ณ"
@18,1 SAY "รฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤฤฤมฤฤฤฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤด"
@19,1 SAY "ณUnidad:           ณMedidas:               ณ          ณSubTotal :             ณ"
@20,1 SAY "ณF.Vta.:           ณMarca  :               ณ          ณ                       ณ"
@21,1 SAY "ณUn.F.V:           ณCalidad:               ณ          ณTotal    :             ณ"
@22,1 SAY "ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤมฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู          ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู"
RETURN

PROCEDURE Imp_Presu
vtot=0
vsub=0
Pos=1
pagina=0
?? CHR(27)+CHR(120)+CHR(0)+CHR(18)
?? CHR(27)+CHR(67)+CHR(72)
?? CHR(27)+CHR(33)+CHR(5)
DO WHILE .T.
 *dibujar encabezado
 pagina=pagina+1
 NORMA()
 GRAN_ON()
 ? SUBSTR(nom_emp+SPACE(25),1,25)
 GRAN_OFF()
 NORMA()
 CHICO()
 ??SPACE(3)+'Fecha       : '+STUFF(DTOC(DATE()),3,4,'-'+MESES(DATE())+'-')
 ? "CONSTRUCCION.ELECTRICIDAD.FERRETERIA  "+Dom_Emp+" - "+Loc_Emp
 ? "Telfono: 02627-470135 electroandina@slatinos.com.ar"
 ?
 NORMA()
 GRAN_ON()
 ? "Presupuesto: "+vNano+"/"+vNpre
 GRAN_OFF()
 NORMA()
 ? "Cliente: "+vClie
 ?
 CHICO()
 ? "It Descripciขn                                                        Unidad     Medida          Marca      Cantidad P.Unit.   P.Total  "
 ? "-- ------------------------------------------------------------------ ---------- --------------- ---------- -------- --------- ---------"
 *  1  123456789012345678901234567890123456789012345678901234567890123456 1234567890 123456789012345 12345678 123456789 123456789
 *  123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456
 DO WHILE INKEY(.1)#27.AND. hor<=63 .AND. Pos<=LEN(Campos)
  IF !EMPTY(SUBSTR(Campos[Pos],1,5))
   SELECT ARTIC
   SET ORDER TO 6
   fClav=SUBSTR(Campos[Pos],78,4)+SUBSTR(campos[Pos],1,5)
   SEEK fClav
   IF FOUND()
    fCpro=cpro
   ELSE
    fCpro=SPACE(15)
   ENDIF
   fArti = fCpro+" "+SUBSTR(Campos[Pos],8,50)
   fUnid = SUBSTR(Campos[Pos],102,10)
   fMedi = SUBSTR(Campos[Pos],132,15)
   fMrca = SUBSTR(Campos[Pos],151,10)
   fCant = SUBSTR(Campos[Pos],60,8)
   fPuni = SUBSTR(Campos[Pos],69,9)
   fPtot = SUBSTR(VAL(fCant)*VAL(fPuni),9,2)

   ? CERO(Pos,2)+" "+fArti+" "+fUnid+" "+fMedi+" "+fMrca+" "+fCant+" "+fPuni+" "+fPtot
   vTot=vTot+VAL(fPtot)
   hor++
  ENDIF
  Pos++
 ENDDO
 IF Pos>LEN(Campos)
  EXIT
 ENDIF
 IF hor>63
  EJECT
 ENDIF
ENDDO
? SPACE(89)+'**SUB T O T A L  : '+STR(vtot,9,2)
*vIIva=(vTot * vIvai)/100
vGral=vTot
? "Iva incluido"   &&SPACE(89)+'**          IVA  : '+STR(vIIva,9,2)
? SPACE(108)+"=========="
? SPACE(89)+'**   TOTAL GRAL. : '+STR(vGral,9,2)
EJECT
?? CHR(27)+CHR(33)+CHR(8)
SET CURSOR ON
RETURN
