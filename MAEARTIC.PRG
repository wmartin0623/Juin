************************************************************
*       ALTA, MODIF. Y CONSULTA DE PRODUCTOS POR PRODUCTOR *
************************************************************

PRIVATE menu1[4],menu2[5],fi_co[1],salir[2],pant1
PUBLIC vRubr,Nom_Rubr,vArti,Nom_Arti,vProv,Nom_PRov,vCPro,vUnid,vFVta,vMone
PUBLIC vUnVt,vStck,vMedi,vMrca,Nom_Mrca,vClor,vCali,vUsos,vDrza,vRefe,vM2Cj
PUBLIC vbMedi,vbMrca,vbClor,vbCali,vbUsos,vbDrza,vbRefe,vBM2cj,vObse
PUBLIC optar,band
PUBLIC vPuCd,vPuCC,bPuCd,bPuCC,vFmod
PRIVATE Ntx1:="NADA.TXT"
PRIVATE Ntx2:="NADA.TXT"
vRubr = SPACE(4)
Nom_Rubr = SPACE(20)
optar = 2
XPM=20  &&CANTIDAD DE GETS QUE TIENE EL PROGRAMA
BAND=0
pant1 = SAVESCREEN(2,1,24,79)
*Rubros
AREA()
IF USAR('MAERUBRO','RUBRO')
 IF !FILE('MAERUBRO.NTX')
  INDEX ON rubr TO MAERUBRO
 ENDIF
 IF !FILE('NOMRUBRO.NTX')
  INDEX ON nomb TO NOMRUBRO
 ENDIF
 SET INDEX TO MAERUBRO,NOMRUBRO
ELSE
 QUIT
ENDIF

*Marcas
AREA()
OPEN_DBF("MAEMARCA","MARCA","C","MAEMARCA","mrca","NOMMARCA","nomb")

*Articulos
 *Lista de Precios
 AREA()
 OPEN_DBF("MAEPROVE","PROVE","C","MAEPROVE","prov","NOMPROVE","nomb")
*Articulos
 AREA()
 IF USAR('MAEARTIC','ARTIC')
  SET RELATION TO prov INTO PROVE, TO rubr INTO RUBRO, TO mrca INTO MARCA
  *Por Rubro,Lista de Precios
  IF !FILE('MAEARTIC&vRDD.')
   INDEX ON rubr+prov TO MAEARTIC
  ENDIF
  *Por Rubro, Lista de Precios y Marca
  IF !FILE('MAEARTI1&vRDD.')
   INDEX ON rubr+prov+mrca TO MAEARTI1
  ENDIF
  *Por Lista de Precios, Marca y Articulo
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
  IF !FILE('MAEARTI6&vRDD.')
   INDEX ON arti TO MAEARTI6
  ENDIF
   Ntx1=ARCH_EXI("BORRA",RIGHT(vRDD,3))
   INDEX ON MARCA->nomb TO &Ntx1
 SET INDEX TO MAEARTIC,MAEARTI1,MAEARTI2,MAEARTI3,MAEARTI4,MAEARTI5,NOMARTIC,&Ntx1,MAEARTI6
 ELSE
  QUIT
 ENDIF

* HAGO LA PANTALLA DEL ARCHIVO A TRATAR

SETCOLOR(color4)
SOMBRA(4,1,22,78,'H1',1,'',color4,'N+')
SET FORMAT TO PAN_ARTIC
READ
SET FORMAT TO
SETCOLOR(color4)
@4,7 SAY CENTRA(' A R T I C U L O S ',60,'อ')

SETCOLOR(color4)
@ 4,69 SAY 'หออออออออป'
@ 5,69 SAY 'บALTAS   บ'
@ 6,69 SAY 'บ        บ'
@ 7,69 SAY 'บMODIFICAบ'
@ 8,69 SAY 'บ        บ'
@ 9,69 SAY 'บBAJAS   บ'
@10,69 SAY 'บ        บ'
@11,69 SAY 'บVOLVER  บ'
@12,69 SAY 'ฬออออออออน'
@13,69 SAY 'บBUSCAR  บ'
@14,69 SAY 'บ        บ'
@15,69 SAY 'บPROXIMO บ'
@16,69 SAY 'บ        บ'
@17,69 SAY 'บANTERIORบ'
@18,69 SAY 'บ        บ'
@19,69 SAY 'บPRIMERO บ'
@20,69 SAY 'บ        บ'
@21,69 SAY 'บULTIMO  บ'
@22,69 SAY 'สออออออออผ'



* ACTIVO EL PANEL DE CONTROL

carga1(menu1,'`ALTAS   ~`MODIFICA~`BAJAS   ~`VOLVER  ')
carga1(menu2,'`BUSCAR  ~`PROXIMO ~`ANTERIOR~P`RIMERO ~`ULTIMO  ')
carga2(salir,'13,9')

* SI ESTA VACIA POSICIONO OPCION EN ALTAS SINO EN BUSCAR

SELECT ARTIC

IF VACIA()
   cual = 2
ELSE
   cual = 1
ENDIF
op1 = 1
op2 = 1

DO WHILE .T.

* SI LA BASE ESTA VACIA INIC.VARIABLES D/L CONTRARIO LAS CARGO C/DATOS D/L CAMPOS
   SELECT ARTIC
   IF !VACIA()
      DO ca_varc2
   ELSE
      DO v_0c1
   ENDIF
* MUESTRO CONTENIDO DE VARIABLES
   DO pon_sayc1
   tecla = 9
   DO WHILE tecla = 9
      colur=SETCOLOR()
      SETCOLOR(color3)
      @ 24,2 SAY CENTRA(CHR(24)+CHR(25)+CHR(26)+CHR(27)+'  Mover      TAB-Cambia Menu   '+CHR(17)+'ฤู-ACEPTA  ESC-Salir',78)
      SETCOLOR(colur)
      cual = IF(cual < 2,cual+1,1)
      DO CASE
      CASE cual = 1
         SETCOLOR(color4)
         @12,69 SAY 'ฬออออออออน'
         @13,69 SAY 'บBUSCAR  บ'
         @14,69 SAY 'บ        บ'
         @15,69 SAY 'บPROXIMO บ'
         @16,69 SAY 'บ        บ'
         @17,69 SAY 'บANTERIORบ'
         @18,69 SAY 'บ        บ'
         @19,69 SAY 'บPRIMERO บ'
         @20,69 SAY 'บ        บ'
         @21,69 SAY 'บULTIMO  บ'
         @22,69 SAY 'สออออออออผ'
         SETCOLOR(color6)
         @ 4,69 SAY 'หออออออออป'
         @ 5,69 SAY 'บ        บ'
         @ 6,69 SAY 'บ        บ'
         @ 7,69 SAY 'บ        บ'
         @ 8,69 SAY 'บ        บ'
         @ 9,69 SAY 'บ        บ'
         @10,69 SAY 'บ        บ'
         @11,69 SAY 'บ        บ'
         @12,69 SAY 'ฬออออออออน'
         RELEASE fi_co
         DECLARE fi_co[4]
         carga1(fi_co,'5,70~7,70~9,70~11,70')
         op1 = PROMPT(menu1,fi_co,color8,op1,salir)
         IF LASTKEY() = 9
          SETCOLOR(color4)
          @ 4,69 SAY 'หออออออออป'
          @ 5,69 SAY 'บALTAS   บ'
          @ 6,69 SAY 'บ        บ'
          @ 7,69 SAY 'บMODIFICAบ'
          @ 8,69 SAY 'บ        บ'
          @ 9,69 SAY 'บBAJAS   บ'
          @10,69 SAY 'บ        บ'
          @11,69 SAY 'บVOLVER  บ'
          @12,69 SAY 'ฬออออออออน'
         ELSE
          optar = op1
        ENDIF
      CASE cual = 2
         SETCOLOR(color4)
         @ 4,69 SAY 'หออออออออป'
         @ 5,69 SAY 'บALTAS   บ'
         @ 6,69 SAY 'บ        บ'
         @ 7,69 SAY 'บMODIFICAบ'
         @ 8,69 SAY 'บ        บ'
         @ 9,69 SAY 'บBAJAS   บ'
         @10,69 SAY 'บ        บ'
         @11,69 SAY 'บVOLVER  บ'
         @12,69 SAY 'ฬออออออออน'
         SETCOLOR(color6)
         @12,69 SAY 'ฬออออออออน'
         @13,69 SAY 'บ        บ'
         @14,69 SAY 'บ        บ'
         @15,69 SAY 'บ        บ'
         @16,69 SAY 'บ        บ'
         @17,69 SAY 'บ        บ'
         @18,69 SAY 'บ        บ'
         @19,69 SAY 'บ        บ'
         @20,69 SAY 'บ        บ'
         @21,69 SAY 'บ        บ'
         @22,69 SAY 'สออออออออผ'
         RELEASE fi_co
         DECLARE fi_co[5]
         carga1(fi_co,'13,70~15,70~17,70~19,70~21,70')
         op2 = PROMPT(menu2,fi_co,color8,op2,salir)
         IF LASTKEY() = 9
          SETCOLOR(color4)
          @12,69 SAY 'ฬออออออออน'
          @13,69 SAY 'บBUSCAR  บ'
          @14,69 SAY 'บ        บ'
          @15,69 SAY 'บPROXIMO บ'
          @16,69 SAY 'บ        บ'
          @17,69 SAY 'บANTERIORบ'
          @18,69 SAY 'บ        บ'
          @19,69 SAY 'บPRIMERO บ'
          @20,69 SAY 'บ        บ'
          @21,69 SAY 'บULTIMO  บ'
          @22,69 SAY 'สออออออออผ'
         ELSE
          optar = op2+4
         ENDIF
      ENDCASE
      tecla = LASTKEY()
   ENDDO
   cual = IF(cual > 1,cual-1,2)

   SELECT ARTIC
   DO CASE
   CASE VACIA() .and. optar # 1 .and. optar # 4 .and. optar # 0
      pan = SAVESCREEN(20,31,23,60)
      SETCOLOR(INVER)
      @20,31 CLEAR TO 23,60
      @20,31 TO 23,60 DOUBLE
      @21,36 SAY 'OPCION NO DISPONIBLE'
      @22,32 SAY 'ฤNO SE HAN REGISTRADO ALTASฤ'
      SET CURSOR OFF
      TONO(2)
      INKEY(0)
      SET CURSOR ON
      SETCOLOR(color1)
      RESTSCREEN(20,31,23,60,pan)
      LOOP

   CASE optar = 1 && ALTAS
      DO V_0C1
      graba = .F.
      DO PON_SAYC1
      DO WHILE !graba .and. LASTKEY()#27
         DO PON_GETC1
         IF LASTKEY()#27
            graba = CARTEL('DATOS CORRECTOS ? [SI] [NO]',22,1,'N')
            IF graba
               SELECT ARTIC
               ADDREC(0)
               RECLOCK(0)
               DO VA_CAMC1
               UNLOCK
            ENDIF
         ENDIF
      ENDDO

   CASE optar = 2 && MODIF.
      graba = .F.
      DO WHILE !graba .and. LASTKEY()#27
         DO PON_GETC1
         IF LASTKEY()#27
            colo_a=SETCOLOR()
            SETCOLOR(color2)
            graba = CARTEL('DATOS CORRECTOS ? [SI] [NO]',22,1,'N')
            SETCOLOR(colo_a)
            IF graba
               SELECT ARTIC
               RECLOCK(0)
               DO VA_CAMC1
               UNLOCK
            ENDIF
         ENDIF
         LOOP
      ENDDO

   CASE optar = 3 && BAJAS
      *  IF vkn <> 0
      *   CARTEL("FRUTICULTOR con Kilos Entregados  [ OK ]",12,1,'N',color3)
      *   LOOP
      *  ENDIF
      CARTEL = CARTEL('CONFIRMA BAJA ? [SI] [NO]',22,2,'N')
      IF CARTEL
       CARTEL1 = CARTEL('ESTA TOTALMENTE SEGURO ? [SI] [NO]',21,2,'N')
        IF CARTEL1
         RECLOCK(0)
         DELETE
         UNLOCK
         SKIP
         SKIP -1
         band=1
        ENDIF
      ENDIF
      LOOP

   CASE optar = 5 && BUSCAR
      pan = SAVESCREEN(5,1,24,78)
      SETCOLOR(color3)
      SOMBRA(5,1,23,77,'H1',1,'',color3)
      @ 7,1  SAY 'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน'
      @ 20,1 SAY 'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน'
      @ 17,1 SAY 'ฬอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน'
      @ 5,13 SAY ' BUSCAR '
      @18,2 SAY 'BUSCAR:'
      @ 21,2 SAY CHR(24)+CHR(25)+' PgUp PgDn mover          '+CHR(17)+'ฤู-ACEPTA'
      @ 22,2 SAY " (F2) - Cambia Clave Busqueda     ESC-salir"
      DECLARE cam[3],sep[3],ind[2],lar[2],nom[2]
      carga1(cam,"rubr~nomb~medi")
      carga2(sep,'1,1,0')
      tit=         'Rubro   Articulo                                           Medida     '
      ind[1] = 7
      lar[1] = 60
      nom[1] = 'Nombre Articulo:      :'
      ind[2] = 8
      lar[2] = 20
      nom[2] = 'Nombre de la Marca    :'
      ord_ant = STR(INDEXORD())
      SET ORDER TO 7
      SETCOLOR(color3)
      GO TOP
      BUSCARE(6,2,16,76,cam,tit,sep,ind,lar,19,2,nom,18,13)
      SETCOLOR(color1)
      SET ORDER TO &ord_ant
      RESTSCREEN(5,1,24,78,pan)
      RELEASE cam,sep,ind,lar,ord_ant

   CASE optar = 6 && PROXIMO
      SKIP
      IF EOF()
         SKIP -1
         TONO(3)
      ENDIF
      LOOP

   CASE optar = 7 && ANTERIOR
      SKIP -1
      IF BOF()
         TONO(3)
      ENDIF
      LOOP

   CASE optar = 8 && PRIMERO
      SELECT ARTIC
      GO TOP
      LOOP

   CASE optar = 9 && ULTIMO
      SELECT ARTIC
      GO BOTTOM
      LOOP

   CASE optar = 4 .OR. optar = 0 && SALIR
      SET KEY 24 TO
      SET KEY 5 TO
      RESTSCREEN(2,1,23,79,pant1)
      SETCOLOR(color3)
      IF BAND=1
       SELECT ARTIC
      ENDIF
      CLOSE ALL
      FERASE("&Ntx1")
      FERASE("&Ntx2")
      RETURN
   ENDCASE
ENDDO

******************
* PROCEDIMIENTOS *
******************
* MOSTRAR POR PANTALLA EL CONTENIDO DE LAS VARIABLES
*!*********************************************************************
*!
*!      Procedure: pon_sayc1
*!
*!      Llamado  : MAESTROS.PRG
*!
*!*********************************************************************
PROCEDURE pon_sayc1
colo_aa=SETCOLOR()
DO VIEW_ATRI
IF DELETE()
 SETCOLOR(color3)
 @22,4 SAY 'Reg.Borrado'
ELSE
 SETCOLOR(color4)
 @22,4 SAY 'Reg.Activado'
ENDIF
SETCOLOR(color9)
*Impresion en Pantalla
@ 6,23 SAY vRubr
@ 6,28 SAY Nom_Rubr
@ 8, 6 SAY vArti
@ 8,12 SAY SUBSTR(nom_Arti,1,45)
@9,12 SAY vObse
@12,10 SAY vProv
@12,15 SAY SUBSTR(Nom_Prov,1,30)
@12,47 SAY vCpro
@14,23 SAY vUnid
@15,23 SAY vFVta
@16,23 SAY vUnVt
@17,23 SAY vStck PICTURE "@Z 999999.99"
@14,47 SAY vMedi
@15,47 SAY vMrca+' '+Nom_Mrca
@16,47 SAY vClor
@17,47 SAY vCali
@18,47 SAY vUsos
@19,47 SAY vDrza
@20,47 SAY vRefe
@21,47 SAY vM2Cj
@20, 8 SAY vPUCd PICTURE "@Z 99999.999"
@20,20 SAY vPUCC PICTURE "@Z 99999.999"
@20,31 SAY vMone
@21,13 SAY vFmod PICTURE "@D"
SETCOLOR(colo_aa)
RETURN

PROCEDURE VIEW_ATRI
PRIVATE Col_Ant
Col_Ant=SETCOLOR()
IF vBMedi
 SETCOLOR("W+/BG")
ELSE
 SETCOLOR("N+/BG")
ENDIF
@14,35 SAY "Medidas"
IF vBMrca
 SETCOLOR("W+/BG")
ELSE
 SETCOLOR("N+/BG")
ENDIF
@15,35 SAY "Marca  "
IF vBClor
 SETCOLOR("W+/BG")
ELSE
 SETCOLOR("N+/BG")
ENDIF
@16,35 SAY "Color  "
IF vBCali
 SETCOLOR("W+/BG")
ELSE
 SETCOLOR("N+/BG")
ENDIF
@17,35 SAY "Calidad"
IF vBUsos
 SETCOLOR("W+/BG")
ELSE
 SETCOLOR("N+/BG")
ENDIF
@18,35 SAY "Utilizado"
IF vBDrza
 SETCOLOR("W+/BG")
ELSE
 SETCOLOR("N+/BG")
ENDIF
@19,35 SAY "Dureza"
IF vBRefe
 SETCOLOR("W+/BG")
ELSE
 SETCOLOR("N+/BG")
ENDIF
@20,35 SAY "Referencia"
IF vBM2Cj
 SETCOLOR("W+/BG")
ELSE
 SETCOLOR("N+/BG")
ENDIF
@21,35 SAY "M2 x Caja "
SETCOLOR(Col_Ant)
RETURN


* INICIALIZAR LAS VARIABLES
*!*********************************************************************
*!
*!      Procedure: v_0c1
*!
*!*********************************************************************
PROCEDURE v_0c1
*Asignar variables
vArti=SPACE(5)
Nom_Arti=SPACE(78)
vProv=SPACE(4)
Nom_Prov=SPACE(30)
vMrca=SPACE(3)
STORE SPACE(10) TO vCpro,vUnid,vFvta,vUnVt,Nom_Mrca,vClor,vUsos,vRefe,vM2Cj
vMedi=SPACE(15)
vCali=SPACE(8)
vDrza=SPACE(5)
STORE 0 TO vPuCd,vPuCC,bPuCd,bPuCC,vStck
vFmod=DATE()
vMone="$  "
vObse=SPACE(40)
RETURN

* PONER EN PANTALLA LOS GET PARA INGRESO DE DATOS
*!*********************************************************************
*!
*!      Procedure: pon_getc1
*!
*!*********************************************************************
PROCEDURE pon_getc1
XP=1
SETCOLOR(color4)
@22,2 SAY CENTRA('[F5] - Campo Anterior',67,'อ')
SETCOLOR(color1)
DO WHILE XP<=XPM
 XD=1
 DO CASE
  CASE XP = 1
   IF optar = 1
    @ 6,23 GET vRubr PICTURE '@!' VALID VAL_RUBR(@vRubr,@Nom_Rubr)
    READ
    @6,28 SAY Nom_Rubr COLOR color9
    DO VIEW_ATRI
   ENDIF
  CASE XP = 2
   IF optar = 1
    vArti=CERO(LIBRE1(vRubr,"ARTI",1),5)
    @ 8, 6 GET vArti PICTURE '@K 99999' VALID VAL_ARTI(@vArti,vRubr)
    READ
   ENDIF
  CASE XP = 3
   IF optar = 1
    @ 8,12 GET Nom_Arti PICTURE '@!S45' VALID !EMPTY(Nom_Arti)
    READ
   ENDIF
  CASE XP = 4
   @12,10 GET vProv PICTURE '@K 9999' VALID VAL_PRO(@vProv,@Nom_Prov)
   READ
   @12,15 SAY Nom_Prov COLOR color9
  CASE XP = 5
   @12,47 GET vCpro PICTURE '@K!'
   READ
  CASE XP = 6
   @14,23 GET vUnid PICTURE '@K!'
   READ
  CASE XP = 7
   @15,23 GET vFvta PICTURE '@K!'
   READ
  CASE XP = 8
   @16,23 GET vUnVt PICTURE '@K!'
   READ
  CASE XP = 9
   @17,23 GET vStck PICTURE '@K 999999.99'
   READ
  CASE XP = 10
   IF vBMedi
    @14,47 GET vMedi PICTURE '@K!'
    READ
   ENDIF
  CASE XP = 11
   IF vBMrca
    @15,47 GET vMrca PICTURE '@K 999' VALID VAL_MRCA(@vMRca,@Nom_Mrca)
    READ
   ENDIF
  CASE XP = 12
   IF vBClor
    @16,47 GET vClor PICTURE '@K!'
    READ
   ENDIF
  CASE XP = 13
   IF vBCali
    @17,47 GET vCali PICTURE '@K!'
    READ
   ENDIF
  CASE XP = 14
   IF vBUsos
    @18,47 GET vUsos PICTURE '@K!'
    READ
   ENDIF
  CASE XP = 15
   IF vBDrza
    @19,47 GET vDrza PICTURE '@K!'
    READ
   ENDIF
  CASE XP = 16
   IF vBRefe
    @20,47 GET vRefe PICTURE '@K!'
    READ
   ENDIF
  CASE XP = 17
   IF vBM2Cj
    @21,47 GET vM2Cj PICTURE '@K!'
    READ
   ENDIF
  CASE XP = 18
   @20, 8 GET vPuCd PICTURE '@K 99999.999'
   READ
*  CASE XP = 17
*   @20,20 GET vPuCC PICTURE '@K 99999.999'
*   READ
  CASE XP = 19
   @20,31 GET vMone PICTURE '@K!'
   READ
  CASE XP = 20
   @9,12 GET vObse PICTURE '@K!'
   READ
 ENDCASE
 IF LASTKEY()=27
  CLEAR GETS
  XP = 21
 ELSE
  XP = XP + XD
  IF XP = 1 .AND. optar <> 1
   XP = 2
  ENDIF
 ENDIF
ENDDO
RETURN

* CARGAR CAMPOS POR DATOS DE VARIABLES
*!*********************************************************************
*!
*!      Procedure: VAR_CAMP1
*!
*!*********************************************************************
PROCEDURE va_camc1
REPLACE rubr WITH vRubr
REPLACE arti WITH vArti
REPLACE nomb WITH Nom_Arti
REPLACE prov WITH vProv
REPLACE cpro WITH vCpro
REPLACE unid WITH vUnid
REPLACE fvta WITH vFVta
REPLACE unvt WITH vUnVt
REPLACE stck WITH vStck
REPLACE medi WITH vMedi
REPLACE mrca WITH vMrca
REPLACE clor WITH vClor
REPLACE cali WITH vCali
REPLACE usos WITH vUsos
REPLACE drza WITH vDrza
REPLACE refe WITH vRefe
REPLACE m2cj WITH vM2Cj
REPLACE pucd WITH vPuCd
REPLACE pucc WITH vPuCc
REPLACE mone WITH vMone
REPLACE obse WITH vObse
IF vPuCd#bPuCd .OR. vPuCC#bPuCC
 REPLACE fmod WITH DATE()
ENDIF
COMMIT
RETURN

* CARGAR VARIABLES CON DATOS DE CAMPOS
*!*********************************************************************
*!
*!      Procedure: CA_VARC2
*!
*!*********************************************************************
PROCEDURE CA_VARC2
SELECT ARTIC
vRubr=rubr
VAL_RUBR(@vRubr,@Nom_Rubr)
vArti=arti
Nom_Arti=nomb
vProv=prov
Nom_Prov=PROVE->Nomb
vCpro=cpro
vUnid=unid
vFvta=fvta
vUnVt=unvt
vStck=stck
vMedi=medi
vMrca=mrca
Nom_Mrca=MARCA->nomb
vClor=clor
vCali=cali
vUsos=usos
vDrza=drza
vRefe=refe
vM2Cj=m2Cj
vPuCd=pucd
vPuCC=pucc
bPuCd=pucd
bPuCC=pucc
vMone=mone
vFmod=fmod
vObse=obse
RETURN

*Control de RUBROS
FUNCTION VAL_RUBR
PARAMETERS fRubr,fNom_Rubr
PRIVATE antsel
antsel=ALLTRIM(STR(SELECT()))
SELECT RUBRO
SET ORDER TO 1
vRubr=PADL(fRubr,4,' ')
SEEK vRubr
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
  vRubr=rubr
  Nom_Rubr=nomb
  vBMedi=bmedi
  vBMrca=bmrca
  vBClor=bclor
  vBCali=bCali
  vBUsos=busos
  vBDrza=bdrza
  vBRefe=brefe
  vBM2Cj=bM2Cj
  SELECT &antsel
  RETURN .T.
 ELSE
  SELECT &antsel
  RETURN .F.
 ENDIF
ENDIF
Nom_Rubr=nomb
vBMedi=bmedi
vBMrca=bmrca
vBClor=bclor
vBCali=bCali
vBUsos=busos
vBDrza=bdrza
vBRefe=brefe
vBM2Cj=bM2Cj
SELECT &antsel
RETURN .T.

*Control de Lista de Precios
FUNCTION VAL_PRO
PARAMETERS fProv,fNom_Prov
PRIVATE antsel
antsel=ALLTRIM(STR(SELECT()))
SELECT PROVE
SET ORDER TO 1
fProv = CERO(VAL(fProv),4)
SEEK fProv
IF .NOT.FOUND()
 *Ayuda de Lista de Precios
 archivo='PROVE'
 campi='prov~nomb'
 ntx1=1
 ntx2=2
 l1=4
 l2=30
 camp1='prov'
 IF AYUD1(campi,ntx1,ntx2,l1,l2,camp1,archivo) = .T.
  fProv=prov
  Nom_Prov=nomb
  SELECT &antsel
  RETURN .T.
 ELSE
  SELECT &antsel
  RETURN .F.
 ENDIF
ENDIF
Nom_Prov=nomb
SELECT &antsel
RETURN .T.

*Control de ARTICULOS
FUNCTION VAL_ARTI
PARAMETERS fArti,fRubr
PRIVATE antsel
antsel=ALLTRIM(STR(SELECT()))
SELECT ARTIC
SET ORDER TO 6
vArti = CERO(VAL(fArti),5)
SEEK vRubr+vArti
IF FOUND()
 CARTEL("CเDIGO YA EXISTE [ OK ]",12,1,"N",color3)
 SELECT &antsel
 RETURN .F.
ENDIF
SELECT &antsel
RETURN .T.

*Control Marcas
FUNCTION VAL_MRCA
PARAMETERS fMrca,fNom_Mrca
PRIVATE antsel
antsel=ALLTRIM(STR(SELECT()))
SELECT MARCA
SET ORDER TO 1
fMrca = CERO(VAL(fMrca),3)
SEEK fMRca
IF .NOT.FOUND()
 *Ayuda de Marcas
 archivo='MARCA'
 campi='mrca~nomb'
 ntx1=1
 ntx2=2
 l1=3
 l2=20
 camp1='mrca'
 IF AYUD1(campi,ntx1,ntx2,l1,l2,camp1,archivo) = .T.
  fMrca=Mrca
  Nom_Mrca=nomb
  SELECT &antsel
  RETURN .T.
 ELSE
  SELECT &antsel
  RETURN .F.
 ENDIF
ENDIF
Nom_Mrca=nomb
SELECT &antsel
RETURN .T.


*Busca el primer codigo libre en la base de datos Activa
*CAMPO = Nombre del campo clave Numerico que debe buscar
FUNCTION LIBRE1
PARAMETERS fRubr,campo,p
IF PCOUNT() = 1
 PRIVATE p
 p=1
ENDIF
ord_ant = STR(INDEXORD())
SET ORDER TO 9
GO BOTTOM
p=VAL(arti)+1
*    SEEK fRubr
*    IF FOUND()
*     vcam = &campo
*     DO WHILE .NOT.EOF()
*      IF rubr=fRubr
*       vcam = &campo
*       IF VAL(vcam) = p
*        SKIP
*       ELSE
*        EXIT
*       ENDIF
*      ENDIF
*      p=p+1
*     ENDDO
*    ENDIF
SET ORDER TO &ord_ant
RETURN (p)
