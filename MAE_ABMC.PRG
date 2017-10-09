***************************************************
* ALTAS, BAJAS, MODIFICACIONES Y CONSULTAS GRALES.*
***************************************************
PRIVATE menu1[4],menu2[5],fi_co[1],salir[2],pant1,nro_reg,optar,N,ok11,VAR1,band
PRIVATE col_ant
PRIVATE CodPrin
col_ant=SETCOLOR()
STORE 0 TO ok11,band
n=1
SELECT &alia
GO TOP
*Abrir archivo de estructuras
AREA()
estru='ESTRUC'+estructu
IF !USAR("&estru","TT2")
 QUIT
ENDIF
*Crear y cargar variables del archivo en variables de programa
GO TOP
ving='INGRES'+ingreso
DO WHILE !EOF()
 mcampo=TRIM(campo)
 IF UPPER(tipo)="C"
  PRIVATE v&mcampo
  v&mcampo=SPACE(lon)
 ENDIF
 IF UPPER(tipo)="N"
  PRIVATE v&mcampo
  v&mcampo=0
 ENDIF
 IF UPPER(tipo)="D"
  PRIVATE v&mcampo
  v&mcampo=CTOD('  /  /    ')
 ENDIF
 IF UPPER(tipo)="L"
  PRIVATE v&mcampo
  v&mcampo=.F.
 ENDIF
 SKIP
ENDDO
pant1 = SAVESCREEN(4,0,23,79)
GO TOP
* HAGO LA PANTALLA DEL ARCHIVO A TRATAR
SETCOLOR(color4)
SOMBRA(4,1,22,78,'H1',2,'',color4,'N+')
DO &ving
SETCOLOR(color4)
@4,3 SAY CENTRA(titu,71,'อ')
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
CARGA1(menu1,'`ALTAS   ~`MODIFICA~`BAJAS   ~`VOLVER  ')
CARGA1(menu2,'`BUSCAR  ~`PROXIMO ~`ANTERIOR~P`RIMERO ~`ULTIMO  ')
CARGA2(salir,'13,9')
* SI ESTA VACIA POSICIONO OPCION EN ALTAS SINO EN BUSCAR
SELECT &alia
IF VACIA()
   cual = 2
ELSE
   cual = 1
ENDIF
op1 = 1
op2 = 1
DO WHILE .T.
* SI LA BASE ESTA VACIA INIC.VARIABLES D/L CONTRARIO LAS CARGO C/DATOS D/L CAMPOS
   SELECT &alia
   IF !VACIA()
    DO camp_varc
   ELSE
    DO var_0c
   ENDIF
* MUESTRO CONTENIDO DE VARIABLES
   DO poner_sayc
   tecla = 9
   DO WHILE tecla = 9
    SETCOLOR(color5)
    @ 24,2 SAY CENTRA(CHR(24)+CHR(25)+CHR(26)+CHR(27)+'  Mover      TAB-Cambia Menu   '+CHR(17)+'ฤู-ACEPTA',78)
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
     *RELEASE fi_co
     DECLARE fi_co[4]
     carga1(fi_co,'5,70~7,70~9,70~11,70')
     op1 = PROMPT(menu1,fi_co,'I',op1,salir)
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
     *RELEASE fi_co
     DECLARE fi_co[5]
     CARGA1(fi_co,'13,70~15,70~17,70~19,70~21,70')
     op2 = PROMPT(menu2,fi_co,'I',op2,salir)
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
   SELECT &alia
   DO CASE
   CASE VACIA() .AND. optar # 1 .AND. optar # 4 .AND. optar # 0
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
    SETCOLOR(col_ant)
    RESTSCREEN(20,31,23,60,pan)
    LOOP
   CASE optar = 1 && ALTAS
    DO var_0c
    graba = .F.
    DO poner_sayc
    DO WHILE !graba .and. LASTKEY()#27
     DO poner_getc
     IF LASTKEY()#27
      colo_a=SETCOLOR()
      SETCOLOR(color3)
      graba = CARTEL('จ DATOS CORRECTOS ? [SI] [NO]',22,1,'N')
      SETCOLOR(colo_a)
      IF graba
       SELECT &alia
       ADDREC(0)
       RECLOCK(0)
       DO var_campc
       UNLOCK
      ENDIF
     ENDIF
    ENDDO
   CASE optar = 2 && MODIF.
    graba = .F.
    DO WHILE !graba .and. LASTKEY()#27
     DO poner_getc
     IF LASTKEY()#27
      colo_a=SETCOLOR()
      SETCOLOR(color3)
      graba = CARTEL('จ DATOS CORRECTOS ? [SI] [NO]',22,1,'N')
      SETCOLOR(colo_a)
      IF graba
       RECLOCK(0)
       DO var_campc
       UNLOCK
      ENDIF
     ENDIF
     LOOP
    ENDDO
   CASE optar = 3 && BAJAS
    *Rubros
    IF alia="RUBROS"
     *Articulos
     AREA()
     IF USAR('MAEARTIC','ARTIC')
      *Por Rubro,Proveedor
      IF !FILE('MAEARTIC&vRDD.')
       INDEX ON rubr+prov TO MAEARTIC
      ENDIF
      SET INDEX TO MAEARTIC
     ELSE
      QUIT
     ENDIF
     SEEK vRubr
     IF FOUND()
      CARTEL = CARTEL('NO PUEDE DAR DE BAJA, RUBRO UTILIZADO [ OK ]',22,2,'N')
      CLOSE
      SELECT &alia
      LOOP
     ENDIF
     CLOSE
     SELECT &alia
    ENDIF

    IF alia="MARCAS"
     *Articulos
     AREA()
     IF USAR('MAEARTIC','ARTIC')
      *Por MARCA
      IF !FILE('MAEARTI3&vRDD.')
       INDEX ON mrca TO MAEARTI3
      ENDIF
      SET INDEX TO MAEARTI3
     ELSE
      QUIT
     ENDIF
     SEEK vMrca
     IF FOUND()
      CARTEL = CARTEL('NO PUEDE DAR DE BAJA, MARCA UTILIZADA [ OK ]',22,2,'N')
      CLOSE
      SELECT &alia
      LOOP
     ENDIF
     CLOSE
     SELECT &alia
    ENDIF

    IF alia="PROVE"
     *Articulos
     AREA()
     IF USAR('MAEARTIC','ARTIC')
      *Por MARCA
      IF !FILE('MAEARTI2&vRDD.')
       INDEX ON prov+mrca TO MAEARTI2
      ENDIF
      SET INDEX TO MAEARTI2
     ELSE
      QUIT
     ENDIF
     SEEK vProv
     IF FOUND()
      CARTEL = CARTEL('NO PUEDE DAR DE BAJA, LISTA UTILIZADA [ OK ]',22,2,'N')
      CLOSE
      SELECT &alia
      LOOP
     ENDIF
     CLOSE
     SELECT &alia
    ENDIF

    colo_a=SETCOLOR()
    SETCOLOR(color3)
    CARTEL = CARTEL('จ CONFIRMA BAJA ? [SI] [NO]',22,2,'N')
    IF CARTEL
     CARTEL1 = CARTEL('จ ESTA TOTALMENTE SEGURO ? [SI] [NO]',21,2,'N')
     IF CARTEL1
      RECLOCK(0)
      DELETE
      UNLOCK
      SKIP
      SKIP -1
      band=1
      var1=''
      SELECT TT2
     ENDIF
    ENDIF
    SETCOLOR(colo_a)
    LOOP
   CASE optar = 5 && BUSCAR
    IF alia="CLIENTES"
     pan=savescreen(5,2,24,72)
     SETCOLOR(color3)
     sombra(5,2,23,71,'V1',3,'',color3)
     @7,2 say'ฬออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน'
     @20,2 say'วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ'
     @17,2 say'ฬออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน'
     @5,38 say' BUSCAR '
     @18,3 SAY'BUSCAR:'
     @19,3 SAY SPACE(38)
     @21,13 say chr(24)+chr(25)+' PgUp PgDn Mover         '+chr(17)+'ฤู Acepta'
     @22,13 say'F2-Cambia Clave Busqueda  [Esc]-Salir'
     DECLARE cam[3],sep[3],ind[2],lar[2],nom[2]
     titu='Nฃmero  Nombre '+tit+'   Direcciขn  '
     CARGA2(sep,'5,3,0')
     CARGA1(cam,"clie~nomb~domi")
     ind[1]=1
     lar[1]=6
     nom[1]='Codigo Cliente:'
     ind[2]=2
     lar[2]=30
     nom[2]='Nombre Cliente:'
     SET ORDER TO 2
     GO TOP
     BUSCARE(6,4,16,70,cam,titu,sep,ind,lar,19,4,nom,18,41)
     SET ORDER TO 1
     SETCOLOR(color1)
     RESTSCREEN(5,2,24,72,pan)
     *RELEASE cam,sep,ind,lar,titu,nom
    ELSE
     pan = SAVESCREEN(5,29,24,78)
     SETCOLOR(color3)
     SOMBRA(5,29,23,77,'H1',1,'',color3)
     SETCOLOR(color3)
     @ 7,29  SAY 'ฬอออออออออออออออออออออออออออออออออออออออออออออออน'
     @ 20,29 SAY 'วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ'
     @ 17,29 SAY 'ฬอออออออออออออออออออออออออออออออออออออออออออออออน'
     @ 5,41 SAY ' BUSCAR '
     @18,30 SAY 'BUSCAR:'
     @ 21,30 SAY CHR(24)+CHR(25)+' PgUp PgDn mover          '+CHR(17)+'ฤู-ACEPTA'
     @ 22,30 SAY "F2-cambia clave      ESC-salir"
     IF long2=0
      DECLARE cam[2],sep[2],ind[1],lar[1],nom[1]
     ELSE
      DECLARE cam[2],sep[2],ind[2],lar[2],nom[2]
     ENDIF
     CARGA1(cam,camp)
     CARGA2(sep,'1,1')
     ind[1] = 1
     lar[1] = long1
     nom[1] = 'CODIGO      '
     ord_ant = STR(INDEXORD())
     IF long2<>0
      ind[2] = 2
      lar[2] = long2
      nom[2] = 'NOMBRE'
      SET ORDER TO 2
     ELSE
      SET ORDER TO 1
     ENDIF
     SETCOLOR(color3)
     GO TOP
     BUSCARE(6,31,16,75,cam,tit,sep,ind,lar,19,31,nom,18,41)
     SETCOLOR(color1)
     SET ORDER TO &ord_ant
     RESTSCREEN(5,29,24,78,pan)
     *RELEASE cam,sep,ind,lar,ord_ant
    ENDIF
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
    SELECT &alia
    GO TOP
    LOOP
   CASE optar = 9 && ULTIMO
    SELECT &alia
    GO BOTTOM
    LOOP
   CASE optar = 4 .OR. optar = 0 && SALIR
    SET KEY 24 TO
    SET KEY 5 TO
    RESTSCREEN(4,0,23,79,pant1)
    SETCOLOR(col_ant)
*   IF BAND=1
*    SELECT &alia
*   PACK   &&Se realizara desde la Opcion 4-2
*   ENDIF
    SELECT TT2
    CLOSE
    RETURN
   ENDCASE
ENDDO

******************
* PROCEDIMIENTOS *
******************
* MOSTRAR POR PANTALLA EL CONTENIDO DE LAS VARIABLES
*!*************************************************
*!Procedure: PONER_SAYc
*!*************************************************
PROCEDURE poner_sayc
SELECT TT2
GO TOP
colo_aa=SETCOLOR()
SETCOLOR(color5)
DO WHILE !EOF()
 mcampo=TRIM(campo)
 vmuest='MUEST'+muestra
 vfila=fila
 vcol=col
 vmasc=TRIM(mascara)
 IF UPPER(tipo)='N'
  @vfila,vcol SAY STR(v&mcampo,lon,decimal) PICTURE "&vmasc"
 ELSE
  IF UPPER(tipo)='C'
   @vfila,vcol SAY v&mcampo PICTURE "&vmasc"
  ELSE
   IF UPPER(tipo)='D'
    @vfila,vcol SAY DTOC(v&mcampo)
   ELSE
    IF UPPER(tipo)='L'
     @vfila,vcol SAY v&mcampo PICTURE "&vMasc"
    ENDIF
   ENDIF
  ENDIF
 ENDIF
 IF VAL(muestra)<>0
  DO &vmuest
 ENDIF
 IF RECNO()=1
  var1=v&mcampo
 ENDIF
 SKIP
enddo
SETCOLOR(colo_aa)
SELECT &alia
RETURN

* INICIALIZAR LAS VARIABLES
*!***************************************
*!Procedure: VAR_0c
*!***************************************
PROCEDURE var_0c
SELECT TT2
GO TOP
DO WHILE !EOF()
 mcampo=TRIM(campo)
 IF tipo='C'
  v&mcampo=SPACE(lon)
 ENDIF
 IF tipo='N'
  v&mcampo=0
 ENDIF
 IF tipo='D'
  v&mcampo=CTOD('  /  /    ')
 ENDIF
 IF tipo='L'
  v&mcampo=.F.
 ENDIF
 SKIP
ENDDO
SELECT &alia
RETURN

* PONER EN PANTALLA LOS GET PARA INGRESO DE DATOS
*!******************************************
*!Procedure: PONER_GETc
*!******************************************
PROCEDURE poner_getc
SET KEY 24 TO avance
SET KEY 5 TO retro
@24,0 CLEAR TO 24,79
SETCOLOR(color1)
SELECT TT2
IF optar = 1
 n=1
ELSE
 n=2
ENDIF
DO WHILE .T.
  ok=0
  GOTO n
  mcampo=TRIM(campo)
  IF n=1
   IF vinicio#99999999
    SELECT TT2
    GO n
    SELECT &alia
    *Buscar un Codigo libre o el ultimo
    GO BOTTOM
    IF alia="CLIENTES"
     rango=LIBRE(mcampo,vinicio)
    ELSE
     rango=VAL(&mcampo)+1
    ENDIF
    SELECT TT2
    GO n
    v&mcampo=CERO(rango,lon)
   ELSE
    v&mcampo=SPACE(lon)
   ENDIF
  ENDIF
  vlon=lon
  vpedir=pedir
  vtipo=tipo
  vvpro = FIELD(10)
  vproc="PROCED"+&vvpro
  vmuest="MUEST"+muestra
  vranInf=ran_inf
  vranSup=ran_sup
  vfila=fila
  vcol=col
  vayuda="AYUDA"+pan_ayuda
  IF VAL(pan_ayuda)<>0
   mens='[F3] - AYUDA  '+mensaje
   SET KEY -2 TO &vayuda
  ELSE
   mens=mensaje
  ENDIF
  IF vpedir='1'
   colo_a=SETCOLOR()
   SETCOLOR(color3)
   @24,2 SAY ALLTRIM(mens)+'.... [Esc] p/Salir'
   SETCOLOR(colo_a)
   vmasc=TRIM(mascara)
   IF vtipo='N'
    @fila,col GET v&mcampo PICTURE "&vmasc" RANGE vranInf,vranSup
   ELSE
    IF vtipo='C'
     @fila,col GET v&mcampo PICTURE "&vmasc"
    ELSE
     IF vtipo='D'
      @fila,col GET v&mcampo
     ELSE
      IF vtipo='L'
       @fila,col GET v&mcampo PICTURE "&vMasc"
      ENDIF
     ENDIF
    ENDIF
   ENDIF
   READ
   SET KEY -2 TO
   @24,2 SAY SPACE(66)
   IF LASTKEY()=27
    ok=2
   ELSE
    IF VAL(&vvpro)<>0
     DO &vproc
     SELECT TT2
     IF ok=1
      LOOP
     ENDIF
    ENDIF
    IF VAL(muestra)<>0
     colo_aa=SETCOLOR()
     SETCOLOR(color5)
     DO &vmuest
     SETCOLOR(colo_aa)
    ENDIF
    col_anti=SETCOLOR()
    SETCOLOR(color5)
    IF vtipo='N'
     @fila,col SAY STR(v&mcampo,lon,decimal) PICTURE "&vmasc"
    ELSE
     IF vtipo='C'
      @fila,col SAY v&mcampo PICTURE "&vmasc"
     ELSE
      IF vtipo='D'
       @fila,col SAY DTOC(v&mcampo)
      ELSE
       IF vtipo='L'
        @fila,col SAY v&mcampo PICTURE "&vMasc"
       ENDIF
      ENDIF
     ENDIF
    ENDIF
    SETCOLOR(col_anti)
   ENDIF LASTKEY()=27
  ENDIF vpedir='1'
  IF ok<>2
   DO CASE
    CASE LASTKEY()=13
     IF n<>LASTREC()
      n=n+1
      LOOP
     ENDIF
    CASE LASTKEY()=3
     IF n<>LASTREC()
      n=n+1
      LOOP
     ENDIF
    CASE LASTKEY()=18
     IF n>1
      IF optar=2 .AND. n=2
       LOOP
      ELSE
       n=n-1
      ENDIF
     ENDIF
     LOOP
    CASE LASTKEY()=27
     EXIT
   ENDCASE
   IF n=LASTREC()
    SET KEY 24 TO
    SET KEY 5 TO
    RETURN
   ELSE
    n=n+1
   ENDIF
  ELSE
   SET KEY 24 TO
   SET KEY 5 TO
   RETURN
  ENDIF ok=2
ENDDO
SET KEY 24 TO
SET KEY 5 TO
RETURN

* CARGAR CAMPOS POR DATOS DE VARIABLES
*!*************************************
*!      Procedure: VAR_CAMP
*!************************************
PROCEDURE var_campc
SELECT TT2
GO TOP
nn=1
DO WHILE !EOF()
 mcampo=TRIM(campo)
 SELECT &alia
 RECLOCK(1)
 REPLACE &mcampo WITH v&mcampo
 UNLOCK
 SELECT TT2
 IF ok11=0.AND.nn=1 .AND. optar=1
  RECLOCK(1)
  REPLACE ran_sup WITH VAL(v&mcampo)
  UNLOCK
  nn=0
 ENDIF
 IF tipo='C'
  v&mcampo=SPACE(lon)
 ENDIF
 IF tipo='N'
  v&mcampo=0
 ENDIF
 IF tipo='L'
  v&mcampo=.F.
 ENDIF
 SKIP
ENDDO
DO poner_sayc
SELECT TT2
DBCOMMITALL()
RETURN

* CARGAR VARIABLES CON DATOS DE CAMPOS
*!***************************************
*!Procedure: CAMP_VAR
*!***************************************
PROCEDURE camp_varc
PRIVATE vari,i
SELECT TT2
GO TOP
n=1
i=1
DO WHILE !EOF()
 SELECT &alia
 IF TYPE(FIELDNAME(i))='N'
  vvar=0
 ELSE
  IF TYPE(FIELDNAME(i))='C'
   vvar=""
  ELSE
   IF TYPE(FIELDNAME(i))='D'
    vvar=CTOD('  /  /    ')
   ELSE
    IF TYPE(FIELDNAME(i))='L'
     vvar=.F.
    ENDIF
   ENDIF
  ENDIF
 ENDIF
 vari=FIELDNAME(i)
 vvar = &vari
 SELECT TT2
 mcampo=TRIM(campo)
 v&mcampo=vvar
 SKIP
 i=i+1
ENDDO
RETURN

PROCEDURE avance
KEYBOARD CHR(3)
RETURN

PROCEDURE retro
KEYBOARD CHR(18)
RETURN
