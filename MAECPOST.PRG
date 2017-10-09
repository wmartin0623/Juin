************************************************************
*       ALTA, MODIF. Y CONSULTA DE C.POSTALES              *
************************************************************
PRIVATE menu1[4],menu2[5],fi_co[1],salir[2],pant1
PRIVATE vpcia,nom_pcia,vdpto,nom_dpto,vcpos
PUBLIC optar,band
vpcia = SPACE(2)
vdpto = SPACE(3)
STORE SPACE(15) TO nom_pcia,nom_dpto
vcpos = SPACE(8)
optar = 2
XPM=3  &&CANTIDAD DE GETS QUE TIENE EL PROGRAMA
BAND=0
pant1 = SAVESCREEN(2,1,24,79)

*Provincias
AREA()
OPEN_DBF("MAEPROVI","PROVIN","C","MAEPROVI","pcia","NOMPROVI","nomb")

*Departamentos
AREA()
OPEN_DBF("MAEDEPAR","DEPAR","C","MAEDEPAR","dpto","NOMDEPAR","nomb")

SELECT CPOST
SET RELATION TO pcia INTO PROVIN,TO dpto INTO DEPAR

* HAGO LA PANTALLA DEL ARCHIVO A TRATAR

SETCOLOR(color4)
SOMBRA(4,1,22,78,'H1',1,'',color4,'N+')
DO ingres14
SETCOLOR(color4)
@4,7 SAY CENTRA(' CODIGOS POSTALES ',60,'Í')

SETCOLOR(color4)
@ 4,69 SAY 'ËÍÍÍÍÍÍÍÍ»'
@ 5,69 SAY 'ºALTAS   º'
@ 6,69 SAY 'º        º'
@ 7,69 SAY 'ºMODIFICAº'
@ 8,69 SAY 'º        º'
@ 9,69 SAY 'ºBAJAS   º'
@10,69 SAY 'º        º'
@11,69 SAY 'ºVOLVER  º'
@12,69 SAY 'ÌÍÍÍÍÍÍÍÍ¹'
@13,69 SAY 'ºBUSCAR  º'
@14,69 SAY 'º        º'
@15,69 SAY 'ºPROXIMO º'
@16,69 SAY 'º        º'
@17,69 SAY 'ºANTERIORº'
@18,69 SAY 'º        º'
@19,69 SAY 'ºPRIMERO º'
@20,69 SAY 'º        º'
@21,69 SAY 'ºULTIMO  º'
@22,69 SAY 'ÊÍÍÍÍÍÍÍÍ¼'

* ACTIVO EL PANEL DE CONTROL

carga1(menu1,'`ALTAS   ~`MODIFICA~`BAJAS   ~`VOLVER  ')
carga1(menu2,'`BUSCAR  ~`PROXIMO ~`ANTERIOR~P`RIMERO ~`ULTIMO  ')
carga2(salir,'13,9')

* SI ESTA VACIA POSICIONO OPCION EN ALTAS SINO EN BUSCAR

SELECT CPOST

IF VACIA()
   cual = 2
ELSE
   cual = 1
ENDIF
op1 = 1
op2 = 1
DO WHILE .T.

   * SI LA BASE ESTA VACIA INIC.VARIABLES D/L CONTRARIO LAS CARGO C/DATOS D/L CAMPOS
   SELECT CPOST
   IF !VACIA()
      DO CA_VARC14
   ELSE
      DO V_0C14
   ENDIF
   * MUESTRO CONTENIDO DE VARIABLES
   DO PON_SAYC14
   tecla = 9
   DO WHILE tecla = 9
      colur=SETCOLOR()
      SETCOLOR(color3)
      @ 24,2 SAY CENTRA(CHR(24)+CHR(25)+CHR(26)+CHR(27)+'  Mover      TAB-Cambia Menu   '+CHR(17)+'ÄÙ-ACEPTA  ESC-Salir',78)
      SETCOLOR(colur)
      cual = IF(cual < 2,cual+1,1)
      DO CASE
      CASE cual = 1
         SETCOLOR(color4)
         @12,69 SAY 'ÌÍÍÍÍÍÍÍÍ¹'
         @13,69 SAY 'ºBUSCAR  º'
         @14,69 SAY 'º        º'
         @15,69 SAY 'ºPROXIMO º'
         @16,69 SAY 'º        º'
         @17,69 SAY 'ºANTERIORº'
         @18,69 SAY 'º        º'
         @19,69 SAY 'ºPRIMERO º'
         @20,69 SAY 'º        º'
         @21,69 SAY 'ºULTIMO  º'
         @22,69 SAY 'ÊÍÍÍÍÍÍÍÍ¼'
         SETCOLOR(color6)
         @ 4,69 SAY 'ËÍÍÍÍÍÍÍÍ»'
         @ 5,69 SAY 'º        º'
         @ 6,69 SAY 'º        º'
         @ 7,69 SAY 'º        º'
         @ 8,69 SAY 'º        º'
         @ 9,69 SAY 'º        º'
         @10,69 SAY 'º        º'
         @11,69 SAY 'º        º'
         @12,69 SAY 'ÌÍÍÍÍÍÍÍÍ¹'
         DECLARE fi_co[4]
         carga1(fi_co,'5,70~7,70~9,70~11,70')
         op1 = PROMPT(menu1,fi_co,color8,op1,salir)
         IF LASTKEY() = 9
          SETCOLOR(color4)
          @ 4,69 SAY 'ËÍÍÍÍÍÍÍÍ»'
          @ 5,69 SAY 'ºALTAS   º'
          @ 6,69 SAY 'º        º'
          @ 7,69 SAY 'ºMODIFICAº'
          @ 8,69 SAY 'º        º'
          @ 9,69 SAY 'ºBAJAS   º'
          @10,69 SAY 'º        º'
          @11,69 SAY 'ºVOLVER  º'
          @12,69 SAY 'ÌÍÍÍÍÍÍÍÍ¹'
         ELSE
          optar = op1
        ENDIF
      CASE cual = 2
         SETCOLOR(color4)
         @ 4,69 SAY 'ËÍÍÍÍÍÍÍÍ»'
         @ 5,69 SAY 'ºALTAS   º'
         @ 6,69 SAY 'º        º'
         @ 7,69 SAY 'ºMODIFICAº'
         @ 8,69 SAY 'º        º'
         @ 9,69 SAY 'ºBAJAS   º'
         @10,69 SAY 'º        º'
         @11,69 SAY 'ºVOLVER  º'
         @12,69 SAY 'ÌÍÍÍÍÍÍÍÍ¹'
         SETCOLOR(color6)
         @12,69 SAY 'ÌÍÍÍÍÍÍÍÍ¹'
         @13,69 SAY 'º        º'
         @14,69 SAY 'º        º'
         @15,69 SAY 'º        º'
         @16,69 SAY 'º        º'
         @17,69 SAY 'º        º'
         @18,69 SAY 'º        º'
         @19,69 SAY 'º        º'
         @20,69 SAY 'º        º'
         @21,69 SAY 'º        º'
         @22,69 SAY 'ÊÍÍÍÍÍÍÍÍ¼'
         DECLARE fi_co[5]
         carga1(fi_co,'13,70~15,70~17,70~19,70~21,70')
         op2 = PROMPT(menu2,fi_co,color8,op2,salir)
         IF LASTKEY() = 9
          SETCOLOR(color4)
          @12,69 SAY 'ÌÍÍÍÍÍÍÍÍ¹'
          @13,69 SAY 'ºBUSCAR  º'
          @14,69 SAY 'º        º'
          @15,69 SAY 'ºPROXIMO º'
          @16,69 SAY 'º        º'
          @17,69 SAY 'ºANTERIORº'
          @18,69 SAY 'º        º'
          @19,69 SAY 'ºPRIMERO º'
          @20,69 SAY 'º        º'
          @21,69 SAY 'ºULTIMO  º'
          @22,69 SAY 'ÊÍÍÍÍÍÍÍÍ¼'
         ELSE
          optar = op2+4
         ENDIF
      ENDCASE
      tecla = LASTKEY()
   ENDDO
   cual = IF(cual > 1,cual-1,2)

   SELECT CPOST
   DO CASE
   CASE VACIA() .and. optar # 1 .and. optar # 4 .and. optar # 0
      pan = SAVESCREEN(20,31,23,60)
      SETCOLOR(INVER)
      @20,31 CLEAR TO 23,60
      @20,31 TO 23,60 DOUBLE
      @21,36 SAY 'OPCION NO DISPONIBLE'
      @22,32 SAY 'ÄNO SE HAN REGISTRADO ALTASÄ'
      SET CURSOR OFF
      TONO(2)
      INKEY(0)
      SET CURSOR ON
      SETCOLOR(color1)
      RESTSCREEN(20,31,23,60,pan)
      LOOP

   CASE optar = 1 && ALTAS
      DO V_0C14
      graba = .F.
      DO PON_SAYC14
      DO WHILE !graba .and. LASTKEY()#27
         DO PON_GETC14
         IF LASTKEY()#27
            graba = CARTEL('DATOS CORRECTOS ? [SI] [NO]',22,1,'N')
            IF graba
               SELECT CPOST
               ADDREC(0)
               RECLOCK(0)
               DO VA_CAMC14
               UNLOCK
            ENDIF
         ENDIF
      ENDDO

   CASE optar = 2 && MODIF.
      graba = .F.
      DO WHILE !graba .and. LASTKEY()#27
         DO PON_GETC14
         IF LASTKEY()#27
            colo_a=SETCOLOR()
            SETCOLOR(color2)
            graba = CARTEL('DATOS CORRECTOS ? [SI] [NO]',22,1,'N')
            SETCOLOR(colo_a)
            IF graba
               RECLOCK(0)
               DO VA_CAMC14
               UNLOCK
            ENDIF
         ENDIF
         LOOP
      ENDDO

   CASE optar = 3 && BAJAS
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
      pan = SAVESCREEN(5,29,24,78)
      SETCOLOR(color3)
      SOMBRA(5,29,23,77,'H1',1,'',color3)
      @ 7,29  SAY 'ÌÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹'
      @ 20,29 SAY 'ÇÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¶'
      @ 17,29 SAY 'ÌÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹'
      @ 5,41 SAY ' BUSCAR '
      @18,30 SAY 'BUSCAR:'
      @ 21,30 SAY CHR(24)+CHR(25)+' PgUp PgDn mover          '+CHR(17)+'ÄÙ-ACEPTA'
      @ 22,30 SAY " (F2) - Cambia Clave Busqueda     ESC-salir"
      DECLARE cam[5],sep[5],ind[5],lar[1],nom[1]
      carga1(cam,"pcia~PROVIN->sigl~dpto~DEPAR->sigl~cpos")
      carga2(sep,'1,8,1,8,0')
      tit=         'Pcia.          Dpto.           Cpos.           '
      ind[1] = 1
      lar[1] = 5
      nom[1] = 'Pcia. + Dpto. :'
      ord_ant = STR(INDEXORD())
      SET ORDER TO 1
      SETCOLOR(color3)
      GO TOP
      BUSCARE(6,31,16,75,cam,tit,sep,ind,lar,19,31,nom,18,41)
      SETCOLOR(color1)
      SET ORDER TO &ord_ant
      RESTSCREEN(5,29,24,78,pan)

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
      SELECT CPOST
      GO TOP
      LOOP

   CASE optar = 9 && ULTIMO
      SELECT CPOST
      GO BOTTOM
      LOOP

   CASE optar = 4 .OR. optar = 0 && SALIR
      SET KEY 24 TO
      SET KEY 5 TO
      RESTSCREEN(2,1,23,79,pant1)
      SETCOLOR(color3)
      IF BAND=1
       SELECT CPOST
      ENDIF
      CLOSE ALL
      RETURN
   ENDCASE
ENDDO

******************
* PROCEDIMIENTOS *
******************

* MOSTRAR POR PANTALLA EL CONTENIDO DE LAS VARIABLES
*!*********************************************************************
*!
*!      Procedure: PON_SAYC14
*!
*!      Llamado  : MAESTROS.PRG
*!
*!*********************************************************************
PROCEDURE PON_SAYC14
 colo_aa=SETCOLOR()
SETCOLOR(color1)
*Impresion en Pantalla
@10,38 SAY vpcia+' '+nom_pcia
@12,38 SAY vdpto+' '+nom_dpto
@14,38 SAY vcpos
SETCOLOR(colo_aa)
RETURN

* INICIALIZAR LAS VARIABLES
*!*********************************************************************
*!
*!      Procedure: V_0C14
*!
*!*********************************************************************
PROCEDURE V_0C14
*Asignar variables
vpcia = SPACE(2)
vdpto = SPACE(3)
STORE SPACE(15) TO nom_pcia,nom_dpto
vcpos = SPACE(8)
RETURN

* PONER EN PANTALLA LOS GET PARA INGRESO DE DATOS
*!*********************************************************************
*!
*!      Procedure: PON_GETC14
*!
*!*********************************************************************
PROCEDURE PON_GETC14
XP=1
SETCOLOR(color4)
@22,2 SAY CENTRA('[F5]-Campo Anterior',67,'Í')
SETCOLOR(color1)
DO WHILE XP<=XPM
 XD=1
 DO CASE
  CASE XP = 1
   *Provincia
   IF optar = 1
    campos="pcia~nomb"
    @10,38 GET vpcia PICTURE "@K 99" VALID VALIDA(@vpcia,@nom_pcia,"PROVIN",campos,2,15,"PCIA","NOMB")
    READ
    @10,38 SAY vpcia+' '+nom_pcia
   ENDIF

  CASE XP = 2
   *Departamento
   IF optar = 1
    campos="dpto~nomb"
    @12,38 GET vdpto PICTURE "@K 999" VALID VALIDA(@vdpto,@nom_dpto,"DEPAR",campos,3,15,"DPTO","NOMB")
    READ
    @12,38 SAY vdpto+' '+nom_dpto
   ENDIF

  CASE XP = 3
   *Codigo Postal
   @14,38 GET vcpos PICTURE 'A9999AAA'
   READ
   @14,38 SAY vcpos

 ENDCASE
 IF LASTKEY()=27
  CLEAR GETS
  XP = 4
 ELSE
  XP = XP + XD
  IF XP = 2 .AND. optar <> 1
   XP = 3
  ENDIF
 ENDIF
ENDDO
RETURN

* CARGAR CAMPOS POR DATOS DE VARIABLES
*!*********************************************************************
*!
*!      Procedure: VAR_CAMP4
*!
*!*********************************************************************
PROCEDURE VA_CAMC14
REPLACE pcia WITH vpcia
REPLACE dpto WITH vdpto
REPLACE cpos WITH vcpos
COMMIT
RETURN

* CARGAR VARIABLES CON DATOS DE CAMPOS
*!*********************************************************************
*!
*!      Procedure: CA_VARC14
*!
*!*********************************************************************
PROCEDURE CA_VARC14
vpcia= pcia
vdpto= dpto
vcpos=cpos
cam="pcia~nomb"
VALIDA(vpcia,@nom_pcia,"PROVIN",cam,2,9,"PCIA","NOMB")
cam="dpto~nomb"
VALIDA(vdpto,@nom_dpto,"DEPAR",cam,3,9,"DPTO","NOMB")
RETURN
