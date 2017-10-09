*Asigar pagos Pendientes de asignacion al Cbte. correspondiente
PRIVATE pan_ant,col_ant
pan_ant=SAVESCREEN(0,0,24,79)
col_ant=SETCOLOR()
*Clientes
AREA()
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
 SET INDEX TO CCCLIENT,CCCLIE01,CCCLIE04
ELSE
 QUIT
ENDIF
SET RELATION TO clie INTO CLIENTES
SET DECIMAL TO 2
PRIVATE vclie,nom_clie
PRIVATE vofic
PRIVATE cbte[1],pagos[1],aux_cbt[1],aux_pag[1],vto_pag,vtot_cbt
PRIVATE menu1[3],salir[2],xAct[3],op1,vnint
DO ASI_PA_CLI
cual=1
vclie=SPACE(6)
nom_clie=SPACE(30)
STORE 0 TO vtot_pag,vtot_cbt
vnint=SPACE(8)
SETCOLOR(color1)
*Menu de Confirmacion
DECLARE fi_co[3]
carga1(fi_co,'22,15~22,25~22,35')
carga1(menu1,'`GRABAR  ~`ABORTAR ~`MODIFICAR ')
carga2(salir,'13,9')
CARGA1(xAct,"F~T~T")
op1=1
DO WHILE .T.
 PRIVATE cbte[1],pagos[1]
 @4,2 GET vclie PICTURE "@K 999999" VALID BUS_CLIE(@vclie)
 READ
 IF LASTKEY()=27
  EXIT
 ENDIF
 @4,2 SAY vclie+' '+nom_clie
 *Reviso la existencia de filas vacias
 *PAGOS
 FOR con=1 TO LEN(pagos)
  IF EMPTY(SUBSTR(pagos[con],12,8))
   ADEL(pagos,con)
   ADEL(aux_pag,con)
   ASIZE(pagos,LEN(pagos)-1)
   ASIZE(aux_pag,LEN(aux_pag)-1)
  ENDIF
 NEXT
 *CBTES.
 FOR con=1 TO LEN(cbte)
  IF EMPTY(SUBSTR(cbte[con],12,8))
   ADEL(cbte,con)
   ADEL(aux_cbt,con)
   ASIZE(cbte,LEN(cbte)-1)
   ASIZE(aux_cbt,LEN(aux_cbt)-1)
  ENDIF
 NEXT
 *Controla la ultima fila de pagos
 IF EMPTY(SUBSTR(pagos[LEN(pagos)],12,8))
  ASIZE(pagos,LEN(pagos)-1)
  ASIZE(aux_pag,LEN(aux_pag)-1)
 ENDIF
 *Mostrar pagos y cbtes.
 DO MOSTRAR1
 tecla = 0
 DO WHILE .T.
    colur=SETCOLOR()
    SETCOLOR(color3)
    @ 24,2 SAY CENTRA(CHR(24)+CHR(25)+CHR(26)+CHR(27)+'  Mover      TAB-Cambia Menu   '+CHR(17)+'ÄÙ-ACEPTA  ESC-Salir',78)
    SETCOLOR(colur)
    IF tecla = 9
     cual = IF(cual < 3,cual+1,1)
    ENDIF
    DO CASE

     CASE cual = 1
      *Desactivar color pantalla anterior
      SETCOLOR(color9)
      @ 6,0 SAY "ÚÄÂDetalle Comprobantes PendientesÄÄÄÄÄÄÄ¿"
      @21,14 SAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
      @22,14 SAY "³                                                            ³"
      @23,14 SAY "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
      *Activar color pantalla actual
      SETCOLOR(color1)
      @6,42 SAY "ÚÄÂDetalle de Pagos a CuentaÄÄÄÄÄ¿"
      *Marcar pagos a cuenta
      SETCOLOR(color1)
      SET CURSOR OFF
      com = 1
      rel = 0
      KEYBOARD CHR(24)+CHR(5)
      DO WHILE .T.
       com = ACHOICE(8,43,18,74,pagos,.T.,"fun_asiPA",com,rel)
       IF com = 0
        EXIT
       ENDIF
      ENDDO
      SET KEY -4 TO
      SET CURSOR ON
      *Controlar si marco pagos
      ok=0
      FOR con=1 TO LEN(pagos)
       IF SUBSTR(pagos[con],1,1) = "±"
        ok=1
        EXIT
       ENDIF
      NEXT
      IF ok=0
       cual=2
       xAct[1] = "F"
       op1=3
      ELSE
       xAct[1] = 'T'
       op1=1
      ENDIF

     CASE cual = 2
      *Desactivar color pantalla anterior
      SETCOLOR(color9)
      @6,42 SAY "ÚÄÂDetalle de Pagos a CuentaÄÄÄÄÄ¿"
      *Activar color pantalla actual
      SETCOLOR(color1)
      @ 6,0 SAY "ÚÄÂDetalle Comprobantes PendientesÄÄÄÄÄÄÄ¿"
      *Seleccionar Cbtes
      *Marcar cbtes para asignar pagos
      SETCOLOR(color1)
      SET CURSOR OFF
      com = 1
      rel = 0
      KEYBOARD CHR(24)+CHR(5)
      DO WHILE .T.
       com = ACHOICE(8,1,18,40,cbte,.T.,"fun_asi1PA",com,rel)
       IF com = 0
        EXIT
       ENDIF
      ENDDO
      SET KEY -4 TO
      SET CURSOR ON
      *Controlar si marco pagos
      ok=0
      FOR con=1 TO LEN(cbte)
       IF SUBSTR(cbte[con],1,1) = "±"
        ok=1
        EXIT
       ENDIF
      NEXT
      IF ok=0
       xAct[1]='F'
       op1=3
      ELSE
       xAct[1] = 'T'
       op1=1
      ENDIF

     CASE cual = 3
      SETCOLOR(color9)
      @ 6,0 SAY "ÚÄÂDetalle Comprobantes PendientesÄÄÄÄÄÄÄ¿"
      @6,42 SAY "ÚÄÂDetalle de Pagos a CuentaÄÄÄÄÄ¿"
      *Activar recuadro de confirmacion
      SETCOLOR(color1)
      @21,14 SAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
      @22,14 SAY "³                                                            ³"
      @23,14 SAY "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
      SETCOLOR(color1)
      *Guardar la elecciones
      op1 = PROMPT(menu1,fi_co,color1,op1,salir,xAct)

      IF LASTKEY()#27 .AND. LASTKEY()#9
       IF op1 = 1
        *Gravar
         DO GRAVAR_ASI
         *Control si quedan pagos
         IF LEN(pagos) = 0
          ULTECLA(27)
         ENDIF
         *Control si quedan comprobantes
         IF LEN(cbte) = 0
          ULTECLA(27)
         ENDIF
         *DO SACAR_CAN
         *-------
         *Sacar pagos cancelados
         con=LEN(pagos)
         x=1
         DO WHILE x <= con .AND. LEN(pagos) > 0
          *Desmarca todos los pagos
          pagos[x] = STUFF(pagos[x],1,1,' ')
          *Elimina los cancelados
          IF VAL(SUBSTR(pagos[x],21,12)) = 0
           ADEL(pagos,x)
           ADEL(aux_pag,x)
           ASIZE(pagos,LEN(pagos)-1)
           ASIZE(aux_pag,LEN(aux_pag)-1)
           con=con-1
          ELSE
           x=x+1
          ENDIF
         ENDDO
         *Sacar cbtes. cancelados
         con=LEN(cbte)
         x=1
         DO WHILE x <= con .AND. LEN(cbte) > 0
          *Desmarcar todos los cbtes.
          cbte[x] = STUFF(cbte[x],1,1,' ')
          *Eliminar los cancelados
          IF VAL(SUBSTR(cbte[x],29,12)) = 0
           ADEL(cbte,x)
           ADEL(aux_cbt,x)
           ASIZE(cbte,LEN(cbte)-1)
           ASIZE(aux_cbt,LEN(aux_cbt)-1)
           con=con-1
          ELSE
           x=x+1
          ENDIF
         ENDDO
         *------
         DO MOSTRAR1
        cual = 1
       ENDIF
       IF op1 = 2
        *Abortar
        ULTECLA(27)
       ELSE
        IF op1 = 3
         *Modificar
         cual = 1
         EXIT
        ENDIF
       ENDIF
      ENDIF

    ENDCASE
    tecla = LASTKEY()
    IF tecla = 27
     EXIT
    ENDIF

 ENDDO
 IF tecla = 27
  EXIT
 ENDIF

ENDDO
RESTSCREEN(0,0,24,79,pan_ant)
SETCOLOR(col_ant)
CLOSE ALL
RETURN

PROCEDURE MOSTRAR1
*Limpiar pantalla
FOR con=8 TO 18
 @con, 1 SAY " ³                ³        ³            "
 @con,43 SAY " ³        ³        ³            "
NEXT
*Mostrar cbtes. y pagos
x=8
FOR con=1 TO LEN(cbte)
 IF x <= 11
  @x,1 SAY SUBSTR(cbte[con],1,40)
 ENDIF
 x=x+1
NEXT
x=8
FOR con=1 TO LEN(pagos)
 IF x <= 11
  @x,43 SAY pagos[con]
 ENDIF
 x=x+1
NEXT
RETURN

PROCEDURE GRAVAR_ASI
PRIVATE con,con1
SELECT CTACTE
*Bucle de Pagos marcados
FOR con1 = 1 TO LEN(pagos)
 IF SUBSTR(pagos[con1],1,1) = "±"
  sal_ent = VAL(SUBSTR(pagos[con1],21,12))
  *Bucle de Cbtes marcados
  FOR con = 1 TO LEN(cbte)
   IF SUBSTR(cbte[con],1,1) = '±'
    sal_aux = sal_ent
    sal_ent = sal_ent - VAL(SUBSTR(cbte[con],29,12))
    IF sal_ent >= 0
     vtota = VAL(SUBSTR(cbte[con],29,12))
    ELSE
     vtota = sal_aux
    ENDIF
    ADDREC(1)
    RECLOCK(1)
    REPLACE clie WITH vclie
    REPLACE tcom WITH 'PA',tipo WITH '0'
    REPLACE ltra WITH SPACE(1),srie WITH SPACE(4)
    REPLACE nume WITH SUBSTR(pagos[con1],3,8)
    REPLACE nint WITH SUBSTR(aux_pag[con1],11,8)
    REPLACE fech WITH DATE()
    REPLACE paga WITH vtota*(-1)
    REPLACE sact WITH 0
    REPLACE fpag WITH CTOD(SUBSTR(aux_pag[con1],1,10))  &&Fecha del Pago
    REPLACE lcom WITH SUBSTR(cbte[con],6,1)  &&Ltra Cbte.Pagado
    REPLACE scom WITH SUBSTR(cbte[con],7,4)  &&Srie Cbte.Pagado
    REPLACE ncom WITH SUBSTR(cbte[con],11,8)  &&N§ comp. pagado
    REPLACE comt WITH SUBSTR(cbte[con],3,2)   &&Tipo comp. pagado
    REPLACE fcom WITH CTOD(SUBSTR(cbte[con],42,10))     &&Fecha Comp.Pagado
    REPLACE cont WITH IIF(SUBSTR(aux_cbt[con],17,1)="T",.T.,.F.)   &&Forma de Pago de Factura
    REPLACE moro WITH 'C'
    REPLACE dias WITH DMA(CTOD(SUBSTR(aux_cbt[con],1,10)),DATE()) &&Dif.de dias en el pago
    UNLOCK
    GO VAL(SUBSTR(aux_cbt[con],11,6))
    RECLOCK(1)
    REPLACE sact WITH sact - vtota
    UNLOCK

    SELECT CTACTE
    new_pag = VAL(SUBSTR(pagos[con1],21,12)) - vtota
    new_cbt = VAL(SUBSTR(cbte[con],29,12)) - vtota
    pagos[con1]=STUFF(pagos[con1],21,12,TRANSFORM(new_pag,'@Z 999999999.99'))
    cbte[con]=STUFF(cbte[con],29,12,TRANSFORM(new_cbt,'@Z 999999999.99'))

   ENDIF

   IF sal_ent <= 0
    con = LEN(cbte)
   ENDIF
  NEXT con
  GO VAL(SUBSTR(aux_pag[con1],19,6))
  IF sal_ent > 0
   RECLOCK(1)
   REPLACE paga WITH sal_ent * (-1)
   UNLOCK
  ELSE
   RECLOCK(1)
   DELETE
   UNLOCK
  ENDIF
  COMMIT
 ENDIF
NEXT
RETURN

*Control de Clientes
FUNCTION BUS_CLIE
PARAMETERS cli,fil,col,colo_sug,lon
PRIVATE colo_a,pan_ant,ok1
IF LASTKEY() = 27
 KEYBOARD CHR(27)
 RETURN .T.
ENDIF
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
SEEK cli
IF .NOT. FOUND()
 pan_ant = SAVESCREEN(4,2,24,72)
 SETCOLOR(color3)
 SOMBRA(4,2,22,71,'V1',3,'',color3)
 @6,2 say'ÌÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹'
 @19,2 say'ÇÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¶'
 @16,2 say'ÌÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹'
 SETCOLOR(INVER)
 @4,38 SAY' BUSCAR '
 SETCOLOR(color3)
 @17,3 SAY'BUSCAR:'
 @18,3 SAY SPACE(38)
 @20,13 SAY CHR(24)+CHR(25)+' PgUp PgDn Mover         '+CHR(17)+'ÄÙ Acepta'
 @21,13 SAY'F2-Cambia Clave Busqueda  [Esc]-Salir'
 DECLARE cam[3],sep[3],ind[2],lar[2],nom[2]
 carga1(cam,"clie~nomb~domi")
 tit='C¢digo  Nombre  Cliente                Domicilio                           '
 carga2(sep,'5,2,0')
 ind[1]=1
 lar[1]=4
 nom[1]='Codigo Cliente    : '
 ind[2]=2
 lar[2]=30
 nom[2]='Nombre Cliente    : '
 ord_ant=STR(INDEXORD())
 SET ORDER TO 2
 GO TOP
 BUSCARE(5,4,15,70,cam,tit,sep,ind,lar,18,4,nom,17,14)
 regis=RECNO()
 SETCOLOR(colo_a)
 RESTSCREEN(4,2,24,72,pan_ant)
 SET ORDER TO &ord_ant
 IF LASTKEY() = 27
  SELECT &antsel
  RETURN .F.
 ENDIF
ENDIF
cli=clie
nom_clie = nomb
vdomi = domi
vciva = civa
sal_clie = sact
vadju=acre
IF ok1=1
 SETCOLOR(colo_sug)
 @fil,col SAY vclie+' '+SUBSTR(nom_clie,1,lon)
 SETCOLOR(colo_a)
ENDIF
*Capturar pagos y cbtes.
SELECT CTACTE
SET ORDER TO 1
SEEK cli+'PA'
k=0
IF FOUND()
 p=0
 DO WHILE (tcom='PA' .AND. clie=cli.AND.!EOF())
  IF ncom='********' .AND. cli=clie .AND. !baja .AND. !DELETED()
   IF p > 0
    ASIZE(pagos,p+1)
    ASIZE(aux_pag,p+1)
   ENDIF
   p=p+1
   pagos[p]=" ³        ³        ³            "
   pagos[p]=STUFF(pagos[p],3,8,nume)
   fFech=DTOC(fech)
   pagos[p]=STUFF(pagos[p],12,8,LEFT(ffech,6)+RIGHT(fFech,2))
   pagos[p]=STUFF(pagos[p],21,12,TRANSFORM(paga*(-1),'@Z 999999999.99'))
   aux_pag[p] = DTOC(fpag)+nint+STR(RECNO(),6)
   k++
  ENDIF
  SKIP
  @5,60 SAY k COLOR color9
 ENDDO
 IF p=0
  CARTEL("NO HAY COMPROBANTES PARA PAGAR [ OK ]",12,1,'N',color3)
  SELECT &antsel
  RETURN .F.
 ENDIF
ELSE
 CARTEL("NO HAY PAGOS A CUENTA [ OK ]",12,1,'N',color3)
 AFILL(pagos," ³        ³        ³            ")
 AFILL(aux_pag,'')
 AFILL(cbte," ³                ³        ³            ")
 AFILL(aux_cbt,'')
 SELECT &antsel
 RETURN .F.
ENDIF
*Buscar Comprobantes pendientes
DBSETORDER(1)
SET SOFTSEEK ON
SEEK cli+'CH'
 SET SOFTSEEK OFF
 p=0
 k=0
 DO WHILE (clie=cli.AND.tcom#"PA".AND.!EOF())
  IF sact > 0 .AND. cli=clie .AND. !baja .AND. !DELETE()
   IF p > 0
    ASIZE(cbte,p+1)
    ASIZE(aux_cbt,p+1)
   ENDIF
   p=p+1
   cbte[p]=" ³                ³        ³            "
   cbte[p]=STUFF(cbte[p],3,2,tcom)
   cbte[p]=STUFF(cbte[p],6,1,ltra)
   cbte[p]=STUFF(cbte[p],7,4,srie)
   cbte[p]=STUFF(cbte[p],11,8,nume)
   fFech=DTOC(fech)
   cbte[p]=STUFF(cbte[p],20,8,LEFT(ffech,6)+RIGHT(fFech,2))
   cbte[p]=STUFF(cbte[p],29,12,TRANSFORM(sact,'@Z 999999999.99'))
   aux_cbt[p] = DTOC(fech)+STR(RECNO(),6)+IIF(cont,"T","F")+nint
   k++
  ENDIF
  SKIP
  @5,10 SAY k COLOR color9
 ENDDO
 IF p=0
  CARTEL("NO HAY COMPROBANTES PARA PAGAR [ OK ]",12,1,'N',color3)
  AFILL(pagos," ³        ³        ³            ")
  AFILL(aux_pag,'')
  AFILL(cbte," ³                ³        ³            ")
  AFILL(aux_cbt,'')
  SELECT &antsel
  RETURN .F.
 ENDIF
IF p=0
 SET SOFTSEEK OFF
 CARTEL("NO HAY PAGOS A CUENTA [ OK ]",12,1,'N',color3)
 SELECT &antsel
 RETURN .F.
ENDIF
SELECT &antsel
RETURN .T.

FUNCTION fun_asiPA
PARAMETERS MOD,sub,pos
PRIVATE tcl,fil,pus,oriy,con,ydes,fCanti
FIL=ROW()
IF LEN(pagos)>11
 ydes = 11
ELSE
 ydes = LEN(pagos)
ENDIF
IF ydes = 1
 @8,42 SAY ''
ELSE
 pus = ydes/LEN(pagos)*sub
 FOR oriy = 1 TO ydes
  @7+oriy,42 SAY IF(pus <= oriy .and. pus >= oriy-.99999,IIF(sub=1,'',IIF(sub=LEN(pagos),'','')),'³')
 NEXT oriy
ENDIF
SET KEY -4 TO
tcl = LASTKEY()
*Suma e imprime Sub_total
vtot_pag = 0
FOR con = 1 TO LEN(pagos)
 IF SUBSTR(pagos[con],1,1) = '±'
  vtot_pag = vtot_pag + VAL(SUBSTR(pagos[con],21,12))
 ENDIF
NEXT con
@20,63 SAY vtot_pag PICTURE '@Z 999999999.99'
DO CASE
CASE MOD = 0
 RETURN(2)
CASE MOD = 1 .OR. MOD = 2
 TONO(2)
 RETURN(2)
CASE MOD = 3
 DO CASE
 CASE tcl = 27         &&Presiona [ESC] para terminar Pagos
  vtot_pag=0
  RETURN(0)
 CASE tcl=9   &&Tab
  RETURN(0)
 CASE tcl = 13
   IF SUBSTR(pagos[sub],1,1) # '±'
    fCanti=0
    FOR con=1 TO LEN(pagos)
     IF SUBSTR(pagos[con],1,1) = '±'
      fCanti=fCanti+1
     ENDIF
    NEXT
    IF fCanti > 0
     CARTEL("SOLO PUEDE MARCAR UN PAGO [ OK ]",18,1,'N',color3)
     rel = pos
     RETURN(1)
    ELSE
     pagos[sub] = STUFF(pagos[sub],1,1,'±')
     vnint = SUBSTR(aux_pag[sub],11,8)
    ENDIF
   ELSE
    pagos[sub] = STUFF(pagos[sub],1,1,' ')
    vnint=SPACE(8)
   ENDIF
   KEYBOARD CHR(24)
   rel = pos
   RETURN(1)
 CASE tcl = 42
  vtot_pag = 0
  FOR CON = 1 TO LEN(pagos)
   IF SUBSTR(pagos[con],1,1) = '±'
    vtot_pag = vtot_pag + VAL(SUBSTR(pagos[con],21,12))
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

FUNCTION fun_asi1PA
PARAMETERS MOD,sub,pos
PRIVATE tcl,fil,pus,oriy,con,ydes
FIL=ROW()
IF LEN(cbte)>11
 ydes = 11
ELSE
 ydes = LEN(cbte)
ENDIF
IF ydes = 1
 @8,0 SAY ''
ELSE
 pus = ydes/LEN(cbte)*sub
 FOR oriy = 1 TO ydes
  @7+oriy,0 SAY IF(pus <= oriy .and. pus >= oriy-.99999,IIF(sub=1,'',IIF(sub=LEN(cbte),'','')),'³')
 NEXT oriy
ENDIF
SET KEY -4 TO
tcl = LASTKEY()
*Suma e imprime Sub_total
vtot_cbt = 0
FOR con = 1 TO LEN(cbte)
 IF SUBSTR(cbte[con],1,1) = '±'
  vtot_cbt = vtot_cbt + VAL(SUBSTR(cbte[con],29,12))
 ENDIF
NEXT con
@20,29 SAY vtot_cbt PICTURE '@Z 999999999.99'
DO CASE
CASE MOD = 0
 RETURN(2)
CASE MOD = 1 .OR. MOD = 2
 TONO(2)
 RETURN(2)
CASE MOD = 3
 DO CASE
 CASE tcl = 27         &&Presiona [ESC] para terminar cbte
  vtot_pag=0
  RETURN(0)
 CASE tcl=9        &&Tab
  RETURN(0)
 CASE tcl = 13
   IF SUBSTR(cbte[sub],1,1) # '±'
    cbte[sub] = STUFF(cbte[sub],1,1,'±')
   ELSE
    cbte[sub] = STUFF(cbte[sub],1,1,' ')
   ENDIF
   KEYBOARD CHR(24)
   rel = pos
   RETURN(1)
 CASE tcl = 42
  vtot_cbt = 0
  FOR CON = 1 TO LEN(cbte)
   IF SUBSTR(cbte[con],1,1) = '±'
    vtot_cbt = vtot_cbt + VAL(SUBSTR(cbte[con],21,12))
   ENDIF
  NEXT
  IF vtot_cbt = 0
   CARTEL("DEBE MARCAR ALGUN COMPROBANTE  [ OK ]",18,1,'N',color3)
   RETURN(1)
  ELSE
   RETURN(0)
  ENDIF
 OTHERWISE
  RETURN(2)
 ENDCASE
ENDCASE
