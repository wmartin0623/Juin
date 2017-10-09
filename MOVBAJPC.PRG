*Baja de Pagos de Clientes
#INCLUDE "INKEY.CH"
#INCLUDE "SETCURS.CH"
LOCAL Pan_Ant,Col_Ant,xx
Pan_Ant=SAVESCREEN(,,,,)
Col_Ant=SETCOLOR()

*clientes
OPEN_DBF("MAECLIEN","CLIEN","C","MAECLIEN","clie","NOMCLIEN","nomb")

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

PRIVATE vClie,Nom_Clie,bMarca,oTAB
vClie=SPACE(6)
Nom_Clie=SPACE(30)
STORE DATE() TO Fe_Des,Fe_Has
bMarca=.F.

DO WHILE .T.
 *Mostrar En Pantalla
 DO BAJA_PAG
 SETCOLOR(color1)
 @15,7 SAY '['+CHR(24)+CHR(25)+' PgUp PgDn]-Mover ['+CHR(17)+'ÄÙ]-Marca/Des. [*]-Acepta [Esc]-Cancela' COLOR color3
 @16,28 SAY "[Tab]-Recorre Detalle" COLOR color3
 DIST_COLOR(15,8,'~W+/R~ PgUp PgDn')
 DIST_COLOR(15,29,'~W+/R~ÄÙ')
 DIST_COLOR(15,46,'~W+/R~*')
 DIST_COLOR(15,57,'~W+/R~Esc')
 DIST_COLOR(16,29,'~W+/R~Tab')
 cam="clie~nomb"
 @4,8 GET vClie PICTURE "@K 999999" VALID VALIDA(@vCLie,@Nom_Clie,"CLIEN",cam,6,30,"CLIE","NOMB")
 READ
 IF LASTKEY()=27
  EXIT
 ENDIF
 @4,8 SAY vclie+' '+Nom_Clie
 ok=0
 FECHAS(DATE(),DATE())
 IF ok=1
  LOOP
 ENDIF
 @4,48 SAY Fe_Des PICTURE "@D"
 @4,59 SAY Fe_Has PICTURE "@D"
 PRIVATE MPago:={}
 *Cargar Matriz con Pagos dentro del Rango
 xx=0
 SELECT CTACTE
 SEEK vClie
 IF FOUND()
  DBEVAL({|elem| AADD(MPago,;
                            DTOC(Fech)+'³'+;
                            +nume+'³'+;
                            +nint+'³'+;
                            STR(paga,12,2)+'³ '+;
                            +comt+' ³ '+;
                            +lcom+scom+ncom+;
                            ' '+STR(RECNO(),6)+SPACE(3)),;
                            xx++;
         },;
         {|| clie=vCLie.AND.;
             fech>=Fe_Des.AND.;
             fech<=Fe_Has.AND.;
             tcom='PA'.AND.;
             !baja;
         },{|| clie=vClie})
 ELSE
  xx=0
 ENDIF
 *Verificamos Existencia de Pagos en el Rango
 IF xx=0
  CARTEL("NO HAY PAGOS EN EL RANGO SOLICITADO [ OK ]",12,1,'N',color3)
  LOOP
 ENDIF
 com = 1
 rel = 0
 KEYBOARD CHR(24)+CHR(5)
 DO WHILE .T.
  com = ACHOICE(8,7,13,67,MPago,.T.,"BAJPAG",com,rel)
  IF com = 0
   EXIT
  ENDIF
 ENDDO
 SET KEY -4 TO
 SET CURSOR ON
 @17,6 CLEAR TO 23,69
 IF LASTKEY()=27
  SETCOLOR(col_ant)
  RESTSCREEN(0,0,24,79,pan_ant)
  LOOP
 ENDIF
 IF !EMPTY(SUBSTR(MPago[1],1,10))
  bien=CARTEL("DATOS CORRECTOS [ SI ] [ NO ]",19,1,'N',colo3)
  IF LASTKEY()=27.OR.!bien
   SETCOLOR(col_ant)
   RESTSCREEN(0,0,24,79,pan_ant)
   LOOP
  ENDIF
 ELSE
  LOOP
 ENDIF
 *Ejecutar la Baja
 SELECT CTACTE
 DBSETORDER(1)
 FOR con=1 TO LEN(MPago)
  IF !EMPTY(SUBSTR(MPago[con],1,10)).AND.SUBSTR(MPago[con],11,1)="±"
   vReg=VAL(SUBSTR(MPago[con],63,6))
   vClav=vCLie+SUBSTR(MPago[con],44,2)+SUBSTR(MPago[con],49,13)
   vClav_Comi=vCLie+"PA"+SPACE(5)+SUBSTR(MPago[con],12,8)
   vPaga=VAL(SUBSTR(MPago[con],30,12))
   fMesAno=CHR(VAL(SUBSTR(MPago[con],4,2))+64)+SUBSTR(MPago[con],7,4)
   ClaveGen=SUBSTR(MPago[con],12,8)+SUBSTR(MPago[con],21,8)
   *Busco Factura para Devolverle el Importe
   SEEK vClav
   IF FOUND()
    RECLOCK(1)
    REPLACE sact WITH sact+ABS(vPaga)
    REPLACE canc WITH .F.
    UNLOCK
    GO vReg
    RECLOCK(1)
    DELETE
    UNLOCK
    SELECT CLIEN
    SEEK vClie
    IF FOUND()
     RECLOCK(1)
     REPLACE sact WITH sact+ABS(vPaga)
     UNLOCK
    ENDIF
*   *Generar Movimiento Negativo en la Caja
*   AREA()
*   IF !FILE("CAJA&fMesAno..DBF")
*    IF USAR('CAJA','CAJA')
*     COPY TO CAJA&fMesAno STRU
*    ELSE
*     CLOSE ALL
*     QUIT
*    ENDIF
*   ENDIF
*   IF USAR("CAJA&fMesAno","CAJA")
*    IF !FILE('CAJA&fMesAno..NTX')
*     INDEX ON tcom+ltra+srie+nume+fpag TO CAJA&fMesAno
*    ENDIF
*    wperio=fMesAno+'1'
*    IF !FILE('CAJ&wperio..NTX')
*     INDEX ON fdia+tcaj+ncta+fpag TO CAJ&wperio
*    ENDIF
*    SET INDEX TO CAJA&fMesAno,CAJ&wperio
*   ELSE
*    QUIT
*   ENDIF
*   ADDREC(1)
*   RECLOCK(1)
*   REPLACE tcom WITH 'PA'
*   REPLACE ltra WITH ' '
*   REPLACE srie WITH SPACE(4)
*   REPLACE nume WITH SUBSTR(MPago[con],12,8)
*   REPLACE fdia WITH SUBSTR(MPago[con],1,2)
*   REPLACE fpag WITH "EF"
*   REPLACE tcaj WITH 'CC'
*   REPLACE impo WITH vPaga
*   REPLACE clie WITH vclie
*   REPLACE vend WITH vVend
*   REPLACE prov WITH SPACE(4)   &&Se usa Cdo.le pagamos a un Proveedor
*   REPLACE ncta WITH '999'
*   REPLACE baja WITH .F.
*   UNLOCK
*   CLOSE

    SELECT CTACTE
   ENDIF
  ENDIF
 NEXT

ENDDO
CLOSE ALL
SETCOLOR(Col_Ant)
RESTSCREEN(,,,,Pan_Ant)
RETURN

FUNCTION BAJPAG
PARAMETERS MOD,sub,pos
LOCAL xx,pus,Oriy,fMEsAno,ClaveGen
PRIVATE tcl
SET KEY -4 TO
tcl=LASTKEY()
*Flechas guias
IF LEN(MPago) < 6
 ydes = LEN(MPago)
ELSE
 ydes = 6
ENDIF
IF ydes=1
 @8,6 SAY ''
ELSE
 pus = ydes/LEN(MPago)*sub
 FOR oriy = 1 TO ydes
  Linea=IF(pus <= oriy .and. pus >= oriy-.99999,IIF(sub=1,'',IIF(sub=LEN(MPago),'','')),'³')
  @7+oriy,6 SAY linea COLOR IIF(Linea="³",color1,color9)
 NEXT oriy
ENDIF
IF SUBSTR(MPago[sub],11,1)="±"
 bMarca=.T.
ELSE
 bMarca=.F.
ENDIF
@21,8 SAY SUBSTR(MPago[Sub],63,6)
fMesAno=CHR(VAL(SUBSTR(MPago[sub],4,2))+64)+SUBSTR(MPago[sub],7,4)
ClaveGen=SUBSTR(MPago[sub],12,8)+SUBSTR(MPago[sub],21,8)
DETALLE1(fMesAno,ClaveGen)
DO CASE
 CASE MOD=0
  RETURN(2)
 CASE MOD=1 .OR. MOD=2
  TONO(2)
  RETURN(2)
 CASE MOD=3
  DO CASE
   CASE tcl=13
    IF SUBSTR(MPago[sub],11,1)="±"
     MPago[sub] = STUFF(MPago[sub],11,1,'³')
    ELSE
     MPago[sub] = STUFF(MPago[sub],11,1,'±')
    ENDIF
    KEYBOARD CHR(24)
    rel = pos
    RETURN(1)

   CASE tcl = 42
    xx=0
    FOR CON = 1 TO LEN(MPago)
     IF SUBSTR(MPago[con],11,1) = '±'
      xx++
      EXIT
     ENDIF
    NEXT
    IF xx=0
     CARTEL("DEBE MARCAR ALGUN COMPROBANTE  [ OK ]",18,1,'N',color3)
     RETURN(1)
    ELSE
     RETURN(0)
    ENDIF

   CASE tcl=-4
    XD=-1
    RETURN(0)

   CASE tcl=27          &&Presiona [ESC] para terminar
    RETURN(0)

   CASE tcl=9      &&Tab
    ***** BUCLE PRINCIPAL
    DETALLE1(fMesAno,ClaveGen)
    SETCOLOR(color1)
    DO WHILE .T.
     DO WHILE NEXTKEY()==0 .AND. .NOT. oTAB:STABILIZE()
     ENDDO
     oTAB:COLORRECT({oTAB:ROWPOS,1,oTAB:ROWPOS,oTAB:COLCOUNT}, {5,1})
     IF oTAB:HITTOP .OR. oTAB:HITBOTTOM
       TONE(2000)
     ENDIF
     TECLA:=INKEY(0)
     IF TECLA==K_ESC
        EXIT
      ELSE
       CUALTECLA(TECLA)
     ENDIF
     oTAB:REFRESHCURRENT()
    ENDDO
    oTAB:DEHILITE()
    oTAB:REFRESHCURRENT()
    RETURN(1)


   CASE tcl>=48 .and. tcl<=57
    KEYBOARD CHR(13)+CHR(tcl)
    RETURN(2)

   OTHERWISE
    RETURN(2)
  ENDCASE
ENDCASE

PROCEDURE BAJA_PAG
* bajpag.FMT
SOMBRA(2,1,23,78,"H5",1,'',colo3)
SOMBRA(3,6,16,69,"H2",1,'',color7)
SETCOLOR(color7)
@ 3,6 SAY "ÚÄDatos del ClienteÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÂRango de FechasÄÄÄÄÄÄ¿"
@ 4,6 SAY "³                                       ³³                     ³"
@ 5,6 SAY "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
@ 6,6 SAY "ÚPagos Encontrados en RangoÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂCbte.PagadosÄÄÄÄÄÄÄÄ¿"
@ 7,6 SAY "ÃFechaÄÄÄÄÄÂN§Ofi.ÄÄÂN§Int.ÄÄÂImporteÄÄÄÄÄÅTipoÂNumeroÄÄÄÄÄÄÄÄÄ´"
@ 8,6 SAY "³          ³        ³        ³            ³    ³               ³"
@ 9,6 SAY "³          ³        ³        ³            ³    ³               ³"
@10,6 SAY "³          ³        ³        ³            ³    ³               ³"
@11,6 SAY "³          ³        ³        ³            ³    ³               ³"
@12,6 SAY "³          ³        ³        ³            ³    ³               ³"
@13,6 SAY "³          ³        ³        ³            ³    ³               ³"
@14,6 SAY "ÃÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´"
@15,6 SAY "³                                                              ³"
@16,6 SAY "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
RETURN

FUNCTION DETALLE1
PARAMETERS fMesAno,ClaveGen
LOCAL AreAnt
AreAnt=ALLTRIM(STR(SELECT()))
*PAGOS
IF SELECT("PAGOS")#0
 SELECT PAGOS
 CLOSE
ENDIF
AREA()
IF FILE("PCL&fMesAno..DBF")
 IF USAR("PCL&fMesAno","PAGOS")
  IF !FILE('PCL&fMesAno..NTX')
   INDEX ON nume+nint TO PCL&fMesAno
  ENDIF
  SET INDEX TO PCL&fMesAno
 ELSE
  SELECT &AreAnt
  RETURN .F.
 ENDIF
ELSE
 SELECT &AreAnt
 RETURN .F.
ENDIF
SELECT PAGOS
SEEK ClaveGen
IF .NOT. FOUND()
 @18,06 CLEAR TO 22,69
 @20,15 SAY "NO EXISTEN MOVIMIENTOS PARA ESTE PAGO" COLOR color3
 SELECT &AreAnt
 RETURN .F.
 RETURN NIL
ELSE
 @20,15 SAY SPACE(40)
ENDIF
oDET:=BROWSEFILT(17,6,23,69,ClaveGen)
DO WHILE .NOT. oDET:STABILIZE()
ENDDO
oDET:DEHILITE()

FUNCTION BROWSEFILT(nLINT,nCOLT,nLINB,nCOLB,ClaveGen)
LOCAL con
*SOMBRA(nLINT-1,nCOLT-1,nLINB+1,nCOLB+1,"H2",1,'',color7)
oTAB:=TBROWSEDB(nLINT,nCOLT,nLINB,nCOLB)     //CREA EL OBJETO oTAB
oTAB:COLORSPEC:=color1
oTAB:HEADSEP:=CHR(205)+CHR(209)+CHR(205)
oTAB:COLSEP:= chr(32) +CHR(179)+chr(32)
oTAB:FOOTSEP:=CHR(205)+CHR(207)+CHR(205)

*Crear Columnas
oTAB:ADDCOLUMN(TBCOLUMNNEW("N§ Ofic.",{|| nume}))
oTAB:ADDCOLUMN(TBCOLUMNNEW("N§ Int. ",{|| nint}))
oTAB:ADDCOLUMN(TBCOLUMNNEW("FP",{|| fpag}))
oTAB:ADDCOLUMN(TBCOLUMNNEW("  Fecha   ",{|| fech}))
oTAB:ADDCOLUMN(TBCOLUMNNEW("N§ Talon",{|| ntal}))
oTAB:ADDCOLUMN(TBCOLUMNNEW("  Importe   ",{|| impo}))
*Color Especial si el Pago ESTA Seleccionado
 FOR con=1 TO 6
  COLUMNA:=oTAB:GETCOLUMN(con)
  COLUMNA:COLORBLOCK:={|d| IF(bmarca,{5,1},{1,2}) }
 NEXT

oTAB:GOTOPBLOCK={|| INICFRANJA(ClaveGen) }
oTAB:GOBOTTOMBLOCK:={|| FINFRANJA(ClaveGen)}
oTAB:SKIPBLOCK:={|nPARAMOVER| MUEVEREG(nPARAMOVER,ClaveGen)}
RETURN(oTAB)

FUNCTION INICFRANJA(ClaveGen)   //DETERMINA EL PRIMER REGISTRO DE LA FRANJA
 DBSEEK(ClaveGen)
RETURN NIL

FUNCTION FINFRANJA(ClaveGen)   //DETERMINA EL ULTIMO
**Liga SOFTSEEK (PARA CLAVE MAYOR DE 1 POSICION) y aumenta el valor de la
**clave aumentando el valor de su £ltimo byte. Por ejemplo, si la £ltima letra
**fuera "A", se transformar¡a en "B"
IF LEN(ClaveGen)=1
  DBSEEK(ULTIMACLAVE(ClaveGen), .F.)
  DBSKIP(-1)
  RETURN ({|| DBSEEK(ULTIMACLAVE(ClaveGen), .F.), DBSKIP(-1)})
ELSE
  DBSEEK(ULTIMACLAVE(ClaveGen), .T.)
  DBSKIP(-1)
  RETURN ({|| DBSEEK(ULTIMACLAVE(ClaveGen),.T.),DBSKIP(-1)})
ENDIF

FUNCTION ULTIMACLAVE(ClaveGen)
 LOCAL ULTIMOREG
 ULTIMOREG := STUFF(ClaveGen, LEN(ClaveGen), 1, ;
                CHR( ASC(RIGHT(ClaveGen,1)) + 1 ))
 RETURN (ULTIMOREG)

FUNCTION MUEVEREG( nPARAMOVER, ClaveGen )
LOCAL nMOVIDOS
nMOVIDOS := 0
IF nPARAMOVER == 0 .OR. LASTREC() == 0
  DBSKIP(0)
 ELSEIF nPARAMOVER > 0 .AND. RECNO() != LASTREC() + 1
   DO WHILE nMOVIDOS <= nPARAMOVER .AND. ! EOF() .AND. ;
    &(INDEXKEY(0)) == ClaveGen              //AVANZA REGISTROS
    DBSKIP(1)
    nMOVIDOS++
   ENDDO
   DBSKIP(-1)        //SIEMPRE DEVUELVE UN REGISTRO PUES EL DO WHILE TERMINA
   nMOVIDOS--        //CUANDO ESTAMOS UN REGISTRO ADELANTE FUERA DE LA FRANJA
 ELSEIF nPARAMOVER < 0   // MOVER PARA TRAS (PARA INICIO)

   DO WHILE nMOVIDOS >= nPARAMOVER .AND. ! BOF() .AND. ;
     &(INDEXKEY(0)) == ClaveGen
     DBSKIP(-1)
     nMOVIDOS--
   ENDDO
   IF ! BOF()            //SIEMPRE DEVUELVE UN REGISTRO PUES EL DO WHILE
      DBSKIP(1)          //TERMINA CUANDO ESTAMOS FUERA DE LA FRANJA SALVO
   ENDIF                 //EN EL CASO DE NO SER INICIO DE ARCHIVO
   nMOVIDOS++
ENDIF
RETURN (nMOVIDOS)

FUNCTION ESTABILIZE()
DO WHILE .NOT. oTAB:STABILIZE
ENDDO
RETURN NIL

FUNCTION CUALTECLA(TECLA)
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