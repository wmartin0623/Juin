*Carga de Partes y Repuestos para cada Parte
#INCLUDE "INKEY.CH"
#INCLUDE "SETCURS.CH"
LOCAL Pan_Ant,Col_Ant,xx
Pan_Ant=SAVESCREEN(,,,,)
Col_Ant=SETCOLOR()

*ARCHIVO AUXILIAR
AREA()
IF USAR('AUXILIAR','AUX')
 GO TOP
 PUBLIC vPu30,vPu60
 vPu30=IIF(pu30#0,(pu30+100)/100,1)
 vPu60=IIF(pu60#0,(pu60+100)/100,1)
ELSE
 QUIT
ENDIF

*Rubros
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
  SET INDEX TO MAEARTIC,MAEARTI1,MAEARTI2,MAEARTI3,MAEARTI4,MAEARTI5,NOMARTIC
 ELSE
  QUIT
 ENDIF

PRIVATE Com_Rbr,Com_Pro,Com_Mrc,Com_Art,tcl,Rel:=0
PRIVATE Com:=1,Rel:=0,Busca,Hasta,Desde,xPos,zPos,Subir
PRIVATE fRubr,fProv,fMrca,Nom_Rubr
PRIVATE Si_Hay:=.T.
PRIVATE MRubr:={}
PRIVATE MProv:={}
PRIVATE MMrca:={}
PRIVATE MArti:={}

DO WHILE .T.
 *Mostrar En Pantalla
 DO PAN_CONSU
 STORE "0000" TO fProv
 STORE "FERR" TO fRubr
 fMrca="000"
 PRIVATE MRubr:={}
 PRIVATE MProv:={}
 PRIVATE MMrca:={}
 PRIVATE MArti:={}
 PRIVATE oTAB
 *Cargar Matriz con Rubros, Proveedores y Marcas

 *Rubros
 Com_Rbr=1
 xx=0
 SELECT RUBRO
 GO TOP
 *AADD(MRubr," ** Todos **        ³0000")
 DBEVAL({|elem| AADD(MRubr,;
                     nomb+'³'+rubr;
                        ),;
                        xx++;
        },;
         {|| .T.;
         })
 *Verificacmos Existencia de RUbros
 IF xx=0
  CARTEL("NO HAY RUBROS CARGADOS [ OK ]",12,1,'N',color3)
  LOOP
 ENDIF
 *Ordenar Matriz Rubros
 ASORT(MRubr,,,{|p1,p2| p1 < p2})
 *Mostrar Rubros
 SETCOLOR(color9)
 FOR con=1 TO LEN(MRubr)
  @con+3,1 SAY MRubr[con]
  IF con=4
   EXIT
  ENDIF
 NEXT
 Nom_Rubr=" ** Todos **        ³0000"
 @4,21 SAY "" COLOR color3

 *Proveedores
 Com_Pro=1
 xx=0
 SELECT PROVE
 GO TOP
 AADD(MProv," ** Todos **                  ³0000")
 DBEVAL({|elem| AADD(MProv,;
                     nomb+'³'+prov;
                        ),;
                        xx++;
        },;
         {|| .T.;
         })
 *Verificacmos Existencia de Proveedores
 IF xx=0
  CARTEL("NO HAY PROVEEDORES CARGADOS [ OK ]",12,1,'N',color3)
  LOOP
 ENDIF
 *Ordenar Matriz Proveedores
 ASORT(MProv,,,{|p1,p2| p1 < p2})
 *Mostrar Proveedores
 SETCOLOR(color9)
 FOR con=1 TO LEN(MProv)
  @con+3,28 SAY MProv[con]
  IF con=4
   EXIT
  ENDIF
 NEXT
 @4,58 SAY "" COLOR color3

 *Marcas
 Com_Mrc=1
 xx=0
 SELECT MARCA
 GO TOP
 AADD(MMrca," *Todas * ³000")
 DBEVAL({|elem| AADD(MMrca,;
                     nomb+'³'+mrca;
                        ),;
                        xx++;
        },;
         {|| .T.;
         })
 *Verificacmos Existencia de Marcas
 IF xx=0
  CARTEL("NO HAY MARCAS CARGADAS [ OK ]",12,1,'N',color3)
  LOOP
 ENDIF
 *Ordenar Matriz marcas
 ASORT(MMrca,,,{|p1,p2| p1 < p2})
 *Mostrar Marcas
 SETCOLOR(color9)
 FOR con=1 TO LEN(MMrca)
  @con+3,65 SAY MMrca[con]
  IF con=4
   EXIT
  ENDIF
 NEXT
 @4,75 SAY "" COLOR color3

 *Leer todos los Articulos
 DETALLE(fRubr,fProv,fMrca)
 Cual=1

 DO WHILE .T.
  DO CASE
  CASE cual = 1
   *Articulos
   @24,0 SAY SPACE(80) COLOR color3
   @24,1 SAY "±Esc±-Salir  ±Alt-R±-Rubros ±Alt-P±-Proveedores ±Alt-M±-Marcas" COLOR color3
   DIST_COLOR(24,2,'~W+/R~Esc')
   DIST_COLOR(24,15,'~W+/R~Alt-R')
   DIST_COLOR(24,30,'~W+/R~Alt-P')
   DIST_COLOR(24,50,'~W+/R~Alt-M')
   Busca=""
   STORE 0 TO Desde,Hasta,xPos,zPos
   KEYBOARD CHR(24)+CHR(5)
   DO WHILE .T.
    Com_Art = ACHOICE(12,1,17,56,MArti,.T.,"Fun_Ar",Com_Art,rel)
    IF Com_Art = 0 .OR. tcl=9
     EXIT
    ENDIF
   ENDDO
   SET KEY -4 TO
   SET CURSOR ON
   @24,1 SAY SPACE(40)
   IF LASTKEY()=27
    SETCOLOR(col_ant)
    RESTSCREEN(,,,,pan_ant)
    EXIT
   ENDIF

  CASE cual = 2
   *Rubros
   @24,0 SAY SPACE(80) COLOR color3
   @24,1 SAY "±Tab±-Ir a Articulos" COLOR color3
   DIST_COLOR(24,2,'~W+/R~Tab')
   DO WHILE .T.
    Com_Rbr = ACHOICE(4,1,7,25,MRubr,.T.,"Fun_Rb",Com_Rbr,rel)
    IF Com_Rbr = 0 .OR. tcl=9 .OR. tcl=27
     EXIT
    ENDIF
   ENDDO
   @22,9 SAY SPACE(50)
   Cual=1
   SET KEY -4 TO
   SET CURSOR ON
   @24,1 SAY SPACE(40)
   IF LASTKEY()=27
    LOOP
   ENDIF

  CASE Cual=3
   *Proveedores
   @24,0 SAY SPACE(80) COLOR color3
   @24,1 SAY "±Tab±-Ir a Articulos" COLOR color3
   DIST_COLOR(24,2,'~W+/R~Tab')
   DO WHILE .T.
    Com_Pro = ACHOICE(4,28,7,62,MProv,.T.,"Fun_Pr",Com_Pro,rel)
    IF Com_Pro = 0 .OR. tcl=9 .OR. tcl=27
     EXIT
    ENDIF
   ENDDO
   @22,9 SAY SPACE(50)
   Cual=1
   SET KEY -4 TO
   SET CURSOR ON
   @24,1 SAY SPACE(78)
   IF LASTKEY()=27
    LOOP
   ENDIF

  CASE Cual=4
   *Marcas
   @24,0 SAY SPACE(80) COLOR color3
   @24,1 SAY "±Tab±-Ir a Articulos" COLOR color3
   DIST_COLOR(24,2,'~W+/R~Tab')
   DO WHILE .T.
    Com_Mrc = ACHOICE(4,65,7,78,MMrca,.T.,"Fun_Mr",Com_Mrc,rel)
    IF Com_Mrc = 0 .OR. tcl=9 .OR. tcl=27
     EXIT
    ENDIF
   ENDDO
   Cual=1
   @22,9 SAY SPACE(50)
   SET KEY -4 TO
   SET CURSOR ON
   @24,1 SAY SPACE(78)
   IF LASTKEY()=27
    LOOP
   ENDIF

  ENDCASE
 ENDDO
 IF LASTKEY()=27
  EXIT
 ENDIF

ENDDO
CLOSE ALL
SETCOLOR(Col_Ant)
RESTSCREEN(,,,,Pan_Ant)
RETURN

FUNCTION Fun_Ar
PARAMETERS MOD,sub,pos
LOCAL xx,pus,Oriy,ClaveGen,fColu,fFila
LOCAL Nom_Arti,Col_Act,zz
Col_Act=SETCOLOR()
fColu=COL()
fFila=ROW()
SET KEY -4 TO
tcl=LASTKEY()
*Flechas guias
IF LEN(MArti) < 6
 ydes = LEN(MArti)
ELSE
 ydes = 6
ENDIF
IF ydes=1
 @12,0 SAY ''
ELSE
 pus = ydes/LEN(MArti)*sub
 FOR oriy = 1 TO ydes
  Linea=IF(pus <= oriy .and. pus >= oriy-.99999,IIF(sub=1,'',IIF(sub=LEN(MArti),'','')),'³')
  @11+oriy,0 SAY linea COLOR IIF(Linea="³",color1,color9)
 NEXT oriy
ENDIF
IF !EMPTY(SUBSTR(MArti[sub],52,5))
 *Leer Datos del articulo
 fArti=SUBSTR(MArti[sub],52,5)
 Nom_Arti=SUBSTR(MArti[sub],1,50)
 zProv=SUBSTR(MArti[Sub],58,35)
 fCpro=SUBSTR(MArti[Sub],93,15)
 funid=SUBSTR(MArti[Sub],108,10)
 fFvta=SUBSTR(MArti[Sub],118,10)
 fUnVt=SUBSTR(MArti[Sub],128,10)
 fMedi=SUBSTR(MArti[Sub],138,15)
 zMrca=SUBSTR(MArti[Sub],153,14)
 fClor=SUBSTR(MArti[Sub],167,10)
 fCali=SUBSTR(MArti[Sub],177, 8)
 fUsos=SUBSTR(MArti[Sub],185,10)
 fDrza=SUBSTR(MArti[Sub],195, 5)
 fRefe=SUBSTR(MArti[Sub],200,10)
 fPuCd=SUBSTR(MArti[Sub],210, 9)
 fPu30=SUBSTR(MArti[Sub],219, 9)
 fPu60=SUBSTR(MArti[Sub],228, 9)
 xRubr=SUBSTR(MArti[Sub],237,25)
 fFmod=SUBSTR(MArti[Sub],262,10)
 fMone=SUBSTR(MArti[Sub],272,3)
 fObse=SUBSTR(MArti[Sub],275,30)
ENDIF
DO VIEW_ART

*Se utilizada para cuando se esta buscando Relativamente con el ingreso del
*usuario. DESDE ser  > 0 hasta que se terminen de ejecuar internamente las
*Pulsaciones de Flecha Arriba ¢ Abajo para llegar el item de la matriz
*que coincida con el ingreso del Usuario
IF Desde>0
 MOD=3
ENDIF

DO CASE
 CASE MOD=0
  RETURN(2)
 CASE MOD=1 .OR. MOD=2
  TONO(2)
  RETURN(2)
 CASE MOD=3

  DO CASE
   CASE tcl=13 .OR. tcl=22    &&Enter o Insert
    RETURN(1)

   CASE tcl = 42
     RETURN(1)

   CASE tcl=-4
    XD=-1
    RETURN(0)

   CASE tcl=27          &&Presiona [ESC] para terminar
    RETURN(0)

   CASE tcl=275   &&Alt-R
    Cual=2
    RETURN(0)

   CASE tcl=281   &&Alt-P
    Cual=3
    RETURN(0)

   CASE tcl=306   &&Alt-M
    Cual=4
    RETURN(0)

   CASE tcl = 8 .AND. LEN(Busca) > 0
    Busca = SUBSTR(Busca,1,LEN(Busca)-1)
    @22,9 SAY Busca+SPACE(50-LEN(Busca))
    SETCOLOR("*"+Col_Act)
    @22,9+LEN(Busca) SAY '_ '
    SETCOLOR(Col_Act)
    RETURN(1)

   CASE ((tcl >= 32 .AND. tcl <= 122) .OR. (tcl >= 164 .AND. tcl <= 165)).OR.Desde>0
    *Busqueda relativa dentro de una matriz
    *Si es la Primer tecla Presionada
    IF Desde=0
     IF LEN(Busca)+1 <= 50
      Busca = Busca+CHR(tcl)
     ELSE
      TONE(2)
     ENDIF
     @22,9 SAY Busca+SPACE(50-LEN(Busca))
     SETCOLOR("*"+Col_Act)
     @22,9+LEN(Busca) SAY '_ '
     SETCOLOR(Col_Act)
     *Busqueda Propiamente dicha
     xPos=BUS_REL(Busca)
     IF xPos # Sub
      IF xPos < Sub
       Hasta= Sub - xPos
       Subir=.T.
      ELSE
       IF xPos > Sub
        Hasta= Xpos - Sub
        Subir=.F.
       ENDIF
      ENDIF
     ELSE
      RETURN(1)
     ENDIF
     Desde=1
     zPos=Sub
    ENDIF

    *For para ejecutar la Flecha Arriba o Abajo hasta encontrar el item buscado
    FOR zz=Desde TO Hasta+1
     *Ordena al Teclado Subir ¢ Bajar
     IF Subir
      KEYBOARD CHR(5)
      zPos=zPos-1
     ELSE
      KEYBOARD CHR(24)
      zPos=zPos+1
     ENDIF
     *Controla si ya se llego al Item Buscado
     IF xPos=zPos .OR. Desde=Hasta
      Desde=0
      IF Subir
       KEYBOARD CHR(5)
      ENDIF
      RETURN(1)
     ENDIF
     Desde=Desde+1
     RETURN(1)
    NEXT
    RETURN(1)

   CASE tcl=7
    RETURN(2)

   OTHERWISE
    RETURN(2)
  ENDCASE
ENDCASE

PROCEDURE VIEW_ART
PRIVATE Co_Ante
Co_Ante=SETCOLOR()
SETCOLOR(color1)
@ 9, 1 SAY xRubr
@ 9,21 SAY "³" COLOR color9
@ 9,28 SAY zProv
@ 9,58 SAY "³" COLOR color9
@20, 1 SAY fCPro
@12,68 SAY PADL(ALLTRIM(fUnid),10," ")
@13,68 SAY PADL(ALLTRIM(fFvta),10," ")
@14,68 SAY PADL(ALLTRIM(fUnVt),10," ")
@15,63 SAY PADL(ALLTRIM(fMedi),15," ")
@ 9,65 SAY zMrca
@ 9,75 SAY "³" COLOR color9
@11,14 SAY fObse
@16,68 SAY PADL(ALLTRIM(fClor),10," ")
@17,70 SAY PADL(ALLTRIM(fCali), 8," ")
@18,68 SAY PADL(ALLTRIM(fUsos),10," ")
@19,73 SAY PADL(ALLTRIM(fDrza), 5," ")
@20,68 SAY PADL(ALLTRIM(fRefe),10," ")
@18,33 SAY fMone COLOR IIF(fMone#"$","*"+color3,color1)
@20,22 SAY fPuCd
@20,34 SAY fPu30
@20,46 SAY fPu60
@18,40 SAY fFmod
SETCOLOR(Co_Ante)
RETURN

FUNCTION Fun_Rb
PARAMETERS MOD,sub,pos
LOCAL xx,pus,Oriy,ClaveGen,fColu,fFila
fColu=COL()
fFila=ROW()
SET KEY -4 TO
tcl=LASTKEY()
*Flechas guias
IF LEN(MRubr) < 4
 ydes = LEN(MRubr)
ELSE
 ydes = 4
ENDIF
IF ydes=1
 @4,0 SAY ''
ELSE
 pus = ydes/LEN(MRubr)*sub
 FOR oriy = 1 TO ydes
  Linea=IF(pus <= oriy .and. pus >= oriy-.99999,IIF(sub=1,'',IIF(sub=LEN(MRubr),'','')),'³')
  @3+oriy,0 SAY linea COLOR IIF(Linea="³",color1,color9)
 NEXT oriy
ENDIF
IF !EMPTY(SUBSTR(MRubr[sub],22,4))
 fRubr=SUBSTR(MRubr[sub],22,4)
 Nom_Rubr=SUBSTR(MRubr[sub],1,20)
ENDIF

*Aplica Filtros
DETALLE(fRubr,fProv,fMrca)

DO CASE
 CASE MOD=0
  RETURN(2)
 CASE MOD=1 .OR. MOD=2
  TONO(2)
  RETURN(2)
 CASE MOD=3
  DO CASE
   CASE tcl=13 .OR. tcl=22    &&Enter o Insert
     IF !Si_Hay
      CARTEL("SELECCIONE Rubro con Articulos [ OK ]",12,1,'N',color3)
      RETURN(2)
     ENDIF
     SETCOLOR(color9)
     fRubr=SUBSTR(MRubr[Sub],22,4)
     @fFila,fColu+20 SAY "" COLOR color3
     RETURN(1)

   *CASE tcl = 42
   *  IF !Si_Hay
   *   CARTEL("SELECCIONE Rubro con Articulos [ OK ]",12,1,'N',color3)
   *   RETURN(2)
   *  ENDIF
   *  RETURN(1)

   CASE tcl=9.OR.tcl=27      &&Tab
     IF !Si_Hay
      CARTEL("SELECCIONE Rubro con Articulos [ OK ]",12,1,'N',color3)
      RETURN(2)
     ENDIF
     SETCOLOR(color9)
     fRubr=SUBSTR(MRubr[Sub],22,4)
     @fFila,fColu+20 SAY "" COLOR color3
     RETURN(1)

   CASE tcl=7
    RETURN(2)

   OTHERWISE
    RETURN(3)
  ENDCASE
ENDCASE

FUNCTION Fun_Pr
PARAMETERS MOD,sub,pos
LOCAL xx,pus,Oriy,ClaveGen,fColu,fFila
LOCAL Nom_Prov
fColu=COL()
fFila=ROW()
SET KEY -4 TO
tcl=LASTKEY()
*Flechas guias
IF LEN(MProv) < 4
 ydes = LEN(MProv)
ELSE
 ydes = 4
ENDIF
IF ydes=1
 @4,27 SAY ''
ELSE
 pus = ydes/LEN(MProv)*sub
 FOR oriy = 1 TO ydes
  Linea=IF(pus <= oriy .and. pus >= oriy-.99999,IIF(sub=1,'',IIF(sub=LEN(MProv),'','')),'³')
  @3+oriy,27 SAY linea COLOR IIF(Linea="³",color1,color9)
 NEXT oriy
ENDIF
IF !EMPTY(SUBSTR(MProv[sub],32,4))
 fProv=SUBSTR(MProv[sub],32,4)
 Nom_Prov=SUBSTR(MProv[sub],1,30)
ENDIF

*Aplica Filtro
DETALLE(fRubr,fProv,fMrca)

DO CASE
 CASE MOD=0
  RETURN(2)
 CASE MOD=1 .OR. MOD=2
  TONO(2)
  RETURN(2)
 CASE MOD=3
  DO CASE
   CASE tcl=13 .OR. tcl=22    &&Enter o Insert
    IF !Si_Hay
     CARTEL("SELECCIONE Proveedor con Articulos [ OK ]",12,1,'N',color3)
     RETURN(2)
    ENDIF
    SETCOLOR(color9)
    fProv=SUBSTR(MProv[Sub],32,4)
    @fFila,fColu+30 SAY "" COLOR color3
    RETURN(1)

   *  CASE tcl = 42
   *   IF !Si_Hay
   *    CARTEL("SELECCIONE Proveedor con Articulos [ OK ]",12,1,'N',color3)
   *    RETURN(2)
   *   ENDIF
   *   SETCOLOR(color9)
   *   fProv=SUBSTR(MProv[Sub],32,4)
   *   @fFila,fColu+30 SAY "" COLOR color3
   *   RETURN(1)

   CASE tcl=-4
    XD=-1
    RETURN(0)

   CASE tcl=9.OR. tcl=27      &&Tab
    IF !Si_Hay
     CARTEL("SELECCIONE Proveedor con Articulos [ OK ]",12,1,'N',color3)
     RETURN(2)
    ENDIF
    SETCOLOR(color9)
    fProv=SUBSTR(MProv[Sub],32,4)
    @fFila,fColu+30 SAY "" COLOR color3
    RETURN(1)

   CASE tcl=7
    RETURN(2)

   OTHERWISE
    RETURN(3)
  ENDCASE
ENDCASE

FUNCTION Fun_Mr
PARAMETERS MOD,sub,pos
LOCAL xx,pus,Oriy,ClaveGen,fColu,fFila
LOCAL Nom_Mrca
fColu=COL()
fFila=ROW()
SET KEY -4 TO
tcl=LASTKEY()
*Flechas guias
IF LEN(MMrca) < 4
 ydes = LEN(MMrca)
ELSE
 ydes = 4
ENDIF
IF ydes=1
 @4,64 SAY ''
ELSE
 pus = ydes/LEN(MMrca)*sub
 FOR oriy = 1 TO ydes
  Linea=IF(pus <= oriy .and. pus >= oriy-.99999,IIF(sub=1,'',IIF(sub=LEN(MMrca),'','')),'³')
  @3+oriy,64 SAY linea COLOR IIF(Linea="³",color1,color9)
 NEXT oriy
ENDIF
IF !EMPTY(SUBSTR(MMrca[sub],12,3))
 fMrca=SUBSTR(MMrca[sub],12,3)
 Nom_Mrca=SUBSTR(MMrca[sub],1,10)
ENDIF

*Aplica Filtros
DETALLE(fRubr,fProv,fMrca)

DO CASE
 CASE MOD=0
  RETURN(2)
 CASE MOD=1 .OR. MOD=2
  TONO(2)
  RETURN(2)
 CASE MOD=3
  DO CASE
   CASE tcl=13 .OR. tcl=22    &&Enter o Insert
    IF !Si_Hay
     CARTEL("SELECCIONE Marcas con Articulos [ OK ]",12,1,'N',color3)
     RETURN(2)
    ENDIF
    SETCOLOR(color9)
    fMrca=SUBSTR(MMrca[Sub],12,3)
    @fFila,fColu+10 SAY "" COLOR color3
    RETURN(1)

   CASE tcl = 42
    IF !Si_Hay
     CARTEL("SELECCIONE Marcas con Articulos [ OK ]",12,1,'N',color3)
     RETURN(2)
    ENDIF
    SETCOLOR(color9)
    fMrca=SUBSTR(MMrca[Sub],12,3)
    @fFila,fColu+10 SAY "" COLOR color3
    RETURN(1)

   CASE tcl=-4
    XD=-1
    RETURN(0)

   CASE tcl=9 .OR. tcl=27     &&Tab
    IF !Si_Hay
     CARTEL("SELECCIONE Marcas con Articulos [ OK ]",12,1,'N',color3)
     RETURN(2)
    ENDIF
    SETCOLOR(color9)
    fMrca=SUBSTR(MMrca[Sub],12,3)
    @fFila,fColu+10 SAY "" COLOR color3
    RETURN(1)

   CASE tcl=7
    RETURN(2)

   OTHERWISE
    RETURN(3)
  ENDCASE
ENDCASE

FUNCTION DETALLE
PARAMETERS xR,xP,xM
LOCAL AreAnt
AreAnt=ALLTRIM(STR(SELECT()))
SELECT ARTIC
IF (xR#"0000".AND.xP="0000".AND.xM="000").OR.;
   (xR#"0000".AND.xP#"0000".AND.xM="000")
 DBSETORDER(1)
ELSE
 IF xR#"0000".AND.xP#"0000".AND.xM#"000"
  DBSETORDER(2)
 ELSE
  IF (xR="0000".AND.xP#"0000".AND.xM="000").OR.;
     (xR="0000".AND.xP#"0000".AND.xM#"000")
   DBSETORDER(3)
  ELSE
   IF xR="0000".AND.xP="0000".AND.xM#"000"
    DBSETORDER(4)
   ELSE
    IF xR#"0000".AND.xP="0000".AND.xM#"000"
     DBSETORDER(5)
    ENDIF
   ENDIF
  ENDIF
 ENDIF
ENDIF
ClavGen=IIF(xR="0000","",xR)+;
        IIF(xP="0000","",xP)+;
        IIF(xM="000","",xM)
SEEK ClavGen
IF !FOUND()
 *Borra zona de Articulos
 @12,1 CLEAR TO 17,56
 SETCOLOR(color1)
 @12,51 TO 17,51
 @15,1 SAY "NO ENCUNTRO ARTICULOS " COLOR color3
 SETCOLOR(color9)
 SELECT &AreAnt
 Si_Hay=.F.
 RETURN .F.
ENDIF
MArti:={}
xx=0
DBEVAL({|elem| AADD(MArti,nomb+'³'+arti+' '+PROVE->nomb+'³'+prov+cpro+unid+fvta+;
                          unvt+medi+MARCA->nomb+'³'+mrca+clor+cali+usos+drza+;
                          refe+STR(pucd,9,3)+STR(pucd*vPu30,9,3)+STR(pucd*vPu60,9,3)+;
                          RUBRO->nomb+'³'+rubr+DTOC(fmod)+mone+obse;
                       ),;
                          xx++;
       },;
       {|| IIF(xR#'0000',xR=rubr,.T.).AND.;
           IIF(xP#'0000',xP=prov,.T.).AND.;
           IIF(xM#'000',  xM=mrca,.T.);
       },;
       {|| IIF(xR#'0000',xR=rubr,.T.).AND.;
           IIF(xP#'0000',xP=prov,.T.).AND.;
           IIF(xM#'000',  xM=mrca,.T.);
       },,.T.)
IF xx#0

 *Ordenar Matriz Articulos
 ASORT(MArti,,,{|p1,p2| p1 < p2})

 SETCOLOR(color9)
 @12,1 CLEAR TO 17,56
 SETCOLOR(color1)
 @12,51 TO 17,51
 SETCOLOR(color9)
 FOR con=1 TO LEN(MArti)
  @con+11,1 SAY LEFT(MArti[con],56)
  IF con=6
   EXIT
  ENDIF
 NEXT
 Si_Hay=.T.
ELSE
 *Borra zona de Articulos
 @12,1 CLEAR TO 17,56
 SETCOLOR(color1)
 @12,51 TO 17,51
 @15,1 SAY "NO ENCUNTRO ARTICULOS " COLOR color3
 SETCOLOR(color9)
 SELECT &AreAnt
 Si_Hay=.F.
 RETURN .F.
 RETURN NIL
ENDIF
RETURN

*Pantalla
PROCEDURE PAN_CONSU
SETCOLOR(color9)
SOMBRA(1,0,24,79,'V5',3,'',colo3)
@1,10 SAY CENTRA("====== CONSULTA ARTICULOS ======  "+DTOC(DATE()),60," ") COLOR colo12
@ 2,0 SAY "ÚRubrosÄÄÄÄÄÄÄAlt-RÄÄÄÄÄÄÄ¿ÚProveedorÄÄÄÄÄÄAlt_PÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿ÚMarcasÄÄAlt-MÄ¿"
@ 3,0 SAY "ÃDescripci¢nÄÄÄÄÄÄÄÄÄÂC¢d.´ÃRaz¢n SocialÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂC¢d.´ÃDetalleÄÄÄÂC¢d´"
@ 4,0 SAY "³                    ³    ³³                              ³    ³³          ³   ³"
@ 5,0 SAY "³                    ³    ³³                              ³    ³³          ³   ³"
@ 6,0 SAY "³                    ³    ³³                              ³    ³³          ³   ³"
@ 7,0 SAY "³                    ³    ³³                              ³    ³³          ³   ³"
@ 8,0 SAY "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄ´ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄ´ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄ´"
@ 9,0 SAY "³                    ³    ³³                              ³    ³³          ³   ³"
@10,0 SAY "ÃArt¡culosÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÁÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÁÁÄÄÄÄÄÄÄÄÄÄÁÄÄÄ´"
@11,0 SAY "ÃDescripci¢nÄ(                              )ÄÄÄÄÄÄÂCod.ÄÂAtributosÄÄÄÄÄÄÄÄÄÄÄÄ´"
@12,0 SAY "³                                                  ³     ³Unidad:              ³"
@13,0 SAY "³                                                  ³     ³F.Vta.:              ³"
@14,0 SAY "³                                                  ³     ³U.FVta:              ³"
@15,0 SAY "³                                                  ³     ³Medi:                ³"
@16,0 SAY "³                                                  ³     ³Color :              ³"
@17,0 SAY "³                                                  ³     ³Calidad:             ³"
@18,0 SAY "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂP.Unitarios(ÄÄÄ)ÄÄ(  /  /    )ÁÄÄÄÄÄ´Util.en:             ³"
@19,0 SAY "ÚCod.ProveedorÄÄÄÄÄÄÅContadoÄÄÄÄÂa 30 d¡asÄÄÂa 60 d¡asÄÄÄ´Dureza :             ³"
@20,0 SAY "³                   ³           ³           ³            ³Refer. :             ³"
@21,0 SAY "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´"
@22,0 SAY "³Buscar:                                                                       ³"
@23,0 SAY "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
DIST_COLOR(2,14,'~*RG+/BG~Alt-R')
DIST_COLOR(2,43,'~*RG+/BG~Alt-P')
DIST_COLOR(2,73,'~*RG+/BG~Alt-M')
RETURN

*Determina Posicion Relativa en la busqueda
FUNCTION BUS_REL(filtro)
* Liga SOFTSEEK (PARA CLAVE MAYOR DE 1 POSICION) y aumenta el valor de la
*clave aumentando el valor de su £ltimo byte. Por ejemplo, si la £ltima letra
*fuera "a", se transformar¡a en "B"
  xPos=ASCAN(MArti,{|aVal| SUBSTR(aVal,1,LEN(Filtro))==UPPER(Filtro)})
  IF xPos=0
   xx=0
   xBusca=Filtro
   DO WHILE .T.
    IF xPos=0
     Filtro=xBusca
     XBusca=ULTIMACLAV(UPPER(Filtro))
     xPos=ASCAN(MArti,{|aVal| SUBSTR(aVal,1,LEN(Filtro))==xBusca})
    ELSE
     EXIT
    ENDIF
    xx++
    IF xx=100
     xPos=LEN(MArti)
     EXIT
    ENDIF
   ENDDO
  ENDIF
RETURN (xPos)

FUNCTION ULTIMACLAV(filtro)
LOCAL ultimoreg
ultimoreg := STUFF(filtro, LEN(filtro), 1, CHR( ASC(RIGHT(filtro,1)) + 1 ))
RETURN (ultimoreg)
