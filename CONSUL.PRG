*Carga de Partes y Repuestos para cada Parte
#INCLUDE "INKEY.CH"
#INCLUDE "SETCURS.CH"
LOCAL Pan_Ant,Col_Ant,xx

*Proveedores pasan a ser Listas de Precios

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
PRIVATE Si_Hay:=.T.,ClaveGen:=""
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
 *Cargar Matriz con Rubros, Listas de Precios y Marcas

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

 *Listas de Precios
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
 *Verificacmos Existencia de Listas de Precios
 IF xx=0
  CARTEL("NO HAY LISTAS DE PRECIOS CARGADOS [ OK ]",12,1,'N',color3)
  LOOP
 ENDIF
 *Ordenar Matriz Listas de Precios
 ASORT(MProv,,,{|p1,p2| p1 < p2})
 *Mostrar Listas de precios
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

 Cual=1

 DO WHILE .T.
  DO CASE
  CASE cual = 1
   *Articulos
   @24,0 SAY SPACE(80) COLOR color3
   @24,1 SAY "±Esc±-Salir  ±Alt-R±-Rubros ±Alt-L±-Listas ±Alt-M±-Marcas" COLOR color3
   DIST_COLOR(24,2,'~W+/R~Esc')
   DIST_COLOR(24,15,'~W+/R~Alt-R')
   DIST_COLOR(24,30,'~W+/R~Alt-L')
   DIST_COLOR(24,50,'~W+/R~Alt-M')
   Busca=""
   STORE 0 TO Desde,Hasta,xPos,zPos
   ***** BUCLE PRINCIPAL
   *Verifica Existencia de Articulos
   IF !DETALLE(fRubr,fProv,fMrca)
    EXIT
   ENDIF

   ********************************************
   *Genero el filtro de busqueda
   xR=fRubr
   xP=fProv
   xM=fMrca
   SELECT ARTIC
   Condi=""
   IF xR#"0000"
    Condi=Condi+"rubr=xR.AND."
   ENDIF
   IF xP#"0000"
    condi=Condi+"prov=xP.AND."
   ENDIF
   IF xM#"000"
    Condi=Condi+"mrca=xM.AND."
   ENDIF
   Condi=SUBSTR(Condi,1,LEN(Condi)-5)
   ********************************************

   DECLARE cam[2],sep[2],ind[1],lar[1],nom[1]
   carga1(cam,"nomb~arti")
   carga2(sep,'1,0')
   tit="Descripci¢nÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"
   ind[1] = 1
   lar[1] = 50
   nom[1] = ""
   ord_ant = STR(INDEXORD())
   SET ORDER TO 7
   SETCOLOR(color9)
   SET FILTER TO &Condi
   GO TOP
   BUS_ART(11,1,17,56,cam,tit,sep,ind,lar,22,10,nom,22,09)
   SETCOLOR(color9)
   SET ORDER TO &ord_ant
   SET FILTER TO
   GO TOP

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
   *Listas de Precios
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
  @3+oriy,0 SAY linea COLOR IIF(Linea="³",color9,color1)
 NEXT oriy
ENDIF
IF !EMPTY(SUBSTR(MRubr[sub],22,4))
 fRubr=SUBSTR(MRubr[sub],22,4)
 Nom_Rubr=SUBSTR(MRubr[sub],1,20)
ENDIF

DO CASE
 CASE MOD=0
  RETURN(2)
 CASE MOD=1 .OR. MOD=2
  TONO(2)
  RETURN(2)
 CASE MOD=3
  DO CASE

   CASE tcl=9  &&Tab
     *Verifica Existencia de Articulos
     DETALLE(fRubr,fProv,fMrca)
     IF !Si_Hay
      CARTEL("SELECCIONE Rubro con Articulos [ OK ]",12,1,'N',color3)
      RETURN(2)
     ENDIF
     SETCOLOR(color9)
     fRubr=SUBSTR(MRubr[Sub],22,4)
     @fFila,fColu+20 SAY "" COLOR color3
     RETURN(1)

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
  @3+oriy,27 SAY linea COLOR IIF(Linea="³",color9,color1)
 NEXT oriy
ENDIF
IF !EMPTY(SUBSTR(MProv[sub],32,4))
 fProv=SUBSTR(MProv[sub],32,4)
 Nom_Prov=SUBSTR(MProv[sub],1,30)
ENDIF

DO CASE
 CASE MOD=0
  RETURN(2)
 CASE MOD=1 .OR. MOD=2
  TONO(2)
  RETURN(2)
 CASE MOD=3
  DO CASE

   CASE tcl=9  &&Tab
    *Verifica Existencia de Articulos
    DETALLE(fRubr,fProv,fMrca)
    IF !Si_Hay
     CARTEL("SELECCIONE Lista con Articulos [ OK ]",12,1,'N',color3)
     RETURN(2)
    ENDIF
    SETCOLOR(color9)
    fProv=SUBSTR(MProv[Sub],32,4)
    @fFila,fColu+30 SAY "" COLOR color3
    RETURN(1)

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
  @3+oriy,64 SAY linea COLOR IIF(Linea="³",color9,color1)
 NEXT oriy
ENDIF
IF !EMPTY(SUBSTR(MMrca[sub],12,3))
 fMrca=SUBSTR(MMrca[sub],12,3)
 Nom_Mrca=SUBSTR(MMrca[sub],1,10)
ENDIF

DO CASE
 CASE MOD=0
  RETURN(2)
 CASE MOD=1 .OR. MOD=2
  TONO(2)
  RETURN(2)
 CASE MOD=3
  DO CASE

   CASE tcl = 9 &&Tab
    *Verifica Existencia de Articulos
    WAIT FRUBR
    WAIT FPROV
    WAIT FMRCA

    DETALLE(fRubr,fProv,fMrca)
    IF !Si_Hay
     CARTEL("SELECCIONE Marcas con Articulos [ OK ]",12,1,'N',color3)
     RETURN(2)
    ENDIF
    SETCOLOR(color9)
    fMrca=SUBSTR(MMrca[Sub],12,3)
    @fFila,fColu+10 SAY "" COLOR color3
    RETURN(1)

   OTHERWISE
    RETURN(3)
  ENDCASE
ENDCASE

FUNCTION DETALLE
PARAMETERS xR,xP,xM
LOCAL AreAnt
AreAnt=ALLTRIM(STR(SELECT()))
SELECT ARTIC
IF (xP="0000".AND.xM="000").OR.;
   (xP#"0000".AND.xM="000")
   DBSETORDER(1)
ELSE
 IF xP#"0000".AND.xM#"000"
  DBSETORDER(1)
 ELSE
  IF (xP#"0000".AND.xM="000").OR.;
     (xP#"0000".AND.xM#"000")
     DBSETORDER(1)
  ELSE
   IF xP="0000".AND.xM#"000"
    DBSETORDER(4)
   ENDIF
  ENDIF
 ENDIF
ENDIF
ClaveGen=IIF(xR="0000","",xR)+;
        IIF(xP="0000","",xP)+;
        IIF(xM="000","",xM)
SEEK ClaveGen
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
ELSE
 Si_Hay=.T.
ENDIF
RETURN .T.

*Pantalla
PROCEDURE PAN_CONSU
SETCOLOR(color9)
SOMBRA(1,0,24,79,'V5',3,'',colo3)
@1,10 SAY CENTRA("====== CONSULTA ARTICULOS ======  "+DTOC(DATE()),60," ") COLOR colo12
@ 2,0 SAY "ÚRubrosÄÄÄÄÄÄÄAlt-RÄÄÄÄÄÄÄ¿ÚLista de PreciosÄÄÄÄÄAlt-LÄÄÄÄÄÄÄÄÄ¿ÚMarcasÄÄAlt-MÄ¿"
@ 3,0 SAY "ÃDescripci¢nÄÄÄÄÄÄÄÄÄÂC¢d.´ÃDescripci¢nÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂC¢d.´ÃDetalleÄÄÄÄÄÄÄ´"
@ 4,0 SAY "³                    ³    ³³                              ³    ³³              ³"
@ 5,0 SAY "³                    ³    ³³                              ³    ³³              ³"
@ 6,0 SAY "³                    ³    ³³                              ³    ³³              ³"
@ 7,0 SAY "³                    ³    ³³                              ³    ³³              ³"
@ 8,0 SAY "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄ´ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄ´ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´"
@ 9,0 SAY "³                    ³    ³³                              ³    ³³              ³"
@10,0 SAY "ÃArt¡culosÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÁÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÁÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´"
@11,0 SAY "ÃDescripci¢nÄ(                              )ÄÄÄÄÄÄÄÄÄÄÄÄÂAtributosÄÄÄÄÄÄÄÄÄÄÄÄ´"
@12,0 SAY "³                                                        ³Unidad:              ³"
@13,0 SAY "³                                                        ³F.Vta.:              ³"
@14,0 SAY "³                                                        ³U.FVta:              ³"
@15,0 SAY "³                                                        ³Medi:                ³"
@16,0 SAY "³                                                        ³Color :              ³"
@17,0 SAY "³                                                        ³Calidad:             ³"
@18,0 SAY "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂP.Unitarios(ÄÄÄ)ÄÄ(  /  /    )ÄÄÄÄÄÄ´Util.en:             ³"
@19,0 SAY "ÚCod.ProveedorÄÄÄÄÄÄÅContadoÄÄÄÄÂa 30 d¡asÄÄÂa 60 d¡asÄÄÄ´Dureza :             ³"
@20,0 SAY "³                   ³           ³           ³            ³Refer. :             ³"
@21,0 SAY "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´"
@22,0 SAY "³Buscar:                                                                       ³"
@23,0 SAY "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
DIST_COLOR(2,14,'~*RG+/BG~Alt-R')
DIST_COLOR(2,49,'~*RG+/BG~Alt-L')
DIST_COLOR(2,73,'~*RG+/BG~Alt-M')
RETURN

FUNCTION BUS_ART
PARAMETERS fs,ci,fi,cd,campo,titulo,separacion,indice,largo,fi_ed,co_ed,nombre,fi_no,co_no
PRIVATE cant_camp,reg_ant,camp_dbe[1],titu_dbe[1],punt_ind,BUSCA,col_act
cant_camp = LEN(campo)
DO CASE
CASE PCOUNT() = 5
 PRIVATE separacion[cant_camp],tiene_ind
 titulo = ''
 AFILL(separacion,1)
 tiene_ind = .F.

CASE PCOUNT() = 6
 PRIVATE separacion[cant_camp],tiene_ind
 AFILL(separacion,1)
 tiene_ind = .F.

CASE PCOUNT() = 7
 PRIVATE tiene_ind
 tiene_ind = .F.

CASE PCOUNT() = 14
 PRIVATE tiene_ind
 tiene_ind = .T.

OTHERWISE
 SETCOLOR("")
 CLEAR
 @ 0, 0 SAY 'ERROR: NUMERO DE PARAMETROS ERRONEOS EN BUSCAR()'
 QUIT

ENDCASE
reg_ant = RECNO()
camp_dbe[1] = ''
titu_dbe[1] = titulo
BUSCA = ''
col_act = SETCOLOR()
IF tiene_ind
 con = ASCAN(indice,INDEXORD())
 punt_ind = IF(con = 0,1,con)
 @fi_no,co_no SAY nombre[punt_ind]
 SETCOLOR("*"+col_act)
 @fi_ed,co_ed SAY '_'
 SETCOLOR(col_act)
ENDIF
FOR con = 1 TO cant_camp
 camp_dbe[1] = camp_dbe[1]+campo[con]+'+"'+SPACE(separacion[con])+'"'+IF(con < cant_camp,'+','')
NEXT con
SETCOLOR(col_act)
DBEDIT(fs,ci,fi,cd,camp_dbe,'FUN_BUS','',titu_dbe,'','','')
SETCOLOR(col_act)
IF LASTKEY() = 27
 GO reg_ant
ENDIF
RETURN(.T.)

FUNCTION FUN_BUS
PARAMETERS modo
PRIVATE tecla
tecla = LASTKEY()
DO CASE
CASE modo = 0
 DO VIEW_ART
 RETURN(1)

CASE modo = 1 .OR. modo = 2
 TONE(600,2)
 DO VIEW_ART
 RETURN(1)

CASE modo = 3 .OR. tecla=275 .OR. tecla = 294 .OR. tecla=306 .OR. tecla =27;
        .OR. tecla=292
 IF tecla=275
  *Alt-R
  cual=2
 ELSE
  IF tecla=294
   *Alt-L
   cual=3
  ELSE
   IF tecla=306
    *Alt-M
    cual=4
   ENDIF
  ENDIF
 ENDIF
 IF tecla=292  &&Modificar precio Alt-J
  xPuCd=pucd
  SET CURSOR ON
  @20,22 GET xPuCd PICTURE "@K 99999.999"
  READ
  IF LASTKEY()=27
   SET CURSOR OFF
   RETURN(1)
  ENDIF
  IF xPuCd # pucd
   Bien=CARTEL("ESTA SEGURO ??? [ SI ] [ NO ]",17,2,'N',colo3)
   IF !Bien .OR. LASTKEY()=27
    SET CURSOR OFF
    RETURN(1)
   ENDIF
   RECLOCK(1)
   REPLACE pucd WITH xPuCd
   REPLACE fmod WITH DATE()
   UNLOCK
   SET CURSOR OFF
   RETURN(1)
  ENDIF
 ENDIF
 RETURN(0)

CASE tiene_ind .and. tecla = -1
 punt_ind = IF(punt_ind < LEN(indice),punt_ind+1,1)
 SET ORDER TO indice[punt_ind]
 @fi_no,co_no SAY nombre[punt_ind]
 @fi_ed,co_ed SAY SPACE(largo[punt_ind])
 BUSCA = ''
 SETCOLOR("*"+col_act)
 @fi_ed,co_ed+LEN(BUSCA) SAY '_ '
 SETCOLOR(col_act)
 DO VIEW_ART
 RETURN(2)

CASE tiene_ind .and. tecla = 8 .and. LEN(BUSCA) > 0
 BUSCA = SUBSTR(BUSCA,1,LEN(BUSCA)-1)
 @fi_ed,co_ed SAY BUSCA+SPACE(largo[punt_ind]-LEN(BUSCA))
 SETCOLOR("*"+col_act)
 @fi_ed,co_ed+LEN(BUSCA) SAY '_ '
 SETCOLOR(col_act)
 DO VIEW_ART
 RETURN(1)

CASE tiene_ind .and. ((tecla >= 32 .and. tecla <= 122) .OR. (tecla >= 164 .and. tecla <= 165))
 IF INDEXORD()#0
  IF LEN(BUSCA)+1 <= largo[punt_ind]
   BUSCA = BUSCA+CHR(tecla)
  ELSE
   TONE(2)
  ENDIF
  @fi_ed,co_ed SAY BUSCA+SPACE(largo[punt_ind]-LEN(BUSCA))
  SETCOLOR("*"+col_act)
  @fi_ed,co_ed+LEN(BUSCA) SAY '_ '
  SETCOLOR(col_act)
  SET SOFTSEEK ON
  SEEK(BUSCA)
  IF !FOUND()
   SEEK(UPPER(BUSCA))
  ENDIF
  SET SOFTSEEK OFF
 ENDIF
 DO VIEW_ART
 RETURN(1)

OTHERWISE
 DO VIEW_ART
 RETURN(1)
ENDCASE

PROCEDURE VIEW_ART
PRIVATE Co_Ante
Co_Ante=SETCOLOR()
SETCOLOR(color1)
*Leer Datos del articulo
fArti   = arti
Nom_arti= nomb
zProv   = PROVE->nomb+" "+Prov
fCpro   = Cpro
funid   = unid
fFvta   = Fvta
fUnVt   = UnVt
fMedi   = Medi
zMrca   = MARCA->nomb+" "+mrca
fClor   = Clor
fCali   = Cali
fUsos   = Usos
fDrza   = Drza
fRefe   = Refe
fPuCd   = STR(pucd,9,3)
fPu30   = STR(pucd*vPu30,9,3)
fPu60   = STR(pucd*vPu60,9,3)
xRubr   = RUBRO->nomb+" "+rubr
fFmod   = Fmod
fMone   = Mone
fObse   = Obse

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
IF EMPTY(fObse)
 @11,14 SAY "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" COLOR color9
ELSE
 @11,14 SAY fObse
ENDIF
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
