*Carga de Presupuestos
PARAMETERS vOpcion
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
  IF !FILE('MAEARTI6&vRDD.')
   INDEX ON arti TO MAEARTI6
  ENDIF
  SET INDEX TO MAEARTIC,MAEARTI1,MAEARTI2,MAEARTI3,MAEARTI4,MAEARTI5,NOMARTIC,MAEARTI6
 ELSE
  QUIT
 ENDIF
 SET ORDER TO 8
 GO BOTTOM
 Ult_Arti=arti
 SET ORDER TO 1


*Numeraci¢n de Presupuestos
AREA()
OPEN_DBF("NUMPRESU","NUMPR","C","NUMPRESU","nano")

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
SET RELATION TO rubr INTO RUBRO, TO mrca INTO MARCA, TO arti INTO ARTIC


*Variables Presupuesto
PRIVATE vFech,vNpre,vNano,vClie,vCpag,vFvto,agg:=0,baja,vObse,sale:=0,xDolar:=vDolar

*Variables Generales
PRIVATE Com_Rbr,Com_Pro,Com_Mrc,Com_Art,tcl,Rel:=0
PRIVATE Com:=1,Rel:=0,Busca,Hasta,Desde,xPos,zPos,Subir
PRIVATE fRubr,fProv,fMrca,Nom_Rubr
PRIVATE Si_Hay:=.T.,ClaveGen:=""
PRIVATE MRubr:={}
PUBLIC MProv:={}
PRIVATE MMrca:={}
PRIVATE MArti:={}
*Modificacion de Presupuestos
Baja=.F.
IF vOpcion=2
 DO WHILE .T.
  SOMBRA(5,19,9,44,'H5',1,'',colo3)
  @  6,20 SAY "ÚPresupuestoÄÄÄÄÄÄÄÄÄ¿"
  @  7,20 SAY "³  N£mero:           ³"
  @  8,20 SAY "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
  vNano=CERO(YEAR(DATE()),4)
  vNpre=SPACE(4)
  @7,31 GET vNano PICTURE "@K 9999"
  @7,36 GET vNpre PICTURE "@K 9999"
  READ
  IF LASTKEY()=27
   IF baja
    DO ELIMINAR
   ENDIF
   CLOSE ALL
   RESTSCREEN(,,,,pan_ant)
   SETCOLOR(col_ant)
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
   agg=0
   SELECT PREDE
   PRIVATE MItems:={}
   SEEK vNano+vNpre
   IF FOUND()
    p=1
    DO WHILE !EOF().AND.nano+npre = vNano+vNpre
    *Agregar a la matriz
     AADD(MItems,nomb+;
                 STR(Cant,8,2)+;
                 STR(puni,9,3)+;
                 ARTIC->unid+;
                 ARTIC->medi+;
                 ARTIC->mrca+;
                 MARCA->nomb+;
                 rubr+;
                 arti+;
                 mone+;
                 IIF(mone="U$S".OR.mone="USD",STR(dolar,7,4),STR(1,7,4));
                 )
     IF ALLTRIM(mone)#"$"
      xDolar=dolar
     ENDIF
     agg=1
     p=p+1
     SKIP
    ENDDO
    IF agg=1
     EXIT
    ELSE
     CARTEL("PRESUPUESTO SIN ITEMS CARGADOS [ OK ]",12,1,'N',color3)
    ENDIF
   ELSE
    CARTEL("NO ENCUENTRO PRESUPUESTO [ OK ]",12,1,'N',color3)
    LOOP
   ENDIF
  ELSE
   CARTEL("NO ENCUENTRO PRESUPUESTO [ OK ]",12,1,'N',color3)
   LOOP
  ENDIF
 ENDDO
ENDIF

SELECT RUBRO
GO TOP
fRubr=rubr

DO WHILE .T.
 *Mostrar En Pantalla
 DO PAN_CONSU
 SETCOLOR(color9)
 @1,10 SAY CENTRA("========= PRESUPUESTOS =========  "+DTOC(DATE())+"  1 usd = $"+STR(vDolar,8,4),60," ") COLOR colo12
 STORE "0000" TO fProv
 fMrca="000"
 PRIVATE MRubr:={}
 PRIVATE MProv:={}
 PRIVATE MMrca:={}
 PRIVATE MArti:={}
 *Items del Presupuesto
 IF vOpcion=1
  PRIVATE MItems:={}
  vFech=DATE()
  vNano=CERO(YEAR(DATE()),4)
  vNPre="----"
  vClie=SPACE(30)
  vCpag=SPACE(30)
  vFvto=SPACE(30)
  vObse=SPACE(40)
 ENDIF
 PRIVATE oTAB
 *Cargar Matriz con Rubros, Listas de Precios y Marcas

 *Rubros
 Com_Rbr=1
 xx=0
 SELECT RUBRO
 GO TOP
 DBEVAL({|elem| AADD(MRubr,;
                     rubr;
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
 @4,5 SAY "" COLOR color3

 *Listas de Precios
 Com_Pro=1
 xx=0
 SELECT PROVE
 GO TOP
 AADD(MProv," ** Todos **                            ³0000")
 DBEVAL({|elem| AADD(MProv,;
                     nomb+'³'+prov;
                        ),;
                        xx++;
        },;
         {|| rubr=fRubr;
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
  @con+3,7 SAY SUBSTR(MProv[con],1,40)
  IF con=4
   EXIT
  ENDIF
 NEXT
 @4,47 SAY "" COLOR color3

 *Marcas
 Com_Mrc=1
 xx=0
 SELECT MARCA
 GO TOP
 AADD(MMrca," *Todas *      ³000")
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
  @con+3,49 SAY SUBSTR(MMrca[con],1,15)
  IF con=4
   EXIT
  ENDIF
 NEXT
 @4,64 SAY "" COLOR color3

 Cual=1

 DO WHILE .T.
  DO CASE
  CASE cual = 1
   *Articulos
   @24,0 SAY SPACE(80) COLOR color3
   @24,1 SAY "±Esc±-Salir  ±Alt-± R-Rubros L-Listas M-Marcas ÄÙ-Agregar V-Ver Presu." COLOR color3
   DIST_COLOR(24,2,'~W+/R~Esc')
   DIST_COLOR(24,15,'~W+/R~Alt')
   DIST_COLOR(24,21,'~W+/R~R')
   DIST_COLOR(24,30,'~W+/R~L')
   DIST_COLOR(24,39,'~W+/R~M')
   DIST_COLOR(24,48,'~W+/R~ÄÙ')
   DIST_COLOR(24,60,'~W+/R~V')
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
   tit="Descripci¢nÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄ"
   ind[1] = 1
   lar[1] = 50
   nom[1] = ""
   ord_ant = STR(INDEXORD())
   SET ORDER TO 7
   SETCOLOR(color9)
   SET FILTER TO &Condi
   GO TOP
   BUS_ART1(09,1,15,78,cam,tit,sep,ind,lar,22,10,nom,22,09)
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
    Com_Rbr = ACHOICE(4,1,7,4,MRubr,.T.,"Fun_Rb",Com_Rbr,rel)
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
    Com_Pro = ACHOICE(4,7,7,46,MProv,.T.,"Fun_Pr",Com_Pro,rel)
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
    Com_Mrc = ACHOICE(4,49,7,63,MMrca,.T.,"Fun_Mr",Com_Mrc,rel)
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
IF baja
 DO ELIMINAR
ENDIF
CLOSE ALL
SETCOLOR(Col_Ant)
RESTSCREEN(,,,,Pan_Ant)
RETURN

FUNCTION BUS_ART1
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
DBEDIT(fs,ci,fi,cd,camp_dbe,'FUN_BUS1','',titu_dbe,'','','')
SETCOLOR(col_act)
IF LASTKEY() = 27
 GO reg_ant
ENDIF
RETURN(.T.)

FUNCTION FUN_BUS1
PARAMETERS modo
PRIVATE tecla,fFil,fCol
fFil=ROW()
fCol=COL()
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
              .OR. tecla=13 .OR. tecla=303

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
 IF tecla =303 &&Alt-V
  Pan_P=SAVESCREEN(,,,,)
  DO VERPRE
  SET CURSOR OFF
  com = 1
  rel = 0
  KEYBOARD CHR(24)+CHR(5)
  DO WHILE .T.
   com = ACHOICE(15,1,18,77,MItems,.T.,"ver_art",com,rel)
   IF com = 0
    EXIT
   ENDIF
  ENDDO
  inkey(.1)
  IF agg=5
   ULTECLA(27)
  ENDIF
  IF LASTKEY() = 27
   IF agg#5
    ULTECLA(42)
   ENDIF
   RESTSCREEN(,,,,Pan_P)
   RETURN(0)
  ENDIF
  SET KEY -4 TO
  SET CURSOR ON
  RESTSCREEN(,,,,Pan_P)
 ENDIF
 IF tecla=13
  *Agregar
  *Resalta Nombre de Articulo elegido
  @fFil,fCol SAY nomb COLOR color8
  xPuCd=pucd
  xcant=0
  SET CURSOR ON
  Colu=SETCOLOR()
  @19,33 SAY "CantidadÄÄ" COLOR color9
  @20,22 GET xPuCd PICTURE "@K 99999.999"
  @20,35 GET xCant PICTURE "@K 99999.99"
  READ
  SETCOLOR(Colu)
  IF LASTKEY()=27
   SET CURSOR OFF
   RETURN(1)
  ENDIF
  IF xCant>0
   Bien=CARTEL("ESTA SEGURO DE AGREGAR ??? [ SI ] [ NO ]",12,2,'N',colo3)
   IF !Bien .OR. LASTKEY()=27
    SET CURSOR OFF
    @19,33 SAY "a 30 D¡as" COLOR color9
    @20,22 SAY Pucd PICTURE "@Z 99999.999"
    @20,34 SAY Pucd*vPu30 PICTURE "@Z 99999.999"
    @20,46 SAY Pucd*vPu60 PICTURE "@Z 99999.999"
    RETURN(1)
   ENDIF
   @19,33 SAY "a 30 D¡as" COLOR color9
   @20,22 SAY Pucd PICTURE "@Z 99999.999"
   @20,34 SAY Pucd*vPu30 PICTURE "@Z 99999.999"
   @20,46 SAY Pucd*vPu60 PICTURE "@Z 99999.999"

   *Agregar a la matriz
   AADD(MItems,nomb+STR(xCant,8,2)+STR(xPuCd,9,3)+unid+medi+mrca+MARCA->nomb+rubr+arti+mone+IIF(mone="U$S".OR.mone="USD",STR(xDolar,8,4),STR(1,8,4)))

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

PROCEDURE VERPRE
SETCOLOR(colo3)
SOMBRA(6,0,22,77,'H5',1,'',colo3)
@  6,0 SAY "                ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂPresupuestoÄÄÄÄÄÄÄÄÄÂAnteriorÄÄ¿           "
@  7,0 SAY "                ³ Fecha:            ³  N£mero:           ³          ³           "
@  8,0 SAY "        ÚÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´           "
@  9,0 SAY "        ³Cliente       :                                            ³           "
@ 10,0 SAY "        ³Forma de Pago :                                            ³           "
@ 11,0 SAY "        ³Validez Oferta:                                            ³           "
@ 12,0 SAY "        ÀObservaciones :                                            Ù           "
@ 13,0 SAY "ÚDetalle Art¡culoÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
@ 14,0 SAY "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´"
@ 15,0 SAY "³                                                                              ³"
@ 16,0 SAY "³                                                                              ³"
@ 17,0 SAY "³                                                                              ³"
@ 18,0 SAY "³                                                                              ³"
@ 19,0 SAY "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
@ 20,0 SAY "ÚRubroÂMarcaÄÄÄÄÄÄÄÄÄÄÂMedidasÄÄÄÄÄÄÄÄÂUnidadÄÄÄÄÂCantidadÂP.Unit.ÄÄ¿ÚTotal ($)¿"
@ 21,0 SAY "³     ³               ³               ³          ³        ³         ³³         ³"
@ 22,0 SAY "ÀÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÙÀÄÄÄÄÄÄÄÄÄÙ"
SETCOLOR(color3)
@ 24,0 SAY "±*±-Guardar Presupuesto  ±Del±-Borrar Item ±Esc±-Volver a Selecci¢n                    "
DIST_COLOR(24,1,'~W+/R~*')
DIST_COLOR(24,26,'~W+/R~Del')
DIST_COLOR(24,44,'~W+/R~Esc')
SETCOLOR(colo3)
RETURN

FUNCTION ver_art
PARAMETERS MOD,sub,pos
PRIVATE tcl,fFil,fCol,pus,oriy,con,ydes
fFil=ROW()
fCol=COL()
IF LEN(MItems) < 4
 ydes = LEN(MItems)
ELSE
 ydes = 4
ENDIF
IF ydes = 1
 @15,0 SAY ''
ELSE
 pus = 4/LEN(MItems)*sub
 FOR oriy = 1 TO 4
    @14+oriy,0 SAY IF(pus <= oriy .and. pus >= oriy-.99999,IIF(sub=1,'',IIF(sub=LEN(MItems),'','')),'³')
 NEXT oriy
ENDIF
tcl = LASTKEY()
*Imprime cabecera cuando es MODIFICACION
@7,26 SAY vFech PICTURE "@D"
@7,48 SAY vNano+'/'+vNpre COLOR colo12
@ 9,25 SAY vClie COLOR colo12
@10,25 say vCpag COLOR colo12
@11,25 SAY vFvto COLOR colo12
@12,25 SAY vObse COLOR colo12
@11,72 SAY "Dolar: "
@12,70 SAY STR(xDolar,8,4) COLOR colo12
*Imprime detalles del Articulo
@21,2 SAY SUBSTR(MItems[sub],139,4)
@21,7 SAY SUBSTR(MItems[sub],124,15)
@21,23 SAY SUBSTR(MItems[sub],106,15)
@21,39 SAY SUBSTR(MItems[sub],96,10)
@21,50 SAY SUBSTR(MItems[sub],79,8)
@21,59 SAY SUBSTR(MItems[sub],87,9)
xPuCd=VAL(SUBSTR(MItems[sub],87,9))
xcant=VAL(SUBSTR(MItems[sub],79,8))
xMone=SUBSTR(MItems[sub],148,3)
@22,59 SAY "("+xMone+")" COLOR color3
xTotal=0
FOR con=1 TO LEN(MItems)
 fMone=SUBSTR(MItems[con],148,3)
 fPuCd=VAL(SUBSTR(MItems[con],87,9))
 fcant=VAL(SUBSTR(MItems[con],79,8))
 xTotal = xTotal + ((fPuCd*fCant)*IIF(fMone="U$S".OR.fMone="USD",xDolar,1))
NEXT
@21,70 SAY xTotal PICTURE "@Z 999999.99"
DO CASE
CASE MOD = 0
 RETURN(2)
CASE MOD = 1 .OR. MOD = 2
 TONO(2)
 RETURN(2)
CASE MOD = 3
 DO CASE
 CASE tcl = 27         &&Presiona [ESC] para terminar Pagos
  KEYBOARD CHR(27)
  RETURN(0)
 CASE tcl = 13
   *Modificar cantidad y Precio
   *Resalta Nombre de Articulo elegido
   @fFil,fCol SAY nomb COLOR color8
   SET CURSOR ON
   @21,50 GET xCant PICTURE "@K 99999.99"
   @21,59 GET xPuCd PICTURE "@K 99999.999"
   READ
   IF LASTKEY()=27
    SET CURSOR OFF
    RETURN(1)
   ENDIF
   IF xCant>0
    Bien=CARTEL("DATOS CORRECTOS ??? [ SI ] [ NO ]",12,2,'N',colo3)
    IF !Bien .OR. LASTKEY()=27
     SET CURSOR OFF
     @21,50 SAY xCant PICTURE "@Z 99999.99"
     @21,59 SAY xPuCd PICTURE "@Z 99999.999"
     RETURN(1)
    ENDIF
    @21,50 SAY xCant PICTURE "@Z 99999.99"
    @21,59 SAY xPuCd PICTURE "@Z 99999.999"

    *Ingresar Modificacion a matriz
    MItems[sub]=STUFF(MItems[sub],79,8,STR(xCant,8,2))
    MItems[sub]=STUFF(MItems[sub],87,9,STR(xPuCd,9,3))


    SET CURSOR OFF
    RETURN(1)
   ENDIF

   KEYBOARD CHR(24)
   rel = pos
   RETURN(1)

 CASE tcl=7
  *Borrar
  IF LEN(MItems)=1
   CARTEL("SE QUEDARµ SIN ARTÖCULOS [ OK ]",12,1,'N',color3)
   ASIZE(MItems,0)
   RETURN(0)
  ENDIF
  ADEL(MItems,sub)
  ASIZE(MItems,LEN(MItems)-1)
  KEYBOARD CHR(24)
  rel=pos
  RETURN(1)


 CASE tcl=42
  *Gravar
  SET KEY -4 TO VA_TRAS
  IF vOpcion=1
   *Busco Nro. Presupuesto anterior para el A¤o en curso
   SELECT NUMPR
   SEEK vNano
   IF !FOUND()
    vAnte="0000"
    vNpre="0001"
   ELSE
    vAnte=CERO(VAL(npre),4)
    vNpre=CERO(VAL(npre)+1,4)
   ENDIF
   @7,59 SAY vNano+'/'+vAnte COLOR "*"+colo12
  ENDIF
  SELECT PRECB
  @7,48 SAY vNano+'/'+vNpre COLOR colo12
  XPM=5
  XP=1
  DO WHILE XP<=XPM
   XD=1
   SET CURSOR ON
   DO CASE
    CASE XP=1
     *Fecha
     Sale=0
     @7,26 GET vFech PICTURE "@D" VALID !EMPTY(vFech)
     READ
     @7,26 SAY vFech PICTURE "@D" COLOR colo12

    CASE XP=2
     *Clinte
     Sale=0
     @9,25 GET vClie PICTURE "@!"
     READ
     @9,25 SAY vClie COLOR colo12

    CASE XP=3
     *Forma de Pago
     Sale=0
     @10,25 GET vCpag PICTURE "@!"
     READ
     @10,25 say vCpag COLOR colo12

    CASE XP =4
     *Validez de la Oferta
     Sale=0
     @11,25 GET vFvto PICTURE "@!"
     READ
     @11,25 SAY vFvto COLOR colo12

    CASE XP =5
     *Observaciones
     @12,25 GET vObse PICTURE "@!"
     READ
     @12,25 SAY vObse COLOR colo12

   ENDCASE
   IF LASTKEY()=27
    CLEAR GETS
    XP=6
   ELSE
    XP=XP+XD
   ENDIF
  ENDDO
  IF LASTKEY()#27
   *Guardamos
   Bien=CARTEL("DATOS CORRECTOS [ SI ] [ NO ]",16,1,'N',colo3)
   IF !Bien.OR.lastkey()=27
    KEYBOARD CHR(24)
    rel = pos
    RETURN(1)
   ENDIF
   *Guardar Informaci¢n
   DO GRA_REG
   IF vOpcion=2
    baja=.T.
   ENDIF
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
   ASIZE(MItems,0)
   agg=5
   RETURN(999)
  ENDIF

  KEYBOARD CHR(24)
  rel = pos
  RETURN(1)
 OTHERWISE
  RETURN(2)
 ENDCASE
ENDCASE

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
FOR con = 1 TO LEN(MItems)
 IF !EMPTY(SUBSTR(MItems[con],1,5))
  ADDREC(0)
  RECLOCK(0)
  REPLACE nano WITH vNano
  REPLACE npre WITH vNpre
  REPLACE fech WITH vFech
  REPLACE rubr WITH SUBSTR(MItems[con],139,4)
  REPLACE arti WITH SUBSTR(MItems[con],143,5)
  REPLACE nomb WITH SUBSTR(MItems[con],1,78)
  REPLACE cant WITH VAL(SUBSTR(MItems[con],79,8))
  REPLACE puni WITH VAL(SUBSTR(MItems[con],87,9))
  REPLACE mone WITH SUBSTR(MItems[con],148,3)
  fMone=SUBSTR(MItems[con],148,3)
  REPLACE dolar WITH IIF(ALLTRIM(fMone)="$",1,xDolar)
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

PROCEDURE Imp_Presu
vtot=0
vsub=0
Pos=1
pagina=0
hor=0
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
 ? "Tel‚fono: 02627-470135 electroandina@slatinos.com.ar"
 ?
 NORMA()
 GRAN_ON()
 ? "Presupuesto: "+vNano+"/"+vNpre
 GRAN_OFF()
 NORMA()
 ? "Cliente: "+vClie
 ?
 CHICO()
 ? "It Descripci¢n                                                        Unidad     Medida          Marca      Cantidad P.Unit.   P.Total  "
 ? "-- ------------------------------------------------------------------ ---------- --------------- ---------- -------- --------- ---------"
 *  1  123456789012345678901234567890123456789012345678901234567890123456 1234567890 123456789012345 1234567890 12345678 123456789
 *  1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789
 DO WHILE INKEY(.1)#27.AND. hor<=63 .AND. Pos<=LEN(MItems)
  IF !EMPTY(SUBSTR(MItems[Pos],143,5))
   SELECT ARTIC
   SET ORDER TO 6
   fClav=SUBSTR(MItems[Pos],139,4)+SUBSTR(MItems[Pos],143,5)
   SEEK fClav
   IF FOUND()
    fCpro=cpro
   ELSE
    fCpro=SPACE(15)
   ENDIF
   fArti = fCpro+" "+SUBSTR(MItems[Pos],1,50)
   fUnid = SUBSTR(MItems[Pos],102,10)
   fMedi = SUBSTR(MItems[Pos],106,15)
   fMrca = SUBSTR(MItems[Pos],124,10)
   rCant = SUBSTR(MItems[Pos],79,8)
   rPuni = SUBSTR(MItems[Pos],87,9)
   rDola = SUBSTR(MItems[Pos],151,8)
   xtotal= VAL(rCant) * VAL(rPuni) * VAL(rDola)

   ? CERO(Pos,2)+" "+fArti+" "+fUnid+" "+fMedi+" "+fMrca+" "+rCant+" "+rPuni+" "+STR(xTotal,9,2)
   vTot=vTot+xTotal
   hor++
  ENDIF
  Pos++
 ENDDO
 IF Pos>LEN(MItems)
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