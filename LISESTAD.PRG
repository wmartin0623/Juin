******************************************************
* LISTADO DE ESTADISTICO DE VENTAS                   *
******************************************************
PUBLIC pan_an,co_an,dia,listar
PRIVATE bmes,bano
STORE 0 TO bmes,bano
dia=DATE()
pa_an=SAVESCREEN(0,0,24,79)
co_an=SETCOLOR()
SOMBRA(10,15,14,50,'H2',1,'',color3)
DO WHILE .T.
 SETCOLOR(color3)
 @09,16 SAY CENTRA("ESTADISTICA DE VENTAS",34)
 @11,17 SAY 'Indique MES   (mm) : ' GET bmes PICTURE '@K 99' VALID bmes>=1 .AND. bmes<=12
 @13,17 SAY 'Indique A¤O (aaaa) : ' GET bano PICTURE '@K 9999' VALID bano>=1994 .AND. bano<=2100
 READ
 IF LASTKEY()=27
  CLOSE ALL
  RESTSCREEN(0,0,24,79,pa_an)
  SETCOLOR(co_an)
  RETURN
 ENDIF
 vper=CHR(bmes+64)+CERO(bano,4)
 IF .NOT.FILE("FCD&vper..DBF")
  CARTEL("NO ENCUENTRO VENTAS DE ESTE MES  [ OK ]",18,1,'N',color3)
  LOOP
 ENDIF
 EXIT
ENDDO
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

*DETALLES
AREA()
IF USAR("FCD&vper","DETALLE")
  INDEX ON arti TO FCDesta
  SET INDEX TO FCDesta
ELSE
 QUIT
ENDIF
IF  CARTEL('CUANTOS PRODUCTOS : [ UNO ]  [ TODOS ] ',22,1,'N',colo3)
 IF LASTKEY()=27
  CLOSE ALL
  RESTSCREEN(0,0,24,79,pa_an)
  SETCOLOR(co_an)
  RETURN
 ENDIF
 varti=SPACE(5)
 vnom=""
 DO WHILE .T.
  SOMBRA(8,10,12,50,'H1',1,'',color3)
  @10,11 SAY"Que Articulo : " GET varti PICTURE '@K 99999' VALID VER_ART1(@varti,@vnom)
  READ
  IF LASTKEY()=27
   CLOSE ALL
   RESTSCREEN(0,0,24,79,pa_an)
   SETCOLOR(co_an)
   RETURN
  ENDIF
  IF CARTEL(vnom+" CORRECTO  [ SI ]  [ NO ]",18,1,'N',colo3)
   IF LASTKEY()=27
    LOOP
   ENDIF
   EXIT
  ELSE
   LOOP
  ENDIF
 ENDDO
 listar=.T.
ELSE
 listar=.F.
ENDIF
IF FILE("ESTADIS.DBF")
 AREA()
 USAR("ESTADIS","ESTA","E")
 ZAP
ELSE
 QUIT
ENDIF
IF FILE("AUXI01.DBF")
 DELETE FILE AUXI01.DBF
ENDIF
SELECT DETALLE
GO TOP
DO WHILE .NOT.EOF()
 IF !baja
  IF listar
   IF arti=varti
    graba=.T.
   ELSE
    graba=.F.
   ENDIF
  ELSE
   graba=.T.
  ENDIF
 ELSE
  graba=.F.
 ENDIF
 IF graba
  vart=arti
  vNomb = nomb
  vcan=cant
  vtot=ptot
  vdes=0  &&desc
  SELECT ESTA
  GO BOTTOM
  ADDREC(0)
  RECLOCK(0)
  REPLACE arti WITH vart
  REPLACE nomb WITH vNomb
  REPLACE cant WITH vcan
  REPLACE ptot WITH vtot
  REPLACE desc WITH vdes
  UNLOCK
 ENDIF
 SELECT DETALLE
 SKIP
ENDDO
SELECT ESTA
IF RECCOUNT()<>0
 SET EXCLUSIVE ON
 INDEX ON arti TO BORRAR
 *REPLACE ALL ptot WITH (ptot-((ptot*desc)/100))
 SET EXCLUSIVE OFF
 GO TOP
ELSE
 CARTEL("LO LAMENTO NO ENCUENTRO ARTICULOS  [ OK ]",18,1,'N',color3)
 CLOSE ALL
 RESTSCREEN(0,0,24,79,pa_an)
 SETCOLOR(co_an)
 RETURN
ENDIF
TOTAL ON arti TO AUXI01 FIELDS cant,ptot
IF USAR("AUXI01.DBF","AUXI")
 GO TOP
ENDIF
*SET RELATION TO arti INTO ARTIC
*MODULO DE LISTADOS
ko=0
IF .NOT.IMPRI()
 @1,0 CLEAR TO 24,79
 @12,15 SAY 'ESPERE..... ESTOY IMPRIMIENDO'
 SET CONSOLE OFF
 SET PRINT ON
 ko=1
ENDIF
IF LASTKEY()#27
 STORE 0 TO pagina,t1
 IF ko=1
  DO esta_imp
 ELSE
  DO esta_pan
 ENDIF
ENDIF
CLOSE ALL
DELETE FILE borrar.ntx
DELETE FILE FCDesta.ntx
IF FILE("AUXI01.DBF")
 DELETE FILE AUXI01.DBF
ENDIF
RESTSCREEN(0,0,24,79,pa_an)
SETCOLOR(co_an)
RETURN

PROCEDURE esta_imp
pagina=0
?? chr(27)+chr(120)+chr(0)+chr(18)
?? chr(27)+chr(67)+chr(72)
?? CHR(27)+CHR(33)+CHR(5)
DO WHILE !EOF() .AND. INKEY(.1)#27
 pagina=pagina+1
 NORMA()
 GRAN_ON()
 ? SUBSTR(nom_emp+SPACE(25),1,25)
 GRAN_OFF()
 NORMA()
 CHICO()
 ?? SPACE(23)+'Pagina Nro. : '+STR(pagina,3)
 NORMA()
 GRAN_ON()
 ? SUBSTR('LISTADO ESTADISTICO DE VENTAS'+SPACE(31),1,31)
 GRAN_OFF()
 NORMA()
 CHICO()
 ??SPACE(3)+'Fecha       : '+STUFF(DTOC(DATE()),3,4,'-'+MESES(DATE())+'-')
 ?
 NORMA()
 ? SPACE(10)+'MES  : '+CERO(bmes,2)+'  A¥O  : '+CERO(bano,4)
 ?'====== ======================================== ========== =Importe===='
 ?'CODIGO  DESCRIPCION                               CANTIDAD  Neto Grav. '
 ?'====== ======================================== ========== ============'
 hor=7
 DO WHILE .NOT.EOF() .AND. hor<=66 .AND. INKEY(.1) # 27
  gram=SUBSTR(nomb,1,40)
  ? ' '+arti+' '+gram+' '+STR(cant,10,2)+' '+STR(ptot,12,2)
  t1=t1+ptot
  hor=hor+1
  SKIP
 ENDDO
 IF LASTKEY()=27
  EXIT
 ENDIF
 IF .NOT.EOF()
  IF ko=1 .AND. t1<>0
   EJECT
  ENDIF
 ENDIF
ENDDO
IF t1<>0
 ?'*** Total ***'
 ? SPACE(57)+STR(t1,12,2)
 IF ko=1
  EJECT
 ENDIF
ELSE
 CARTEL('LO SIENTO, NO HAY VENTAS EN ESTE MES  [ OK ]',18,1,'N',color3)
ENDIF
?? CHR(27)+CHR(33)+CHR(8)
RETURN

PROCEDURE esta_pan
DO WHILE .NOT.EOF()
 SETCOLOR(color7)
 @1,0 CLEAR TO 24,79
 SET FORMAT TO lis_esta
 READ
 SET FORMAT TO
 SETCOLOR(color3)
 @1,1 SAY CENTRA('ESTADISTICA DE VENTAS MES  : '+CERO(bmes,2)+'  A¥O  : '+CERO(bano,4),78)
 SETCOLOR(color1)
 hor=5
 DO WHILE .NOT.EOF() .AND. hor<=21 .AND. INKEY(.1) # 27
  gram=SUBSTR(nomb,1,40)
  @hor,2 SAY arti
  @hor,10 SAY gram
  @hor,53 SAY cant PICTURE "@Z 9999999.99"
  @hor,66 SAY ptot PICTURE "@Z 999999999.99"
  t1=t1+ptot
  SETCOLOR(color3)
  @23,66 SAY t1 PICTURE "@Z 999999999.99"
  SETCOLOR(color1)
  hor=hor+1
  SKIP
 ENDDO
 IF LASTKEY()=27
  EXIT
 ENDIF
 IF .NOT.EOF()
  SETCOLOR(color3)
  @23,20 SAY " ±Esc±-Sale  ±ÄÙ±-Continua "
  SETCOLOR(color7)
  DIST_COLOR(23,22,'~W+/R~Esc')
  DIST_COLOR(23,34,'~W+/R~ÄÙ')
  tecla=65
  SET CURSOR OFF
  DO WHILE tecla#27 .AND. tecla#13
   tecla=INKEY(0)
   IF tecla=-9
    CALCULATOR()
   ENDIF
  ENDDO
  @23,20 SAY SPACE(40)
  SET CURSOR ON
  IF tecla=27
   EXIT
  ENDIF
 ENDIF
ENDDO
@23,50 SAY'*** Total ***'
SETCOLOR(color3)
@23,66 SAY t1 PICTURE "@Z 999999999.99"
@23,20 SAY " ±Esc±-Sale "
SETCOLOR(color7)
DIST_COLOR(23,22,'~W+/R~Esc')
tecla = 65
SET CURSOR OFF
DO WHILE tecla # 27
 tecla=INKEY(0)
 IF tecla=-9
  CALCULATOR()
 ENDIF
ENDDO
@23,20 SAY SPACE(40)
SET CURSOR ON
RETURN
