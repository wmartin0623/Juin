*Listado Articulos por Marca
PUBLIC iva1,iva2,col_ant,ok
PRIVATE pan_ant,col_ant
pan_ant=SAVESCREEN(0,0,24,79)
col_ant=SETCOLOR()
PRIVATE vNtx
STORE "" TO vNtx

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
  vNtx=ARCH_EXI("BORRAR","NTX")
  INDEX ON mrca+prov+nomb TO &vNtx
  SET INDEX TO &vNTx
 ELSE
  QUIT
 ENDIF

PRIVATE pant_ant
PRIVATE vMrca,Nom_Mrca
pan_ant=SAVESCREEN(0,0,24,79)
vMrca=SPACE(3)
STORE SPACE(30) TO Nom_Mrca
DO WHILE .T.
 vtot=0
 SETCOLOR(color3)
 SOMBRA(5,22,08,57,'H1',1,'',color3)
 @6,25 SAY "Ingrese Marca: " GET vMrca PICTURE "@K 999" VALID VAL_MRCA(@vMrca,@Nom_Mrca)
 READ
 IF LASTKEY()=27
  EXIT
 ENDIF
 @7,24 SAY Nom_Mrca COLOR "W+/R"
 IF CARTEL("DATOS CORRECTOS  [ SI ]  [ NO ]",12,1,'N',colo3)
  IF LASTKEY()=27
   EXIT
  ENDIF
  SELECT ARTIC
  condicion="mrca=vMrca"
  SET FILTER TO mrca=vMrca.AND.!DELETE()
  GO TOP
  SEEK vMrca
  IF !FOUND()
   CARTEL("NO HAY ARTICULOS en "+LOWER(ALLTRIM(Nom_Mrca))+" [ OK ]",20,1,'N',color3)
   RESTSCREEN(0,0,24,79,pan_ant)
   LOOP
  ENDIF
  o1=0
  IF !IMPRI()
   o1=1
  ENDIF
  IF LASTKEY()#27
   IF o1=1
    DO Mrc_imp
   ELSE
    DO Mrc_pan
   ENDIF
  ENDIF
  RESTSCREEN(0,0,24,79,pan_ant)
 ELSE
  LOOP
 ENDIF
ENDDO
RESTSCREEN(0,0,24,79,pan_ant)
CLOSE ALL
SETCOLOR(col_ant)
RETURN

PROCEDURE Mrc_pan
SET CENTURY OFF
vProv=prov
bProv=vProv
DO WHILE mrca=vMrca .AND. INKEY(.1)#27
 *dibujar encabezado
 SETCOLOR(colo3)
 @1,0 CLEAR TO 24,79
 @3,0 TO 3,79 DOUBLE
 SETCOLOR(color3)
 @1,1 SAY CENTRA('ARTICULOS: Marca='+vMrca+' '+TRIM(Nom_Mrca),78)
 SETCOLOR(colo3)
 @2,0 SAY 'Descripci¢n del Articulo                                                        ' COLOR colo12
 @3,0 SAY ' Cod.Provee      Unidad     F.Venta    Medidas         Color      Calidad  Drza ' COLOR "R+/B"
 @4,0 SAY ' Usos            Referencia M2 x Caja  PU Cdo.         PU 30D¡as  PU 60D¡as Mda '
 @5,0 SAY '================ ========== ========== =============== ========== ========= ===='
 *         1234567890123456789012345678901234567890123456789012345678901234567890123456789
 *          12345cpro012345 123unid890 123fvta890 123medi89012345 123clor890 123cali8 drza5
 *          123usos890      123refe890 123m2cj890 123pucd89       123pucd89  123pucd89 123
 hor=6
 @hor,0
 DO WHILE mrca=vMrca.AND.INKEY(.1)#27.AND. hor<=21.AND.bProv=vProv
   @hor,1 SAY CENTRA("**** Lista de Precios: "+vProv+'  '+ALLTRIM(PROVE->nomb)+' ****',78) COLOR color3
   hor++
   DO WHILE mrca=vMrca.AND. INKEY(.1)#27 .AND. hor<=21 .AND. bProv=vProv
    IF hor+3<=22
     @hor,0 SAY nomb COLOR colo12
     hor++
     @hor,1 SAY SUBSTR(cpro+SPACE(15),1,15)+" ";
               +SUBSTR(unid+SPACE(10),1,10)+" ";
               +SUBSTR(fvta+SPACE(10),1,10)+" ";
               +SUBSTR(medi+SPACE(15),1,15)+" ";
               +SUBSTR(clor+SPACE(10),1,10)+" ";
               +SUBSTR(cali+SPACE(8) ,1, 8)+" ";
               +SUBSTR(drza+SPACE(5),1,5) COLOR "R+/B"
     hor++
     @hor,1 SAY SUBSTR(usos+SPACE(10),1,10)+SPACE(6);
               +SUBSTR(refe+SPACE(10),1,10)+" ";
               +SUBSTR(m2cj+SPACE(10),1,10)+" ";
               +STR(pucd,9,3)+SPACE(7);
               +STR(pucd*vPu30,9,3)+"  ";
               +STR(pucd*vPu60,9,3)+" ";
               +SUBSTR(mone+SPACE(3),1,3)
     hor++
     @hor,0 SAY REPLI("Ä",79)
     hor++
     SKIP
     vProv=prov
    ELSE
     hor=22
    ENDIF
   ENDDO
   IF vProv#bProv
    *IF hor+1<>23
    *  @hor,0 SAY REPLI("Í",79)
    *  hor=hor+1
    *ENDIF
    bProv=vProv
   ENDIF
 ENDDO
 IF hor>21 .AND. mrca=vMrca
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
  @23,20 SAY SPACE(40) COLOR colo3
  SET CURSOR ON
  IF tecla=27
   EXIT
  ENDIF
 ENDIF
ENDDO
SETCOLOR(color3)
@23,20 SAY " ±Esc±-Sale "
SETCOLOR(color7)
DIST_COLOR(23,22,'~W+/R~Esc')
tecla=65
SET CURSOR OFF
DO WHILE tecla#27
 tecla=INKEY(0)
 IF tecla=-9
  CALCULATOR()
 ENDIF
ENDDO
@23,20 SAY SPACE(40)
SET CURSOR ON
SET CENTURY ON
RETURN

PROCEDURE mrc_imp
SET CENTURY OFF
SET CONSOLE OFF
vProv=prov
bProv=vProv
pagina=0
?? CHR(27)+CHR(120)+CHR(0)+CHR(18)
?? CHR(27)+CHR(67)+CHR(72)
?? CHR(27)+CHR(33)+CHR(5)
DO WHILE mrca=vMrca.AND. INKEY(.1)#27
 *dibujar encabezado
 pagina=pagina+1
 NORMA()
 GRAN_ON()
 ? SUBSTR(nom_emp+SPACE(25),1,25)
 GRAN_OFF()
 NORMA()
 CHICO()
 ?? SPACE(23)+'Pagina Nro. : '+STR(pagina,2)
 NORMA()
 GRAN_ON()
 ? 'ARTICULOS: Marca='+vMrca+' '+TRIM(Nom_Mrca)
 GRAN_OFF()
 NORMA()
 CHICO()
 ?? SPACE(3)+'Fecha       : '+STUFF(DTOC(DATE()),3,4,'-'+MESES(DATE())+'-')
 NORMA()
 ? 'Descripci¢n del Articulo                                                         '
 ? ' Cod.Provee      Unidad     F.Venta    Medidas         Color      Calidad  Drza  '
 ? ' Usos            Referencia M2 x Caja  PU Cdo.         PU 30D¡as  PU 60D¡as Mda  '
 ? 'ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ÄÄÄÄÄÄÄÄÄÄ ÄÄÄÄÄÄÄÄÄÄ ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ ÄÄÄÄÄÄÄÄÄÄ ÄÄÄÄÄÄÄÄÄ ÄÄÄÄ '
 hor=6
 DO WHILE mrca=vMrca.AND.INKEY(.1)#27.AND. hor<=63.AND.bProv=vProv
   GRAN_ON()
   ? "****  Lista de Precios: "+vProv+'  '+PROVE->nomb
   GRAN_OFF()
   hor++
   DO WHILE mrca=vMrca.AND. INKEY(.1)#27 .AND. hor<=63 .AND. bProv=vProv
    IF hor+3<=64
     ? nomb
     hor++
     ? " "+SUBSTR(cpro+SPACE(15),1,15)+" ";
          +SUBSTR(unid+SPACE(10),1,10)+" ";
          +SUBSTR(fvta+SPACE(10),1,10)+" ";
          +SUBSTR(medi+SPACE(15),1,15)+" ";
          +SUBSTR(clor+SPACE(10),1,10)+" ";
          +SUBSTR(cali+SPACE(8) ,1, 8)+" ";
          +SUBSTR(drza+SPACE(5),1,5)
     hor++
     ? " "+SUBSTR(usos+SPACE(10),1,10)+SPACE(6);
          +SUBSTR(refe+SPACE(10),1,10)+" ";
          +SUBSTR(m2cj+SPACE(10),1,10)+" ";
          +STR(pucd,9,3)+SPACE(7);
          +STR(pucd*vPu30,9,3)+"  ";
          +STR(pucd*vPu60,9,3)+" ";
          +SUBSTR(mone+SPACE(3),1,3)
     hor++
     ? REPLI("Ä",79)
     hor++
     SKIP
     vProv=prov
    ELSE
     hor=64
    ENDIF

   ENDDO
   IF vProv#bProv
    bProv=vProv
   ENDIF
 ENDDO
 IF hor>63 .AND. mrca=vMrca
  EJECT
 ENDIF
ENDDO
EJECT
?? CHR(27)+CHR(33)+CHR(8)
SET CURSOR ON
SET CONSOLE ON
SET CENTURY ON
RETURN
