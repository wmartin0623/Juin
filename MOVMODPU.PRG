*Modificacion de Lista de Precios
PRIVATE Col_Ant,Pan_Ant
Col_Ant=SETCOLOR()
Pan_Ant=SAVESCREEN(,,,,)
*Abrir Archivos
*Rubros
OPEN_DBF("MAERUBRO","RUBRO","C","MAERUBRO","rubr","NOMRUBRO","nomb")

*Lista de Precios
AREA()
OPEN_DBF("MAEPROVE","PROVE","C","MAEPROVE","prov","NOMPROVE","nomb")

*marcas
AREA()
OPEN_DBF("MAEMARCA","MARCA","C","MAEMARCA","mrca","NOMMARCA","nomb")

*Articulos
AREA()
IF USAR('MAEARTIC','ARTIC')
 SET RELATION TO prov INTO PROVE, TO rubr INTO RUBRO, TO mrca INTO MARCA
 *Por Rubro,Lista de Precios y Articulo
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
  SET INDEX TO MAEARTIC,MAEARTI1,MAEARTI2,MAEARTI3,MAEARTI4,MAEARTI5,NOMARTIC,;
               MAEARTI6
ELSE
 QUIT
ENDIF

*Variables a Utilizar
PRIVATE vRubrD,Nom_RubrD,vRubrH,Nom_RubrH,bRubr
PRIVATE vProvD,Nom_ProvD,vProvH,Nom_ProvH,bProv
PRIVATE vMrcaD,Nom_MrcaD,vMrcaH,Nom_MrcaH,bMrca
PRIVATE vPorce,vMonto,vSuma,vResta,vCte,vCdo,bPorce,bMonto
PRIVATE vMetodo
STORE SPACE( 4) TO vRubrD,vRubrH,vProvD,vProvH
STORE SPACE(20) TO Nom_RubrD,Nom_RubrH
STORE SPACE(30) TO Nom_ProvD,Nom_ProvH
STORE SPACE( 3) TO vMrcaD,vMrcaH
STORE SPACE(10) TO Nom_MrcaD,Nom_MrcaH
STORE 0 TO vPorce,vMonto
STORE .F. TO vSuma,vResta,vCte,vCdo,bRubr,bProv,bMrca,bPorce,bMonto

SET DECIMAL TO 2
DO WHILE .T.
 SETCOLOR(colo3)
 DO PAN_PUNI
 tecla=65
 DIST_COLOR(01,8,'~W+/R~**** MODIFICACIЮN PRECIOS UNITARIOS ****')
 SET CURSOR OFF
 DO WHILE tecla#13 .AND. tecla#27
  tecla=INKEY(0)
 ENDDO
 IF tecla=27
  EXIT
 ENDIF
 @20,20 SAY SPACE(40)
 SET CURSOR ON
 XPC=9
 XC=1
 SET KEY -4 TO va_tras
 SETCOLOR(colo3)
 STORE .F. TO bRubr,bProv,bMrca,bPorce,bMonto
 vSuma=.T.
 vMetodo=.T.  &&Masiva por defecto
 DO WHILE XC<=XPC
  XD=1
  SET CURSOR ON
  DO CASE
   CASE XC=1
    SELECT PROVE
    SET FILTE TO
    GO TOP

    @3,11 GET vRubrD PICTURE '@!' VALID VAL_RUBR1(@vRubrD,@Nom_RubrD)
    READ
    bRubr=.T.
    vRubrH=vRubrD
    Nom_RubrH=Nom_RubrD
    @3,16 SAY Nom_RubrD COLOR colo12
    @4,11 SAY vRubrH+" "+Nom_RubrH COLOR colo12
    @20,3 SAY SPACE(55)

   CASE XC=2
    *Controlar que hay listas para el rubro elegido
    SELECT PROVE
    GO TOP
    vCount:=0
    bBloq1:={|| vCount++}
    bCondi1:={|| vCount=0.AND.!DELETE().AND.rubr=vRubrD }
    bWhile1:={|| vCount=0}
    DBEVAL(bBloq1,bCondi1,bWhile1)
    IF vCount=0
     CARTEL("NO HAY LISTAS PAR EL RUBRO SOLICITADO [ OK ]",12,1,'N',color3)
     XC=1
     LOOP
    ENDIF
    *Filtrar las listas para el rubro elegido
    SET FILTER TO rubr=vRubrD
    GO TOP
    @20,3 SAY "**** Campo en 'Blanco' para Todas las Listas de Precios ****" COLOR color3
    @3,39 GET vProvD PICTURE '@K 9999' VALID EMPTY(vProvD).OR.VAL_PROV(@vProvD,@Nom_ProvD)
    READ
    IF EMPTY(vProvD)
     Nom_ProvD="*** Todas ***"
     bProv=.F.
    ELSE
     bProv=.T.
    ENDIF
    @3,44 SAY SUBSTR(Nom_ProvD,1,25) COLOR colo12
    @20,3 SAY SPACE(55)

   CASE XC=3
    IF !EMPTY(vProvD)
     @4,39 GET vProvH PICTURE '@K 9999' VALID VAL_PROV(@vProvH,@Nom_ProvH)
     READ
     @4,44 SAY SUBSTR(Nom_ProvH,1,25) COLOR colo12
     IF vProvH < vProvD
      vTemp=vProvD
      vProdD=vProvH
      vProvH=vTemp
      vTemp=Nom_PRovD
      Nom_ProvD=Nom_ProvH
      Nom_ProvH=vTemp
      @3,39 SAY vProvD+' '+SUBSTR(Nom_provD,1,25)
      @4,39 SAY vProvH+' '+SUBSTR(Nom_provH,1,25)
     ENDIF
    ENDIF

   CASE XC=4
    *Controlar que hay Marcas para el rubro elegido
    SELECT MARCA
    GO TOP
    vCount:=0
    bBloq1:={|| vCount++}
    bCondi1:={|| vCount=0.AND.!DELETE().AND.rubr=vRubrD }
    bWhile1:={|| vCount=0}
    DBEVAL(bBloq1,bCondi1,bWhile1)
    IF vCount=0
     CARTEL("NO HAY MARCAS PAR EL RUBRO SOLICITADO [ OK ]",12,1,'N',color3)
     XC=1
     LOOP
    ENDIF
    *Filtrar las Marcas para el rubro elegido
    SELECT MARCA
    SET FILTE TO rubr=vRubrD
    GO TOP
    @7,11 GET vMrcaD PICTURE '@K 999' VALID VAL_MRCA1(@vMrcaD,@Nom_MrcaD)
    READ
    bMrca=.T.
    @7,15 SAY SUBSTR(Nom_MrcaD,1,7) COLOR colo12
    @20,3 SAY SPACE(55)

   CASE XC=5
    IF !EMPTY(vMrcaD)
     @8,11 GET vMrcaH PICTURE '@K 999' VALID VAL_MRCA1(@vMrcaH,@Nom_MrcaH)
     READ
     @8,15 SAY SUBSTR(Nom_MrcaH,1,7) COLOR colo12
     IF vMrcaH < vMrcaD
      vTemp=vMrcaD
      vMrcaD=vMrcaH
      vMrcaH=vTemp
      vTemp=Nom_MrcaD
      Nom_MrcaD=Nom_MrcaH
      Nom_MrcaH=vTemp
      @7,11 SAY vMrcaD+' '+SUBSTR(Nom_MrcaD,1,7)
      @8,11 SAY vMrcaH+' '+SUBSTR(Nom_MrcaH,1,7)
     ENDIF
    ENDIF

   CASE XC=6
    *Porcentaje Variaci╒n
    @7,47 GET vPorce PICTURE "@K 99.99" VALID vPorce >= 0
    READ
    @7,47 SAY vPorce PICTURE "@Z 99.99" COLOR colo12

   CASE XC=7
    *Monto Fijo Variaci╒n
    IF vPorce=0
     @8,43 GET vMonto PICTURE "@K 999999.99" VALID vMonto # 0
     READ
     bMonto=.T.
     bPorce=.F.
    ELSE
     bMonto=.F.
     bPorce=.T.
    ENDIF
    @8,43 SAY vMonto PICTURE "@Z 999999.99" COLOR colo12

   CASE XC=8
    *Sumando
    @20,3 SAY "╠T, t, S, s╠ = SI     ╠F, f, N, n╠ = NO" COLOR color3
    @7,70 GET vSuma PICTURE "L"
    READ
    vResta=!vSuma
    @7,72 SAY IIF(vSuma ,"Si","No") COLOR colo12
    @8,72 SAY IIF(vResta,"Si","No") COLOR colo12
    vCdo=.T.

   CASE XC=9
    *Metodo de Aplicacion
    @20,3 SAY "╠T, t, S, s╠ = SI     ╠F, f, N, n╠ = NO" COLOR color3
    @11,12 GET vMetodo PICTURE "L"
    READ
    @11,12 SAY IIF(vMetodo ,"Si","No") COLOR colo12
    @11,32 SAY IIF(!vMetodo,"Si","No") COLOR colo12
    vCdo=.T.

  ENDCASE
  IF LASTKEY()=27
   CLEAR GETS
   XC=10
  ELSE
   XC=XC+XD
  ENDIF
 ENDDO
 IF LASTKEY()#27
  IF CARTEL("DATOS CORRECTOS  [ SI ]  [ NO ]",20,1,'N',color3)
   IF LASTKEY()#27
    *Gravar datos
    IF !vMetodo
     *Individual
     clave="rubr=vRubrD.AND.mrca>=vMrcaD.AND.mrca<=vMrcaH"
     IF bProv
      clave=clave+".AND.prov>=vProvD.AND.prov<=vProvH"
     ENDIF
     *Llenar Matriz
     campos:={}
     SELECT ARTIC
     SET ORDER TO 2
     SET FILTE TO &clave
     GO TOP
     SEEK vRubrD
     IF FOUND()
      pp=0
      DO WHILE .NOT.EOF().AND.rubr=vRubrD
       okis=.T.
       *    okis=.F.
       *    IF mrca>=vMrcaD.AND.mrca<=vMrcaH
       *     IF bProv
       *      IF prov>=vProvD.AND.prov<vProvH
       *       okis=.T.
       *      ELSE
       *       okis=.F.
       *      ENDIF
       *     ELSE
       *      okis=.T.
       *     ENDIF
       *    ELSE
       *     okis=.F.
       *    ENDIF
       *     *Si el articulo se encuadra dentro del rango lo cargo en la tabla
       IF okis
        AADD(campos," Ё"+SUBSTR(nomb,1,75)+mrca+MARCA->nomb+medi+cali+m2cj+cpro+STR(pucd,9,3)+prov+arti)
        pp=pp+1
        SKIP
       ELSE
        SKIP
       ENDIF
      ENDDO
     ENDIF
     IF pp=0
      CARTEL("NO HAY ARTICULOS EN EL RANGO [ OK ]",12,1,'N',color3)
      LOOP
     ENDIF

     DO MODPU
     SET CURSOR OFF
     com = 1
     rel = 0
     KEYBOARD CHR(24)+CHR(5)
     DO WHILE .T.
      com = ACHOICE(15,1,18,77,campos,.T.,"sel_art",com,rel)
      IF com = 0
       EXIT
      ENDIF
     ENDDO
     inkey(.1)
     IF LASTKEY() = 27 .OR. com=999
      LOOP
     ENDIF
     SET KEY -4 TO
     SET CURSOR ON
     *Datos correctos
     IF CARTEL("DATOS CORRECTOS PARA GUARDAR [ SI ]  [ NO ]",20,1,'N',color3)
      IF LASTKEY()#27
       SELECT ARTIC
       SET ORDER TO 6
       FOR con=1 TO LEN(campos)
        IF SUBSTR(campos[con],1,1)='╠'
         vMrca=SUBSTR(campos[con],78,3)
         vArti=SUBSTR(campos[con],157,5)
         vImpo=VAL(SUBSTR(campos[con],144,9))
         *Calcula el Punit del Articulo aplicando la variaci╒n
         IF vSuma
          *Si Suma
          IF bPorce
           *Porcentaje
           vImpo=vImpo+((vImpo*vPorce)/100)
          ELSE
           *Monto
           vImpo=vImpo+vMonto
          ENDIF
         ELSE
          *Resta
          IF bPorce
           *Porcentaje
           vImpo=vImpo-((vImpo*vPorce)/100)
          ELSE
           *Monto
           vImpo=vImpo-vMonto
          ENDIF
         ENDIF
         *Buscar Articulo
         SEEK vRubrD + vArti
         IF FOUND()
          RECLOCK(1)
          REPLACE pucd WITH vImpo
          REPLACE fmod WITH DATE()
          UNLOCK
         ENDIF
        ENDIF
       NEXT
      ELSE
       LOOP
      ENDIF
     ELSE
      LOOP
     ENDIF

    ELSE

     *Masivo
     SELECT ARTIC
     GO TOP
       IF vCdo

        *Solamente Contado
        IF vSuma
         *Sumando
         IF bPorce
          *Porcentaje
          bloq1 := {|| _FIELD->pucd:=_FIELD->pucd+((_FIELD->pucd*vPorce)/100),;
                       _FIELD->fmod:=DATE();
                   }
         ELSE
          *Monto
          bloq1 := {|| _FIELD->pucd:=_FIELD->pucd+vMonto,;
                       _FIELD->fmod:=DATE();
                   }
         ENDIF
        ELSE
         *Restando
         IF bPorce
          *Porcentaje
          bloq1 := {|| _FIELD->pucd:=_FIELD->pucd-((_FIELD->pucd*vPorce)/100),;
                       _FIELD->fmod:=DATE();
                   }
         ELSE
          *Monto
          bloq1 := {|| _FIELD->pucd:=_FIELD->pucd-vMonto,;
                       _FIELD->fmod:=DATE();
                   }
         ENDIF
        ENDIF
       ENDIF

     *Condicion
     bcondi1 := {|| IIF(bRubr,rubr=vRubrD,.T.).AND.;
                    IIF(bProv,prov>=vProvD.AND.prov<=vProvH,.T.).AND.;
                    IIF(bMrca,mrca>=vMrcaD.AND.mrca<=vMrcaH,.T.);
                }
     IF FLOCK()
      DBEVAL(bloq1,bcondi1)
      UNLOCK
      olo=1
     ELSE
      CARTEL("NO PUEDO ABRIR, Otra Terminal esta Usando el Archivo [ OK ]",12,1,'N',color3)
     ENDIF
    ENDIF

   ENDIF
   DBCOMMITALL()
  ENDIF
 ENDIF
ENDDO
CLOSE ALL
RESTSCREEN(,,,,Pan_Ant)
SETCOLOR(Col_Ant)
RETURN

FUNCTION sel_art
PARAMETERS MOD,sub,pos
PRIVATE tcl,fil,pus,oriy,con,ydes
FIL=ROW()
IF LEN(campos) < 4
 ydes = LEN(campos)
ELSE
 ydes = 4
ENDIF
IF ydes = 1
 @15,0 SAY ''
ELSE
 pus = 4/LEN(campos)*sub
 FOR oriy = 1 TO 4
    @14+oriy,0 SAY IF(pus <= oriy .and. pus >= oriy-.99999,IIF(sub=1,'',IIF(sub=LEN(campos),'','')),'Ё')
 NEXT oriy
ENDIF
tcl = LASTKEY()
*Imprime detalles del Articulo
@21,1 SAY SUBSTR(campos[sub],81,15)
@21,17 SAY SUBSTR(campos[sub],96,15)
@21,33 SAY SUBSTR(campos[sub],111,8)
@21,42 SAY SUBSTR(campos[sub],119,10)
@21,53 SAY SUBSTR(campos[sub],129,15)
@21,69 SAY SUBSTR(campos[sub],144,9)
fImpo=VAL(SUBSTR(campos[sub],144,9))
*Calcula el Punit del Articulo aplicando la variaci╒n
IF vSuma
 *Si Suma
 IF bPorce
  *Porcentaje
  fImpo=fImpo+((fImpo*vPorce)/100)
 ELSE
  *Monto
  fImpo=fImpo+vMonto
 ENDIF
ELSE
 *Resta
 IF bPorce
  *Porcentaje
  fImpo=fImpo-((fImpo*vPorce)/100)
 ELSE
  *Monto
  fImpo=fImpo-vMonto
 ENDIF
ENDIF
*Imprime Monto
@22,57 SAY "PUni Nuevo: "+STR(fImpo,9,3) COLOR colo12
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
   IF SUBSTR(campos[sub],1,1) # '╠'
    campos[sub] = STUFF(campos[sub],1,1,'╠')
   ELSE
    campos[sub] = STUFF(campos[sub],1,1,' ')
   ENDIF
   KEYBOARD CHR(24)
   rel = pos
   RETURN(1)
 CASE tcl = 42
  vMarcas = 0
  FOR CON = 1 TO LEN(campos)
   IF SUBSTR(campos[con],1,1) = '╠'
    vMarcas++
   ENDIF
  NEXT
  IF vMarcas = 0
   CARTEL("DEBE MARCAR ALGUN ARTICULO  [ OK ]",18,1,'N',color3)
   RETURN(1)
  ELSE
   RETURN(0)
  ENDIF
 OTHERWISE
  RETURN(2)
 ENDCASE
ENDCASE

*Control de Rubros
FUNCTION VAL_RUBR1
PARAMETERS fRubr,Nombre
LOCAL colo_a,antsel
IF LASTKEY()=27
 KEYBOARD CHR(27)
 RETURN .T.
ENDIF
antsel=ALLTRIM(STR(SELECT()))
colo_a=SETCOLOR()
SELECT RUBRO
SET ORDER TO 1
fRubr=PADL(fRubr,4,' ')
SEEK fRubr
IF .NOT. FOUND()
 *Ayuda de Rubros
 archivo='RUBRO'
 campi='rubr~nomb'
 ntx1=1
 ntx2=2
 l1=4
 l2=20
 camp1='rubr'
 IF AYUDO(campi,ntx1,ntx2,l1,l2,camp1,archivo) = .T.
  fRubr=rubr
  Nombre=nomb
  SELECT &antsel
  RETURN .T.
 ELSE
  SELECT &antsel
  RETURN .F.
 ENDIF
ELSE
 Nombre=nomb
 SELECT &antsel
 RETURN .T.
ENDIF
RETURN .T.

*Control de Lista de Precios
FUNCTION VAL_PROV
PARAMETERS fProv,Nombre
LOCAL colo_a,antsel
IF LASTKEY()=27
 KEYBOARD CHR(27)
 RETURN .T.
ENDIF
antsel=ALLTRIM(STR(SELECT()))
colo_a=SETCOLOR()
SELECT PROVE
SET ORDER TO 1
fProv=CERO(VAL(fprov),4)
SEEK fProv
IF .NOT. FOUND()
 *Ayuda de Lista de Precios
 archivo='PROVE'
 campi='prov~nomb'
 ntx1=1
 ntx2=2
 l1=4
 l2=30
 camp1='prov'
 IF AYUDO(campi,ntx1,ntx2,l1,l2,camp1,archivo) = .T.
  fProv=prov
  Nombre=nomb
  SELECT &antsel
  RETURN .T.
 ELSE
  SELECT &antsel
  RETURN .F.
 ENDIF
ELSE
 Nombre=nomb
 SELECT &antsel
 RETURN .T.
ENDIF
RETURN .T.

*Control de Marcas
FUNCTION VAL_MRCA1
PARAMETERS fMrca,Nombre
LOCAL colo_a,antsel
IF LASTKEY()=27
 KEYBOARD CHR(27)
 RETURN .T.
ENDIF
antsel=ALLTRIM(STR(SELECT()))
colo_a=SETCOLOR()
SELECT MARCA
SET ORDER TO 1
fMrca=CERO(VAL(fMrca),3)
SEEK fMrca
IF .NOT. FOUND()
 *Ayuda de Mrcaeedores
 archivo='MARCA'
 campi='Mrca~nomb'
 ntx1=1
 ntx2=2
 l1=3
 l2=10
 camp1='Mrca'
 IF AYUDO(campi,ntx1,ntx2,l1,l2,camp1,archivo) = .T.
  fMrca=Mrca
  Nombre=nomb
  SELECT &antsel
  RETURN .T.
 ELSE
  SELECT &antsel
  RETURN .F.
 ENDIF
ELSE
 Nombre=nomb
 SELECT &antsel
 RETURN .T.
ENDIF
RETURN .T.

PROCEDURE PAN_PUNI
SOMBRA(2,0,22,78,'H5',1,'',colo3)
@ 2,3 SAY "зRubro a Tratarдддддддддддддддддд©зRango Lista de Preciosддддддддддддддд©"
@ 3,3 SAY "ЁDesde:                          ЁЁ                                     Ё"
@ 4,3 SAY "ЁHasta:                          ЁЁ                                     Ё"
@ 5,3 SAY "юддддддддддддддддддддддддддддддддыюддддддддддддддддддддддддддддддддддддды"
@ 6,3 SAY "зRango Marcasдддддддд©    зVariaci╒n P.Unitarioддд©    зAplicarддддддддд©"
@ 7,3 SAY "ЁDesde:              Ё    Ё Porcentaje:           Ё    Ё Sumando :      Ё"
@ 8,3 SAY "ЁHasta:              Ё    Ё Monto Fijo:           Ё    Ё Restando:      Ё"
@ 9,3 SAY "юдддддддддддддддддддды    юддддддддддддддддддддддды    юдддддддддддддддды"
@10,3 SAY "зAplicar a P.Unitario de forma:дддддддд©                                 "
@11,3 SAY "ЁMasiva:          Individual:          Ё                                 "
@12,3 SAY "юдддддддддддддддддддддддддддддддддддддды                                 "
SETCOLOR(color3)
@20,20 SAY " ╠Esc╠-Sale  ╠ды╠-Ingresa "
SETCOLOR(colo3)
DIST_COLOR(20,22,'~W+/R~Esc')
DIST_COLOR(20,34,'~W+/R~ды')
RETURN

PROCEDURE MODPU
* modpu.FMT
SOMBRA(13,0,22,77,'H5',1,'',colo3)
@ 13,0 SAY "зMбDetalle Art║culoддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©"
@ 14,0 SAY "цcеддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢"
@ 15,0 SAY "Ё Ё                                                                           Ё"
@ 16,0 SAY "Ё Ё                                                                           Ё"
@ 17,0 SAY "Ё Ё                                                                           Ё"
@ 18,0 SAY "Ё Ё                                                                           Ё"
@ 19,0 SAY "юдаддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды"
@ 20,0 SAY "зMarcaддддддддддбMedidasддддддддбCalidadдбM2 x CajaдбC╒d.ProveedorддбP.Unit.дд©"
@ 21,0 SAY "Ё               Ё               Ё        Ё          Ё               Ё         Ё"
@ 22,0 SAY "юдддддддддддддддадддддддддддддддаддддддддаддддддддддадддддддддддддддаддддддддды"
SETCOLOR(color3)
@ 23,0 SAY "╠*╠-Guardar Nuevos Precios  ╠ды╠-Marcar/Desmarcar  ╠Esc╠-Salir Sin Guardar"
DIST_COLOR(23,1,'~W+/R~*')
DIST_COLOR(23,29,'~W+/R~ды')
DIST_COLOR(23,53,'~W+/R~Esc')
SETCOLOR(colo3)
RETURN
