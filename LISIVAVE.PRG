*Listado del LIBRO IVA VENTAS
PARAMETERS p6
*p6=1 Lista IVA con el IVA por defecto
*p6=2 Lista el IVA con el %iva Indicado y el Titulo que elija el Operador
*p6=3 Lista Todos los Importes de Iva Usados
PRIVATE pant_ant,col_ant,bmes,bano,vIvaI,bIvaI,vTitul,Tecla:=65
PRIVATE vSrie
PRIVATE bList  &&Bandera Ya listado, si algun dia ya fue listado, habilito
               &&Solamente el listado por pantalla
pant_ant=SAVESCREEN(0,0,24,79)
col_ant=SETCOLOR()
bmes=SPACE(2)
bano=SPACE(4)
*Porcentaje de iva
AREA()
IF USAR('PORCEIVA','PIVAS')
 GO TOP
 vIvaI=IVAI
ELSE
 QUIT
ENDIF
CLOSE
DO WHILE .T.
 *Solicitud de MES Y A¤o del Libro de IVA
 CARTEL('MES A TRATAR   <mm> : [  ]',9,1,'E',colo3)
 CARTEL('A¥0 A TRATAR <aaaa> : [    ]',14,1,'E',colo3)
 SETCOLOR(color3)
 @8,32 SAY "I.V.A.  VENTAS"
 SETCOLOR(colo3)
 @10,51 GET bmes PICTURE '@K 99' VALID VAL(bmes)>=1 .AND. VAL(bmes)<=12
 @15,50 GET bano PICTURE '@K 9999' VALID VAL(bano)>=1994 .AND. VAL(bano)<=2021
 READ
 IF LASTKEY()=27
  RESTSCREEN(0,0,24,79,pant_ant)
  SETCOLOR(col_ant)
  EXIT
 ENDIF
 xbmes=CHR(VAL(bmes)+64)
 bano=CERO(VAL(bano),4)
 vper=xbmes+bano
 IF .NOT.FILE("FCC&vper..DBF")
  CARTEL("PERIODO SIN REGISTRACION DE VENTAS  [ OK ]",18,1,'N',color3)
  LOOP
 ENDIF
 *clientes
 AREA()
 OPEN_DBF("MAECLIEN","CLIENTES","C","MAECLIEN","clie","NOMCLIEN","nomb")

 *Provincia
 AREA()
 OPEN_DBF("MAEPROVI","PCIA","C","MAEPROVI","pcia")

 *Archivo Consumidores Finales
 AREA()
 OPEN_DBF("MAECOFIN","CONS_FIN","C","MAECOFIN","tcom+ltra+srie+nume")

 *FACTURAS
 AREA()
 IF USAR("FCC&vper","FACTURA","E")
  INDEX ON DTOS(fech)+LTRA TO BORRARE
  SET INDEX TO BORRARE
  SET RELATION TO clie INTO CLIENTES, TO tcom+ltra+srie+nume INTO CONS_FIN
 ELSE
  QUIT
 ENDIF
 *Abrir totalizador de Libro de IVA
 AREA()
 IF !FILE("LIV&vper..DBF")
  IF USAR("LIBROIVA","LIBRO")
   COPY TO LIV&vper STRU
  ENDIF
 ENDIF
 IF !USAR("LIV&vper","LIBRO")
  CARTEL("NO PUEDO ABRIR TOTALIZADOR LIBRO DE IVA [ OK ]",12,2,'N',color3)
  LOOP
 ENDIF
 SELECT LIBRO
 IF !VACIA()
  GO BOTTOM
  DECLARE Sal_Ant[11]
  Sal_Ant[1]=SPACE(10)
  Sal_Ant[2]=SPACE(2)
  Sal_Ant[3]=SPACE(13)
  Sal_Ant[4]="Saldo Anterior:"
  Sal_Ant[5]=SPACE(11)
  Sal_Ant[6]=ngra
  Sal_Ant[7]=iexe
  Sal_Ant[8]=irin
  Sal_Ant[9]=irni
  Sal_Ant[10]=itot
  Sal_Ant[11]=SPACE(4)
 ELSE
  GO BOTTOM
  DECLARE Sal_Ant[11]
  Sal_Ant[1]=SPACE(10)
  Sal_Ant[2]=SPACE(2)
  Sal_Ant[3]=SPACE(13)
  Sal_Ant[4]="Saldo Anterior:"
  Sal_Ant[5]=SPACE(11)
  Sal_Ant[6]= 0
  Sal_Ant[7]= 0
  Sal_Ant[8]= 0
  Sal_Ant[9]= 0
  Sal_Ant[10]=0
  Sal_Ant[11]=SPACE(4)
 ENDIF

 *Solicitud del dia comienzo el final de la caja
 DO WHILE .T.
  SELECT FACTURA
  SETCOLOR(colo3)
  SOMBRA(11,30,14,46,'H1',1,'',colo3)
  @12,31 SAY 'Desde D¡a : '
  @13,31 SAY 'Hasta D¡a : '
  ult_dia=ULTIMO(VAL(bmes),VAL(bano))
  STORE '  ' TO dia_des,dia_has
  @12,43 GET dia_des PICTURE '@K 99' VALID VAL(dia_des)>=1.AND.VAL(dia_des)<=ult_dia
  @13,43 GET dia_has PICTURE '@K 99' VALID VAL(dia_has)>=VAL(dia_des).AND.VAL(dia_has)<=ult_dia
  READ
  IF LASTKEY()=27
   CLOSE ALL
   RESTSCREEN(0,0,24,79,pant_ant)
   SETCOLOR(col_ant)
   EXIT
  ENDIF
  dia_des=CERO(VAL(dia_des),2)
  dia_has=CERO(VAL(dia_has),2)
  *Controlar si ya listo estos dias
  SELECT LIBRO
  bList=.F.
  IF !VACIA()
   GO TOP
   ok=0
   DO WHILE !EOF()
    vddia=ddia
    vhdia=hdia
    IF (dia_des >= vddia .AND. dia_des <= vhdia) .OR.;
       (dia_has <= vhdia)
     CARTEL("VERIFIQUE, YA LISTO ALGUNO DE LOS DIAS [ OK ]",12,1,'N',color3)
     ok=1
     EXIT

    ENDIF
    SKIP
   ENDDO
   IF ok=1
    bList=.T.
    *LOOP
   ENDIF
  ENDIF
  SELECT FACTURA

  fe_des=CTOD(dia_des+'/'+bmes+'/'+bano)
  fe_has=CTOD(dia_has+'/'+bmes+'/'+bano)

  IF p6=2
   *Solicitar %IVA a listar
   bIvaI=vIvaI
   vTitul="VTAS.EXPLOTACION AGRARIA"
   SOMBRA(17,09,20,58,"H1",1,'',colo3)
   SETCOLOR(colo3)
   @18,10 SAY "Que Porcentaje de IVA: " GET bIvaI PICTURE "@K 99.99" VALID bIvaI#0
   @19,10 SAY "Titulo del Listado   : " GET vTitul PICTURE "@K!"
   READ
   IF LASTKEY()=27
    LOOP
   ENDIF
  ELSE
   IF p6=1
    bIvaI=vIvaI
    vTitul="I.V.A. VENTAS"
   ELSE
    bIvaI=0
    vTitul="I.V.A. VENTAS (Todos los %)"
   ENDIF
  ENDIF
  vTitul=vTitul+": "
  vSrie=SPACE(4)
  Pan_Sri=SAVESCREEN(,,,,)
  ok=0
  DO WHILE .T.
   SOMBRA(10,10,14,50,"H1",1,'',colo3)
   @13,15 SAY "9999-Todos" COLOR colo3
   @11,12 SAY "Que Punto Venta: " GET vSrie PICTURE "@K 9999" VALID VAL(vSrie)#0
   READ
   RESTSCREEN(,,,,Pan_Sri)
   IF LASTKEY()=27
    ok=1
    EXIT
   ENDIF
   vSrie=CERO(VAL(vSrie),4)
   EXIT
  ENDDO
  IF ok=1
   ok=0
   SET FILTER TO
   LOOP
  ENDIF

  bien = CARTEL("Enviamos a un Archivo ? [ SI ] [ NO ]",18,2,'N',colo3)

  IF !bien .OR. LASTKEY() = 27
   SET FILTER TO fech>=fe_des .AND. fech<=fe_has.AND.IIF(p6#3,prin=bIvaI,.T.);
                 .AND.tcom#'RT'.AND.IIF(vSrie="9999",.T.,vSrie=srie)
   GO TOP
   COUNT TO pipi
   IF pipi=0
    CARTEL("PERIODO SIN REGISTRACION DE VENTAS  [ OK ]",18,1,'N',color3)
    SET FILTER TO
    LOOP
   ENDIF
   ok=0
   IF CONFIR()
    IF ok=0
     DO proceso
    ENDIF
   ENDIF
  ELSE
   *Enviar a un Archivo
   IF CREA_ARCH2(vPer)
    SELECT FACTURA
    SET ORDER TO 1
    SET FILTER TO fech>=fe_des .AND. fech<=fe_has.AND.IIF(p6#3,prin=bIvaI,.T.);
                  .AND.tcom#'RT'.AND.IIF(vSrie="9999",.T.,vSrie=srie)
    GO TOP
    DO Iva_Arch
    SET FILTER TO
    GO TOP
   ENDIF
  ENDIF
 ENDDO
 CLOSE ALL
ENDDO

PROCEDURE proceso
PUBLIC total1,total2,total3,total4
STORE 0 TO total1,total2,total3,total4
DO iva_list
RETURN

PROCEDURE iva_list
SELECT FACTURA
GO TOP
IF !IMPRI()
 okk=0

 DO IMP_IVA

 IF okk=0
  *ARCHIVO AUXILIAR
  AREA()
  IF USAR('AUXILIAR','AUX')
   SELECT AUX
   GO TOP
   RECLOCK(1)
   REPLACE hiva WITH pag
   UNLOCK
   CLOSE
  ENDIF
  SELECT FACTURA
  SET EXCLUSIVE ON
  REPLACE ALL liva WITH .T. FOR fech>=fe_des .AND. fech<=fe_has .AND.IIF(p6#3,prin=bIvaI,.T.);
                  .AND.tcom#'RT'.AND.IIF(vSrie="9999",.T.,vSrie=srie)
  SET EXCLUSIVE OFF
  SELECT LIBRO
  guardar=CARTEL("DESEA ACTUALIZAR SALDO ANTERIOR [ SI ] [ NO ]",18,2,'N',colo3)
  IF guardar
   ADDREC(1)
   RECLOCK(1)
   REPLACE ddia WITH dia_des
   REPLACE hdia WITH dia_has
   REPLACE ngra WITH Sal_Ant[6]
   REPLACE iexe WITH Sal_Ant[7]
   REPLACE irin WITH Sal_Ant[8]
   REPLACE irni WITH Sal_Ant[9]
   REPLACE itot WITH Sal_Ant[10]
   UNLOCK
  ENDIF
 ENDIF
ELSE
 DO PAN_IVA
ENDIF
RETURN

PROCEDURE IMP_IVA
*ARCHIVO AUXILIAR
AREA()
IF USAR('AUXILIAR','AUX')
 GO TOP
 pagina=hiva      &&N§ Hoja de iva
ELSE
 pagina=0
ENDIF
CLOSE
SELECT FACTURA
Hor=0
SET CONSOLE OFF
SET PRINTER ON
?? CHR(27)+CHR(120)+CHR(0)+CHR(18)
?? CHR(27)+CHR(67)+CHR(72)
?? CHR(27)+CHR(33)+CHR(5)
condi="fech>=fe_des .AND. fech<=fe_has.AND.IIF(p6#3,prin=bIvaI,.T.)"
Condi=Condi+".AND.tcom#'RT'.AND.IIF(vSrie='9999',.T.,vSrie=srie)"
titul=vTitul+NO_MES(bmes)+'/'+bano
stitul="C.U.I.T.: 30-54484283-4  N§I.Brutos: 0505986   N§Establ. : 17-0505986-0  Sede Timbrado : 17  Domicilio Empresa : Av.Mitre 3620"

SET FILTER TO &Condi
GO TOP

*Variable para listar por Grupo
gru =DTOC(fech)+' - '+LTRA
vgru=DTOC(fech)+' - '+LTRA
ti_gru='Totales DIA - Cbtes.: '

*SubGrupo para Totalizador General
SGru=LTRA
PRIVATE p:=ltra

*Totalizador General
PRIVATE TNGra,TExen,TIRin,TIRni,TTota
STORE 0 TO TNGra,TExen,TIRin,TIRni,TTota

*Subtotalizador por Dia
PRIVATE SNGra,SExen,SIRin,SIRni,STota
STORE 0 TO SNGra,SExen,SIRin,SIRni,STota

*Totalizador por Letra
PRIVATE INgra&p,IExen&p,IIrin&p,IIrni&p,ITota&p
STORE 0 TO INgra&p,IExen&p,IIrin&p,IIrni&p,ITota&p

PRIVATE Letras:={}
AADD(Letras,SGru)
Amor=0
Paso=0
DO WHILE INKEY(.1)#27.AND.!EOF()
 *dibujar encabezado
 DO IV_TI_IMP

 DO WHILE INKEY(.1)#27.AND. hor<=63.AND.!EOF()
  ? "**** Totales Dia : "+Gru
  hor++

  DO WHILE INKEY(.1)#27.AND. hor<=63.AND.Gru=vGru
   * Tipo Cbte.
   vPcia=CLIENTES->pcia
   SELECT PCIA
   SEEK vPcia
   IF FOUND()
    vExto=exto
   ELSE
    vExto="N"
   ENDIF
   SELECT FACTURA

   ? tcom+' '

   * Numero Cbte.
   ?? ltra+srie+nume+' '

   * Cliente
   ?? IIF(clie='000000',SUBSTR(CONS_FIN->nomb,1,15),SUBSTR(CLIENTES->nomb,1,15))+' '

   * Cuit
   ?? IIF(clie='000000',IIF(VAL(CONS_FIN->cuit)=0,SPACE(11),CONS_FIN->cuit),IIF(VAL(CLIENTES->cuit)=0,SPACE(11),CLIENTES->cuit))+' '

   * Neto Gravado
   NGrax=IIF(!BAJA,IIF(civa='4',IIF(vExto="N",ngra/((prin+100)/100),0),;
                   IIF(civa='3'.OR.(civa>='5'),;
                       ngra/((prin+100)/100),ngra)),;
           0)
   NGraxS=IIF(NGrax=0,SPACE(10),STR(NGrax,10,2))
   ?? NGraxS+' '

   * Exento
   Exenx=IIF(!BAJA,IIF(civa='4',IIF(vExto="S",ngra,0),0),0)
   ExenxS=IIF(Exenx=0,SPACE(10),STR(Exenx,10,2))
   ?? ExenxS+' '

   *Iva Inscripto
   Irinx=IIF(!BAJA,IIF(civa='4',IIF(vExto="N",((ngra/((prin+100)/100))*prin)/100,0),IIF(civa='3'.OR.(civa>='5'),((ngra/((prin+100)/100))*prin)/100,;
         (ngra*prin)/100)),0)
   IrinxS=IIF(Irinx=0,SPACE(10),STR(Irinx,10,2))
   ?? IrinxS+' '

   * Iva No Inscripto
   Irnix=IIF(!BAJA,IIF(civa='2',((ngra*prni)/100),0),0)
   IrnixS=IIF(Irnix=0,SPACE(10),STR(Irnix,10,2))
   ?? IrnixS+' '

   *Importe Total
   Totax=IIF(!BAJA,paga,0)
   TotaxS=IIF(Totax=0,SPACE(10),STR(Totax,10,2))
   ?? TotaxS+' '

   *Estado de Registro
   ?? IIF(baja,'Anu.',SPACE(4))

   *Subtotal Por Dia y Letra
   SNGra=SNGra+NGrax
   SExen=SExen+Exenx
   SIrin=SIrin+Irinx
   SIrni=SIRni+Irnix
   STota=STota+Totax

   *Totalizar  Por Letra
   INGra&p=INGra&p+NGrax
   IExen&p=IExen&p+Exenx
   IIrin&p=IIRin&p+Irinx
   IIrni&p=IIRni&p+Irnix
   ITota&p=ITota&p+Totax

   IF tcom="NC"
    IF Amor=0 &&Para Ejecutar la primera vez que aparece una NC y Nada Mas
     *SubGrupo para Totalizador General de NC/Ltra
     SSGru=tcom+' '+LTRA
     PRIVATE pp:=tcom+ltra

     *Totalizador por NC/Letra
     PRIVATE INg&pp,IEx&pp,IIi&pp,IIn&pp,ITo&pp
     STORE 0 TO INg&pp,IEx&pp,IIi&pp,IIn&pp,ITo&pp

     PRIVATE NCLtra:={}
     AADD(NCLtra,SSGru)
     Amor=1
    ELSE
     SSGru=tcom+' '+ltra
     Pos=ASCAN(NCLtra, {|x| x==SSGru})
     IF Pos=0
      AADD(NCLtra,SSGru)
      pp=LEFT(SSGru,2)+RIGHT(SSGru,1)
      *Totalizador por NCLetra
      PRIVATE INg&pp,IEx&pp,IIi&pp,IIn&pp,ITo&pp
      STORE 0 TO INg&pp,IEx&pp,IIi&pp,IIn&pp,ITo&pp
     ELSE
      pp=LEFT(SSGru,2)+RIGHT(SSGru,1)
     ENDIF
    ENDIF
    *Totalizar  Por NC/Letra
    ING&pp=ING&pp+NGrax
    IEx&pp=IEx&pp+Exenx
    IIi&pp=IIi&pp+Irinx
    IIn&pp=IIn&pp+Irnix
    ITo&pp=ITo&pp+Totax
   ENDIF

   *Sumo Linea Impresa
   Hor++

   *Pasar al Siguiente Registro
   SKIP
   vGru=DTOC(fech)+' - '+LTRA
   p=ltra

  ENDDO
  IF LASTKEY()=27
   okk=1
   EXIT
  ENDIF
  IF Gru # vGru
   IF Hor+2 <= 63
    ? "   ***Sub Total***"
    ? SPACE(45)+STR(SNGra,10,2)+' '+STR(SExen,10,2)+SPACE(1)+STR(SIRin,10,2)+' '+;
      STR(SIRni,10,2)+' '+STR(STota,10,2)
    Hor=Hor+2
    *Totalizador Principal
    TNGra=TNgra+SNGra
    TExen=TExen+SExen
    TIrin=TIrin+SIRin
    TIrni=TIrni+SIRni
    TTota=TTota+STota
    STORE 0 TO SNGra,SExen,SIRin,SIRni,STota
    Gru=vGru
    SGru=ltra
    Pos=ASCAN(Letras, {|x| x==SGru})
    IF Pos=0
     AADD(Letras,SGru)
     p=SGru
     *Totalizador por Letra
     PRIVATE INgra&p,IExen&p,IIrin&p,IIrni&p,ITota&p
     STORE 0 TO INgra&p,IExen&p,IIrin&p,IIrni&p,ITota&p
    ELSE
     p=SGru
    ENDIF
    *Totalizador por NCletra
    IF tcom="NC"
     IF Amor=0 &&Para Ejecutar la primera vez que aparece una NC y Nada Mas
      *SubGrupo para Totalizador General de NC/Ltra
      SSGru=tcom+' '+LTRA
      PRIVATE pp:=tcom+ltra

      *Totalizador por NC/Letra
      PRIVATE INg&pp,IEx&pp,IIi&pp,IIn&pp,ITo&pp
      STORE 0 TO INg&pp,IEx&pp,IIi&pp,IIn&pp,ITo&pp

      PRIVATE NCLtra:={}
      AADD(NCLtra,SSGru)
      Amor=1
     ELSE
      SSGru=tcom+' '+ltra
      Pos=ASCAN(NCLtra, {|x| x==SSGru})
      IF Pos=0
       AADD(NCLtra,SSGru)
       pp=LEFT(SSGru,2)+RIGHT(SSGru,1)
       *Totalizador por NCLetra
       PRIVATE INg&pp,IEx&pp,IIi&pp,IIn&pp,ITo&pp
       STORE 0 TO INg&pp,IEx&pp,IIi&pp,IIn&pp,ITo&pp
      ELSE
       pp=LEFT(SSGru,2)+RIGHT(SSGru,1)
      ENDIF
     ENDIF
    ENDIF
    Paso=1
   ELSE
    hor=64
   ENDIF
  ENDIF
 ENDDO
 IF LASTKEY()=27
  okk=1
  EXIT
 ENDIF
 IF hor>=63
  hor=0
  EJECT
 ENDIF
ENDDO
IF LASTKEY()=27
 okk=1
 ? "LISTADO ABORTADO POR EL OPERADOR"
ELSE
 IF Hor+3 > 63
  EJECT
  DO IV_TI_IMP
 ENDIF
 IF EOF().AND.Paso=0
  ? "   ***Sub Total***"
  ? SPACE(45)+STR(SNGra,10,2)+' '+STR(SExen,10,2)+SPACE(1)+STR(SIRin,10,2)+' '+;
    STR(SIRni,10,2)+' '+STR(STota,10,2)
  ?
  Hor=Hor+3
  *Totalizador Principal
  TNGra=TNgra+SNGra
  TExen=TExen+SExen
  TIrin=TIrin+SIRin
  TIrni=TIrni+SIRni
  TTota=TTota+STota
  STORE 0 TO SNGra,SExen,SIRin,SIRni,STota
  Gru=vGru
  SGru=ltra
  Pos=ASCAN(Letras, {|x| x==SGru})
  IF Pos=0
   AADD(Letras,SGru)
   p=SGru
   *Totalizador por Letra
   PRIVATE INgra&p,IExen&p,IIrin&p,IIrni&p,ITota&p
   STORE 0 TO INgra&p,IExen&p,IIrin&p,IIrni&p,ITota&p
  ELSE
   p=SGru
  ENDIF
  *Totalizador por NCletra
  IF tcom="NC"
   SSGru=tcom+' '+ltra
   Pos=ASCAN(NCLtra, {|x| x==SSGru})
   IF Pos=0
    AADD(NCLtra,SSGru)
    pp=LEFT(SSGru,2)+RIGHT(SSGru,1)
    *Totalizador por NCLetra
    PRIVATE INg&pp,IEx&pp,IIi&pp,IIn&pp,ITo&pp
    STORE 0 TO INg&pp,IEx&pp,IIi&pp,IIn&pp,ITo&pp
   ELSE
    pp=LEFT(SSGru,2)+RIGHT(SSGru,1)
   ENDIF
  ENDIF
 ENDIF
 ? "***Total***"
 ? SPACE(45)+ STR(TNGra,10,2)+' '+STR(TExen,10,2)+SPACE(1)+STR(TIRin,10,2)+' '+;
   STR(TIRni,10,2)+' '+STR(TTota,10,2)
 ?
 Hor=Hor+3
 Longi=LEN(Letras)+LEN(NCLtra)
 IF Longi>0
  IF Hor+Longi > 63
   EJECT
   DO IV_TI_IMP
  ENDIF
  *Ordeno Por Letra
  ORDEN(Letras)
  FOR Con=1 TO LEN(Letras)
   p=LEFT(Letras[con],1)
   IF !EMPTY(p)
    ? "** Total Cbte.Tipo: "+Letras[Con]
    ?? SPACE(24)+STR(INGra&p,10,2)+' '+STR(IExen&p,10,2)+SPACE(1)+;
                STR(IIRin&p,10,2)+' '+STR(IIRni&p,10,2)+;
                ' '+STR(ITota&p,10,2)
    hor++
   ENDIF
  NEXT
  ?
  hor++
  *Ordeno Por NC/Letra
  ORDEN(NCLtra)
  FOR Con=1 TO LEN(NCLtra)
   pp=LEFT(NCLtra[con],2)+RIGHT(NCLtra[con],1)
   IF !EMPTY(pp)
    ? "** Total: "+NCLtra[Con]+SPACE(31)
    ?? STR(ING&pp,10,2)+' '+STR(IEx&pp,10,2)+' '+STR(IIi&pp,10,2)+' '+STR(IIn&pp,10,2)+;
                ' '+STR(ITo&pp,10,2)
    hor++
   ENDIF
  NEXT
  ?
  EJECT
 ENDIF
ENDIF
?? CHR(27)+CHR(120)+CHR(0)+CHR(18)
?? CHR(27)+CHR(33)+CHR(8)
SET CONSOLE ON
SET PRINTER OFF
RETURN

PROCEDURE PAN_IVA
*ARCHIVO AUXILIAR
AREA()
IF USAR('AUXILIAR','AUX')
 GO TOP
 pagina=hiva      &&N§ Hoja de iva
ELSE
 pagina=0
ENDIF
CLOSE
SELECT FACTURA
Hor=0
condi="fech>=fe_des .AND. fech<=fe_has.AND.IIF(p6#3,prin=bIvaI,.T.)"
Condi=Condi+".AND.tcom#'RT'.AND.IIF(vSrie='9999',.T.,vSrie=srie)"
titul=vTitul+NO_MES(bmes)+'/'+bano
stitul="Primer Nro. Pagina:"

SET FILTER TO &Condi
GO TOP

*Variable para listar por Grupo
gru=DTOC(fech)+' - '+LTRA
vgru=DTOC(fech)+' - '+LTRA
ti_gru='Totales DIA - Cbtes.: '

*SubGrupo para Totalizador General
SGru=LTRA
PRIVATE p:=ltra

*Totalizador General
PRIVATE TNGra,TIRin,TiExe,TTota
STORE 0 TO TNGra,TIRin,TiExe,TTota

*Subtotalizador por Dia
PRIVATE SNGra,SIRin,SiExe,STota
STORE 0 TO SNGra,SIRin,SiExe,STota

*Totalizador por Letra
PRIVATE INgra&p,IIrin&p,IiExe&p,ITota&p
STORE 0 TO INgra&p,IIrin&p,IiExe&p,ITota&p

PRIVATE Letras:={}
PRIVATE NCLtra:={}
AADD(Letras,SGru)
Amor=0
Paso=0
DO WHILE INKEY(.1)#27.AND.!EOF()
 *dibujar encabezado
 DO IV_TI_PAN

 DO WHILE INKEY(.1)#27.AND. hor<=21.AND.!EOF()
  @hor,7 SAY "**** Totales Dia : "+Gru COLOR colo12
  hor++

  DO WHILE INKEY(.1)#27.AND. hor<=21.AND.Gru=vGru
   IF baja
    SETCOLOR("N+/B")
   ENDIF
   vPcia=CLIENTES->pcia
   SELECT PCIA
   SEEK vPcia
   *IF FOUND()
   * vExto=exto
   *ELSE
    vExto="N"
   *ENDIF
   SELECT FACTURA
   * Tipo Cbte.
   Paso=0
   @hor,0 SAY tcom

   * Numero Cbte.
   @hor,3 SAY ltra+srie+nume

   * Cliente
   @hor,17 SAY clie

   * Neto Gravado
   NGrax=IIF(!BAJA,IIF(civa='4',IIF(vExto="N",ngra/((prin+100)/100),0),IIF(civa='3'.OR.civa>='5',ngra/((prin+100)/100),;
         ngra)),0)
   SNGra=SNGra+NGrax
   @hor,24 SAY STR(NGrax,10,2)

   *Iva Inscripto
   Irinx=IIF(!BAJA,IIF(civa='4',IIF(vExto="N",((ngra/((prin+100)/100))*prin)/100,0),IIF(civa='3'.OR.civa>='5',((ngra/((prin+100)/100))*prin)/100,;
         (ngra*prin)/100)),0)
   SIrin=SIrin+Irinx
   @hor,35 SAY STR(Irinx,10,2)

   * Importe Exento
   IExex=IIF(!BAJA,IIF(civa='4',IIF(vExto="S",ngra,0),0),0)
   IxenxS=IIF(IExex=0,SPACE(10),STR(IExex,10,2))

   SiExe=SiExe+iExex
   @hor,46 SAY STR(iExex,10,2)

   *Importe Total
   Totax=IIF(!BAJA,paga,0)
   STota=STota+Totax
   @hor,68 SAY STR(Totax,10,2)

   *Estado de Registro
   @hor,78 SAY IIF(baja,'Anu.',SPACE(4))
   SETCOLOR(colo3)

   *Totalizar  Por Letra
   INGra&p=INGra&p+NGrax
   IIrin&p=IIRin&p+Irinx
   IiExe&p=IiExe&p+iExex
   ITota&p=ITota&p+Totax

   IF tcom="NC"
    IF Amor=0
     *SubGrupo para Totalizador General de NC/Ltra
     SSGru=tcom+' '+LTRA
     PRIVATE pp:=tcom+ltra

     *Totalizador por NC/Letra
     PRIVATE INg&pp,IIi&pp,IIn&pp,ITo&pp
     STORE 0 TO INg&pp,IIi&pp,IIn&pp,ITo&pp

     PRIVATE NCLtra:={}
     AADD(NCLtra,SSGru)
     Amor=1
    ELSE
     SSGru=tcom+' '+ltra
     Pos=ASCAN(NCLtra, {|x| x==SSGru})
     IF Pos=0
      AADD(NCLtra,SSGru)
      pp=LEFT(SSGru,2)+RIGHT(SSGru,1)
      *Totalizador por NCLetra
      PRIVATE INg&pp,IIi&pp,IIn&pp,ITo&pp
      STORE 0 TO INg&pp,IIi&pp,IIn&pp,ITo&pp
     ELSE
      pp=LEFT(SSGru,2)+RIGHT(SSGru,1)
     ENDIF
    ENDIF
    *Totalizar  Por NC/Letra
    ING&pp=ING&pp+NGrax
    IIi&pp=IIi&pp+Irinx
    IIn&pp=IIn&pp+iExex
    ITo&pp=ITo&pp+Totax
   ENDIF

   *Sumo Linea Impresa
   Hor++

   *Pasar al Siguiente Registro
   SKIP
   vGru=DTOC(fech)+' - '+LTRA
   p=ltra

  ENDDO
  IF Gru # vGru
   IF Hor+2 <= 21
    @hor,0 SAY "***Sub Total***" COLOR colo12
    @hor,24 SAY STR(SNGra,10,2)+SPACE(1)+STR(SIRin,10,2)+' '+;
      STR(SiExe,10,2)+' '+SPACE(10)+' '+STR(STota,10,2) COLOR color3
    Hor=Hor+2
    *Totalizador Principal
    TNGra=TNgra+SNGra
    TIrin=TIrin+SIRin
    TiExe=TiExe+SiExe
    TTota=TTota+STota
    STORE 0 TO SNGra,SIRin,SiExe,STota
    Gru=vGru
    *Totalizador por letra
    SGru=ltra
    Pos=ASCAN(Letras, {|x| x==SGru})
    IF Pos=0
     AADD(Letras,SGru)
     p=SGru
     *Totalizador por Letra
     PRIVATE INgra&p,IIrin&p,IiExe&p,ITota&p
     STORE 0 TO INgra&p,IIrin&p,IiExe&p,ITota&p
    ELSE
     p=SGru
    ENDIF
    *Totalizador por NCletra
    IF tcom="NC"
     SSGru=tcom+' '+ltra
     Pos=ASCAN(NCLtra, {|x| x==SSGru})
     IF Pos=0
      AADD(NCLtra,SSGru)
      pp=LEFT(SSGru,2)+RIGHT(SSGru,1)
      *Totalizador por NCLetra
      PRIVATE INg&pp,IIi&pp,IIn&pp,ITo&pp
      STORE 0 TO INg&pp,IIi&pp,IIn&pp,ITo&pp
     ELSE
      pp=LEFT(SSGru,2)+RIGHT(SSGru,1)
     ENDIF
    ENDIF
    Paso=1
   ELSE
    hor=22
   ENDIF
  ENDIF
  IF hor>=21 .OR. EOF()
   SETCOLOR(color3)
   @23,20 SAY " ±Esc±-Sale  ±ÄÙ±-Continua "
   SETCOLOR(colo3)
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
   EXIT
  ENDIF
 ENDDO
 IF tecla=27
  EXIT
 ENDIF
ENDDO
IF LASTKEY()#27
 IF Hor+2 > 21
  DO IV_TI_PAN
 ENDIF
 IF EOF().AND.Paso=0
  @hor,0 SAY "***Sub Total***" COLOR colo12
  @hor,24 SAY STR(SNGra,10,2)+SPACE(1)+STR(SIRin,10,2)+' '+;
    STR(SiExe,10,2)+' '+SPACE(11)+STR(STota,10,2) COLOR color3
  Hor=Hor+2
  *Totalizador Principal
  TNGra=TNgra+SNGra
  TIrin=TIrin+SIRin
  TiExe=TiExe+SiExe
  TTota=TTota+STota
  STORE 0 TO SNGra,SIRin,SiExe,STota
  Gru=vGru
  *Totalizador por letra
  SGru=ltra
  Pos=ASCAN(Letras, {|x| x==SGru})
  IF Pos=0
   AADD(Letras,SGru)
   p=SGru
   *Totalizador por Letra
   PRIVATE INgra&p,IIrin&p,IiExe&p,ITota&p
   STORE 0 TO INgra&p,IIrin&p,IiExe&p,ITota&p
  ELSE
   p=SGru
  ENDIF
  *Totalizador por NCletra
  IF tcom="NC"
   SSGru=tcom+' '+ltra
   Pos=ASCAN(NCLtra, {|x| x==SSGru})
   IF Pos=0
    AADD(NCLtra,SSGru)
    pp=LEFT(SSGru,2)+RIGHT(SSGru,1)
    *Totalizador por NCLetra
    PRIVATE INg&pp,IIi&pp,IIn&pp,ITo&pp
    STORE 0 TO INg&pp,IIi&pp,IIn&pp,ITo&pp
   ELSE
    pp=LEFT(SSGru,2)+RIGHT(SSGru,1)
   ENDIF
  ENDIF
 ENDIF
 @hor,0 SAY "***TOTAL***" COLOR color3
 @hor,24 SAY STR(TNGra,10,2)+SPACE(1)+STR(TIRin,10,2)+' '+;
   STR(TiExe,10,2)+' '+SPACE(11)+STR(TTota,10,2) COLOR color3
 Hor=Hor+2
 Longi=LEN(Letras)+LEN(NCLtra)
 IF Longi>0
  IF Hor+2+Longi > 21
   SETCOLOR(color3)
   @23,20 SAY " ±Esc±-Sale  ±ÄÙ±-Continua "
   SETCOLOR(colo3)
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
    RETURN
   ENDIF
   DO IV_TI_PAN
  ENDIF
  *Ordeno Por Letra
  ORDEN(Letras)
  FOR Con=1 TO LEN(Letras)
   p=LEFT(Letras[con],1)
   IF !EMPTY(p)
    @hor,0 SAY "** Total: "+Letras[Con]
    @hor,24 SAY STR(INGra&p,10,2)+SPACE(1)+;
                STR(IIRin&p,10,2)+' '+STR(IiExe&p,10,2)+;
                ' '+SPACE(10)+' '+STR(ITota&p,10,2)
    hor++
   ENDIF
  NEXT
  Hor++
  *Ordeno Por NC/Letra
  ORDEN(NCLtra)
  FOR Con=1 TO LEN(NCLtra)
   pp=LEFT(NCLtra[con],2)+RIGHT(NCLtra[con],1)
   IF !EMPTY(pp)
    @hor,0 SAY "** Total: "+NCLtra[Con]
    @hor,24 SAY STR(ING&pp,10,2)+' '+STR(IIi&pp,10,2)+' '+STR(IIn&pp,10,2)+;
                ' '+SPACE(10)+' '+STR(ITo&pp,10,2)
    hor++
   ENDIF
  NEXT
 ENDIF
ELSE
 @hor+1,10 SAY "LISTADO ABORTADO POR EL OPERADOR"
ENDIF
SETCOLOR(color3)
@23,20 SAY " ±Esc±-Sale "
SETCOLOR(colo3)
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
RETURN

PROCEDURE IV_TI_IMP
 *dibujar encabezado
 pagina=pagina+1
 NORMA()
 GRAN_ON()
 ? SUBSTR(nom_emp+SPACE(25),1,25)
 GRAN_OFF()
 NORMA()
 CHICO()
 ?? SPACE(23)+'Pagina Nro. : '+STR(pagina,5)
 NORMA()
 GRAN_ON()
 ? Titul
 GRAN_OFF()
 NORMA()
 CHICO()
 ? sTitul
 ? "== ============= =============== =========== ========== ========== ========== ========== ========== ===="
 ? "Tp     N—mero                                   Neto                   IVA      IVA NO    Importe      "
 ? "Cb  Comprobante  Nombre Cliente   Nro. CUIT   Gravado     Exento   Inscripto  Inscripto    Total       "
 ? "== ============= =============== =========== ========== ========== ========== ========== ========== ===="
*   12 1234567890123 123456789012345 12345678901 1234567890 1234567890 1234567890 1234567890 1234567890 1234
 hor=7
RETURN

PROCEDURE IV_TI_PAN
 *dibujar encabezado
 @1,0 CLEAR TO 24,79
 @1,0 SAY CENTRA(Titul,79) COLOR colo12
 @2,0 SAY CENTRA(sTitul+' '+STR(Pagina+1,5),79) COLOR "*"+colo12
 @3,0 SAY "Tp     N—mero    C•digo    Neto       IVA      Importe               Importe    "
 @4,0 SAY "Cb  Comprobante  Client   Gravado  Inscripto   Exento                 Total     "
 @5,0 SAY "== ============= ====== ========== ========== ==========            ============"
       *   0  3             17     24         35         46         57         67        77
 hor=6
RETURN

FUNCTION CREA_ARCH2(fPer)
PRIVATE Num_Arch,aEstruct,fBien
DO WHILE .T.
 Nom_Arch = Rut_Lis+"LIV"+vPer+".DBF"
 IF !FILE("&Nom_Arch")
  EXIT
 ELSE
  fBien=CARTEL("USTED YA CREO ESTE MES [ SOBREESCRIBO ] [ ABORTAR ]",12,2,'N',color3)
  IF LASTKEY()=27.OR.!fBien
   RETURN .F.
  ENDIF
  FERASE("&Nom_Arch")
  EXIT
 ENDIF
ENDDO
aEstruct:={}
AADD(aEstruct,{"FECH","D",8,0})
AADD(aEstruct,{"TCOM","C",2,0})
AADD(aEstruct,{"LTRA","C",1,0})
AADD(aEstruct,{"SRIE","C",4,0})
AADD(aEstruct,{"NUME","C",8,0})
AADD(aEstruct,{"NOMB","C",30,0})
AADD(aEstruct,{"CUIT","C",11,0})
AADD(aEstruct,{"NGRA","N",12,2})
AADD(aEstruct,{"IEXE","N",12,2})
AADD(aEstruct,{"IRIN","N",12,2})
AADD(aEstruct,{"IRNI","N",12,2})
AADD(aEstruct,{"ITOT","N",12,2})
AADD(aEstruct,{"ESTA","C",7,0})
DBCREATE("&Nom_Arch",aEstruct)
AREA()
IF !USAR("&Nom_Arch","ARCHIVO")
 CARTEL("NO PUDE CREAR EL ARCHIVO PARA ALOJAR EL LISTADO [ OK ]",12,1,'N',color3)
 RETURN .F.
ENDIF
RETURN .T.

PROCEDURE IVA_ARCH
SELECT FACTURA
GO TOP
DO WHILE !EOF()
 cam01=fech
 cam02=tcom
 cam03=ltra
 cam04=srie
 cam05=nume
 cam06=IIF(clie='000000',CONS_FIN->nomb,CLIENTES->nomb)
 cam07=IIF(clie='000000',IIF(VAL(CONS_FIN->cuit)=0,SPACE(11),CONS_FIN->cuit),IIF(VAL(CLIENTES->cuit)=0,SPACE(11),CLIENTES->cuit))
 *Neto gravado
 cam08=IIF(!BAJA,IIF(civa='4',0,IIF(civa='3'.OR.(civa>='5'),ngra/((prin+100)/100),;
       ngra)),0)
 *Importe Exento
 cam09=IIF(!BAJA,IIF(civa='4',ngra,0),0)
 *Importe IVA Inscripto
 cam10=IIF(!BAJA,IIF(civa='4',0,IIF(civa='3'.OR.(civa>='5'),((ngra/((prin+100)/100))*prin)/100,;
       (ngra*prin)/100)),0)
 *Importe IVA NO Inscripto
 cam11=IIF(!BAJA,IIF(civa='2',((ngra*prni)/100),0),0)
 *Importe Total
 cam14=IIF(!BAJA,paga,0)
 *Estado
 cam15=IIF(BAJA,'Anulado',SPACE(7))
 SELECT ARCHIVO
 ADDREC(1)
 RECLOCK(1)
 REPLACE fech WITH cam01
 REPLACE tcom WITH cam02
 REPLACE ltra WITH cam03
 REPLACE srie WITH cam04
 REPLACE nume WITH cam05
 REPLACE nomb WITH cam06
 REPLACE cuit WITH cam07
 REPLACE ngra WITH cam08
 REPLACE iexe WITH cam09
 REPLACE irin WITH cam10
 REPLACE irni WITH cam11
 REPLACE itot WITH cam14
 REPLACE esta WITH cam15
 UNLOCK
 SELECT FACTURA
 SKIP
ENDDO
SELECT ARCHIVO
CLOSE
SELECT FACTURA
RETURN

