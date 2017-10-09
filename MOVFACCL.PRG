*FACTURACION
#INCLUDE "INKEY.CH"
#INCLUDE "SETCURS.CH"
SET KEY 306 TO MUES_CBTE
IF vmes<>wwmes .OR. vano <> wwano
 CARTEL('!!! PRESTE ATENCION, MES/A¥O INCORRECTO PARA FACTURAR  [ OK ]',12,1,'N',color3)
 RETURN
ENDIF
PUBLIC iva1,iva2,imp_ven,coli_ant,ok,regcli,sale,vdol,vOpcion:=1,xDolar:=vDolar
PRIVATE Pan_Cabe
sale=0
OK=0
coli_ant=SETCOLOR()
*Porcentaje de iva
AREA()
IF USAR('PORCEIVA')
 iva1=IVAI
 iva2=IVANI
 imp_ven=VENTA
ELSE
 QUIT
ENDIF
IF USAR('PORCINBR')
 pb1=inbr1
 pb2=inbr2
 pb3=inbr3
 pb4=inbr4
ELSE
 QUIT
ENDIF
*chequear carpetas del Spooler revisando que no existan archivos mal borrados
 IF (FILE("&carpfis.\&tempofi3."))
    FERASE("&carpfis.\&tempofi3.")
 ENDIF
 IF (FILE("&respfis.\&respufi3."))
    FERASE("&respfis.\&respufi3.")
 ENDIF

*ARCHIVO AUXILIAR
IF USAR('AUXILIAR','AUX')
 GO TOP
 PUBLIC vsrie,Fac_A,Fac_B,Cre_A,Cre_B,vRG212,vNrem,Ticket,vPago
 PRIVATE xPtoA,xPtoB

 vsrie=srie
 vNRem = nrem
 Fac_A=nFacA
 Fac_B=nFacB
 Cre_A=nCreA
 Cre_B=nCreB
 Ticket=ntck
 vPago=npcl
 vdol=DOLA
 vRG212=RG212
 xPtoA=PtoA
 xPtoB=PtoB

 PUBLIC vPu30,vPu60
 vPu30=IIF(pu30#0,(pu30+100)/100,1)
 vPu60=IIF(pu60#0,(pu60+100)/100,1)
ELSE
 QUIT
ENDIF
*clientes
OPEN_DBF("MAECLIEN","CLIENTES","C","MAECLIEN","clie","NOMCLIEN","nomb")

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

*FACTURAS
AREA()
IF USAR("FCC&periodo","FACTURA")
 IF !FILE('FCC&periodo..NTX')
  INDEX ON clie+tcom+ltra+srie+nume TO FCC&periodo
 ENDIF
 wn=periodo+'1'
 IF !FILE('FC&wn..NTX')
  INDEX ON DTOS(fech) TO FC&wn
 ENDIF
 SET INDEX TO FCC&periodo,FC&wn
ELSE
 QUIT
ENDIF

*DETALLES
AREA()
IF USAR("FCD&periodo","DETALLE")
 IF !FILE('FCD&periodo..NTX')
  INDEX ON clie+tcom+ltra+srie+nume TO FCD&periodo
 ENDIF
 SET INDEX TO FCD&Periodo
ELSE
 QUIT
ENDIF
SET RELATION TO codi INTO ARTIC

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

*Remitos (Cabecera del Remito)
AREA()
OPEN_DBF("REMCLIEN","CAB_REM","C","REMCLIEN","nrem")

*Condiciones de Pago Remitos
AREA()
OPEN_DBF("MAEREMIT","REMIT","C","MAEREMIT","nopc","NOMREMIT","cpag")

SET DECIMAL TO 2
PUBLIC vnom,vdom,loc_Cli,vciv,n_iva,vCpos,vTele,vEmai,xTdoc,vNdoc,vTdoc,n_Resul
PRIVATE pan_ant,vtip,nom_tip,vnum,vrem,vcli,vart,vcct,vsal,vadju
PRIVATE vpci,vdpt
PRIVATE vcui,vfec,pan_trn,com,rel
PRIVATE vtot,descr,dsto,vnc_tp,vimp_rni
PRIVATE fact_nc,imp_nc,REG_FAC,Reg_Pa,nc,ltra_nc,srie_nc,fech_nc,Nint_Pago
PRIVATE reg,regi,mens1,mens2,ing,vsck
PRIVATE VTTA,vnet,iva,reciva,tgral,vFpag
PRIVATE MItems:={},xDesc
STORE 0 TO VTTA,vnet,iva,reciva,tgral,dsto

*Para Remitos
PRIVATE vbul,vcbul,vigu,vdbul,exi_bul,exi_rem,fec_Rem,nro_reparto
PRIVATE vrem,vfec_rem,vimp,vree,vfac1,vfac2,vfac3,vtr1,vtr2,vtr_fl1,vtr_fl2
PRIVATE vde1,vde2,vde_fl1,vde_fl2,vco_pag,wpag,vmens
STORE SPACE(30) TO vtr1,vtr2

*Variables Generales para Carga de Articulos
PRIVATE Com_Rbr,Com_Pro,Com_Mrc,Com_Art,tcl,Rel:=0,agg:=0
PRIVATE Com:=1,Rel:=0,Busca,Hasta,Desde,xPos,zPos,Subir
PRIVATE fRubr,fProv,fMrca,Nom_Rubr
PRIVATE Si_Hay:=.T.,ClaveGen:=""
PRIVATE MRubr:={}
PUBLIC MProv:={}
PRIVATE MMrca:={}
PRIVATE MArti:={}

*Variables para Impresora Fiscal
PRIVATE bandera,nu_fac
PRIVATE bit48 &&Tipo Cliente I=RI,N=RNI,C=CF,E=EXE,M=MT,A=NoResponsable T=NC
PRIVATE bit50 &&Tipo Documento C=CUIT L=CUIL 0=LE 1=LC 2=DNI 3=Pasaporte 4=CI " "=Sin Calificador

PAN_ANT=SAVESCREEN(0,0,24,79)
DO WHILE .T.

 *chequear carpetas del Spooler revisando que no existan archivos mal borrados
  IF (FILE("&carpfis.\&tempofi3."))
     FERASE("&carpfis.\&tempofi3.")
  ENDIF
  IF (FILE("&respfis.\&respufi3."))
     FERASE("&respfis.\&respufi3.")
  ENDIF

 SETCOLOR(color7)
 @1,0 CLEAR TO 24,79
 DO MOVCLFAC
 @22,67 SAY "Pto A:"+xPtoA COLOR color9
 @23,67 SAY "Pto B:"+xPtoB COLOR color9
 vven=SPACE(3)
 vtip=SPACE(1)
 STORE SPACE(2) TO vpci,vdpt,nom_tip
 vart=SPACE(5)
 vcli=SPACE(6)
 vcct=SPACE(1)
 vnum=SPACE(8)
 vNdoc = SPACE(8)
 xTdoc=1 &&CUIT
 agg=0
 n_Resul=0
 vrem=SPACE(8)
 ltra_nc=' '
 srie_nc=SPACE(4)
 fact_nc=SPACE(8)
 imp_nc=0
 vnc_tp=.F.
 fech_nc=CTOD("  /  /    ")
 vnom=SPACE(30)
 vdom=SPACE(25)
 vciv=SPACE(1)
 n_iva = "C"
 loc_Cli=SPACE(15)
 vcui=SPACE(11)
 vCpos=SPACE(8)
 vTele=SPACE(13)
 vEmai=SPACE(30)
 STORE 0 TO vsal,vtot,reg_fac,reg_Pa,nc,ing,dsto,vsck
 Nint_Pago = SPACE(8)
 STORE 0 TO vimp_rni,reg,xreg
 vfec=DATE()
 conta=.T.
 vFpag=1
 STORE SPACE(30) TO descr,mens1,mens2
 vobse=SPACE(78)
 Graba=.F.
 *Para Remitos
 Nro_Reparto = SPACE(8)
 Exi_Rem = .F.
 fec_Rem=CTOD("  /  /    ")
 vFVto=CTOD("  /  /    ")
 *Para Registrador Fiscal
 bandera = SPACE(6)
 nu_fac = 0
 bit48="I"
 bit50="C"
 DO WHILE .T.
  SETCOLOR(color1)
  XPM=4
  XP=1
  SET KEY -4 TO VA_TRAS
  @ 3,59 SAY vfec
  @22,56 SAY xDolar
  DO WHILE XP<=XPM
   SETCOLOR(color1)
   XD=1
   SET CURSOR ON
   DO CASE
    CASE XP=1
     SETCOLOR(color9)
     @24,3 SAY '[Esc] - Salir   [*] - Cliente Ocasional   ['+chr(17)+'ÄÙ] - Ayuda Clientes'
     SETCOLOR(color1)
     sale=0
     vsal=0
     imp_nc=0
     tgral=0
     @ 2,1 GET vcli PICTURE '@K x99999' VALID VAL_CLI(@vcli,2,1,color1,25)
     READ
     @24,18 SAY SPACE(55)
     SETCOLOR(color9)
     @24,19 SAY '[F5] - Campo Anterior '
     IF vsal>1000
      SETCOLOR(color3)
     ELSE
      SETCOLOR(color9)
     ENDIF
     @22,28 SAY vsal PICTURE "@Z 999999999.99"
     SETCOLOR(color1)

    CASE XP=2
     sale=0
     @ 3,45 GET vtip PICTURE `9' VALID VAL_TIP(@vtip,vcli)
     READ
     IF LASTKEY()<>27
      nom_tip=IIF(vtip='1','FA',IIF(vtip='2','NC',IIF(vTip='3','ND',IIF(vTip='4','RT','TK'))))
      vnum="00000000"    &&CERO(reg,8)
      IF vTip#'4'
       IF vciv<='2'
        vltra='A'
       ELSE
        IF xreg=1
         vltra='A'
        ELSE
         vltra='B'
        ENDIF
       ENDIF
      ELSE
       vLtra = "X"
       vnum = vRem
      ENDIF
      SETCOLOR(color9)
      @ 3,45 SAY vtip+' '+nom_tip
      @3,50 SAY vnum
      @4,50 SAY vltra+'-'+vsrie
      vrem=vnum
      IF vTip="2"
       @4,59 SAY "S/FC: "+ltra_Nc+srie_Nc+fact_Nc
      ENDIF
     ENDIF

    CASE XP=3
     IF vTip='1' .OR. vTip = "5"
      sale=0
      @3,71 GET vrem PICTURE "@K 99999999" VALID BUS_REMI(@vRem)
      READ
      vrem=CERO(VAL(vrem),8)
      SETCOLOR(color9)
      @3,71 SAY vrem
     ENDIF

    CASE XP=4
     sale=0
     IF vcli#'000000'
      IF vtip='1' .OR. vTip = "3" .OR. vTip="5"
       IF conta
        vFpag=1
       ELSE
        vFpag=2
       ENDIF
       papi=SAVESCREEN(18,10,23,24)
       SOMBRA(18,10,22,23,'H1',1,'',colo3)
       @18,12 SAY "FORMA PAGO" COLOR colo12
       SETCOLOR(colo3)
       @19,12 PROMPT '1-Contado '
       @20,12 PROMPT '2-30 dias '
       @21,12 PROMPT '3-60 dias '
       MENU TO vFpag
       IF LASTKEY()=27
        XP=5
       ENDIF
       IF vFpag=1
        conta=.T.
       ELSE
        conta=.F.
       ENDIF
       vFVto=CTOD("  /  /    ")
       SETCOLOR(color9)
       @2,39 SAY IIF(.NOT.conta,'C.C',IIF(conta,'CDO',SPACE(3)))
       @3, 9 SAY IIF(vFpag=1,'Contado',IIF(vFpag=2,'30 dias','60 dias'))
       RESTSCREEN(18,10,23,24,papi)
      ENDIF
     ELSE
      SETCOLOR(color9)
      @2,39 SAY "CDO"
      conta=.T.
     ENDIF

   ENDCASE
   IF LASTKEY()=27 .AND. ok=0
    vtip='1' &&Por si era NC y el importe es mayor que la factura
    CLEAR GETS
    CLOSE ALL
    RESTSCREEN(0,0,24,79,Pan_Ant)
    SETCOLOR(Coli_Ant)
    RETURN
    XP=5
   ELSE
    IF  XD=-1.AND.XP=5
     XP=3
    ENDIF
    XP=XP+XD
   ENDIF
  ENDDO

  *Solamente Entro a seleccion de articulos si es FA, NC por devolucion o RT
  IF vTip = "1" .OR. (vTip = "2" .AND. nc = 0 ) .OR. vTip >= "4"
   Pan_Cabe = SAVESCREEN(,,,,)
   ****************************************************************
   SELECT RUBRO
   GO TOP
   fRubr=rubr

   DO WHILE .T.
    *Mostrar En Pantalla
    DO PAN_CONSU1
    SETCOLOR(color9)
    @1,10 SAY CENTRA("========= FACTURACION =========  "+DTOC(DATE())+"  1 usd = $"+STR(vDolar,8,4),60," ") COLOR colo12
    STORE "0000" TO fProv
    fMrca="000"
    PRIVATE MRubr:={}
    PRIVATE MProv:={}
    PRIVATE MMrca:={}
    PRIVATE MArti:={}
    *Items del cBTE
    IF vTip # "2" .AND. !Exi_Rem
     PRIVATE MItems:={}
     vFech=DATE()
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
      SELECT ARTIC
      SET ORDER TO 7
      SETCOLOR(color9)
      SET FILTER TO &Condi
      GO TOP
      zzz = BUS_ART2(09,1,15,78,cam,tit,sep,ind,lar,22,10,nom,22,09)
      SETCOLOR(color9)
      IF agg=5 .OR. !zzz
       CLOSE ALL
       RESTSCREEN(0,0,24,79,PAN_ANT)
       SETCOLOR(coli_ant)
       SET KEY 306 TO
       RETURN
      ENDIF
      SELECT ARTIC
      SET ORDER TO &ord_ant
      SET FILTER TO
      GO TOP

      SET KEY -4 TO
      SET CURSOR ON
      @24,1 SAY SPACE(40)
      IF LASTKEY()=27
       SETCOLOR(coli_ant)
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
   RESTSCREEN(,,,,Pan_Cabe)
  ELSE

   DO VERPRE2
   AADD(MItems,SPACE(171))
   SET CURSOR OFF
   com = 1
   rel = 0
   SETCOLOR(color1)
   KEYBOARD CHR(24)+CHR(5)
   DO WHILE .T.
    com = ACHOICE(7,1,13,77,MItems,.T.,"ver_art2",com,rel)
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
   ENDIF
   SET KEY -4 TO
   SET CURSOR ON
  ENDIF
  exit
 ENDDO
 IF agg=5
  EXIT
 ENDIF
ENDDO
CLOSE ALL
RESTSCREEN(0,0,24,79,PAN_ANT)
SETCOLOR(coli_ant)
SET KEY 306 TO
RETURN

*Actualizacion de Archivos
PROCEDURE GRAVAR_REG
vnum=CERO(VAL(vnum),8)
IF vtip='2'
 nega=(-1)
ELSE
 nega=1
ENDIF
IF vtip='1'.OR. vTip="4" .OR. vTip="5".OR. (vtip='2' .AND. nc=0)
 *Actualizo archivo detalles
 FOR con=1 TO LEN(MItems)
  IF !EMPTY(SUBSTR(MItems[con],143,5))
   SELECT DETALLE
   ADDREC(1)
   RECLOCK(1)
   REPLACE tcom WITH nom_tip
   REPLACE tipo WITH vtip
   REPLACE ltra WITH vltra
   REPLACE srie WITH vsrie
   REPLACE nume WITH vnum
   REPLACE fech WITH vfec
   REPLACE clie WITH vcli
   REPLACE rubr WITH SUBSTR(MItems[con],139,4)
   REPLACE arti WITH SUBSTR(MItems[con],143,5)
   REPLACE nomb WITH SUBSTR(MItems[con],1,78)
   REPLACE cant WITH VAL(SUBSTR(MItems[con],79,8))*nega  &&nega=-1 (N.C.)
   REPLACE puni WITH VAL(SUBSTR(MItems[con],87,9))
   REPLACE ptot WITH VAL(SUBSTR(MItems[con],159,12))*nega &&nega=-1 (N.C.)
   REPLACE mone WITH SUBSTR(MItems[con],148,3)
   REPLACE dola WITH VAL(SUBSTR(MItems[con],151,8))
   REPLACE baja WITH .F.
   IF conta
    REPLACE canc WITH .T.
   ELSE
    replace canc with .f.
   ENDIF
   UNLOCK
  ENDIF
 NEXT con
ENDIF vtip='1'.OR. vTip="4" .OR. vTip="5" .OR. (vtip='2' .AND. nc=0)

*Actualizo cantidad facturada del Remito utilizado
IF vTip = "1" .AND. Exi_Rem
 fe_rm=CHR(MONTH(fec_Rem)+64)+CERO(YEAR(fec_Rem),4)
 *DETALLES para devolucion
 AREA()
 IF USAR("FCD&fe_rm","REMITO")
  IF !FILE('FCD&fe_rm..NTX')
   INDEX ON clie+tcom+ltra+srie+nume TO FCD&fe_rm
  ENDIF
  SET INDEX TO FCD&Fe_Rm
 ELSE
  QUIT
 ENDIF
 SELECT REMITO
 SEEK vcli+'RTX'+vSrie+vRem
 IF FOUND()
  DO WHILE .NOT.EOF() .AND. clie=vcli .AND. tcom='RT' .AND. nume=vRem;
           .AND. ltra="X" .AND. srie=vSrie
   xArti = arti
   FOR con=1 TO LEN(MItems)
    IF !EMPTY(SUBSTR(MItems[con],143,5))
     zArti = SUBSTR(MItems[con],143,5)
     IF zArti = xArti
      RECLOCK(1)
      REPLACE cfac WITH cfac + VAL(SUBSTR(MItems[con],79,8))
      UNLOCK
     ENDIF
    ENDIF
   NEXT
   SKIP
  ENDDO
 ENDIF
 CLOSE
ENDIF

*Actualizo archivo articulos
IF !vnc_tp
 FOR con=1 TO LEN(MItems)
  IF !EMPTY(SUBSTR(MItems[con],143,5))
   SELECT ARTIC
   SET ORDER TO 8
   SEEK SUBSTR(MItems[con],143,5)
   IF FOUND()
    RECLOCK(1)
    stock=stck
    IF vtip='1' .OR. vTip ="4" .OR. vTip ="5"
     stock=stock-VAL(SUBSTR(MItems[con],79,8))
    ELSE
     IF vtip='2' .AND. nc=0
      stock=stock+VAL(SUBSTR(MItems[con],79,8))
     ENDIF
    ENDIF
    REPLACE stck WITH stock
    UNLOCK
   ENDIF
  ENDIF
 NEXT con
ENDIF

*Actualizo archivo clientes
IF vTip#"4"
 IF vcli<>'000000'
  SELECT CLIENTES
  IF vtip='2'
   vsal=vsal - tgral
  ELSE
   IF (vtip='1' .AND. conta=.F.) .OR. vtip='3'.OR. vTip ="5"
    vsal=vsal + tgral
   ENDIF
  ENDIF vtip='2'
  salsa=0
  SEEK vcli
  IF FOUND()
   RECLOCK(1)
   IF vFpag#1
    REPLACE sact WITH vsal
   ENDIF
   IF vtip='1' .OR. vTip = "5"
    REPLACE fuco WITH vfec
   ENDIF
   UNLOCK
  ENDIF
 ELSE
  *Actualizo Archivo Cliente='000000'
  AREA()
  OPEN_DBF("MAECOFIN","CONS_FIN","C","MAECOFIN","tcom+ltra+srie+nume")
  SELECT CONS_FIN
  ADDREC(0)
  RECLOCK(0)
  REPLACE tcom WITH nom_tip
  REPLACE ltra WITH vltra
  REPLACE srie WITH vsrie
  REPLACE nume WITH vnum
  REPLACE nomb WITH vnom
  REPLACE domi WITH vdom
  REPLACE civa WITH vciv
  REPLACE cuit WITH vcui
  UNLOCK
  CLOSE
 ENDIF
 COMMIT
ENDIF

*Actualizo Archivo de Facturas
SELECT FACTURA
GO BOTTOM
ADDREC(0)
RECLOCK(0)
wali=RECNO()
REPLACE tcom WITH nom_tip
REPLACE ltra WITH vltra
REPLACE srie WITH vsrie
REPLACE nume WITH vnum
REPLACE fech WITH vfec
REPLACE cpag WITH STR(vFpag,1)
REPLACE fvto WITH vFVto
REPLACE tipo WITH vtip
REPLACE lcom WITH ltra_nc
REPLACE scom WITH srie_nc
REPLACE ncom WITH fact_nc
REPLACE clie WITH vcli
REPLACE stot WITH vtot*nega
REPLACE ngra WITH vnet*nega
REPLACE prin WITH iva1
REPLACE prni WITH iva2
REPLACE civa WITH vciv
REPLACE desc WITH dsto
REPLACE ides WITH tdes*nega
REPLACE irin WITH iva*nega
REPLACE irni WITH reciva*nega
REPLACE paga WITH tgral*nega
REPLACE cont WITH conta
REPLACE nrem WITH vrem
REPLACE canc WITH .F.
REPLACE obs1 WITH mens1
REPLACE obs2 WITH mens2
REPLACE impr WITH .F.
REPLACE baja WITH .F.
COMMIT
UNLOCK

*Actualizo Archivo de Ctas.Ctes.
ok=0
SELECT CTACTE
ADDREC(0)
RECLOCK(0)
REPLACE tcom WITH nom_tip
REPLACE tipo WITH vtip
REPLACE clie WITH vcli
REPLACE ltra WITH vltra
REPLACE srie WITH vsrie
REPLACE nume WITH vnum
REPLACE nint WITH SPACE(8)      &&solo para pagos
REPLACE fech WITH vfec
REPLACE cpag WITH STR(vFpag,1)
REPLACE fvto WITH vFVto
REPLACE paga WITH tgral
IF vtip='2'
 REPLACE fpag WITH DATE()
ELSE
 REPLACE fpag WITH CTOD("  /  /    ")
ENDIF
REPLACE lcom WITH ltra_nc
REPLACE scom WITH srie_nc
REPLACE ncom WITH fact_nc
REPLACE fcom WITH fech_nc
REPLACE cont WITH conta
REPLACE baja WITH .F.
REPLACE moro WITH 'C'
IF Exi_Rem
 REPLACE nrem WITH vRem
ENDIF
IF vtip='2'
 REPLACE paga WITH tgral*(-1)
 REPLACE sact WITH 0
 REPLACE canc WITH .F.
 REPLACE comt WITH IIF(fact_nc#"********",'FA',SPACE(2))
ELSE
 IF vtip='1' .OR. vTip="5"
  REPLACE paga WITH tgral
  REPLACE sact WITH tgral      &&Factura Contado y NO paga
  REPLACE canc WITH .F.
 ELSE
  REPLACE paga WITH tgral
  IF vTip#"4"
   *Factura en Curso
    REPLACE sact WITH tgral
    REPLACE canc WITH .F.
  ELSE
   REPLACE sact WITH 0
   REPLACE canc WITH .T.
  ENDIF
 ENDIF
ENDIF vtip='2'
COMMIT
UNLOCK
vcta=RECNO()

*Actualizo factura afectada por N.C. en CTA.CTE.
IF vtip='2'
 SEEK vcli+'FA'+ltra_nc+srie_nc+fact_nc
 IF FOUND()
  RECLOCK(1)
  REPLACE sact WITH sact-tgral
  IF sact<=.1 .OR. sact=0
   REPLACE canc WITH .T.
   UNLOCK
   GO vcta
   RECLOCK(1)
   REPLACE canc WITH .T.
   UNLOCK
  ENDIF
  UNLOCK
 ENDIF
 IF vFpag = 1 .AND. reg_Pa # 0
  GO Reg_Pa
  RECLOCK(1)
  DELETE
  UNLOCK
  *aNULO EL PAGO GENERADO AUTOMATICAMENTE AL CARGAR LA FACTURA CONTADO
  *PAGOS
  AREA()
  IF USAR("PCL&periodo","PAGOS")
   IF !FILE('PCL&periodo..NTX')
    INDEX ON nume+nint TO PCL&periodo
   ENDIF
   SET INDEX TO PCL&periodo
  ELSE
   QUIT
  ENDIF
  SEEK fact_Nc + Nint_Pago
  IF FOUND()
   RECLOCK(1)
   DELETE
   UNLOCK
  ENDIF
  CLOSE
 ENDIF
ENDIF vtip='2'

IF vFpag = 1 .AND. vTip="1"
 *Pago Contado
 SELECT CTACTE
 ADDREC(0)
 RECLOCK(0)
 REPLACE tcom WITH 'PA'
 REPLACE tipo WITH '0'
 REPLACE clie WITH vcli
 REPLACE ltra WITH ' '
 REPLACE srie WITH SPACE(4)
 REPLACE nume WITH vnum
 REPLACE nint WITH CERO(VAL(vpago)+1,8)
 vPago=CERO(VAL(vpago)+1,8)
 REPLACE fech WITH vFech   &&DATE()
 REPLACE paga WITH tgral*(-1)
 REPLACE fpag WITH vFech   &&DATE()
 REPLACE lcom WITH vLtra  &&Letra comp. pagado
 REPLACE scom WITH vSrie  &&Serie comp. pagado
 REPLACE ncom WITH vNum  &&N§ comp. pagado
 REPLACE comt WITH Nom_Tip   &&Tipo comp. pagado
 REPLACE fcom WITH vFech      &&Fecha Comp.Pagado
 REPLACE cont WITH .F.
 REPLACE sact WITH 0
 REPLACE canc WITH .F.
 REPLACE baja WITH .F.
 REPLACE moro WITH 'C'
 REPLACE dias WITH 0    &&DATE()) &&Dif.de dias en el pago
 REPLACE myan WITH periodo
 UNLOCK
 *PAGOS
 AREA()
 IF USAR("PCL&periodo","PAGOS")
  IF !FILE('PCL&periodo..NTX')
   INDEX ON nume+nint TO PCL&periodo
  ENDIF
  SET INDEX TO PCL&periodo
 ELSE
  QUIT
 ENDIF
 ADDREC(0)
 RECLOCK(0)
 REPLACE tcom WITH 'PA'
 REPLACE nume WITH vnum     &&Numero de pago
 REPLACE nint WITH vpago
 REPLACE clie WITH vcli
 REPLACE fpag WITH "EF"  
 REPLACE fech WITH vFech    &&DATE()
 REPLACE ntal WITH SPACE(8)
 REPLACE impo WITH tgral
 REPLACE banc WITH SPACE(2)
 REPLACE plaz WITH SPACE(3)
 REPLACE fvto WITH CTOD("  /  /    ")
 REPLACE dola WITH 0
 UNLOCK
 CLOSE
ENDIF

IF vTip="4"
  SELECT CAB_REM
  IF !exi_rem
   ADDREC(1)
   exi_rem=.T.
  ENDIF
  RECLOCK(1)
  REPLACE nrem WITH vrem
  REPLACE fech WITH vfec
  REPLACE clie WITH vcli
  REPLACE tra1 WITH vtr1
  REPLACE tra2 WITH vtr2
  REPLACE ftr1 WITH vtr_fl1
  REPLACE ftr2 WITH vtr_fl2
  REPLACE des1 WITH vde1
  REPLACE des2 WITH vde2
  REPLACE fde1 WITH vde_fl1
  REPLACE fde2 WITH vde_fl2
  REPLACE impo WITH vnet
  REPLACE reem WITH IIF(vree$"sS",.T.,.F.)
  REPLACE nfac WITH vfac1+vfac2+vfac3
  REPLACE nopc WITH wPag
  REPLACE cpag WITH vco_pag
  REPLACE obse WITH vmens
  REPLACE orig WITH "F"
  UNLOCK
  COMMIT
ENDIF

*Actuazo archivo AUXILIAR
*ARCHIVO AUXILIAR
AREA()
IF USAR('AUXILIAR','AUX')
 GO TOP
ELSE
 QUIT
ENDIF
SELECT AUX
GO TOP
IF vTip#"4"
 IF vTip = "5"
   *Facturacion Ticket
   Ticket=CERO(VAL(Ticket)+1,8)
   RECLOCK(1)
   REPLACE ntck WITH Ticket
   UNLOCK
 ELSE
  IF vTip="1" .OR. vTip="3"
   IF vciv <= '2'.OR. xreg=1
    *Facturacion "A"
    Fac_A=vNum
    RECLOCK(1)
    REPLACE nFacA WITH Fac_A
    UNLOCK
   ELSE
    *Facturacion "B"
    Fac_B=vNum
    RECLOCK(1)
    REPLACE nFacB WITH Fac_B
    UNLOCK
   ENDIF
  ENDIF
  IF vTip="2"
   IF vciv <= '2'.OR. xreg=1
    *Nota Credito "A"
    Cre_A=vNum
    RECLOCK(1)
    REPLACE nCreA WITH Cre_A
    UNLOCK
   ELSE
    *Nota Credito "B"
    Cre_B=vNum
    RECLOCK(1)
    REPLACE nCreB WITH Cre_B
    UNLOCK
   ENDIF
  ENDIF
 ENDIF
 IF vFpag = 1
  RECLOCK(1)
  REPLACE npcl WITH vPago
  UNLOCK
 ENDIF
ELSE
  vNrem=CERO(VAL(vNrem)+1,8)
  vRem = vNrem
  RECLOCK(1)
  REPLACE nrem WITH vNrem
  UNLOCK
ENDIF
CLOSE
SELECT ARTIC
DBCOMMITALL()
Graba=.T.
RETURN

*Control de clientes
FUNCTION VAL_CLI
PARAMETERS cli,fil,col,colo_sug,lon
LOCAL colo_a,pan_ant,vMoro,Ord_Ant
IF LASTKEY()=27
 KEYBOARD CHR(27)
 RETURN .T.
ENDIF
IF PCOUNT()=1
 PRIVATE fil,col
 fil=12
 col=20
 ok1=0
ELSE
 ok1=1
ENDIF
antsel=ALLTRIM(STR(SELECT()))
colo_a=SETCOLOR()
IF ALLTRIM(cli)='*'
 *Consumidor Ocasional
 CLEAR GETS
 @FIL,COL SAY '000000'
 vnom=SPACE(30)
 vdom=SPACE(25)
 @2,1 GET vnom PICTURE '@!'
 READ
 IF LASTKEY()=27
  CLEAR GETS
  SETCOLOR(colo_a)
  KEYBOARD CHR(27)
  RETURN .T.
 ENDIF
 pan_ant=SAVESCREEN(7,0,12,79)
 SETCOLOR(color3)
 SOMBRA(8,7,10,54,'H1',1,'',color3)
 @9,8 SAY'Ingrese DOMICILIO : 'GET vdom PICTURE '@!'
 READ
 SETCOLOR(colo_a)
 RESTSCREEN(7,0,12,79,PAN_ANT)
 IF LASTKEY()=27
  CLEAR GETS
  KEYBOARD CHR(27)
  RETURN .T.
 ENDIF
 pan_ant=SAVESCREEN(9,15,19,30)
 civ=1
 SETCOLOR(color3)
 SOMBRA(9,15,17,29,'H1',1,'',color3)
 @10,16 PROMPT "1 - R.I.    "
 @11,16 PROMPT "2 - R.N.I.  "
 @12,16 PROMPT "3 - C.F.    "
 @13,16 PROMPT "4 - EXENTO  "
 @14,16 PROMPT "5 - MonoTrib"
 @15,16 PROMPT "6 - No Alca."
 @16,16 PROMPT "7 - No Categ"
 MENU TO civ
 SETCOLOR(colo_a)
 RESTSCREEN(9,15,19,30,pan_ant)
 IF LASTKEY()=27 .OR. civ=0
  CLEAR GETS
  KEYBOARD CHR(27)
  RETURN .T.
 ENDIF
 vciv=STR(civ,1)
 pan_ant=SAVESCREEN(0,1,23,63)
 DO WHILE .T.
  xTdoc=1
  IF vCiv = "3"
   xTdoc = 0
  ENDIF
  vTdoc = IIF(xTdoc=1,"CUIT","   ")
  IF xTdoc=1
   vNdoc = SPACE(11)
  ELSE
   vNdoc = SPACE(8)
  ENDIF

  SETCOLOR(color3)
  SOMBRA(18,17,20,61,'H1',1,'',color3)
  IF xTdoc=1
   @19,18 SAY 'Numero de CUIT/CUIL : 'GET vcui PICTURE'@Z 99999999999'
   READ
   IF !F_VALICUI(vCui)
    CARTEL("!!!! CUIT ERRONEO !!!! Intente Nuevamente [ OK ]",12,1,"N",color3)
    LOOP
   ENDIF
  ENDIF
  SETCOLOR(colo_a)
  RESTSCREEN(0,1,23,63,pan_ant)
  IF LASTKEY()=27
   CLEAR GETS
   KEYBOARD CHR(27)
   RETURN .T.
  ENDIF
  IF xTdoc=1
   IF !DUDA("CONFIRMA N§ CUIT",1,colo3,17,30,vCui," NO ")
    LOOP
   ELSE
    EXIT
   ENDIF
  ELSE
   EXIT
  ENDIF
 ENDDO
 *Codigo Provincia
 pan_ant=SAVESCREEN(17,15,21,63)
 DO WHILE .T.
  SETCOLOR(color3)
  SOMBRA(18,17,20,61,'H1',1,'',color3)
  @19,18 SAY`Ingrese C¢digo Provincia   : 'GET vpci PICTURE '@K 99' VALID VAL_PCIA(@vpci)
  READ
  SETCOLOR(colo_a)
  RESTSCREEN(17,15,21,63,pan_ant)
  IF LASTKEY()=27
   CLEAR GETS
   KEYBOARD CHR(27)
   RETURN .T.
  ENDIF
  IF !DUDA("CONFIRMA PROVINCIA",1,colo3,17,30," SI "," NO ")
   LOOP
  ELSE
   EXIT
  ENDIF
 ENDDO
 @2,34 SAY IIF(vciv='1','R.I. ',IIF(vciv='2','R.N.I',IIF(vciv='3','C.F. ',IIF(vciv='4','EXEN.',IIF(vciv='5','M.T. ',;
 IIF(vciv='6','N.A.',IIF(vciv='7','N.C.',SPACE(5))))))))
 vcli='000000'
 @2,1 SAY vCli+" "+SUBSTR(vNom,1,25) COLOR color9
 conta=.T.
 RETURN .T.
ENDIF
*Cliente Normal
SELECT CLIENTES
SET ORDER TO 1
cli=CERO(VAL(cli),6)
vcli=cli
SEEK cli
IF .NOT. FOUND()
 pan_ant=savescreen(4,2,24,72)
 SETCOLOR(color3)
 SOMBRA(4,2,22,71,'V1',3,'',color3)
 @6,2 say'ÌÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹'
 @19,2 say'ÇÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¶'
 @16,2 say'ÌÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹'
 SETCOLOR("I")
 @4,38 say' BUSCAR '
 SETCOLOR(color3)
 @17,3 SAY'BUSCAR:'
 @18,3 SAY SPACE(38)
 @20,13 say chr(24)+chr(25)+' PgUp PgDn Mover         '+chr(17)+'ÄÙ Acepta'
 @21,13 say'F2-Cambia Clave Busqueda  [Esc]-Salir'
 DECLARE cam[3],sep[3],ind[2],lar[2],nom[2]
 carga1(cam,"clie~nomb~domi")
 tit='Numero  Nombre  Cliente                Domicilio                           '
 carga2(sep,'5,2,0')
 ind[1]=1
 lar[1]=6
 nom[1]='Codigo cliente : '
 ind[2]=2
 lar[2]=30
 nom[2]='Nombre del Cliente : '
 ord_ant=STR(INDEXORD())
 SET ORDER TO 2
 GO TOP
 BUSCARE(5,4,15,70,cam,tit,sep,ind,lar,18,4,nom,17,14)
 regis=RECNO()
 SETCOLOR(colo_a)
 RESTSCREEN(4,2,24,72,pan_ant)
 SET ORDER TO &ord_ant
 IF LASTKEY()<>27
  vcli=clie
  vnom=nomb
  vdom=domi
  xTdoc=1 &&CUIT
  vcui=cuit
  vciv=civa
  IF vCiv = "3"
   xTdoc = 0
   vCui  = SPACE(11)
  ELSE
   IF !F_VALICUI(vCui)
     CARTEL("!!! NUMERO CUIT ERRONEO !!! VERIFIQUE EN MAESTRO [ OK ]",12,1,'N','*'+color3)
     SELECT &antsel
     RETURN .F.
   ENDIF
  ENDIF
  vCpos=cpos
  vTele=tele
  vEmai=emai
  vcct=tcli
  vMoro=moro
  vNdoc=ndoc
  vTdoc=tdoc
  IF vcct='2'
   conta=.T.
  ELSE
   conta=.F.
  ENDIF
  vsal=sact
  vadju=acre
  vpci=pcia
  vdpt=dpto
  regcli=RECNO()
  IF ok1=1
   SETCOLOR(colo_sug)
   @fil,col SAY vcli+'& '+SUBSTR(vnom,1,lon)
   @2,34 SAY IIF(vciv='1','R.I. ',IIF(vciv='2','R.N.I',IIF(vciv='3','C.F. ',IIF(vciv='4','EXEN.',IIF(vciv='5','M.T. ',;
   IIF(vciv='6','N.A.',IIF(vciv='7','N.C.',SPACE(5))))))))
   @2,39 SAY IIF(vcct='1','C.C',IIF(vciv='2','CDO',SPACE(3)))
   SETCOLOR(colo_a)
  ENDIF
  SELECT &antsel
  IF vMoro="I"
   CARTEL("CLIENTE INACTIVO... [ OK ]",12,1,'N','*'+color3)
   RETURN .F.
  ENDIF
  IF vciv='2'
   CARTEL("NO FACTURE A R.N.I. [ OK ]",12,1,'N',color3)
   RETURN .F.
  ENDIF
  RETURN .T.
 ELSE
  SELECT &antsel
  RETURN .F.
 ENDIF
ELSE
 vnom=nomb
 vdom=domi
 xTdoc=1  &&CUIT
 vcui=cuit
 vciv=civa
 IF vCiv = "3"
  xTdoc = 0
  vCui  = SPACE(11)
 ELSE
  IF !F_VALICUI(vCui)
    CARTEL("!!! NUMERO CUIT ERRONEO !!! VERIFIQUE EN MAESTRO [ OK ]",12,1,'N','*'+color3)
    SELECT &antsel
    RETURN .F.
  ENDIF
 ENDIF
 vCpos=cpos
 vTele=tele
 vEmai=emai
 vcct=tcli
 vsal=sact
 vadju=acre
 vpci=pcia
 vMoro=moro
 vNdoc=ndoc
 vTdoc=tdoc
 vdpt=dpto
 regcli=RECNO()
 IF ok1=1
  SETCOLOR(colo_sug)
  @fil,col SAY vcli+' '+SUBSTR(vnom,1,lon)
  @2,34 SAY IIF(vciv='1','R.I. ',IIF(vciv='2','R.N.I',IIF(vciv='3','C.F. ',IIF(vciv='4','EXEN.',IIF(vciv='5','M.T. ',;
  IIF(vciv='6','N.A.',IIF(vciv='7','N.C.',SPACE(5))))))))
  @2,39 SAY IIF(vcct='1','C.C',IIF(vciv='2','CDO',SPACE(3)))
  SETCOLOR(colo_a)
 ENDIF
 SELECT &antsel
ENDIF
IF vMoro="I"
 CARTEL("CLIENTE INACTIVO... [ OK ]",12,1,'N','*'+color3)
 RETURN .F.
ENDIF
IF vciv='2'
 CARTEL("NO FACTURE A R.N.I. [ OK ]",12,1,'N',color3)
 RETURN .F.
ENDIF
RETURN .T.

*Control de PROVINCIAS
FUNCTION VAL_PCIA
PARAMETERS fPcia,fpb1,fpb2,fpb3,fpb4,fR122,fExto,f122IMin
LOCAL colo_a,ok1,antsel
IF LASTKEY()=27
 KEYBOARD CHR(27)
 RETURN .T.
ENDIF
antsel=ALLTRIM(STR(SELECT()))
*Provincias
AREA()
OPEN_DBF("MAEPROVI","PCIAS","C","MAEPROVI","pcia","NOMPROVI","nomb")
colo_a=SETCOLOR()
SELECT PCIAS
SET ORDER TO 1
fPcia=CERO(VAL(fPcia),2)
SEEK fPcia
IF .NOT. FOUND()
 *Ayuda de Pcias
 archivo='PCIAS'
 campi='pcia~nomb'
 ntx1=1
 ntx2=2
 l1=2
 l2=15
 camp1='pcia'
 IF ayudo(campi,ntx1,ntx2,l1,l2,camp1,archivo) = .T.
  fPcia=pcia
  CLOSE
  SELECT &antsel
  RETURN .T.
 ELSE
  CLOSE
  SELECT &antsel
  RETURN .F.
 ENDIF
ELSE
 fPcia=pcia
 CLOSE
 SELECT &antsel
 RETURN .T.
ENDIF
CLOSE
RETURN .T.

FUNCTION VAL_CAN
PARA can,tip
IF can=0
 RETURN .F.
ENDIF
IF tip='1'
 IF can>vsck
   CARTEL("NO SUPERE STOCK ACTUAL "+PUNTO(vsck,8)+" [ OK ]",20,1,'N',color3)
 ENDIF
ENDIF
RETURN .T.

FUNCTION VAL_TIP
PARAMETERS tip,bcli
LOCAL papan,colo_ant,are_ant
ok=0
IF LASTKEY()=27
 KEYBOARD CHR(27)
 RETURN .T.
ENDIF
are_ant=ALLTRIM(STR(SELECT()))
colo_ant=SETCOLOR()
SETCOLOR(color3)
papan=SAVESCREEN(9,14,16,30)
IF tip<>'1' .AND. tip<>'2' .AND. tip<>'3'.AND. tip<>'4'  &&.AND. tip<>'5'
 SOMBRA(9,14,14,29,'H1',1,'',color3)
 ti=1
 @10,16 PROMPT "1- FACTURA  "
 @11,16 PROMPT "2- N.CREDITO"
 @12,16 PROMPT "3- N.DEBITO "
 @13,16 PROMPT "4- REMITO   "
 *@14,16 PROMPT "5- TICKET   "
 MENU TO ti
 SETCOLOR(colo_ant)
 IF LASTKEY()=27
  CLEAR GETS
  KEYBOARD CHR(27)
  RESTSCREEN(9,14,16,30,papan)
  RETURN .T.
 ENDIF
 vtip=STR(ti,1)
ELSE
 vtip=tip
ENDIF
SETCOLOR(colo_ant)
RESTSCREEN(9,14,16,30,papan)
nom_tip=IIF(vtip='1','FA',IIF(vtip='2','NC',IIF(vTip='3','ND',IIF(vTip='4','RT','TK'))))
@ 3,45 SAY vtip+' '+nom_tip
IF vTip#'4'
 IF vTip="5"
  reg = VAL(Ticket)+1
 ELSE
  *Facturas
  IF vTip="1" .OR. vTip="3"
   IF vciv <= '2'
    reg=VAL(Fac_A)+1
   ELSE
    reg=VAL(Fac_B)+1
   ENDIF
  ENDIF
  *N.Creditos
  IF vTip="2"
   IF vciv <= '2'
    reg=VAL(Cre_A)+1
   ELSE
    reg=VAL(Cre_B)+1
   ENDIF
  ENDIF
 ENDIF
ELSE
 reg=VAL(vNRem)+1
 vRem = CERO(reg,8)
 DO MOVDATRE
 RETURN .T.
ENDIF
xreg=0
IF vciv='4'.AND.(vtip='1'.OR.vTip="2")
 bien=CARTEL("FACTURA EN: [ A ] [ B]",12,2,'N',color3)
 IF LASTKEY()#27.AND.bien
  reg=VAL(Fac_A)+1
  xreg=1
 ENDIF
ENDIF
IF vtip='2'
 IF vciv='2'
  *Se utiliza ESPECIAL cuando hacemos una N.C. por el Importe del IVA RNI cobrado en FAC
  vnc_tp=DUDA("TIPO DE N.CREDITO",2,colo3,12,30," ESPECIAL "," NORMAL ")
  IF LASTKEY()=27
   CLEAR GETS
   KEYBOARD CHR(27)
   RESTSCREEN(9,14,14,30,papan)
   RETURN .T.
  ENDIF
 ENDIF
 SETCOLOR(color3)
 PAPAN=SAVESCREEN(8,25,17,58)
 SOMBRA(8,25,16,56,'H1',1,'',color3)
 CLEAR GETS
 DO WHILE .T.
  SETCOLOR(color3)
  SELECT CTACTE
  factnc=0
  ltra_nc=' '
  srie_nc=SPACE(4)
  @15,26 SAY CENTRA(`[Esc] para Salir',30)
  @09,27 SAY "Letra y Serie : " GET ltra_nc PICTURE "!"
  @09,46 GET srie_nc PICTURE "@K 9999"
  @10,27 SAY "Sobre Factura : " GET factnc PICTURE`@K 99999999'
  READ
  IF LASTKEY()<>27
   IF vnc_tp .AND. factnc=0
    CARTEL("DEBE INGRESAR N§FACTURA PARA N.CREDITO ESPECIAL   [ OK ]",20,1,'N',color3)
    LOOP
   ENDIF
   IF factnc#0
    srie_nc=CERO(VAL(srie_nc),4)
    fact_nc=CERO(factnc,8)
    @9,44 SAY ltra_nc+'-'+srie_nc
    @10,44 SAY fact_nc

    bno=0
    IF vciv<='2'.AND.ltra_nc#'A'
     bno=1
    ELSE
     IF vciv>'2'.AND.ltra_nc#'B'.AND.xReg=0
      bno=1
     ENDIF
    ENDIF
    IF bno=1
     CARTEL("LETRA DE LA N.C. DEBE SER IGUAL QUE LA FACTURA [ OK ]",12,1,'N',color3)
     LOOP
    ENDIF
    cla=bcli+"FA"+ltra_nc+srie_nc+fact_nc
    SEEK cla
    IF !FOUND()
     CARTEL('NO ENCUENTRO ESA FACTURA  [ ok ]',18,1,'N',color3)
     LOOP
    ELSE
     reg_fac=RECNO()
     fech_nc=fech
     fe_nc=CHR(VAL(SUBSTR(DTOC(fech_nc),4,2))+64)+RIGHT(DTOC(fech_nc),4)
     @12,27 SAY`Fecha Factura : '+DTOC(fech)
     @14,27 SAY`Importe ..... : '+STR(sact,12,2)
     IF cont
      CARTEL('NO PUEDE ACREDITAR O DEBITAR DE UNA FACTURA DE CONTADO [ OK ]',18,1,'N',color3)
      GO REG_FAC+1
      IF tcom="PA" .AND. nume = fact_nc
       reg_Pa = RECNO()
       Nint_Pago = nint
      ENDIF
      GO Reg_fac
      *LOOP
     ENDIF
     IF canc .OR. sact<=.01
      CARTEL('FACTURA YA CANCELADA  [ OK ]',18,1,'N',color3)
      IF !cont
       LOOP
      ENDIF
     ENDIF
    ENDIF
   ELSE
    CARTEL('DEBE INGRESAR N§ FACTURA  [ OK ]',18,1,'N',color3)
    LOOP
   ENDIF
   IF CARTEL('FACTURA CORRECTA ? [ SI ] [ NO ]',16,1,'N',colo3)
    RESTSCREEN(8,25,17,58,PAPAN)
    IF vtip='2' .AND. fact_nc#"********".AND.!vnc_tp
     *IF CARTEL('NOTA DE CREDITO POR : [ DEVOLUCION ] [ PAGO ]',16,1,'N',colo3)
     * nc=0
     *ELSE
      nc=1
     *ENDIF
    ELSE
     IF vnc_tp
      nc=1
      vimp_rni=0
      DO WHILE .T.
       SETCOLOR(color1)
       @22,54 GET vimp_rni PICTURE "@K 999999999.99" VALID vimp_rni <>0
       READ
       IF LASTKEY()=27
        CLEAR GETS
        KEYBOARD CHR(27)
        RESTSCREEN(9,14,14,30,papan)
        RETURN .T.
       ENDIF
       EXIT
      ENDDO
     ENDIF
    ENDIF
    IF fact_nc#"********"
     IF cont
      imp_Nc = paga
     ELSE
      imp_nc=sact        &&Saldo Factura
     ENDIF
    ENDIF
    VAL_CLI(vcli,2,1,color1,25)
    IF nc=1
     SETCOLOR(colo_ant)
     ASIZE(MItems,0)
     RETURN .T.
    ENDIF
    IF vtip='2' .AND. !vnc_tp
     *DETALLES para devolucion
     AREA()
     IF USAR("FCD&fe_nc","DEVOLU")
      IF !FILE('FCD&fe_nc..NTX')
       INDEX ON clie+tcom+ltra+srie+nume TO FCD&fe_nc
      ENDIF
      SET INDEX TO FCD&Fe_Nc
     ELSE
      QUIT
     ENDIF
     SELECT DEVOLU
     ASIZE(MItems,0)
     SEEK vcli+'FA'+ltra_nc+srie_nc+fact_nc
     DO WHILE .NOT.EOF() .AND. clie=vcli .AND. tcom='FA' .AND. nume=fact_nc;
              .AND. ltra=ltra_nc .AND. srie=srie_nc
      AADD(MItems,SPACE(171))
      MItems[LEN(MItems)]=STUFF(MItems[LEN(MItems)],1,78,nomb)
      MItems[LEN(MItems)]=STUFF(MItems[LEN(MItems)],143,5,arti)
      MItems[LEN(MItems)]=STUFF(MItems[LEN(MItems)],139,4,rubr)
      MItems[LEN(MItems)]=STUFF(MItems[LEN(MItems)],79,8,TRANSFORM(cant,'@Z 99999.99'))
      MItems[LEN(MItems)]=STUFF(MItems[LEN(MItems)],87,9,TRANSFORM(puni,'@Z 99999.999'))
      MItems[LEN(MItems)]=STUFF(MItems[LEN(MItems)],148,3,mone)
      MItems[LEN(MItems)]=STUFF(MItems[LEN(MItems)],151,8,TRANSFORM(dola,'@Z 999.9999'))
      MItems[LEN(MItems)]=STUFF(MItems[LEN(MItems)],159,12,TRANSFORM(ptot,'@Z 999999999.99'))
      art=arti
      SELECT ARTIC
      SEEK art
      IF FOUND()
       vMrca = mrca
       MItems[LEN(MItems)]=STUFF(MItems[LEN(MItems)],121,3,mrca)
       MItems[LEN(MItems)]=STUFF(MItems[LEN(MItems)],96,10,unid)
       MItems[LEN(MItems)]=STUFF(MItems[LEN(MItems)],106,15,medi)
       SELECT MARCA
       SEEK vMrca
       IF FOUND()
        MItems[LEN(MItems)]=STUFF(MItems[LEN(MItems)],124,15,nomb)
       ENDIF
      ENDIF
      SELECT DEVOLU
      SKIP
     ENDDO
     SELECT DEVOLU
     CLOSE
    ENDIF
    SETCOLOR(colo_ant)
    RETURN .T.
   ELSE
    LOOP
   ENDIF
  ELSE
   RESTSCREEN(8,25,17,58,papan)
   SETCOLOR(colo_ant)
   RETURN .F.
  ENDIF
 ENDDO
ENDIF
RETURN .T.

PROCEDURE LEER_OBSE
PRIVATE are_ant
are_ant=ALLTRIM(STR(SELECT()))
*Descripciones de Clientes
AREA()
OPEN_DBF("MAEOBSCL","OBSCLI","E","MAEOBSCL","clie")
SEEK vcli
IF FOUND()
 mens1=obs1
 mens2=obs2
ENDIF
CLOSE
SELECT &are_ant
RETURN

PROCEDURE IMPR_REMI
SET CENTURY OFF
pagina=0
?? CHR(27)+CHR(120)+CHR(0)+CHR(18)
?? CHR(27)+CHR(67)+CHR(48)      &&Longitud de Remito 48 filas
?? CHR(27)+CHR(33)+CHR(5)
vbul="01"
amor=0
con=1
ok=0
*Total de lineas a imprimir
lin_ctas=12     &&CTAS_LIN()
*Calcula lineas por hoja, para controlar el salto de hoja
IF lin_ctas>=19 .AND. lin_ctas<=25
 Lon_Rem=lin_ctas-3
ELSE
 IF lin_ctas>25
  Lon_Rem=25
 ELSE
  Lon_Rem=18
 ENDIF
ENDIF

 IF SELECT("PROVIN")=0
  AREA()
  *PROVINCIAS
  AREA()
  OPEN_DBF("MAEPROVI","PROVIN","C","MAEPROVI","pcia")
  SEEK vPci
  IF FOUND()
   provi=ALLTRIM(nomb)
  ELSE
   provi='DESCONOCIDA'
  ENDIF
 ELSE
  SELECT CLIENTES
  GO regcli
  vPci = pcia
  vDpt = dpto
  vdom=domi
  vcui=cuit
  IF !F_VALICUI(vCui)
    CARTEL("!!! NUMERO CUIT ERRONEO !!! VERIFIQUE EN MAESTRO [ OK ]",12,1,'N','*'+color3)
    SELECT &antsel
    RETURN .F.
  ENDIF
  SELECT PROVIN
  SEEK vPci
  IF FOUND()
   provi=ALLTRIM(nomb)
  ELSE
   provi='DESCONOCIDA'
  ENDIF

 ENDIF

 IF SELECT("DEPARTA")=0
  *DEPARTAMENTOS
  OPEN_DBF("MAEDEPAR","DEPARTA","C","MAEDEPAR","dpto")
  SEEK vDpt
  IF FOUND()
   ciu=ALLTRIM(nomb)
  ELSE
   ciu='DESCONOCIDA'
  ENDIF
  CLOSE
 ELSE
  SELECT DEPARTA
  SEEK vDpt
  IF FOUND()
   ciu=ALLTRIM(nomb)
  ELSE
   ciu='DESCONOCIDA'
  ENDIF
  CLOSE
 ENDIF
  vLoc = ALLTRIM(ciu)+"-"+ALLTRIM(provi)
DO WHILE INKEY(.1)#27
 *dibujar encabezado
 IF amor=0
  NORMA()
  ? CHR(15)+SPACE(49)+CHR(18)+SPACE(31)+DTOC(vfec)
  ?? CHR(15)
  ?
  ?
  ?
  ? ' '+CHR(18)
  GRAN_ON()
  ? SPACE(3)+"("+vCli+")"+vnom
  GRAN_OFF()
  NORMA()
  ? SPACE(7)+vdom+' Local.: '+vloc
  ? IIF(EMPTY(vciv),'',IIF(vciv='1',SPACE(7),IIF(vciv='2',SPACE(21),IIF(vciv='3','C.F.:  ',IIF(vciv='4',SPACE(15)+'Exe.:  ',SPACE(15)+'M.T.:  '))))+"±±")
  vspace= IIF(EMPTY(vciv),SPACE(40),IIF(vciv='1',SPACE(33),IIF(vciv='2',SPACE(19),IIF(vciv='3',SPACE(33),SPACE(18)))))
  ?? vspace+vcui+'  N§Dep.: '  &&+A_Mano
  CHICO()
  ? SPACE(89)+'Facturas: '+vfac1+'-'+vfac2+'-'+vfac3
  NORMA()
  amor=1
  hor=0
  ?
  ?
 ENDIF
 IF con>1
  CHICO()
  ? SPACE(78)+"Viene de Remito Anterior....."
  ?
  NORMA()
  hor=hor+2
 ENDIF
 DO WHILE INKEY(.1)#27 .AND. hor<=Lon_Rem .AND. con<=LEN(campos)
  *Imprimir detalle
  ? SPACE(3)+SUBSTR(campos[con],40,7)+SPACE(4)+UPPER(SUBSTR(vDesc[con],1,37))+"   "+SUBSTR(campos[con],79,5)
  hor=hor+1
  con=con+1
  IF con>LEN(campos)
   EXIT
  ENDIF
 ENDDO
 IF con>=LEN(campos)
  *Imprimir detalles al pie
  ?
  CHICO()
  ? SPACE(23)+SUB_ON()+"TRANSPORTE"+SUB_OFF()+": "+vtr1+SPACE(5)+SUB_ON()+"REDESPACHO"+SUB_OFF()+": "+vde1
  ? SPACE(23)+SPACE(12)+vtr2+SPACE(17)+vde2
  ? SPACE(23)+SUB_ON()+"FLETE Tran"+SUB_OFF()+": "+vtr_fl1+SPACE(5)+SUB_ON()+"FLETE Red."+SUB_OFF()+": "+vde_fl1
  ? SPACE(23)+SPACE(12)+vtr_fl2+SPACE(17)+vde_fl2
  ? SPACE(23)+SUB_ON()+IIF(vree="S","IMPORTE  $","VALOR    $")+SUB_OFF()+": "+STR(vNet,12,2)+SPACE(23)+SUB_ON()+"C./PAGO   "+SUB_OFF()+": "+vco_pag
  ?
  GRAN_ON()
  ? IIF(vree="S",SPACE(7)+"CONTRA REEMBOLSO    -   ",SPACE(12))+vmens
  *---------------
  *Mensaje Especial al Pie del Remito
  vMen_Pie = .F.
  IF vmen_pie
   ?
   FOR pie=1 TO 5
    st=STR(pie,1)
    IF !EMPTY(pie&st)
     ? SPACE(3)+pie&st
    ENDIF
   NEXT
  ENDIF
  ?
  ? SPACE(3)+mens1
  ? SPACE(3)+mens2
  *--------------
  GRAN_OFF()
  NORMA()
  EXIT
 ELSE
  amor=0
  CHICO()
  ? SPACE(78)+"Sigue en otro Remito......"
  NORMA()
  EJECT
 ENDIF
ENDDO
?
EJECT
?? CHR(27)+CHR(33)+CHR(8)
?? CHR(27)+CHR(67)+CHR(72)      &&Devuelve Longitud de pagina 72 filas
SET CURSOR ON
SET CENTURY ON
RETURN

FUNCTION BUS_REMI(fNrem)
fNRem = CERO(VAL(fNRem),8)
SELECT CAB_REM
SEEK fNRem
IF FOUND()
 Bien = CARTEL("EL REMITO YA EXISTE, LO UTILIZA [ SI ] [ NO ]",16,1,'N',colo3)
 IF !Bien .OR.LASTKEY()=27
  RETURN .T.
 ENDIF
 Exi_Rem=.T.
 fec_Rem = fech
 fFech = fech

 fe_rm=CHR(MONTH(fFech)+64)+CERO(YEAR(fFech),4)
 *DETALLES para devolucion
 AREA()
 IF USAR("FCD&fe_rm","REMITO")
  IF !FILE('FCD&fe_rm..NTX')
   INDEX ON clie+tcom+ltra+srie+nume TO FCD&fe_rm
  ENDIF
  SET INDEX TO FCD&Fe_Rm
 ELSE
  Exi_rem = .F.
  fec_Rem=CTOD("  /   /    ")
  RETURN .T.
 ENDIF
 SELECT REMITO
 SEEK vcli+'RTX'+vSrie+fNrem
 pp=0
 DO WHILE .NOT.EOF() .AND. clie=vcli .AND. tcom='RT' .AND. nume=fNrem;
          .AND. ltra="X" .AND. srie=vSrie
  IF cfac < cant
   pp++
   AADD(MItems,SPACE(171))
   MItems[LEN(MItems)]=STUFF(MItems[LEN(MItems)],1,78,nomb)
   MItems[LEN(MItems)]=STUFF(MItems[LEN(MItems)],143,5,arti)
   MItems[LEN(MItems)]=STUFF(MItems[LEN(MItems)],139,4,rubr)
   MItems[LEN(MItems)]=STUFF(MItems[LEN(MItems)],79,8,TRANSFORM(cant-cfac,'@Z 99999.99'))
   MItems[LEN(MItems)]=STUFF(MItems[LEN(MItems)],87,9,TRANSFORM(puni,'@Z 99999.999'))
   MItems[LEN(MItems)]=STUFF(MItems[LEN(MItems)],148,3,mone)
   MItems[LEN(MItems)]=STUFF(MItems[LEN(MItems)],151,8,TRANSFORM(dola,'@Z 999.9999'))
   MItems[LEN(MItems)]=STUFF(MItems[LEN(MItems)],159,12,TRANSFORM(ptot,'@Z 999999999.99'))
   art=arti
   SELECT ARTIC
   SEEK art
   IF FOUND()
    vMrca = mrca
    MItems[LEN(MItems)]=STUFF(MItems[LEN(MItems)],121,3,mrca)
    MItems[LEN(MItems)]=STUFF(MItems[LEN(MItems)],96,10,unid)
    MItems[LEN(MItems)]=STUFF(MItems[LEN(MItems)],106,15,medi)
    SELECT MARCA
    SEEK vMrca
    IF FOUND()
     MItems[LEN(MItems)]=STUFF(MItems[LEN(MItems)],124,15,nomb)
    ENDIF
   ENDIF
  ENDIF
  SELECT REMITO
  SKIP
 ENDDO
 SELECT REMITO
 CLOSE
 IF pp=0
  CARTEL("EL REMITO NO TIENE ARTICULOS PENDIENTES [ OK ]",16,1,'N',colo3)
  Exi_Rem=.F.
  fec_Rem=CTOD("  /   /    ")
  RETURN .F.
 ENDIF
ENDIF
RETURN .T.

*Pantalla
PROCEDURE PAN_CONSU1
SETCOLOR(color9)
SOMBRA(1,0,24,79,'V5',3,'',colo3)
@1,10 SAY CENTRA("====== CONSULTA ARTICULOS ======  "+DTOC(DATE())+"  1 usd = $"+STR(vDolar,8,4),60," ") COLOR colo12
@ 2,0 SAY "ÚRubr¿ÚLista de PreciosÄÄÄÄÄAlt-LÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿ÚMarcasÄÄAlt-MÄÄ¿ÚAtributosÄÄÄÄ¿"
@ 3,0 SAY "ÃC¢d.´ÃDescripci¢nÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´ÃDetalleÄÄÄÄÄÄÄÄ´ÃÄÄÄÄUnidadÄÄÄ´"
@ 4,0 SAY "³    ³³                                        ³³               ³³             ³"
@ 5,0 SAY "³    ³³                                        ³³               ³ÃÄÄForma Vta.Ä´"
@ 6,0 SAY "³    ³³                                        ³³               ³³             ³"
@ 7,0 SAY "³    ³³                                        ³³               ³ÃÄUnid.FVta.ÄÄ´"
@ 8,0 SAY "ÃÄÄÄÄÁÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ³             ³"
@ 9,0 SAY "ÃDescripci¢nÄ(                              )ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄ´"
@10,0 SAY "³                                                                              ³"
@11,0 SAY "³                                                                              ³"
@12,0 SAY "³                                                                              ³"
@13,0 SAY "³                                                                              ³"
@14,0 SAY "³                                                                              ³"
@15,0 SAY "³                                                                              ³"
@16,0 SAY "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´"
@17,0 SAY "³Medi.:               ³Color:          ³Calidad:         ³Util.En:             ³"
@18,0 SAY "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂP.Un.c/IVA (ÄÄÄ)ÄÄ(  /  /    )ÄÄÄÄÄÄ´Dureza :             ³"
@19,0 SAY "ÚCod.ProveedorÄÄÄÄÄÄÅContadoÄÄÄÄÂa 30 d¡asÄÄÂa 60 d¡asÄÄÄ´Referen:             ³"
@20,0 SAY "³                   ³           ³           ³            ³m2xCaja:             ³"
@21,0 SAY "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´"
@22,0 SAY "³Buscar:                                                                       ³"
@23,0 SAY "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
DIST_COLOR(2,28,'~*RG+/BG~Alt-L')
DIST_COLOR(2,57,'~*RG+/BG~Alt-M')
DIST_COLOR(18,21,'~*RG+/BG~P.Un.c/IVA')
RETURN

FUNCTION BUS_ART2
PARAMETERS fs,ci,fi,cd,campo,titulo,separacion,indice,largo,fi_ed,co_ed,nombre,fi_no,co_no
PRIVATE cant_camp,reg_ant,camp_dbe[1],titu_dbe[1],punt_ind,BUSCA,col_act
cant_camp = LEN(campo)
SELECT ARTIC
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
DBEDIT(fs,ci,fi,cd,camp_dbe,'FUN_BUS2','',titu_dbe,'','','')
IF agg=5
 SETCOLOR(col_act)
 RETURN .F.
ENDIF
SETCOLOR(col_act)
IF LASTKEY() = 27
 GO reg_ant
ENDIF
RETURN(.T.)

FUNCTION FUN_BUS2
PARAMETERS modo
PRIVATE tecla,fFil,fCol
fFil=ROW()
fCol=COL()
tecla = LASTKEY()
DO CASE
CASE modo = 0
 DO VIEW_ART2
 RETURN(1)

CASE modo = 1 .OR. modo = 2
 TONE(600,2)
 DO VIEW_ART2
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
  DO VERPRE2
  SET CURSOR OFF
  com = 1
  rel = 0
  SETCOLOR(color1)
  KEYBOARD CHR(24)+CHR(5)
  DO WHILE .T.
   com = ACHOICE(7,1,13,77,MItems,.T.,"ver_art2",com,rel)
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
  RETURN(1)
 ENDIF
 IF tecla=13
  *Agregar
  *Resalta Nombre de Articulo elegido
  @fFil,fCol SAY nomb COLOR color8
  IF vciv='3'.OR.(vciv > '4')
   xPuCd=pucd
   xPu30=xPucd*vPu30
   xPu60=xPucd*vPu60
  ELSE
   *DIST_COLOR(18,21,'~*RG+/B~P.Un.S/IVA')
   xPuCd=pucd    &&/((iva1+100)/100)
   xPu30=xPucd*vPu30
   xPu60=xPucd*vPu60
  ENDIF
  xPuni=0
  xcant=0
  SET CURSOR ON
  Colu=SETCOLOR()
  IF vFpag = 1
   *Contado
   @19,21 SAY "ContadoÄÄÄ" COLOR color9
   xPuni = xPuCd
  ELSE
   IF vFpag = 2
    *CtaCte 30 dias
    @19,21 SAY "a 30 diasÄ" COLOR color9
    xPuni = xPu30
   ELSE
    *CtaCte 60 dias
    @19,21 SAY "a 60 diasÄ" COLOR color9
    xPuni = xPu60
   ENDIF
  ENDIF
  @19,33 SAY "CantidadÄÄ" COLOR color9
  SETCOLOR(color1)
  @20,22 GET xPuni PICTURE "@K 99999.999"
  @20,35 GET xCant PICTURE "@K 99999.99"
  READ
  SETCOLOR(Colu)
   DIST_COLOR(18,21,'~*RG+/BG~P.Un.C/IVA')
  IF LASTKEY()=27
   @19,21 SAY "ContadoÄÄÄ" COLOR color9
   @19,33 SAY "a 30 D¡as" COLOR color9
   SET CURSOR OFF
   RETURN(1)
  ENDIF
  IF xCant>0
   Bien=CARTEL("ESTA SEGURO DE AGREGAR ??? [ SI ] [ NO ]",12,1,'N',colo3)
   @19,21 SAY "ContadoÄÄÄ" COLOR color9
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

   xPuni = IIF(Mone="U$S".OR.Mone="USD",xPuni*xDolar,xPuni)

   *Agregar a la matriz
   IF vCiv="1" .OR. vCiv="2"
     xPuni = xPuni/((iva1+100)/100)
   ENDIF

   AADD(MItems,nomb+STR(xCant,8,2)+STR(xPuni,9,3)+unid+medi+mrca+MARCA->nomb+rubr+arti+mone+IIF(mone="U$S".OR.mone="USD",STR(xDolar,8,4),STR(1,8,4))+;
    STR(xPuni*xCant,12,2))
   SET CURSOR OFF
   Busca=""
   @fi_ed,co_ed SAY SPACE(50)
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
 DO VIEW_ART2
 RETURN(2)

CASE tiene_ind .and. tecla = 8 .and. LEN(BUSCA) > 0
 BUSCA = SUBSTR(BUSCA,1,LEN(BUSCA)-1)
 @fi_ed,co_ed SAY BUSCA+SPACE(largo[punt_ind]-LEN(BUSCA))
 SETCOLOR("*"+col_act)
 @fi_ed,co_ed+LEN(BUSCA) SAY '_ '
 SETCOLOR(col_act)
 DO VIEW_ART2
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
 DO VIEW_ART2
 RETURN(1)

OTHERWISE
 DO VIEW_ART2
 RETURN(1)
ENDCASE

PROCEDURE VERPRE2
RESTSCREEN(,,,,Pan_Cabe)
SETCOLOR(color3)
@ 24,0 SAY "±*±-Guardar Comprobante  ±Del±-Borrar Item ±Esc±-Volver a Selecci¢n                    "
DIST_COLOR(24,1,'~W+/R~*')
DIST_COLOR(24,26,'~W+/R~Del')
DIST_COLOR(24,44,'~W+/R~Esc')
SETCOLOR(colo3)
RETURN

FUNCTION ver_art2
PARAMETERS MOD,sub,pos
PRIVATE tcl,fFil,fCol,pus,oriy,con,ydes
fFil=ROW()
fCol=COL()
IF LEN(MItems) < 7
 ydes = LEN(MItems)
ELSE
 ydes = 7
ENDIF
IF ydes = 1
 @7,0 SAY ''
ELSE
 pus = ydes/LEN(MItems)*sub
 FOR oriy = 1 TO ydes
    @6+oriy,0 SAY IF(pus <= oriy .and. pus >= oriy-.99999,IIF(sub=1,'',IIF(sub=LEN(MItems),'','')),'³')
 NEXT oriy
ENDIF
tcl = LASTKEY()
*@11,72 SAY "Dolar: "
*@12,70 SAY STR(xDolar,8,4) COLOR colo12
*Imprime detalles del Articulo
@16,2 SAY SUBSTR(MItems[sub],139,4)
@16,7 SAY SUBSTR(MItems[sub],124,15)
@16,23 SAY SUBSTR(MItems[sub],106,15)
@16,39 SAY SUBSTR(MItems[sub],96,10)
@16,50 SAY SUBSTR(MItems[sub],79,8)
@16,59 SAY SUBSTR(MItems[sub],87,9)
vObse = SUBSTR(MItems[sub],1,78)
xPuni=VAL(SUBSTR(MItems[sub],87,9))
xcant=VAL(SUBSTR(MItems[sub],79,8))
xMone=SUBSTR(MItems[sub],148,3)
@17,59 SAY "("+xMone+")" COLOR color3
xTotal=0
FOR con=1 TO LEN(MItems)
 fMone=SUBSTR(MItems[con],148,3)
 fPuni=VAL(SUBSTR(MItems[con],87,9))
 fcant=VAL(SUBSTR(MItems[con],79,8))
 xTotal = xTotal + (fPuni*fCant)  &&*IIF(fMone="U$S".OR.fMone="USD",xDolar,1))
NEXT
*@16,70 SAY ((xPuni*xCant)*IIF(xMone="U$S".OR.xMone="USD",xDolar,1)) PICTURE "@Z 999999.99"
@16,70 SAY (xPuni*xCant) PICTURE "@Z 999999.99"
@20, 1 SAY xTotal PICTURE "@Z 999999.99"
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

  IF !vnc_tp
   IF vtip='3' .OR. (vtip='2' .AND. nc=1)
    art='*****'
    IF vTip="2"
     IF vCiv="1" .OR. vCiv="2"
       xPuni = Imp_Nc/((iva1+100)/100)
     ELSE
      xPuni = Imp_Nc
     ENDIF
    ELSE
     xPuni = 0
    ENDIF
    SETCOLOR(color7)
    @6,54 SAY 'Sub_total'
    SETCOLOR(color1)
    SET CURSOR ON
    @7,1 GET vObse PICTURE "@!"
    @16,70 GET xPuni PICTURE '@Z 999999.99'
    READ
    SET CURSOR OFF
    IF LASTKEY()=27
     RETURN(2)
    ENDIF
    xCant=1
   ELSE
    *Modificar cantidad y Precio
    *Resalta Nombre de Articulo elegido
    SET CURSOR ON
    IF vCiv="1" .OR. vCiv="2"
     xPuni = xPuni + ((xPuni*iva1)/100)
    ENDIF
    @fFil,fCol GET vObse PICTURE "@K!"    &&COLOR color8
    @16,50 GET xCant PICTURE "@K 99999.99"
    @16,59 GET xPuni PICTURE "@K 99999.999"
    READ
    IF LASTKEY()=27
     SET CURSOR OFF
     RETURN(1)
    ENDIF
    IF vCiv="1" .OR. vCiv="2"
     xPuni = xPuni/((iva1+100)/100)
    ENDIF
   ENDIF
  ELSE
   art='*****'
  ENDIF

  IF xCant>0
   Bien=CARTEL("DATOS CORRECTOS ??? [ SI ] [ NO ]",12,2,'N',colo3)
   IF !Bien .OR. LASTKEY()=27
    SET CURSOR OFF
    @16,50 SAY xCant PICTURE "@Z 99999.99"
    @16,59 SAY xPuni PICTURE "@Z 99999.999"
    RETURN(1)
   ENDIF
   @16,50 SAY xCant PICTURE "@Z 99999.99"
   @16,59 SAY xPuni PICTURE "@Z 99999.999"

   xPuni = ROUND(xPuni,2)

   *Ingresar Modificacion a matriz
   IF vtip='3' .OR. (vtip='2' .AND. nc=1)
    MItems[1]=STUFF(MItems[1],143,5,art)
    MItems[1]=STUFF(MItems[1],159,12,STR(xPuni,12,2))
    vObse = SUBSTR(vObse+SPACE(78),1,78)
    MItems[1]=STUFF(MItems[1],1,78,vObse)
    MItems[1]=STUFF(MItems[1],79,8,STR(xCant,8,2))
    MItems[1]=STUFF(MItems[1],87,9,STR(xPuni,9,3))
    MItems[1]=STUFF(MItems[1],159,12,STR(xPuni*xCant,12,2))
   ELSE
    vObse = SUBSTR(vObse+SPACE(78),1,78)
    MItems[sub]=STUFF(MItems[sub],1,78,vObse)
    MItems[sub]=STUFF(MItems[sub],79,8,STR(xCant,8,2))
    MItems[sub]=STUFF(MItems[sub],87,9,STR(xPuni,9,3))
    MItems[sub]=STUFF(MItems[sub],159,12,STR(xPuni*xCant,12,2))
   ENDIF

   SET CURSOR OFF
   RETURN(1)
  ENDIF

  IF vtip='3' .OR. (vtip='2' .AND. nc=1) .OR. vnc_tp
   KEYBOARD CHR(27)
  ELSE
   KEYBOARD CHR(24)
  ENDIF
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
  IF LASTKEY()#27
   *Guardamos
   ok=0
   STORE 0 TO VTTA,vnet,iva,reciva,tgral
   FOR con=1 TO LEN(MItems)
    VTTA=VTTA + VAL(SUBSTR(MItems[con],159,12))
   NEXT con
   vtot=VTTA
   SETCOLOR('+'+color1)
   @18,16 SAY 'Descuento'
   SETCOLOR(color1)

   *PEDIR observaciones al pie y % DESCUENTO SOBRE EL TOTAL DEL COMPROBANTE
   CLEAR GETS

   SET CURSOR ON
   *   pmen=SAVESCREEN(20,2,24,55)
   *   SETCOLOR(color3)
   *   SOMBRA(20,2,23,53,'H1',1,'',color3)
   *   @21,3 SAY'Ingrese Mensaje : 'GET mens1 PICTURE "@K!"
   *   @22,22 GET mens2 PICTURE "@K!"
   *   READ
   *   SETCOLOR(color1)
   *   RESTSCREEN(20,2,24,55,pmen)

   @20,18 GET dsto PICTURE "@K 99.99"
   READ

   tdes = (vtta * dsto) / 100

   @20,15 SAY tdes PICTURE '@Z 999999999.99' COLOR color9
   *Calculo el Neto Gravado
   vnet=vtta - tdes
   @20,28 SAY vnet PICTURE '@Z 999999999.99' COLOR color9
   IF !vnc_tp
    IF vciv <= '2'
     iva=(vnet*iva1)/100
    ENDIF
    IF vciv='2'
     reciva=(vnet*iva2)/100
    ENDIF
   ELSE
    *Se utiliza cuando hacemos una N.C. por el Importe del IVA RNI cobrado en FAC
    STORE 0 TO vtot,vnet,tdes,iva
    reciva=vimp_rni
   ENDIF
   iva=ROUND(iva,2)
   reciva=ROUND(reciva,2)
   SETCOLOR(color9)
   @19,48 SAY STR(iva1,5,2)
   @20,41 SAY iva PICTURE '@Z 999999999.99'
   @19,61 SAY STR(iva2,5,2)
   @20,54 SAY reciva PICTURE '@Z 999999999.99'

   tgral=ROUND(vnet+iva+reciva,2)
   SETCOLOR(color3)
   IF vtip='2'
    mens='Acreditado'
   ELSE
    IF tgral<0
     CARTEL("UNA FACTURA NO PUEDE TENER IMPORTE NEGATIVO [ OK ]",12,2,'N',color3)
     RETURN(1)
    ENDIF
    mens='A Pagar'
   ENDIF vtip='2'
   @18,68 SAY mens
   @20,67 SAY tgral PICTURE '@Z 999999999.99'
   IF tgral>imp_nc.AND.fact_nc#"********".AND.vtip='2'
    CARTEL("!! ATENCION !! NO ACREDITE MAS DE "+STR(imp_nc,12,2)+" [ OK ]",12,1,'N',color3)
    SET CURSOR OFF
    RETURN(1)
   ENDIF

   IF vTip="2".AND.Reg_Pa#0
    difxx = tgral - imp_nc
    IF difxx < (-0.01) .OR. difxx > 0.01
     CARTEL("!! ATENCION !! NC DEBE SER POR "+STR(imp_nc,12,2)+" [ OK ]",12,1,'N',color3)
     SET CURSOR OFF
     RETURN(1)
    ENDIF
   ENDIF
   SETCOLOR(color3)
   scr1=SAVESCREEN(14,10,19,25)
   SOMBRA(14,11,18,23,'H2',1,'',color3)
   ok=0
   sale=1
   IF !EMPTY(SUBSTR(MItems[1],143,5)) .AND. sale = 1
    @ 15,12 PROMPT ' MODIFICAR '
    @ 16,12 PROMPT ' GRABAR    '
    opc1=2
    ok=1
   ENDIF
   @ 17,12 PROMPT ' CANCELAR  '
   MENU TO opc1
   IF ok=0
    opc1=3
   ENDIF
   ok=0
   SETCOLOR(color1)
   RESTSCREEN(14,10,19,25,scr1)
   DO CASE
    CASE opc1=1
     SET CURSOR OFF
     RETURN(1)

    CASE opc1=2
     *GRAVAR ARCHIVO
     remi=0
     entrega=.F.
     SETCOLOR(color1)
     IF !DUDA("DATOS CORRECTOS")
       SET CURSOR OFF
       RETURN(1)
     ENDIF

***************************************************************
     *Control Tipo Cliente para Registrador
     DO CASE
      CASE vCiv="1" &&RI
       bit48 = "I"
      CASE vCiv="2" &&RNI
       bit48="N"
      CASE vCiv="3" &&CF
       bit48="C"
      CASE vCiv="4" &&EXE
        bit48="E"
      CASE vCiv="5" &&MT
        bit48="M"
      CASE vCiv="6" &&NA
        bit48=""
      CASE vCiv="7" &&NC
        bit48="T"
      ENDCASE
     *Control Tipo Documneto para registrador
     DO CASE
      CASE xTdoc=1  &&CUIT
       bit50="C"
      CASE xTdoc=0
       bit50=" "
     ENDCASE
     pan_comu = SAVESCREEN(,,,,)
     bDiscri = .T.
     IF vTip#"4"
      *No es Remito
      IF vTip="1"
       bDiscri = CARTEL("IMPRIME DETALLE ARTICULOS ???  [ SI ] [ NO ]",15,1,"N",colo3)
       IF LASTKEY()=27
        bDiscri = .T.
       ENDIF
      ENDIF
      if (fisc_local == 1)
        CARTEL("COMUNICANDOSE CON EL CONTROLADOR FISCAL. Espere un momento...",12,1,'E',colo3)
        IF vTip="1"
         P715_FA(p_Com,bDiscri)
        ELSE
         IF vTip="2"
          P715_NC(p_Com)
         ELSE
          IF vTip="3"
           P715_ND(p_Com)
          ELSE
           IF vTip="5"
            *P715_TK(p_Com)
           ENDIF
          ENDIF
         ENDIF
        ENDIF
        IF (bandera = "ERROR1" .OR. nu_fac <= 0)
           CARTEL("ERROR EN IMPRESORA. LOS DATOS NO SE GUARDARAN. Pulse una tecla",12,1,'N',color3)
           RETURN(1)
        ENDIF
      ELSE
        Pan_Error=SAVESCREEN(13,0,15,78)
        DO WHILE (FILE("&carpfis.\&tempofi3."))
           @14,15 SAY ("­­­ Existe: &carpfis.\&tempofi3. !!!")
        ENDDO
        RESTSCREEN(13,0,15,78,Pan_Error)

        IF vTip="1"
         SpoolerFA(bDiscri)
        ELSE
         IF vTip="2"
          SpoolerNC()
         ELSE
          IF vTip="3"
           SpoolerND()
          ELSE
           IF vTip="5"
            *SpoolerTK()
           ENDIF
          ENDIF
         ENDIF
        ENDIF

        DO WHILE (!FILE("&respfis.\&respufi3."))
           @14,15 SAY ("­­­ No Existe: &respfis.\&respufi3. !!!")
        ENDDO
        RESTSCREEN(13,0,15,78,Pan_Error)
        AREA()
        SET EXCLUSIVE ON
        IF USAR((spooltmp),'SPOOL',"E")
         APPEND FROM "&respfis.\&respufi3." delimited ALL
         GOTO BOTTOM
         IF vTip#"2"
          SKIP -1
         ENDIF
         IF (p_Com == "1")
            nu_fac:= VAL(RIGHT(TRIM(campo1), 8))
         ELSE
            nu_fac:= VAL(SUBSTR(campo1, 35, 8))
         ENDIF
         CLOSE
        ENDIF
        DO WHILE (FILE("&respfis.\&respufi3."))
           FERASE("&respfis.\&respufi3.")
           @14,15 SAY ("­­­ BORRANDO &respfis.\&respufi3. !!!")
        ENDDO
        RESTSCREEN(13,0,15,78,Pan_Error)
      ENDIF
      RESTSCREEN(,,,,pan_comu)
      IF (nu_fac <= 0 .OR. nu_fac = 100000)
         CARTEL("ERROR EN IMPRESORA. LOS DATOS NO SE GUARDARAN. Pulse una tecla",12,1,'N',color3)
         RETURN(1)
      ENDIF
      vNum:= CERO(nu_fac,8)
     ELSE
      *Es Remito
      vNum:=vRem
     ENDIF

**************************************************

     IF !Graba
      DO GRAVAR_REG
     ENDIF

     *   IF DUDA('IMPRIME EN PAPEL')
     *    SET CONSOLE OFF
     *    IF vLtra="B"
     *     SET PRINTER TO &xPtoB
     *    ELSE
     *     IF vLtra="A"
     *      SET PRINTER TO &xPtoA
     *     ELSE
     *      IF vLtra="X"
     *        xPtoRem = "LPT3"
     *        SET PRINTER TO &xPtoRem
     *      ELSE
     *        SET PRINTER TO &vNlpt
     *      ENDIF
     *     ENDIF
     *    ENDIF
     *    IF .NOT.ISPRINTER()
     *     CARTEL("LA IMPRESORA NO ESTA PREPARADA...",12,1,'E','*'+color3)
     *    ENDIF
     *    DO WHILE .NOT.ISPRINTER()
     *     INKEY(.1)
     *     TONE(700,1)
     *    ENDDO
     *    SET PRINT ON
     *    IF vTip#"5"
     *     IF vciv > '2'.AND.xreg=0
     *      *Papel para  "B"
     *      CARTEL("POR FAVOR, INSERTE PAPEL CBTE. {B} [ GRACIAS ]",18,1,'N',color3)
     *      TONO(1)
     *      TONO(2)
     *      TONO(1)
     *      TONO(2)
     *      CARTEL("ACORDATE DEL CBTE. 'B' [ OK ]",12,1,'N',"*"+color3)
     *      TONO(1)
     *      TONO(2)
     *      CARTEL("ESTAS SEGURO? [ OK ]",12,1,'N',"*"+color3)
     *      DO IMPR_FACB
     *      SET PRINTER TO &vNlpt
     *      ok=0
     *      *EXIT
     *     ELSE
     *      *Facturacion en "A"
     *      CARTEL("POR FAVOR, INSERTE PAPEL CBTE. {A} [ GRACIAS ]",18,1,'N',color3)
     *      DO IMPR_FAC
     *      SET PRINTER TO &vNlpt
     *     ENDIF
     *    ELSE
     *      CARTEL("POR FAVOR, INSERTE PAPEL REMITOS [ GRACIAS ]",18,1,'N',color3)
     *      DO IMPR_REMI
     *      SET PRINTER TO &vNlpt
     *    ENDIF
     *    SET PRINT OFF
     *    SET CONSOLE ON
     *    SET PRINTER TO &vNlpt
     *    SELECT FACTURA
     *    GO wali
     *    RECLOCK(1)
     *    REPLACE impr WITH .T.
     *    UNLOCK
     *   ENDIF
     ok=0
     *agg=5 AQUI
     RETURN(0)

    CASE opc1=3 .OR. opc1=0
     IF DUDA("CONFIRMA CANCELACION",1,color3)
      SET CURSOR OFF
      IF LASTKEY()=27
       RETURN(1)
      ENDIF
      CLOSE ALL
      RESTSCREEN(0,0,24,79,PAN_ANT)
      SETCOLOR(coli_ant)
      SET KEY 306 TO
      agg=5
      RETURN(0)
     ELSE
      SET CURSOR OFF
      RETURN(1)
     ENDIF
   ENDCASE
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

PROCEDURE VIEW_ART2
PRIVATE Co_Ante
Co_Ante=SETCOLOR()
SETCOLOR(color1)
*Leer Datos del articulo
fArti   = arti
Nom_arti= nomb
zProv   = PROVE->nomb    &&+" "+Prov
fCpro   = Cpro
funid   = unid
fFvta   = Fvta
fUnVt   = UnVt
fMedi   = Medi
zMrca   = MARCA->nomb   &&+" "+mrca
fClor   = Clor
fCali   = Cali
fUsos   = Usos
fDrza   = Drza
fRefe   = Refe
fM2Cj   = m2cj
fPuCd   = STR(pucd,9,3)
fPu30   = STR(pucd*vPu30,9,3)
fPu60   = STR(pucd*vPu60,9,3)
xRubr   = rubr    &&RUBRO->nomb+" "+rubr
fFmod   = Fmod
fMone   = Mone
fObse   = Obse

@ 8, 1 SAY xRubr
@ 8,7 SAY SUBSTR(ALLTRIM(zProv)+REPLI('Ä',40),1,40)
@ 4,67 SAY PADL(ALLTRIM(fUnid),10," ")
@ 6,67 SAY PADL(ALLTRIM(fFvta),10," ")
@ 8,67 SAY PADL(ALLTRIM(fUnVt),10," ")
@17, 7 SAY PADL(ALLTRIM(fMedi),15," ")
@ 8,49 SAY SUBSTR(ALLTRIM(zMrca)+REPLI('Ä',40),1,15)
IF EMPTY(fObse)
 @09,14 SAY "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ" COLOR color9
ELSE
 @9,14 SAY fObse
ENDIF
@9,57 SAY "C¢d.: "+fArti
@17,29 SAY PADL(ALLTRIM(fClor),10," ")
@17,48 SAY PADL(ALLTRIM(fCali), 8," ")
@17,67 SAY PADL(ALLTRIM(fUsos),10," ")
@18,67 SAY PADL(ALLTRIM(fDrza), 5," ")
@19,67 SAY PADL(ALLTRIM(fRefe),10," ")
@20, 1 SAY PADL(ALLTRIM(fCpro),15," ")
@20,67 SAY PADL(ALLTRIM(fM2Cj),10," ")
@18,33 SAY fMone COLOR IIF(fMone#"$","*"+color3,color1)
@20,22 SAY fPuCd
@20,34 SAY fPu30
@20,46 SAY fPu60
@18,40 SAY fFmod
SETCOLOR(Co_Ante)
RETURN

PROCEDURE MOVDATRE
PRIVATE GetList:={}
PRIVATE pan_uti,col_uti,vmasc
pan_uti=SAVESCREEN(0,0,24,79)
col_uti=SETCOLOR()
STORE SPACE(8) TO vrem,vfac1,vfac2,vfac3
vfec_rem=DATE()
vimp=0
vree="N"
vmens="DEVOLVER REMITO FIRMADO"
vmasc=REPLI("X",30)
STORE SPACE(30) TO vtr_fl2,vde1,vde2,vde_fl1,vde_fl2,vco_pag
vtr_fl1=PADR("ORIGEN",30," ")
exi_rem=.F.
wpag='01'
DO WHILE .T.
 SETCOLOR(colo3)
 DO CAB_REM
 tecla=65
 SET CURSOR OFF
 DO WHILE tecla#13 .AND. tecla#27
  tecla=INKEY(0)
 ENDDO
 IF tecla=27
  EXIT
 ENDIF
 SETCOLOR(color3)
 @22,20 SAY " ±Esc±-Sale  ±F5±-Campo Anterior "
 SETCOLOR(color7)
 DIST_COLOR(22,22,'~W+/R~Esc')
 DIST_COLOR(22,34,'~W+/R~F5')
 SETCOLOR("G+/B,N+/W,,,G+/B")
 SET CURSOR ON
 XPC=17
 XC=1
 SET KEY -4 TO va_tras
 @4,35 SAY vcli+' '+SUBSTR(vnom,1,25)
 DO WHILE XC<=XPC
  XD=1
  SET CURSOR ON
  DO CASE
   CASE XC=1
    @4,10 GET vrem PICTURE "@K 99999999" VALID UTI_REM(@vrem,@exi_rem)
    READ
    vrem=CERO(VAL(vrem),8)
    @4,10 SAY vrem
    IF !exi_rem
     STORE SPACE(8) TO vfac1,vfac2,vfac3
     vfec_rem=DATE()
     vimp=0
     vree="N"
     STORE SPACE(30) TO vtr_fl2,vde1,vde2,vde_fl1,vde_fl2,vco_pag
     vtr_fl1=PADR("ORIGEN",30," ")
    ENDIF
    @4,21 SAY vfec_rem
    @6,10 SAY vimp PICTURE "@Z 999999999.99"
    @6,31 SAY vree
    @6,37 SAY vfac1
    @6,46 SAY vfac2
    @6,55 SAY vfac3
    @10,8 SAY vtr1
    @11,8 SAY vtr2
    @10,39 SAY vtr_fl1
    @11,39 SAY vtr_fl2
    @15,8 SAY vde1
    @16,8 SAY vde2
    @15,39 SAY vde_fl1
    @16,39 SAY vde_fl2
    @20,8 SAY vco_pag
    @20,39 SAY vmens
   CASE XC=2
    @4,21 GET vfec_rem VALID !EMPTY(vfec_rem)
    READ
   CASE XC=3
    @6,10 GET vimp PICTURE "@K 999999999.99" VALID vimp <> 0
    READ
   CASE XC=4
    @6,31 GET vree PICTURE "@!" VALID (vree$"SsNn")
    READ
   CASE XC=5
    @6,37 GET vfac1 PICTURE "@!"
    READ
    *vfac1=CERO(VAL(vfac1),8)
    @6,37 SAY vfac1
   CASE XC=6
    @6,46 GET vfac2 PICTURE "@!"
    READ
    *vfac2=CERO(VAL(vfac2),8)
    @6,46 SAY vfac2
   CASE XC=7
    @6,55 GET vfac3 PICTURE "@!"
    READ
    *vfac3=CERO(VAL(vfac3),8)
    @6,55 SAY vfac3
   CASE XC=8
    @10,8 GET vtr1 PICTURE "&vmasc"
    READ
   CASE XC=9
    @11,8 GET vtr2 PICTURE "&vmasc"
    READ
   CASE XC=10
    @10,39 GET vtr_fl1 PICTURE "&vmasc"
    READ
   CASE XC=11
    @11,39 GET vtr_fl2 PICTURE "&vmasc"
    READ
   CASE XC=12
    @15,8 GET vde1 PICTURE "&vmasc"
    READ
   CASE XC=13
    @16,8 GET vde2 PICTURE "&vmasc"
    READ
   CASE XC=14
    @15,39 GET vde_fl1 PICTURE "&vmasc"
    READ
   CASE XC=15
    @16,39 GET vde_fl2 PICTURE "&vmasc"
    READ

   CASE XC=16
    cam="nopc~cpag"
    @20,8 GET wpag PICTURE "@K 99" VALID VALIDA(@wPag,@vCo_Pag,"REMIT",cam,2,30,"NOPC","CPAG")
    READ
    @20,8 SAY vco_pag

   CASE XC=17
    @20,39 GET vmens PICTURE "&vmasc"
    READ
  ENDCASE
  IF LASTKEY()=27
   CLEAR GETS
   XC=18
  ELSE
   XC=XC+XD
  ENDIF
 ENDDO
 IF LASTKEY()#27
  IF CARTEL("DATOS CORRECTOS  [ SI ]  [ NO ]",21,1,'N',color3)
   IF LASTKEY()#27
    IF exi_rem
     bien=CARTEL("DESEA GRABAR CABECERA Y SALIR  [ SI ]  [ NO ]",12,2,'N',color3)
     IF bien
      *Grabar Cabecera de Remito y Salir
      DO GRABA_CABEC
      ULTECLA(27)
     ENDIF
    ENDIF
    EXIT
   ENDIF
  ENDIF
 ENDIF
ENDDO
RESTSCREEN(0,0,24,79,pan_uti)
SETCOLOR(col_uti)
RETURN

PROCEDURE CAB_REM
SOMBRA(2,6,22,70,"H5",1,'',colo3)
@ 2,7 SAY " ÚCabecera del RemitoÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿ "
@ 3,7 SAY " ÃN£meroÄÄÄÄÂFechaÄÄÄÄÄÄÄÄÂClienteÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´ "
@ 4,7 SAY " ³          ³             ³                                  ³ "
@ 5,7 SAY " ÃValorÄÄÄÄÄÁÄÄÄÂReembolsoÅN£mero de FacturasÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´ "
@ 6,7 SAY " ³              ³(S/N)    ³           -        -             ³ "
@ 7,7 SAY " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ "
@ 8,7 SAY "ÚDatos del TransporteÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
@ 9,7 SAY "ÃTransporteÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂFleteÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´"
@10,7 SAY "³                              ³                              ³"
@11,7 SAY "³                              ³                              ³"
@12,7 SAY "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
@13,7 SAY "ÚDatos del ReDespachoÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
@14,7 SAY "ÃReDespachoÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂFleteÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´"
@15,7 SAY "³                              ³                              ³"
@16,7 SAY "³                              ³                              ³"
@17,7 SAY "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
@18,7 SAY "ÚCondiciones de PagoÄÄÄÄÄÄÄÄÄÄÄÂMensaje al PieÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
@19,7 SAY "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´"
@20,7 SAY "³                              ³                              ³"
@21,7 SAY "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
@22,7 SAY "                                                               "
SETCOLOR(color3)
@22,20 SAY " ±Esc±-Sale  ±ÄÙ±-Ingresa "
SETCOLOR(color7)
DIST_COLOR(22,22,'~W+/R~Esc')
DIST_COLOR(22,34,'~W+/R~ÄÙ')
RETURN

PROCEDURE GRABA_CABEC
SELECT CAB_REM
IF !exi_rem
 ADDREC(1)
 exi_rem=.T.
ENDIF
RECLOCK(1)
REPLACE nrem WITH vrem
REPLACE fech WITH vfec_rem
REPLACE clie WITH vcli
REPLACE tra1 WITH vtr1
REPLACE tra2 WITH vtr2
REPLACE ftr1 WITH vtr_fl1
REPLACE ftr2 WITH vtr_fl2
REPLACE des1 WITH vde1
REPLACE des2 WITH vde2
REPLACE fde1 WITH vde_fl1
REPLACE fde2 WITH vde_fl2
REPLACE impo WITH vimp
REPLACE reem WITH IIF(vree$"sS",.T.,.F.)
REPLACE nfac WITH vfac1+vfac2+vfac3
REPLACE nopc WITH wPag
REPLACE cpag WITH vco_pag
REPLACE obse WITH vmens
REPLACE orig WITH "R"
UNLOCK
COMMIT
RETURN

FUNCTION UTI_REM
PARA remi,exi_rem
PRIVATE antsel
antsel=ALLTRIM(STR(SELECT()))
SELECT CAB_REM
remi=CERO(VAL(remi),8)
SEEK remi
IF FOUND()
  IF vcli#clie
   CARTEL("ESTE REMITO ES DE OTRO CLIENTE  [ OK ]",15,1,'N',color3)
   SELECT &antsel
   RETURN .F.
  ENDIF
 exi_rem=.T.
 vfec_rem=fech
 vtr1=tra1
 vtr2=tra2
 vtr_fl1=ftr1
 vtr_fl2=ftr2
 vde1=des1
 vde2=des2
 vde_fl1=fde1
 vde_fl2=fde2
 vimp=impo
 vree=IIF(reem,'S','N')
 vfac1=SUBSTR(nfac,1,8)
 vfac2=SUBSTR(nfac,9,8)
 vfac3=SUBSTR(nfac,17,8)
 vco_pag=cpag
 wPag=nopc
 vmens=obse
ELSE
 exi_rem=.F.
ENDIF
SELECT &antsel
RETURN .T.
