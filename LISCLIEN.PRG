*Listado de clientes
PRIVATE pan_ant,col_ant,cual,Ind1:=""
pan_cli=SAVESCREEN(0,0,24,79)
col_ant=SETCOLOR()
cual='C' &&Lista Activos (si es I seran los Inactivos)
DO WHILE .T.
 p=1
 vm=PADR(op,4,'0')
 SOMBRA(7,44,17,60,'V2',1,'',INVER,'N+')
 SETCOLOR(color2)
 *-----------------------------------------
 DECLARE menu1[9],vmenss[9],fi_co[9],salir[1]
 menu1[ 1]='`1- Domicilio  '
 menu1[ 2]='`2- Cta.Cte.   '+CHR(26)
 menu1[ 3]='`3- Mayor      '
 menu1[ 4]='`4- Saldo Clie.'
 menu1[ 5]='`5- Comprobante'  &&+CHR(26)
 menu1[ 6]='`6- Ventas     '
 menu1[ 7]='`7- Deuda Total'
 menu1[ 8]='`8- Libros     '+CHR(26)
 menu1[ 9]='`9- Cbte.Dia   '
 vmenss[ 1]='                        Listado de Clientes por Domicilio                        '
 vmenss[ 2]='                          Listado de Cuentas Corrientes                          '
 vmenss[ 3]='                     Listado ESTADO DE CUENTA de un Cliente                      '
 vmenss[ 4]='                        Listado DEUDA ACTUAL DE CLIENTES                         '
 vmenss[ 5]='                    Listado de BOLETAS o FACTURAS PENDIENTES                     '
 vmenss[ 6]='                  Ventas realizadas en un Mes entre Dos Fechas                   '
 vmenss[ 7]='          Presenta en Pantalla el Monto Total Adeudado por los Clientes          '
 vmenss[ 8]='                Listado LIBRO DE I.V.A., ING.BRUTO y ESTADISTICAS                '
 vmenss[ 9]='               Listado de los Comprobantes del un Dia en Especial                '
 carga1(fi_co,'08,45~09,45~10,45~11,45~12,45~13,45~14,45~15,45~16,45')
 salir[1]=13
 p=PROMPT(menu1,fi_co,'I',p,salir,menu&vm,'N+/W',vmenss,24)
 vm=PADR(op+STR(p,1),4,'0')        &&Activa menu de Habilitacion
 *-----------------------------------------
 IF p=0
  CLOSE ALL
  RESTSCREEN(0,0,24,79,pan_cli)
  SETCOLOR(col_ant)
  RETURN
 ENDIF
 IF (p=4)
  bien=CARTEL("Que Clientes Incluyo [ ACTIVOS ] [ INACTIVOS ]",18,1,'N',colo3)
  IF LASTKEY()=27.OR.bien
   cual='C'
  ELSE
   cual="I"
  ENDIF
 ENDIF
 DO CASE
  CASE p=1
   *Clientes por Domicilio
   DO LISCLDOM WITH cual

  CASE p=2
   *Listado de Zonas de Venta
   SOMBRA(11,46,14,61,'V1',1,'',INVER,'N+')
   SETCOLOR(color2)
   @12,48 PROMPT'1- General   'MESS'              Permite Listar la los Cbtes.Pendientes en Una Zona Determinada     '
   @13,48 PROMPT'2- Individual'MESS'    Listar entre Dos Fechas, los Cbtes.Pendientes de un Cliente para una Zona    '
   MENU TO p6
   IF p6#0
    *resumen de cuentas
    DO LISRESUM WITH p6
   ENDIF

  CASE p=3
   *Listado MAYOR
   DO WHILE .T.
    SOMBRA(14,46,20,61,'V1',1,'',INVER,'N+')
    SETCOLOR(color2)
    @15,48 PROMPT'1- Pendientes'MESS'                Permite Listar Mayor con Comprobantes PENDIENTES                 '
    @16,48 PROMPT'2- Canceladas'MESS'                Permite Listar Mayor con Comprobantes CANCELADOS                 '
    @17,48 PROMPT'3- Ambas     'MESS'      Permite Listar Mayor con Comprobantes PENDIENTES Y CANCELADAS JUNTAS       '
    @18,48 PROMPT'4- Historico 'MESS'           Permite Listar Mayor mostrando Historia de Cada Comprobante           '
    @19,48 PROMPT'5- Sald/Fecha'MESS'            Permite Listar Saldo de Clientes a una Fecha Determinada             '
    MENU TO p6
    DO CASE
     CASE p6=0
      EXIT

     CASE p6=1.OR.p6=2
      DO LISMAYCL WITH p6

     CASE p6=3
      *Ambas
      DO LISMAYCT

     CASE p6=4
      *Historico
      DO LISCLMAY

     CASE p6=5
      *Saldo Fecha
      DO LISCCCLI

    ENDCASE
   ENDDO

  CASE p=4
   *Clientes por saldo
   PRIVATE Zon_Des,Zon_Has,nom_des,nom_has
   STORE SPACE(3) TO Zon_Des,Zon_Has
   STORE SPACE(30) TO nom_des,nom_has
   PUBLIC camp
   clpr='CLIENTES'
   ok=0
   DO saldos1
   IF ok=0
    *clientes
    OPEN_DBF("MAECLIEN","CLIENTES","C","MAECLIEN","clie","NOMCLIEN","nomb")
    SET ORDER TO 2
    DO WHILE .T.
     tecla=65
     SET CURSOR OFF
     DO WHILE .T.
      *Impresion
      IF !IMPRI()
       o1=1
      ELSE
       o1=0
      ENDIF
      IF LASTKEY()=27
       CLOSE ALL
       SETCOLOR(col_ant)
       RESTSCREEN(0,0,24,79,pan_cli)
       FERASE("&ind1")
       RETURN
      ENDIF
      IF CONFIR()
       empr=nom_emp
       titul='CLIENTES POR SALDO '
       DECLARE cam[3],sep[3],long[3],colum[3],toti[3],deci[3]
       carga1(cam,"clie~nomb~STR(sact-acre,12,2)")
       carga2(sep,"6,6,0")
       carga2(long,"7,30,12")
       carga1(colum,"Cliente~Apellido y Nombre~Saldo Actual")
       CARGA1(toti,"F~F~T")
       CARGA2(deci,"0,0,2")
       IF ss=2
        condi='moro#"M".AND.moro#"I"'   &&Morosos
       ELSE
        IF ss=3
         condi="sact-acre>imp1.AND.moro#'M'.AND.moro#'I'"
        ELSE
         IF ss=4
          condi="sact-acre<imp1.AND.moro#'M'.AND.moro#'I'"
         ELSE
          IF ss=5
           condi="sact-acre>=imp1.AND.sact-acre<=imp2.AND.moro#'M'.AND.moro#'I'"
          ENDIF
         ENDIF
        ENDIF
       ENDIF
       IF o1=1
        LISTA(cam,sep,long,colum,titul,stitul,condi,empr,.T.,dia_li,pag,toti,deci)
       ELSE
        INFO_TOT(cam,sep,long,colum,titul,sTitul,condi,empr,toti,deci)
       ENDIF
      ENDIF
      vOtro=CARTEL("LISTAMOS NUEVAMENTE [ SI ]  [ NO ]",18,1,'N',color3)
      IF LASTKEY()=27.OR.!vOtro
       CLOSE ALL
       SETCOLOR(col_ant)
       RESTSCREEN(0,0,24,79,pan_cli)
       FERASE("&ind1")
       RETURN
      ENDIF
     ENDDO
    ENDDO

   ENDIF ok=0
   FERASE("&ind1")
   SET PRINT OFF

  CASE p=5
   *Listado de Comprobantes
   SOMBRA(14,48,18,63,'V1',1,'',INVER,'N+')
   SETCOLOR(color2)
   *@15,50 prompt'1- FA, NC, ND'MESS'          Permite Reimprimir un Comprobante en Papel Com£n (FA, NC, ND)          '
   @16,50 prompt'1- Pendientes'MESS'             Emite un Detalle de los Comprobantes Pendientes de Pago             '
   menu to p6
   do case
    *  case p6=1
    *    DO LIST_FAC
    case p6=1
     DO LISPENCL
   endcase

  CASE p=6
   *Listado de Ventas Mensuales
   DO LISVEMES

  CASE p=7
   *Cuenta Corriente
   AREA()
   OPEN_DBF("CCCLIENT","CTACTE","C","CCCLIENT","clie+tcom+nume")

   SETCOLOR(color3)
   SOMBRA(11,20,15,60,'H1',1,'',color3)
   @11,21 SAY CENTRA(`SALDO A COBRAR A LA FECHA',39,"Í")
   total:=0
   DBEVAL({|| total:=total+sact},{|| .NOT.baja.AND.moro#'M'.AND.moro#"I"})
   CLOSE
   SETCOLOR(colo3)
   @12,33 SAY total PICTURE "@Z 999,999,999.99"
   SETCOLOR(color3)
   @14,30 SAY " ±Esc±-Salir "
   SETCOLOR(color7)
   DIST_COLOR(14,32,'~W+/R~Esc')
   SET CURSOR OFF
   tecla=65
   DO WHILE tecla # 27
    tecla=INKEY(0)
   ENDDO
   @15,30 SAY SPACE(40)
   SET CURSOR ON

  CASE p=8
   *Listado de Iva ventas
   SOMBRA(11,49,17,64,'H1',1,'',INVER,'N+')
   @12,50 PROMPT '1- I.V.A.   '+CHR(26) MESS`                             Listado I.V.A. Ventas                             '
   @13,50 PROMPT '2- Estadis. '         MESS'                             Estadistica de Ventas                             '
   MENU TO pp
   DO CASE
    CASE pp=1
     *IVA Vtas.
     SOMBRA(13,60,18,75,'V1',1,'',INVER,'N+')
     SETCOLOR(color2)
     @14,62 prompt'1-% EN CURSO 'MESS'               Libro IVA VENTAS con Cbtes. con el IVA POR DEFECTO                '
     @15,62 PROMPT'2-Otros %s   'MESS'                Libro IVA VENTAS con Cbtes. con el % iva Indicado                '
     @16,62 PROMPT'3-Todos los %'MESS'                  Libro IVA VENTAS con todos los % encontrados                   '
     @17,62 PROMPT'4-Cerrar     'MESS'              Permite Cerrar o Reabrir el Libro IVa ventas Mensual               '
     menu to p6
     do case

      case p6#0.AND.p6<4
       DO LISIVAVE WITH p6

      CASE p6=4

       DO CERRAR1

     ENDCASE

    CASE pp=2
     DO LISESTAD
   ENDCASE

  CASE p=9
   *Comprobantes de un dia en especial
   PRIVATE bdia,Ind1:=""
   bDia=DATE()

   *clientes
   AREA()
   OPEN_DBF("MAECLIEN","CLIEN","C","MAECLIEN","clie")

   DO WHILE .T.
    SOMBRA(10,10,14,65,"H5",1,'',colo3)
    SETCOLOR(colo3)
    @11,11 SAY "Que D¡a Desea Listar: " GET bdia VALID !EMPTY(bdia)
    READ
    IF LASTKEY()=27
     CLOSE ALL
     EXIT
    ENDIF
    bien=CARTEL("DATOS CORRECTOS? [ SI ] [ NO ]",18,1,'N',color3)
    IF !bien.OR.LASTKEY()=27
     LOOP
    ENDIF

    bmes=MONTH(bdia)

    bPerio=CHR(bmes+64)+RIGHT(DTOC(bdia),4)
    Ind1=ARCH_EXI("BORRAR","NTX")
    *FACTURAS
    IF FILE("FCC&bPerio..DBF")
     AREA()
     IF !USAR("FCC&bPerio","FACTURA")
      LOOP
     ENDIF
     IF !FILE('FCC&periodo..NTX')
      INDEX ON clie+tcom+ltra+srie+nume TO FCC&periodo
     ENDIF
     SET INDEX TO FCC&periodo
    ELSE
     CARTEL("ARCHIVO MENSUAL INEXISTENTE.. [ OK ]",12,1,'N',color3)
     LOOP
    ENDIF

    SELECT FACTURA
    SET RELATION TO clie INTO CLIEN

    IF HAY_REG()
     IF !IMPRI()
      o1=1
     ELSE
      o1=0
     ENDIF
     IF LASTKEY()=27
      CLOSE ALL
      SETCOLOR(col_ant)
      RESTSCREEN(0,0,24,79,pan_cli)
      FERASE("&Ind1")
      RETURN
     ENDIF
     IF CONFIR()
      empr=nom_emp
      titul=' CBTES. Dia: '+DTOC(bDia)
      DECLARE cam[7],sep[7],long[7],colum[7],toti[7],deci[7]
      CARGA2(sep,"1,1,1,1,1,1,0")
      condi='fech=bDia'
      IF o1=1
       CARGA1(colum,"Tp~Numero~Cliente~Importe;Neto Grav.~Importe;Total~N.C.Sobre~Estado")
       CARGA1(toti,"F~F~F~T~T~F~F")
       CARGA2(deci,"0,0,0,2,2,0,0")
       CARGA1(cam,"tcom~ltra+srie+nume~clie+' '+CLIEN->nomb~STR(ngra,12,2)~STR(paga,12,2)~lcom+scom+ncom~IIF(baja,'Anulado',SPACE(7))")
       CARGA2(long,"2,13,30,12,12,13,7")
       LISTA(cam,sep,long,colum,titul,stitul,condi,empr,.F.,dia_li,pag,toti,deci)
      ELSE
       CARGA1(colum,"Tp~Numero~Cliente~Apellido y Nombre~Importe;Total~N.C.Sobre~Estado")
       CARGA1(toti,"F~F~F~F~T~F~F")
       CARGA2(deci,"0,0,0,0,2,0,0")
       CARGA1(cam,"tcom~ltra+srie+nume~clie~CLIEN->nomb~STR(paga,12,2)~ncom~IIF(baja,'Anulado',SPACE(7))")
       CARGA2(long,"2,13,6,25,12,8,7")
       INFO_TOT(cam,sep,long,colum,titul,sTitul,condi,empr,toti,deci)
      ENDIF
     ENDIF
    ENDIF
    SET PRINT OFF
    SELECT FACTURA
    CLOSE
    FERASE("&Ind1")
   ENDDO

 ENDCASE
 CLOSE ALL
 RESTSCREEN(0,0,24,79,pan_cli)
 SETCOLOR(col_ant)
ENDDO

FUNCTION CREA_ARCL(Nom_Resu)
PRIVATE aEstruct,fBien,Nom_Arch
Nom_Arch=Nom_Resu
aEstruct:={}
AADD(aEstruct,{"TITULO","C",60,0})
AADD(aEstruct,{"ARTICULO","C",5,0})
AADD(aEstruct,{"DETALLE","C",43,0})
AADD(aEstruct,{"P_DSTO","N",5,2})
AADD(aEstruct,{"P_UNITAR","N",7,2})
DBCREATE("&Nom_Arch",aEstruct)
AREA()
IF !USAR("&Nom_Arch","RESUM")
 CARTEL("NO PUDE CREAR EL ARCHIVO PARA ALOJAR EL LISTADO [ OK ]",12,1,'N',color3)
 RETURN .F.
ENDIF
RETURN .T.

PROCEDURE SALDOS1
DO WHILE .T.
 PRIVATE pant1,colu
 pant1=SAVESCREEN(7,11,16,70)
 colu := SETCOLOR(color3)
 SOMBRA(8,11,15,68,'H5',1,'',color3,'N+')
 @9,12 SAY CENTRA(`LISTADO DE &clpr POR SALDO',55)
 @10,13 PROMPT`Salir               '
 @11,13 PROMPT`Todos lo Saldos     '
 @12,13 PROMPT`Saldos MAYORES que :'
 @13,13 PROMPT`Saldos MENORES que :'
 @14,13 PROMPT`Saldos  ENTRE .... :'
 MENU TO ss
 IF ss=1.OR.ss=0.OR.LASTKEY()=27
  ok=1
  EXIT
 ELSE
  IF ss=3
   hor=12
  ELSE
   IF ss=4
    hor=13
   ELSE
    IF ss=5
     hor=14
     @ hor,49 SAY`y'
    ENDIF ss=5
   ENDIF ss=4
  ENDIF ss=3
 ENDIF ss=1
 imp1=0
 imp2=0
 IF ss<>2
  @hor,34 GET imp1 PICTURE `@K 999999999.99' RANGE 0,999999999.99
  READ
  IF ss=5
   @hor,51 GET imp2 PICTURE `@K 999999999.99' RANGE imp1,999999999.99
   READ
  ENDIF ss=5
 ENDIF ss<>2
 EXIT
ENDDO
RESTSCREEN(7,11,16,70,pant1)
SETCOLOR(colu)
RETURN

FUNCTION LETRA
PARAMETERS numero
PRIVATE grupos[5],enletra,grupo,numstr,unidad,decena,centena,uni0,uni1,uni2
PRIVATE uni4,uni5,uni6,uni7,uni8,uni9,dec0,dec1,dec2,dec3,dec4,dec5,dec6,dec7,dec8
PRIVATE dec9,cen0,cen1,cen2,cen3,cen4,cen5,cen6,cen7,cen8,cen9,dec11,dec12,dec13
PRIVATE dec14,dec15,dec16,dec17,dec18,dec19,conect1,conect2,conect3,conect4,conect5
PRIVATE unilet,declet,cenlet,conector,grupostr
uni0="IIF(numero=0.and.grupo=1,`cero ',`')"
uni1="IIF(decena=`1',dec1&unidad,iif(grupos[grupo]=`001'.and.(grupo=2.or.grupo=4),`un ',iif(grupo>=2,`un ',`uno ')))"
uni2="IIF(decena=`1',dec1&unidad,`dos ')"
uni3="IIF(decena=`1',dec1&unidad,`tres ')"
uni4="IIF(decena=`1',dec1&unidad,`cuatro ')"
uni5="IIF(decena=`1',dec1&unidad,`cinco ')"
uni6="IIF(decena=`1',dec1&unidad,`seis ')"
uni7="IIF(decena=`1',dec1&unidad,`siete ')"
uni8="IIF(decena=`1',dec1&unidad,`ocho ')"
uni9="IIF(decena=`1',dec1&unidad,`nueve ')"
dec0="`'"
dec1="IIF(unidad=`0',`diez ',`')"
dec11=`once '
dec12=`doce '
dec13=`trece '
dec14=`catorce '
dec15=`quince '
dec16=`dieciseis '
dec17=`diecisiete '
dec18=`dieciocho '
dec19=`diecinueve '
dec2="IIF(unidad=`0',`veinte ',`veinti')"
dec3="`treinta '+IIF(unidad<>`0',`y ',`')"
dec4="`cuarenta '+IIF(unidad<>`0',`y ',`')"
dec5="`cincuenta '+IIF(unidad<>`0',`y ',`')"
dec6="`sesenta '+IIF(unidad<>`0',`y ',`')"
dec7="`setenta '+IIF(unidad<>`0',`y ',`')"
dec8="`ochenta '+IIF(unidad<>`0',`y ',`')"
dec9="`noventa '+IIF(unidad<>`0',`y ',`')"
cen0="`'"
cen1="IIF(decena+unidad=`00',`cien ',`ciento ')"
cen2="`doscient'+IIF(grupo<3,`os ',`os ')"
cen3="`trescient'+IIF(grupo<3,`os ',`os ')"
cen4="`cuatrocient'+IIF(grupo<3,`os ',`os ')"
cen5="`quinient'+IIF(grupo<3,`os ',`os ')"
cen6="`seiscient'+IIF(grupo<3,`os ',`os ')"
cen7="`setecient'+IIF(grupo<3,`os ',`os ')"
cen8="`ochocient'+IIF(grupo<3,`os ',`os ')"
cen9="`novecient'+IIF(grupo<3,`os ',`os ')"
conect1="`'"
conect2="IIF(grupos[2]>`000',`mil ',`')"
conect3="IIF(grupos[3]>`000'.or.grupos[4]>`000',IIF(grupos[3]=`001',`millon ',`millones '),`')"
conect4="IIF(grupos[4]>`000',`mil ',`')"
conect5="IIF(grupos[5]>`000',IIF(grupos[5]=`001',`billon ',`billones '),`')"
enletra=`'
numstr=ALLTRIM(STR(numero,12))
*Conversion a caracter del nro.justificado con 0 a la izq.
numstr=REPLICATE(`0',15-LEN(numstr))+numstr
FOR grupo=1 TO 5
 grupos[5-grupo+1]=SUBSTR(numstr,(grupo-1)*3+1,3)
NEXT
*proceso
FOR grupo=5 TO 1 STEP -1
 unidad=RIGHT(grupos[grupo],1)
 decena=SUBSTR(grupos[grupo],2,1)
 centena=LEFT(grupos[grupo],1)
 unilet=uni&unidad
 declet=dec&decena
 cenlet=cen&centena
 grupostr=STR(grupo,1)
 conector=conect&grupostr
 enletra=enletra+&cenlet+&declet+&unilet+&conector
NEXT
RETURN enletra

*Devuelve el nombre del mes
*Recibe el numero del mes como string
FUNCTION NOM_MES1
PARAMETERS ydia
PRIVATE ymes
IF EMPTY(DTOC(ydia))
 ydia=DATE()
ENDIF
ymes=CERO(MONTH(ydia),2)
DECLARE mes1[12]
mes1[1]="Enero"
mes1[2]="Febrero"
mes1[3]="Marzo"
mes1[4]="Abril"
mes1[5]="Mayo"
mes1[6]="Junio"
mes1[7]="Julio"
mes1[8]="Agosto"
mes1[9]="Setiembre"
mes1[10]="Octubre"
mes1[11]="Noviembre"
mes1[12]="Diciembre"
RETURN SUBSTR(DTOC(ydia),1,2)+" de "+mes1[VAL(ymes)]+" de "+STR(YEAR(ydia),4)

PROCEDURE CERRAR1
PRIVATE lib_mes,lib_ano,col_ant,pan_ant
col_ant=SETCOLOR()
pan_ant=SAVESCREEN(0,0,24,79)
*Permite Cerrar un Libro de Iva ya Listado
IF .NOT.CRIPTA02("CLA_IVA",vUsuario)
 CARTEL("USTED NO CONOCE LA CLAVE [ OK ]",12,1,'N',color3)
 RETURN
ENDIF

*controlar que el libro de iva no este cerrado
AREA()
IF USAR("LIBIVACL","LIBCL")
 IF !FILE("LIBIVACL.NTX")
  INDEX ON nmes+nano TO LIBIVACL
 ENDIF
 SET INDEX TO LIBIVACL
ENDIF
SOMBRA(10,10,15,45,"H1",1,'',colo3)
SETCOLOR(colo3)
lib_mes=SPACE(2)
lib_ano=SPACE(4)
DO WHILE .T.
 @10,11 SAY "Cerrar ¢ Reabrir Libro IVA VENTAS"
 @15,12 SAY "[Esc] - Salir"
 @11,15 SAY "Mes del Libro: "
 @13,15 SAY "A¤o del Libro: "
 @11,30 GET lib_mes PICTURE "@K 99" VALID VAL(lib_mes)>=1.AND.VAL(lib_mes)<=12
 @13,30 GET lib_ano PICTURE "@K 9999" VALID VAL(lib_ano)>=1999
 READ
 IF LASTKEY()=27
  EXIT
 ENDIF
 NO_MES(lib_mes,.T.,11,30)
 lib_mes=CERO(VAL(lib_mes),2)
 lib_ano=CERO(VAL(lib_ano),4)
 SEEK lib_mes+lib_ano
 IF !FOUND()
  ADDREC(1)
  RECLOCK(1)
  REPLACE nmes WITH lib_mes
  REPLACE nano WITH lib_ano
  REPLACE cerr WITH .F.
  UNLOCK
 ENDIF
 vcerr = cerr
 IF vcerr
  bien="DESEA REABRIR [ SI ] [ NO ]"
 ELSE
  bien="DESEA CERRAR [ SI ] [ NO ]"
 ENDIF
 vbien=CARTEL(bien,18,1,'N',color3)
 IF LASTKEY()=27 .OR. !vbien
  EXIT
 ENDIF
 RECLOCK(1)
 REPLACE cerr WITH IIF(vcerr,.F.,.T.)
 UNLOCK
 EXIT
ENDDO
CLOSE ALL
SETCOLOR(col_ant)
RESTSCREEN(0,0,24,79,pan_ant)
RETURN
