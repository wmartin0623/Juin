**********************************************************************
*MENU PRINCIPAL DE CUALQUIR SISTEMA                                  *
**********************************************************************
#include "inkey.ch"
#include "ORD.CH"
DOSIDLE()

*PARAMETERS WCLA
*REQUEST DBFCDX
*RDDSetDefault("DBFCDX")
*IF PCOUNT()=0
 WCLA = ""
*ENDIF
*FECHA :07-08-93 14:56:56
*   fx_Enable()

SET WRAP ON
SET MESS TO 24
SET SCOREBOARD OFF
SET CONFIRM ON
SET DATE BRITISH
SET CENTURY ON
SET DELETE ON
SETBLINK(.T.)
SET KEY -9 TO CALCULATOR

DECLARE vSeg:={67,58,92,87,73,78,68,;
               79,87,83,92,83,89,83,;
               84,69,77,92,76,79,87,;
               69,82,46,83,89,83 }

bSegu=''
FOR xSeg = 1 TO LEN(vSeg)
 bSegu = bSegu + CHR(vSeg[xSeg])
NEXT

IF !FILE("&bsegu")
 CHAUSS()
 CLOSE ALL
 CANCEL
ENDIF

*     DECLARE vSeg:={70,58,92,65,70,65,92,;
*                    68,66,70,92,77,79,86,;
*                    73,83,73,83,84,46,68,66,70 }
*
*     bSegu=''
*     FOR xSeg = 1 TO LEN(vSeg)
*      bSegu = bSegu + CHR(vSeg[xSeg])
*     NEXT
*
*     IF !FILE("&bsegu")
*      CHAUSS()
*      CLOSE ALL
*      CANCEL
*     ENDIF

*        DECLARE vSeg:={51,48,47,48,51,47,50,48,48,51 }
*
*        bSegu=''
*        FOR xSeg = 1 TO LEN(vSeg)
*         bSegu = bSegu + CHR(vSeg[xSeg])
*        NEXT
*
        xSegu=.F.
*        IF DATE() > CTOD(bSegu)
*
*         xSegu=.T.
*
*        ENDIF

******************************************************************
*PREPARAR VARIABLES PARA TRABAJO EN RED CON EL CONTROLADOR FISCAL*
******************************************************************
   PUBLIC compu, tempofis, fisc_local
   PUBLIC tempofi2,tempofi3,respufis,respufi2,respufi3,spooltmp
   fisc_local:= 0
   DO CASE
    CASE FILE("C:\COMPU2.CON")
       compu:= 3
       fisc_local:= 1
    CASE FILE("C:\COMPU3.CON")
       compu:= 3
       fisc_local:= 1
    CASE FILE("C:\COMPU6.CON")
       compu:= 2
       tempofis:= "COMP16.TXT"
       tempofi2:= "COMP26.TXT"
       tempofi3:= "COMP36.TXT"
       respufis:= "COMP16.ANS"
       respufi2:= "COMP26.ANS"
       respufi3:= "COMP36.ANS"
       spooltmp:= "SPOOL06.DBF"
    CASE FILE("C:\COMPU7.CON")
       compu:= 2
       tempofis:= "COMP17.TXT"
       tempofi2:= "COMP27.TXT"
       tempofi3:= "COMP37.TXT"
       respufis:= "COMP17.ANS"
       respufi2:= "COMP27.ANS"
       respufi3:= "COMP37.ANS"
       spooltmp:= "SPOOL07.DBF"
    CASE FILE("C:\COMPU8.CON")
       compu:= 2
       tempofis:= "COMP18.TXT"
       tempofi2:= "COMP28.TXT"
       tempofi3:= "COMP38.TXT"
       respufis:= "COMP18.ANS"
       respufi2:= "COMP28.ANS"
       respufi3:= "COMP38.ANS"
       spooltmp:= "SPOOL08.DBF"
    CASE FILE("C:\COMPU9.CON")
       compu:= 2
       tempofis:= "COMP19.TXT"
       tempofi2:= "COMP29.TXT"
       tempofi3:= "COMP39.TXT"
       respufis:= "COMP19.ANS"
       respufi2:= "COMP29.ANS"
       respufi3:= "COMP39.ANS"
       spooltmp:= "SPOOL09.DBF"
    CASE FILE("C:\COMPU10.CON")
       compu:= 2
       tempofis:= "COMP110.TXT"
       tempofi2:= "COMP210.TXT"
       tempofi3:= "COMP310.TXT"
       respufis:= "COMP110.ANS"
       respufi2:= "COMP210.ANS"
       respufi3:= "COMP310.ANS"
       spooltmp:= "SPOOL10.DBF"
    CASE FILE("C:\COMPU11.CON")
       compu:= 2
       tempofis:= "COMP111.TXT"
       tempofi2:= "COMP211.TXT"
       tempofi3:= "COMP311.TXT"
       respufis:= "COMP111.ANS"
       respufi2:= "COMP211.ANS"
       respufi3:= "COMP311.ANS"
       spooltmp:= "SPOOL11.DBF"
    CASE FILE("C:\COMPU12.CON")
       compu:= 2
       tempofis:= "COMP112.TXT"
       tempofi2:= "COMP212.TXT"
       tempofi3:= "COMP312.TXT"
       respufis:= "COMP112.ANS"
       respufi2:= "COMP212.ANS"
       respufi3:= "COMP312.ANS"
       spooltmp:= "SPOOL12.DBF"
    CASE FILE("C:\COMPU13.CON")
       compu:= 2
       tempofis:= "COMP113.TXT"
       tempofi2:= "COMP213.TXT"
       tempofi3:= "COMP313.TXT"
       respufis:= "COMP113.ANS"
       respufi2:= "COMP213.ANS"
       respufi3:= "COMP313.ANS"
       spooltmp:= "SPOOL13.DBF"
    CASE FILE("C:\COMPU14.CON")
       compu:= 2
       tempofis:= "COMP114.TXT"
       tempofi2:= "COMP214.TXT"
       tempofi3:= "COMP314.TXT"
       respufis:= "COMP114.ANS"
       respufi2:= "COMP214.ANS"
       respufi3:= "COMP314.ANS"
       spooltmp:= "SPOOL14.DBF"
    CASE FILE("C:\COMPU15.CON")
       compu:= 2
       tempofis:= "COMP115.TXT"
       tempofi2:= "COMP215.TXT"
       tempofi3:= "COMP315.TXT"
       respufis:= "COMP115.ANS"
       respufi2:= "COMP215.ANS"
       respufi3:= "COMP315.ANS"
       spooltmp:= "SPOOL15.DBF"
    CASE FILE("C:\COMPU16.CON")
       compu:= 2
       tempofis:= "COMP116.TXT"
       tempofi2:= "COMP216.TXT"
       tempofi3:= "COMP316.TXT"
       respufis:= "COMP116.ANS"
       respufi2:= "COMP216.ANS"
       respufi3:= "COMP316.ANS"
       spooltmp:= "SPOOL16.DBF"
    CASE FILE("C:\COMPU17.CON")
       compu:= 2
       tempofis:= "COMP117.TXT"
       tempofi2:= "COMP217.TXT"
       tempofi3:= "COMP317.TXT"
       respufis:= "COMP117.ANS"
       respufi2:= "COMP217.ANS"
       respufi3:= "COMP317.ANS"
       spooltmp:= "SPOOL17.DBF"
    CASE FILE("C:\COMPU18.CON")
       compu:= 2
       tempofis:= "COMP118.TXT"
       tempofi2:= "COMP218.TXT"
       tempofi3:= "COMP318.TXT"
       respufis:= "COMP118.ANS"
       respufi2:= "COMP218.ANS"
       respufi3:= "COMP318.ANS"
       spooltmp:= "SPOOL18.DBF"
    CASE FILE("C:\COMPU19.CON")
       compu:= 2
       tempofis:= "COMP119.TXT"
       tempofi2:= "COMP219.TXT"
       tempofi3:= "COMP319.TXT"
       respufis:= "COMP119.ANS"
       respufi2:= "COMP219.ANS"
       respufi3:= "COMP319.ANS"
       spooltmp:= "SPOOL19.DBF"
    CASE FILE("C:\COMPU20.CON")
       compu:= 2
       tempofis:= "COMP120.TXT"
       tempofi2:= "COMP220.TXT"
       tempofi3:= "COMP320.TXT"
       respufis:= "COMP120.ANS"
       respufi2:= "COMP220.ANS"
       respufi3:= "COMP320.ANS"
       spooltmp:= "SPOOL20.DBF"
    OTHERWISE
       CARTEL("NUMERO DE COMPU. INCORRECTO [ Salir ]",1,12,"N",color3)
       QUIT
   ENDCASE

   PUBLIC opspo
   opspo:= 1
*   @ 15, 29 PROMPT "  SPOOLER 1  "
*   @ 16, 29 PROMPT "  SPOOLER 2  "
*   MENU TO opspo
   SET COLOR TO
   IF (LASTKEY() == K_ESC)
   ELSE
      PUBLIC carpfis, respfis, oppap, ejecfac, puerto_com
      oppap:= 2
      IF (opspo == 1)
         puerto_com:= 1
         compu:= 2
         carpfis:= "R:\ARCHIVO1"
         respfis:= "R:\RESPUES1"
         ejecfac:= "SPO_FACT"
      ELSE
         puerto_com:= 2
         compu:= 5
         carpfis:= "ARCHIVO2"
         respfis:= "RESPUES2"
      ENDIF
   ENDIF
*********************************************************************

PUBLIC XP,XD,XPM,VMES,VANO,periodo,rut_bak,wwmes,wwano,hGauge,nPercent
PUBLIC op,opc,opc1,opc2,vusuario,Rut_Tem,Rut_Lis
PUBLIC pri_ntx,dia,fi1,co1,fi2,co2,pan1,vRDD,vDolar:=0
PUBLIC p_Com:=STR(puerto_com,1)
*Barritas de avances de procesos
PUBLIC qbarra:={"|","/","-","\"}
*Leer Ruta Archivo CCCLIBAK.DBF
AREA()
IF USAR("AUXILIAR","AUX")
 GO TOP
 rut_bak = ALLTRIM(rbak)
 Rut_Tem = Rut_Bak        &&Se Utiliza para listados, por si han activado otro
                          &&Directorio que no sea el de trabajo activo
 Rut_Lis = ALLTRIM(rlis)
 vDolar = dola

 CarpFis = ALLTRIM(car_fis)
 RespFis = ALLTRIM(res_fis)
ELSE
 QUIT
ENDIF
*Extencion de Indices por Defecto
vRDD=ORDBAGEXT()

*----------------------------------------------------
*Leer de Base De Datos las Opciones Activas
AREA()
*vusuario=NETNAME()
IF USAR("MENU_OPC","OPC_MENU")
 IF !FILE("MENU_OPC&vRDD.")
  INDEX ON nomb TO MENU_OPC
 ENDIF
 SET INDEX TO MENU_OPC
ELSE
 QUIT
ENDIF
GO TOP
*Controlar si usuario existe
vusuario=''
IF EMPTY(vusuario)
 pa=SAVESCREEN(16,15,19,57)
 vusuario=SPACE(8)
 SETCOLOR(color3)
 SOMBRA(16,15,18,56,'H5',1,'',color3)
 @17,16 SAY "Ingrese Su Nombre de Usuario : "
 vUsuario=""
 vc=47
 Tecla=7
 DO WHILE Tecla#13
  tecla = INKEY(0)
  IF tecla=13
   EXIT
  ENDIF
  IF (Tecla=7.OR.Tecla=8)
   IF LEN(vUsuario)>=1
    vUsuario=LEFT(vUsuario,LEN(vUsuario)-1)
    vc--
   ENDIF
   @17,vc SAY " " COLOR "W+/R"
   @17,vc SAY ""
  ELSE
   IF LEN(vUsuario)<8
    vUsuario = vUsuario + CHR(tecla)
    @17,vc SAY "*" COLOR "W+/R"
    vc++
   ENDIF
  ENDIF
 ENDDO
 vUsuario = PADR(SUBSTR(UPPER(vUsuario)+SPACE(8),1,8),8,' ')
 RESTSCREEN(16,15,19,57,pa)
 SEEK vusuario
 IF !FOUND()
  vusuario="NINGUNO "    &&Activa Solamente Menu FIN
 ENDIF
ENDIF
*Leer usuario y opciones habilitadas para el mismo
SEEK PADR(vusuario,8,' ')
IF FOUND()
 DO WHILE nomb=PADR(vusuario,8,' ')
  il=CERO(nmen,2)
  DECLARE vactivo&il[copc]
  vacti=acti
  CARGA1(vactivo&il,"&vacti")
  SKIP
 ENDDO
ELSE
 CARTEL("No Encuentro MENU para este Usuario   [ OK ]",12,1,'N',color3)
 CLOSE ALL
 RETURN
ENDIF
*----------------------------------------------------

vmes=MONTH(DATE())
wwmes = vmes
vano=YEAR(DATE())
wwano=vano
pri_ntx = .F.
dia = DATE()
DECLARE fi_co[1]
CLEAR
pace=SPACE(75)
SELECT AUX
GO TOP
IF EMPTY(nomb).OR.VACIA()
 SET DELIMITERS TO chr(26)+chr(27)
 SET DELIMITERS ON
 DO WHILE .t.
  CLEAR
  nom_emp=SPACE(30)
  dom_emp=SPACE(30)
  loc_emp=SPACE(25)
  iv_emp=' '
  ib_emp=SPACE(10)
  cuit_emp=SPACE(11)
  ret_emp=SPACE(7)
  SETCOLOR(color1)
  CLEAR
  @0,0,24,79 BOX REPLICATE('±',9)
  @0,0 SAY REPLICATE(' ',80)
  SOMBRA(8,15,16,70,'H1',1,'')
  @7,15 SAY CENTRA('DATOS DE LA EMPRESA ',56)
  SETCOLOR(color6)
  @9,16 SAY'Nombre Empresa : 'get nom_emp picture '@!'
  @10,16 SAY'Domicilio      : 'get dom_emp picture '@!'
  @11,16 SAY'Localidad      : 'get loc_emp picture '@!'
  @12,16 SAY'Ing. Brutos    : 'get ib_emp picture'9999999999'
  @13,16 SAY'Condicion IVA  : 'get iv_emp picture'9'
  @14,16 SAY'C.U.I.T.       : 'get cuit_emp picture'99999999999'
  @15,16 SAY'Ag.Retenci¢n   : 'get ret_emp picture'9999999'
  READ
  SETCOLOR(color3)
  @18,18 PROMPT'  Grabar   '
  @18,38 PROMPT' No Grabar '
  @18,58 PROMPT'   Salir   '
  MENU TO ter
  SETCOLOR(color1)
  IF ter=2
   LOOP
  ELSE
   IF ter=3
    CLEAR
    CLOSE ALL
    CANCEL
   ENDIF
  ENDIF
  IF VACIA()
   ADDREC(0)
  ELSE
   GO TOP
  ENDIF
  RECLOCK(0)
  REPLACE nomb WITH nom_emp
  REPLACE domi WITH dom_emp
  REPLACE loca WITH loc_emp
  REPLACE ibru WITH ib_emp
  REPLACE cuit WITH cuit_emp
  REPLACE aret WITH ret_emp
  REPLACE civa WITH iv_emp
  UNLOCK
  EXIT
 ENDDO
 SET DELIMITERS OFF
ELSE
 nom_emp =nomb
 dom_emp =domi
 ib_emp  =ibru
 iv_emp  =civa
 loc_emp =loca
 cuit_emp=cuit
 ok=0
ENDIF

CLEAR
CLOSE DATABASE
CLOSE ALL
CLOSE PROCEDURE
SETCOLOR(colo3)
CLEAR
@0,0,24,79 BOX REPLICATE('±',9)
DO PANT_DIFA
SETCOLOR(color3)
@ 0,0 SAY SPACE(79)
@ 0,0 SAY "Usuario: "+vusuario
@ 0,26 SAY CENTRA(ALLTRIM(nom_emp),32,'*')
*@ 0,62 SAY 'Periodo: '+CERO(vmes,2)+'-'+CERO(vano,4)
papi=SAVESCREEN(0,0,24,79)
IF .NOT.CRIPTA02("CLAVE",vUsuario)
 CARTEL("USTED NO TIENE IDEA DE LA CLAVE.... [  CHAUSSSS ]",12,1,'N',color3)
 CLEAR
 CLOSE ALL
 CANCEL
ENDIF
*Pedir Periodo de Trabajo
SOMBRA(9,15,16,50,'H5',1,'',colo3)
SETCOLOR(colo3)
@10,15 TO 10,50
@9,15 SAY CENTRA("PERIODO DE TRABAJO",36)
@12,17 SAY 'Ingrese MES (mm) : ' GET vmes PICTURE '@K 99' VALID vmes>=1 .AND. vmes<=12
@14,17 SAY 'Ingrese A¤O (aaaa) : ' GET vano PICTURE '@K 9999' VALID vano>=1994 .AND. vano<=2100
READ
RESTSCREEN(0,0,24,79,papi)
IF LASTKEY() = 27
 CLOSE ALL
 CLEAR
 CANCEL
ENDIF

*Obtenemos Periodo : al N§de mes se le suma 64 para obtener el valor numerico
*de una letra, que sera el codigo del mes de trabajo = A-ENE  B-FEB  C-MAR
periodo=CHR(vmes+64)+CERO(vano,4)

 *Pedir Cotizaci¢n Dolar
 SOMBRA(9,15,14,50,'H5',1,'',color9)
 SETCOLOR(color1)
 @10,15 TO 10,50
 @9,15 SAY CENTRA("COTIZACION DOLAR",36) COLOR color9
 @12,17 SAY 'Cotizaci¢n Dolar : ' GET vDolar PICTURE '@K 999.9999'
 READ
 IF LASTKEY() = 27
  CLOSE ALL
  CLEAR
  CANCEL
 ENDIF
 AREA()
 IF USAR("AUXILIAR","AUX")
  GO TOP
  RECLOCK(1)
  REPLACE dola WITH vDolar
  UNLOCK
  CLOSE
 ENDIF

**ABRIR BASES DE DATOS
 RESTSCREEN(0,0,24,79,papi)
 opc1=20
 DO UTILIDAD
*Declaracion de Constantes para Menus
SETCOLOR(color1)
*PRINCIPAL
IF WCLA # "RESUMI"
 DECLARE menup[6],mensaje[6],filas[6],cols[6]
 CARGA1(menup,"1-MAESTROS~2-MOVIMIENTOS~3-LISTADOS~4-INDICES ~5-UTILIDADES~6-F I N")
 mensaje[1]='          ALTAS, MODIF. Y BAJAS DE Rubros, Marcas, Proveedores, Articulos, Etc.  '
 mensaje[2]='                    Consultas y Modificaci¢n Listas de Precios                   '
 mensaje[3]='                   Listados Varios por Pantalla e Impresora                      '
 mensaje[4]='              Creaci¢n de Indices de Busqueda de Todos los Archivos              '
 mensaje[5]='                            Utilidades Varias                                    '
 mensaje[6]='                       Volver al Sistema Operativo                               '
 CARGA2(filas,"2,2,2,2,2,2")
 CARGA2(cols,"5,17,32,44,55,69")
ELSE
 DECLARE menup[2],mensaje[2],filas[2],cols[2]
 CARGA1(menup,"1-MAESTROS~2-F I N")
 mensaje[1]='          ALTAS, MODIF. Y BAJAS DE Rubros, Marcas, Proveedores, Articulos, Etc.  '
 mensaje[2]='                       Volver al Sistema Operativo                               '
 CARGA2(filas,"2,2")
 CARGA2(cols,"5,69")
ENDIF

*MAESTROS
DECLARE submenu1[11],mensaje1[11]
submenu1[ 1]="`1-RUBROS     "
submenu1[ 2]="`2-LISTAS     "
submenu1[ 3]="`3-MARCAS     "
submenu1[ 4]="`4-ARTICULOS  "
submenu1[ 5]="`5-CLIENTES   "
submenu1[ 6]="`6-BANCOS     "
submenu1[ 7]="`7-PLAZAS BCOS"
submenu1[ 8]="`8-PROVINCIAS "
submenu1[ 9]="`9-DEPARTAMENT"
submenu1[10]="`A-C.POSTALES "
submenu1[11]="`B-REMITOS    "
mensaje1[ 1]='            ALTAS, MODIF. Y BAJAS DE RUBROS à FAMILIAS DE ARTICULOS            '
mensaje1[ 2]='                  ALTAS, MODIF. Y BAJAS DE LISTAS DE PRECIOS                   '
mensaje1[ 3]='                ALTAS, MODIF. Y BAJAS DE MARCAS PARA ARTICULOS                 '
mensaje1[ 4]='                      ALTAS, MODIF. Y BAJAS DE ARTICULOS                       '
mensaje1[ 5]='                       ALTAS, MODIF. Y BAJAS DE CLIENTES                       '
mensaje1[ 6]='                        ALTAS, MODIF. Y BAJAS DE BANCOS                        '
mensaje1[ 7]='                   ALTAS, MODIF. Y BAJAS DE PLAZAS DE BANCOS                   '
mensaje1[ 8]='                      ALTAS, MODIF. Y BAJAS DE PROVINCIAS                      '
mensaje1[ 9]='                    ALTAS, MODIF. Y BAJAS DE DEPARTAMENTOS                     '
mensaje1[10]='                   ALTAS, MODIF. Y BAJAS DE CODIGOS POSTALES                   '
mensaje1[11]='            ALTAS, MODIF. Y BAJAS DE CONDICIONES DE PAGO EN REMITOS            '

*MOVIMIENTOS
DECLARE submenu2[2],mensaje2[2]
submenu2[1]="`1-ARTICULOS   "+CHR(26)
submenu2[2]="`2-CLIENTES    "+CHR(26)
mensaje2[1]='                 Consulta y Modificaci¢n de Listas de Precios                  '
mensaje2[2]='                 Movimiento de Cuentas Corrientes de Clientes                  '

*LISTADOS
DECLARE submenu3[5],mensaje3[5]
submenu3[ 1]="`1-RUBROS     "
submenu3[ 2]="`2-LISTAS     "+CHR(26)
submenu3[ 3]="`3-MARCAS     "+CHR(26)
submenu3[ 4]="`4-ARTICULOS  "+CHR(26)
submenu3[ 5]="`5-CLIENTES   "+CHR(26)
mensaje3[ 1]='                           Listados Varios de Rubros                           '
mensaje3[ 2]='                     Listados Varios de Listas de Precios                      '
mensaje3[ 3]='                           Listados Varios de Marcas                           '
mensaje3[ 4]='                         Listados Varios de Articulos                          '
mensaje3[ 5]='                          Listados Varios de CLIENTES                          '

*INDEXADOS
DECLARE submenu4[1],mensaje4[1] &&,vactivo4[1]
CARGA1(submenu4,"`1-INDEXAR")
mensaje4[1]='              Creaci¢n de Indices de Busqueda de Todos los Archivos            '

*UTILIDADES
DECLARE submenu5[4],mensaje5[4]
submenu5[ 1]="`1-CLAVES      "+CHR(26)
submenu5[ 2]="`2-EMPACAR     "
submenu5[ 3]="`3-PARAMETROS  "
submenu5[ 4]="`4-SALDOS      "
mensaje5[1]='                 Permite Cambiar la Clave de acceso al SISTEMA                 '
mensaje5[2]='          Eliminaci¢n de Registros Borrados definitivamente de MAESTROS        '
mensaje5[3]='                       Parametros Generales del Sistema                        '
mensaje5[4]='           Permite calcular Saldo Clientes en Funcion de la CtaCte.            '

*TERMINAR
DECLARE submenu6[2],mensaje6[2]  &&,vactivo6[2]
CARGA1(submenu6," `1-NO ~ `2-SI ")
mensaje6[1]='                      SEGUIR EN EL SISTEMA DE FACTURACION                      '
mensaje6[2]='                        SALIR AL SISTEMA OPERATIVO                             '
*Declaracion de Submenues
IF WCLA # "RESUMI"
 DECLARE popups[6],salire[3]
 CARGA2(salire,"13,19,4")
 popups[1]="PROMPT(submenu1,fi_co,'I',1,salire,vactivo01,'N+/W',mensaje1,24)"
 popups[2]="PROMPT(submenu2,fi_co,'I',1,salire,vactivo02,'N+/W',mensaje2,24)"
 popups[3]="PROMPT(submenu3,fi_co,'I',1,salire,vactivo03,'N+/W',mensaje3,24)"
 popups[4]="PROMPT(submenu4,fi_co,'I',1,salire,vactivo04,'N+/W',mensaje4,24)"
 popups[5]="PROMPT(submenu5,fi_co,'I',1,salire,vactivo05,'N+/W',mensaje5,24)"
 popups[6]="PROMPT(submenu6,fi_co,'I',1,salire,vactivo06,'N+/W',mensaje6,24)"
ELSE
 DECLARE popups[2],salire[3]
 popups[1]="PROMPT(submenu1,fi_co,'I',1,salire,vactivo01,'N+/W',mensaje1,24)"
 popups[2]="PROMPT(submenu6,fi_co,'I',1,salire,vactivo06,'N+/W',mensaje6,24)"
 CARGA2(salire,"13,19,4")
ENDIF

opc = 0
SETCOLOR(color3)
@0,0 CLEAR TO 0,79
@ 0,0 SAY "Usuario: "+vusuario
@ 0,26 SAY CENTRA(ALLTRIM(nom_emp),32,'*')
@ 0,62 SAY 'Periodo: '+CERO(vmes,2)+'-'+CERO(vano,4)
SETCOLOR(color3)
@21,56 SAY' [F10]-CALCULADORA '
SETCOLOR(color1)
DO WHILE .T.
 op=popup(popups,menup,filas,cols,mensaje,opc)
 opc = VAL(SUBSTR(op,1,1))
 opc1 = VAL(SUBSTR(op,2,2))
 IF op = '602' .OR. (WCLA="RESUMI" .AND. op='202')
   ant=SETCOLOR()
   SETCOLOR(color3)
   TONE(130,1)
   TONE(700,1)
   if !DUDA('ESTA SEGURO DE QUERER SALIR ?.',2)
    opc = 0
    RESTSCREEN(fi1,co1,fi2+1,co2+1,pan1)
    LOOP
   ELSE
    RESTSCREEN(fi1,co1,fi2+1,co2+1,pan1)
    TONE(500,1)
    TONE(600,1)
    TONE(130,1)
    CARTEL('­ NO OLVIDE REALIZAR RESPALDOS !  [Ok] ',12,1,'N')
    SETCOLOR("B/W")
    CLEAR
    EXIT
   ENDIF
   SETCOLOR(ant)
 ELSE
  IF op = '601' .OR. (WCLA="RESUMI" .AND. op='201')
   opc = 0
  ENDIF
 ENDIF
 *-------------------------------------
 IF opc#0
  *Leer accesos habilitados a sub_menues para el usuraio en curso
  IF SELECT("MSUB")#0
   SELECT MSUB
   CLOSE
  ENDIF
  IF USAR("MENU_SUB","MSUB")
   IF !FILE("MENU_SUB&vRDD.")
    INDEX ON nomb+nmen TO MENU_SUB
   ENDIF
   SET INDEX TO MENU_SUB
   SEEK PADR(vusuario,8,' ')+STR(opc,1)
   IF FOUND()
    DO WHILE PADR(vusuario,8,' ')=nomb.AND.STR(opc,1)=nmen
     il=nmen+nsub
     DECLARE menu&il[copc]
     vacti=acti
     CARGA1(menu&il,"&vacti")
     SKIP
    ENDDO
   ENDIF
  ENDIF
 ENDIF
 *-------------------------------------
 DO CASE
  CASE opc = 1

   DO MAESTROS
   RESTSCREEN(fi1,co1,fi2+1,co2+1,pan1)
  CASE opc = 2
   DO CASE
    CASE opc1=1
     DO MOVARTIC
    CASE opc1=2
     DO MOVCLIEN
   ENDCASE

   RESTSCREEN(fi1,co1,fi2+1,co2+1,pan1)
  CASE opc = 3
   PUBLIC re,ok,mesa,desde,hasta,des,has,in1,imp1,imp2,ss,o1,stitul,dia_li,pag
   dia_li = DTOC(DATE())
   pag=0
   stitul=''
   DO CASE
    CASE opc1=1
     DO LISTADOS
    CASE opc1=2
     DO LISTADOS
    CASE opc1=3
     DO LISTADOS
    CASE opc1=4
     DO LISARTIC
    CASE opc1=5
     DO LISCLIEN
   ENDCASE
   RESTSCREEN(fi1,co1,fi2+1,co2+1,pan1)
  CASE opc = 4
   pri_ntx = .T.
   opc1=20
   DO UTILIDAD
   pri_ntx = .F.
  CASE opc = 5
   DO UTILIDAD
   RESTSCREEN(fi1,co1,fi2+1,co2+1,pan1)
 ENDCASE
 RESTSCREEN(fi1,co1,fi2+1,co2+1,pan1)
ENDDO
*fx_Disable()
RETURN

FUNCTION POPUP
PARA tabla1,tabla2,tabla3,tabla4,mensa,op
PRIVATE pantalla,cont,opcion,op
SAVE SCREEN TO pantalla
IF PCOUNT() = 4
 PRIVATE mensa[LEN(tabla2)],op
 AFILL(mensa,'')
 op = 1
ENDIF
IF PCOUNT() = 5
 PRIVATE op
 op = 1
ENDIF
IF op <> 0
 KEYBOARD CHR(13)
ENDIF
DO WHILE .T.
 SOMBRA(1,4,3,76,'V5',10,'',INVER,color2)
 SETCOLOR(color2)
 FOR cont=1 TO len(tabla2)
  @tabla3[cont],tabla4[cont] PROMPT tabla2[cont] MESS mensa[cont]
 NEXT
 MENU TO op
 IF LASTKEY()=27
  op=LEN(tabla2)
 ENDIF
 IF op = 2 .AND. WCLA = "RESUMI"
  DECLARE fi_co[2]
  CARGA1(fi_co,'06,70~07,70')
  s1=6
 ELSE
  IF op=1        &&MAESTROS
   DECLARE fi_co[11]
   CARGA1(fi_co,'06,06~07,06~08,06~09,06~10,06~11,06~12,06~13,06~14,06~15,06~16,06')
   s1=13                &&ancho de ventana
  ELSE
   IF op=2       &&MOVIMIENTOS
    DECLARE fi_co[2]
    CARGA1(fi_co,'06,18~07,18')
    s1=14
   ELSE
    IF op=3      &&LISTADOS
     DECLARE fi_co[5]
     CARGA1(fi_co,'06,33~07,33~08,33~09,33~10,33')
     s1=13
    ELSE
     IF op=4     &&INDEXAR
      DECLARE fi_co[1]
      CARGA1(fi_co,'06,45')
      s1=9
     ELSE
      IF op=5    &&UTILIDADES
       DECLARE fi_co[4]
       CARGA1(fi_co,'06,56~07,56~08,56~09,56')
       s1=15
      ELSE
       IF op=6   &&FIN DEL PROGRAMA
        DECLARE fi_co[2]
        CARGA1(fi_co,'06,70~07,70')
        s1=6
       ENDIF
      ENDIF
     ENDIF
    ENDIF
   ENDIF
  ENDIF
 ENDIF
 fi1=VAL(SUBSTR(fi_co[1],1,2)) - 1
 co1=VAL(SUBSTR(fi_co[1],4,2)) - 1
 fi2=VAL(SUBSTR(fi_co[LEN(fi_co)],1,2)) + 1
 co2=VAL(SUBSTR(fi_co[LEN(fi_co)],4,2)) + s1
 pan1=SAVESCREEN(fi1,co1,fi2+1,co2+1)
 SOMBRA(fi1,co1,fi2,co2,'H5',6,'',INVER,color2)
 SETCOLOR(color2)
 unpopup=tabla1[IIF(op>0,op,1)]
 opcion=STR(op,1)+CERO(&unpopup,2)
 SETCOLOR(color1)
 DO CASE
  CASE LASTKEY()=13
   EXIT
  CASE LASTKEY()=27
   opcion=''
   op=LEN(tabla2)
   RESTSCREEN(fi1,co1,fi2+1,co2+1,pan1)
   KEYB CHR(13) &&   loop
  OTHERWISE
   RESTSCREEN(fi1,co1,fi2+1,co2+1,pan1)
   KEYBOARD CHR(LASTKEY())+CHR(13)
 ENDCASE
 RESTORE SCREEN FROM pantalla
ENDDO
RETURN opcion

FUNCTION a
RETURN ACHOICE()

PROCEDURE PANT_DIFA
*    @ 8,5 SAY "ÉÍÍÍÍÍÍ»±±±ÉÍÍÍ»±±ÉÍÍÍÍÍÍÍ»±±±±ÉÍÍÍÍÍÍ»±±±±ÉÍÍÍÍÍÍ»±±±ÉÍÍÍ»±±±ÉÍÍÍÍÍ»±"
*    @ 9,5 SAY "È» ÉÍ» È»±±È» É¼±±È»  ÉÍÍÍ¼±±±±È» É» É¼±±±±È» ÉÍ» È»±±È» É¼±±É¼ ÉÍ» È»"
*    @10,5 SAY "±º º±È» º±±±º º±±±±º  º±±±±±±±±±º ºº º±±±±±±º º±º  º±±±º º±±±º É¼±È» º"
*    @11,5 SAY "±º º±±º º±±±º º±±±±º  ÈÍ»±±±±±±±º È¼ º±±±±±±º ÈÍ¼ É¼±±±º º±±±º º±±±º º"
*    @12,5 SAY "±º º±±º º±±±º º±±±±º  ÉÍ¼±±±±±±±º É» º±±±±±±º ÉÍ» È»±±±º º±±±º º±±±º º"
*    @13,5 SAY "±º º±É¼ º±±±º º±±±±º  º±±±±±±±ÉÍ¼É¼È»ÈÍ»±±±±º º±º  º±±±º º±±±º È»±É¼ º"
*    @14,5 SAY "É¼ ÈÍ¼ É¼±±É¼ È»±±É¼  È»±±±±±É¼  È»É¼  È»±±É¼ ÈÍ¼ É¼±±É¼ È»±±È» ÈÍ¼ É¼"
*    @15,5 SAY "ÈÍÍÍÍÍÍ¼±±±ÈÍÍÍ¼±±ÈÍÍÍÍ¼±±±±±ÈÍÍÍÍ¼ÈÍÍÍÍ¼±±ÈÍÍÍÍÍÍ¼±±±ÈÍÍÍ¼±±±ÈÍÍÍÍÍ¼±"
RETURN

FUNCTION BERRE
PARA dato
RETURN RIGHT(dato,4)+SUBSTR(dato,4,2)+LEFT(dato,2)

FUNCTION AYUDO
PARAMETERS campi,ntx1,ntx2,l1,l2,camp1,archi
PRIVATE col_col
col_col=SETCOLOR()
IF RECCOUNT()=0
 pan=SAVESCREEN(20,31,23,61)
 SETCOLOR(color3)
 @20,31 CLEAR TO 23,61
 @20,31 TO 23,61 DOUBLE
 @21,36 SAY '   PRESTE ATENCION  '
 @22,32 SAY 'Ä ARCHIVO '+archi+' VACIO Ä'
 SET CURSOR OFF
 TONO(2)
 INKEY(0)
 SET CURSOR ON
 SETCOLOR(col_col)
 RESTSCREEN(20,31,23,61,pan)
 RETURN .F.
ENDIF
SET ORDER TO 1
pan=SAVESCREEN(4,29,24,78)
SETCOLOR(color3)
SOMBRA(4,29,22,77,'H1',1,'',color3)
SETCOLOR(color3)
@ 6,29  SAY 'ÌÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹'
@ 19,29 SAY 'ÇÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¶'
@ 16,29 SAY 'ÌÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹'
@ 4,41 SAY ' BUSCAR '
@17,30 SAY 'BUSCAR:'
@ 20,30 SAY CHR(24)+CHR(25)+' PgUp PgDn mover          '+CHR(17)+'ÄÙ-ACEPTA'
@ 21,30 SAY "F2-cambia clave      ESC-salir"
DECLARE cam[2],sep[2],ind[2],lar[2],nom[2]
carga1(cam,campi)
carga2(sep,'6,0')
titul1=archi+'       NOMBRE              '
ind[1]=ntx1
lar[1]=l1
nom[1]=archi
ind[2]=ntx2
lar[2]=l2
nom[2]='NOMBRE :'
ord_ant=STR(INDEXORD())
SET ORDER TO NTX2
SETCOLOR(color3)
GO TOP
BUSCARE(5,31,15,75,cam,titul1,sep,ind,lar,18,31,nom,17,41)
SETCOLOR(col_col)
SET ORDER TO &ord_ant
RESTSCREEN(4,29,24,78,pan)
*RELEASE cam,sep,ind,lar,ord_ant
IF LASTKEY()=27
 RETURN .F.
ENDIF
RETURN .T.

*scroll por pantalla
FUNCTION SCR
PARA menus,posvis,fila,col,largo,f1,c1,f2,c2,princ,tono_il,tono
LOCAL xa:=0
PRIV ind,a,pos,n,maxtab,x,i,bb,col_ant
col_ant=SETCOLOR()
IF PCOUNT()>=5
 maxtab=largo
ELSE
 IF PCOUNT()<5
  maxtab=len(menus)
 ENDIF
ENDIF
bb='n+/r'
DO CASE
 CASE PCOUNT()=4
  f1=13
  c1=39
  f2=23
  c2=78
  princ=1
  tono='w/n'
  tono_il='n/w'
 CASE PCOUNT()=5
  f1=13
  c1=39
  f2=23
  c2=78
  princ=1
  tono='w/n'
  tono_il='n/w'
 CASE PCOUNT()=8
  princ=1
  tono='w/n'
  tono_il='n/w'
 CASE PCOUNT()=9
  tono_il='n/w'
  tono='w/n'
 CASE PCOUNT()=10
  tono='w/n'
ENDCASE
ind=princ
a=0
IF maxtab<posvis
 posvis=maxtab
ENDIF
IF ind>maxtab-posvis-1
 pos=ind+posvis-maxtab
 princ=maxtab-posvis    &&+1
ELSE
 pos=2
ENDIF
IF Pos=0
 Pos=1
ENDIF
SAVE SCREEN
SET CURSOR OFF
SETCOLOR(tono)
xa=0
IF Princ=0
 Princ=1
ENDIF
FOR n=1 TO posvis
 @fila+xa,col SAY menus[princ+xa]
 xa++
NEXT
DO WHILE a<>13.AND.a<>27.AND.a<>-1.AND.a<>-2.AND.a<>-3.AND.a<>-4.AND.a<>-5.AND.;
a<>-6.AND.a<>-7.AND.a<>-8.AND.a<>-9.AND.a<>7.AND.a<>22
 DO CASE
  CASE a=5
   DO sube
  CASE a=24
   DO cae
  CASE a=1
   DO comien
  CASE a=6
   DO fin
  CASE a=3
   DO avpag
  CASE a=18
   DO repag
  CASE a>47.AND.a<123
   DO buscar
   LOOP
 ENDCASE
 DO escribe
 a=INKEY(0)
ENDDO
SET CURSOR ON
SETCOLOR(col_ant)
RESTORE SCREEN
RETURN(ind)

PROCEDURE CAE
IF ind<maxtab
 ind=ind+1
 IF pos<posvis
  pos=pos+1
 ELSE
  x=ind
  FOR n=pos TO 1 step -1
   @fila+n-1,col SAY menus[x]
   x=x-1
  NEXT
 ENDIF
 @fila+pos-2,col SAY menus[ind-1]
ENDIF
DO escribe
RETURN

PROCEDURE SUBE
IF ind>1
 ind=ind-1
 IF pos>1
  pos=pos-1
 ELSE
  x=ind
  FOR n=1 TO posvis
   @fila+n-1,col SAY menus[x]
   x=x+1
  NEXT
 ENDIF
 @fila+pos,col SAY menus[ind+1]
ENDIF
DO escribe
RETURN

PROCEDURE COMIEN
FOR ind=1 TO posvis
 @fila+ind-1,col SAY menus[ind]
NEXT
ind=1
pos=1
DO escribe
RETURN

PROCEDURE FIN
FOR n=1 TO posvis
 @fila+n-1,col SAY menus[maxtab-posvis+n]
NEXT
ind=maxtab
pos=posvis
DO escribe
RETURN

PROCEDURE AVPAG
x=pos
IF ind+posvis-x<=maxtab-posvis
 FOR n=1 TO posvis
  x=x-1
  @fila+n-1,col SAY menus[ind+posvis-x]
 NEXT
 ind=ind+posvis
ELSE
 FOR n=1 TO posvis
  @fila+n-1,col SAY menus[maxtab-posvis+n]
 NEXT
 ind=maxtab+pos-posvis
ENDIF
DO escribe
RETURN

PROCEDURE REPAG
x=pos
IF ind-x>=posvis
 FOR n=1 TO posvis
  x=x-1
  @fila+n-1,col SAY menus[ind-x-posvis]
 NEXT
 ind=ind-posvis
ELSE
 FOR ind=posvis TO 1 step-1
  @fila+ind-1,col SAY menus[ind]
 NEXT
 ind=pos
ENDIF
DO ESCRIBE
RETURN

PROCEDURE BUSCAR
PRIV b,wcont,var,indice
b=a
wcont=0
var=""
DO WHILE (b>47.AND.b<123).OR.b=0.OR.b=32.OR.(b>128.AND.b<169)
 var=var+CHR(b)
 wcont=wcont+1
 ctrl=0
 FOR indice=1 TO maxtab
  IF UPPER(var)=UPPER(SUBSTR(menus[indice],1,wcont))
   IF indice>ind-pos.AND.indice<=ind-pos+posvis
    pos=pos+indice-ind
   ELSE
    IF indice>maxtab-posvis
     pos=indice+posvis-maxtab
    ELSE
     IF indice<=posvis
      pos=indice
     ENDIF
    ENDIF
   ENDIF
   inicio=indice-pos+1
   FOR n=1 TO posvis
    @fila+n-1,col SAY menus[inicio+n-1]
   NEXT
   ind=indice
   indice=maxtab
   ctrl=1
   var=SUBSTR(menus[ind],1,wcont)
   DO ESCRIBE
   SETCOLOR(bb)
   @fila+pos-1,col SAY var
   SETCOLOR(tono)
  ENDIF
 NEXT
 IF ctrl=0
  var=SUBSTR(var,1,LEN(var)-1)
  wcont=wcont-1
 ENDIF
 b=0
 DO WHILE b=0
  b=INKEY(0)
 ENDDO
ENDDO
a=b
RETURN

PROCEDURE ESCRIBE
SETCOLOR(tono_il)
@fila+pos-1,col SAY menus[ind]
SETCOLOR(tono)
RETURN

FUNCTION CHAUSS
 CARTEL("USTED NO ESTA AUTORIZADO [ OK ]",12,1,'N',color3)
RETURN NIL

*PARA LISTADOS
*scroll por pantalla
FUNCTION SCR1
PARA menus,posvis,fila,col,largo,f1,c1,f2,c2,princ,tono_il,tono
PRIV ind,a,pos,n,maxtab,x,i,bb,col_ant
col_ant=SETCOLOR()
IF PCOUNT()>=5
 maxtab=largo
ELSE
 IF PCOUNT()<5
  maxtab=len(menus)
 ENDIF
ENDIF
bb=`n+/r'
DO CASE
 CASE PCOUNT()=4
  f1=13
  c1=39
  f2=23
  c2=78
  princ=1
  tono=`w/n'
  tono_il=`n/w'
 CASE PCOUNT()=5
  f1=13
  c1=39
  f2=23
  c2=78
  princ=1
  tono=`w/n'
  tono_il=`n/w'
 CASE PCOUNT()=8
  princ=1
  tono=`w/n'
  tono_il=`n/w'
 CASE PCOUNT()=9
  tono_il='n/w'
  tono=`w/n'
 CASE PCOUNT()=10
  tono='w/n'
ENDCASE
ind=princ
a=0
IF maxtab<posvis
 posvis=maxtab
ENDIF
IF ind>maxtab-posvis-1
 pos=ind+posvis-maxtab
 princ=maxtab-posvis+1
ELSE
 pos=1
ENDIF
SAVE SCREEN
SET CURSOR OFF
SETCOLOR(tono)
FOR n=1 TO posvis
 @fila+n-1,col SAY menus[n+princ-1]
NEXT
DO WHILE a<>13.AND.a<>27.AND.a<>-1.AND.a<>-2.AND.a<>-3.AND.a<>-4.AND.a<>-5.AND.;
a<>-6.AND.a<>-7.AND.a<>-8.AND.a<>-9.AND.a<>7.AND.a<>22
 DO CASE
  CASE a=5
   DO sube1
  CASE a=24
   DO cae1
  CASE a=1
   DO comien1
  CASE a=6
   DO fin1
  CASE a=3
   DO avpag1
  CASE a=18
   DO repag1
  CASE a>47.AND.a<123
   DO buscar1
   LOOP
 ENDCASE
 DO escribe1
 a=INKEY(0)
ENDDO
SET CURSOR ON
SETCOLOR(col_ant)
RESTORE SCREEN
RETURN(ind)

PROCEDURE CAE1
IF ind<maxtab
 ind=ind+1
 IF pos<posvis
  pos=pos+1
 ELSE
  x=ind
  FOR n=pos TO 1 step -1
   @fila+n-1,col SAY menus[x]
   x=x-1
  NEXT
 ENDIF
 @fila+pos-2,col SAY menus[ind-1]
ENDIF
DO escribe1
RETURN

PROCEDURE SUBE1
IF ind>1
 ind=ind-1
 IF pos>1
  pos=pos-1
 ELSE
  x=ind
  FOR n=1 TO posvis
   @fila+n-1,col SAY menus[x]
   x=x+1
  NEXT
 ENDIF
 @fila+pos,col SAY menus[ind+1]
ENDIF
DO escribe1
RETURN

PROCEDURE COMIEN1
FOR ind=1 TO posvis
 @fila+ind-1,col SAY menus[ind]
NEXT
ind=1
pos=1
DO escribe1
RETURN

PROCEDURE FIN1
FOR n=1 TO posvis
 @fila+n-1,col SAY menus[maxtab-posvis+n]
NEXT
ind=maxtab
pos=posvis
DO escribe1
RETURN

PROCEDURE AVPAG1
x=pos
IF ind+posvis-x<=maxtab-posvis
 FOR n=1 TO posvis
  x=x-1
  @fila+n-1,col SAY menus[ind+posvis-x]
 NEXT
 ind=ind+posvis
ELSE
 FOR n=1 TO posvis
  @fila+n-1,col SAY menus[maxtab-posvis+n]
 NEXT
 ind=maxtab+pos-posvis
ENDIF
DO escribe1
RETURN

PROCEDURE REPAG1
x=pos
IF ind-x>=posvis
 FOR n=1 TO posvis
  x=x-1
  @fila+n-1,col SAY menus[ind-x-posvis]
 NEXT
 ind=ind-posvis
ELSE
 FOR ind=posvis TO 1 step-1
  @fila+ind-1,col SAY menus[ind]
 NEXT
 ind=pos
ENDIF
DO ESCRIBE1
RETURN

PROCEDURE BUSCAR1
PRIV b,wcont,var,indice
b=a
wcont=0
var=""
DO WHILE (b>47.AND.b<123).OR.b=0.OR.b=32.OR.(b>128.AND.b<169)
 var=var+CHR(b)
 wcont=wcont+1
 ctrl=0
 FOR indice=1 TO maxtab
  IF UPPER(var)=UPPER(SUBSTR(menus[indice],1,wcont))
   IF indice>ind-pos.AND.indice<=ind-pos+posvis
    pos=pos+indice-ind
   ELSE
    IF indice>maxtab-posvis
     pos=indice+posvis-maxtab
    ELSE
     IF indice<=posvis
      pos=indice
     ENDIF
    ENDIF
   ENDIF
   inicio=indice-pos+1
   FOR n=1 TO posvis
    @fila+n-1,col SAY menus[inicio+n-1]
   NEXT
   ind=indice
   indice=maxtab
   ctrl=1
   var=SUBSTR(menus[ind],1,wcont)
   DO ESCRIBE1
   SETCOLOR(bb)
   @fila+pos-1,col SAY var
   SETCOLOR(tono)
  ENDIF
 NEXT
 IF ctrl=0
  var=SUBSTR(var,1,LEN(var)-1)
  wcont=wcont-1
 ENDIF
 b=0
 DO WHILE b=0
  b=INKEY(0)
 ENDDO
ENDDO
a=b
RETURN

*aqui modifique

PROCEDURE ESCRIBE1
SETCOLOR(tono_il)
@fila+pos-1,col SAY menus[ind]
SETCOLOR(tono)
RETURN
