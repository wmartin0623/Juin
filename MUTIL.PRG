*Menu de Utilidades
PRIVATE pan_uti
pan_uti=SAVESCREEN(0,0,24,79)
posi = opc1+5
DO CASE
 CASE opc1=1
  *Clave de Acceso
   pan5 = SAVESCREEN(posi,62,posi+6,78)
   SOMBRA(posi,62,posi+2,77,'V2',1,'',INVER,'N+')
   SETCOLOR(color2)
   opcion = 0
   @posi+1,63 prompt '1- Sistema   'MESS'                      Clave de Acceso Principal al SISTEMA                       '
*  @posi+2,63 prompt '2- Baja Cbtes'MESS'                 Clave de Acceso para Dar de Baja a Comprobantes                 '
*  @posi+3,63 prompt '3- Cr�dito CL'MESS'                    Clave de Acceso para Credito de Clientes                     '
*  @posi+4,63 prompt '4- Ej.Cbles. 'MESS'                    Clave de Acceso para Ejercicios Contables                    '
   MENU TO opcion
   DO CASE
    CASE opcion = 0
     RESTSCREEN(posi,62,posi+6,78,pan5)
     RETURN
    CASE opcion = 1
      DO UTICLAVE WITH "CLAVE",vUsuario
    CASE opcion = 2
      DO UTICLAVE WITH "CLA_FUNC",vUsuario
    CASE opcion = 3
      DO UTICLAVE WITH "CLAVCLIE",vUsuario
    CASE opcion = 4
      DO UTICLAVE WITH "CLA_EJE",vUsuario
   ENDCASE
   RESTSCREEN(posi,62,posi+6,78,pan5)

 CASE opc1 = 2
  *Eliminar Registros Borrados de Maestros
  sino=CARTEL('ESTE PROCESO NECESITA USO EXCLUSIVO DE ARCHIVOS [ SIGUE ] [ ABORTA ]',12,1,'N',color3)
  IF sino
   DO EMPACAR
  ENDIF

 CASE opc1 = 3
  *Parametros
  DO UTIPARAM

 CASE opc1 = 20
  pan20=SAVESCREEN(0,0,24,79)
  PRIVATE col_ant,hor,pa
  col_ant = SETCOLOR()
  pa=SAVESCREEN(0,0,24,79)
  hGauge := GaugeNew(10, 5,12, MAXCOL() - 5, "W/r", "W+/r" )
  GaugeDisplay( hGauge )
  nPercent=0
  vper=1/7
  VGAUGE(hGauge,nPercent)
  **ABRIR BASES DE DATOS
  CLOSE ALL
  SET CURSOR OFF
  *Rubros
  SELECT 1
  IF USAR('MAERUBRO','RUBROS')
   IF !FILE('MAERUBRO&vRDD.') .OR. pri_ntx
    INDEX ON rubr TO MAERUBRO eval EnCurso("MAERUBRO") every INT(lastrec()/10)
   ENDIF
   IF !FILE('NOMRUBRO&vRDD.') .OR. pri_ntx
    INDEX ON nomb TO NOMRUBRO eval EnCurso("NOMRUBRO") every INT(lastrec()/10)
   ENDIF
   CLOSE
  ELSE
   QUIT
  ENDIF
  VGAUGE(hgauge,vper)

  *Proveedores
  SELECT 1
  ar1 = "MAEPROVE.DBF"
  ar2 = "MAEPROVE&vRDD."
  ar3 = "NOMPROVE&vRDD."
  IF USAR('&ar1','PROVE')
   IF !FILE('&ar2') .OR. pri_ntx
    INDEX ON prov TO &ar2  eval EnCurso("MAEPROVE") every INT(lastrec()/10)
   ENDIF
   IF !FILE('&ar3') .OR. pri_ntx
    INDEX ON nomb TO &ar3  eval EnCurso("NOMPROVE") every INT(lastrec()/10)
   ENDIF
   CLOSE
  ELSE
   QUIT
  ENDIF
  VGAUGE(hgauge,vper)

  *Marcas
  SELECT 1
  IF USAR('MAEMARCA','MARCA')
   IF !FILE('MAEMARCA&vRDD.') .OR. pri_ntx
    INDEX ON mrca TO MAEMARCA  eval EnCurso("MAEMARCA") every INT(lastrec()/10)
   ENDIF
   IF !FILE('NOMMARCA&vRDD.') .OR. pri_ntx
    INDEX ON nomb TO NOMMARCA  eval EnCurso("NOMMARCA") every INT(lastrec()/10)
   ENDIF
   CLOSE
  ELSE
   QUIT
  ENDIF
  VGAUGE(hgauge,vper)

  *articulos
    AREA()
    IF USAR('MAEARTIC','ARTIC')
     *Por Rubro,Proveedor
     IF !FILE('MAEARTIC&vRDD.') .OR. pri_ntx
      INDEX ON rubr+prov TO MAEARTIC  eval EnCurso("MAEARTIC") every INT(lastrec()/10)
     ENDIF
     *Por Rubro, Proveedor y Marca
     IF !FILE('MAEARTI1&vRDD.') .OR. pri_ntx
      INDEX ON rubr+prov+mrca TO MAEARTI1  eval EnCurso("MAEARTI1") every INT(lastrec()/10)
     ENDIF
     *Por proveedor, Marca
     IF !FILE('MAEARTI2&vRDD.') .OR. pri_ntx
      INDEX ON prov+mrca TO MAEARTI2   eval EnCurso("MAEARTI2") every INT(lastrec()/10)
     ENDIF
     *Por Marca
     IF !FILE('MAEARTI3&vRDD.') .OR. pri_ntx
      INDEX ON mrca TO MAEARTI3  eval EnCurso("MAEARTI3") every INT(lastrec()/10)
     ENDIF
     *Por rubro, Marca
     IF !FILE('MAEARTI4&vRDD.') .OR. pri_ntx
      INDEX ON rubr+mrca TO MAEARTI4  eval EnCurso("MAEARTI4") every INT(lastrec()/10)
     ENDIF
     *Por rubro y Articulo
     IF !FILE('MAEARTI5&vRDD.') .OR. pri_ntx
      INDEX ON rubr+arti TO MAEARTI5  eval EnCurso("MAEARTI5") every INT(lastrec()/10)
     ENDIF
     IF !FILE('NOMARTIC&vRDD.') .OR. pri_ntx
      INDEX ON nomb TO NOMARTIC  eval EnCurso("NOMARTIC") every INT(lastrec()/10)
     ENDIF
     *Por Articulo
     IF !FILE('MAEARTI6&vRDD.') .OR. pri_ntx
      INDEX ON arti TO MAEARTI6  eval EnCurso("MAEARTI6") every INT(lastrec()/10)
     ENDIF
     CLOSE
    ELSE
     QUIT
    ENDIF
    VGAUGE(hgauge,vper)

  *Cabecera Presupuestos
  SELECT 1
  IF USAR('MOVPRECB','PRECB')
   IF !FILE('MOVPRECB&vRDD.') .OR. pri_ntx
    INDEX ON nano+npre TO MOVPRECB  eval EnCurso("Presupuesto") every INT(lastrec()/10)
   ENDIF
   CLOSE
  ELSE
   QUIT
  ENDIF
  VGAUGE(hgauge,vper)

  *Detalle Presupuestos
  SELECT 1
  IF USAR('MOVPREDE','PREDE')
   IF !FILE('MOVPREDE&vRDD.') .OR. pri_ntx
    INDEX ON nano+npre TO MOVPREDE  eval EnCurso("Presupuesto") every INT(lastrec()/10)
   ENDIF
   CLOSE
  ELSE
   QUIT
  ENDIF
  VGAUGE(hgauge,vper)

  *Numeracion Presupuestos
  SELECT 1
  IF USAR('NUMPRESU','NUMPR')
   IF !FILE('NUMPRESU&vRDD.') .OR. pri_ntx
    INDEX ON nano TO NUMPRESU  eval EnCurso("Num.Presup.") every INT(lastrec()/10)
   ENDIF
   CLOSE
  ELSE
   QUIT
  ENDIF
  VGAUGE(hgauge,vper)

  RESTSCREEN(0,0,24,79,pa)
  CLOSE ALL
  SET CURSOR ON
  RESTSCREEN(0,0,24,79,pan20)
ENDCASE
RESTSCREEN(0,0,24,79,pan_uti)
RETURN

PROCEDURE uticlave
PARAMETERS fArchi,fUsuario
PRIVATE papan,reg,col_ant
papan=SAVESCREEN(0,0,23,79)
col_ant=SETCOLOR()
IF PCOUNT()=0
 PRIVATE fArchi:="CLAVE",fUsuario:=vUsuario
ENDIF
IF PCOUNT()=1
 PRIVATE fUsuario:=vUsuario
ENDIF
CLEAR
IF USAR("&fArchi","CLAVE","C")
 IF !FILE("&fArchi..NTX")
  INDEX ON nomb+nombre TO &fArchi
 ENDIF
 SET INDEX TO &fArchi
ELSE
 QUIT
ENDIF
CARTEL(' � CAMBIO  CLAVE  ACCESO ! ',5,1,'E')
SOMBRA(8,25,14,55,'H1',.1,'',INVER)
@ 9,26 SAY 'CLAVE VIEJA : _'
@11,26 SAY 'NUEVA CLAVE :'
@13,26 SAY 'CONFIRMACION:'
@14,30 SAY 'Clave '+IIF(farchi="CLAVE","Sistema",IIF(fArchi="CLA_FUNC","Cbtes. ","Cr�dito Clientes")) COLOR color8
*Solicitud clave Vieja
SET CURSOR OFF
vnuevos=''
vc=40
DO WHILE LEN(vnuevos)<9
 @ 9,vc SAY '_'
 tecla=INKEY(0)
 IF tecla=13
  EXIT
 ENDIF
 vnuevos=vnuevos+CHR(tecla)
 @ 9,vc SAY '*'
 vc=vc+1
ENDDO
vnuevos=SUBSTR(UPPER(vnuevos)+SPACE(8),1,8)
SEEK vUsuario+CRIPT(vnuevos)
IF !FOUND()
 CARTEL('� USTED NO ESTA AUTORIZADO [Ok] ',13,1,'N',color3)
 CLOSE ALL
 SET CURSOR ON
 RESTSCREEN(0,0,23,79,papan)
 SETCOLOR(col_ant)
 RETURN
ENDIF
reg=RECNO()
*Ingreso Nueva Clave
DO WHILE .T.
 SET CURSOR OFF
 vnuevos=''
 vc=40
 @11,vc SAY SPACE(8)
 DO WHILE LEN(vnuevos)<9
  @ 11,vc SAY '_'
  tecla=INKEY(0)
  IF tecla=13.OR. tecla=27
   EXIT
  ENDIF
  vnuevos=vnuevos+CHR(tecla)
  @ 11,vc SAY '*'
  vc=vc+1
 ENDDO
 IF tecla=27
  EXIT
 ENDIF
 vnuevos=SUBSTR(UPPER(vnuevos)+SPACE(8),1,8)
 *Confirma Clave Nueva
 vnuevo1=''
 vc=40
 @13,vc SAY SPACE(8)
 DO WHILE LEN(vnuevo1)<9
  @ 13,vc SAY '_'
  tecla=INKEY(0)
  IF tecla=13.OR. tecla=27
   EXIT
  ENDIF
  vnuevo1=vnuevo1+CHR(tecla)
  @ 13,vc SAY '*'
  vc=vc+1
 ENDDO
 IF tecla=27
  EXIT
 ENDIF
 vnuevo1=SUBSTR(UPPER(vnuevo1)+SPACE(8),1,8)
 IF vNuevos # vNuevo1
  CARTEL("INTENTE NUEVAMENTE.... [ OK ]",12,1,'N',color3)
  LOOP
 ENDIF
 SEEK vUsuario+CRIPT(vnuevo1)
 IF !FOUND()
  GO reg
  RECLOCK(1)
  REPLACE nombre WITH CRIPT(vnuevo1)
  UNLOCK
  EXIT
 ELSE
  CARTEL('� CLAVE YA EXISTE. VERIFIQUE [Ok] ',13,1,'N',color3)
 ENDIF
ENDDO
SET CURSOR ON
INDEX ON nomb+nombre TO CLAVE
RESTSCREEN(0,0,23,79,papan)
SETCOLOR(col_ant)
RETURN

*Devuelve el nombre del mes
*Recibe el numero del mes como string
FUNCTION NO_MES1
PARAMETERS ymes,imp,fil,COL
PRIVATE txt
IF PCOUNT()=1
   PRIVATE imp
   imp=.F.
ENDIF
IF PCOUNT()=2
 PRIVATE fil,col
 fil=ROW()
 colu=COL()
ENDIF
IF EMPTY(ymes)
   ymes=CERO(MONTH(DATE()),2)
ENDIF
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
txt = mes1[VAL(ymes)]
IF imp
 col_ant=SETCOLOR()
 SETCOLOR(color3)
 @ fil,COL SAY txt
 SETCOLOR(col_ant)
 RETURN(.T.)
ENDIF
RETURN(txt)

PROCEDURE EMPACAR
**Realiza PACK en archivos Maestros
PRIVATE col_ant,hor,pa
col_ant = SETCOLOR()
pa=SAVESCREEN(0,0,24,79)
hGauge := GaugeNew(10, 5,12, MAXCOL() - 5, "W/r", "W+/r" )
GaugeDisplay( hGauge )
nPercent=0
vper=1/12       &&12=cantidad de bloques
CLOSE ALL
SET CURSOR OFF
SET EXCLUSIVE ON
*Rubro
SELECT 1
IF USAR('MAERUBRO','RUBROS',"E")
 IF !FILE('MAERUBRO&vRDD.')
  INDEX ON rubr TO MAERUBRO
 ENDIF
 IF !FILE('NOMRUBRO&vRDD.')
  INDEX ON nomb TO NOMRUBRO
 ENDIF
 SET INDEX TO MAERUBRO,NOMRUBRO
 PACK
ELSE
 QUIT
ENDIF
CLOSE
VGAUGE(hgauge,vper)

*Marcas
SELECT 1
IF USAR('MAEMARCA','MARCAS',"E")
 IF !FILE('MAEMARCA&vRDD.')
  INDEX ON mrca TO MAEMARCA
 ENDIF
 IF !FILE('NOMMARCA&vRDD.')
  INDEX ON nomb TO NOMMARCA
 ENDIF
 SET INDEX TO MAEMARCA,NOMMARCA
 PACK
ELSE
 QUIT
ENDIF
CLOSE
VGAUGE(hgauge,vper)

*Proveedores
SELECT 1
IF USAR('MAEPROVE','PROVE',"E")
 IF !FILE('MAEPROVE&vRDD.')
  INDEX ON prov TO MAEPROVE
 ENDIF
 IF !FILE('NOMPROVE&vRDD.')
  INDEX ON nomb TO NOMPROVE
 ENDIF
 SET INDEX TO MAEPROVE,NOMPROVE
 PACK
ELSE
 QUIT
ENDIF
CLOSE
VGAUGE(hgauge,vper)

RESTSCREEN(0,0,24,79,pa)
CLOSE ALL
SET CURSOR ON
SET EXCLUSIVE OFF
RETURN

****
*  Gauge.prg
*
*  Sample functions to create, display, and update a percentage completed
*  progress gauge.  This function can be used for creating user interface
*  options such as a status bar to indicate the current status of a process.
*
*  Copyright (c) 1990, Nantucket Corp.  All rights reserved.
*  David R. Alison
*
*  Note: Compile with /W/N options
****
* Box array definitions
#define B_LEN           9
#define B_TOP           1
#define B_LEFT          2
#define B_BOTTOM        3
#define B_RIGHT         4
#define B_BACKCOLOR     5
#define B_BARCOLOR      6
#define B_DISPLAYNUM    7
#define B_BARCHAR       8
#define B_PERCENT       9
#define B_BOXLINES      "�Ŀ�����"

***
*  GAUGENEW( <nRowTop>, <nColumnTop>, <nRowBottom>, <nColumnBottom>,
*     [<cBackgroundColor>],
*     [<cGaugeColor>],
*     [<cGaugeCharacter>] ) --> aGauge
*
*  Create a new gauge array
*
***
FUNCTION GAUGENEW( nTop, nLeft, nBottom, nRight, ;
                 cBackColor, cBarColor, cBarCharacter )
LOCAL aHandle[ B_LEN ]

* Assign default values
aHandle[ B_TOP ]        := nTop
aHandle[ B_LEFT ]       := nLeft
aHandle[ B_BOTTOM ]     := nBottom
aHandle[ B_RIGHT ]      := nRight
aHandle[ B_BACKCOLOR ]  := "W/N"
aHandle[ B_BARCOLOR ]   := "W+/N"
aHandle[ B_DISPLAYNUM ] := .T.
aHandle[ B_BARCHAR ]    := CHR( 219 )
aHandle[ B_PERCENT ]    := 0

* Resolve parameters
IF cBackColor <> NIL
   aHandle[ B_BACKCOLOR ] := cBackColor
ENDIF
IF cBarColor <> NIL
   aHandle[ B_BARCOLOR ] := cBarColor
ENDIF
IF cBarCharacter <> NIL
   aHandle[ B_BARCHAR ] := cBarCharacter
ENDIF

* OK, the defaults are set, now let's make sure it will fit on the
* screen correctly
IF aHandle[ B_RIGHT ] < aHandle[ B_LEFT ] + 4
   aHandle[ B_RIGHT ] := aHandle[ B_LEFT ] + 4
ENDIF

IF aHandle[ B_BOTTOM ] < aHandle[ B_TOP ] + 2
   aHandle[ B_BOTTOM ] := aHandle[ B_TOP ] + 2
ENDIF

* Determine if we can fit the bracketed number on top of the graph
IF aHandle[ B_RIGHT ] < aHandle[ B_LEFT ] + 9
   aHandle[ B_DISPLAYNUM ] := .F.
ENDIF
RETURN( aHandle )

***
* GAUGEDISPLAY( aGauge ) --> aGauge
* Display a gauge array to the screen
***
FUNCTION GAUGEDISPLAY( aHandle )
LOCAL nCenter := ROUND((aHandle[ B_RIGHT ] - aHandle[ B_LEFT ]) / 2, 0 ) + 1
LOCAL cOldColor := SETCOLOR( aHandle[ B_BACKCOLOR ] )

*@ aHandle[ B_TOP ], aHandle[ B_LEFT ] CLEAR TO ;
*  aHandle[ B_BOTTOM ], aHandle[ B_RIGHT ]

*@ aHandle[ B_TOP ], aHandle[ B_LEFT ], ;
*  aHandle[ B_BOTTOM ], aHandle[ B_RIGHT ] BOX B_BOXLINES
SOMBRA(aHandle[ B_TOP ], aHandle[ B_LEFT ],;
   aHandle[ B_BOTTOM ], aHandle[ B_RIGHT ],"H2",1,'',aHandle[ B_BACKCOLOR ])

IF aHandle[ B_DISPLAYNUM ]
 @ aHandle[ B_TOP ], nCenter SAY "[      ]"
ENDIF
SETCOLOR( cOldColor )
* Draw bar to show current percent
GAUGEUPDATE( aHandle, aHandle[ B_PERCENT ] )
RETURN( aHandle )

***
*  GAUGEUPDATE( aGauge, nPercent ) --> aGauge
*  Updates a gauge with a new progress value and redisplays the gauge
*  to the screen to the screen
***
FUNCTION GAUGEUPDATE( aHandle, nPercent )
LOCAL nCenter := ROUND((aHandle[ B_RIGHT ] - aHandle[ B_LEFT ]) / 2, 0 ) + 1
LOCAL cOldColor := SETCOLOR( aHandle[ B_BARCOLOR ] )
LOCAL nBarRatio := (aHandle[ B_RIGHT ]) - (aHandle[ B_LEFT ] + 1)
LOCAL nRow := 0, nCols := 0

IF aHandle[ B_DISPLAYNUM ]
 @ aHandle[ B_TOP ], nCenter + 2 SAY STR( nPercent * 100, 3 ) + "%"
ENDIF
IF nPercent > 1
 nPercent := 1
ENDIF
IF nPercent < 0
 nPercent := 0
ENDIF
nCols := ROUND( nPercent * nBarRatio, 0 )
@ aHandle[ B_TOP ] + 1, aHandle[ B_LEFT ] + 1 CLEAR TO ;
  aHandle[ B_BOTTOM ] - 1, aHandle[ B_RIGHT ] - 1
FOR nRow := 1 TO (aHandle[ B_BOTTOM ] - aHandle[ B_TOP ] - 1)
 @ nRow + aHandle[ B_TOP ], aHandle[ B_LEFT ] + 1 SAY ;
   REPLICATE( aHandle[ B_BARCHAR ], nCols )
NEXT
SETCOLOR( cOldColor )
RETURN( aHandle )

FUNCTION VGAUGE
PARAMETERS hgauge,vpor
nPercent+=vpor
IF nPercent < 0
 TONE(300, 1)
 nPercent := 0
ENDIF
IF nPercent > 1
 TONE(300, 1)
 nPercent := 1
ENDIF
GAUGEUPDATE(hGauge,nPercent)
RETURN []

FUNCTION EnCurso
PARAMETERS fArch
LOCAL cCompleta := LTRIM(STR((RECNO()/LASTREC()*100)))
@18,20 SAY CENTRA("Indexando "+fArch+"..."+cCompleta+" %",40)
RETURN .T.
