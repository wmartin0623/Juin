*Menu de maestros
PUBLIC alia,estructu,titu,vfila,vcol,camp,long1,long2,tit,vayuda,antsel
PUBLIC vart,vinicio
SET PROCEDURE TO maepant
SET PROCEDURE TO maeproce
SET PROCEDURE TO maemuest
SET PROCEDURE TO maeayuda
SET KEY -4 TO va_tras           &&[F5]
DO CASE
 CASE opc1 = 1
  *Rubros
   AREA()
   IF USAR('MAERUBRO','RUBROS')
    IF !FILE('MAERUBRO&vRDD.')
     INDEX ON rubr TO MAERUBRO
    ENDIF
    IF !FILE('NOMRUBRO&vRDD.')
     INDEX ON nomb TO NOMRUBRO
    ENDIF
    SET INDEX TO MAERUBRO,NOMRUBRO
   ELSE
      QUIT
   ENDIF
   alia='RUBROS'
   estructu='01'
   titu='RUBROS'
   camp='rubr~nomb'
   tit='Codigo    Descripciขn        '
   long1=4
   long2=20
   vinicio = 99999999

 CASE opc1 = 2
  *Listas de Precios
  AREA()
  IF USAR('MAEPROVE','PROVE')
   IF !FILE('MAEPROVE&vRDD.')
    INDEX ON prov TO MAEPROVE
   ENDIF
   IF !FILE('NOMPROVE&vRDD.')
    INDEX ON nomb TO NOMPROVE
   ENDIF
   SET INDEX TO MAEPROVE,NOMPROVE
  ELSE
     QUIT
  ENDIF
  alia='PROVE'
  estructu='02'
  titu='LISTA DE PRECIOS'
  camp='prov~nomb'
  tit='Cขdigo    Descripciขn        '
  long1=4
  long2=30
  vinicio = 1

 CASE opc1 = 3
  *MARCAS
  AREA()
  IF USAR('MAEMARCA','MARCAS')
   IF !FILE('MAEMARCA&vRDD.')
    INDEX ON mrca TO MAEMARCA
   ENDIF
   IF !FILE('NOMMARCA&vRDD.')
    INDEX ON nomb TO NOMMARCA
   ENDIF
   SET INDEX TO MAEMARCA,NOMMARCA
  ELSE
     QUIT
  ENDIF
  alia='MARCAS'
  estructu='03'
  titu='MARCAS DE ARTICULOS'
  camp='mrca~nomb'
  tit='Cขdigo    Descripciขn        '
  long1=2
  long2=10
  vinicio = 1

 CASE opc1 = 5
  *Clientes
   AREA()
   IF USAR('MAECLIEN','CLIEN')
    IF !FILE('MAECLIEN&vRDD.')
     INDEX ON clie TO MAECLIEN
    ENDIF
    IF !FILE('NOMCLIEN&vRDD.')
     INDEX ON nomb TO NOMCLIEN
    ENDIF
    SET INDEX TO MAECLIEN,NOMCLIEN
   ELSE
      QUIT
   ENDIF
   alia='CLIEN'
   estructu='05'
   titu='CLIENTES'
   camp='clie~nomb'
   tit='Codigo    Apellido y Nombre  '
   long1=6
   long2=30
   vinicio = 1

 CASE opc1 = 6
  *Bancos
   AREA()
   IF USAR('MAEBANCO','BANCO')
    IF !FILE('MAEBANCO&vRDD.')
     INDEX ON banc TO MAEBANCO
    ENDIF
    IF !FILE('NOMBANCO&vRDD.')
     INDEX ON nomb TO NOMBANCO
    ENDIF
    SET INDEX TO MAEBANCO,NOMBANCO
   ELSE
      QUIT
   ENDIF
   alia='BANCO'
   estructu='06'
   titu='BANCOS'
   camp='banc~nomb'
   tit='Codigo    Nombre del Banco   '
   long1=2
   long2=20
   vinicio = 1

 CASE opc1 = 7
    *PLAZAS
    AREA()
    IF USAR('MAEPLAZA','PLAZA')
     IF !FILE('MAEPLAZA.NTX')
      INDEX ON plaz TO MAEPLAZA
     ENDIF
     IF !FILE('NOMPLAZA.NTX')
      INDEX ON NOMB TO NOMPLAZA
     ENDIF
     SET INDEX TO MAEPLAZA,NOMPLAZA
    ELSE
       QUIT
    ENDIF
    alia='PLAZA'
    estructu='07'
    titu='PLAZAS'
    camp="plaz~nomb+' '+pcia"
    tit='Codigo    Descripciขn / Provincia'
    long1=3
    long2=25
    vinicio = 1

 CASE opc1 = 8
  *Provincias
   AREA()
   IF USAR('MAEPROVI','PCIAS')
    IF !FILE('MAEPROVI&vRDD.')
     INDEX ON pcia TO MAEPROVI
    ENDIF
    IF !FILE('NOMPROVI&vRDD.')
     INDEX ON nomb TO NOMPROVI
    ENDIF
    SET INDEX TO MAEPROVI,NOMPROVI
   ELSE
      QUIT
   ENDIF
   alia='PCIAS'
   estructu='08'
   titu='PROVINCIAS'
   camp='pcia~nomb'
   tit='Codigo    Nombre Provincia   '
   long1=2
   long2=15
   vinicio = 1

 CASE opc1 = 9
  *Departamentos
   AREA()
   IF USAR('MAEDEPAR','DPTOS')
    IF !FILE('MAEDEPAR&vRDD.')
     INDEX ON dpto TO MAEDEPAR
    ENDIF
    IF !FILE('NOMDEPAR&vRDD.')
     INDEX ON nomb TO NOMDEPAR
    ENDIF
    SET INDEX TO MAEDEPAR,NOMDEPAR
   ELSE
      QUIT
   ENDIF
   alia='DPTOS'
   estructu='09'
   titu='DEPARTAMENTOS'
   camp='dpto~nomb'
   tit='Codigo    Nombre Departamento'
   long1=3
   long2=15
   vinicio = 1

 CASE opc1 = 10
  *Codigos Postales
  AREA()
  IF USAR('MAECPOST','CPOST')
   IF !FILE('MAECPOST.NTX')
    INDEX ON pcia+dpto TO MAECPOST
   ENDIF
   SET INDEX TO MAECPOST
  ELSE
     QUIT
  ENDIF

 CASE opc1 = 11
  *Condiciones de Pago en Remitos
  AREA()
  IF USAR('MAEREMIT','REMIT')
   IF !FILE('MAEREMIT.NTX')
    INDEX ON nopc TO MAEREMIT
   ENDIF
   IF !FILE('NOMREMIT.NTX')
    INDEX ON cpag TO NOMREMIT
   ENDIF
   SET INDEX TO MAEREMIT,NOMREMIT
  ELSE
     QUIT
  ENDIF
  alia='REMIT'
  estructu='11'
  titu='CONDICIONES DE PAGO EN REMITOS'
  camp='nopc~cpag'
  tit='Codigo    Descripciขn        '
  long1=2
  long2=30
  vinicio = 1

ENDCASE
IF opc1=4
 DO MAEARTIC
ELSE
 IF opc1 = 10
  DO MAECPOST
 ELSE
  DO MAE_ABMC
 ENDIF
ENDIF
CLOSE ALL
RETURN

FUNCTION ayud1
PARAMETERS campi,ntx1,ntx2,l1,l2,camp1,archi
PRIVATE col_anti
col_anti = SETCOLOR()
IF RECCOUNT()=0
 pan = SAVESCREEN(20,31,23,61)
 SETCOLOR(INVER)
 @20,31 CLEAR TO 23,61
 @20,31 TO 23,61 DOUBLE
 @21,36 SAY '   PRESTE ATENCION  '
 @22,32 SAY 'ฤ ARCHIVO '+archi+' VACIO ฤ'
 SET CURSOR OFF
 TONO(2)
 INKEY(0)
 SET CURSOR ON
 SETCOLOR(col_anti)
 RESTSCREEN(20,31,23,61,pan)
 RETURN .F.
ENDIF
SET ORDER TO 1
pan = SAVESCREEN(5,29,24,78)
SETCOLOR(color3)
SOMBRA(5,29,23,77,'H1',1,'',color3)
SETCOLOR(color3)
@ 7,29  SAY 'ฬอออออออออออออออออออออออออออออออออออออออออออออออน'
@ 20,29 SAY 'วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ'
@ 17,29 SAY 'ฬอออออออออออออออออออออออออออออออออออออออออออออออน'
@ 5,41 SAY ' BUSCAR '
@18,30 SAY 'BUSCAR:'
@ 21,30 SAY CHR(24)+CHR(25)+' PgUp PgDn mover          '+CHR(17)+'ฤู-ACEPTA'
@ 22,30 SAY "F2-cambia clave      ESC-salir"
DECLARE cam[2],sep[2],ind[2],lar[2],nom[2]
carga1(cam,campi)
carga2(sep,'10,0')
titul1=archi+'          NOMBRE      '
ind[1] = ntx1
lar[1] = l1
nom[1] = archi
ind[2] = ntx2
lar[2] = l2
nom[2] = 'NOMBRE :'
ord_ant = STR(INDEXORD())
SET ORDER TO l2
GO TOP
SETCOLOR(color3)
BUSCARE(6,31,16,75,cam,titul1,sep,ind,lar,19,31,nom,18,41)
SETCOLOR(col_anti)
SET ORDER TO &ord_ant
RESTSCREEN(5,29,24,78,pan)
*RELEASE cam,sep,ind,lar,ord_ant
IF LASTKEY()=27
 RETURN .F.
ENDIF
RETURN .T.

*Busca el primer codigo libre en la base de datos Activa
*CAMPO = Nombre del campo clave Numerico que debe buscar
FUNCTION LIBRE
PARAMETERS campo,p
IF PCOUNT() = 1
 PRIVATE p
 p=1
ENDIF
GO TOP
vcam = &campo
DO WHILE .NOT.EOF()
 vcam = &campo
 IF VAL(vcam) = p
  SKIP
 ELSE
*  SEEK vcam
 * IF FOUND()
*   p=p+1
*   LOOP
*  ENDIF
  EXIT
 ENDIF
 p=p+1
ENDDO
RETURN (p)

FUNCTION MAE_CIVA(IvaC,IvaNom)
PRIVATE pan_ant,colo_a
IF VAL(IvaC) < 1 .OR. VAL(IvaC) > 7
 colo_a=SETCOLOR()
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
 IvaC=STR(civ,1)
ENDIF
IvaNom=IIF(IvaC='1','R.I.',IIF(IvaC='2','RNI.',IIF(IvaC='3','C.F.',IIF(IvaC='4','EXE.',IIF(IvaC='5','M.T.',;
     IIF(IvaC='6','N.A.',IIF(IvaC='7','N.C.','    ')))))))
RETURN .T.
