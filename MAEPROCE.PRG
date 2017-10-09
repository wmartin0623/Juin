*Control codigo Principal de cada archivo
PROCEDURE proced0101
vcod=STRZERO(val(v&mcampo),vlon)
SELECT &alia
SEEK vcod
IF FOUND()
 TONO(2)
 colo_a=SETCOLOR()
 SETCOLOR(color3)
 CARTEL('­ ESE CODIGO YA EXISTE !  [Ok] ',vfila+1,1,'N')
 SETCOLOR(colo_a)
 ok=1
ELSE
 v&mcampo=vcod
ENDIF
RETURN

**Control Tipo de Cliente
PROCEDURE proced0102
if v&mcampo<>`1'.and.v&mcampo<>`2'
 ok=1
ENDIF
RETURN

*Control Codigo
PROCEDURE proced0103
vcod=PADL(v&mcampo,vlon,' ')
SELECT &alia
SEEK vcod
IF FOUND()
 TONO(2)
 colo_a=SETCOLOR()
 SETCOLOR(color3)
 CARTEL('­ ESE CODIGO YA EXISTE !  [Ok] ',vfila+1,1,'N')
 SETCOLOR(colo_a)
 ok=1
ELSE
 v&mcampo=vcod
ENDIF
RETURN

**Control codigo Departam. para clientes y Proveedores
PROCEDURE proced0104
colu=SETCOLOR()
antsel=ALLTRIM(STR(SELECT()))
AREA()
IF USAR("MAEDEPAR","DPTOS")
 IF !FILE("MAEDEPAR.NTX")
  INDEX ON dpto TO MAEDEPAR
 ENDIF
 SET INDEX TO MAEDEPAR
ENDIF
IF RECCOUNT()=0
 pan = SAVESCREEN(20,31,23,60)
 SETCOLOR(INVER)
 @20,31 CLEAR TO 23,60
 @20,31 TO 23,60 DOUBLE
 @21,36 SAY '   PRESTE ATENCION  '
 @22,32 SAY 'Ä ARCHIVO DEPARTAM.  VACIO Ä'
 SET CURSOR OFF
 TONO(2)
 INKEY(0)
 SET CURSOR ON
 SETCOLOR(colu)
 RESTSCREEN(20,31,23,60,pan)
 ok=1
 USE
 SELECT &antsel
 RETURN
ENDIF
SET ORDER TO 1
vdep=STRZERO(VAL(v&mcampo),3)
SEEK vdep
IF !FOUND()
 CARTEL(`DEPARTAMENTO... INEXISTENTE  [ OK ]',18,1,'N',color3)
 ok=1
ELSE
 v&mcampo=vdep
ENDIF
USE
SELECT &antsel
SETCOLOR(colu)
DO PROCED0114
RETURN

**Control codigo provincia para clientes y Proveedores
PROCEDURE proced0105
antsel=ALLTRIM(STR(SELECT()))
AREA()
colu=SETCOLOR()
IF USAR("MAEPROVI","PROVIN")
 IF !FILE("MAEPROVI.NTX")
  INDEX ON pcia TO MAEPROVI
 ENDIF
 SET INDEX TO MAEPROVI
ENDIF
IF RECCOUNT()=0
 pan = SAVESCREEN(20,31,23,60)
 SETCOLOR(INVER)
 @20,31 CLEAR TO 23,60
 @20,31 TO 23,60 DOUBLE
 @21,36 SAY '   PRESTE ATENCION  '
 @22,32 SAY 'Ä ARCHIVO PROVINCIAS VACIO Ä'
 SET CURSOR OFF
 TONO(2)
 INKEY(0)
 SET CURSOR ON
 SETCOLOR(colu)
 RESTSCREEN(20,31,23,60,pan)
 ok=1
 USE
 SELECT &antsel
 RETURN
ENDIF
SET ORDER TO 1
vpro=STRZERO(VAL(v&mcampo),2)
SEEK vpro
IF !FOUND()
 CARTEL(`PROVINCIA... INEXISTENTE  [ OK ]',18,1,'N',color3)
 ok=1
ELSE
 v&mcampo=vpro
 * vExto=exto
 * vR122=r122
 * IF vExto="S"
 *  vCiva="4"
 * ENDIF
 * zPib1=pib1
 * zPib2=pib2
 * zPib3=pib3
 * zPib4=pib4
 * IF Opc1=1   &&Clientes
 *  IF vR122="N"
 *   n=n+1
 *   vIbru="0"
 *   @12,21 SAY vIbru
 *  ENDIF
 *
 *  @12,21 SAY vIbru+' '+IIF(vIbru="1",STR(Pib1,5,2),;
 *                       IIF(vIbru="2",STR(Pib2,5,2),;
 *                       IIF(vIbru="3",STR(Pib3,5,2),;
 *                       IIF(vIbru="4",STR(Pib4,5,2)," 0,00"))))
 * ENDIF
ENDIF
USE
SELECT &antsel
SETCOLOR(colu)
RETURN

**Control Condicion IVA
PROCEDURE PROCED0106
IF v&mcampo<>`1'.AND.v&mcampo<>`2'.AND.v&mcampo<>`3'.AND.v&mcampo<>`4'.AND.v&mcampo<>`5';
 .AND.v&mcampo<>`6'.AND.v&mcampo<>`7'
 ok=1
ELSE
 *  IF vExto="S".AND.v&mCampo#"4"
 *   CARTEL("PROVINCIA EXENTA, CLIENTE DEBE SER EXENTO [ OK ]",12,1,'N',color3)
 *   ok=1
 *  ENDIF
ENDIF
RETURN

**Agrega ceros al Codigo de Zona
procedure proced0107
v&mcampo = CERO(VAL(v&mcampo),3)
@vfila,vcol SAY v&mcampo
RETURN

*Control Estado del Cliente
PROCEDURE PROCED0115
v&mcampo=UPPER(v&mcampo)
IF v&mcampo<>'C'.AND.v&mcampo<>'M'.AND.v&mcampo<>'I'
 ok=1
ENDIF
RETURN
*Control Codigo Postal
PROCEDURE proced0114
antsel=ALLTRIM(STR(SELECT()))
AREA()
colu=SETCOLOR()
IF USAR("MAECPOST","CPOST")
 IF !FILE("MAECPOST.NTX")
  INDEX ON pcia+dpto TO MAECPOST
 ENDIF
 SET INDEX TO MAECPOST
ENDIF
IF RECCOUNT()=0
 pan = SAVESCREEN(20,31,23,60)
 SETCOLOR(INVER)
 @20,31 CLEAR TO 23,60
 @20,31 TO 23,60 DOUBLE
 @21,36 SAY '   PRESTE ATENCION  '
 @22,32 SAY 'Ä ARCHIVO COD.POSTAL VACIO Ä'
 SET CURSOR OFF
 TONO(2)
 INKEY(0)
 SET CURSOR ON
 SETCOLOR(colu)
 RESTSCREEN(20,31,23,60,pan)
 ok=1
 USE
 SELECT &antsel
 RETURN
ENDIF
SET ORDER TO 1
SEEK vpcia+vdpto
IF FOUND()
 vcpos=cpos
ENDIF
USE
SELECT &antsel
SETCOLOR(colu)
RETURN

*Control
PROCEDURE PROCED0402
v&mcampo=UPPER(v&mcampo)
IF v&mcampo<>"S".and.v&mcampo<>"N"
 ok=1
ENDIF
RETURN

**Control codigo Rubro en Listas
PROCEDURE proced0201
colu=SETCOLOR()
antsel=ALLTRIM(STR(SELECT()))
AREA()
IF USAR("MAERUBRO","RUBRO")
 IF !FILE("MAERUBRO.NTX")
  INDEX ON rubr TO MAERUBRO
 ENDIF
 SET INDEX TO MAERUBRO
ENDIF
IF RECCOUNT()=0
 pan = SAVESCREEN(20,31,23,60)
 SETCOLOR(INVER)
 @20,31 CLEAR TO 23,60
 @20,31 TO 23,60 DOUBLE
 @21,36 SAY '   PRESTE ATENCION  '
 @22,32 SAY 'Ä ARCHIVO RUBROS     VACIO Ä'
 SET CURSOR OFF
 TONO(2)
 INKEY(0)
 SET CURSOR ON
 SETCOLOR(colu)
 RESTSCREEN(20,31,23,60,pan)
 ok=1
 USE
 SELECT &antsel
 RETURN
ENDIF
SET ORDER TO 1
vRubr=PADL(v&mcampo,vlon," ")
SEEK vRubr
IF !FOUND()
 CARTEL("RUBROO... INEXISTENTE  [ OK ]",18,1,"N",color3)
 ok=1
ELSE
 v&mcampo=vRubr
ENDIF
USE
SELECT &antsel
SETCOLOR(colu)
RETURN

*************************************************************

*Pedir Importe Minimo Resolucion 16/07
PROCEDURE proced0401
v&mcampo=UPPER(v&mcampo)
IF v&mcampo<>`S'.and.v&mcampo<>`N'
 ok=1
 RETURN
ENDIF
IF v&mcampo="N"
 n++
ENDIF
RETURN

**Control Lista en IVA cuenta contable yProveedores cuando Hacemos Retencion
PROCEDURE PROCED1202
v&mcampo=UPPER(v&mcampo)
IF v&mcampo<>`S'.and.v&mcampo<>`N'
 ok=1
ENDIF
RETURN

**Control Tipo Documento
PROCEDURE PROCED0501
IF v&mcampo<>'   '.AND.v&mcampo<>'DNI'.AND.v&mcampo<>'LE '.AND.v&mcampo<>'LC '.AND.v&mcampo<>'CE '.AND.v&mcampo<>'CI '
 ok=1
ENDIF
RETURN

PROCEDURE proced9999
vcod=v&mcampo
IF !F_VALICUI(vCod)
 CARTEL('­ CUIT ERRONEO !  [Ok] ',vfila+1,1,'N')
 ok = 1
ENDIF
RETURN
