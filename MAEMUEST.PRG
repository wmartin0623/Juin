*Muestra Si o No
PROCEDURE muest0102
td=v&mcampo
@vfila,vcol+2 SAY IIF(td,'SI',IIF(!td,'NO','  '))
RETURN

*Muestra Condicion iva
PROCEDURE muest0106
td=v&mcampo
@vfila,vcol+2 SAY IIF(td='1','R.I.',IIF(td='2','RNI.',IIF(td='3','C.F.',IIF(td='4','EXE.',IIF(td='5','M.T.',;
     IIF(td='6','N.A.',IIF(td='7','N.C.','    ')))))))
RETURN

************************************************
*Muestra TIPO DE CLIENTE
PROCEDURE muest0108
td=v&mcampo
@vfila,vcol+2 SAY IIF(td='1','C.C.',IIF(td='2','CDO.','    '))
RETURN

**Impresion nombre PROVINCIAS
PROCEDURE muest0103
antsel=ALLTRIM(STR(SELECT()))
AREA()
IF USAR("MAEPROVI","PROVIN")
 IF !FILE("MAEPROVI.NTX")
  INDEX ON pcia TO MAEPROVI
 ENDIF
 SET INDEX TO MAEPROVI
ENDIF
vpro=STRZERO(VAL(v&mcampo),2)
SEEK vpro
IF FOUND()
 v&mcampo=vpro
 @vfila,vcol+3 SAY sigl
 IF opc1=1
  *Si viene de Clientes
  IF vIbru#"0".AND.!EMPTY(vIbru)
   xx="pib"+vIbru
   @vFila,vCol+8 SAY vIbru+" "+STR(&xx,5,2)
  ELSE
   @vFila,vCol+8 SAY SPACE(7)
  ENDIF
 ENDIF
ELSE
 @vfila,vcol+3 SAY '...'
ENDIF
USE
SELECT &antsel
RETURN

**Impresion nombre DEPARTAMENTOS
PROCEDURE muest0104
antsel=ALLTRIM(STR(SELECT()))
AREA()
IF USAR("MAEDEPAR","DPTOS")
 IF !FILE("MAEDEPAR.NTX")
  INDEX ON dpto TO MAEDEPAR
 ENDIF
 SET INDEX TO MAEDEPAR
ENDIF
vdep=STRZERO(VAL(v&mcampo),3)
SEEK vdep
IF FOUND()
 v&mcampo=vdep
 @vfila,vcol+4 SAY sigl+SPACE(5)
ELSE
 @vfila,vcol+4 SAY 'NO EXISTE'
ENDIF
USE
SELECT &antsel
RETURN

*Muestra Condicion iva
PROCEDURE muest0109
td=v&mcampo
@vfila,vcol+2 SAY IIF(td='1','R.I.',IIF(td='2','RNI.',IIF(td='3','C.F.',IIF(td='4','EXE.',IIF(td='5','M.T.',;
     IIF(td='6','N.A.',IIF(td='7','N.C.','    ')))))))
RETURN

*Muestra IMPORTE ACREDITADO A UN CLIENTE pero NEGATIVO
PROCEDURE muest0107
td=v&mcampo
@vfila,vcol SAY td*(-1) PICTURE "&vmasc"
RETURN

*Muestra Esatado del cliente Moroso o Bien
PROCEDURE muest0113
td=v&mcampo
colk=SETCOLOR()
IIF(td='M',SETCOLOR(color3),SETCOLOR(colk))
@7,61 SAY IIF(td='C','Bien',IIF(td='M','Moro',IIF(td='I','Inac',SPACE(4))))
SETCOLOR(colk)
RETURN
************************************************


**Impresion nombre RUBRO en listas
PROCEDURE muest0201
antsel=ALLTRIM(STR(SELECT()))
AREA()
IF USAR("MAERUBRO","RUBRO")
 IF !FILE("MAERUBRO.NTX")
  INDEX ON RUBR TO MAERUBRO
 ENDIF
 SET INDEX TO MAERUBRO
ENDIF
vRubr=v&mcampo
SEEK vRubr
IF FOUND()
 v&mcampo=vRubr
 @vfila,vcol+5 SAY nomb
ELSE
 @vfila,vcol+5 SAY 'NO EXISTE        '
ENDIF
USE
SELECT &antsel
RETURN

*Muestra Lista en IVA cuenta contable
PROCEDURE muest1203
td=v&mcampo
@vfila,vcol+2 SAY IIF(td='S','Si','No')
RETURN
