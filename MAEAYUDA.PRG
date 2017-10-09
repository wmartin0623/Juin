FUNCTION ayudare
PARAMETERS campi,ntx1,ntx2,l1,l2,camp1,archi
SET KEY -2 TO
SET KEY 24 TO
SET KEY 5 TO
coli=SETCOLOR()
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
 SETCOLOR(coli)
 RESTSCREEN(20,31,23,61,pan)
 ok=1
 USE
 SELECT &antsel
 SET KEY -2 TO &vayuda
 SET KEY 24 TO avance
 SET KEY 5 TO retro
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
SETCOLOR(coli)
SET ORDER TO &ord_ant
RESTSCREEN(5,29,24,78,pan)
*RELEASE cam,sep,ind,lar,ord_ant
IF LASTKEY()=27
 ok=1
 USE
 SELECT &antsel
 SET KEY -2 TO &vayuda
 SET KEY 24 TO avance
 SET KEY 5 TO retro
 RETURN .F.
ELSE
 v&mcampo=&camp1
 @vfila,vcol SAY &camp1
ENDIF
IF alia="PROVE"
 vcod=PADL(v&mcampo,4,' ')
ELSE
 vcod=STRZERO(VAL(v&mcampo),l1)
ENDIF
v&mcampo=vcod
USE
SELECT &antsel
SET KEY -2 TO &vayuda
SET KEY 24 TO avance
SET KEY 5 TO retro
RETURN .T.

*Ayuda de   rubros en listas
FUNCTION ayuda0201
PRIVATE campi,ntx1,ntx2,l1,l2,camp1,archivo,antsel
antsel=ALLTRIM(STR(SELECT()))
AREA()
*MAESTRO DE rubros
IF USAR('MAERUBRO','RUBRO')
 IF !FILE('MAERUBRO&vRDD.')
  INDEX ON RUBR TO MAERUBRO
 ENDIF
 IF !FILE('NOMRUBRO&vRDD.')
  INDEX ON nomb TO NOMRUBRO
 ENDIF
 SET INDEX TO MAERUBRO,NOMRUBRO
ELSE
 QUIT
ENDIF
archivo='RUBRO'
campi='rubr~nomb'
ntx1=1
ntx2=2
l1=4
l2=20
camp1='rubr'
IF AYUDARE(campi,ntx1,ntx2,l1,l2,camp1,archivo) = .T.
 RETURN .T.
ELSE
 RETURN .F.
ENDIF

*Ayuda de PROVINCIAS para CLIENTES
FUNCTION ayuda0103
antsel=ALLTRIM(STR(SELECT()))
AREA()
*MAESTRO DE PROVINICAS
IF USAR('MAEPROVI','PROVIN')
   IF !FILE('MAEPROVI.NTX')
      INDEX ON pcia TO MAEPROVI
   ENDIF
   IF !FILE('NOMPROVI.NTX')
      INDEX ON NOMB TO NOMPROVI
   ENDIF
   SET INDEX TO MAEPROVI,NOMPROVI
ELSE
   QUIT
ENDIF
archivo='PROVINCIA'
campi='pcia~nomb'
ntx1=1
ntx2=2
l1=2
l2=15
camp1='pcia'
IF AYUDARE(campi,ntx1,ntx2,l1,l2,camp1,archivo) = .T.
 RETURN .T.
ELSE
 RETURN .F.
ENDIF

*Ayuda de DEPARTAMENTOS para CLIENTES
FUNCTION ayuda0104
antsel=ALLTRIM(STR(SELECT()))
AREA()
*MAESTRO DE DEPARTAMENTOS
IF USAR('MAEDEPAR','DPTOS')
   IF !FILE('MAEDEPAR.NTX')
      INDEX ON dpto TO MAEDEPAR
   ENDIF
   IF !FILE('NOMDEPAR.NTX')
      INDEX ON NOMB TO NOMDEPAR
   ENDIF
   SET INDEX TO MAEDEPAR,NOMDEPAR
ELSE
   QUIT
ENDIF
archivo='DEPARTAMENTOS'
campi='dpto~nomb'
ntx1=1
ntx2=2
l1=3
l2=25
camp1='dpto'
IF AYUDARE(campi,ntx1,ntx2,l1,l2,camp1,archivo) = .T.
 RETURN .T.
ELSE
 RETURN .F.
ENDIF
