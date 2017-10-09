*Actualizar Campo SACT del Archigvo MAECLIEN a partir del Archivo CCCLIENT
PRIVATE Col_Ant,Pan_Ant
Col_Ant=SETCOLOR()
Pan_Ant=SAVESCREEN(0,0,24,79)

*clientes
AREA()
OPEN_DBF("MAECLIEN","CLIENTES","E","MAECLIEN","clie","NOMCLIEN","nomb")
REPLACE ALL sact WITH 0
CLOSE
OPEN_DBF("MAECLIEN","CLIENTES","C","MAECLIEN","clie","NOMCLIEN","nomb")


*CUENTA CORRIENTE
AREA()
IF USAR('CCCLIENT','CTACTE')
 IF !FILE('CCCLIE01.NTX')
  INDEX ON clie+DTOS(fech) TO CCCLIE01
 ENDIF
 SET INDEX TO CCCLIE01
ELSE
 RETURN
ENDIF
SELECT CTACTE
*Totalizar por Cliente
CARTEL("ESPERE.... ESTOY TRABAJANDO",12,1,"E",color3)
TOTAL ON clie FIELD paga TO TOTALCL FOR !baja.AND. tcom#"RT"
*Abrir Archivo con Totales
IF !USAR("TOTALCL","TOTALCL")
 CARTEL("NO PUDE CREAR TOTALES [ OK ]",12,1,'N',color3)
 CLOSE ALL
 RETURN
ENDIF

GO TOP

SETCOLOR(color3)
SOMBRA(12,38,16,60,'H1',1,'',color3)
vtot=CERO(RECCOUNT(),5)
SETCOLOR("W+/R")
@13,40 SAY "Reg.Actual    Total "
vcan=1
DO WHILE .NOT.EOF()
 vclie=clie
 vsact=paga
 SELECT CLIENTES
 SEEK vclie
 IF FOUND()
  RECLOCK(1)
  REPLACE sact WITH vSact
  UNLOCK
 ENDIF
 SELECT TOTALCL
 @14,43 SAY CERO(vcan,5)+'   /  '+vtot
 @15,43 SAY vSact
 vcan=vcan+1
 SKIP
ENDDO
CLOSE ALL
SETCOLOR(Col_Ant)
RESTSCREEN(0,0,24,79,Pan_Ant)
RETURN
