*Listado totalizador: muestra el saldo por clientes a una fecha determinada
PRIVATE pan_ant,col_ant
pan_ant=SAVESCREEN(,,,,)
col_ant=SETCOLOR()

*clientes
OPEN_DBF("MAECLIEN","CLIENTES","C","MAECLIEN","clie","NOMCLIEN","nomb")

*CUENTA CORRIENTE
AREA()
IF USAR('CCCLIENT','CTACTE')
 IF !FILE("CCCLIE01.NTX")
  INDEX ON clie+DTOS(fech) TO CCCLIE01
 ENDIF
 SET INDEX TO CCCLIE01
ELSE
 QUIT
ENDIF

PRIVATE vfech,vDbf:="",vDbf1:="",vNtx:=""
vfech=DATE()
SETCOLOR(colo3)
SOMBRA(10,10,12,40,"H1",1,'',colo3)
@10,12 SAY "*** SALDO DE CLIENTES ***" COLOR colo12
@11,11 SAY "A que D¡a: " GET vfech VALID !EMPTY(vfech)
READ
IF LASTKEY()#27
 *Totalizar a la fecha solicitada
 *Totalizar en TEMP.DBF
 vDbf=ARCH_EXI("TEMP","DBF")
 TOTAL ON clie+DTOS(fech) FIELD paga TO &vDbf FOR fech<=vfech.AND.!baja

 *Abrir archivo de totales
 IF FILE("&vDbf")
  OPEN_DBF("&vDbf","TEMP","C")
  vNtx=ARCH_EXI("BORRAR","NTX")
  INDEX ON clie TO &vNtx
  vDbf1=ARCH_EXI("TEMP1","DBF")
  TOTAL ON clie FIELD paga TO &vDbf1
  CLOSE
  IF FILE("&vDbf1")
   OPEN_DBF("&vDbf1","TEMP","C")
  ENDIF
  GO TOP
  SET RELATION TO clie INTO CLIENTES
 ELSE
  IF RECCOUNT()=0
   RESTSCREEN(,,,,pan_ant)
   SETCOLOR(col_ant)
   CLOSE ALL
   FERASE("&vDbf")
   FERASE("&vDbf1")
   FERASE("&vNtx")
   RETURN
  ENDIF
 ENDIF
 IF !IMPRI()
  o1=1
 ELSE
  o1=0
 ENDIF

 IF CONFIR()
  empr=nom_emp
  titul='SALDO DE CLIENTES al '+DTOC(vfech)
  sTitul=""
  DECLARE cam[3],sep[3],long[3],colum[3],toti[3],deci[3]
  carga1(cam,"clie~CLIENTES->nomb~STR(paga,12,2)")
  carga2(sep,"6,6,0")
  carga2(long,"7,30,12")
  carga1(colum,"Cliente~Apellido y Nombre~Saldo")
  CARGA1(toti,"F~F~T")
  CARGA2(deci,"0,0,2")
  condi=""
  IF o1=1
   LISTA(cam,sep,long,colum,titul,stitul,condi,empr,.T.,dia_li,pag,toti,deci)
  ELSE
   INFO_TOT(cam,sep,long,colum,titul,sTitul,condi,empr,toti,deci)
  ENDIF
 ENDIF
ENDIF
RESTSCREEN(,,,,pan_ant)
SETCOLOR(col_ant)
CLOSE ALL
FERASE("&vDbf")
FERASE("&vDbf1")
FERASE("&vNtx")
RETURN
