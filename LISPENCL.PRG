*Prg. p/listar facturas pendientes de un  mes en especial.
PRIVATE pa_an,co_an,con_aux
pa_an=SAVESCREEN(0,0,24,79)
co_an=SETCOLOR()
*clientes
AREA()
OPEN_DBF("MAECLIEN","CLIENTES","C","MAECLIEN","clie")

*Cuenta Corriente
AREA()
IF USAR('CCCLIENT','CTACTE')
 IF !FILE("CCCLIE07.NTX")
  INDEX ON DTOS(fech)+clie+tcom+ltra+srie+nume TO CCCLIE07
 ENDIF
 SET INDEX TO CCCLIE07
ELSE
   QUIT
ENDIF
SET RELATION TO clie INTO CLIENTES
DO WHILE .T.
 nro_mes=0
 nro_ano=0
 SETCOLOR(color3)
 SOMBRA(8,9,15,64,'H1',1,'',color3)
 @16,9 SAY CENTRA('[Esc] para Terminar',55)
 @10,10 SAY'Ingrese MES  (mm) de  las Facturas a Listar : 'GET nro_mes PICTURE '@K 99' RANGE 1,12
 @14,10 SAY'Ingrese ANO (aaaa) de las Facturas a Listar : 'GET nro_ano PICTURE '@K 9999' RANGE 1994,2021
 READ
 IF LASTKEY()=27
  EXIT
 ENDIF
 O1=0
 IF !IMPRI()
  o1=1
 ENDIF
 IF LASTKEY()=27
  LOOP
 ENDIF
 IF CONFIR()
  SELECT CTACTE
  DIA1=CTOD('01/'+CERO(nro_mes,2)+'/'+CERO(nro_ano,4))
  empr=nom_emp
  mes=meses(DIA1)
  titul='COMPROBANTES PENDIENTES : '+mes+'-'+CERO(nro_ano,4)
  pag=0
  stitul=''
  con_aux=CERO(nro_mes,2)+'/'+CERO(nro_ano,4)
  condi="SUBSTR(DTOC(fech),4,7)=con_aux.AND.!canc.AND.SACT<>0.AND.!baja"
  DECLARE cam[6],sep[6],long[6],colum[6],toti[6],dec[6]
  CARGA1(cam,"tcom~ltra+srie+nume~DTOC(fech)~clie~CLIENTES->nomb~STR(sact,12,2)")
  CARGA2(sep,"1,1,1,1,1,0")
  CARGA2(long,"4,13,10,6,30,12")
  CARGA1(toti,"F~F~F~F~F~T")
  CARGA2(dec,'0,0,0,0,0,2')
  grup="DTOC(fech)"
  ti_grup="Dia   :"
  CARGA1(colum,"Tipo~N£mero~Fecha~Clie.~Apellido y nombre~Saldo")
  IF o1=1
   LISTA(cam,sep,long,colum,titul,stitul,condi,empr,.T.,dia_list,pag,toti,dec,grup,ti_grup)
  ELSE
   INFORME(cam,sep,long,colum,titul,condi,empr,grup,ti_grup)
  ENDIF
 ENDIF
ENDDO
SET PRINT OFF
CLOSE ALL
IF FILE("CCCLIE07.NTX")
 DELETE FILE CCCLIE07.NTX
ENDIF
RETURN
