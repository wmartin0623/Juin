*ARCHIVO CLIENTES
PARAMETERS cual
PRIVATE pant_ant,col_ant
pant_ant=SAVESCREEN(0,0,24,79)
col_ant=SETCOLOR()
PUBLIC vdomi
*clientes
AREA()
OPEN_DBF("MAECLIEN","CLIENTES","C","MAECLIEN","clie","NOMCLIEN","nomb")

SELECT CLIENTES
DO WHILE .T.
 SOMBRA(10,10,14,55,'V1',1,'',color3)
 vdomi=SPACE(25)
 SETCOLOR(color3)
 @12,12 SAY 'Que DOMICILIO : ' GET vdomi PICTURE'@!'
 READ
 IF EMPTY(vdomi) .OR. LASTKEY()=27
  RESTSCREEN(0,0,24,79,pant_ant)
  SETCOLOR(col_ant)
  CLOSE ALL
  RETURN
 ENDIF
 o1=0
 IF !IMPRI()
  o1=1
 ENDIF
 IF LASTKEY()=27
  RESTSCREEN(0,0,24,79,pant_ant)
  SETCOLOR(col_ant)
  CLOSE ALL
  RETURN
 ENDIF
 empr=nom_emp
 titul='CLIENTES POR DOMICILIO'
 dialist=DTOC(DATE())
 stitul="Calle "+UPPER(ALLTRIM(vdomi))
 vdomi=UPPER(ALLTRIM(vdomi))
 condi="UPPER(DOMI)=vdomi.AND.IIF(cual='I',moro='I',moro#'I')"
 DECLARE cam[3],sep[3],long[3],colum[3]
 carga1(cam,"clie~nomb~domi")
 carga2(sep,"1,2,0")
 carga2(long,"6,26,26")
 carga1(colum,"COD.~NOMBRE CLIENTE~DOMICILIO")
 IF o1=1
  IF cual='I'
   stitul=sTitul+' (Inactivos)'
  ENDIF
  LISTA(cam,sep,long,colum,titul,stitul,condi,empr,.T.,dialist)
 ELSE
  IF cual='I'
   titul=titul+' (Inactivos)'
  ENDIF
  INFORME(cam,sep,long,colum,titul,condi,empr)
 ENDIF
 RESTSCREEN(0,0,24,79,pant_ant)
 SETCOLOR(col_ant)
ENDDO
