*Listado de Articulos
PRIVATE pan_ant,col_ant
pan_ant=SAVESCREEN(0,0,24,79)
col_ant=SETCOLOR()
DECLARE salir[1]
DO WHILE .T.
 p=1
 SOMBRA(9,44,11,60,'V2',1,'',INVER,'N+')
 SETCOLOR(color2)
 vm=PADR(op,4,'0')
 *-----------------------------------------
 DECLARE menu1[1],vmenss[1],fi_co[1]
 CARGA1(menu1,"`1- Marca ")  &&~`2-       ~`3- Planillas~`4- Deuda ")
 vmenss[1]='                   Listado de Art¡culos por Marca                                '
* vmenss[2]='                   Listado de Cobradores Ordenadas por Nombre                    '
* vmenss[3]='                   Planilla de Cobranzas Analitica o Sintetica                   '
* vmenss[4]='                         Listado de Cobranzas Pendientes                         '
 carga1(fi_co,'10,45')  &&~11,45~12,45~13,45')
 salir[1]=13
 p=PROMPT(menu1,fi_co,'I',p,salir,menu&vm,'N+/W',vmenss,24)
 vm=PADR(op+STR(p,1),4,'0')        &&Activa menu de Habilitacion
 *-----------------------------------------
 DO CASE
  CASE p=0.OR.LASTKEY()=27
   RESTSCREEN(0,0,24,79,pan_ant)
   SETCOLOR(col_ant)
   RETURN
  CASE p=1
   *Articulos por Marca
   DO LISARTMC
 ENDCASE
ENDDO
