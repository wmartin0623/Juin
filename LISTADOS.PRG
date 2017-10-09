*Menu de LISTADOS
PUBLIC re,ok,mesa,desde,hasta,des,has,in1,imp1,imp2,ss,o1,pant_ant,stitul,dia_li,pag
dia_li = DTOC(DATE())
PRIVATE pant_ant,col_ant
pag=0
pant_ant = SAVESCREEN(0,0,24,79)
col_ant = SETCOLOR()
stitul=''
DECLARE salir[1]
p=1
DO CASE
 CASE opc1=1 .OR. opc1=2 .OR. opc1=3
  *Listado de Rubros, Lista de Precios ข Marcas
  PRIVATE vsel
  SOMBRA(opc1+5,44,opc1+8,56,'V2',1,'',INVER,'N+')
  SETCOLOR(color2)
  vm=PADR(op,4,'0')
  *-----------------------------------------
  DECLARE menu1[2],vmenss[2],fi_co[2]
  CARGA1(menu1,'`1- Codigo~`2- Nombre')
  vmenss[1]='                      Listado Ordenado por Codigo                                '
  vmenss[2]='                      Listado Ordenado por Nombre                                '
  IF opc1=1
   carga1(fi_co,'07,45~08,45')
  ELSE
   IF opc1=2
    carga1(fi_co,'08,45~09,45')
   ELSE
    IF opc1=3
     carga1(fi_co,'09,45~10,45')
    ENDIF
   ENDIF
  ENDIF
  salir[1]=13
  p=PROMPT(menu1,fi_co,'I',p,salir,menu&vm,'N+/W',vmenss,24)
  vm=PADR(op+STR(p,1),4,'0')        &&Activa menu de Habilitacion
  *-----------------------------------------
  DO CASE
   CASE p=1.OR.p=2
    PRIVATE panti
    panti=SAVESCREEN(0,0,24,79)
    empr=nom_emp
    DO CASE
     CASE opc1=1
      *Rubros
      DECLARE cam[2],sep[2],long[2],colum[2]
      CARGA2(sep,"5,0")
      CARGA2(long,"4,20")
      CARGA1(colum,"Cod.~Nombre")
      AREA()
      OPEN_DBF("MAERUBRO","RUBRO","C","MAERUBRO","rubr","NOMRUBRO","nomb")

      vsel="RUBRO"
      titul='LISTADO DE RUBROS'
      CARGA1(cam,"rubr~nomb")

     CASE opc1=2
      *Lista de Precios
      DECLARE cam[2],sep[2],long[2],colum[2]
      CARGA2(sep,"5,0")
      CARGA2(long,"4,30")
      CARGA1(colum,"Cod.~Descripciขn")
      AREA()
      OPEN_DBF("MAEPROVE","PROVE","C","MAEPROVE","prov","NOMPROVE","nomb")

      vsel="PROVE"
      titul='LISTADO DE LISTAS DE PRECIOS'
      CARGA1(cam,"prov~nomb")

     CASE opc1=3
      *Marcas
      DECLARE cam[2],sep[2],long[2],colum[2]
      CARGA2(sep,"5,0")
      CARGA2(long,"4,10")
      CARGA1(colum,"Cod.~Nombre")
      AREA()
      OPEN_DBF("MAEMARCA","MARCA","C","MAEMARCA","mrca","NOMMARCA","nomb")

      vsel="MARCA"
      titul='LISTADO DE MARCAS'
      CARGA1(cam,"' '+mrca+' '~nomb")
    ENDCASE
    stitul=''
    o1=0
    IF !IMPRI()
     o1=1
    ENDIF
    IF LASTKEY()=27
     CLOSE ALL
     SETCOLOR(col_ant)
     RESTSCREEN(0,0,24,79,pant_ant)
     RETURN
    ENDIF
    ok=0
    IF HAY_REG()
     IF CONFIR()
      IF ok=0
       SELECT &vsel
       IF p=1
        SET ORDER TO 1
       ELSE
        SET ORDER TO 2
       ENDIF
       condi=""
       IF o1=1
        LISTA(cam,sep,long,colum,titul,stitul,condi,empr,.F.,dia_li)
       ELSE
        INFORME(cam,sep,long,colum,titul,condi,empr)
       ENDIF
      ENDIF
      CLOSE ALL
     ENDIF
    ENDIF
    RESTSCREEN(0,0,24,79,panti)
  ENDCASE
ENDCASE
CLOSE ALL
RESTSCREEN(0,0,24,79,pant_ant)
SETCOLOR(col_ant)
RETURN

**************************************************************************
*FUNCTION informe                                                        *
*PARAMETERS campo,titulo,separacion,titu,empr                            *
* donde : "campo" son los campos que se mostraran por pant_antla del       *
*                 archivo abierto                                        *
*         "titulo" son los titulos de las distintas columnas del informe *
*         "separacion" es la separcion entre columnas                    *
*         "titu" es el Titulo pricipal del informe                       *
*         "empr" es el nombre de la empresa                              *
**************************************************************************
FUNCTION INFORME
PARA campo,separacion,longi,colu,titu,CONDICION,empr,grupo,tit_gru
LOCAL cant_camp,reg_ant,camp_dbe[1],titu_dbe[1],lin1[1],lin2[1],lin3[1]
LOCAL col_ant,pos,can_lin,repi,sgrup,vgru,vgru1
col_ant=SETCOLOR()
cant_camp=LEN(campo)
sgrup=.F.
DO CASE
CASE PCOUNT()<4
   SETCOLOR("")
   CLEAR
   @ 0, 0 SAY 'ERROR: NUMERO DE PARAMETROS ERRONEOS EN "INFORME()"'
   QUIT
CASE PCOUNT()=4
   titu=''
   empr=''
CASE PCOUNT()=5
   empr=''
CASE PCOUNT()=8
   tit_gru=""
   sgrup=.T.
CASE PCOUNT()=9
   sgrup=.T.
ENDCASE
reg_ant=RECNO()
camp_dbe[1]=''
clear
CARTEL('ญ ESPERE UN MOMENTO ......',13,1,'E')
go top
amor=0
linea=0
repi=0
set cursor off
SET FILTER TO &CONDICION
GO TOP
IF sgrup
 vgru=&grupo
ENDIF
can_lin=0
DO WHILE .t.
 SETCOLOR("W+/B")
 if amor=0
  SETCOLOR("G+/B")
  clear
  lin1[1]=''
  FOR con=1 TO LEN(colu)
   lin1[1]=lin1[1]+REPLICATE('อ',longi[con])+SPACE(separacion[con])
  NEXT con
  lin2[1]=''
  lin3[1]=''
  ok=0
  FOR con=1 TO LEN(colu)
   STORE '' TO el1,el2
   pos=AT(';',colu[con])
   IF pos<>0
    ok=1
    IF pos=1
     el1=SPACE(longi[con]-1)
    ELSE
     el1=SUBSTR(colu[con],1,pos-1)
    ENDIF
    el2=SUBSTR(colu[con],pos+1)
    lin2[1]=lin2[1]+SUBSTR(SPACE(INT((longi[con]-LEN(el1))/2))+el1+SPACE(longi[con]),1,longi[con])+SPACE(separacion[con])
    lin3[1]=lin3[1]+SUBSTR(SPACE(INT((longi[con]-LEN(el2))/2))+el2+SPACE(longi[con]),1,longi[con])+SPACE(separacion[con])
   ELSE
    el1=SPACE(longi[con]-1)
    el2=colu[con]
    lin2[1]=lin2[1]+SUBSTR(SPACE(INT((longi[con]-LEN(el1))/2))+el1+SPACE(longi[con]),1,longi[con])+SPACE(separacion[con])
    lin3[1]=lin3[1]+SUBSTR(SPACE(INT((longi[con]-LEN(el2))/2))+el2+SPACE(longi[con]),1,longi[con])+SPACE(separacion[con])
   ENDIF
  NEXT con
  @0,0 SAY empr
  @1,0 SAY CENTRA('๐'+titu+'๐',79)
  @2,0 CLEAR TO 24,79
  IF ok=1
   @2,0 SAY CENTRA(lin2[1],79)
   @3,0 SAY CENTRA(lin3[1],79)
   @4,0 SAY CENTRA(lin1[1],79)
   linea=5
   s1=14
   s2=32
  ELSE
   @2,0 SAY CENTRA(lin3[1],79)
   @3,0 SAY CENTRA(lin1[1],79)
   linea=4
   s1=16
   s2=34
  ENDIF
  IF sgrup
   @ROW()+1,0 SAY CENTRA("****"+tit_gru+" "+vgru,79)
   linea=linea+1
  ENDIF
  SETCOLOR("W+/B")
  amor=1
 endif
 camp_dbe[1]=''
 FOR con=1 TO cant_camp
  IF EOF()
   con=cant_camp
   repi=repi+1
  ELSE
   ca=campo[con]
   camp_dbe[1]=camp_dbe[1]+SUBSTR(&ca+SPACE(longi[con]),1,longi[con])+SPACE(separacion[con])
  ENDIF
 NEXT con
 IF sgrup
  vgru1=&grupo
  IF vgru1<>vgru
   SETCOLOR("G+/B")
   IF linea=5
    @ROW(),0 SAY CENTRA('****'+tit_gru+' '+vgru1,79)
   ELSE
    @ROW()+1,0 SAY CENTRA('****'+tit_gru+' '+vgru1,79)
    linea=linea+1
   ENDIF
   vgru=vgru1
   SETCOLOR("W+/B")
  ENDIF
 ENDIF
 lin=camp_dbe[1]
 IF LEN(lin)>80
  lin=SUBSTR(lin,1,79)
 ENDIF
 IF LEN(lin)<>0
  can_lin=can_lin+1
 ELSE
  IF EOF()
   IF repi<>1
    GO TOP
    linea=0
    amor=0
    can_lin=0
    LOOP
   ELSE
    lin=''
    linea=20
   ENDIF
  ENDIF
 ENDIF
 IF linea<21
  @ROW()+1,0 SAY CENTRA(lin,79)
  linea=linea+1
  SKIP
 ENDIF
 IF EOF()
  IF linea=5.AND.can_lin>=s1
   GO BOTTOM
   SKIP -s1
   amor=0
   can_lin=0
  ELSE
   linea=21
  ENDIF
 ENDIF
 IF linea>=21
  @21,0 SAY REPLI('฿',80)
  SETCOLOR(color3)
  @22,2 SAY 'ฐ Enter - sigue ฐ PgUp - Atras ฐ End - Final ฐ Home - Princ. ฐ Esc - Salir ฐ'
  @23,2 SAY 'ฐ F10 - Calcu.  ฐ'
  DIST_COLOR(22,4,'~W+/R~Enter')
  DIST_COLOR(22,20,'~W+/R~PgUp')
  DIST_COLOR(22,35,'~W+/R~End')
  DIST_COLOR(22,49,'~W+/R~Home')
  DIST_COLOR(22,65,'~W+/R~Esc')
  DIST_COLOR(23,4,'~W+/R~F10')
  SETCOLOR("W+/B")
  tecla=INKEY(0)
  DO WHILE tecla<>13.AND.tecla<>18.AND.tecla<>57.AND.tecla<>6.AND.tecla<>49;
  .AND.tecla<>1.AND.tecla<>55.AND.tecla<>27
   tecla=INKEY(0)
   IF tecla=-9
    CALCULATOR()
   ENDIF
  ENDDO
  IF LASTKEY()=18.OR.LASTKEY()=57
   SKIP -s2
   IF BOF()
    TONE(1000,1)
    GO TOP
   ENDIF
  ENDIF
  IF LASTKEY()=6.OR.LASTKEY()=49
   TONE(600,1)
   GO BOTTOM
   SKIP -s1
  ENDIF
  IF LASTKEY()=1.OR.LASTKEY()=55
   TONE(600,1)
   GO TOP
  ENDIF
  IF LASTKEY()=27
   EXIT
  ENDIF
  can_lin=0
  amor=0
  linea=0
  repi=0
 ENDIF
ENDDO
IF LASTKEY()=27
 GO reg_ant
ENDIF
SETCOLOR(col_ant)
SET CURSOR ON
RETURN(.T.)

FUNCTION lista
*SI letra=.t. se usa letra normal
*SI letra=.f. se use letra comprimida
*SI salto=.T. salto de pagina tras grupo
*SI salto=.F. no saltar de pagina tras el grupo
*            1       2        3    4    5    6     7        8     9     10      11    12   13    14    15      16     17
PARAMETERS campo,separacion,longi,colu,titu,stit,CONDICION,empr,letra,dia_list,pagina,toti,deci,grupo,tit_gru,salto,Renglon
PRIVATE cant_camp,reg_ant,camp_dbe[1],titu_dbe[1],lin1[1],lin2[1],lin3[1]
PRIVATE opc,col_ant,pos,mensa1,mensa2,final[1],sgrup,vgru,vgru1,subt[1]
mensa1=''
mensa2=''
col_ant=SETCOLOR()
cant_camp = LEN(campo)
DECLARE to1[cant_camp],sb[cant_camp]
AFILL(to1,0)
AFILL(sb,0)
sgrup=.F.
DO CASE
CASE PCOUNT()<4
   SETCOLOR("")
   CLEAR
   @ 0, 0 SAY 'ERROR: NUMERO DE PARAMETROS ERRONEOS EN "LISTA()"'
   QUIT
CASE PCOUNT()=4
   PRIVATE titu,stit,condicion,empr,letra,dia_list,pagina,toti,DECI,Renglon
   STORE '' TO titu,condicion,empr
   stit='*'
   letra=.T.
   dia_list=''
   pagina=0
   DECLARE toti[LEN(campo)],deci[LEN(campo)]
   AFILL(toti,'F')
   AFILL(deci,0)
   Renglon=0
CASE PCOUNT()=5
   PRIVATE stit,condicion,empr,letra,dia_list,pagina,toti,deci,Renglon
   STORE '' TO condicion,empr
   stit='*'
   letra=.T.
   dia_list=''
   pagina=0
   DECLARE toti[LEN(campo)],deci[LEN(campo)]
   AFILL(toti,'F')
   AFILL(deci,0)
   Renglon=0
CASE PCOUNT()=6
   PRIVATE condicion,empr,letra,dia_list,pagina,toti,deci,Renglon
   STORE '' TO condicion,empr
   letra=.T.
   dia_list=''
   pagina=0
   DECLARE toti[LEN(campo)],deci[LEN(campo)]
   AFILL(toti,'F')
   AFILL(deci,0)
   Renglon=0
CASE PCOUNT()=7
   PRIVATE empr,letra,dia_list,pagina,toti,deci,Renglon
   empr=''
   letra=.T.
   dia_list=''
   pagina=0
   DECLARE toti[LEN(campo)],deci[LEN(campo)]
   AFILL(toti,'F')
   AFILL(deci,0)
   Renglon=0
CASE PCOUNT()=8
   PRIVATE letra,dia_list,pagina,toti,deci,Renglon
   letra=.T.
   dia_list=''
   pagina=0
   DECLARE toti[LEN(campo)],deci[LEN(campo)]
   AFILL(toti,'F')
   AFILL(deci,0)
   Renglon=0
CASE PCOUNT()=9
   PRIVATE dia_list,pagina,toti,deci,Renglon
   dia_list =''
   pagina = 0
   DECLARE deci[LEN(campo)],toti[LEN(campo)]
   AFILL(deci,0)
   AFILL(toti,'F')
   Renglon=0
CASE PCOUNT()=10
   PRIVATE pagina,toti,deci,Renglon
   pagina=0
   DECLARE toti[LEN(campo)],deci[LEN(campo)]
   AFILL(deci,0)
   AFILL(toti,'F')
   Renglon=0
CASE PCOUNT()=11
   PRIVATE toti,deci,Renglon
   DECLARE toti[LEN(campo)],deci[LEN(campo)]
   AFILL(deci,0)
   AFILL(toti,'F')
   Renglon=0
CASE PCOUNT()=13
   PRIVATE tit_gru,salto,Renglon
   tit_gru=""
   salto=.F.
   Renglon=0
CASE PCOUNT()=14
   PRIVATE tit_gru,salto,Renglon
   tit_gru=""
   sgrup=.T.
   salto=.F.
   Renglon=0
CASE PCOUNT()=15
   PRIVATE salto,Renglon
   salto=.F.
   sgrup=.T.
   Renglon=0
CASE PCOUNT()=16
   PRIVATE Renglon
   sgrup=.T.
   Renglon=0
ENDCASE
IF EMPTY(stit)
 stit='*'
ENDIF
reg_ant=RECNO()
camp_dbe[1]=''
titu_dbe[1]=titu
*MODULO DE LISTADOS
CLEAR
CARTEL('ญ ESPERE.... ESTOY IMPRIMIENDO !',13,1,'E')
SET CONSOLE OFF
SET PRINT ON
?? CHR(27)+CHR(33)+CHR(5)
?? CHR(27)+CHR(67)+CHR(72)
STORE 0 TO t1,t2,t3,t4
SET FILTER TO &CONDICION
GO TOP
final[1]=''
IF sgrup
 vgru=&grupo
ENDIF
DO WHILE !EOF().AND.INKEY(.1)#27
 pagina=pagina+1
 norma()
 IF Renglon#0
  FOR Rengl=1 TO Renglon
   ?
  NEXT
  norma()
  ? SPACE(55)+CHR(15)+'Pagina Nro. : '+STR(pagina,5)
  Hor=Renglon+1
 ELSE
  gran_on()
  ? SUBSTR(empr+SPACE(25),1,25)
  gran_off()
  norma()
  chico()
  ?? space(23)+'Pagina Nro. : '+str(pagina,5)
  Hor=1
 ENDIF
 norma()
 gran_on()
 ? SUBSTR(titu_dbe[1]+SPACE(31),1,31)
 Hor++
 gran_off()
 norma()
 CHICO()
 IF dia_list<>''
  ??space(2)+'Fecha       : '+STUFF(dia_list,3,4,'-'+MESES(CTOD(dia_list))+'-')
 ENDIF
 IF stit='*'
  ?
 ELSE
  CHICO()
  ? '๐'+stit+'๐'
 ENDIF
 Hor++
 lin1[1]=''
 FOR con = 1 TO LEN(colu)
  ca=colu[con]
  lin1[1]=lin1[1]+REPLICATE('=',longi[con])+SPACE(separacion[con])
 NEXT con
 lin2[1]=''
 lin3[1]=''
 ok=0
 FOR con=1 TO LEN(colu)
  STORE '' TO el1,el2
  pos=AT(';',colu[con])
  IF pos<>0
   ok=1
   IF pos=1
    el1=SPACE(longi[con]-1)
   ELSE
    el1=SUBSTR(colu[con],1,pos-1)
   ENDIF
   el2=SUBSTR(colu[con],pos+1)
   lin2[1]=lin2[1]+SUBSTR(SPACE(INT((longi[con]-LEN(el1))/2))+el1+SPACE(longi[con]),1,longi[con])+SPACE(separacion[con])
   lin3[1]=lin3[1]+SUBSTR(SPACE(INT((longi[con]-LEN(el2))/2))+el2+SPACE(longi[con]),1,longi[con])+SPACE(separacion[con])
  ELSE
   el1=SPACE(longi[con]-1)
   el2=colu[con]
   lin2[1]=lin2[1]+SUBSTR(SPACE(INT((longi[con]-LEN(el1))/2))+el1+SPACE(longi[con]),1,longi[con])+SPACE(separacion[con])
   lin3[1]=lin3[1]+SUBSTR(SPACE(INT((longi[con]-LEN(el2))/2))+el2+SPACE(longi[con]),1,longi[con])+SPACE(separacion[con])
  ENDIF
 NEXT con
 IF letra
  ?? CHR(27)+CHR(120)+CHR(0)+CHR(18)
 ELSE
  ?? CHR(27)+CHR(120)+CHR(0)+CHR(15)
 ENDIF
 ? lin1[1]
 hor++
 IF ok=1
  ? lin2[1]
  Hor++
 ENDIF
 ? lin3[1]
 Hor++
 ? lin1[1]
 Hor++
 IF sgrup
  ?
  ? '****'+tit_gru+' '+vgru
  hor = hor+2
 ENDIF
 DO WHILE .NOT.EOF().AND.hor<=62.AND.INKEY(.1)#27
  camp_dbe[1]=''
  FOR con=1 TO LEN(campo)
   ca=campo[con]
   IF toti[con]='T'
    to1[con]=to1[con]+VAL(&ca)
    sb[con]=sb[con]+VAL(&ca)
    mensa1='***Total***'
    IF sgrup
     mensa2='***Sub_Total***'
    ENDIF
   ENDIF
   camp_dbe[1]=camp_dbe[1]+SUBSTR(&ca+SPACE(longi[con]),1,longi[con])+SPACE(separacion[con])
  NEXT con
  IF sgrup
   vgru1=&grupo
   IF vgru1<>vgru
    subt[1]=''
    FOR con=1 TO LEN(campo)
     ca=campo[con]
     IF toti[con]='T'
      sb[con]=sb[con]-VAL(&ca)
     ENDIF
    NEXT con
    FOR con=1 TO cant_camp
     IF toti[con]='T'
      subt[1]=subt[1]+SUBSTR(STR(sb[con],longi[con],deci[con])+SPACE(longi[con]),1,longi[con])+SPACE(separacion[con])
     ELSE
      subt[1]=subt[1]+SPACE(longi[con])+SPACE(separacion[con])
     ENDIF
    NEXT
    IF !EMPTY(mensa2)
     ? mensa2
     ? subt[1]
     IF salto
      hor=99
     ELSE
      hor=hor+2
     ENDIF
     AFILL(sb,0)
     IF hor<>99
      FOR con=1 TO LEN(campo)
       ca=campo[con]
       IF toti[con]='T'
        sb[con]=sb[con]+VAL(&ca)
       ENDIF
      NEXT con
     ELSE
      FOR con=1 TO LEN(campo)
       ca=campo[con]
       IF toti[con]='T'
        to1[con]=to1[con]-VAL(&ca)
       ENDIF
      NEXT con
     ENDIF
    ENDIF
    IF .NOT.salto
     ?
     ? '****'+tit_gru+' '+vgru1
     hor=hor+2
    ENDIF
    vgru=vgru1
   ENDIF
  ENDIF
  IF hor<>99
   ? camp_dbe[1]
   SKIP
   hor=hor+1
  ENDIF
 ENDDO
 IF LASTKEY()=27
  ? SPACE(20)+'LISTADO ABORTADO POR EL OPERADOR'
  EXIT
 ENDIF
 IF .NOT.EOF()
  EJECT
 ENDIF
enddo
IF sgrup
 subt[1]=''
 FOR con=1 TO cant_camp
  IF toti[con]='T'
   subt[1]=subt[1]+SUBSTR(STR(sb[con],longi[con],deci[con])+SPACE(longi[con]),1,longi[con])+SPACE(separacion[con])
  ELSE
   subt[1]=subt[1]+SPACE(longi[con])+SPACE(separacion[con])
  ENDIF
 NEXT
 IF !EMPTY(mensa2)
  ? mensa2
  ? subt[1]
  hor=hor+2
  AFILL(sb,0)
 ENDIF
ENDIF
FOR con=1 TO cant_camp
 IF toti[con]='T'
  final[1]=final[1]+SUBSTR(STR(to1[con],longi[con],deci[con])+SPACE(longi[con]),1,longi[con])+SPACE(separacion[con])
 ELSE
  final[1]=final[1]+SPACE(longi[con])+SPACE(separacion[con])
 ENDIF
NEXT
IF !EMPTY(mensa1)
 ? mensa1
 ? final[1]
ENDIF
EJECT
?? CHR(27)+CHR(33)+CHR(8)
?? chr(27)+chr(67)+chr(72)
SET PRINT OFF
SET CONSOLE ON
SETCOLOR(col_ant)
pag = pagina
RETURN .T.

PROCEDURE RANGOS
PRIVATE pant1
pant1=SAVESCREEN(15,19,22,66)
SOMBRA(16,20,20,65,'H5',2,'',INVER,'N+')
DO WHILE .T.
 desde=0
 hasta=0
 @22,26 SAY 'Rango = 1 a '+STR(re,4)+'  <Esc> p/Salir'
 @17,22 SAY 'Desde '+mesa+' <Enter>p/Primero :'GET desde PICTURE'999999' RANGE 1,re
 READ
 desde=IIF(desde=0,1,desde)
 @17,56 SAY STR(desde,6)
 @19,22 SAY 'Hasta '+mesa+' <Enter>p/Final   :'GET hasta PICTURE'999999' RANGE desde,re
 read
 hasta=IIF(hasta=0,re,hasta)
 @19,56 SAY STR(hasta,6)
 IF LASTKEY()=27
  ok=1
  EXIT
 ELSE
  ok=0
  EXIT
 ENDIF
ENDDO
des=CERO(desde,6)
has=CERO(hasta,6)
RESTSCREEN(15,19,22,66,pant1)
RETURN

PROCEDURE saldos
DO WHILE .T.
 PRIVATE pant1,colu
 colu=SETCOLOR()
 pant1=SAVESCREEN(7,11,16,70)
 SETCOLOR(color3)
 SOMBRA(8,11,15,68,'H5',1,'',color3,'N+')
 @9,12 SAY CENTRA('LISTADO DE &clpr POR SALDO',55)
 @10,13 PROMPT 'Salir               '
 @11,13 PROMPT 'Todos lo Saldos     '
 @12,13 PROMPT 'Saldos MAYORES que :'
 @13,13 PROMPT 'Saldos MENORES que :'
 @14,13 PROMPT 'Saldos  ENTRE .... :'
 MENU TO ss
 IF ss=1.OR.ss=0
  EXIT
 ELSE
  IF ss=3
   hor=12
  ELSE
   IF ss=4
    hor=13
   ELSE
    IF ss=5
     hor=14
     @ hor,49 SAY 'y'
    ENDIF ss=5
   ENDIF ss=4
  ENDIF ss=3
 ENDIF ss=1
 imp1=0
 imp2=0
 IF ss<>2
  @hor,34 GET imp1 PICTURE '@K 999999999.99' RANGE 0,999999999.99
  READ
  IF ss=5
   @hor,51 GET imp2 PICTURE '@K 999999999.99' RANGE imp1,999999999.99
   READ
  ENDIF ss=5
 ENDIF ss<>2
 EXIT
ENDDO
RESTSCREEN(7,11,16,70,pant1)
SETCOLOR(colu)
RETURN

FUNCTION chequeo
PARA im_tit,hor1
IF PCOUNT() = 1
 PRIVATE hor1
 hor1=0
ENDIF
IF o1=1
 IF hor+hor1>=65
  EJECT
  DO &im_tit
 ENDIF
ELSE
 IF hor+hor1>=21
  @23,3 SAY CENTRA('Presione [Enter] p/Seguir  [Esc] p/Terminar',75)
  INKEY(0)
  IF LASTKEY()=27
   sale = 1
   RETURN []
  ENDIF
   DO &im_tit
 ENDIF
ENDIF
sale = 0
RETURN []

FUNCTION ini_fin
PARAMETERS base,tabla,ini,fin,larg,tit,sep
PRIVATE pant_ant,col_ant,are_ant,lon
lon = LEN(tabla)
PRIVATE tabla1[lon]
ACOPY(tabla,tabla1)
pant_ant=SAVESCREEN(0,0,24,79)
col_ant=SETCOLOR()
are_ant=ALLTRIM(STR(SELECT()))
IF PCOUNT()=5
 PRIVATE tit,sep[lon]
 tit=""
 FOR con=1 TO lon
  sep[con]=1
 NEXT
 sep[lon]=0
ENDIF
IF PCOUNT()=6
 PRIVATE sep[lon]
 FOR con=1 TO lon
  sep[con]=1
 NEXT
 sep[lon]=0
ENDIF
DECLARE ind[1],lar[1],nom[1]
ind[1]=1
lar[1]=larg
nom[1]="Codigo: "
DO WHILE .T.
 SELECT &base
 sub_pan1 = SAVESCREEN(3,0,22,79)
 SETCOLOR(color3)
 SOMBRA(3,10,21,56,"V1",2,'',color3)
 @ 5,10 SAY 'ฬอออออออออออออออออออออออออออออออออออออออออออออน'
 @19,10 SAY 'วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ'
 @22,10 SAY SPACE(46)
 @22,40 SAY "[Esc] - Todos"
 SETCOLOR(color1)
 @20,11 SAY 'DESDE : '+SPACE(34)
 SETCOLOR(color3)
 ord_ant=STR(INDEXORD())
 reg_ant=RECNO()
 SET ORDER TO 1
 BUSCARE(4,11,18,52,tabla,tit,sep,ind,lar,22,20,nom,22,9)
 IF LASTKEY()=27
  SET ORDER TO &ord_ant
  RESTSCREEN(3,0,22,79,sub_pan1)
  ok=1
  EXIT
 ENDIF
 IF INDEXORD()=1
  camp=tabla[1]
  ini=&camp
  @20,19 SAY ini+' '+nomb
  SET FILTER TO &camp>=ini
 ENDIF
 TONE(1000,5)
 SETCOLOR(color3)
 BUSCARE(4,11,18,52,tabla1,tit,sep,ind,lar,22,20,nom,22,9)
 RESTSCREEN(3,0,22,79,sub_pan1)
 IF LASTKEY()=27
  SET ORDER TO &ord_ant
  SET FILTER TO
  GOTO reg_ant
  *RELEASE tabla,sep,ind,lar,tit,ord_ant,reg_ant
  ok=1
  EXIT
 ENDIF
 IF INDEXORD()=1
  fin=&camp
 ENDIF
 SET FILTER TO
 GO TOP
 *RELEASE tabla,sep,ind,lar,tit,ord_ant,reg_ant
 EXIT
ENDDO
RESTSCREEN(0,0,24,79,pant_ant)
SETCOLOR(col_ant)
SELECT &are_ant
RETURN []

*****************
FUNCTION ver_art1
*****************
PARAMETERS var1,bnom
IF PCOUNT()=1
 PRIVATE bnom
 bnom=""
ENDIF
PRIVATE sub_pan,area_ant,tcl,VAR,colo_ant
colo_ant=SETCOLOR()
IF LASTKEY()=27         &&tcl=27
 RETURN(.T.)
ENDIF
area_ant=ALLTRIM(STR(SELECT()))
SELECT ARTICULO
SET ORDER TO 1
VAR1=CERO(VAL(var1),5)
VAR=VAR1
SEEK var
IF !FOUND()
 sub_pan=SAVESCREEN(5,2,24,78)
 SETCOLOR(color3)
 SOMBRA(5,2,23,77,'V1',3,'',color3)
 @7,2 SAY 'ฬออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน'
 @20,2 SAY 'วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ'
 @17,2 SAY 'ฬออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน'
 SETCOLOR("I")
 @5,38 SAY ' BUSCAR '
 SETCOLOR(color3)
 @18,3 SAY 'BUSCAR:'
 @19,3 SAY SPACE(38)
 @21,13 SAY CHR(24)+CHR(25)+' PgUp PgDn Mover         '+CHR(17)+'ฤู Acepta'
 @22,13 SAY 'F2-Cambia Clave Busqueda  [Esc]-Salir'
 DECLARE cam[3],sep[3],ind[2],lar[2],nom[2]
 CARGA1(cam,"codi~nomb~SUBSTR(ALLTRIM(LEFT(for2,3))+IIF(VAL(SUBSTR(for2,4,3))<>0,'x'+ALLTRIM(SUBSTR(for2,4,3)),'')+IIF(VAL(RIGHT(for2,3))<>0,'x'+ALLTRIM(RIGHT(for2,3)),'')+SPACE(11),1,11)")
 tit='Codigo  Nombre  Producto                   Unidad      '
 CARGA2(sep,'5,3,0')
 ind[1]=1
 lar[1]=5
 nom[1]='Codigo Producto : '
 ind[2]=2
 lar[2]=30
 nom[2]='Nombre del Producto : '
 ord_ant=STR(INDEXORD())
 SET ORDER TO 2
 GO TOP
 SETCOLOR(color3)
 BUSCARE(6,4,16,75,cam,tit,sep,ind,lar,19,4,nom,18,14)
 SETCOLOR(colo_ant)
 RESTSCREEN(5,2,24,78,sub_pan)
 SET ORDER TO &ord_ant
 *RELEASE cam,sep,ind,lar,tit,ord_ant
 IF LASTKEY()=27
  RETURN(.F.)
 ENDIF
ENDIF
gram=SUBSTR(ALLTRIM(LEFT(for2,3))+IIF(VAL(SUBSTR(for2,4,3))<>0,'x'+ALLTRIM(SUBSTR(for2,4,3)),'')+IIF(VAL(RIGHT(for2,3))<>0,'x'+ALLTRIM(RIGHT(for2,3)),'')+SPACE(11),1,11)
bnom=gram+' '+TRIM(nomb)
SETCOLOR(colo_ant)
SELECT &area_ant
SET FILTER TO
RETURN(.T.)

*Devuelve el nombre del mes
*Recibe el numero del mes como string
FUNCTION NO_MES
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
mes1[1]= "Enero"
mes1[2]= "Febrero"
mes1[3]= "Marzo"
mes1[4]= "Abril"
mes1[5]= "Mayo"
mes1[6]= "Junio"
mes1[7]= "Julio"
mes1[8]= "Agosto"
mes1[9]= "Setiembre"
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

FUNCTION IMPRI
PARA fila
IF PCOUNT()=0
 PRIV fila
 fila=18
ENDIF
IF .NOT. CARTEL(' [ PANTALLA ]  [ IMPRESORA ] ',fila,1,'N',colo3)
 IF LASTKEY()=27
  KEYBOARD CHR(27)
  RETURN .F.
 ENDIF
 IF ONLINE()
  SET PRINT ON
  RETURN .F.
 ELSE
  KEYBOARD CHR(27)
  RETURN .F.
 ENDIF
ELSE
 SET PRINT OFF
 RETURN .T.
ENDIF

FUNCTION CONFIR
PARA fila
IF PCOUNT()=0
 PRIV fila
 fila=20
ENDIF
IF .NOT. CARTEL(' [ COMENZAR ]  [ TERMINAR ] ',fila,1,'N',colo3)
 RETURN .F.
ELSE
 RETURN .T.
ENDIF

FUNCTION ONLINE
PRIVATE col_ant,pan_ant
pan_ant=SAVESCREEN(10,19,15,61)
col_ant=SETCOLOR()
DO WHILE .T.
 SETCOLOR(colo3)
 SOMBRA(11,20,14,60,'H5',1,'',colo3,'N+')
 @12,21 SAY CENTRA('Prepare Impresora en [ON LINE]',38)
 @13,21 SAY CENTRA('[Enter] - Seguir  [Esc] - Salir',38)
 INKEY(0)
 SETCOLOR(col_ant)
 RESTSCREEN(10,19,15,61,pan_ant)
 IF LASTKEY()=27
  RETURN .F.
 ENDIF
 IF .NOT.ISPRINTER()
  LOOP
 ENDIF
 RETURN .T.
ENDDO

FUNCTION NGR_ON
?? CHR(27)+CHR(69)
RETURN []

FUNCTION NGR_OF
?? CHR(27)+CHR(70)
RETURN []

FUNCTION MAYUSC(lin,col,texto)
LOCAL excepcion:={" De "," Y "," Del "}
nuevotexto=SPACE(1)+LOWER(texto)
fin:=LEN(texto)
FOR i=1 TO fin
 IF SUBSTR(nuevotexto,i,1)=" ".OR.SUBSTR(nuevotexto,i,1)="."
  nuevotexto=STUFF(nuevotexto,i+1,1,UPPER(SUBSTR(nuevotexto,i+1,1)))
 ENDIF
NEXT
tamanho=LEN(excepcion)
FOR i=1 TO tamanho
 IF excepcion[1]$nuevotexto
  nuevotexto=STUFF(nuevotexto,AT(excepcion[i],nuevotexto),;
            LEN(excepcion[i]),LOWER(excepcion[i]))
 ENDIF
NEXT
IF lin=NIL.OR.col=NIL
 RETURN (LTRIM(nuevotexto))
ELSE
 @lin,col SAY LTRIM(nuevotexto)
 RETURN NIL
ENDIF

****************************************************************************
*FUNCTION INFO_TOT  (Informes por PANTALLA con Totalizadores               *
*PARAMETERS campo,separacion,longi,colu,titu,CONDICION,empr,toti,deci,     *
*           grupo,tit_gru                                                  *
* donde : "CAMPO" son los campos que se mostraran por pantalla del         *
*                 archivo abierto                                          *
*         "SEPARACION" es la Separacion entre Columnas                     *
*         "LONGI" es la longitud de cada campo                             *
*         "COLU" Subtitulos de Cada Columna a imprimir                     *
*         "TITU" Es el Titulo Principal del Listado                        *
*         "CONDICION" Condicion o FILTRO del Listado                       *
*         "EMPR" es el nombre de la empresa                                *
*         "TOTI" Tendra los campos que se deberan Totalizar                *
*                .F.=No totalizar     .T.=SI totalizar                     *
*         "DECI" Cantidad de Decimales de los campos a Totalizar           *
*         "GRUPO" Nombre del Campo que servira de SUBGRUPO para totalizar  *
*         "TIT_GRU" Titulo del Subgrupo                                    *
****************************************************************************

FUNCTION INFO_TOT1
PARAMETERS campo,separacion,longi,colu,titu,CONDICION,empr,toti,deci,grupo,tit_gru
LOCAL cant_camp,reg_ant,camp_dbe[1],titu_dbe[1],lin1[1],lin2[1],lin3[1]
LOCAL col_ant,pos,can_lin,sgrup,vgru,vgru1,final[1],subt[1],mensa1,mensa2
col_ant=SETCOLOR()
cant_camp = LEN(campo)
DECLARE to1[cant_camp],sb[cant_camp]
AFILL(to1,0)
AFILL(sb,0)
sgrup = .F.
******** Control de Parametros pasados ***************
DO CASE
CASE PCOUNT() < 4
   SETCOLOR("")
   CLEAR
   @ 0, 0 SAY 'ERROR: NUMERO DE PARAMETROS ERRONEOS EN "INFORME()"'
   QUIT

CASE PCOUNT() = 4
   STORE '' TO titu,condicion,empr
   DECLARE toti[LEN(campo)],deci[LEN(campo)]
   AFILL(toti,'F')
   AFILL(deci,0)


CASE PCOUNT() = 5
   STORE '' TO condicion,empr
   DECLARE toti[LEN(campo)],deci[LEN(campo)]
   AFILL(toti,'F')
   AFILL(deci,0)

CASE PCOUNT() = 6
   STORE '' TO empr
   DECLARE toti[LEN(campo)],deci[LEN(campo)]
   AFILL(toti,'F')
   AFILL(deci,0)

CASE PCOUNT() = 7
   DECLARE toti[LEN(campo)],deci[LEN(campo)]
   AFILL(toti,'F')
   AFILL(deci,0)

CASE PCOUNT() = 8
   DECLARE deci[LEN(campo)]
   AFILL(deci,0)

CASE PCOUNT() = 10
   tit_gru = ""
   sgrup = .T.

CASE PCOUNT() = 11
   sgrup = .T.

ENDCASE

reg_ant = RECNO()
camp_dbe[1] = ''

CLEAR
*CARTEL('ญ ESPERE UN MOMENTO ......',13,1,'E')
amor=0
linea=0
SET CURSOR OFF
********** Filtrar el archivo segun CONDICION ************
SET FILTER TO &CONDICION
GO TOP
*GO reg_ant
STORE 0 TO t1,t2,t3,t4
final[1] = ""
********** Control de Subgrupos ************
IF sgrup
 vgru = &grupo
ENDIF
*CARTEL('ญ ESPERE UN MOMENTO ......',13,1,'S')
DO WHILE .T.
 ***********************Titulos****************************
 SETCOLOR("W+/B")
 IF amor=0      &&amor=0 se imprimen titulos... =1 No se imprimen
  SETCOLOR("G+/B")
  CLEAR
  **** Primer Linea del Titulo (Lineas) ******
  lin1[1]=''
  FOR con = 1 TO LEN(colu)
   lin1[1] = lin1[1]+REPLICATE('อ',longi[con])+SPACE(separacion[con])
  NEXT con
  lin2[1]=''
  lin3[1]=''
  ok=0
  FOR con = 1 TO LEN(colu)
   STORE '' TO el1,el2
   pos=AT(';',colu[con])
   IF pos<>0    &&Si pos <> 0 Titulo en dos Lineas de lo Contrario -Una linea
    ok=1
    IF pos=1
     el1=SPACE(longi[con]-1)
    ELSE
     el1=SUBSTR(colu[con],1,pos-1)
    ENDIF
    **Dos lineas
    el2=SUBSTR(colu[con],pos+1)
    lin2[1] = lin2[1]+SUBSTR(SPACE(INT((longi[con]-LEN(el1))/2))+el1+SPACE(longi[con]),1,longi[con])+SPACE(separacion[con])
    lin3[1] = lin3[1]+SUBSTR(SPACE(INT((longi[con]-LEN(el2))/2))+el2+SPACE(longi[con]),1,longi[con])+SPACE(separacion[con])
   ELSE
    **Una linea
    el1=SPACE(longi[con]-1)
    el2=colu[con]
    lin2[1] = lin2[1]+SUBSTR(SPACE(INT((longi[con]-LEN(el1))/2))+el1+SPACE(longi[con]),1,longi[con])+SPACE(separacion[con])
    lin3[1] = lin3[1]+SUBSTR(SPACE(INT((longi[con]-LEN(el2))/2))+el2+SPACE(longi[con]),1,longi[con])+SPACE(separacion[con])
   ENDIF
  NEXT con
  **Impresion del Titulo
  @0,0 SAY empr
  @1,0 SAY CENTRA('๐'+titu+'๐',79)
  @2,0 CLEAR TO 24,79
  IF ok=1
   @2,0 SAY CENTRA(lin2[1],79)
   @3,0 SAY CENTRA(lin3[1],79)
   @4,0 SAY CENTRA(lin1[1],79)
   linea=5
  ELSE
   @2,0 SAY CENTRA(lin3[1],79)
   @3,0 SAY CENTRA(lin1[1],79)
   linea=4
  ENDIF
  **Si se usa Subgrupos se imprime Subtitulo y grupo
  IF sgrup
   @ROW()+1,0 SAY CENTRA("****"+tit_gru+" "+vgru,79)
   linea = linea+1
  ENDIF
  SETCOLOR("W+/B")
  amor=1
 ENDIF
 ******Creacion de la Linea de Impresion y Subtotales *******
 camp_dbe[1]=''
 FOR con = 1 TO cant_camp
  ca=campo[con]
  **Si se usan totalizadores
  IF toti[con] = 'T'
   to1[con]=to1[con]+VAL(&ca)
   sb[con]=sb[con]+VAL(&ca)
   mensa1='***Total***'
   IF sgrup
    mensa2='***Sub_Total***'
   ENDIF
  ENDIF
  **Linea de Impresion con datos
  camp_dbe[1] = camp_dbe[1]+SUBSTR(&ca+SPACE(longi[con]),1,longi[con])+SPACE(separacion[con])
 NEXT con
 ****Si usamos subgrupos   *******************
 IF sgrup
  vgru1 = &grupo
  ********Si el Grupo cambio
  IF vgru1 <> vgru
   subt[1]=''
   **Si el grupo cambio, resto del Totalizador y subtotalizador el ultimo
   **Registro porque pertenece al siguiente grupo
   FOR con = 1 TO LEN(campo)
    ca=campo[con]
    IF toti[con] = 'T'
     to1[con]=to1[con]-VAL(&ca)
     sb[con]=sb[con]-VAL(&ca)
    ENDIF
   NEXT con
   **Formo la linea de impresion para el subtotal
   FOR con = 1 TO cant_camp
    IF toti[con] = 'T'
     subt[1] = subt[1]+SUBSTR(STR(sb[con],longi[con],deci[con])+SPACE(longi[con]),1,longi[con])+SPACE(separacion[con])
    ELSE
     subt[1] = subt[1]+SPACE(longi[con])+SPACE(separacion[con])
    ENDIF
   NEXT
   **Si usamos subtotalizador se Imprime la linea de subtotal
   IF !EMPTY(mensa2)  &&Si se totaliza - MENSA2 = "***Sub_total***"
    SETCOLOR("R+/B")
    @ROW()+1,0 SAY CENTRA(subt[1],79)
    SETCOLOR("W+/B")
    AFILL(sb,0)         &&Borro el Subtotal luego de imprimir
    linea = linea+1
    IF linea < 24
     **Si se no se llego al final de la pantalla totalizo y subtotalizo el
     **Primer registro del siguiente grupo; de lo contrario espero la sgte.
     **pasada
     FOR con = 1 TO LEN(campo)
      ca=campo[con]
      IF toti[con] = 'T'
       to1[con]=to1[con]+VAL(&ca)
       sb[con]=sb[con]+VAL(&ca)
      ENDIF
     NEXT con
    ENDIF
   ENDIF
   SETCOLOR("G+/B")
   IF linea = 5
    @ROW(),0 SAY CENTRA('****'+tit_gru+' '+vgru1,79)
   ELSE
    @ROW()+1,0 SAY CENTRA('****'+tit_gru+' '+vgru1,79)
    linea = linea+1
    *Si la linea del Nombre de grupo coincide con la ultima linea
    *le resto el registro sumado porque sino lo suma dos veces
    IF linea = 24
     FOR con = 1 TO LEN(campo)
      ca=campo[con]
      IF toti[con] = 'T'
       to1[con]=to1[con]-VAL(&ca)
       sb[con]=sb[con]-VAL(&ca)
      ENDIF
     NEXT con
    ENDIF
   ENDIF
   **Cambio de grupo luego de imprimir el titulo del nuevo grupo
   vgru = vgru1
   SETCOLOR("W+/B")
  ENDIF
 ENDIF
 **Traspaso la linea a imprimir a LIN y verifico la long. por si supera los 80
 lin=camp_dbe[1]
 IF LEN(lin)>80
  lin=SUBSTR(lin,1,79)
 ENDIF
 **Si no llegue al final de la pantalla imprimo el registro
 IF linea < 24
  @ROW()+1,0 SAY CENTRA(lin,79)
  linea=linea+1
  SKIP
 ENDIF
 **Control del final del fichero
 IF EOF()
  **Si trabajo con subgrupos imprimo el subtotal(si usamos totales)
  IF sgrup
   subt[1]=''
   FOR con = 1 TO cant_camp
    IF toti[con] = 'T'
     subt[1] = subt[1]+SUBSTR(STR(sb[con],longi[con],deci[con])+SPACE(longi[con]),1,longi[con])+SPACE(separacion[con])
    ELSE
     subt[1] = subt[1]+SPACE(longi[con])+SPACE(separacion[con])
    ENDIF
   NEXT
   IF !EMPTY(mensa2)
    SETCOLOR("R+/B")
    @ROW()+1,0 SAY CENTRA(subt[1],79)
    SETCOLOR("W+/B")
    linea = linea+1
    AFILL(sb,0)
   ENDIF
  ENDIF
  **Forma la linea con los Totales Finales
  FOR con = 1 TO cant_camp
   IF toti[con] = 'T'
    final[1] = final[1]+SUBSTR(STR(to1[con],longi[con],deci[con])+SPACE(longi[con]),1,longi[con])+SPACE(separacion[con])
   ELSE
    final[1] = final[1]+SPACE(longi[con])+SPACE(separacion[con])
   ENDIF
  NEXT
  **Imprimo la linea con los totales finales
  IF !EMPTY(mensa1)
   SETCOLOR("W+/GR+")
   @ROW()+1,0 SAY CENTRA(final[1],79)
   SETCOLOR("W+/B")
   linea = 9999 &&se asigna 9999 para detectar el final de archivo
  ENDIF
 ENDIF
 **Controlo final de pantalla para imprimir mensaje para pasar a la sgte.Pant.
 IF linea>=24
  tecla=65     &&Asigno el valor ASCII 65 (A) para poder permanecer en el bucle
  **Si llego al final del archivo debo presionar ESCAPE para salir
  IF linea = 9999
   SETCOLOR(color3)
   @24,1 SAY 'ฐ Esc - Salir ฐ F10 - Calculadora ฐ'
   DIST_COLOR(24,3,'~W+/R~Esc')
   DIST_COLOR(24,17,'~W+/R~F10')
   SETCOLOR("W+/B")
   DO WHILE tecla<>27
    tecla = INKEY(0)
    IF tecla=-9
     CALCULATOR()
    ENDIF
   ENDDO
  ELSE
   **Continuar con la siguiente pantalla
   SETCOLOR(color3)
   @24,1 SAY '          ฐ Enter - sigue ฐ Esc - Salir ฐ F10 - Calculadora ฐ                 '
   DIST_COLOR(24,13,'~W+/R~Enter')
   DIST_COLOR(24,29,'~W+/R~Esc')
   DIST_COLOR(24,43,'~W+/R~F10')
   SETCOLOR("W+/B")
   DO WHILE tecla<>13.AND.tecla<>27
    tecla = INKEY(0)
    IF tecla=-9
     CALCULATOR()
    ENDIF
   ENDDO
  ENDIF

  **Detecto Deseos de salir del Operador o Final de Archivo
  IF LASTKEY()=27 .OR. linea = 9999
   EXIT
  ENDIF

  **Inicializo AMOR y LINEA para volver a imprimir titulos
  amor=0
  linea=0
 ENDIF
ENDDO
IF LASTKEY() = 27
   GO reg_ant
ENDIF
SETCOLOR(col_ant)
SET CURSOR ON
RETURN(.T.)

****************************************************************************
*FUNCTION INFO_TOT  (Informes por PANTALLA con Totalizadores               *
*PARAMETERS campo,separacion,longi,colu,titu,sTitu,CONDICION,empr,toti,deci,     *
*           grupo,tit_gru                                                  *
* donde : "CAMPO" son los campos que se mostraran por pantalla del         *
*                 archivo abierto                                          *
*         "SEPARACION" es la Separacion entre Columnas                     *
*         "LONGI" es la longitud de cada campo                             *
*         "COLU" Subtitulos de Cada Columna a imprimir                     *
*         "TITU" Es el Titulo Principal del Listado                        *
*         "sTITU" Es el SubTitulo del Listado                              *
*         "CONDICION" Condicion o FILTRO del Listado                       *
*         "EMPR" es el nombre de la empresa                                *
*         "TOTI" Tendra los campos que se deberan Totalizar                *
*                .F.=No totalizar     .T.=SI totalizar                     *
*         "DECI" Cantidad de Decimales de los campos a Totalizar           *
*         "GRUPO" Nombre del Campo que servira de SUBGRUPO para totalizar  *
*         "TIT_GRU" Titulo del Subgrupo                                    *
****************************************************************************

FUNCTION INFO_TOT
PARAMETERS campo,separacion,longi,colu,titu,sTitu,CONDICION,empr,toti,deci,grupo,tit_gru
LOCAL cant_camp,reg_ant,camp_dbe[1],titu_dbe[1],lin1[1],lin2[1],lin3[1]
LOCAL col_ant,pos,can_lin,sgrup,vgru,vgru1,final[1],subt[1],mensa1,mensa2
col_ant=SETCOLOR()
cant_camp = LEN(campo)
DECLARE to1[cant_camp],sb[cant_camp]
AFILL(to1,0)
AFILL(sb,0)
sgrup = .F.
******** Control de Parametros pasados ***************
DO CASE
CASE PCOUNT() < 4
   SETCOLOR("")
   CLEAR
   @ 0, 0 SAY 'ERROR: NUMERO DE PARAMETROS ERRONEOS EN "INFORME()"'
   QUIT

CASE PCOUNT() = 4
   STORE '' TO titu,sTitu,condicion,empr
   DECLARE toti[LEN(campo)],deci[LEN(campo)]
   AFILL(toti,'F')
   AFILL(deci,0)


CASE PCOUNT() = 5
   STORE '' TO sTitu,condicion,empr
   DECLARE toti[LEN(campo)],deci[LEN(campo)]
   AFILL(toti,'F')
   AFILL(deci,0)

CASE PCOUNT() = 6
   STORE '' TO Condicion,empr
   DECLARE toti[LEN(campo)],deci[LEN(campo)]
   AFILL(toti,'F')
   AFILL(deci,0)

CASE PCOUNT() = 7
   Empr=""
   DECLARE toti[LEN(campo)],deci[LEN(campo)]
   AFILL(toti,'F')
   AFILL(deci,0)

CASE PCOUNT() = 8
   DECLARE toti[LEN(campo)],deci[LEN(campo)]
   AFILL(toti,'F')
   AFILL(deci,0)

CASE PCOUNT() = 9
   DECLARE deci[LEN(campo)]
   AFILL(deci,0)

CASE PCOUNT() = 11
   tit_gru = ""
   sgrup = .T.

CASE PCOUNT() = 12
   sgrup = .T.

ENDCASE

reg_ant = RECNO()
camp_dbe[1] = ''

CLEAR
*CARTEL('ญ ESPERE UN MOMENTO ......',13,1,'E')
amor=0
linea=0
SET CURSOR OFF
********** Filtrar el archivo segun CONDICION ************
SET FILTER TO &CONDICION
GO TOP
*GO reg_ant
STORE 0 TO t1,t2,t3,t4
final[1] = ""
********** Control de Subgrupos ************
IF sgrup
 vgru = &grupo
ENDIF
*CARTEL('ญ ESPERE UN MOMENTO ......',13,1,'S')
DO WHILE .T.
 ***********************Titulos****************************
 SETCOLOR("W+/B")
 IF amor=0      &&amor=0 se imprimen titulos... =1 No se imprimen
  SETCOLOR("G+/B")
  CLEAR
  **** Primer Linea del Titulo (Lineas) ******
  lin1[1]=''
  FOR con = 1 TO LEN(colu)
   lin1[1] = lin1[1]+REPLICATE('อ',longi[con])+SPACE(separacion[con])
  NEXT con
  lin2[1]=''
  lin3[1]=''
  ok=0
  FOR con = 1 TO LEN(colu)
   STORE '' TO el1,el2
   pos=AT(';',colu[con])
   IF pos<>0    &&Si pos <> 0 Titulo en dos Lineas de lo Contrario -Una linea
    ok=1
    IF pos=1
     el1=SPACE(longi[con]-1)
    ELSE
     el1=SUBSTR(colu[con],1,pos-1)
    ENDIF
    **Dos lineas
    el2=SUBSTR(colu[con],pos+1)
    lin2[1] = lin2[1]+SUBSTR(SPACE(INT((longi[con]-LEN(el1))/2))+el1+SPACE(longi[con]),1,longi[con])+SPACE(separacion[con])
    lin3[1] = lin3[1]+SUBSTR(SPACE(INT((longi[con]-LEN(el2))/2))+el2+SPACE(longi[con]),1,longi[con])+SPACE(separacion[con])
   ELSE
    **Una linea
    el1=SPACE(longi[con]-1)
    el2=colu[con]
    lin2[1] = lin2[1]+SUBSTR(SPACE(INT((longi[con]-LEN(el1))/2))+el1+SPACE(longi[con]),1,longi[con])+SPACE(separacion[con])
    lin3[1] = lin3[1]+SUBSTR(SPACE(INT((longi[con]-LEN(el2))/2))+el2+SPACE(longi[con]),1,longi[con])+SPACE(separacion[con])
   ENDIF
  NEXT con
  **Impresion del Titulo
  @0,0 SAY empr
  @1,0 SAY CENTRA('๐'+titu+'๐',79)
  vvc=0
  IF !EMPTY(sTitu)
   @2,0 SAY CENTRA('๐'+sTitu+'๐',79) COLOR colo3
   vvc=1
  ENDIF
  @2+vvc,0 CLEAR TO 24,79
  IF ok=1
   @2+vvc,0 SAY CENTRA(lin2[1],79)
   @3+vvc,0 SAY CENTRA(lin3[1],79)
   @4+vvc,0 SAY CENTRA(lin1[1],79)
   linea=5+vvc
  ELSE
   @2+vvc,0 SAY CENTRA(lin3[1],79)
   @3+vvc,0 SAY CENTRA(lin1[1],79)
   linea=4+vvc
  ENDIF
  **Si se usa Subgrupos se imprime Subtitulo y grupo
  IF sgrup
   @ROW()+1,0 SAY CENTRA("****"+tit_gru+" "+vgru,79)
   linea = linea+1
  ENDIF
  SETCOLOR("W+/B")
  amor=1
 ENDIF
 ******Creacion de la Linea de Impresion y Subtotales *******
 camp_dbe[1]=''
 FOR con = 1 TO cant_camp
  ca=campo[con]
  **Si se usan totalizadores
  IF toti[con] = 'T'
   to1[con]=to1[con]+VAL(&ca)
   sb[con]=sb[con]+VAL(&ca)
   mensa1='***Total***'
   IF sgrup
    mensa2='***Sub_Total***'
   ENDIF
  ENDIF
  **Linea de Impresion con datos
  camp_dbe[1] = camp_dbe[1]+SUBSTR(&ca+SPACE(longi[con]),1,longi[con])+SPACE(separacion[con])
 NEXT con
 ****Si usamos subgrupos   *******************
 IF sgrup
  vgru1 = &grupo
  ********Si el Grupo cambio
  IF vgru1 <> vgru
   subt[1]=''
   **Si el grupo cambio, resto del Totalizador y subtotalizador el ultimo
   **Registro porque pertenece al siguiente grupo
   FOR con = 1 TO LEN(campo)
    ca=campo[con]
    IF toti[con] = 'T'
     to1[con]=to1[con]-VAL(&ca)
     sb[con]=sb[con]-VAL(&ca)
    ENDIF
   NEXT con
   **Formo la linea de impresion para el subtotal
   FOR con = 1 TO cant_camp
    IF toti[con] = 'T'
     subt[1] = subt[1]+SUBSTR(STR(sb[con],longi[con],deci[con])+SPACE(longi[con]),1,longi[con])+SPACE(separacion[con])
    ELSE
     subt[1] = subt[1]+SPACE(longi[con])+SPACE(separacion[con])
    ENDIF
   NEXT
   **Si usamos subtotalizador se Imprime la linea de subtotal
   IF !EMPTY(mensa2)  &&Si se totaliza - MENSA2 = "***Sub_total***"
    SETCOLOR("R+/B")
    @ROW()+1,0 SAY CENTRA(subt[1],79)
    SETCOLOR("W+/B")
    AFILL(sb,0)         &&Borro el Subtotal luego de imprimir
    linea = linea+1
    IF linea < 23
     **Si no se llego al final de la pantalla totalizo y subtotalizo el
     **Primer registro del siguiente grupo; de lo contrario espero la sgte.
     **pasada
     FOR con = 1 TO LEN(campo)
      ca=campo[con]
      IF toti[con] = 'T'
       to1[con]=to1[con]+VAL(&ca)
       sb[con]=sb[con]+VAL(&ca)
      ENDIF
     NEXT con
    ENDIF
   ENDIF
   SETCOLOR("G+/B")
   IF linea = 5
    @ROW(),0 SAY CENTRA('****'+tit_gru+' '+vgru1,79)
   ELSE
    @ROW()+1,0 SAY CENTRA('****'+tit_gru+' '+vgru1,79)
    linea = linea+1
    *Si la linea del Nombre de grupo coincide con la ultima linea
    *le resto el registro sumado porque sino lo suma dos veces
    IF linea = 23
     FOR con = 1 TO LEN(campo)
      ca=campo[con]
      IF toti[con] = 'T'
       to1[con]=to1[con]-VAL(&ca)
       sb[con]=sb[con]-VAL(&ca)
      ENDIF
     NEXT con
    ENDIF
   ENDIF
   **Cambio de grupo luego de imprimir el titulo del nuevo grupo
   vgru = vgru1
   SETCOLOR("W+/B")
  ENDIF
 ENDIF
 **Traspaso la linea a imprimir a LIN y verifico la long. por si supera los 80
 lin=camp_dbe[1]
 IF LEN(lin)>80
  lin=SUBSTR(lin,1,79)
 ENDIF
 **Si no llegue al final de la pantalla imprimo el registro
 IF linea < 23
  @ROW()+1,0 SAY CENTRA(lin,79)
  linea=linea+1
  SKIP
 ENDIF
 **Control del final del fichero
 IF EOF()
  **Si trabajo con subgrupos imprimo el subtotal(si usamos totales)
  IF sgrup
   subt[1]=''
   FOR con = 1 TO cant_camp
    IF toti[con] = 'T'
     subt[1] = subt[1]+SUBSTR(STR(sb[con],longi[con],deci[con])+SPACE(longi[con]),1,longi[con])+SPACE(separacion[con])
    ELSE
     subt[1] = subt[1]+SPACE(longi[con])+SPACE(separacion[con])
    ENDIF
   NEXT
   IF !EMPTY(mensa2)
    SETCOLOR("R+/B")
    @ROW()+1,0 SAY CENTRA(subt[1],79)
    SETCOLOR("W+/B")
    linea = linea+1
    AFILL(sb,0)
   ENDIF
  ENDIF
  **Forma la linea con los Totales Finales
  FOR con = 1 TO cant_camp
   IF toti[con] = 'T'
    final[1] = final[1]+SUBSTR(STR(to1[con],longi[con],deci[con])+SPACE(longi[con]),1,longi[con])+SPACE(separacion[con])
   ELSE
    final[1] = final[1]+SPACE(longi[con])+SPACE(separacion[con])
   ENDIF
  NEXT
  **Imprimo la linea con los totales finales
  IF !EMPTY(mensa1)
   SETCOLOR("W+/GR+")
   @ROW()+1,0 SAY CENTRA(final[1],79)
   SETCOLOR("W+/B")
   linea = 9999 &&se asigna 9999 para detectar el final de archivo
  ENDIF
 ENDIF
 **Controlo final de pantalla para imprimir mensaje para pasar a la sgte.Pant.
 IF linea>=23
  tecla=65     &&Asigno el valor ASCII 65 (A) para poder permanecer en el bucle
  **Si llego al final del archivo debo presionar ESCAPE para salir
  IF linea = 9999
   SETCOLOR(color3)
   @24,1 SAY 'ฐ Esc - Salir ฐ F10 - Calculadora ฐ'
   DIST_COLOR(24,3,'~W+/R~Esc')
   DIST_COLOR(24,17,'~W+/R~F10')
   SETCOLOR("W+/B")
   DO WHILE tecla<>27
    tecla = INKEY(0)
    IF tecla=-9
     CALCULATOR()
    ENDIF
   ENDDO
  ELSE
   **Continuar con la siguiente pantalla
   SETCOLOR(color3)
   @24,1 SAY '          ฐ Enter - sigue ฐ Esc - Salir ฐ F10 - Calculadora ฐ                 '
   DIST_COLOR(24,13,'~W+/R~Enter')
   DIST_COLOR(24,29,'~W+/R~Esc')
   DIST_COLOR(24,43,'~W+/R~F10')
   SETCOLOR("W+/B")
   DO WHILE tecla<>13.AND.tecla<>27
    tecla = INKEY(0)
    IF tecla=-9
     CALCULATOR()
    ENDIF
   ENDDO
  ENDIF

  **Detecto Deseos de salir del Operador o Final de Archivo
  IF LASTKEY()=27 .OR. linea = 9999
   EXIT
  ENDIF

  **Inicializo AMOR y LINEA para volver a imprimir titulos
  amor=0
  linea=0
 ENDIF
ENDDO
IF LASTKEY() = 27
   GO reg_ant
ENDIF
SETCOLOR(col_ant)
SET CURSOR ON
RETURN(.T.)

FUNCTION CHICO_HP
?? CHR(27)+CHR(40)+CHR(115)+CHR(49)+CHR(54)+CHR(46)+CHR(54)+CHR(55)+CHR(72)
RETURN []

FUNCTION NORMA_HP()
?? CHR(27)+CHR(40)+CHR(115)+CHR(49)+CHR(48)+CHR(72)
*?? CHR(27)+CHR(33)+CHR(1)
RETURN []

FUNCTION GRAN_ON_HP
?? CHR(27)+CHR(40)+CHR(115)+CHR(54)+CHR(72)
RETURN []

FUNCTION GRAN_OFF_HP
?? CHR(27)+CHR(40)+CHR(115)+CHR(49)+CHR(48)+CHR(72)
RETURN []

FUNCTION NGR_ON_HP
?? CHR(27)+CHR(40)+CHR(115)+CHR(51)+CHR(66)
RETURN []

FUNCTION NGR_OF_HP
?? CHR(27)+CHR(40)+CHR(115)+CHR(48)+CHR(66)
RETURN []

FUNCTION ONLINE1
PARAM xDefecto
IF PCOUNT()=0
 PRIVATE xDefecto:=vNLpt
ENDIF
PRIVATE col_ant,pan_ant,bmenuW[4],bmessW[4],bfi_coW[4],bsalirW[1],bactiW[4],bNpt
PRIVATE p
pan_ant=SAVESCREEN(10,19,15,61)
col_ant=SETCOLOR()
CARGA1(bmenuW,' LPT`1 ~ LPT`2 ~ LPT`3 ~`Abortar')
bmessW[1]='                       Selecciona Impresion en Puerto LPT1                       '
bmessW[2]='                       Selecciona Impresion en Puerto LPT2                       '
bmessW[3]='                       Selecciona Impresion en Puerto LPT3                       '
bmessW[4]='                               Aborta la IMPRESION                               '
CARGA1(bfi_coW,'13,27~13,36~13,45~13,54')
CARGA1(bactiW,"T~T~T~T")
bsalirW[1]=13
p=IIF(xDefecto='LPT1',1,IIF(xDefecto='LPT2',2,IIF(xDefecto="LPT3",3,4)))
DO WHILE .T.
 SETCOLOR(color2)
 SOMBRA(11,20,14,60,'H5',1,'',color2,'N+')
 @12,26 SAY 'Prepare Impresora en [ON LINE]'
 @13,26 SAY '[ LPT1 ] [ LPT2 ] [ LPT3 ] [ Abortar ]'
 p=PROMPT(bmenuW,bfi_coW,'I',p,bsalirW,bactiW,'N+/W',bmessW,24)
 SETCOLOR(col_ant)
 RESTSCREEN(10,19,15,61,pan_ant)
 DO CASE
  CASE p=0 .OR. p=4
   RETURN .F.
  CASE p=1
   bNpt="LPT1"
  CASE p=2
   bNpt="LPT2"
  CASE p=3
   bNpt="LPT3"
 ENDCASE
 SET PRINTER TO &bNpt
 IF .NOT.ISPRINTER()
  LOOP
 ENDIF
 EXIT
ENDDO
RETURN .T.

FUNCTION LETTER1
PARA numero
PRIVATE grupos[5],enletra,grupo,numstr,unidad,decena,centena,uni0,uni1,uni2,;
uni4,uni5,uni6,uni7,uni8,uni9,dec0,dec1,dec2,dec3,dec4,dec5,dec6,dec7,dec8,;
dec9,cen0,cen1,cen2,cen3,cen4,cen5,cen6,cen7,cen8,cen9,dec11,dec12,dec13,;
dec14,dec15,dec16,dec17,dec18,dec19,conect1,conect2,conect3,conect4,conect5,;
unilet,declet,cenlet,conector,grupostr
uni0="IIF(numero=0.and.grupo=1,`cero ',`')"
uni1="IIF(decena=`1',dec1&unidad,iif(grupos[grupo]=`001'.and.(grupo=2.or.grupo=4),`un ',iif(grupo>=2,`un ',`uno ')))"
uni2="IIF(decena=`1',dec1&unidad,`dos ')"
uni3="IIF(decena=`1',dec1&unidad,`tres ')"
uni4="IIF(decena=`1',dec1&unidad,`cuatro ')"
uni5="IIF(decena=`1',dec1&unidad,`cinco ')"
uni6="IIF(decena=`1',dec1&unidad,`seis ')"
uni7="IIF(decena=`1',dec1&unidad,`siete ')"
uni8="IIF(decena=`1',dec1&unidad,`ocho ')"
uni9="IIF(decena=`1',dec1&unidad,`nueve ')"
dec0="`'"
dec1="IIF(unidad=`0',`diez ',`')"
dec11=`once '
dec12=`doce '
dec13=`trece '
dec14=`catorce '
dec15=`quince '
dec16=`dieciseis '
dec17=`diecisiete '
dec18=`dieciocho '
dec19=`diecinueve '
dec2="IIF(unidad=`0',`veinte ',`veinti')"
dec3="`treinta '+IIF(unidad<>`0',`y ',`')"
dec4="`cuarenta '+IIF(unidad<>`0',`y ',`')"
dec5="`cincuenta '+IIF(unidad<>`0',`y ',`')"
dec6="`sesenta '+IIF(unidad<>`0',`y ',`')"
dec7="`setenta '+IIF(unidad<>`0',`y ',`')"
dec8="`ochenta '+IIF(unidad<>`0',`y ',`')"
dec9="`noventa '+IIF(unidad<>`0',`y ',`')"
cen0="`'"
cen1="IIF(decena+unidad=`00',`cien ',`ciento ')"
cen2="`doscient'+IIF(grupo<3,`os ',`os ')"
cen3="`trescient'+IIF(grupo<3,`os ',`os ')"
cen4="`cuatrocient'+IIF(grupo<3,`os ',`os ')"
cen5="`quinient'+IIF(grupo<3,`os ',`os ')"
cen6="`seiscient'+IIF(grupo<3,`os ',`os ')"
cen7="`setecient'+IIF(grupo<3,`os ',`os ')"
cen8="`ochocient'+IIF(grupo<3,`os ',`os ')"
cen9="`novecient'+IIF(grupo<3,`os ',`os ')"
conect1="`'"
conect2="IIF(grupos[2]>`000',`mil ',`')"
conect3="IIF(grupos[3]>`000'.or.grupos[4]>`000',IIF(grupos[3]=`001',`millon ',`millones '),`')"
conect4="IIF(grupos[4]>`000',`mil ',`')"
conect5="IIF(grupos[5]>`000',IIF(grupos[5]=`001',`billon ',`billones '),`')"
enletra=`'
numstr=ALLTRIM(STR(numero,12))
*Conversion a caracter del nro.justificado con 0 a la izq.
numstr=REPLICATE(`0',15-LEN(numstr))+numstr
FOR grupo=1 TO 5
 grupos[5-grupo+1]=SUBSTR(numstr,(grupo-1)*3+1,3)
NEXT
*proceso
FOR grupo=5 TO 1 STEP -1
 unidad=RIGHT(grupos[grupo],1)
 decena=SUBSTR(grupos[grupo],2,1)
 centena=LEFT(grupos[grupo],1)
 unilet=uni&unidad
 declet=dec&decena
 cenlet=cen&centena
 grupostr=STR(grupo,1)
 conector=conect&grupostr
 enletra=enletra+&cenlet+&declet+&unilet+&conector
NEXT
RETURN enletra

