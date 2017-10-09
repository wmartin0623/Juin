*********************************************************
*   Funciones de Usuario                                *
*********************************************************
*Ingreso de nuevas claves de acceso
FUNCTION cripta
PARAMETERS arch
IF PCOUNT() = 0
 PRIVATE arch
 arch = "CLAVE"
ENDIF
AREA()
IF USAR("&arch")
 INDEX ON nombre TO &arch
ELSE
 CLOSE ALL
 QUIT
ENDIF
PRIVATE pan_fun,col_fun,are_fun
pan_fun = SAVESCREEN(0,0,24,79)
col_fun = SETCOLOR()
are_fun=LTRIM(STR(SELECT()))
SET CURSOR OFF
GO TOP
SOMBRA(9,20,12,50,'H1',1,'',color3)
SETCOLOR("I")
@9,25 SAY 'NUEVA CLAVE ACCESO'
@10,20 TO 10,50 DOUBLE
DO WHILE LASTKEY()<>27
 vnuevos=SPACE(8)
 @11,30 GET vnuevos PICTURE '@!'
 READ
 IF LASTKEY()=27
  LOOP
 ENDIF
 SEEK CRIPT(vnuevos)
 IF !FOUND()
  GO BOTTOM
  ADDREC(0)
  RECLOCK(0)
  REPLACE nombre WITH CRIPT(vnuevos)
  UNLOCK
  EXIT
 ELSE
  IF CARTEL(vnuevos+' YA EXISTE ... [ REESCRIBE ]   [ ABORTA ]',17,1,'N',color3)
   IF LASTKEY() = 27
    LOOP
   ENDIF
   RECLOCK(1)
   REPLACE nombre WITH CRIPT(vnuevos)
   UNLOCK
  ENDIF
 ENDIF
ENDDO
SET CURSOR ON
RESTSCREEN(0,0,24,79,pan_fun)
SETCOLOR(col_fun)
USE
SELECT &are_fun
RETURN []
*aqui modifique

*Solicitud clave de acceso
FUNCTION cripta01
PARAMETERS arch
IF PCOUNT() = 0
 PRIVATE arch
 arch = "CLAVE"
ENDIF
AREA()
IF USAR("&arch")
 INDEX ON nombre TO &arch
ELSE
 CLOSE ALL
 QUIT
ENDIF
PRIVATE pan_fun,col_fun,are_fun
pan_fun = SAVESCREEN(0,0,24,79)
col_fun = SETCOLOR()
are_fun=LTRIM(STR(SELECT()))
SETCOLOR(color3)
*CARTEL('INGRESE CLAVE ACCESO [        ]',12,1,'E',color3)
SOMBRA(12,24,14,57,"H1",1,'',color3)
TONE(500,1)
@13,25 SAY "INGRESE CLAVE ACCESO"
SETCOLOR(color1)
@13,47 SAY SPACE(8)
@13,47 SAY ""
SETCOLOR(color3)
c=0
DO WHILE .T.
 vnuevos=''
 vc=47
 DO WHILE LEN(vnuevos)<9
  tecla=INKEY(0)
  IF tecla=13
   EXIT
  ENDIF
  vnuevos=vnuevos+CHR(tecla)
  @13,vc SAY '±'
  vc=vc+1
 ENDDO
 vnuevos =SUBSTR(UPPER(vnuevos)+SPACE(8),1,8)
 SEEK CRIPT(vnuevos)
 IF !FOUND()
  c=c+1
  IF c=3
   RESTSCREEN(0,0,24,79,pan_fun)
   SETCOLOR(col_fun)
   SELECT &are_fun
   RETURN .F.
  ELSE
   CARTEL('CLAVE INCORRECTA...... [ OK ]',22,1,'N',color3)
   SETCOLOR(color1)
   @13,47 say SPACE(8)
   @13,47 SAY ""
   SETCOLOR(color3)
   LOOP
  ENDIF
 ELSE
  RESTSCREEN(0,0,24,79,pan_fun)
  SETCOLOR(col_fun)
  SELECT &are_fun
  RETURN .T.
 ENDIF
ENDDO

*Solicitud clave de acceso para cada Usuario
FUNCTION cripta02
PARAMETERS arch,fUsuario
IF PCOUNT() = 0
 PRIVATE arch
 arch = "CLAVE"
ENDIF
AREA()
IF USAR("&arch")
 INDEX ON Nomb+nombre TO &arch
ELSE
 CLOSE ALL
 QUIT
ENDIF
PRIVATE pan_fun,col_fun,are_fun
pan_fun = SAVESCREEN(0,0,24,79)
col_fun = SETCOLOR()
are_fun=LTRIM(STR(SELECT()))
SETCOLOR(color3)
SOMBRA(12,24,14,57,"H1",1,'',color3)
TONE(500,1)
@13,25 SAY "INGRESE CLAVE ACCESO"
SETCOLOR(color1)
@13,47 SAY SPACE(8)
@13,47 SAY ""
SETCOLOR(color3)
c=0
DO WHILE .T.
 vnuevos=''
 vc=47
 DO WHILE LEN(vnuevos)<9
  tecla=INKEY(0)
  IF tecla=13
   EXIT
  ENDIF
  vnuevos=vnuevos+CHR(tecla)
  @13,vc SAY '±'
  vc=vc+1
 ENDDO
 vnuevos =SUBSTR(UPPER(vnuevos)+SPACE(8),1,8)
 SEEK fUsuario+CRIPT(vnuevos)
 IF !FOUND()
  c=c+1
  IF c=3
   RESTSCREEN(0,0,24,79,pan_fun)
   SETCOLOR(col_fun)
   SELECT &are_fun
   RETURN .F.
  ELSE
   CARTEL('CLAVE INCORRECTA...... [ OK ]',22,1,'N',color3)
   SETCOLOR(color1)
   @13,47 say SPACE(8)
   @13,47 SAY ""
   SETCOLOR(color3)
   LOOP
  ENDIF
 ELSE
  RESTSCREEN(0,0,24,79,pan_fun)
  SETCOLOR(col_fun)
  SELECT &are_fun
  RETURN .T.
 ENDIF
ENDDO

FUNCTION hay_reg
IF RECCOUNT()=0
 CARTEL("ARCHIVO SIN REGISTROS....  [ OK ]",12,1,'N',color3)
ENDIF
RETURN IIF(RECCOUNT()=0,.F.,.T.)

FUNCTION NORMA
?? CHR(27)+CHR(120)+CHR(0)+CHR(18)
RETURN[]

FUNCTION CHICO
?? CHR(27)+CHR(120)+CHR(0)+CHR(15)
RETURN[]

FUNCTION GRAN_ON
?? CHR(27)+CHR(87)+CHR(1)
RETURN[]

FUNCTION GRAN_OFF
?? CHR(27)+CHR(87)+CHR(0)
RETURN[]

******************
*FUNCION CARTEL  *
******************
*msj = mensaje
*y = linea de pantalla para desplazamiento
*opcion = N§ de opcion a resaltar
*sentido = N - normal  E - Entrada
*inv= color
FUNCTION CARTEL
PARA msj,y,opcion,sentido,inv
PRIV largo,x,primero,segundo,finprimero,finsegundo
PRIV xa,xa1,p,recuadro
IF y>=22
 y=21
ENDIF
IF PCOUNT() = 4
 PRIVATE inv
 inv=color3
ENDIF
largo=LEN(msj)
col_ant=SETCOLOR()
x=INT((80-largo)/2) &&posicion del mensaje
IF UPPER(sentido)='N'
 *me fijo donde estan los dos '[' ']'
 primero=AT('[',msj)
 segundo=RAT('[',msj)
 finprimero=AT(']',msj)
 finsegundo=RAT(']',msj)
 prom1=SUBSTR(msj,primero+1,finprimero-primero-1)
 prom2=SUBSTR(msj,segundo+1,finsegundo-segundo-1)
ENDIF
f=INT((80-largo)/2)
p=INT((80-(largo+2+f))/3)   &&calculo la posicion de los 4 BOX
xa=x
xa1=xa+largo+1
TONE(700,1)
pant=SAVESCREEN(y,xa,y+3,xa1+1)
SOMBRA(y,xa,y+2,xa1,"H2",4,'',inv)
SETCOLOR(inv)
@y+1,xa+1 SAY msj
IF UPPER(sentido)='N'
 SETCOLOR(inv)
 @y+1,primero+x+1 PROMPT prom1
 @y+1,segundo+x+1 PROMPT prom2
 MENU TO opcion
ELSE
 IF UPPER(sentido)='E'
  SETCOLOR(col_ant)
  RETURN(.F.)
 ENDIF
ENDIF
RESTSCREEN(y,xa,y+3,xa1+1,pant)
SETCOLOR(col_ant)
IF LASTKEY() = 27
 RETURN(.F.)
ENDIF
IF opcion=1
 RETURN(.T.)
ELSE
 RETURN(.F.)
ENDIF

FUNCTION DUDA
PARA varmen,eleccion,col_or,fila,col,op1st,op2nd
PRIV col_pan,pantalla
pantalla=SAVESCREEN(0,0,24,79)
col_pan=SETCOLOR()
IF PCOUNT()=7
 ncol=MAX(LEN(op1st)+LEN(op2nd)+9,LEN(varmen))+2
ENDIF
DO CASE
 CASE PCOUNT()=1
  PRIV eleccion,col_or,fila,ncol,col,op1st,op2nd
  col_or=colo3
  op1st=' SI '
  op2nd=' NO '
  eleccion=1
  fila=15
  ncol=MAX(13,LEN(varmen))+2
  col=INT((80-ncol)/2)
 CASE PCOUNT()=2
  PRIV col_or,fila,ncol,col,op1st,op2nd
  col_or=colo3
  op1st=' SI '
  op2nd=' NO '
  fila=15
  ncol=MAX(13,LEN(varmen))+2
  col=INT((80-ncol)/2)
 CASE PCOUNT()=3
  PRIV fila,ncol,col,op1st,op2nd
  fila=15
  ncol=MAX(13,LEN(varmen))+2
  col=INT((80-ncol)/2)
  op1st=' SI '
  op2nd=' NO '
 CASE PCOUNT()=4
  PRIV ncol,col,op1st,op2nd,ncol
  op1st=' SI '
  op2nd=' NO '
  ncol=MAX(13,LEN(varmen))+2
  col=INT((80-ncol)/2)
 CASE PCOUNT()=5
  PRIV ncol,op1st,op2nd,ncol
  op1st=' SI '
  op2nd=' NO '
  ncol=MAX(13,LEN(varmen))+2
ENDCASE
SETCOLOR(col_or)
@fila,col,fila+5,col+ncol+IIF(ISPAR(ncol),2,1)BOX('ÉÍ»º¼ÍÈº ')
BOXSHADOW(fila,col,fila+5,col+ncol+IIF(ISPAR(ncol),2,1))
SET CURSOR OFF
TONE(400,1)
op1st=CENTRA(op1st,INT(ncol/2)-3)
op2nd=CENTRA(op2nd,INT(ncol/2)-3)
@fila+1,1+col SAY CENTRA(varmen,ncol)
@fila+2,2+col TO fila+4,3+col+LEN(op1st)
@fila+2,5+col+LEN(op1st) TO fila+4,6+col+LEN(op1st)+LEN(op2nd)
@fila+3,3+col PROMPT op1st
@fila+3,6+col+LEN(op1st) PROMPT op2nd
MENU TO eleccion
IF LASTKEY()=27
 eleccion=0
ENDIF
SET CURSOR ON
RESTSCREEN(0,0,24,79,pantalla)
SETCOLOR(col_pan)
RETURN IIF(eleccion=1,.T.,.F.)

FUNCTION ULTDIA
PARA fechi
PRIV d,m,a,q9
d=VAL(LEFT(fechi,2))
m=VAL(SUBSTR(fechi,4,2))
a=VAL(RIGHT(fechi,2))
q9='00312831303130313130313031'
q9=IIF(INT(a/4)*4=a,STUFF(q9,5,2,'29'),q9)
RETURN IIF(d=VAL(SUBSTR(q9,2*m+1,2)),.T.,.F.)

FUNCTION MESES
PARA fechi
IF PCOUNT()=0
 PRIV fechi
 fechi=DATE()
ENDIF
IF TYPE("fechi")#"D"
 RETURN ""
ENDIF
PRIV vpos,vtabla[12]
vtabla[1]='ENE'
vtabla[2]='FEB'
vtabla[3]='MAR'
vtabla[4]='ABR'
vtabla[5]='MAY'
vtabla[6]='JUN'
vtabla[7]='JUL'
vtabla[8]='AGO'
vtabla[9]='SET'
vtabla[10]='OCT'
vtabla[11]='NOV'
vtabla[12]='DIC'
vpos=MONTH(fechi)
RETURN vtabla[vpos]

FUNCTION FECHAS
PARA de,ha
PUBLIC fe_des,fe_has
IF PCOUNT()=0
 fe_des=CTOD('  /  /    ')
 fe_has=CTOD('  /  /    ')
ELSE
 fe_des=de
 fe_has=ha
ENDIF
SAVE SCREEN
SOMBRA(8,12,18,45,"H5",1,'',color3)
s=.T.
DO WHILE s
 d1=.T.
 DO WHILE d1
   fe_des=INGFECHA('Desde fecha : ',10,15,fe_des,.T.)
   IF fe_des=CTOD('  /  /    ')
    ok=1
    RESTORE SCREEN
    RETURN[]
   ENDIF
   d1=.F.
 ENDDO
 d1=.T.
 IF fe_des>fe_has
  fe_has=fe_des
 ENDIF
 DO WHILE d1
  fe_has=INGFECHA('Hasta Fecha : ',12,15,fe_has)
  IF fe_has=CTOD('  /  /    ')
   ok=1
   RESTORE SCREEN
   RETURN[]
  ENDIF
  IF fe_has<fe_des
   CARTEL('CONTROLE.. Fecha equivocada  [ OK ]',16,1,'N',color3)
   LOOP
  ENDIF
  d1=.F.
 ENDDO
 S=.F.
ENDDO
RESTORE SCREEN
RETURN[]

FUNCTION INGFECHA
PARA mensa,fila,col,fecha,limpia
PRIV pan_ant,col_ant
IF PCOUNT() = 0
 PRIVATE mensa,fila,col,fecha,limpia
 mensa = 'Ingrese Fecha : '
 fila = 12
 col = 20
 fecha = DATE()
 limpia = .F.
ENDIF
IF PCOUNT()=3
 PRIVATE fecha,limpia
 fecha=CTOD('  /  /    ')
 limpia=.F.
ENDIF
IF PCOUNT()=4
 PRIVATE limpia
 limpia=.F.
ENDIF
IF .NOT.limpia
 pan_ant = SAVESCREEN(fila-2,col-2,fila+4,col+LEN(mensa)+15)
ENDIF
col_ant = SETCOLOR()
SOMBRA(fila-1,col,fila+1,col+LEN(mensa)+13,'H5',1,'',INVER,'N+')
DO WHILE .T.
 @fila+4,col+4 SAY '[Esc]  p/Salir'
 @fila,col+1 SAY mensa GET fecha
 READ
 IF LASTKEY()=27
  SETCOLOR(col_ant)
  IF .NOT.limpia
   RESTSCREEN(fila-2,col-2,fila+4,col+LEN(mensa)+15,PAN_ANT)
  ENDIF
  RETURN CTOD("  /  /    ")
 ENDIF
 IF !DUDA('CONFIRMA : ')
  LOOP
 ELSE
  SETCOLOR(col_ant)
  IF .NOT.limpia
   RESTSCREEN(fila-2,col-2,fila+4,col+LEN(mensa)+15,PAN_ANT)
  ENDIF
  RETURN (fecha)
 ENDIF
ENDDO

FUNCTION AREA
PRIV contador
FOR contador=1 TO 50
 SELECT (contador)
 IF EMPTY(ALIAS())
  EXIT
 ENDIF
NEXT
RETURN contador

*FUNCTION CERO
*PARA vari,dig
*RETURN  STRZERO(vari,dig)

FUNCTION cript
PARA  cad1
PRIV long,car,pos
long=LEN(cad1)
pos=1
FOR pos=1 TO long
 car=CHR(ASC(SUBSTR(cad1,pos,1))+long)
 cad1=STUFF(cad1,pos,1,car)
NEXT
RETURN cad1

************************************************************************
*FUNCION: PROMPT()
*FORMATO: PROMPT(a_opciones,a_lugar_imp,color,elemento[,a_cod_sal][,a_mensajes,fila_mensaje])
*        cada elemento puede contener el caracter '`' chr(96) antecediendo
*        al caracter que se quiera resaltar
*        COLOR= 'N' normal    'I' Inverso
*        ARRAY_CODIGOS_SALIDA= tendra los codigos de salida de la funcion
*        si se omite, sera 13 [ENTER]
************************************************************************
FUNCTION PROMPT
PARA a_opc , a_pos , color , elem , a_salir , a_activo , a_col_act , a_mess , fil_mess
PRIV a_cod[LEN(a_opc)],col_act,cant,con,pos,fil,col,txt,tecla
SET KEY -9 TO
DO CASE
 CASE PCOUNT() = 4
  DECLARE a_salir[1],a_activo[LEN(a_opc)]
  a_salir[1]=13
  a_mess=''
  fil_mess=0
  AFILL(a_activo,"T")
  a_col_act=SETCOLOR()
 CASE PCOUNT() = 5
  DECLARE a_activo[LEN(a_opc)]
  a_mess=''
  fil_mess=0
  AFILL(a_activo,"T")
  a_col_act=SETCOLOR()
 CASE PCOUNT() = 6
  a_col_act=SETCOLOR()
  a_mess=''
  fil_mess=0
 CASE PCOUNT() = 7
  a_mess=''
  fil_mess=0
 CASE PCOUNT() = 8
  fil_mess=24
 CASE PCOUNT() >8 .AND. PCOUNT() # 9
  SETCOLOR(color3)
  CLEAR
  @0,0 SAY 'PROGRAMA ABORTADO, CANTIDAD DE PARAMETROS ERRONEOS EN PROMPT()'
  QUIT
ENDCASE
cant=LEN(a_opc)
i=0
DO WHILE elem<=cant
 IF a_activo[elem]="F"
  elem=IF(elem<cant,elem+1,1)
 ELSE
  EXIT
 ENDIF
 i=i+1
 IF i>cant
  EXIT
 ENDIF
ENDDO
col_act=SETCOLOR()
SET CURSOR OFF
*calcular codigos de acceso
FOR con=1 TO cant
 pos=AT('`',a_opc[con])
 a_cod[con]=ASC(SUBSTR(a_opc[con],IF(pos#0,pos+1,1),1))
NEXT con
*impresion
DO WHILE .T.
 FOR con=1 TO cant
  pos=AT('`',a_opc[con])
  fil=VAL(a_pos[con])
  col=VAL(SUBSTR(a_pos[con],AT(',',a_pos[con])+1))
  IF con=elem
   IF LEN(a_mess) # 0
    IF a_activo[con]="F"
     SETCOLOR(a_col_act)
    ELSE
     SETCOLOR(color7)
    ENDIF
    @fil_mess,0 SAY CENTRAR(a_mess[con],0,79)
   ENDIF
   *Elegimos color para opcion resaltada
   IF a_activo[con]="T"
    IF color = 'N'
     SETCOLOR(col_inv)
    ELSE
     SETCOLOR(color8)
    ENDIF
   ELSE
    SETCOLOR(a_col_act)
   ENDIF
  ELSE
   IF a_activo[con]="F"
    SETCOLOR(a_col_act)
   ENDIF
  ENDIF
  @fil,col SAY STRTRAN(a_opc[con],'`')
  *Elegimos color para total de opcion
  IF a_activo[con]="T"
   IF color='N'.OR.con=elem
    *SETCOLOR("+"+col_nor)
   ELSE
    SETCOLOR(color8)
   ENDIF
  ELSE
   SETCOLOR(a_col_act)
  ENDIF
  @fil,col+pos-1 SAY SUBSTR(a_opc[con],pos+1,1)
  SETCOLOR(col_act)
 NEXT con
 tecla=ASC(UPPER(CHR(INKEY(0))))
 IF tecla=-9 .OR. tecla=247
  CALCULATOR()
 ENDIF
 IF ASCAN(a_cod,tecla) # 0
  aux=elem
  elem=ASCAN(a_cod,tecla)
  IF a_activo[elem]="F"
   elem=aux
  ELSE
   SET CURSOR ON
   KEYBOARD CHR(13)
  ENDIF
 ENDIF
 IF ASCAN(a_salir,tecla) # 0
  IF (a_activo[elem]='F'.AND.tecla#13).OR.a_activo[elem]='T'
   SET CURSOR ON
   SET KEY -9 TO CALCULATOR
   RETURN(elem)
  ENDIF
 ENDIF
 DO CASE
  CASE tecla=24 .OR. tecla=4
   i=0
   DO WHILE .T.
    elem=IF(elem<cant,elem+1,1)
    IF a_activo[elem]="T"
     EXIT
    ENDIF
    i=i+1
    IF i>cant
     EXIT
    ENDIF
   ENDDO
   CLEAR TYPEAHEAD
  CASE tecla=5 .OR. tecla=19
   i=0
   DO WHILE .T.
    elem=IF(elem>1,elem-1,cant)
    IF a_activo[elem]="T"
     EXIT
    ENDIF
    i=i+1
    IF i>cant
     EXIT
    ENDIF
   ENDDO
   CLEAR TYPEAHEAD
  CASE tecla=27
   SET CURSOR ON
   RETURN(0)
 ENDCASE
ENDDO

FUNCTION CENTRAR
PARA txt,inicio,fin
PRIV extra,n,desplaza
extra=fin-inicio-LEN(txt)+1
n=MOD(extra,2)
desplaza=INT(extra/2)
RETURN(SPACE(desplaza)+txt+SPACE(desplaza+n))

FUNCTION CENTRA
PARA cad1,long,cad2
DO CASE
 CASE PCOUNT()=1
  PRIV cad2,long
  cad2=' '
  long=78
 CASE PCOUNT()=2
  PRIV cad2
  cad2=' '
ENDCASE
cad1=REPLICATE(cad2,INT((long-LEN(cad1))/2))+cad1+REPLICATE(cad2,INT((long-LEN(cad1))/2))
RETURN cad1

FUNCTION carga1
*formato : CARGA1(<nombre del vector>,<'cadena~cadena~....'>)
PARAMETER vec,elems
PRIV pos,con
con=1
pos=AT('~',elems)
DO WHILE pos<>0
 vec[con]=SUBSTR(elems,1,pos-1)
 elems=SUBSTR(elems,pos+1,LEN(elems)-pos)
 con=con+1
 pos=AT('~',elems)
ENDDO
vec[con]=elems
RETURN(.T.)

FUNCTION carga2
*formato : CARGA2(<nombre del vector>,<'numero,numero,....'>)
PARAMETER vec,elems
PRIV pos,con
con=1
pos=AT(',',elems)
DO WHILE pos<>0
 vec[con]=VAL(SUBSTR(elems,1,pos-1))
 elems=SUBSTR(elems,pos+1,LEN(elems)-pos)
 con=con+1
 pos=AT(',',elems)
ENDDO
vec[con]=VAL(elems)
RETURN(.T.)

**********************************************************************
*FUNCION: SOMBRA
*FORMATO: SOMBRA(<fil_sup>,<col_izq>,<fil_inf>,<col_der>[,modo][,veloc.]
*               [,car.sombra][,color vent][,color sombra])
*modo = 'xn' donde x=N (apertura normal) x=H (horizontal) x=V (vertical)
*            donde n=1,2,3,4,5 ¢ 6 (tipo de caja)
**********************************************************************
FUNCTION SOMBRA
PARA fs,ci,fi,cd,modo,salto,som,col_ven,col_som
PRIV forma[6],col_ant,con,con1
col_ant=SETCOLOR()
DO CASE
 CASE PCOUNT() < 4
  RETURN(.F.)
 CASE PCOUNT() = 4
  modo='N1'
  salto=2
  som='°'
  col_ven='N/W'
  col_som='W/N'
 CASE PCOUNT() = 5
  salto=2
  som='°'
  col_ven='N/W'
  col_som='W/N'
 CASE PCOUNT() = 6
  som='°'
  col_ven='N/W'
  col_som='W/N'
 CASE PCOUNT() = 7
  col_ven='N/W'
  col_som='W/N'
 CASE PCOUNT() = 8
  col_som='W/N'
ENDCASE
CARGA1(forma,'ÉÍ»º¼ÍÈº ~ÚÄ¿³ÙÄÀ³ ~ÖÄ·º½ÄÓº ~ÖÄ·³½ÍÓ³ ~         ~........ ')
SET CURSOR OFF
DO CASE
 CASE UPPER(SUBSTR(modo,1,1)) = 'N'
  SETCOLOR(col_ven)
  @fs,ci,fi,cd BOX forma[VAL(SUBSTR(modo,2,1))]
  SETCOLOR(col_som)
  FOR con=1 TO (fi-fs)
   @fs+con,cd+1 SAY som
  NEXT con
  @fi+1,ci+2 SAY REPLICATE(som,cd-ci)
 CASE UPPER(SUBSTR(modo,1,1)) = 'H'
  FOR con=1 TO (cd-ci) STEP salto
   SETCOLOR(col_ven)
   @fs,ci,fi,ci+con BOX forma[VAL(SUBSTR(modo,2,1))]
   SETCOLOR(col_som)
   @fi+1,ci+2 SAY REPLICATE(som,con)
  NEXT con
  IF con#cd-ci
   con=cd-ci
   SETCOLOR(col_ven)
   @fs,ci,fi,ci+con BOX forma[VAL(SUBSTR(modo,2,1))]
   SETCOLOR(col_som)
   @fi+1,ci+2 SAY REPLICATE(som,con)
  ENDIF
  FOR con=1 TO (fi-fs)
   @fs+con,cd+1 SAY som
  NEXT con
 CASE UPPER(SUBSTR(modo,1,1)) = 'V'
  FOR con=1 TO (fi-fs) STEP salto
   SETCOLOR(col_ven)
   @fs,ci,fs+con,cd BOX forma[VAL(SUBSTR(modo,2,1))]
   SETCOLOR(col_som)
   @fs+con,cd+1 say som
   @fs+con+1,ci+2 say replicate(som,cd-ci)
  next con
  if con # fi-fs
   con=fi-fs
   SETCOLOR(col_ven)
   @fs,ci,fs+con,cd BOX forma[val(SUBSTR(modo,2,1))]
   SETCOLOR(col_som)
   @fs+con,cd+1 say som
   @fs+con+1,ci+2 say replicate(som,cd-ci)
  ENDIF
ENDCASE
BOXSHADOW(fs,ci,fi,cd)
SETCOLOR(col_ant)
SET CURSOR ON
RETURN(.T.)

***
*  BoxShadow( <nTop>, <nLeft>, <nBottom>, <nRight> ) --> NIL
*  Draw a box shadow with see through
*/
FUNCTION BOXSHADOW( nTop, nLeft, nBottom, nRight ,fondo)
LOCAL nShadTop, nShadLeft, nShadBottom, nShadRight
PRIVATE col_ant
IF PCOUNT()=4
 fondo=8
ENDIF
col_ant=SETCOLOR()
nShadTop   := nShadBottom := MIN(nBottom + 1, MAXROW())
nShadLeft  := nLeft + 1
nShadRight := MIN(nRight + 1, MAXCOL())
RESTSCREEN( nShadTop, nShadLeft, nShadBottom, nShadRight,;
    TRANSFORM( SAVESCREEN(nShadTop, nShadLeft, nShadBottom, nShadRight),;
    REPLICATE("X"+CHR(fondo), nShadRight - nShadLeft + 1 ) ) )
nShadTop    := nTop + 1
nShadLeft   := nShadRight := MIN(nRight + 1, MAXCOL())
nShadBottom := nBottom
RESTSCREEN( nShadTop, nShadLeft, nShadBottom, nShadRight,;
    TRANSFORM( SAVESCREEN(nShadTop,  nShadLeft , nShadBottom,  nShadRight),;
    REPLICATE("X"+CHR(fondo), nShadBottom - nShadTop + 1 ) ) )
SETCOLOR(col_ant)
RETURN NIL

FUNCTION BUSCARE
PARAMETERS fs,ci,fi,cd,campo,titulo,separacion,indice,largo,fi_ed,co_ed,nombre,fi_no,co_no
PRIVATE cant_camp,reg_ant,camp_dbe[1],titu_dbe[1],punt_ind,BUSCA,col_act
cant_camp = LEN(campo)
DO CASE
CASE PCOUNT() = 5
 PRIVATE separacion[cant_camp],tiene_ind
 titulo = ''
 AFILL(separacion,1)
 tiene_ind = .F.
CASE PCOUNT() = 6
 PRIVATE separacion[cant_camp],tiene_ind
 AFILL(separacion,1)
 tiene_ind = .F.
CASE PCOUNT() = 7
 PRIVATE tiene_ind
 tiene_ind = .F.
CASE PCOUNT() = 14
 PRIVATE tiene_ind
 tiene_ind = .T.
OTHERWISE
 SETCOLOR("")
 CLEAR
 @ 0, 0 SAY 'ERROR: NUMERO DE PARAMETROS ERRONEOS EN BUSCAR()'
 QUIT
ENDCASE
reg_ant = RECNO()
camp_dbe[1] = ''
titu_dbe[1] = titulo
BUSCA = ''
col_act = SETCOLOR()
IF tiene_ind
 con = ASCAN(indice,INDEXORD())
 punt_ind = IF(con = 0,1,con)
 @fi_no,co_no SAY nombre[punt_ind]
 SETCOLOR("*"+col_act)
 @fi_ed,co_ed SAY '_'
 SETCOLOR(col_act)
ENDIF
FOR con = 1 TO cant_camp
 camp_dbe[1] = camp_dbe[1]+campo[con]+'+"'+SPACE(separacion[con])+'"'+IF(con < cant_camp,'+','')
NEXT con
SETCOLOR(col_act)
DBEDIT(fs,ci,fi,cd,camp_dbe,'BUSCAR_USR','',titu_dbe)
SETCOLOR(col_act)
IF LASTKEY() = 27
 GO reg_ant
ENDIF
RETURN(.T.)

FUNCTION buscar_usr
PARAMETERS modo
PRIVATE tecla
tecla = LASTKEY()
DO CASE
CASE modo = 0
 RETURN(1)
CASE modo = 1 .OR. modo = 2
 TONE(600,2)
 RETURN(1)
CASE modo = 3 .OR. tecla = 13 .OR. tecla =27
 RETURN(0)
CASE tiene_ind .and. tecla = -1
 punt_ind = IF(punt_ind < LEN(indice),punt_ind+1,1)
 SET ORDER TO indice[punt_ind]
 @fi_no,co_no SAY nombre[punt_ind]
 @fi_ed,co_ed SAY SPACE(largo[punt_ind])
 BUSCA = ''
 SETCOLOR("*"+col_act)
 @fi_ed,co_ed+LEN(BUSCA) SAY '_ '
 SETCOLOR(col_act)
 RETURN(2)
CASE tiene_ind .and. tecla = 8 .and. LEN(BUSCA) > 0
 BUSCA = SUBSTR(BUSCA,1,LEN(BUSCA)-1)
 @fi_ed,co_ed SAY BUSCA+SPACE(largo[punt_ind]-LEN(BUSCA))
 SETCOLOR("*"+col_act)
 @fi_ed,co_ed+LEN(BUSCA) SAY '_ '
 SETCOLOR(col_act)
 RETURN(1)
CASE tiene_ind .and. ((tecla >= 32 .and. tecla <= 122) .OR. (tecla >= 164 .and. tecla <= 165))
 IF INDEXORD()#0
  IF LEN(BUSCA)+1 <= largo[punt_ind]
   BUSCA = BUSCA+CHR(tecla)
  ELSE
   TONE(2)
  ENDIF
  @fi_ed,co_ed SAY BUSCA+SPACE(largo[punt_ind]-LEN(BUSCA))
  SETCOLOR("*"+col_act)
  @fi_ed,co_ed+LEN(BUSCA) SAY '_ '
  SETCOLOR(col_act)
  SET SOFTSEEK ON
  SEEK(BUSCA)
  IF !FOUND()
   SEEK(UPPER(BUSCA))
  ENDIF
  SET SOFTSEEK OFF
 ENDIF
 RETURN(1)
OTHERWISE
 RETURN(1)
ENDCASE

FUNCTION VACIA
SKIP
SKIP -1
RETURN(IF(RECCOUNT()=0,.T.,.F.))

FUNCTION TONO
PARAMETERS modo
DO CASE
CASE modo = 1
 TONE(1000,5)
CASE modo = 2
 TONE(100,3)
ENDCASE
RETURN(.T.)

****************************
*    FUNCION   [ EXICOD ]  *
****************************
* VERIFICAR SI EXISTE UN CODIGO O NO EXISTE DEPENDIENDO DE exis
* BASE1 = ES LA BASE DE DATOS DONDE ESTA EL CODIGO A BUSCAR
* NTX1  = ES EL NUMERO DE LA INDEXACION
* BASE2 = ES LA BASE DE DATOS QUE HAY QUE DEJAR ABIERTA CUANDO SALGA
* NTX2  = ES EL NUMERO DE LA INDEXACION PARA DEJAR ACTIVA
* CODIGO = ES  EL CODIGO QUE DESEAMOS BUSCAR
* COL    = ES LA COLUMNA DONDE DESEO QUE SALGA EL CARTEL DE `SI EXISTE !'
* EXIS   = SI ES .T. Verifica si existe el Codigo
* EXIS   = SI ES .F. Verifica si no existe
******************
FUNCTION EXICOD
PARAMETERS base1,ntx1,codigo,mensa,col,exis
PRIVATE base2,ntx2,pos1,pos2,men_aux
base2 = ALIAS()
ntx2 = INDEXORD()
IF EMPTY(codigo)
 RETURN(.F.)
ENDIF
IF PCOUNT() = 5
 PRIVATE exis
 exis = .T.
ENDIF
IF PCOUNT() = 4
 PRIVATE COL,exis
 col = 18
 exis = .T.
ENDIF
IF PCOUNT() = 3
 PRIVATE mensa,COL,exis
 mensa = "! ESE CODIGO YA EXISTE !  [ OK ]"
 col = 18
 exis = .T.
ENDIF
pos1=AT("[",mensa)
IF pos1#0
 men_aux=SUBSTR(mensa,pos1+1)
 pos2=AT("[",men_aux)
ELSE
 pos2=0
ENDIF
reg_ant = RECNO()
SELECT &base1
SET ORDER TO ntx1
SEEK codigo
IF FOUND() = exis
 TONO(2)
 exis=.F.
 exis=CARTEL(mensa,col,1,'N',color3)
 IF LASTKEY()=27
  exis=.F.
 ENDIF
 IF pos2=0
  exis=.F.
 ENDIF
 SELECT &base2
 SET ORDER TO ntx2
 GO reg_ant
 RETURN(exis)
ENDIF
SELECT &base2
SET ORDER TO ntx2
GO reg_ant
RETURN(.T.)

******************
*FUNCIONES DE RED*
******************
*USAR(<base>[,alias][,modo])
*Donde "base" es el nombre de la base de datos a usar
*Donde "alias" es el ALIAS de la base de datos
*Donde "modo" es = "C" para Compartida / "E" para Exclusiva
*Retorna .T. si la base se logro abrir con exito
*y .F. si no se pudo abrir
FUNCTION USAR
PARA base,alias,modo
PRIVATE pan,arch,mens
arch = IIF(AT('.',base)=0,base+".DBF",base)
DO CASE
 CASE PCOUNT()=1
  PRIV con
  con=AT('.',base)
  alias=IIF(con=0,base,SUBSTR(base,1,con-1))
  modo='C'
 CASE PCOUNT()=2
  modo='C'
  PRIV con
ENDCASE
IF !FILE("&arch")
 CARTEL("Base de Datos &alias..DBF no EXISTE   [ OK ]",14,1,'N',color3)
 RETURN .F.
ENDIF
modo=(SUBSTR(UPPER(modo),1,1)='E')
DO WHILE .T.
 IF modo
  USE &base ALIAS &alias EXCLUSIVE
 ELSE
  USE &base ALIAS &alias SHARED
 ENDIF
 IF !NETERR()
  RETURN(.T.)
 ENDIF
 pan = SAVESCREEN(0,0,24,79)
 mens='ESPERE, NO PUEDO ABRIR &base'
 SOMBRA(10,10,16,10+LEN(mens)+2,'H1',1,'',color3)
 @13,12 SAY "ESPERE, NO PUEDE ABRIR &base"
 INKEY(1)
 RESTSCREEN(0,0,24,79,pan)
 IF LASTKEY()=27
  RETURN(.F.)
 ENDIF
ENDDO

*Agregar un registro nuevo en red
*ADDREC(<tiempo>)
FUNCTION ADDREC
PARA tiempo
PRIV paso
APPEND BLANK
IF .NOT. NETERR()
 RETURN(.T.)
ENDIF
paso=(tiempo=0)
DO WHILE (paso.OR.tiempo>0)
 APPEND BLANK
 IF .NOT. NETERR()
  RETURN(.T.)
 ENDIF
 INKEY(.5) &&espera 1/2 segundo
 tiempo=tiempo-.5
ENDDO
RETURN(.F.)  &&no agrego registro

*Bloquea un registro
*RECLOCK(<tiempo>)
FUNCTION RECLOCK
PARA tiempo
PRIV paso
IF RLOCK()
 RETURN(.t.)  &&bloqueo
ENDIF
paso=(tiempo=0)
DO WHILE (paso.OR.tiempo>0)
 IF RLOCK()
  RETURN(.T.)  &&bloqueo
 ENDIF
 INKEY(.5)  &&espera 1/2 segundo
 tiempo=tiempo-.5
ENDDO
RETURN(.F.)  &&no bloqueo

*Bloqueo de un archivo
FUNCTION FilLock
PARA nSeconds
PRIVATE lForever
IF FLOCK()
   RETURN (.T.)      && Locked
ENDIF
lForever = (nSeconds = 0)
DO WHILE (lForever .OR. nSeconds > 0)
   INKEY(.5)         && Wait 1/2 second
   nSeconds = nSeconds - .5
   IF FLOCK()
      RETURN (.T.)   && Locked
   ENDIF
ENDDO
RETURN (.F.)         && Not locked

FUNCTION OPEN_DBF
PARAMETERS nom_dbf,nom_alias,modo,ntx1,clave1,ntx2,clave2
PRIVATE ntxs1,ntxs2
STORE .T. TO ntxs1,ntxs2
IF PCOUNT()=1
 PRIVATE nom_alias,modo
 nom_alias=nom_dbf
 modo='C'
 STORE .F. TO ntxs1,ntxs2
ENDIF
IF PCOUNT()=2
 PRIVATE modo
 modo='C'
 STORE .F. TO ntxs1,ntxs2
ENDIF
IF PCOUNT()=3
 STORE .F. TO ntxs1,ntxs2
ENDIF
IF PCOUNT()=5
 ntxs2=.F.
ENDIF
IF USAR("&nom_dbf","&nom_alias","&modo")
 IF ntxs1
  IF !FILE("&ntx1."+vRDD)
   INDEX ON &clave1 TO &ntx1
  ENDIF
 ENDIF
 IF ntxs2
  IF !FILE("&ntx2."+vRDD)
   INDEX ON &clave2 TO &ntx2
  ENDIF
 ENDIF
 IF ntxs1.AND.ntxs2
  SET INDEX TO &ntx1,&ntx2
 ELSE
  IF ntxs1.AND.!ntxs2
   SET INDEX TO &ntx1
  ENDIF
 ENDIF
ELSE
 QUIT
ENDIF
RETURN .T.

FUNCTION VA_TRAS
SET KEY -4 TO
CLEAR GETS
XD = -1
SET KEY -4 TO va_tras
RETURN []

FUNCTION DIST_COLOR
PARAMETERS xfil,xCOL,txt
PRIVATE col_ant,pos,con,atr,sub_txt
col_ant =  SETCOLOR()
pos = AT('~',txt)
con = 0
DO WHILE pos # 0
 txt = SUBSTR(txt,2)
 pos = AT('~',txt)
 atr = SUBSTR(txt,1,pos-1)
 txt = SUBSTR(txt,pos+1)
 pos = AT('~',txt)
 sub_txt = SUBSTR(txt,1,IF(pos # 0,pos-1,LEN(txt)))
 txt = SUBSTR(txt,pos)
 SETCOLOR(atr)
 @xfil,xCOL+con SAY sub_txt
 con = con+LEN(sub_txt)
ENDDO
SETCOLOR(col_ant)
RETURN(.T.)

FUNCTION div_cero
PARAMETERS divden,divisor
RETURN IIF(divisor=0,0,divden/divisor)

*----------------------------------------------------------*
* - DMA - Funcion Diferencia de fechas                     *
*----------------------------------------------------------*
FUNCTION DMA
PARA xfi,xfe
IF PCOUNT()<>2
 CARTEL('CANTIDAD DE PARAMETROS ERRONEOS  [ OK ]',12,1,'N',color3)
 RETURN(0)
ENDIF
fecc=MAX(xfi,xfe)
if xfe<>fecc
*CARTEL('!!! ATENCION !!!  Primer fecha debe ser Menor  [ OK ]',14,1,'N',color3)
 RETURN(0)
ENDIF
xai = YEAR(xfi)
xae = YEAR(xfe)
xmi = MONT(xfi)
xme = MONT(xfe)
xdi = DAY(xfi)
xde = DAY(xfe)
* calculo de dias *
vdias=0
if xai=xae
*si el anio y el mes son iguales
 if xmi=xme
  vdias=xde-xdi
 else
*si el mes es distinto y el anio igual
  xm=xmi+1
  if xm=13
   xm=1
  ENDIF
  vdias=ultimo(xmi,xai)-xdi
  do while xm<=(xme-1)
   y=ultimo(xm,xai)
   vdias=vdias+y
   xm=xm+1
   if xm=13
    xm=1
   ENDIF
  enddo
  vdias=vdias+xde
 ENDIF
else
*acumulacion de dias hasta fin de anio si los anios son distintos
 xm=xmi+1
 vdias=ultimo(xmi,xai)-xdi
 if xm<=12
   do while xm<=12
   y=ultimo(xm,xai)
   vdias=vdias+y
   xm=xm+1
  enddo
 ENDIF
* vdias=vdias+xde
*acumulacion de dias de todos los anios intermedios
 xa=xai+1
 if xa<=xae-1
  do while xa<=(xae-1)
   if int(xa/4)*4=xa
    k=1
   else
    k=0
   ENDIF
   vdias=vdias+365+k
   xa=xa+1
  enddo
 ENDIF
*acumulacion de dias del ultimo anio
 xm=1
 if xm<=xme-1
  do while xm<=(xme-1)
   y=ultimo(xm,xae)
   vdias=vdias+y
   xm=xm+1
   if xm=13
    xm=1
   ENDIF
  enddo
 ENDIF
 vdias=vdias+xde
ENDIF
RETURN IIF(vdias>=0,vdias,0)

FUNCTION ULTIMO &&Extrae ultimo dia mes
PARA m,a
PRIV q9
q9='00312831303130313130313031'
q9=IIF(INT(a/4)*4=a,STUFF(q9,5,2,'29'),q9)
RETURN (VAL(SUBSTR(q9,2*m+1,2)))

***********************************************
*ORDENACION DE UNA TABLA =Metodo de la BURBUJA*
***********************************************
* Donde : tabla_pri = tabla principal a ordenar
*         [tabla_aux] = Lista de tablas a ordenar con la Principal
FUNCTION ORDEN
PARAMETERS tabla_pri,tabla_aux
PRIVATE ind,i,pos,aux,none,y
DO CASE
 CASE PCOUNT() = 1
  none = 0
 CASE pcount() = 2
  none =1
 OTHERWISE
  CARTEL('N£mero de Parametros Erroneos en "ORDEN',12,1,'E',color3)
  CLOSE ALL
  CANCEL
ENDCASE
ind = LEN(tabla_pri)
FOR i = 1 TO ind-1
 FOR pos = 1 TO ind-i
  IF tabla_pri[pos] > tabla_pri[pos+1]
   aux = tabla_pri[pos]
   tabla_pri[pos] = tabla_pri[pos+1]
   tabla_pri[pos+1] = aux
   IF none = 1
    FOR y = 1 TO LEN(tabla_aux)
     var1 = tabla_aux[y]
     aux = &var1[pos]
     &var1[pos] = &var1[pos+1]
     &var1[pos+1] = aux
    NEXT
   ENDIF
  ENDIF
 NEXT
NEXT
RETURN []

FUNCTION PUNTO
* LA FUNCIO PUNTO DEVUELVE EL NUMERO EN TEXTO CON PUNTOS Y COMA
* LA PARTE DECIMAL ES SIEMPRE 00 SI NO MANDAMOS PARAMETROS
* DE LO CONTRARIO ES SIN DECIMAL
* EJ.:PUNTO(A)
PARAMETERS num,dec
PRIVATE valor,txt
valor = TRANSFORM(ROUND(num,2),'9,999,999,999.99')
txt=STRTRAN(valor,'.','@')
txt=STRTRAN(txt,',','.')
txt=STRTRAN(txt,'@',',')
*TXT=SUBSTR(txt,1,LEN(TXT)-3)+','+ SUBSTR(TXT,LEN(TXT)-1,2)
RETURN txt

*CREAR UNA BASE DE DATOS
FUNCTION CREAR
PARA nombre_arc,campos,tipos,longi,deci
oko=.T.
IF PCOUNT()#5 .OR. LEN(nombre_arc)>8 .OR.LEN(campos)>1024 .OR.TYPE("campos")#"A".OR.;
 TYPE("tipos")#"A".OR.TYPE("longi")#"A".OR.TYPE("deci")#"A"
 oko=.F.   && error
ENDIF
FOR i=1 TO LEN(campos)
 IF LEN(campos[i])>10.OR.tipos[i]="C".AND.longi[i]>64*1024.OR.;
  tipos[i]="M".AND.longi[i]#10.OR.tipos[i]="N".AND.longi[i]+2+deci[i]>19.OR.tipos[i]="N".AND.longi[i]<=deci[i].OR.tipos[i]="D".AND.longi[i]#8
  oko=.F.   && error
 ENDIF
NEXT
IF !oko
 @10,10 SAY 'Parametros erroneos'
 RETURN .F.
ENDIF

************************************
**Procesos de creacion
AREA()
CREATE fprov
USE fprov
FOR i=1 TO LEN(campos)
 APPEND BLANK
 REPLACE field_name WITH campos[i]
 REPLACE field_type WITH tipos[i]
 REPLACE field_len WITH longi[i]
 REPLACE field_dec WITH deci[i]
NEXT
CREATE &nombre_arc FROM fprov
USE
ERASE fprov.dbf
RETURN .T.

*Funcion para mandar una tecla al buffer del teclado
FUNCTION ULTECLA
PARAMETERS tecla
KEYBOARD CHR(tecla)
INKEY()
RETURN []

FUNCTION VALIDA
******************************************************
* cod = Codigo a Controlar
* nom = Nombre del Codigo a buscar (DEVOLUCION DE ESTA FUNCION)
* base= Alias de la Base de Datos donde debemos buscar
* campo= Lista de campos a mostrar en ayuda en caso de no encontrar el buscado
*        Ej.  "codi~nomb"
* lon1 = longitud del campo 1 para busqueda rapida en ayuda
* lon2 = longitud del campo 2 para busqueda rapida en ayuda
* cam1 = Nombre del campo a buscar
* cam2 = Nombre del campo a retornar
* vbla = Si es .T. permite Codigo "cod" en blanco
*      = Si es .F. No permite Codigo "Cod" en blanco
********************************************************
PARAMETERS cod,nom,base,campo,lon1,lon2,cam1,cam2,vbla
IF LASTKEY() = 27
 KEYBOARD CHR(27)
 RETURN .T.
ENDIF
IF PCOUNT()=8
 PRIVATE vbla
 vbla = .F.
ENDIF
PRIVATE colo_a
antsel=ALLTRIM(STR(SELECT()))
colo_a=SETCOLOR()

cod=CERO(VAL(cod),lon1)
*Permite Codigo en blanco
IF VAL(cod)=0 .AND. vbla
 RETURN .T.
ENDIF

SELECT &base
SET ORDER TO 1
SEEK cod
IF .NOT. FOUND()
 *Ayuda General
 archivo=base
 campi=campo
 ntx1=1
 ntx2=2
 l1=lon1
 l2=lon2
 IF AYUDI(campi,ntx1,ntx2,l1,l2,archivo) = .T.
  cod=&cam1
  nom=&cam2
  SELECT &antsel
  RETURN .T.
 ELSE
  SELECT &antsel
  RETURN .F.
 ENDIF
ELSE
 cod=&cam1
 nom=&cam2
 SELECT &antsel
 RETURN .T.
ENDIF
RETURN .T.

FUNCTION AYUDI
PARAMETERS campi,ntx1,ntx2,l1,l2,archi
PRIVATE col_col
col_col = SETCOLOR()
IF RECCOUNT()=0
 pan = SAVESCREEN(20,31,23,61)
 SETCOLOR(INVER)
 @20,31 CLEAR TO 23,61
 @20,31 TO 23,61 DOUBLE
 @21,36 SAY '   PRESTE ATENCION  '
 @22,32 SAY 'Ä ARCHIVO '+archi+' VACIO Ä'
 SET CURSOR OFF
 TONO(2)
 INKEY(0)
 SET CURSOR ON
 SETCOLOR(col_col)
 RESTSCREEN(20,31,23,61,pan)
 RETURN .F.
ENDIF
SET ORDER TO 1
pan = SAVESCREEN(5,29,24,78)
SETCOLOR(color3)
SOMBRA(5,29,23,77,'H1',1,'',color3)
SETCOLOR(color3)
@ 7,29  SAY 'ÌÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹'
@ 20,29 SAY 'ÇÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¶'
@ 17,29 SAY 'ÌÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹'
@ 5,41 SAY ' BUSCAR '+archivo
@18,30 SAY 'BUSCAR:'
@ 21,30 SAY CHR(24)+CHR(25)+' PgUp PgDn mover          '+CHR(17)+'ÄÙ-ACEPTA'
@ 22,30 SAY "F2-cambia clave      ESC-salir"
DECLARE cam[2],sep[2],ind[2],lar[2],nom[2]
carga1(cam,campi)
carga2(sep,'10,0')
titul1=archi+'          NOMBRE      '
ind[1]=ntx1
lar[1]=l1
nom[1]=archi
ind[2]=ntx2
lar[2]=l2
nom[2]='NOMBRE :'
ord_ant=STR(INDEXORD())
SET ORDER TO ind[2]
GO TOP
SETCOLOR(color3)
BUSCARE(6,31,16,75,cam,titul1,sep,ind,lar,19,31,nom,18,41)
SETCOLOR(col_col)
SET ORDER TO &ord_ant
RESTSCREEN(5,29,24,78,pan)
IF LASTKEY()=27
 RETURN .F.
ENDIF
RETURN .T.

*Pasa un Vto.de un sab/dom al lunes siguiente
FUNCTION LUNES(ffech)
PRIVATE fdias
fdias=DOW(ffech)
IF fdias=7
 ffech=ffech+2
ELSE
 IF fdias=1
  ffech=ffech+1
 ENDIF
ENDIF
RETURN ffech

FUNCTION VALLA(valor,whor,wver)
IF PCOUNT()=1
 whor=24
 wver=79
ENDIF
@whor,wver SAY qbarra[valor]
valor=valor+1
IF valor>4
 valor=1
ENDIF
RETURN NIL

FUNCTION TIP_IVA(iba)
RETURN ( IIF(iba='1','R.I.',;
         IIF(iba='2','RNI.',;
         IIF(iba='3','C.F.',;
         IIF(iba='4','EXE.',;
         IIF(iba='5','M.T.',;
         IIF(iba='6','N.A.',;
         IIF(iba='7','N.C.',;
         '    ' ))))))) )

FUNCTION SALPRO
*Abrir Saldo Proveedores
AREA()
OPEN_DBF("SALPROVE","SALPR","C","SALPROVE","prov")
RETURN .T.

FUNCTION CALCULATOR()
PRIVATE entry,fcode,mcode,lDecSet,enter,inkey,code,;
        Pan_Fon,Pan_Cal,Pan_NewC,;
        pos,NumStr,Can_Int,Max_Int,;
        Can_Dec,Tot_Int,Tot_Dec,Dec_Ant,LenStr
PRIVATE _FIL1,_FIL2,_COL1,_COL2,_F1,_C1,_F2,_C2
PRIVATE Col_Ant,Cur_Ant,oGet,vFix,FilCol:={},Cad1,Cad2,Ctr_Paste
PRIVATE nTotal,nSavTotal,lNewNum,lDivideErr,nSavSubTotal,nAddMode
PRIVATE prm
*Variable para Simular el Papel
PRIVATE Papel:={},vRow,Resta,vPorc,l,Primero,cDefTotPict
PRIVATE vMascara

*Coordenadas de Cada Tecla
FilCol:={"ESC-03,01"," / -03,05"," * -03,09"," - -03,13"," C -03,17","AC -03,21",;
         " 7 -05,01"," 8 -05,05"," 9 -05,09",            "M+ -05,17","MR -05,21","M* -05,25",;
         " 4 -07,01"," 5 -07,05"," 6 -07,09"," + -06,13","M- -07,17","MC -07,21","M/ -07,25",;
         " 1 -09,01"," 2 -09,05"," 3 -09,09"," = -10,13"," % -09,17","CE -09,21"," F -09,25",;
         " 0 -11,03",            " . -11,09",            " R -11,17"," H -11,21"}

*Captura Variable de Origen
oGet:=GetActive()

*Cantidad Maxima de Enteros
Max_Int=15

*Obtener Mascara para PICTURE y Total de ENTEROS y DECIMALES
*Solamente si la variable es NUMERICA
PRIVATE vVar:=READVAR()
IF oGet # NIL
 IF oGet:Type == "N"
  vMascara:=oGet:Picture
  *Controla si existe Mascara: Si no existe no Habilito Control-Paste
  IF vMascara = NIL
   vMascara="999999999999.99"
   Ctr_Paste=.F.
  ELSE
   Ctr_Paste=.T.
  ENDIF
  Tot_Int=Max_Int-IIF(AT('.',vMascara)#0,LEN(SUBSTR(vMascara,AT('.',vMascara)+1))-1,0)
  Tot_dec:=IIF(AT('.',vMascara)#0,LEN(SUBSTR(vMascara,AT('.',vMascara)+1)),0)
  Val_Var:=&vVar
  Cad1=TRANSFORM(Val_Var,vMascara)
 ELSE
  Tot_Int=Max_Int - 3
  Tot_Dec=2
  Cad1="0"
  Val_Var=0
  vMascara=oGet:Picture
  *Controla si existe Mascara: Si no existe no Habilito Control-Paste
  IF vMascara = NIL
   vMascara="999999999999.99"
   Ctr_Paste=.F.
  ELSE
   Ctr_Paste=.T.
  ENDIF
  IF VALTYPE(&vVar) # "N"
   LenStr=LEN(&vVar)
  ELSE
   LenStr = 1
  ENDIF
 ENDIF
ELSE
 Tot_Int=Max_Int - 3
 Tot_Dec=2
 Cad1="0"
 Val_Var=0
 Ctr_Paste=.F.
ENDIF

Col_Ant=SETCOLOR()
*Captura Estado Cursor y lo Oculta
Cur_Ant  :=SETCURSOR(0)
*Desactivo llamadas a la Funcion para Evitar Entrar Nuevamente
SET KEY -9 TO
SET KEY 247 TO
*DEfino Coordenadas Iniciales Fijas
_F1 =  8
_C1 = 50
_F2 = 23
_C2 = 78
*DEfino Coordenadas Iniciales Variables
_FIL1 =  8
_COL1 = 50
_FIL2 = 23
_COL2 = 78


SET BELL OFF
*Captura Contidad de Decimales al Entrar en la Funcion
Dec_Ant:=SET(_SET_DECIMALS)

*Armar pantalla

*Capturo Pantalla Actual
Pan_Fon = SAVESCREEN(_FIL1,_COL1,_FIL2,_COL2)
Primero=.T.

Linea1:="      ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿  "
Linea2:="ÚÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄ¿"

SETCOLOR(color3)
@ 8,50 SAY "      ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿  "
@ 9,50 SAY "ÚÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄ¿"
@10,50 SAY "³                      0    ³"
@11,50 SAY "ÃÄÄÄÂÄÄÄÂÄÄÄÂÄÄÄÂÄÄÄÂÄÄÄÂÄÄÄ´"
@12,50 SAY "³ESC³ / ³ * ³ - ³ C ³AC ³   ³"
@13,50 SAY "ÃÄÄÄÅÄÄÄÅÄÄÄÅÄÄÄÅÄÄÄÅÄÄÄÅÄÄÄ´"
@14,50 SAY "³ 7 ³ 8 ³ 9 ³   ³M+ ³MR ³M* ³"
@15,50 SAY "ÃÄÄÄÅÄÄÄÅÄÄÄ´ + ÃÄÄÄÅÄÄÄÅÄÄÄ´"
@16,50 SAY "³ 4 ³ 5 ³ 6 ³   ³M- ³MC ³M/ ³"
@17,50 SAY "ÃÄÄÄÅÄÄÄÅÄÄÄÅÄÄÄÅÄÄÄÅÄÄÄÅÄÄÄ´"
@18,50 SAY "³ 1 ³ 2 ³ 3 ³   ³ % ³CE ³ F ³"
@19,50 SAY "ÃÄÄÄÁÄÄÄÅÄÄÄ´ = ÃÄÄÄÅÄÄÄÅÄÄÄ´"
@20,50 SAY "³   0   ³ . ³   ³ R ³ H ³   ³"
@21,50 SAY "ÃÄÄÄÄÄÄÄÁÄÄÄÁÄÄÄÁÄÄÄÁÄÄÄÁÄÄÄ´"
@22,50 SAY "³ M                         ³"
@23,50 SAY "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"

*Capturar Pantalla Con Calculadora ya Impresa
Pan_Cal = SAVESCREEN(_FIL1,_COL1,_FIL2,_COL2)

*Pantalla a usar Cuando se utilice "CE"
Pan_NewC = SAVESCREEN(_FIL1,_COL1,_FIL2,_COL2)

*Tomo como valor Inicial el de la Variable de Origen

*Asigno Valores Iniciales
cDefTotPict:= '999999999999999'
nTotal:= nSavSubTotal:= calcmem:= Can_Int:= Can_Dec:= pos:= vRow:= vPorc:= 0
nSavTotal:=nTotal
entry=Val_Var
IF ROUND(entry,Tot_Dec) # 0
 lNewNum := .T.
ELSE
 lNewNum := .F.
ENDIF
lDivideErr := .F.
nAddMode := 1
lDecSet=.F.
fcode=' '
code=' '
enter=.T.
inkey=13
mcode=' '
Cad2:=' = '
Resta:=.T.
prm=.F.

*Rutina Principal
Act_Pan()
DO WHILE .T.

  *Lee el Teclado
  ESPERA 0 TO inkey

  *Obtengo el Valor de la Tecla
  code=UPPER(CHR(inkey))
  IF fcode='T'.and.enter.AND.(code='+'.OR.code='-'.OR.code='*'.OR.code='/')
   fcode=code
  ENDIF

  *Iluminar tecla
  *--------------
*ILUMINAR TECLA
  *0-9 . + - % / R H F = *
  IF ((inkey>=48 .AND. inkey<=57).OR.inkey=46.OR.inkey=43.OR.inkey=45.OR.;
      inkey=37.OR.inkey=47.OR.inkey=82.OR.inkey=114.OR.inkey=72.OR.inkey=104;
      .OR.inkey=70.OR.inkey=102.OR.inkey=13.OR.inkey=42).AND.mcode=' '
      IF inkey=13
       Cad2=' = '
      ELSE
       Cad2=' '+UPPER(CHR(inkey))+' '
      ENDIF
  ELSE
   Cad2=mCode+code+' '
  ENDIF
  pos=ASCAN(FIlCol,{|z| LEFT(z,3)==Cad2})
  IF pos#0
   SETCOLOR(color1)
   @_F1+VAL(SUBSTR(FilCol[pos],5,2))+1,_COL1+VAL(RIGHT(FilCol[pos],2)) SAY Cad2
   ESPERA .06 TO tecla
   SETCOLOR(color3)
   @_F1+VAL(SUBSTR(FilCol[pos],5,2))+1,_COL1+VAL(RIGHT(FilCol[pos],2)) SAY Cad2
  ENDIF
  *--------------

  *Si es el Primer Ingreso Asigno "" a la Var.que contiene los Ingresos Numericos
  IF !lNewNUm .AND. code$"1234567890."
   Cad1=""
   lNewNum=.T.
   entry=0
  ENDIF

  *Chequeo la Tecla Presionada
  DO CASE
    CASE code $'1234567890.'
      *Acumula en Cad1 (String) los Ingresos Numericos y el Punto
      *Controla la Cantidad de Enteros Ingresados
      IF .NOT.lDecSet.AND.Can_Int=Tot_Int
       LOOP
      ENDIF

      lTotalOk  := .F.
      lClAdder  := .F.                      // Reset the Clear flag
      lAddError := .F.                      // Reset adder error flag
      IF inKey=46                            // Period (.) decimal point
        IF lDecSet                          // Has decimal already been set
          RingBell(.T.)
        ELSE
          lDecSet := .T.
        ENDIF
      ELSE                                  // It must be a number input
        lNewNum := .T.
        nNum := inKey-48
        IF lDecSet                          // Decimal set
          IF Can_Dec<Tot_Dec             // Check how many decimals they are allowed
            Can_Dec := ++Can_Dec
            entry := entry+nNum/(10**Can_Dec)
          ENDIF
        ELSE
          entry := entry*10+nNum
          Can_Int++
        ENDIF
      ENDIF
      Cad1=Cad1+Code
      Act_Pan()

    CASE (code='+' .OR. code='-' .OR. code='/' .OR. code='*';
      .OR. code='R' .OR. code='%') .AND. mcode=' '
      AddNUM(code,fcode)
      fcode=code
      Act_Pan()
      Cad1=''

    CASE inKey = 13 .AND. mcode=' '
      *Actualizar Papel
      *Ultimo Elemento
      AddTotal()
      Act_Papel(Papel,IIF(vPorc#0,STR(ROUND(vPorc,Tot_Dec),Tot_Int,Tot_Dec)+' ','0 ')+'=')
      vPorc=0

      *Total
      Act_Papel(Papel,STR(ROUND(nTotal,Tot_dec),Max_Int,Tot_dec)+' '+'T')

      *Espacio en Blanco
      Act_Papel(Papel,' ')

      Cad1=STR(ROUND(nTotal,Tot_dec),Max_Int,Tot_dec)
      fcode='T'
      Act_Pan()

      *Poner en cero los ingresos

      entry := 0
      nSavTotal := nTotal
      nTotal    := 0
      lNewNum := .F.
      prm=.F.

    CASE inkey = 7 .OR. inkey = 8
     *Borrar
     IF LEN(Cad1) > 0
      pos=AT('.',Cad1)
      Cad1=LEFT(Cad1,LEN(Cad1)-1)
      entry = VAL(Cad1)
      IF pos # 0
       Can_Dec=Can_Dec - 1
      ELSE
       Can_Int=Can_Int - 1
      ENDIF
      Act_Pan()
     ENDIF

    CASE code='A'
      mcode='A'

    CASE code='M'
      mcode='M'

    CASE code='C' .AND. mcode=' '
      mcode='C'

    CASE code='C' .AND. mcode='A'
      *Borrar Todo, incluida la Memoria
      nTotal=0
      nSavTotal:=0
      entry=0
      calcmem=0
      mcode=' '
      fcode=' '
      Cad1=""
      Papel:={}
      Resta=.T.
      RESTSCREEN(_FIL1,_COL1,_FIL2,_COL2,Pan_Fon)
      Pan_Fon = SAVESCREEN(_F1,_COL1,_F2,_COL2)
      RESTSCREEN(_F1,_COL1,_F2,_COL2,Pan_NewC)
      Pan_Cal = SAVESCREEN(_F1,_COL1,_F2,_COL2)
      _FIL1=_F1

    CASE code='C' .AND. mcode='M'
      *Borra Memoria
      calcmem=0
      mcode=' '

    CASE mcode='C' .AND. code='E'
      *Borrar Ultima Entrada
      entry=0
      lDecSet=.F.
      mcode=' '
      fcode=' '
      lNewNum=.F.
      Cad1=''
      Act_Pan()

    CASE mcode='C' .AND. inkey=13
      *Borrar Total
      entry=0
      nTotal=0
      lDecSet=.F.
      mcode=' '
      fcode=' '
      Cad1=""
      KEYBOARD CHR(13)

    CASE  mcode='M' .AND. code='R'
      *Trae Memoria al Visor
      entry=calcmem
      lDecSet=.F.
      mcode=' '
      Cad1=STR(ROUND(CalcMem,Tot_dec),Max_Int,Tot_Dec)

    CASE mcode='M' .AND. code='-'
      *Resta a la Memoria
      calcmem=calcmem-nTotal
      mcode=' '
      entry=0
      fcode=' '

    CASE mcode='M' .AND. code='+'
      *Suma a la Memoria
      calcmem=calcmem+nTotal
      mcode=' '
      entry=0
      fcode=' '

    CASE mcode='M' .AND. code='*'
      *Multiplica por la Memoria
      calcmem=calcmem*nTotal
      mcode=' '
      entry=0
      fcode=' '

    CASE mcode='M' .AND. code='/'
      *Divide por la Memoria
      calcmem=nTotal/calcmem
      mcode=' '
      entry=0
      fcode=' '

    CASE code='H'
      *Ayuda
      AYUDA()

    CASE code='F'
     *Cantidad de Decimales
     ChangeDec()

    CASE inkey = 5 .AND.  _FIL1 > 0
       *Arriba
       *Recorrer Papel
       IF LEN(Papel) # 0
        l=SCR(Papel,;
              _F1-_FIL1,;                         //Posiciones Visibles
              _FIL1+1,;                           //Primer Fila
              _COL1+9,;                           //Primer Columna
              LEN(Papel),;                        //Long.del Array
              _FIL1-1,;                           //Fila Superior
              _COL1+9,;                           //Columna Superior
              _F1,;                               //Fila Inferior
              _COL2,;                             //Columna Inferior
              1,;                                 //Posicion Inicial del Cursor
              color1,;                            //Color Principal
              color3)                             //Color Cursor
        SETCURSOR(0)
       ENDIF

    CASE inkey = 24 .AND. _FIL2 < 24
       *Abajo
       *Recorrer Papel
       IF LEN(Papel) # 0
        l=SCR(Papel,;
              _F1-_FIL1,;                         //Posiciones Visibles
              _FIL1+1,;                           //Primer Fila
              _COL1+9,;                           //Primer Columna
              LEN(Papel),;                        //Long.del Array
              _FIL1-1,;                           //Fila Superior
              _COL1+9,;                           //Columna Superior
              _F1,;                               //Fila Inferior
              _COL2,;                             //Columna Inferior
              1,;                                 //Posicion Inicial del Cursor
              color1,;                            //Color Principal
              color3)                             //Color Cursor
        SETCURSOR(0)
       ENDIF

    CASE inkey = 19 .AND. _COL1 > 0
       *Izquierda
           RESTSCREEN(_FIL1,_COL1,_FIL2,_COL2,Pan_Fon)
          _COL1 = _COL1-1
          _COL2 = _COL2-1
          Pan_Fon = SAVESCREEN(_FIL1,_COL1,_FIL2,_COL2)
          RESTSCREEN(_FIL1,_COL1,_FIL2,_COL2,Pan_Cal)

    CASE inkey = 4 .AND. _COL2 < 79      //Mover
       *Derecha
          RESTSCREEN(_FIL1,_COL1,_FIL2,_COL2,Pan_Fon)
          _COL1 = _COL1+1
          _COL2 = _COL2+1
          Pan_Fon = SAVESCREEN(_FIL1,_COL1,_FIL2,_COL2)
          RESTSCREEN(_FIL1,_COL1,_FIL2,_COL2,Pan_Cal)

    CASE code='X' .OR. inkey=27   //Salir
          RESTSCREEN(_FIL1,_COL1,_FIL2,_COL2,Pan_Fon)
          SETCOLOR(Col_Ant)

          SET KEY -9 to CALCULATOR
          SET KEY 247 TO CALCULATOR
          SET DECIMAL TO Dec_Ant
          SETCURSOR(Cur_Ant)
          RETURN

    CASE inkey=10    //Paste
         IF Ctr_Paste
          IF TYPE(vVar)=="N"
           pos=AT('9',vMascara)
           IF pos # 0
            IF ROUND(IIF(nSavTotal#0.AND.entry=0,nSavTotal,entry),Tot_dec) <= VAL(SUBSTR(vMascara,pos))
             &vVar:=ROUND(IIF(nSavTotal#0.AND.entry=0,nSavTotal,entry),Tot_dec)
            ENDIF
           ELSE
            &vVar:=ROUND(IIF(nSavTotal#0.AND.entry=0,nSavTotal,entry),Tot_dec)
           ENDIF
          ELSE
           IF TYPE(vVar)=="C"
            &vVar:=TRANSFORM(SUBSTR(ALLTRIM(&vVar)+' '+;
                   ALLTRIM(STR(ROUND(IIF(nSavTotal#0.AND.entry=0,nSavTotal,entry),Tot_Dec),Max_Int,Tot_Dec))+;
                   SPACE(LenStr),1,LenStr),vMascara)
           ENDIF
          ENDIF
         ENDIF
         RESTSCREEN(_FIL1,_COL1,_FIL2,_COL2,Pan_Fon)
         SETCOLOR(Col_Ant)

         SET KEY -9 to CALCULATOR
         SET KEY 247 TO CALCULATOR
         SET DECIMAL TO Dec_Ant
         SETCURSOR(Cur_Ant)
         RETURN

    OTHERWISE
          TONE(2000,1)
  ENDCASE
  enter=.F.
  IF inkey=13
    enter=.T.
    fcode='T'
  ENDIF
ENDDO

STATIC FUNCTION AddNum(Code,fcode)         // Process + or - keypress
  lTotalOk  := .F.
  lDecSet   := .F.
  Can_Dec   := 0
  Can_Int   := 0
  lSubRtn   := .F.

  *Producto por un Porcentaje
  IF fcode = '*' .AND. code = '%'
   entry = entry/100
   Act_Papel(Papel,STR(ROUND(entry,Tot_Dec),Tot_Int,Tot_dec)+' '+fcode)
   code='@'
   KEYBOARD CHR(13)
  ENDIF

  IF code='*' .OR. code='/'
    // They pressed the * or / key to process the previous total
    IF Round(entry,Tot_Dec)==0 .AND. Round(nTotal,Tot_Dec)==0
      entry := nSavTotal
    ENDIF
    // Get the first number of the product or division
    IF Round(nTotal,Tot_Dec)==0

     *Multiplicacion
     IF code == '*'                 // Setup mode
       nAddMode := 3
       Act_Papel(Papel,STR(ROUND(entry,Tot_Dec),Tot_Int,Tot_dec)+' '+fcode)
     ENDIF

     *Division
     IF code == '/'
       nAddMode := 4
       Act_Papel(Papel,STR(ROUND(entry,Tot_Dec),Tot_Int,Tot_dec)+' '+fcode)
     ENDIF
     nTotal    := entry
     entry     := 0

    ELSE

     *Producto
     IF code == '*'
       nAddMode := 3
       Act_Papel(Papel,STR(ROUND(entry,Tot_Dec),Tot_Int,Tot_dec)+' '+fcode)
       nTotal    := nTotal*entry
       entry := 0
     ENDIF

     *Division
     IF code == '/'                // Divide
      nAddMode := 4
      Act_Papel(Papel,STR(ROUND(entry,Tot_Dec),Tot_Int,Tot_dec)+' '+fcode)
      nTotal:=Divide(nTotal,entry)
      IF lDivideErr
        ALERT("Lo siento, pero usted no puede DIVIDIR por CERO!")
        lDivideErr := .F.
      ENDIF
      entry := 0
     ENDIF
    ENDIF
  ELSE
    // They pressed the + or - key to process the previous total
    IF Round(entry,Tot_Dec)==0 .AND. Round(nTotal,Tot_Dec)==0
      entry := nSavTotal
      lNewNum := .T.
    ENDIF

    *Suma
    IF code == '+'                      // Add
      nAddMode := 1
      IF !lNewNum                       // They pressed + again to add the same
        entry := nSavTotal              // number without re-entering
      ENDIF
      Act_Papel(Papel,STR(ROUND(entry,Tot_Dec),Tot_Int,Tot_dec)+' '+code)
      nTotal := nTotal+entry
      lNewNum := .F.
      nSavTotal := entry            // Save this number in case they just press + or -
      entry := 0
    ENDIF

    *Resta
    IF code == '-'
     nAddMode := 2
     IF !lNewNum                      // Apretar el Signo - para restar el mismo
       entry := nSavTotal             // numero sin re-entrar ninguno
       lNewNum := .T.
     ENDIF
     Act_Papel(Papel,STR(ROUND(entry,Tot_Dec),Tot_Int,Tot_dec)+' '+code)
     IF ROUND(nTotal,Tot_Dec)==0 .AND. !prm
      nTotal:=entry
      prm=.T.
     ELSE
      nTotal    := nTotal-entry
     ENDIF
     lNewNum := .F.
     nSavTotal := entry         // Save this number in case they just press + or -
     entry:= 0
    ENDIF

    *Porcentaje
    IF code=='%' .OR. code=='\'
     *Sumar o Restar Porcentaje
     entry=nTotal*.01*(entry)
     Act_Papel(Papel,STR(ROUND(entry,Tot_Dec),Tot_Int,Tot_dec)+' '+fcode)
    ENDIF

    *Raiz Cuadrada
    IF code=='R'
     nTotal=SQRT(entry)
     Act_Papel(Papel,STR(ROUND(entry,Tot_Dec),Tot_Int,Tot_dec)+' '+code)
     entry:=0
     KEYBOARD CHR(13)
    ENDIF
  ENDIF
  Cad1=STR(ROUND(nTotal,Tot_dec),Max_Int,Tot_dec)
RETURN NIL

STATIC FUNCTION AddTotal             // Enter key - SUBTOTAL\TOTAL
  lDecSet := .F.
  Can_dec := 0
  Can_Int := 0
    IF Round(nTotal,Tot_Dec)!=0 .OR. Round(entry,Tot_Dec)!=0
      IF Round(entry,Tot_Dec)!=0 .AND. fcode # '%'
        Act_Papel(Papel,STR(ROUND(entry,Tot_Dec),Tot_Int,Tot_dec)+' '+fcode)
      ENDIF
      IF nAddMode == 1                  // Add
        nTotal := nTotal+entry
      ELSEIF nAddMode == 2              // Subtract
        nTotal := nTotal-entry
      ELSEIF nAddMode == 3              // Multiply
        nTotal := nTotal*entry
      ELSEIF nAddMode == 4              // Divide
        nTotal := Divide(nTotal,entry)
        IF lDivideErr
          ALERT("Lo siento, pero Usted no puede DIVIDIR por CERO!")
          lDivideErr := .F.
        ENDIF
      ENDIF
    ENDIF
RETURN NIL

STATIC FUNCTION Divide(nNumerator,nDenominator)  // Check divide by zero not allowed
  IF nDenominator==0.0
    lDivideErr := .T.
    RETU 0
  ELSE
    lDivideErr := .F.
  ENDIF
RETURN(nNumerator/nDenominator)

STATIC FUNCTION Act_Pan()
  *Imprime Ingresos
  @ _F1+2,_COL1+2 SAY fcode
  @ _F1+14,_COL1+9 SAY PADL(STR(CalcMem,Tot_Int,Tot_Dec),Max_Int,' ')
  @ _F1+2,_COL1+9 SAY PADL(Cad1,Max_Int,' ')
  @ _F1+2,_COL1+9+(Max_Int-1) SAY ''
 RETURN NIL

STATIC FUNCTION Act_Papel(Papel,Contenido)
 AADD(Papel,Contenido)
 Papel[ LEN(Papel) ]=PADL(Papel[ LEN(Papel) ],Max_Int+2,' ')
 RESTSCREEN(_FIL1,_COL1,_FIL2,_COL2,Pan_Fon)
 IF _FIL1-1 > 1 .AND. Resta
  _FIL1 = _FIL1 - 1
 ELSE
  Resta=.F.
 ENDIF
 Pan_Fon = SAVESCREEN(_FIL1,_COL1,_FIL2,_COL2)
 RESTSCREEN(IIF(Resta,_FIL1+1,_FIL1),_COL1,_FIL2,_COL2,Pan_Cal)
 SCROLL(_FIL1+IIF(Resta,0,1),_COL1,_F1,_COL2,1)
 @_F1,_COL1 SAY "      ³  "+Papel[ LEN(Papel) ]+"³"
 Pan_Cal = SAVESCREEN(_FIL1,_COL1,_FIL2,_COL2)
 RETURN NIL

STATIC FUNCTION ChangeDec(nNumDec)   // Change the decimal position in the
  LOCAL y                               // display
  IF nNumDec == NIL
    nNumDec := 0
    nNumDec := Question('aca va un mensaje',nNumDec,;
                        '9',{|oGet| ValDeci(oGet)},.T.)
  ENDIF
  vMascara := PosRepl(cDefTotPict,'.',15-ABS(nNumDec))
  FOR y=14-ABS(nNumDec) TO 2 STEP -4
    vMascara := PosRepl(vMascara,',',y)
  NEXT
  Tot_Dec := nNumDec
RETURN NIL

STATIC FUNCTION ValDeci(oGet)
  IF oGet:VarGet()>8
    ALERT('no ingrese mas de 8 lugares decimales, por FAVOR!')
    RETU .F.
  ENDIF
RETURN .T.

/* Question(cMessage,xVarVal,cPict,bValid,lNoESC,nWinColor,nTop,cHelp) ;
*           -->xVarVal
*
*    Push a Question Box on the screen and get the answer with a local
*    variable, and return their answer
*
*    cMessage  -> Message printed above variable that describes explains
*                    what they are getting
*    xVarVal   -> Initial value of the variable Data types C,N,L,D
*    cPict     -> Picture for GET                              - Optional
*    bValid    -> Valid Block                                  - Optional
*    lNoESC    -> When .T. they cannot <ESC>, default .F.      - Optional
*    nWinColor -> Window color, default next window color      - Optional
*    nTop      -> Top row of window, default Center of screen  - Optional
*    cHelp     -> If passed pushes the specific help variable to help stack
*                 If Not passed pushes the variable name 'NOQuHelp'  - Opt.
*/

STATIC FUNCTION Question(cMessage,xVarVal,cPict,bValid,lNoESC,nWinColor,nTop)

  LOCAL nOldRow, nOldCol, cOldColor, nMessLen, nWide, nNumRows, nBottom, nLeft
  LOCAL nRight, oNewGet, nNumMessRow, nLenLastRow, lGetOnNextLine, nOldCurs
  LOCAL cVarType := VALTYPE(xVarVal)
  LOCAL nVarLen  := IF(cVarType='C',LEN(xVarVal),IF(cVarType='D',8, ;
                       IF(cVarType='L',1,IF(cVarType='N',IF(cPict=NIL,9, ;
                       LEN(cPict)),0))))
  LOCAL nOldLastKey := LASTKEY()
  MEMVAR GETLIST

  nOldRow   := ROW()
  nOldCol   := COL()
  nOldCurs  := SETCURSOR(0)
  cOldColor := SETCOLOR()
  lNoESC    := IF(lNoESC==NIL,.F.,lNoESC)

  nMessLen  := LEN(cMessage)+nVarLen+1
  nWide     := IF(nMessLen>66,66,IF(nMessLen<12,12,nMessLen))

  nNumMessRow    := MLCOUNT(cMessage,nWide)
  nLenLastRow    := LEN(TRIM(MEMOLINE(cMessage,nWide,nNumMessRow)))
  lGetOnNextLine := (nLenLastRow + nVarLen) > nWide
  nNumRows       := nNumMessRow + IF(lGetOnNextLine,1,0)

  nTop        := IF(nTop=NIL,INT((MAXROW() - nNumRows)/2),nTop)  // Center it in the screen
  nBottom     := nTop+nNumRows+1
  nLeft       := INT((MAXCOL()-nWide)/2)-4
  nRight      := nLeft+nWide+4

  @_F1+2,_COL1+1 SAY "Cuantos Decimales:      "

  oNewGet := GetNew( IF(lGetOnNextLine,Row()+1,Row()), ;
                     IF(lGetOnNextLine,nLeft+2,Col()+1), ;
                     {|x| IF(PCOUNT() > 0, xVarVal := x, xVarVal)}, ;
                     'xVarVal' )

  // If the input line is character & wider than window SCROLL
  IF lGetOnNextLine .AND. VALTYPE(xVarVal)='C' .AND. nVarLen>nWide
    oNewGet:Picture   := '@S'+LTRIM(STR(nWide,4,0))+IF(cPict=NIL,'',' '+cPict)
  ENDIF

  IF cPict != NIL                       // Use the picture they passed
    oNewGet:Picture   := cPict
  ELSE                                  // Else setup default pictures
    IF VALTYPE(xVarVal)='D'
      oNewGet:Picture   := '99/99/99'
    ELSEIF VALTYPE(xVarVal)='L'
      oNewGet:Picture   := 'Y'
    ELSEIF VALTYPE(xVarVal)='N'
      oNewGet:Picture   := '999999.99'  // Guess that they are inputting dollars
    ENDIF
  ENDIF

  oNewGet:PostBlock := IF(bValid=NIL,NIL,bValid)

  oNewGet:Display()

  RingBell()

  DO WHILE .T.                          // Loop so we can check for <ESC>
                                        // without reissuing the gets
    ReadModal({oNewGet})
    IF LASTKEY() == 27 .AND. lNoESC  // They pressed <ESC>
      ALERT('usted No puede ABORTAR! entre una respuesta por Favor.')
    ELSE
      EXIT
    ENDIF
  ENDDO

  SETCURSOR(nOldCurs)
  SETCOLOR(cOldColor)
  SETPOS(nOldRow,nOldCol)
RETURN xVarVal

STATIC FUNCTION PosRepl(cString,cChar,posit)  // Replace a Character in a
RETURN(STRTRAN(cString,'9',cChar,posit,1)+'')    // String

STATIC FUNCTION CharRem(cChar,cString)  // Removes character from string
RETURN(STRTRAN(cString,cChar))

STATIC FUNCTION RingBell(lMustRing)  // I can turn off the bell!
  lMustRing := IF(lMustRing == NIL, .F., lMustRing)
  IF lMustRing          &&.OR. lStatMustRing
    ?? CHR(7)
  ENDIF
RETURN NIL

FUNCTION AYUDA
LOCAL pan,ant_color
pan=SAVESCREEN(1,1,23,40)
ant_color=SETCOLOR()
SETCOLOR(colo12)
@ 1,1 CLEAR TO 23,39
@ 1,1 TO 23,39
@ 1,10 SAY "´ Ayuda Calculadora Ã"
@ 2,2 SAY "Del   = Borra D¡gito"
@ 3,2 SAY "AC    = Borrar Todo"
@ 4,2 say "MC    = Borrar Memoria"
@ 5,2 say "MR    = Leer Memoria"
@ 6,2 say "M+    = Sumar Total a Memoria"
@ 7,2 say "M-    = Restar Total a Memoria"
@ 8,2 say "M*    = Multiplica por Memoria"
@ 9,2 say "M/    = Divide por Memoria"
@10,2 say "C"+CHR(17)+"ÄÙ"+"  = Borrar Total"
@11,2 say "CE"+CHR(17)+"ÄÙ"+" = Borrar Entrada"
@12,2 say CHR(17)+"ÄÙ"+" para <=>"
@13,2 say "Fn    = Establece Decimales [ 0-9 ]"
@14,2 say " /    = Dividir"
@15,2 say " *    = Producto (Ej.con %: 100*5%=5)"
@16,2 say " -    = Restar"
@17,2 say " +    = Sumar"
@18,2 say " R    = Raiz Cuadrada"
@19,2 say " %    = Porcentaje (100*5%=5)"
@20,2 SAY "               Ej. 100+21%(Suma 21%)"
@21,2 SAY "Mover =  Izq.    Der."
@22,2 SAY "Papel =  Arr.    Aba.(Esc-Salir)"
INKEY(30)
RESTSCREEN(1,1,23,40,PAN)
SETCOLOR(ant_color)
RETURN []

FUNCTION ARCH_EXI(fNomb,fExte)
 *Generar Nombre archivo Temporal
 PRIVATE Nro_Arch,fTemp
 FOR Nro_Arch = 1 TO 99
  fTemp = fNomb + CERO(Nro_Arch,2)+"."+fExte
  IF !FILE("&fTemp")
   EXIT
  ENDIF
 NEXT
 RETURN fTemp

FUNCTION CON_COM
PARAMETERS MOD,sub,pos
PRIVATE tcl
SET KEY -4 TO
tcl=LASTKEY()
DO CASE
 CASE MOD=0
  RETURN(2)
 CASE MOD=1 .OR. MOD=2
  TONO(2)
  RETURN(2)
 CASE MOD=3
  DO CASE
   CASE tcl=13
    RETURN(0)
   CASE tcl=-4
    XD=-1
    RETURN(0)
   CASE tcl=27          &&Presiona [ESC] para terminar
    RETURN(0)
   CASE tcl>=48 .and. tcl<=57
    KEYBOARD CHR(13)+CHR(tcl)
    RETURN(2)
   OTHERWISE
    RETURN(2)
  ENDCASE
ENDCASE

FUNCTION EXI_DIR( cDrvDir )
  // Chequea si fue Pasado el "\" ; Agrega si es Necesario
RETURN FILE(( cDrvDir + IF(SUBS( cDrvDir, -1, 1) == "\", "", "\" ) + "NUL" ) )

FUNCTION EXI_ARC( cDrvDir,F_Arch)
  // Chequea si fue Pasado el "\" ; Agrega si es Necesario
RETURN FILE(( cDrvDir + IF(SUBS( cDrvDir, -1, 1) == "\", "", "\" ) + "NUL" +F_Arch) )

*Sirve para obtener la Inversa de una Cadena
*Devuelve un valor de Tipo Caracter
FUNCTION STRINV(f_Cad)
PRIVATE f_Long,f_Ind
f_Long=LEN(f_Cad)
f_Cad =SUBSTR(f_Cad,2,f_Long-1) + SUBSTR(f_Cad,1,1)
FOR f_Ind = f_Long TO 3 STEP -1
 f_Cad = SUBSTR(f_Cad,2,f_Ind-2) + SUBSTR(f_Cad,1,1) + ;
         SUBSTR(f_Cad,f_Ind,f_Long-f_Ind+1)
NEXT
RETURN (f_Cad)

FUNCTION NOMB_A
PARAMETERS Nom_Resu
PRIVATE Panti,Col_ant
Panti=SAVESCREEN(,,,,)
COl_Ant=SETCOLOR()
*Ingresar Nombre Archivo Destino
Nom_Resu=SPACE(8)
SOMBRA(10,10,12,50,"H1",1,'',colo3)
DO WHILE .T.
 @11,44 SAY ".DBF"
 @11,11 SAY "Nombre Archivo Destino: " GET Nom_Resu PICTURE "@!"
 READ
 IF LASTKEY()=27
  RESTSCREEN(0,0,24,79,panti)
  SETCOLOR(col_ant)
  RETURN .F.
 ENDIF
 Nom_Resu=ALLTRIM(Nom_Resu)
 IF FILE(Rut_Lis+"&Nom_Resu..DBF")
  Bien=CARTEL("ARCHIVO YA EXISTE [ SOBREESCRIBE ] [ ABORTA ]",16,1,'N',color3)
  IF LASTKEY()=27
   RESTSCREEN(0,0,24,79,pantI)
   SETCOLOR(col_ant)
   RETURN .F.
  ENDIF
  IF !Bien
   LOOP
  ENDIF
  EXIT
 ELSE
  EXIT
 ENDIF
ENDDO
Nom_Resu=Rut_Lis + Nom_Resu+".DBF"
RESTSCREEN(0,0,24,79,pantI)
SETCOLOR(col_ant)
RETURN .T.

*Pasa un Vto.de un sab/dom al Viernes Anterior
FUNCTION VIERNES(ffech)
PRIVATE fdias
fdias=DOW(ffech)
IF fdias=7
 ffech=ffech-1
ELSE
 IF fdias=1
  ffech=ffech-2
 ENDIF
ENDIF
RETURN ffech

FUNCTION AMPLIA(cad1,cad2)
IF PCOUNT()=1
 cad2=SPACE(1)
ENDIF
PRIVATE pos,cad3
cad3 = SPACE(0)
FOR pos = 1 TO LEN(cad1)
 cad3 = cad3 + SUBSTR(cad1,pos,1) + cad2
NEXT
cad1 = cad3
RETURN cad1

FUNCTION SUB_ON
RETURN CHR(27)+CHR(45)+CHR(1)

FUNCTION SUB_OFF
RETURN CHR(27)+CHR(45)+CHR(0)

*============================================================================
* Ejemplo:            Clipper 5.02 - linkeditar con fiscal.lib
* Valido para:        Impresoras fiscales HASAR
* Modelos:            SMH/P-715F v1.00
* Consultar:          publtick.pdf - drivers.pdf
*
*GENERADOR DE FACTURAS A o B
*============================================================================
FUNCTION P715_FA(Com,fDiscri)
 PUBLIC Handler, Port, n, Se
 * Separador de campos en los comandos a generar
 * ---------------------------------------------
 Se = CHR(28)

 * Apertura del puerto serie
 * -------------------------
 n = VAL(Com)
 IF ( n < 0 .OR. n > 4 )
  RETURN
 ENDIF
 Handler = OpenPort(Com)
 * Sincronizar velocidad de la comunicacion serial. No es necesario llamar
 * a esta funcion toda vez que se arranque el programa.
 * Una vez establecida la velocidad de trabajo, utilizar la funcion
 * SetBaud para fijar la velocidad en la PC.
 * NOTA: otros modelos trabajan unicamente a 9600 baudios
 * -----------------------------------------------------------------------
 BaudRate = SearchPr(Handler)
 @0,1 SAY "Controlador Fiscal detectado a " + alltrim(str(BaudRate))

 IF (baudrate < 1)
    bandera:= "ERROR1"
 ELSE
  * Sincronizarse con la impresora fiscal
  * -------------------------------------
  InitFiscal(Handler)

  * Si hay un documento abierto se lo cancela. Si no se pudo cancelar,
  * se intenta cerrarlo.
  * ------------------------------------------------------------------
  CancelDoc()
  CloseFiscal()
  * Se carga en la memoria de trabajo de la impresora fiscal el texto de
  * las lineas de fantasia, encabezamiento y pie del documento.
  * --------------------------------------------------------------------
  CargarTexto()

  * Emision del Documento
      * ---------------------
  ImprimirFA(fDiscri)

  * Cierre del Puerto serie
  * -----------------------
  ClosePort(Handler)
 ENDIF
RETURN .T.

*============================================================================
* Ejemplo:            Clipper 5.02 - linkeditar con fiscal.lib
* Valido para:        Impresoras fiscales HASAR
* Modelos:            SMH/P-715F v1.00
* Consultar:          publtick.pdf - drivers.pdf
*
*GENERADOR DE NOTAS DE CREDITO
*============================================================================
FUNCTION P715_NC(Com)
 PUBLIC Handler, Port, n, Se
 Se = CHR(28)
 n = VAL(Com)
 IF ( n < 0 .OR. n > 4 )
  RETURN
 ENDIF
 Handler = OpenPort(Com)
 BaudRate = SearchPr(Handler)
 @0,1 SAY "Controlador Fiscal detectado a " + alltrim(str(BaudRate))
 IF (baudrate < 1)
    bandera:= "ERROR1"
 ELSE
  InitFiscal(Handler)
  CancelDoc()
  CloseFiscal()
  CargarTexto()
  ImprimirNC()
  ClosePort(Handler)
 ENDIF
RETURN .T.

*============================================================================
* Ejemplo:            Clipper 5.02 - linkeditar con fiscal.lib
* Valido para:        Impresoras fiscales HASAR
* Modelos:            SMH/P-715F v1.00
* Consultar:          publtick.pdf - drivers.pdf
*
*GENERADOR DE NOTAS DE DEBITOS
*============================================================================
FUNCTION P715_ND(Com)
 PUBLIC Handler, Port, n, Se
 Se = CHR(28)
 n = VAL(Com)
 IF ( n < 0 .OR. n > 4 )
  RETURN
 ENDIF
 Handler = OpenPort(Com)
 BaudRate = SearchPr(Handler)
 @0,1 SAY "Controlador Fiscal detectado a " + alltrim(str(BaudRate))
 IF (baudrate < 1)
    bandera:= "ERROR1"
 ELSE
  InitFiscal(Handler)
  CancelDoc()
  CloseFiscal()
  CargarTexto()
  ImprimirND()
  ClosePort(Handler)
 ENDIF
RETURN .T.

*----------------------------------------------------------------------------
* Rutina que intenta cancelar un Documento abierto.
* Genera comando: Cancel
*----------------------------------------------------------------------------
FUNCTION CancelDoc
 s = CHR(152)
 Enviar(s)
RETURN 0

*----------------------------------------------------------------------------
* Rutina que intenta cerrar un Documento Fiscal abierto.
* Genera comando: CloseFiscalReceipt
*----------------------------------------------------------------------------
FUNCTION CloseFiscal
 s = "E"
 Enviar(s)
RETURN 0

*----------------------------------------------------------------------------
* Rutina que carga en la memoria de trabajo de la impresora fiscal las lineas
* de fantasia, encabezado y pie del documento.
* Los textos fijos cargarlos en el arranque del programa. Los textos varia-
* cargarlos al emitir el nuevo documento.
*----------------------------------------------------------------------------
FUNCTION CargarTexto
 * Carga lineas de fantasia
 * Genera comando: SetfantayName
 * -----------------------------
 FOR i=1 TO 2
  linea = ALLTRIM(STR(i))
  linea="0"
* s = "]" + Se + linea + Se + "Walter " + linea + " Fantasia..."
  s = "]" + Se + linea + Se + "Walter " + linea + " PRUEBA nOMBRE"
  Enviar(s)
 NEXT

 * Carga lineas de encabezado
 * Genera comando: SetHeaderTrailer
 * --------------------------------
 FOR i=1 TO 10
  linea = ALLTRIM(STR(i))
  linea="0"
 * s = "]" + Se + linea + Se + "David " + linea + " Header..."
  s = "]" + Se + linea + Se + "David walter martin"
  Enviar(s)
 NEXT

 * Carga lineas de pie
 * Genera comando: SetHeaderTrailer
 * --------------------------------
 FOR i=11 TO 20
  linea = ALLTRIM(STR(i))
  linea="0"
  s = "]" + Se + linea + Se + "Martin" + linea + " Trailer..."
  Enviar(s)
 NEXT
RETURN 0

*----------------------------------------------------------------------------
* Rutina de impresion del Documento.
* El Documento a emitir dependera del comando de apertura. El comando de
* cierre debera ser coherente al de apertura.
* El cuerpo es valido cualquiera sea el Documento a emitir ( Fiscal o Ticket
* Nota de Credito ).
*
*ESTE MODULO ES PARA FACTURAS
*
*----------------------------------------------------------------------------
FUNCTION ImprimirFA(fDiscri)

  * Se cargan en la memoria de trabajo de la impresora fiscal, los datos
  * del comprador ( opcional en Documentos Fiscales, obligatorio en Notas
  * de Credito ).
  * Genera comando: SetCustomerData
  * ---------------------------------------------------------------------
  s = "b" + Se + vNom + Se + vCui + Se + bit48 + Se + bit50 + Se + vDom
  Enviar(s)

  * Se carga en la memoria de trabajo de la impresora fiscal al relacion
  * del documento a emitir con un Remito o una factura, segun corresponda
  * ( opcional en Documentos Fiscales, obligatorio en Notas de Credito )
  * Genera comando: SetEmbarkNumber
  * ---------------------------------------------------------------------
  *Habilitar para cuando se utiliza un remito para el despacho
  IF vRem#"00000000"
    s = CHR(147) + Se + "1" + Se + vSrie+"-"+vRem
    Enviar(s)
  ENDIF

  * Comando de apertura del Documento
  * ---------------------------------
  IF vCiv="1" .OR. vCiv="2"
    * Ticket Factura "A"
    * Genera comando: OpenFiscalReceipt
    s = "@" + Se + "A" + Se + "T"
  ELSE
    * Ticket Factura "B"
    * Genera comando: OpenFiscalReceipt
    s = "@" + Se + "B" + Se + "T"
  ENDIF

  * Ticket Nota de Credito "A"
  * Genera comando:  OpenDNFH
  * s = CHR(128) + Se + "R" + Se + "T"
  Enviar(s)

  * Impresion Texto Fiscal - solamente previo al item
  * Genera comando: PrintFiscalText
  * -------------------------------------------------
  *s = "A" + Se + "Texto Fiscal..." + Se + "0"
  *Enviar(s)

  * Impresion de item
  * Genera comando: PrintLineitem
  * -----------------------------
  IF fDiscri
   FOR con = 1 TO LEN(MItems)
    IF !EMPTY(SUBSTR(MItems[con],143,5))
     vPuni = VAL(SUBSTR(MItems[con],87,9))
     IF vCiv="1" .OR. vCiv="2"
      xPuni = StrZero(vPuni+((vPuni*iva1)/100),9,3)
      s = "B" + Se + SUBSTR(MItems[con],1,20) +;
                Se + ALLTRIM(SUBSTR(MItems[con],79,7)) +;
                Se + xPuni +;
                Se + StrZero(iva1,5,2) +;
                Se + "M" +;
                Se + "0.0" +;
                Se + "0" +;
                Se + "T"
     ELSE
      s = "B" + Se + SUBSTR(MItems[con],1,20) +;
                Se + ALLTRIM(SUBSTR(MItems[con],79,7)) +;
                Se + StrZero(vPuni,9,3) +;
                Se + StrZero(iva1,5,2) +;
                Se + "M" +;
                Se + "0.0" +;
                Se + "0" +;
                Se + "T"
     ENDIF
     Enviar(s)
     *Lo sisguiente se utilizara si el item tiene Dsto individual
     * Descuento sobre ultima venta
     * Genera comando: LastItemDiscount
     * --------------------------------
     *IF dsto # 0
     *  s = "U" + Se + "Descuento: " + Se + StrZero(dsto,5,2) + Se + "m" + Se + "0" + Se + "T"
     *  Enviar(s)
     *ENDIF
    ENDIF
   NEXT
  ELSE
   xPuni = 0
   FOR con = 1 TO LEN(MItems)
    IF !EMPTY(SUBSTR(MItems[con],143,5))
     vPuni = VAL(SUBSTR(MItems[con],87,9))
     xCant = VAL(SUBSTR(MItems[con],79,7))
     IF vCiv="1" .OR. vCiv="2"
      xPuni = xPuni + ((vPuni+((vPuni*iva1)/100))*xCant)
     ELSE
      xPuni = xPuni + (vPuni*xCant)
     ENDIF
    ENDIF
   NEXT
   *Armo Linea para reemplazar detalle de articulos
     IF vCiv="1" .OR. vCiv="2"
      s = "B" + Se + "Su Compra " +;
                Se + "1.0" +;
                Se + StrZero(xPuni,9,3) +;
                Se + StrZero(iva1,5,2) +;
                Se + "M" +;
                Se + "0.0" +;
                Se + "0" +;
                Se + "T"
     ELSE
      s = "B" + Se + "Su Compra " +;
                Se + "1.0" +;
                Se + StrZero(xPuni,9,3) +;
                Se + StrZero(iva1,5,2) +;
                Se + "M" +;
                Se + "0.0" +;
                Se + "0" +;
                Se + "T"
    ENDIF
    Enviar(s)
  ENDIF

  * Descuento General
  * Genera comando: GeneralDiscount
  * --------------------------------
  IF dsto # 0
    s = "T" + Se + "%" + StrZero(dsto,5,2) + Se + StrZero(tdes,9,2) + Se + "m" + Se + "0" + Se + "B"
    Enviar(s)
  ENDIF

  * Bonificacion a una alicuota de IVA
  * Genera comando: ReturnRecharge
  * ----------------------------------
  *s = "m" + Se + "Bonif Iva21..." + Se + "1.0" + Se + "21.00" + Se + "m" + Se + "0.0" + Se + "0" + Se + "T" + Se + "B"
  *Enviar(s)

  * Recargo General
  * Genera comando: GeneralDiscount
  * -------------------------------
  *s = "T" + Se + "Financiero..." + Se + "10.0" + Se + "M" + Se + "0" + Se + "T"
  *Enviar(s)

  * Percepciones a aplicar
  * Genera comando: Perception
  * --------------------------
  *s = "`" + Se + "21.0" + Se + "Percep IVA 21..." + Se + "5.00"
  *Enviar(s)
  *s = "`" + Se + "**.**" + Se + "Percep Gral..." + Se + "5.00"
  *Enviar(s)

  * Impresion del pago
  * Genera comando: TotalTender
  * ---------------------------
  IF Conta
    s = "D" + Se + "Pago..." + Se + StrZero(tgral,12,2) + Se + "T" + Se + "0"
    Enviar(s)
  ENDIF

  * Cierre del Documento
  * --------------------

  * Documento Fiscal
  * Genera comando: CloseFiscalReceipt
  * ----------------------------------
  s = "E"

  Enviar(s)
  * Nota de Credito
  * Genera comando: CloseDNFH
  * -------------------------
  * s = CHR(129)

  Enviar(s)
RETURN 0

*----------------------------------------------------------------------------
* Rutina de impresion del Documento.
* El Documento a emitir dependera del comando de apertura. El comando de
* cierre debera ser coherente al de apertura.
* El cuerpo es valido cualquiera sea el Documento a emitir ( Fiscal o Ticket
* Nota de Credito ).
*
*ESTE MODULO ES PARA NOTAS DE CREDITO
*
*----------------------------------------------------------------------------
FUNCTION ImprimirNC
  * Genera comando: SetCustomerData
  s = "b" + Se + vNom + Se + vCui + Se + bit48 + Se + bit50 + Se + vDom
  Enviar(s)

  *Informar Nro Cbte Factura a la que afectara la NCredito
  s:= chr(147) + se + "1" + se + f_trans(Transform(VAL(fact_nc), "######"))
  Enviar(s)

  * Comando de apertura del Documento
  * ---------------------------------
  * Genera comando:  OpenDNFH
  IF vCiv="1" .OR. vCiv="2"
    * Nota de Credito "A"
    s = CHR(128) + Se + "R" + Se + "S" + Se + ""
  ELSE
    * Nota de Credito "B"
    * Genera comando: OpenFiscalReceipt
    s = CHR(128) + Se + "S" + Se + "S" + Se + ""
  ENDIF
  Enviar(s)

  * Impresion Texto Fiscal - solamente previo al item
  * Genera comando: PrintFiscalText
  * -------------------------------------------------
  s = "A" + Se + SUBSTR(MItems[1],1,30) + Se + "0"
  Enviar(s)

  ltx:= "S/FC " + ltra_nc + " " + srie_nc + "-" + fact_nc
  s:= "B" + se + ltx + se + "1.0" + se + StrZero(tgral, 9, 2) + se + ;
     StrZero(iva1, 5, 2) + se + "M" + se + "0.0" + se + "0" + se + "T"
  Enviar(s)

  * Cierre del Documento
  * --------------------
  * Nota de Credito
  * Genera comando: CloseDNFH
  * -------------------------
  s = CHR(129)

  Enviar(s)
RETURN 0

*----------------------------------------------------------------------------
*
*ESTE MODULO ES PARA NOTAS DE DEBITO
*
*----------------------------------------------------------------------------
FUNCTION ImprimirND
  * Genera comando: SetCustomerData
  s = "b" + Se + vNom + Se + vCui + Se + bit48 + Se + bit50 + Se + vDom
  Enviar(s)

  * Comando de apertura del Documento
  * ---------------------------------
  * Genera comando: OpenFiscalReceipt
  IF vCiv="1" .OR. vCiv="2"
    * Nota de Debito "A"
    s = "@" + Se + "D" + Se + "T"
  ELSE
    * Nota de Debito "B"
    s = "@" + Se + "E" + Se + "T"
  ENDIF
  Enviar(s)

  * Impresion Texto Fiscal - solamente previo al item
  * Genera comando: PrintFiscalText
  * -------------------------------------------------
  s = "A" + Se + SUBSTR(MItems[1],1,30) + Se + "0"
  Enviar(s)

  ltx:= "Importe ND: "
  IF vCiv="1" .OR. vCiv="2"
    s:= "B" + se + ltx + se + "1.0" + se + StrZero(vtta, 9, 2) + se + ;
        StrZero(iva1, 5, 2) + se + "M" + se + "0.0" + se + "0" + se + "T"
  ELSE
    s:= "B" + se + ltx + se + "1.0" + se + StrZero(vtta, 9, 2) + se + ;
        StrZero(iva1, 5, 2) + se + "M" + se + "0.0" + se + "0" + se + "T"
  ENDIF
  Enviar(s)

  * Descuento General
  * Genera comando: GeneralDiscount
  * --------------------------------
  IF dsto # 0
    s = "T" + Se + "%" + StrZero(dsto,5,2) + Se + StrZero(tdes,9,2) + Se + "m" + Se + "0" + Se + "B"
    Enviar(s)
  ENDIF

  * Impresion del pago
  * Genera comando: TotalTender
  * ---------------------------
  IF Conta
   s:= "D" + se + "Su Pago: " + se + StrZero(tgral, 9, 2) + se + "T" + se + "0"
   Enviar(s)
  ENDIF

  * Cierre del Documento
  * --------------------
  * Nota de Debito
  * Genera comando: CloseFiscalReceipt
  * -------------------------
  s = CHR(69)
  Enviar(s)
RETURN 0

*----------------------------------------------------------------------------
* Rutina que emite el Cierre Diario "X" o "Z".
* Genera el comando: DailyClose
*----------------------------------------------------------------------------
FUNCTION ReporteXZ

        * Cierre X - Reporte por cambio de turno
        * --------------------------------------
        Enviar("9" + Se + "X")

        * Cierre Z - Acumulados de la jornada
        * -----------------------------------
        * Enviar("9" + Se + "Z")

return 0

*----------------------------------------------------------------------------
* Rutina para enviar un comando a la impresora fiscal y atrapar la respuesta.
* La inteligencia para el tratamiento de errores es responsabilidad del de-
* sarrollador de software.
*----------------------------------------------------------------------------
FUNCTION Enviar
PARAMETERS String
PRIVATE Result, StatPrn

* Si la funcion MandaPaq() retorna un valor menor a cero, se retorna indi-
* cando Error.
* Consultar: drivers.pdf para el tratamiento de estos errores
* ------------------------------------------------------------------------
StatPrn = MandaPaq(Handler, String)

IF (statprn < 0)
   DO WHILE (statprn = -9)
      CARTEL("Impresora Ocupada [OK]",12,1,'E',color3)
      string:= "¡"
      statprn:= MandaPaq(handler, string)
      IF (LASTKEY() == K_ESC)
         CARTEL("Fin de la consulta intermedia [OK]",16,1,'N',color3)
         RETURN -9
      ENDIF
      INKEY(1)
   ENDDO
   RETURN -1
ENDIF

* Se obtiene la respuesta de la impresora fiscal
* ----------------------------------------------
Result = Respuesta(Handler)
   n_strin:= Left(string, 1)

   *Busca una E ¢ una  en el string enviado
   *Si es una E sera una Factura
   *Si es una  sera una Nota de Credito
   if (n_strin = CHR(69) .OR. n_strin = chr(129))
      nu_fac:= VAL(RIGHT(Result, 8))
   ENDIF
   GETERRORS( result )

* Tratamiento de la respuesta recibida
* ------------------------------------
GetErrors(Result)
RETURN 0

*----------------------------------------------------------------------------
* Rutina que a partir de la respuesta de la impresora fiscal muestra mensajes
* asociados al status de impresora y al status fiscal, de corresponder.
*----------------------------------------------------------------------------
FUNCTION GetErrors
PARAMETERS Resp
PRIVATE Origen, OffsetSep, i, c

DECLARE FiscalErrors [16]
DECLARE PrinterErrors[16]

FiscalErrors[1] =       "Error en chequeo de memoria fiscal"
FiscalErrors[2] =       "Error en chequeo de la memoria de trabajo"
FiscalErrors[3] =       "Carga de bateria baja"
FiscalErrors[4] =       "Comando desconocido"
FiscalErrors[5] =       "Datos no validos en un campo"
FiscalErrors[6] =       "Comando no valido para el estado fiscal actual"
FiscalErrors[7] =       "Desborde del total"
FiscalErrors[8] =       "Memoria fiscal llena"
FiscalErrors[9] =       "Memoria fiscal a punto de llenarse"
FiscalErrors[10] =      ""
FiscalErrors[11] =      ""
FiscalErrors[12] =      "Error en ingreso de fecha"
FiscalErrors[13] =      "Documento fiscal abierto"
FiscalErrors[14] =      "Documento abierto"
FiscalErrors[15] =      "Factura abierta"
FiscalErrors[16] =      ""

PrinterErrors[1]  = ""
PrinterErrors[2]  = ""
PrinterErrors[3]  = "Error de Impresora"
PrinterErrors[4]  = "Impresora Offline"
PrinterErrors[5]  = "Falta papel del diario"
PrinterErrors[6]  = "Falta papel de tickets"
PrinterErrors[7]  = "Buffer de Impresora lleno"
PrinterErrors[8]  = ""
PrinterErrors[9]  = ""
PrinterErrors[10] = ""
PrinterErrors[11] = ""
PrinterErrors[12] = ""
PrinterErrors[13] = ""
PrinterErrors[14] = ""
PrinterErrors[15] = ""
PrinterErrors[16] = ""

Origen = 0
OffsetSep = AT( CHR(28), Resp )

* Convierte en hexa el campo de status de impresora
* -------------------------------------------------
PrinterStatus = HexaToInt(SUBSTR ( Resp, Origen, OffsetSep - 1))

IF PrinterStatus < 0
        RETURN -1
ENDIF

Origen = OffsetSep

* Analiza los bits comenzando del menos significativo
* ---------------------------------------------------
FOR i = 1 TO 16
        IF ( INT(PrinterStatus % 2) == 1 )
                IF ( LEN(PrinterErrors[i]) > 0 )
                    *    ? "Status Impresora: " + PrinterErrors[i]
                        * Agregar la inteligencia de su tratamiento
                ENDIF
        ENDIF
        PrinterStatus = PrinterStatus / 2
NEXT

OffsetSep = AT( CHR(28), SUBSTR(Resp, Origen + 1) )

IF OffsetSep == 0
        OffsetSep = LEN(Resp)
ENDIF

* Convierte en hexa el status fiscal
* ----------------------------------
FiscalStatus = HexaToInt(SUBSTR(Resp, Origen + 1, OffsetSep - 1))

IF FiscalStatus < 0
        RETURN -1
ENDIF

* Analiza los bits comenzando del menos significativo
* ---------------------------------------------------
FOR i = 1 TO 16
        IF ( INT(FiscalStatus % 2) == 1 )
                IF ( LEN(FiscalErrors[i]) > 0 )
                   *     ? "Status Fiscal: " + FiscalErrors[i]
                        * Agregar la inteligencia de su tratamiento
                ENDIF
        ENDIF
        FiscalStatus = FiscalStatus / 2
NEXT

RETURN 0

*----------------------------------------------------------------------------
* Rutina que convierte un string de digitos hexadecimales en su equivalente
* en binario.
*----------------------------------------------------------------------------
FUNCTION HexaToInt
PARAMETERS HexValue
PRIVATE i, Value, Status

Status = 0

FOR i = 4 TO 1 STEP -1
        S = SUBSTR(HexValue, i, 1)
        Value = ASC(S)

        IF ( Value >= ASC("A") .AND. Value <= ASC("F") )
                Value = Value - ASC("A") + 10
        ELSEIF ( Value >= ASC("a") .AND. Value <= ASC("f") )
                Value = Value - ASC("a") + 10
        ELSEIF ( Value >= ASC("0") .AND. Value <= ASC("9") )
                Value = Value - ASC("0")
        ELSE
                *? "HexaToInt: Digito hexadecimal incorrecto: " + HexValue
                RETURN -1
        ENDIF

        Status = Status + Value * (16 ** ( 4 - i ))

NEXT

RETURN Status

function F_TRANS
   PARAMETERS n
   m:= StrTran(n, ".", "*")
   m:= StrTran(m, ",", ".")
   m:= StrTran(m, "*", ",")
   RETURN m

*********************************************
* para trabajar en red con registrador fiscal
* opcion SPOOLER
*  SPOOLER -p1 -l -s d:\archivo1 -a d:\respues1
*********************************************

*----------------------------------------------------------------------------
FUNCTION SpoolerFA(fDiscri)
   SET PRINTER TO "&carpfis.\&tempofi3."
   SET CONSOLE OFF
   SET DEVICE TO PRINTER
   SET PRINTER ON
   se:= CHR(28)
   s:= "D" + se + " " + se + "0.00" + se + "C" + se + "0"
   ? s
   s:= "E"
   ? s

  * Se cargan en la memoria de trabajo de la impresora fiscal, los datos
  * del comprador ( opcional en Documentos Fiscales, obligatorio en Notas
  * de Credito ).
  * Genera comando: SetCustomerData
  * ---------------------------------------------------------------------
  s = "b" + Se + vNom + Se + vCui + Se + bit48 + Se + bit50 + Se + vDom
  ? s

  * Se carga en la memoria de trabajo de la impresora fiscal al relacion
  * del documento a emitir con un Remito o una factura, segun corresponda
  * ( opcional en Documentos Fiscales, obligatorio en Notas de Credito )
  * Genera comando: SetEmbarkNumber
  * ---------------------------------------------------------------------
  *Habilitar para cuando se utiliza un remito para el despacho
  IF vRem#"00000000"
    s = CHR(147) + Se + "1" + Se + vSrie+"-"+vRem
    ? s
  ENDIF

  * Comando de apertura del Documento
  * ---------------------------------
  IF vCiv="1" .OR. vCiv="2"
    * Ticket Factura "A"
    * Genera comando: OpenFiscalReceipt
    s = "@" + Se + "A" + Se + "T"
  ELSE
    * Ticket Factura "B"
    * Genera comando: OpenFiscalReceipt
    s = "@" + Se + "B" + Se + "T"
  ENDIF

  * Ticket Nota de Credito "A"
  * Genera comando:  OpenDNFH
  * s = CHR(128) + Se + "R" + Se + "T"
  ? s

  * Impresion Texto Fiscal - solamente previo al item
  * Genera comando: PrintFiscalText
  * -------------------------------------------------
  *s = "A" + Se + "Texto Fiscal..." + Se + "0"
  *? s

  * Impresion de item
  * Genera comando: PrintLineitem
  * -----------------------------
  IF fDiscri
   FOR con = 1 TO LEN(MItems)
    IF !EMPTY(SUBSTR(MItems[con],143,5))
     vPuni = VAL(SUBSTR(MItems[con],87,9))
     IF vCiv="1" .OR. vCiv="2"
      xPuni = StrZero(vPuni+((vPuni*iva1)/100),9,3)
      s = "B" + Se + SUBSTR(MItems[con],1,20) +;
                Se + ALLTRIM(SUBSTR(MItems[con],79,7)) +;
                Se + xPuni +;
                Se + StrZero(iva1,5,2) +;
                Se + "M" +;
                Se + "0.0" +;
                Se + "0" +;
                Se + "T"
     ELSE
      s = "B" + Se + SUBSTR(MItems[con],1,20) +;
                Se + ALLTRIM(SUBSTR(MItems[con],79,7)) +;
                Se + StrZero(vPuni,9,3) +;
                Se + StrZero(iva1,5,2) +;
                Se + "M" +;
                Se + "0.0" +;
                Se + "0" +;
                Se + "T"
     ENDIF
     ? s
     *Lo sisguiente se utilizara si el item tiene Dsto individual
     * Descuento sobre ultima venta
     * Genera comando: LastItemDiscount
     * --------------------------------
     *IF dsto # 0
     *  s = "U" + Se + "Descuento: " + Se + StrZero(dsto,5,2) + Se + "m" + Se + "0" + Se + "T"
     *  ? s
     *ENDIF
    ENDIF
   NEXT
  ELSE
   xPuni = 0
   FOR con = 1 TO LEN(MItems)
    IF !EMPTY(SUBSTR(MItems[con],143,5))
     vPuni = VAL(SUBSTR(MItems[con],87,9))
     xCant = VAL(SUBSTR(MItems[con],79,7))
     IF vCiv="1" .OR. vCiv="2"
      xPuni = xPuni + ((vPuni+((vPuni*iva1)/100))*xCant)
     ELSE
      xPuni = xPuni + (vPuni*xCant)
     ENDIF
    ENDIF
   NEXT
   *Armo Linea para reemplazar detalle de articulos
     IF vCiv="1" .OR. vCiv="2"
      s = "B" + Se + "Su Compra " +;
                Se + "1.0" +;
                Se + StrZero(xPuni,9,3) +;
                Se + StrZero(iva1,5,2) +;
                Se + "M" +;
                Se + "0.0" +;
                Se + "0" +;
                Se + "T"
     ELSE
      s = "B" + Se + "Su Compra " +;
                Se + "1.0" +;
                Se + StrZero(xPuni,9,3) +;
                Se + StrZero(iva1,5,2) +;
                Se + "M" +;
                Se + "0.0" +;
                Se + "0" +;
                Se + "T"
    ENDIF
    ? s
  ENDIF

  * Descuento General
  * Genera comando: GeneralDiscount
  * --------------------------------
  IF dsto # 0
    s = "T" + Se + "%" + StrZero(dsto,5,2) + Se + StrZero(tdes,9,2) + Se + "m" + Se + "0" + Se + "B"
    ? s
  ENDIF

  * Bonificacion a una alicuota de IVA
  * Genera comando: ReturnRecharge
  * ----------------------------------
  *s = "m" + Se + "Bonif Iva21..." + Se + "1.0" + Se + "21.00" + Se + "m" + Se + "0.0" + Se + "0" + Se + "T" + Se + "B"
  *? s

  * Recargo General
  * Genera comando: GeneralDiscount
  * -------------------------------
  *s = "T" + Se + "Financiero..." + Se + "10.0" + Se + "M" + Se + "0" + Se + "T"
  *? s

  * Percepciones a aplicar
  * Genera comando: Perception
  * --------------------------
  *s = "`" + Se + "21.0" + Se + "Percep IVA 21..." + Se + "5.00"
  *? s
  *s = "`" + Se + "**.**" + Se + "Percep Gral..." + Se + "5.00"
  *? s

  * Impresion del pago
  * Genera comando: TotalTender
  * ---------------------------
  IF Conta
    s = "D" + Se + "Pago..." + Se + StrZero(tgral,12,2) + Se + "T" + Se + "0"
    ? s
  ELSE
    s = "D" + Se + "Pago..." + Se + StrZero(0,12,2) + Se + "T" + Se + "0"
    ? s
  ENDIF

  * Cierre del Documento
  * --------------------

  * Documento Fiscal
  * Genera comando: CloseFiscalReceipt
  * ----------------------------------
  s = "E"  &&CHR(69)
  ? s
  s = "™"  &&CHR(153)
  ? s
  SET PRINTER OFF
  SET DEVICE TO SCREEN
  SET CONSOLE ON
  SET PRINTER TO
RETURN 0

FUNCTION SpoolerNC
  SET PRINTER TO "&carpfis.\&tempofi3."
  SET CONSOLE OFF
  SET DEVICE TO PRINTER
  SET PRINTER ON
  se:= CHR(28)
  s:= "D" + se + " " + se + "0.00" + se + "C" + se + "0"
  ? s
  s:= "E"
  ? s
  * Genera comando: SetCustomerData
  s = "b" + Se + vNom + Se + vCui + Se + bit48 + Se + bit50 + Se + vDom
  ? s

  s:= chr(147) + se + "1" + se + f_trans(Transform(VAL(fact_nc), "######"))
  ? s

  * Comando de apertura del Documento
  * ---------------------------------
  * Genera comando:  OpenDNFH
  IF vCiv="1" .OR. vCiv="2"
    * Nota de Credito "A"
    s = CHR(128) + Se + "R" + Se + "S"  && + Se + ""
  ELSE
    * Nota de Credito "B"
    * Genera comando: OpenFiscalReceipt
    s = CHR(128) + Se + "S" + Se + "S" + Se + ""
  ENDIF
  ? s

  * Impresion Texto Fiscal - solamente previo al item
  * Genera comando: PrintFiscalText
  * -------------------------------------------------
  s = "A" + Se + SUBSTR(MItems[1],1,30) + Se + "0"
  ? s

  ltx:= "S/FC " + ltra_nc + " " + srie_nc + "-" + f_trans(Transform(VAL(fact_nc), "######"))
  s:= "B" + se + ltx + se + "1.0" + se + StrZero(tgral, 9, 2) + se + ;
     StrZero(iva1, 5, 2) + se + "M" + se + "0.0" + se + "0" + se + "T"
  ? s

   s:= "D" + se + "Su Pago: " + se + StrZero(0, 9, 2) + se + "T" + se + "0"
   ? s

  * Cierre del Documento
  * --------------------
  * Nota de Credito
  * Genera comando: CloseDNFH
  * -------------------------
  s = CHR(129)
  ? s
  SET PRINTER OFF
  SET DEVICE TO SCREEN
  SET CONSOLE ON
  SET PRINTER TO
  RUN COPY R:\ARCHIVO1\COMP36.TXT R:\
RETURN 0

FUNCTION SpoolerND
  SET PRINTER TO "&carpfis.\&tempofi3."
  SET CONSOLE OFF
  SET DEVICE TO PRINTER
  SET PRINTER ON
  se:= CHR(28)
  s:= "D" + se + " " + se + "0.00" + se + "C" + se + "0"
  ? s
  s:= "E"
  ? s
  * Genera comando: SetCustomerData
  s = "b" + Se + vNom + Se + vCui + Se + bit48 + Se + bit50 + Se + vDom
  ? s

  * Comando de apertura del Documento
  * ---------------------------------
  * Genera comando: OpenFiscalReceipt
  IF vCiv="1" .OR. vCiv="2"
    * Nota de Debito "A"
    s = "@" + Se + "D" + Se + "T"
  ELSE
    * Nota de Debito "B"
    s = "@" + Se + "E" + Se + "T"
  ENDIF
  ? s


  * Impresion Texto Fiscal - solamente previo al item
  * Genera comando: PrintFiscalText
  * -------------------------------------------------
  s = "A" + Se + SUBSTR(MItems[1],1,30) + Se + "0"
  ? s

  ltx:= "Importe ND: "
  IF vCiv="1" .OR. vCiv="2"
    s:= "B" + se + ltx + se + "1.0" + se + StrZero(vtta, 9, 2) + se + ;
        StrZero(iva1, 5, 2) + se + "M" + se + "0.0" + se + "0" + se + "T"
  ELSE
    s:= "B" + se + ltx + se + "1.0" + se + StrZero(vtta, 9, 2) + se + ;
        StrZero(iva1, 5, 2) + se + "M" + se + "0.0" + se + "0" + se + "T"
  ENDIF
  ? s

  * Descuento General
  * Genera comando: GeneralDiscount
  * --------------------------------
  IF dsto # 0
    s = "T" + Se + "%" + StrZero(dsto,5,2) + Se + StrZero(tdes,9,2) + Se + "m" + Se + "0" + Se + "B"
    ? s
  ENDIF

  * Impresion del pago
  * Genera comando: TotalTender
  * ---------------------------
  IF Conta
   s:= "D" + se + "Su Pago: " + se + StrZero(tgral, 9, 2) + se + "T" + se + "0"
   ? s
  ELSE
   s:= "D" + se + "Su Pago: " + se + StrZero(0, 9, 2) + se + "T" + se + "0"
   ? s
  ENDIF

  * Cierre del Documento
  * --------------------
  * Nota de Debito
  * Genera comando: CloseFiscalReceipt
  * -------------------------
  s = CHR(69)
  ? s
  SET PRINTER OFF
  SET DEVICE TO SCREEN
  SET CONSOLE ON
  SET PRINTER TO
RETURN 0

FUNCTION F_VALICUI(nro)
***********************
LOCAL XCampo,XCoef,Ret,Suma,X

IF EMPTY(nro)
 RETURN .T.
ELSE
 IF LEN(nro) = 13
    Xcampo = LEFT(nro,2) + SUBSTR(nro,4,8) + RIGHT(nro,1)
 ELSE
    Xcampo = nro
 ENDIF

 Xcoef  = "5432765432"
 Ret    = .T.
 Suma   = 0
 FOR X = 1 to 10
     Suma = Suma + VAL(SUBSTR(XCampo,X,1)) * VAL(SUBSTR(XCoef,X,1))
 NEXT
 Resto = INT(suma % 11)
 Digito_Ver = VAL(RIGHT(Xcampo,1))
 IF Resto > 1
    Resto = 11 - Resto
 ENDIF
 IF Resto <> Digito_Ver
    RETURN .F.
 ELSE
    RETURN .T.
 ENDIF
ENDIF
