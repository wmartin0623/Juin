*Consulta de pagos de clientes
PRIVATE pan_ant,col_ant
pan_ant=SAVESCREEN(0,0,24,79)
col_ant=SETCOLOR()
*clientes
AREA()
OPEN_DBF("MAECLIEN","CLIENTES","C","MAECLIEN","clie","NOMCLIEN","nomb")

*CUENTA CORRIENTE
AREA()
IF USAR('CCCLIENT','CTACTE')
  IF !FILE('CCCLIENT.NTX')
   INDEX ON clie+tcom+ltra+srie+nume TO CCCLIENT
  ENDIF
  IF !FILE('CCCLIE01.NTX')
   INDEX ON clie+DTOS(fech) TO CCCLIE01
  ENDIF
  IF !FILE('CCCLIE04.NTX')
   INDEX ON tcom+ltra+srie+nume TO CCCLIE04
  ENDIF
  SET INDEX TO CCCLIENT,CCCLIE01,CCCLIE04
ELSE
 QUIT
ENDIF
SET RELATION TO clie INTO CLIENTES

*SET CENTURY OFF
SET DECIMAL TO 2
PUBLIC vCli,vnom,vdom,vciv,vCuit:=SPACE(13)
PRIVATE vPcia,vDpto,vFech
PRIVATE vnum,ok,vPago
PRIVATE com,rel,campos[30]
PUBLIC cbte[1],dia[1],vTot_Cbt,vTot_Pag
ok=0
DO WHILE .T.
 SETCOLOR(color7)
 @1,0 CLEAR TO 24,79
 DO MOVCPACL
 SETCOLOR(color1)
 XPM=3
 XP=1
 SET KEY -4 TO VA_TRAS
 DO WHILE XP<=XPM
  XD=1
  SET ESCAPE ON
  SET CURSOR ON
  DO CASE
   CASE XP=1
    STORE 0 TO vtot_cbt,vtot_pag
    vnum=SPACE(8)
    AFILL(campos,'         ³          ³    ³    ³          ³          ³             '+SPACE(9)+SPACE(3)+SPACE(9))
    PUBLIC cbte[1],dia[1]
    @24,3 SAY '[Esc] - Salir '
    sale=0
    @ 2,68 GET vnum PICTURE '@K 99999999' VALID VAL_PAG(@vnum)
    READ
   CASE XP=2
    @12,60 SAY vtot_cbt PICTURE "@Z 999999999.99" COLOR color9
    @22,61 SAY vtot_pag PICTURE "@Z 999999999.99" COLOR color9
    SET CURSOR OFF
    com=1
    rel=0
    KEYBOARD CHR(24)+CHR(5)
    DO WHILE .T.
     com=ACHOICE(6,1,10,71,cbte,.T.,"CON_COM1",com,rel)
     SET KEY -4 TO VA_TRAS
     IF LASTKEY()=-4
      XD=-1
      EXIT
     ENDIF
     IF com=0
      EXIT
     ENDIF
    ENDDO
   CASE XP=3
    SET CURSOR OFF
    com=1
    rel=0
    KEYBOARD CHR(24)+CHR(5)
    DO WHILE .T.
     com=ACHOICE(15,7,20,72,campos,.T.,"CON_COM1",com,rel)
     SET KEY -4 TO VA_TRAS
     IF LASTKEY()=-4
      XD=-1
      EXIT
     ENDIF
     IF com=0
      EXIT
     ENDIF
    ENDDO
  ENDCASE
  IF LASTKEY()=27 .AND. OK=0
   CLEAR GETS
   XP=6
  ELSE
   XP=XP+XD
  ENDIF
 ENDDO
 SETCOLOR(color3)
 scr1=SAVESCREEN(14,10,19,25)
 SOMBRA(14,11,18,23,'H2',1,'',color3)
 opc1=1
 IF XP#6
  @ 15,12 PROMPT ' O T R A   '
  @ 16,12 PROMPT ' IMPRIMIR  '
 ENDIF
 @ 17,12 PROMPT ' CANCELAR  '
 MENU TO opc1
 SETCOLOR(color1)
 RESTSCREEN(14,10,19,25,scr1)
 IF XP=6
  Opc1=3
 ENDIF
 DO CASE
  CASE opc1=1
   SET CURSOR OFF
   LOOP
  CASE opc1=2
   *Impresion en papel
   vMess="IMPRIME ORDEN DE PAGO ? "
   DO WHILE .T.
    IF DUDA(vMess)
     SET CONSOLE OFF
     DO WHILE .NOT.ISPRINTER()
      CARTEL("LA IMPRESORA NO ESTA PREPARADA...",12,1,'E','*'+color3)
      INKEY(.1)
      TONE(700,1)
     ENDDO
     DO LIS_DATO5
     vMess="IMPRIME OTRA COPIA ? "
    ELSE
     EXIT
    ENDIF
   ENDDO

  CASE opc1=3 .OR. opc1=0
  IF CARTEL('CONFIRMA CANCELACION [SI] [NO]',22,1,'N',color3)
    SET CURSOR OFF
    IF LASTKEY()=27
     LOOP
    ENDIF
    CLOSE ALL
    SET ESCAPE ON
    RESTSCREEN(0,0,24,79,PAN_ANT)
    SETCOLOR(COL_ANT)
    SET CENTURY ON
    RETURN
   ELSE
    SET CURSOR OFF
    LOOP
   ENDIF
 ENDCASE
ENDDO
CLOSE ALL
SET ESCAPE ON
RESTSCREEN(0,0,24,79,pan_ant)
SETCOLOR(col_ant)
SET CENTURY ON
RETURN

FUNCTION CON_COM1
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

*Control de Numero de pagos
FUNCTION VAL_PAG
PARAMETERS num
PRIVATE pan_ant,col_ant,antsel,wfec,p,vimp,tip,wltra,wsrie,wnum,reg,wperi,hor
IF LASTKEY()=27
 KEYBOARD CHR(27)
 RETURN .T.
ENDIF
AFILL(campos,'         ³          ³    ³    ³          ³          ³             '+SPACE(9)+SPACE(3)+SPACE(9))
PUBLIC cbte[1],dia[1]
col_ant=SETCOLOR()
antsel=ALLTRIM(STR(SELECT()))
SELECT CTACTE
vnum=CERO(VAL(num),8)
SET ORDER TO 3
SEEK 'PA'+SPACE(5)+vnum
IF !FOUND()
 *Ayuda
ELSE
 wfec=DTOC(fpag)
 wperi=myan
ENDIF
p=0
DO WHILE nume=vnum .AND. tcom='PA'
 IF p>0
  ASIZE(cbte,p+1)
  ASIZE(dia,p+1)
 ENDIF
 p=p+1
 vCli=clie
 vimp=paga
 tip=comt
 wltra=lcom
 wsrie=scom
 wnum=ncom
 reg=RECNO()
 SEEK tip+wltra+wsrie+wnum
 IF FOUND()
  cbte[LEN(cbte)]="      ³                       ³  ³             ³          ³            "
  cbte[LEN(cbte)]=STUFF(cbte[LEN(cbte)],1,6,clie)
  cbte[LEN(cbte)]=STUFF(cbte[LEN(cbte)],8,23,SUBSTR(CLIENTES->nomb,1,23))
  vCli=clie
  vNom=CLIENTES->nomb
  vDpto=CLIENTES->dpto
  vPcia=CLIENTES->pcia
  vDom =CLIENTES->domi
  vCuit=CLIENTES->cuit
  vCuit=LEFT(vcuit,2)+"-"+SUBSTR(vcuit,3,8)+"-"+RIGHT(vcuit,1)
  cbte[LEN(cbte)]=STUFF(cbte[LEN(cbte)],32,2,tcom)
  cbte[LEN(cbte)]=STUFF(cbte[LEN(cbte)],35,13,ltra+srie+nume)
  cbte[LEN(cbte)]=STUFF(cbte[LEN(cbte)],49,10,DTOC(fech))
  cbte[LEN(cbte)]=STUFF(cbte[LEN(cbte)],60,12,TRANSFORM(vimp,"999999999.99"))
  dia[LEN(dia)]=BERRE(DTOC(fech))
  vtot_cbt=vtot_cbt+vImp
  GO reg
 ENDIF
 SKIP
ENDDO
IF p=0
 CARTEL(" NO ENCUENTRO N§ COMPROBANTE  [ OK ]",20,1,'N',color3)
 SELECT &antsel
 RETURN .F.
ENDIF
*Ordena por fecha
DECLARE cam[1]
cam[1]="cbte"
ORDEN(dia,cam)
*Extraigo del archivo de pagos del mes de pago el detalle de pago
*PAGOS
AREA()
IF USAR("PCL&wperi","PAGOS")
 IF !FILE('PCL&wperi..NTX')
  INDEX ON nume+nint TO PCL&wperi
 ENDIF
 SET INDEX TO PCL&wperi
ELSE
 SELECT &antsel
 RETURN .F.
ENDIF
p=0
*Bancos
AREA()
OPEN_DBF("MAEBANCO","BANCO","C","MAEBANCO","banc","NOMBANCO","nomb")

*Plazas
AREA()
OPEN_DBF("MAEPLAZA","PLAZA","C","MAEPLAZA","plaz","NOMPLAZA","nomb")
SELECT PAGOS
SEEK CERO(VAL(vnum),8)
IF FOUND()
 DO WHILE nume=vnum
  p=p+1
  vBanc=banc
  vPlaz=plaz
  vPago=nint
  campos[p]=STUFF(campos[p],1,8,nint)
  campos[p]=STUFF(campos[p],11,10,DTOC(fech))
  campos[p]=STUFF(campos[p],23,2,fpag)
  campos[p]=STUFF(campos[p],28,2,banc)
  campos[p]=STUFF(campos[p],76,3,plaz)
  campos[p]=STUFF(campos[p],33,8,ntal)
  campos[p]=STUFF(campos[p],43,10,DTOC(fvto))
  campos[p]=STUFF(campos[p],55,12,TRANSFORM(impo,"999999999.99"))
  vFech=fech
  vtot_pag=vtot_pag+impo
  SELECT BANCO
  SEEK vBanc
  IF FOUND()
   campos[p]=STUFF(campos[p],67,9,sigl)
  ELSE
   campos[p]=STUFF(campos[p],67,9,"...      ")
  ENDIF
  SELECT PLAZA
  SEEK vPlaz
  IF FOUND()
   campos[p]=STUFF(campos[p],79,9,sigl)
  ELSE
   campos[p]=STUFF(campos[p],79,9,"...      ")
  ENDIF
  SELECT PAGOS
  SKIP
 ENDDO
 hor=15
 FOR con=1 TO LEN(campos)
  IF !EMPTY(campos[con])
   @hor,7 SAY SUBSTR(campos[con],1,66)
   hor=hor+1
   IF hor>20
    con=LEN(campos)
   ENDIF
  ENDIF
 NEXT
ENDIF
SELECT PAGOS
CLOSE
SELECT BANCO
CLOSE
SELECT PLAZA
CLOSE
IF p=0
 CARTEL(" NO ENCUENTRO DETALLES DE PAGO  [ OK ]",20,1,'N',color3)
ENDIF
SELECT &antsel
RETURN .T.

PROCEDURE LIS_DATO5
PRIVATE AntSel
AntSel=ALLTRIM(STR(SELECT()))
*Leo Nombre de Provincia y Departamento
*Departamentos
AREA()
OPEN_DBF("MAEDEPAR","DPTOS","C","MAEDEPAR","dpto","NOMDEPAR","nomb")
SELECT DPTOS
SEEK vdpto
IF FOUND()
 nom_dpto=nomb
ELSE
 nom_dpto=SPACE(15)
ENDIF
CLOSE
*Provincias
AREA()
OPEN_DBF("MAEPROVI","PCIAS","C","MAEPROVI","pcia","NOMPROVI","nomb")
SELECT PCIAS
SEEK vpcia
IF FOUND()
 nom_pcia=nomb
ELSE
 nom_pcia=SPACE(15)
ENDIF
CLOSE
SELECT &AntSel
vdia=LEFT(DTOC(vFech),2)
mes=MESES(vFech)
ano=CERO(YEAR(vFech),4)
hor=0
pagina=0
SET CONSOLE OFF
SET PRINT ON
?? CHR(27)+CHR(120)+CHR(0)+CHR(18)
?? CHR(27)+CHR(67)+CHR(36)
?? CHR(27)+CHR(33)+CHR(5)
*Numero de Pago y Fecha
NORMA()
? SPACE(50)+"I-"+vPago+SPACE(4)+"O-"+vNum
? SPACE(75)+vdia+' '+mes+' '+ano
?
?
*Datos del Cliente
? SPACE(5)+vNom+SPACE(38)+vCli
? SPACE(7)+vdom+SPACE(2)+nom_dpto+SPACE(2)+nom_pcia
? SPACE(5)+vCuit
?
*Importe en letras
CHICO()
vlet= UPPER(LETTER1(INT(ABS(vTot_Pag))))+' con/'+RIGHT(STR(ABS(vTot_Pag),12,2),2)
IF LEN(vlet) > 100
 vle1 = SUBSTR(vlet,1,100)
 FOR con = 1 TO LEN(vle1)
  IF RIGHT(vle1,1) # ' '
   vle1 = SUBSTR(vle1,1,LEN(vle1)-1)
  ELSE
   con = LEN(vle1)
  ENDIF
 NEXT
 vle2 = SUBSTR(vlet,LEN(vle1)+1)
ELSE
 vle1 = vlet
 vle2 = ''
ENDIF
? SPACE(40) + vle1
? SPACE( 3) + vle2
?
?
vsub=0
*Contar longitud maxima de Array de Cbtes.(xx) y De Formas de Pago (yy)
xx=LEN(Cbte)
yy=0
FOR con= 1 TO LEN(campos)
 IF !EMPTY(SUBSTR(campos[con],1,2))
  yy++
 ENDIF
NEXT
vSub_Cb=0   &&Totaliza Cbtes. Pagados
vSub_Pa=0   &&Totaliza Formas de Pago
*Obtener Mayor Longitud para el Bucle de Impresion
IF xx > 17  &&Cbtes. que entran en una boleta
 xx=1
 xRes=.T.
 FOR con=1 TO LEN(Cbte)
  IF !EMPTY(SUBSTR(Cbte[con],32,2))
   vSub_Cb=vSub_Cb+VAL(SUBSTR(Cbte[con],60,12))
  ENDIF
 NEXT
ELSE
 xRes=.F.
ENDIF
zz=MAX(xx,yy)
Hor=0
FOR con=1 TO zz
 *Obtiene Cbte.
 IF LEN(Cbte)>=con
  IF !xRes
   IF !EMPTY(SUBSTR(Cbte[con],32,2))
    vLin=SUBSTR(Cbte[con],49,10)+SPACE(3)+;
         SUBSTR(Cbte[con],32, 2)+' '+;
         SUBSTR(Cbte[con],35,13)+SPACE(13)+;
         SUBSTR(Cbte[con],60,12)
    *Obtengo Subtotales
    vSub_Cb=vSub_Cb+VAL(SUBSTR(Cbte[con],60,12))
   ELSE
    vLin=SPACE(54)
   ENDIF
  ELSE
   *Resumen por Muchos Cbtes.Pagados
   vLin="Segun Resumen"+SPACE(29)+STR(vSub_Cb,12,2)
  ENDIF
 ELSE
  vLin=SPACE(54)
 ENDIF
 *Obtiene Forma de Pago
 IF LEN(Campos)>=Con
  IF !EMPTY(SUBSTR(campos[con],23,2))
   vLin=vLin+SPACE(6)+SUBSTR(Campos[con],23,2)+SPACE(4)
   IF SUBSTR(Campos[con],23,2) # "CH"
    vLin=vLin+SPACE(30)+SPACE(37)
   ELSE
    vLin=vLin+SUBSTR(campos[con],67,9)+SPACE(6)+SUBSTR(campos[con],79,9)+SPACE(3)
    vLin=vLin+SUBSTR(campos[con],43,10)+SPACE(10)+SUBSTR(campos[con],33,8)+SPACE(12)
   ENDIF
   vLin=vLin+SUBSTR(campos[con],55,12)
   *Obtengo Subtotales
   vSub_Pa=vSub_Pa+VAL(SUBSTR(campos[con],55,12))
  ELSE
   vLin=vLin+SPACE(89)
  ENDIF
 ELSE
  vLin=vLin+SPACE(89)
 ENDIF
 *Imprimo Linea
 IF Hor=15
  *Imprimo Total de Formas de Pago
  vLin=STUFF(vLin,134,12,STR(vSub_Pa,12,2))
 ENDIF
 ? vLin
 *Suma Lineas Impresas
 Hor++
 vLin=""
NEXT
IF Hor<17
 FOR con=Hor TO 17
  IF con=15
   *Imprimo Total de Formas de Pago
   ? SPACE(134)+STR(vSub_Pa,12,2)
  ELSE
   ?
  ENDIF
 NEXT
ENDIF
*Imprimo Total de Cbtes. Pagados
?
? SPACE(40)+STR(vSub_Cb,12,2)
?
?? CHR(27)+CHR(33)+CHR(8)
EJECT
SET PRINT OFF
SET CONSOLE ON
RETURN
