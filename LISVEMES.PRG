*Revision mensual de ventas
PRIVATE pant_ant,col_ant,bmes,bano,vai,vani
pant_ant=SAVESCREEN(0,0,24,79)
col_ant=SETCOLOR()
@1,0 CLEAR TO 24,79
bmes=SPACE(2)
bano=SPACE(4)
*Porcentaje de iva
AREA()
IF USAR('PORCEIVA')
 vai=IVAI
 vani=IVANI
ELSE
 QUIT
ENDIF
DO WHILE .T.
 IF SELECT("FACTURA")#0
  SELECT FACTURA
  CLOSE
 ENDIF
 *Solicitud de MES Y A¤o del Listado de Ventas
 CARTEL('MES A TRATAR   <mm> : [  ]',9,1,'E',colo3)
 CARTEL('A¥0 A TRATAR <aaaa> : [    ]',14,1,'E',colo3)
 SETCOLOR(colo3)
 @10,51 GET bmes PICTURE '@K 99' VALID VAL(bmes)>=1 .AND. VAL(bmes)<=12
 @15,50 GET bano PICTURE '@K 9999' VALID VAL(bano)>=1994 .AND. VAL(bano)<=2021
 READ
 IF LASTKEY()=27
  RESTSCREEN(0,0,24,79,pant_ant)
  SETCOLOR(col_ant)
  EXIT
 ENDIF
 xBmes = CHR(VAL(bmes)+64)
 bmes=CERO(VAL(bmes),2)
 bano=CERO(VAL(bano),4)
 vper=xbmes+bano
 IF .NOT.FILE("FCC&vper..DBF")
  CARTEL("PERIODO SIN REGISTRACION DE VENTAS  [ OK ]",18,1,'N',color3)
  LOOP
 ENDIF
 *FACT
 AREA()
 IF USAR("FCC&vper","FACTURA")
  wn = vper+'1'
  IF !FILE('FC&wn..NTX')
   INDEX ON DTOS(fech) TO FC&wn
  ENDIF
  SET INDEX TO FC&wn
 ELSE
  QUIT
 ENDIF
 *Solicitud del dia comienzo Al final de la Venta
 DO WHILE .T.
  SELECT FACTURA
  SETCOLOR(colo3)
  SOMBRA(11,30,14,46,'H1',1,'',colo3)
  @12,31 SAY 'Desde D¡a : '
  @13,31 SAY 'Hasta D¡a : '
  ult_dia=ULTIMO(VAL(bmes),VAL(bano))
  STORE '  ' TO dia_des,dia_has
  @12,43 GET dia_des PICTURE '@K 99' VALID VAL(dia_des)>=1.AND.VAL(dia_des)<=ult_dia
  @13,43 GET dia_has PICTURE '@K 99' VALID VAL(dia_has)>=VAL(dia_des).AND.VAL(dia_has)<=ult_dia
  READ
  IF LASTKEY()=27
   CLOSE ALL
   RESTSCREEN(0,0,24,79,pant_ant)
   SETCOLOR(col_ant)
   EXIT
  ENDIF
  dia_des=CERO(VAL(dia_des),2)
  dia_has=CERO(VAL(dia_has),2)
  fe_des=CTOD(dia_des+'/'+bmes+'/'+bano)
  fe_has=CTOD(dia_has+'/'+bmes+'/'+bano)
  SET FILTER TO fech>=fe_des .AND. fech<=fe_has .AND. !baja
  GO TOP
  COUNT TO pipi
  IF pipi=0
   CARTEL("PERIODO SIN REGISTRACION DE VENTAS  [ OK ]",18,1,'N',color3)
   SET FILTER TO
   LOOP
  ENDIF
  o1=0
  IF .NOT.IMPRI()
   o1=1
  ENDIF
  IF LASTKEY()#27
   IF CONFIR()
    ok=0
    DO venta
    IF ok=1
     EXIT
    ENDIF
   ENDIF
  ENDIF
 ENDDO
ENDDO
CLOSE ALL
RETURN

PROCEDURE venta
STORE 0 TO ng,iva1,iva2,apagar
SELECT FACTURA
GO TOP
STORE 0 TO total1,total2,total3,total4,total5,total6,total7
DO WHILE !EOF()
 tc=tipo
 ci=civa
 i1=prin
 i2=prni
 ng=ngra
 ip=0    &&iper
 im=0    &&imul
 *Acumulador de NETOS GRAVADOS impresos en libro de IVA
 total1=total1+IIF(tc='1'.OR.tc='3'.OR.tc='4',IIF(ci='4',0,IIF(ci='3'.OR.(ci>='5'),;
        ng/((i1+100)/100),ng)),IIF(tc='2',IIF(ci='4',0,IIF(ci='3'.OR.(ci>='5'),;
        (ng/((i1+100)/100)),ng)),0))
 *Acumulador de EXENTO impresos en libro de IVA
* total4=total4+IIF(ci='4',IIF(tc='1'.OR.tc='3'.OR.tc='4',ng,IIF(tc='2',ng,0)),0)
 total4=total4+IIF(ci='4',ng,0)
 *Acumulador de IVA INSCRIPTO impresos en libro de IVA
 total2=total2+IIF(ci='4',0,IIF(tc='1'.OR.tc='3'.OR.tc='4',IIF(ci='3'.OR.(ci>='5'),;
        ((ng/((i1+100)/100))*i1)/100,irin),IIF(tc='2',IIF(ci='3'.OR.(ci>='5'),;
        ((ng/((i1+100)/100))*i1)/100,irin),0)))
 *Acumulador de IVA NO INSCRIPTO impresos en libro de IVA
 total3=total3+IIF(ci='2',IIF(tc='2',irni,irni),0)
 ***total a pagar
 total5=total5+IIF(ci='3',(paga/((i1+100)/100)),paga)
***Acumulador de Importe Ing.Bruto Percepcion
 total6=total6 + ip
***Acumulador de Importe MUlta Cedula fiscal
 total7=total7 + im
 SKIP
ENDDO
ng=total1
iva1=total2
iva2=total3
ex=total4
apagar=total5
iperce=total6
imulta=total7
*Impresion
IF o1=0
 SETCOLOR(color7)
 @1,0 CLEAR TO 24,79
 @7,18 TO 9,57 DOUBLE
 @10,18 TO 20,57 DOUBLE
 @8,19 SAY 'VENTAS Entre : '+DTOC(fe_des)+` y '+DTOC(fe_has)
 @12,20 SAY `Total NETO GRAVADO   : '+STR(ng,13,2)
 @13,20 SAY 'Total E X E N T O    : '+STR(EX,13,2)
 @14,20 SAY `Total I.V.A. '+STR(vai,5,2)+` % : '+STR(iva1,13,2)
 @15,20 SAY `Total I.V.A. '+STR(vani,5,2)+` % : '+STR(iva2,13,2)
 *@16,20 SAY `Total Ing.Bru.Percep.: '+STR(iperce,13,2)
 @18,20 SAY `Total  V E N D I D O : '+STR(apagar,13,2)
 SETCOLOR(color3)
 @23,20 SAY " ±Esc±-Sale  ±ÄÙ±-Continua"
 SETCOLOR(color7)
 DIST_COLOR(23,22,'~W+/R~Esc')
 DIST_COLOR(23,34,'~W+/R~ÄÙ')
 SET CURSOR OFF
 tecla=65
 DO WHILE tecla#13 .AND. tecla#27
  tecla=INKEY(0)
  IF tecla=-9
   CALCULATOR()
  ENDIF
 ENDDO
 IF tecla=27
  ok=1
 ENDIF
 @23,20 SAY SPACE(40)
 SET CURSOR ON
ELSE
 r='|'
 ??CHR(27)+CHR(120)+CHR(0)+CHR(18)
 ??CHR(14)
 ?`   VENTAS Entre : '+DTOC(fe_des)+` y '+DTOC(fe_has)
 ??CHR(20)
 ?
 ?SPACE(31)+REPLI('-',37)
 ?SPACE(30)+r+` Total NETO GRAVADO   : '+STR(ng,13,2)+r
 ?SPACE(30)+r+' Total E X E N T O    : '+STR(ex,13,2)+r
 ?SPACE(30)+r+` Total I.V.A. '+STR(vai,5,2)+` % : '+STR(iva1,13,2)+r
 ?SPACE(30)+r+` Total I.V.A. '+STR(vani,5,2)+` % : '+STR(iva2,13,2)+r
 *?SPACE(30)+r+` Total Ing.Bru.Percep.: '+STR(iperce,13,2)+r
 *?SPACE(30)+r+' Total Multa C.F.     : '+STR(imulta,13,2)+r
 ?SPACE(30)+r+` Total  V E N D I D O : '+STR(apagar,13,2)+r
 ?SPACE(31)+REPLI('-',37)
 ?
 SETCOLOR(color3)
 @23,20 SAY " ±Esc±-Sale  ±ÄÙ±-Continua"
 SETCOLOR(color7)
 DIST_COLOR(23,22,'~W+/R~Esc')
 DIST_COLOR(23,34,'~W+/R~ÄÙ')
 SET CURSOR OFF
 tecla=65
 DO WHILE tecla#13 .AND. tecla#27
  tecla=INKEY(0)
  IF tecla=-9
   CALCULATOR()
  ENDIF
 ENDDO
 IF tecla=27
  ok=1
 ENDIF
 @23,20 SAY SPACE(40)
 SET CURSOR ON
ENDIF
RETURN
