*Listado MAYOR de clientes, Historico
LOCAL Col_Ant,Pan_Ant
Pan_Ant=SAVESCREEN(,,,,)
col_ant=SETCOLOR()

*Clientes
AREA()
OPEN_DBF("MAECLIEN","CLIENTES","C","MAECLIEN","clie","NOMCLIEN","nomb")


*-----------------------------------------------------------------------------------------------
*CUENTA CORRIENTE
AREA()
IF USAR('CCCLIENT','CTACTE')
   IF !FILE('CCCLIE01.NTX')
    INDEX ON clie+DTOS(fech) TO CCCLIE01
   ENDIF
  INDEX ON clie+comt+lcom+scom+ncom TO BORRAR
  SET INDEX TO CCCLIE01,BORRAR
ELSE
 QUIT
ENDIF
*-----------------------------------------------------------------------------------------------
PRIVATE vclie,vnom,sal_clie,vtot,vacr,vnum,reg_cbte,tp_cbte
vclie=SPACE(6)
vnom=SPACE(30)
sal_clie=0
DO WHILE .T.
 vtot=0
 SETCOLOR(color3)
 SOMBRA(04,15,08,51,'H1',1,'',color3)
 @4,16 SAY "MAYOR HISTORICO DE CLIENTES"
 @05,18 SAY "Que Cliente: " GET vclie PICTURE "@K 999999" VALID VAL_CLI5(@vclie)

 READ
 IF LASTKEY()=27
  EXIT
 ENDIF
 @06,17 SAY vnom
 ok=0
 FECHAS(DATE(),DATE())
 IF ok=1
  LOOP
 ENDIF
 @07,16 SAY "Desde: "+DTOC(fe_des)+" Hasta: "+DTOC(fe_has)
 IF CARTEL("DATOS CORRECTOS  [ SI ]  [ NO ]",18,1,'N',colo3)
  IF LASTKEY()=27
   EXIT
  ENDIF
  SELECT CTACTE
  SET ORDER TO 1
  SEEK vclie
  IF !FOUND()
   CARTEL("SALDO DE "+LOWER(ALLTRIM(vnom))+" ES CERO (0) [ OK ]",20,1,'N',color3)
   RESTSCREEN(0,0,24,79,pan_ant)
   LOOP
  ENDIF
  vtot=0
  o1=0
  IF !IMPRI()
   o1=1
  ENDIF
  IF LASTKEY()#27
   SET ORDER TO 1
   IF SELECT("CTACTE1")#0
    SELECT CTACTE1
    CLOSE
   ENDIF
   SELECT CTACTE
   COPY TO TEMP1 FOR clie=vclie .AND. tcom#"RT"
   AREA()
   IF USAR("TEMP1","CTACTE1")
    INDEX ON clie+DTOS(fech) TO BORRAR2
    INDEX ON clie+comt+lcom+scom+ncom TO BORRAR3
    SET INDEX TO BORRAR2,BORRAR3
   ELSE
    RESTSCREEN(0,0,24,79,pan_ant)
    LOOP
   ENDIF

   IF o1=1
    DO macl_imp1
   ELSE
    DO macl_pan1
   ENDIF
  ENDIF
  RESTSCREEN(0,0,24,79,pan_ant)
 ELSE
  LOOP
 ENDIF
ENDDO
RESTSCREEN(0,0,24,79,pan_ant)
CLOSE ALL
SETCOLOR(col_ant)
RETURN

PROCEDURE macl_pan1
ok=0
con1=0
hoja=1
vSalAnt=0
xSalAnt=0
vTot=0
DO WHILE .NOT.EOF().AND.vclie=clie
 SETCOLOR(color7)
 @1,0 CLEAR TO 24,79
 DO PAN_MAYOCL
 SETCOLOR(color3)
 @1,0 SAY "MAYOR DE : "+vclie+' '+SUBSTR(vnom,1,20)+' '+"De: "+DTOC(fe_des)+" a: "+DTOC(fe_has)
 SETCOLOR('*'+color3)
 SETCOLOR(color1)
 hor = 4
 Amor = 1
 IF hoja=1
  hoja=2
  IF vacr<>0
   vtot=vtot+(vacr*(-1))
   @hor,1 SAY ' AC'
   @hor,6 SAY 'Resto de'
   @hor,15 SAY 'FACTURA'
   @hor,42 SAY vtot PICTURE "@Z 999999999.99"
   @hor,66 SAY vtot PICTURE "999999999.99"
   SETCOLOR(color3)
   @23,66 SAY vtot PICTURE "999999999.99"
   SETCOLOR(color1)
   hor=hor+1
  ENDIF
 ENDIF
 DO WHILE .NOT.EOF() .AND. hor<=21.AND.vclie=clie
  IF fech>=fe_des.AND.fech<=fe_has
   IF baja
    SKIP
    LOOP
   ENDIF
  ELSE
   IF fech<fe_des .AND. xSalAnt=0
    IF baja
     SKIP
     LOOP
    ELSE
     *Acumula saldo Anterior
     IF !baja
      IF paga>=0
       debe=paga
       habe=0
      ELSE
       debe=0
       habe=paga
      ENDIF
     ELSE
      STORE 0 TO debe,habe
     ENDIF
     vSalAnt=vSalAnt+debe+habe
     RECLOCK(1)
     REPLACE myan WITH '@@@@@'
     UNLOCK
    ENDIF
   ENDIF
   SKIP
   LOOP
  ENDIF

  IF tcom='CH'.OR.tcom='FA'.OR.tcom='ND'.OR.paga > 0
   reg_cbte = RECNO()
   tp_cbte = 'CB'
  ENDIF

  IF tp_cbte = 'CB'
   DO PONE
   RECLOCK(1)
   REPLACE myan WITH '@@@@@'
   UNLOCK
   tp_cbte = 'PA'
   amor=0
  ENDIF

  IF tp_cbte = 'PA'
   SET ORDER TO 2
   IF amor = 0
    SET ORDER TO 2
    SEEK vclie+vnum
    busca = FOUND()
   ELSE
    busca=.T.
   ENDIF
   IF busca
    xbus =comt+lcom+scom+ncom
    DO WHILE !EOF().AND.hor<=21.AND.(comt+lcom+scom+ncom=xbus.AND.;
             clie=vclie).AND.!baja
     IF fech>=fe_des.AND.fech<=fe_has
      DO PONE
      RECLOCK(1)
      REPLACE myan WITH '@@@@@'
      UNLOCK
     ENDIF
     SKIP
    ENDDO
    IF hor > 21
     IF xbus # comt+lcom+scom+ncom
      SET ORDER TO 1
      tp_cbte='NA'
      GO reg_cbte
      SKIP
     ENDIF
     EXIT
    ENDIF
    IF xbus # comt+lcom+scom+ncom
     SET ORDER TO 1
     tp_cbte='NA'
     GO reg_cbte
    ENDIF
   ELSE
    SET ORDER TO 1
    tp_cbte='NA'
    GO reg_cbte
   ENDIF
  ENDIF

  SKIP
 ENDDO
 IF .NOT.EOF()
  IF xSalAnt=0.AND.vSalAnt#0
   @hor,40 SAY "Saldo Anterior: "
   @hor,66 SAY vSalAnt PICTURE "999999999.99"
   xSalAnt=1
   hor=hor+1
   vtot=vtot+vSalAnt
   @23,66 SAY vtot PICTURE "999999999.99"
  ENDIF
  SETCOLOR(color3)
  @23,20 SAY " ±Esc±-Sale  ±ÄÙ±-Continua"
  SETCOLOR(color7)
  DIST_COLOR(23,22,'~W+/R~Esc')
  DIST_COLOR(23,34,'~W+/R~ÄÙ')
  IF con1=1
   DIST_COLOR(24,5,'~W+/R~* - Factura Contado')
  ENDIF
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
   EXIT
  ENDIF
  @23,20 SAY SPACE(30)
  SET CURSOR ON
 ENDIF
ENDDO
IF ok=0
  IF xSalAnt=0.AND.vSalAnt#0
   @hor,40 SAY "Saldo Anterior: "
   @hor,66 SAY vSalAnt PICTURE "999999999.99"
   vtot=vtot+vSalAnt
   hor=hor+1
  ENDIF
  *Sumar Cbtes. No incluidos
  SUM paga TO xtotal FOR myan#"@@@@@".AND.fech<=fe_has.AND.paga<0
  IF xtotal # 0
   vtot=vtot+xtotal
   @hor,6 SAY "Cbtes. Excluidos: "
   @hor,42 SAY xtotal PICTURE "999999999.99"
   @hor,66 SAY vtot PICTURE "999999999.99"
   hor=hor+1
   @23,66 SAY vtot PICTURE "999999999.99"
  ENDIF
  SETCOLOR(color3)
  @23,20 SAY " ±Esc±-Termina "
  SETCOLOR(color7)
  DIST_COLOR(23,22,'~W+/R~Esc')
  IF con1=1
   DIST_COLOR(24,5,'~W+/R~* - Factura Contado')
  ENDIF
  SET CURSOR OFF
  tecla=65
  DO WHILE tecla#27
   tecla=INKEY(0)
   IF tecla=-9
    CALCULATOR()
   ENDIF
  ENDDO
  @23,20 SAY SPACE(30)
  SET CURSOR ON
ENDIF
RETURN

PROCEDURE pone
LOCAL vFec,vObse,vNpa,xPaga,Debe,Habe,vCont,Con1
  vnum=tcom+ltra+srie+nume
  vfec=fech
  vobse=""    &&obse
  vnpa= comt+lcom+scom+ncom

  xpaga=IIF(baja,0,paga)

  IF xpaga>=0
   debe=xpaga
   habe=0
  ELSE
   debe=0
   habe=xpaga
  ENDIF
  IF cont
   con1=1
   vcont='*'
  ELSE
   vcont='º'
  ENDIF
  IF baja
   vcont='B'
  ENDIF
  IF xSalAnt=0
   @hor,40 SAY "Saldo Anterior: "
   @hor,68 SAY vSalAnt PICTURE "9999999.99"
   hor=hor+1
   xSalAnt=1
   vtot=vtot+vSalAnt
  ENDIF

  vtot=vtot+debe+habe
  @hor,0 SAY vcont+LEFT(vnum,2) COLOR IIF(tp_cbte='CB',color1,color9)
  @hor,4 SAY SUBSTR(vnum,3) COLOR IIF(tp_cbte='CB',color1,color9)
  @hor,18 SAY DTOC(vfec) COLOR IIF(tp_cbte='CB',color1,color9)
  @hor,29 SAY LEFT(vnpa,2) COLOR IIF(tp_cbte='CB',color1,color9)
  @hor,32 SAY SUBSTR(vnpa,3) COLOR IIF(tp_cbte='CB',color1,color9)
  @hor,57 SAY debe PICTURE "@Z 9999999.99" COLOR IIF(tp_cbte='CB',color1,color9)
  @hor,46 SAY habe PICTURE "@Z 9999999.99" COLOR IIF(tp_cbte='CB',color1,color9)
  SETCOLOR(color1)
  @hor,68 SAY vtot PICTURE "9999999.99"
  SETCOLOR(color3)
  @23,68 SAY vtot PICTURE "9999999.99"
  SETCOLOR(color1)
  hor = hor+1
RETURN

PROCEDURE macl_imp1
@1,0 CLEAR TO 24,79
@12,15 SAY 'ESPERE..... ESTOY IMPRIMIENDO'
SET CONSOLE OFF
SET PRINT ON
SELECT CTACTE1
ok=0
pagina=0
con1=0
hoja=1
vSalAnt=0
xSalAnt=0
?? chr(27)+chr(120)+chr(0)+chr(18)
?? chr(27)+chr(67)+chr(72)
?? CHR(27)+CHR(33)+CHR(5)
DO WHILE !EOF() .AND. INKEY(.1) # 27.AND.vclie=clie
 pagina=pagina+1
 NORMA()
 GRAN_ON()
 ? SUBSTR(nom_cemp+SPACE(25),1,25)
 GRAN_OFF()
 NORMA()
 CHICO()
 ?? SPACE(23)+'Pagina Nro. : '+STR(pagina,3)
 NORMA()
 GRAN_ON()
 ? SUBSTR('MAYOR DE '+vNom+SPACE(31),1,31)
 GRAN_OFF()
 NORMA()
 CHICO()
 ??SPACE(3)+'Fecha       : '+STUFF(DTOC(DATE()),3,4,'-'+MESES(DATE())+'-')
 NORMA()
*CHICO()
 ? "Periodo: "+DTOC(fe_des)+"..."+DTOC(fe_has)
 ? '=Comprobante================= =Rec.Pagado=======  ==Importes=============================='
 ? 'Tipo  Numero         Fecha    Tipo  Numero           Debe           Haber        Saldo    '
 ? '==== ============= ========== ==== =============  ============   ============ ============'
*   1234567890123456789012345678901234567890123456789012345678901234567890
 hor=6
 Amor=1
 IF hoja=1
  hoja=2
  IF vacr<>0
   vtot=vtot+(vacr*(-1))
   ? ' AC  Resto de FACTURA'+SPACE(29)+STR(vtot,12,2)+SPACE(16)+STR(vtot,12,2)
   hor=hor+1
  ENDIF
 ENDIF

 DO WHILE .NOT.EOF() .AND. hor<=66 .AND. INKEY(.1) # 27.AND.vclie=clie
  IF fech>=fe_des.AND.fech<=fe_has
   IF baja
    SKIP
    LOOP
   ENDIF
  ELSE
   IF fech<fe_des .AND. xSalAnt=0
    IF baja
     SKIP
     LOOP
    ELSE
     *Acumulaa saldo Anterior
     IF !baja
      IF paga>=0
       debe=paga
       habe=0
      ELSE
       debe=0
       habe=paga
      ENDIF
     ELSE
      STORE 0 TO debe,habe
     ENDIF
     vSalAnt=vSalAnt+debe+habe
     RECLOCK(1)
     REPLACE myan WITH '@@@@@'
     UNLOCK
    ENDIF
   ENDIF
   SKIP
   LOOP
  ENDIF
  IF tcom='CH'.OR.tcom='FA'.OR.tcom='ND'.OR.paga > 0
   reg_cbte = RECNO()
   tp_cbte = 'CB'
  ENDIF

  IF tp_cbte = 'CB'
   DO PONE1
   RECLOCK(1)
   REPLACE myan WITH '@@@@@'
   UNLOCK
   tp_cbte = 'PA'
   amor=0
  ENDIF

  IF tp_cbte = 'PA'
   SET ORDER TO 2
   IF amor = 0
    SET ORDER TO 2
    SEEK vclie+vnum
    busca = FOUND()
   ELSE
    busca=.T.
   ENDIF
   IF busca
    xbus =comt+lcom+scom+ncom
    DO WHILE !EOF().AND.hor<=66.AND.(comt+lcom+scom+ncom=xbus.AND.;
             clie=vclie).AND.!baja
     IF fech>=fe_des.AND.fech<=fe_has
      DO PONE1
      RECLOCK(1)
      REPLACE myan WITH '@@@@@'
      UNLOCK
     ENDIF
     SKIP
    ENDDO
    IF hor > 66
     IF xbus # comt+lcom+scom+ncom
      SET ORDER TO 1
      tp_cbte='NA'
      GO reg_cbte
      SKIP
     ENDIF
     EXIT
    ENDIF
    IF xbus # comt+lcom+scom+ncom
     SET ORDER TO 1
     tp_cbte='NA'
     GO reg_cbte
    ENDIF
   ELSE
    SET ORDER TO 1
    tp_cbte='NA'
    GO reg_cbte
   ENDIF
  ENDIF

  SKIP
 ENDDO
 IF LASTKEY()=27
  ? "LISTADO ABORTADO POR EL OPERADOR"
  EXIT
 ENDIF
 IF vclie=clie
  IF con1=1
   ? SPACE(20)+'[ * ] - Factura Contado No Pagada'
  ENDIF
  EJECT
 ELSE
  IF xSalAnt=0.AND.vSalAnt#0
   ? SPACE(61)+'Saldo Anterior : '+STR(vSalAnt,12,2)
   vtot=vtot+vSalAnt
   xSalAnt=1
  ENDIF
  *Sumar Cbtes. No incluidos
  SUM paga TO xtotal FOR myan#"@@@@@".AND.fech<=fe_has.AND.paga<0
  IF xtotal # 0
   vtot=vtot+xtotal
   ? SPACE(10)+'Cbtes. Excluidos:   '+SPACE(20)+STR(xtotal,12,2)+SPACE(16)+STR(vtot,12,2)
   hor=hor+1
  ENDIF
  ?
  ? SPACE(61)+'**** SALDO *** : '+STR(vtot,12,2)
  ?
  IF con1=1
   ? SPACE(20)+'[ * ] - Factura Contado No Pagada'
  ENDIF
 ENDIF
ENDDO
EJECT
?? CHR(27)+CHR(33)+CHR(8)
SET PRINT OFF
RETURN

PROCEDURE pone1
LOCAL vFec,vObse,vNpa,xPaga,Debe,Habe,vCont,Con1
 vtip=tcom
 vnum=tcom+ltra+srie+nume
 vfec=fech
 vobse=""    &&obse
 vnpa= lcom+scom+ncom
 vtpa=comt

 xpaga=IIF(baja,0,paga)

 IF xpaga>=0
  debe=xpaga
  habe=0
 ELSE
  debe=0
  habe=xpaga
 ENDIF
 IF cont
  con1=1
  vcont='*'
 ELSE
  vcont=' '
 ENDIF
 IF baja
  vcont='B'
 ENDIF
 IF xSalAnt=0
  ? SPACE(61)+'Saldo Anterior : '+STR(vSalAnt,12,2)
  hor=hor+1
  xSalAnt=1
  vtot=vtot+vSalAnt
 ENDIF
 vtot=vtot+debe+habe
 IF xpaga < 0
  ?  vcont+'  '+vtip+SUBSTR(vnum,3)+' '+DTOC(vfec)+'  '+vtpa+'  '+vnpa+'  '+IIF(habe=0,SPACE(12),STR(habe,12,2))+'   '+IIF(debe=0,SPACE(12),STR(debe,12,2))+' '+STR(vtot,12,2)
 ELSE
  ?  vcont+' '+vtip+' '+SUBSTR(vnum,3)+' '+DTOC(vfec)+'  '+vtpa+'  '+vnpa+'  '+IIF(habe=0,SPACE(12),STR(habe,12,2))+'   '+IIF(debe=0,SPACE(12),STR(debe,12,2))+' '+STR(vtot,12,2)
 ENDIF
 hor=hor+1
RETURN
