*Listado RESUMEN por Zonas
PARAMETERS p6
PUBLIC iva1,iva2,col_ant,ok,regcli
PRIVATE pan_ant,col_ant
pan_ant=SAVESCREEN(0,0,24,79)
col_ant=SETCOLOR()
*clientes
AREA()
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
SET RELATION TO clie INTO CLIENTES

PRIVATE pant_ant,vsal,vtot,vMoro,vAn_Si
PRIVATE vCli,Nom_Clie
pan_ant=SAVESCREEN(0,0,24,79)
vcli=SPACE(6)
STORE SPACE(30) TO nom_cli
vsal=0
vMoro="N"
vAn_Si=.T.
DO WHILE .T.
 vtot=0
 SETCOLOR(color3)
 SOMBRA(5,22,12,57,'H1',1,'',color3)
 IF p6=1
  @8,25 SAY "Morosos S/N : " GET vmoro PICTURE "!" VALID vMoro$"sSnN"
 ELSE
  vMoro="S"
 ENDIF
 READ
 IF LASTKEY()=27
  EXIT
 ENDIF
 IF p6=2
  @9,25 SAY "Que Cliente  : " GET vcli PICTURE "@K 999999" VALID VAL_CLI8(@vcli)
  READ
  IF LASTKEY()=27
   EXIT
  ENDIF
  @10,24 SAY SUBSTR(nom_cli,1,20) COLOR "W+/R"
  vAn_Si=.T.
 ELSE
  vAn_Si=CARTEL("COMO DESEA EL LISTADO... ?  [ ANALITICO ] [ SINTETICO ]",16,1,'N',colo3)
  IF LASTKEY()=27
   EXIT
  ENDIF
 ENDIF
 ok=0
 FECHAS(DATE(),DATE())
 IF ok=1
  EXIT
 ENDIF
 @11,24 SAY 'Desde '+DTOC(fe_des)+' Hasta '+DTOC(fe_has) COLOR "W+/R"
 IF CARTEL("DATOS CORRECTOS  [ SI ]  [ NO ]",18,1,'N',colo3)
  IF LASTKEY()=27
   EXIT
  ENDIF
  SELECT CTACTE
  IF p6=1
   condicion=".T."
   SET FILTER TO sact>0 .AND. !canc .AND. !baja.and. IIF(vMoro="S",moro="M",moro#"M");
                 .AND.fech>=fe_des.AND.fech<=fe_has
   GO TOP
   Encontro = .T.
  ELSE
   condicion="clie=vcli"
   SET FILTER TO clie=vcli .AND. sact>0 .AND. !canc .AND. !baja;
                 .AND.fech>=fe_des.AND.fech<=fe_has
   GO TOP
   SEEK vcli
   Encontro = FOUND()
  ENDIF
  IF !Encontro
   CARTEL("NO HAY COMPROBANTES PENDIENTES [ OK ]",20,1,'N',color3)
   RESTSCREEN(0,0,24,79,pan_ant)
   LOOP
  ENDIF
  vtot=0
  o1=0
  IF !IMPRI()
   o1=1
  ENDIF
  IF LASTKEY()#27
   IF o1=1
    DO res_imp
   ELSE
    DO res_pan
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

PROCEDURE res_pan
vtot=0
vsub=0
SET CENTURY OFF
vcli=clie
bcli=vcli
DO WHILE !EOF() .AND. INKEY(.1)#27 .AND. &condicion
 *dibujar encabezado
 SETCOLOR(color7)
 @1,0 CLEAR TO 24,79
 IF vAn_Si
  @1,0 TO 4,79 DOUBLE
  @22,0 TO 24,79
 ELSE
  @3,0 TO 3,79 DOUBLE
 ENDIF
 SETCOLOR(color3)
 IF p6=1
  @1,1 SAY CENTRA(`RESUMEN CTA.CTE.:  De: '+DTOC(fe_des)+' a '+DTOC(fe_has)+" "+IIF(vMoro="S","Deudores",SPACE(8)),78,'อ')
 ELSE
  @1,1 SAY CENTRA(`CBTES.PENDIENTES :  De: '+DTOC(fe_des)+' a '+DTOC(fe_has),78,'อ')
 ENDIF
 SETCOLOR(color1)
 @2,1 SAY`Codigo   Nombre Cliente                     S A L D O S'
 IF vAn_Si
  @3,1 SAY`Fecha     Comp.         0a10       11a20       21a30       31a45    +45 Dias'
  hor=5
 ELSE
  hor=4
 ENDIF
 @hor,0
 DO WHILE !EOF() .AND. INKEY(.1)#27.AND. hor<=21.AND.bcli=vcli
   @hor,0 SAY vcli+'  '+CLIENTES->nomb
   IF vAn_Si
    @hor,45 SAY "Ultima Compra:"
    @hor,60 SAY CLIENTES->fuco PICTURE "@D" COLOR color9
    hor=hor+1
   ENDIF
   DO WHILE !EOF() .AND. INKEY(.1)#27 .AND. hor<=21 .AND. bcli=vcli
     saldo=sact
     vfec=fech
     dif=DMA(vfec,DATE())
     STORE SPACE(11) TO vim1,vim2,vim3,vim4,vim5
     IF dif>=0 .AND. dif<=10
      vim1=STR(saldo,11,2)
     ENDIF
     IF dif>=11 .AND. dif<=20
      vim2=STR(saldo,11,2)
     ENDIF
     IF dif>=21 .AND. dif<=30
      vim3 = STR(saldo,11,2)
     ENDIF
     IF dif>=31 .AND. dif<=45
      vim4=STR(saldo,11,2)
     ENDIF
     IF dif>45
      vim5=STR(saldo,11,2)
     ENDIF
     vsub=vsub+saldo
     vtot=vtot+saldo
     IF vAn_Si
      @hor,0 SAY DTOC(vfec)+' '+tcom+nume+' '+vim1+' '+vim2+' '+vim3+' '+vim4+' '+vim5
      hor=hor+1
     ENDIF
    SKIP
    vcli=clie
   ENDDO
   IF vcli#bcli .AND. vsub<>0
    IF vAn_Si
     @hor,35 SAY '*** Sub-Total : '+STR(vsub,12,2)
    ELSE
     @hor,45 SAY STR(vsub,12,2)
    ENDIF
    IF hor+1<>23
     IF vAn_Si
      @hor+1,0 SAY REPLI("ฤ",79)
      hor=hor+2
     ELSE
      hor=hor+1
     ENDIF
    ELSE
     hor=hor+1
    ENDIF
    vsub=0
    bcli=vcli
   ENDIF
 ENDDO
 IF hor>21 .AND.!EOF()
  SETCOLOR(color3)
  @23,20 SAY " ฑEscฑ-Sale  ฑฤูฑ-Continua "
  SETCOLOR(color7)
  DIST_COLOR(23,22,'~W+/R~Esc')
  DIST_COLOR(23,34,'~W+/R~ฤู')
  tecla=65
  SET CURSOR OFF
  DO WHILE tecla#27 .AND. tecla#13
   tecla=INKEY(0)
   IF tecla=-9
    CALCULATOR()
   ENDIF
  ENDDO
  @23,20 SAY SPACE(40)
  SET CURSOR ON
  IF tecla=27
   EXIT
  ENDIF
 ENDIF
ENDDO
SETCOLOR(color3)
@hor+1,45 SAY '***   T O T A L  : '+STR(vtot,12,2)
@23,20 SAY " ฑEscฑ-Sale "
SETCOLOR(color7)
DIST_COLOR(23,22,'~W+/R~Esc')
tecla=65
SET CURSOR OFF
DO WHILE tecla#27
 tecla=INKEY(0)
 IF tecla=-9
  CALCULATOR()
 ENDIF
ENDDO
@23,20 SAY SPACE(40)
SET CURSOR ON
SET CENTURY ON
RETURN

PROCEDURE res_imp
vtot=0
vsub=0
SET CENTURY OFF
vcli=clie
bcli=vcli
pagina=0
?? CHR(27)+CHR(120)+CHR(0)+CHR(18)
?? CHR(27)+CHR(67)+CHR(72)
?? CHR(27)+CHR(33)+CHR(5)
DO WHILE !EOF() .AND. INKEY(.1)#27.AND.&condicion
 *dibujar encabezado
 pagina=pagina+1
 NORMA()
 GRAN_ON()
 ? SUBSTR(nom_emp+SPACE(25),1,25)
 GRAN_OFF()
 NORMA()
 CHICO()
 ?? SPACE(23)+'Pagina Nro. : '+STR(pagina,2)
 NORMA()
 GRAN_ON()
 ? "CUENTAS CORRIENTES"
 GRAN_OFF()
 NORMA()
 CHICO()
 ??SPACE(3)+'Fecha       : '+STUFF(DTOC(DATE()),3,4,'-'+MESES(DATE())+'-')
 NORMA()
 IF p6=1
  ? IIF(vMoro="S","Deudores","")+"  Periodo: "+DTOC(fe_des)+" a "+DTOC(fe_has)
 ELSE
  ? "Periodo: "+DTOC(fe_des)+" a "+DTOC(fe_has)
 ENDIF
 ?  'Codigo y Nombre Cliente                            S A L D O S'
 IF vAn_Si
  ? 'Fecha    Tp.N#.Comp               0a10        11a20        21a30        31a45     +45 Dias'
  ? 'ฤฤฤฤฤฤฤฤ ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ ฤฤฤฤฤฤฤฤฤฤฤฤ ฤฤฤฤฤฤฤฤฤฤฤฤ ฤฤฤฤฤฤฤฤฤฤฤฤ ฤฤฤฤฤฤฤฤฤฤฤฤ ฤฤฤฤฤฤฤฤฤฤฤฤ'
  hor=6
 ELSE
  ? '------ ------------------------------             ------------'
  hor=5
 ENDIF
 DO WHILE !EOF() .AND. INKEY(.1)#27.AND. hor<=63.AND.bcli=vcli
   ? vcli+'  '+CLIENTES->nomb+IIF(vAn_Si,'  Ultima Compra: '+DTOC(CLIENTES->fuco),SPACE(12))
   IF vAn_Si
    hor=hor+1
   ENDIF
   DO WHILE !EOF() .AND. INKEY(.1)#27 .AND. hor<=63 .AND. bcli=vcli
     saldo=sact
     vfec=fech
     dif=DMA(vfec,DATE())
     STORE SPACE(12) TO vim1,vim2,vim3,vim4,vim5
     IF dif>=0 .AND. dif<=10
      vim1=STR(saldo,12,2)
     ENDIF
     IF dif>=11 .AND. dif<=20
      vim2=STR(saldo,12,2)
     ENDIF
     IF dif>=21 .AND. dif<=30
      vim3=STR(saldo,12,2)
     ENDIF
     IF dif>=31 .AND. dif<=45
      vim4=STR(saldo,12,2)
     ENDIF
     IF dif>45
      vim5=STR(saldo,12,2)
     ENDIF
     vsub=vsub+saldo
     vtot=vtot+saldo
     IF vAn_Si
      ? DTOC(vfec)+' '+tcom+' '+ltra+srie+nume+' '+vim1+' '+vim2+' '+vim3+' '+vim4+' '+vim5
      hor=hor+1
     ENDIF
    SKIP
    vcli=clie
   ENDDO
   IF vcli#bcli .AND. vsub#0
    IF vAn_Si
     ? SPACE(41)+'*** Sub-Total : '+STR(vsub,12,2)
     ?       &&REPLI("ฤ",90)
     hor=hor+2
    ELSE
     ?? STR(vSub,12,2)
     hor++
    ENDIF
    vsub=0
    bcli=vcli
   ENDIF
 ENDDO
 IF hor>63 .AND. !EOF()
  EJECT
 ENDIF
ENDDO
? SPACE(46)+'***   T O T A L  : '+STR(vtot,12,2)
EJECT
?? CHR(27)+CHR(33)+CHR(8)
SET CURSOR ON
SET CENTURY ON
RETURN

*Control de clientes
FUNCTION VAL_CLI8
PARAMETERS cli
IF LASTKEY() = 27
 KEYBOARD CHR(27)
 RETURN .T.
ENDIF
PRIVATE colo_a,pan_ant
antsel=ALLTRIM(STR(SELECT()))
colo_a=SETCOLOR()
SELECT CLIENTES
SET ORDER TO 1
cli=CERO(VAL(cli),6)
vcli=cli
IF vcli='000000'
 nom_cli='CONSUMIDOR OCASIONAL'
 SELECT &antsel
 RETURN .T.
ENDIF
SEEK cli
IF .NOT. FOUND()
 pan_ant=savescreen(5,2,24,72)
 SETCOLOR(color3)
 sombra(5,2,23,71,'V1',3,'',color3)
 @7,2 say'ฬออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน'
 @20,2 say'วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ'
 @17,2 say'ฬออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน'
 SETCOLOR("I")
 @5,38 say' BUSCAR '
 SETCOLOR(color3)
 @18,3 SAY'BUSCAR:'
 @19,3 SAY SPACE(38)
 @21,13 say chr(24)+chr(25)+' PgUp PgDn Mover         '+chr(17)+'ฤู Acepta'
 @22,13 say'F2-Cambia Clave Busqueda  [Esc]-Salir'
 DECLARE cam[3],sep[3],ind[2],lar[2],nom[2]
 carga1(cam,"clie~nomb~domi")
 tit='Codigo  Nombre  Cliente                Domicilio                           '
 carga2(sep,'5,2,0')
 ind[1]=1
 lar[1]=6
 nom[1]='Codigo cliente : '
 ind[2]=2
 lar[2]=30
 nom[2]='Nombre del Cliente : '
 ord_ant=str(INDEXORD())
 SET ORDER TO 2
 GO TOP
 BUSCARE(6,4,16,70,cam,tit,sep,ind,lar,19,4,nom,18,14)
 regis=recno()
 SETCOLOR(colo_a)
 restscreen(5,2,24,72,pan_ant)
 set order to &ord_ant
 release cam,sep,ind,lar,tit,ord_ant
 IF LASTKEY() <> 27
  vcli=clie
  nom_cli=nomb
 ELSE
  RELEASE campi,ntx1,ntx2,l1,l2,camp1,archivo
  SELECT &antsel
  RETURN .F.
 ENDIF
ELSE
 nom_cli=nomb
ENDIF
SELECT &antsel
RETURN .T.
