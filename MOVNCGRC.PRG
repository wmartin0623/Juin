*Carga de Notas de Creditos a CLIENTES para FA SALTEADAS
PRIVATE iva1,iva2,col_ant,ok,Ntxs1:="NADA"
col_ant=SETCOLOR()
PUBLIC iva1,iva2
*Porcentaje de iva
AREA()
IF USAR('PORCEIVA')
 iva1=IVAI
 iva2=IVANI
ELSE
 QUIT
ENDIF
*Clientes
AREA()
OPEN_DBF("MAECLIEN","CLIEN","C","MAECLIEN","clie","NOMCLIEN","nomb")
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
 Ntxs1=ARCH_EXI("BORRAR","NTX")
 INDEX ON clie+tcom TO &Ntxs1 FOR sact#0
 SET INDEX TO CCCLIENT,CCCLIE01,CCCLIE04,&Ntxs1
ELSE
 QUIT
ENDIF
SET RELATION TO clie INTO CLIEN
*Cuentas
*AREA()
*OPEN_DBF("MAECUENT","CUENTAS","C","MAECUENT","ncta","NOMCUENT","nomb")
PRIVATE pan_ant
PRIVATE vClie,vnom,vciv,vCta,vsal,vLiva
PRIVATE vFech,regCli,vMoro
PRIVATE vTcom,vLtra,vSrie,vnume,vImpo,vFech,vFVto,Reg_Nc
pan_ant=SAVESCREEN(0,0,24,79)
vClie=SPACE(6)
vnom=SPACE(30)
vsal=0
vFech=DATE()
DO WHILE .T.
 DO PAN_NCGRUCLI
 PRIVATE cbte[1],regh[1]
 vTcom="FA"
 vLtra=" "
 vSrie=SPACE(4)
 vnume=SPACE(8)
 vLiva=.T.
 Reg_Nc=0
 SETCOLOR(color3)
 @ 4,22 GET vClie PICTURE "@K 999999" VALID VAL_CLI7(@vClie,4,22,"W+/R",25)
 @ 4,73 GET vTcom PICTURE "@!" VALID vTcom="FA".OR.vTcom="ND"
 READ
 IF LASTKEY()=27
  EXIT
 ENDIF
 SETCOLOR("W+/R")
 @ 6,21 SAY vLtra+vSrie+vnume
 @ 6,43 SAY vImpo*(-1) PICTURE "999999999.99"
 @ 6,64 SAY vFech PICTURE "@D"
 SETCOLOR(color3)
 IF CARTEL("DATOS CORRECTOS  [ SI ]  [ NO ]",18,1,'N',colo3)
  IF LASTKEY()=27
   EXIT
  ENDIF
 ELSE
  LOOP
 ENDIF
 SELECT CTACTE
 SET ORDER TO 4
 SET FILTER TO tcom=vTcom.AND.clie=vClie
 GO TOP
 *Cargar comprobantes en CBTE[]
 SELECT CTACTE
 SEEK vClie+vTcom
 IF !FOUND()
  CARTEL(ALLTRIM(vnom)+" NO TIENE NADA para NOTA DE CREDITO  [ OK ]",20,1,'N',color3)
  SET FILTER TO
  LOOP
 ENDIF
 p=0
 DO WHILE Clie = vClie .AND. tcom=vTcom
  IF sact # 0 .AND. tcom=vTcom .AND. !BAJA
   IF p>0
    ASIZE(cbte,p+1)
    ASIZE(regh,p+1)
   ENDIF
   p = p+1
   cbte[LEN(cbte)] = "  ณ                ณ            ณ              ณ             "
   cbte[LEN(cbte)] = STUFF(cbte[LEN(cbte)],4,2,TCOM)
   cbte[LEN(cbte)] = STUFF(cbte[LEN(cbte)],7,13,ltra+srie+nume)
   cbte[LEN(cbte)] = STUFF(cbte[LEN(cbte)],22,10,DTOC(fech))
   cbte[LEN(cbte)] = STUFF(cbte[LEN(cbte)],35,12,TRANSFORM(sact,"999999999.99"))
   regh[LEN(regh)] = RECNO()
  ENDIF
  SKIP
 ENDDO
 IF p = 0
  CARTEL(ALLTRIM(vnom)+" NO TIENE NADA para NOTA DE CREDITO  [ OK ]",20,1,'N',color3)
  SET FILTER TO
  LOOP
 ENDIF
 *Ordena por numero
 DECLARE cam[1]
 cam[1] = "regh"
 ORDEN(cbte,cam)

 SETCOLOR("W+/R")
 @18,31 SAY "Totales :"
 @20,11 SAY CHR(24)+CHR(25)+' PgUp PgDn Mover   '+CHR(17)+'ฤู Marcar   [*]-Carga N.C.  [Esc]-Salir'
 SETCOLOR(color3)
 *mostrar para marcar
 SET CURSOR OFF
 com = 1
 rel = 0
 KEYBOARD CHR(24)+CHR(5)
 DO WHILE .T.
  com = ACHOICE(10,12,16,72,cbte,.T.,"sel_cbt1",com,rel)
  IF com = 0
   EXIT
  ENDIF
 ENDDO
 inkey(.1)
 IF LASTKEY() = 27 .OR. com=999
  SET FILTER TO
  LOOP
 ENDIF
 vtot_nc=0
 FOR CON = 1 TO LEN(cbte)
  IF SUBSTR(cbte[con],1,1) = 'ฑ'
   vtot_nc = vtot_nc + VAL(SUBSTR(cbte[con],50,12))
  ENDIF
 NEXT

 vtgral=ROUND(vTot_Nc,2)

 bien=CARTEL("CONFIRME NOTA DE CREDITO por [ "+STR(vtgral,12,2)+" ]  [ NO ]",20,1,'N',colo3)
 IF LASTKEY()=27.OR.!bien
  SET FILTER TO
  LOOP
 ENDIF

 *Actualizar CBTE en CTACTE
 SELECT CTACTE
 FOR con=1 TO LEN(cbte)
  IF SUBSTR(cbte[con],1,1)='ฑ'
   GO regh[con]
   RECLOCK(1)
   REPLACE sact WITH sact-VAL(SUBSTR(cbte[con],50,12))
   UNLOCK
  ENDIF
 NEXT
 IF Reg_Nc#0
  *Descontar de la NC original el importe utilizado y/o DELETE si el SACT=0
  GO Reg_Nc
  RECLOCK(1)
  REPLACE paga WITH paga + vTgral
  IF paga=0
   DELETE
  ENDIF
  UNLOCK
 ENDIF

 *Generar en ctacte el registro de NC
 *Actualizo Archivo de Ctas.Ctes.
 *Genero una NC por cada cbte.
 SELECT CTACTE
 FOR con = 1 TO LEN(cbte)
  IF SUBSTR(cbte[con],1,1)='ฑ'
   ADDREC(0)
   RECLOCK(0)
   REPLACE tcom WITH "NC"
   REPLACE tipo WITH "2"
   REPLACE clie WITH vClie
   REPLACE nume WITH vnume
   REPLACE ltra WITH vLtra
   REPLACE srie WITH vSrie
   REPLACE nint WITH SPACE(8)   &&solo para pagos
   REPLACE fech WITH vFech
   REPLACE fvto WITH vFVto
   REPLACE fpag WITH CTOD("  /  /    ")
   REPLACE ncom WITH SUBSTR(cbte[con],12,8)
   REPLACE lcom WITH SUBSTR(cbte[con],7,1)
   REPLACE scom WITH SUBSTR(cbte[con],8,4)
   REPLACE fcom WITH CTOD(SUBSTR(cbte[con],22,10) )
   REPLACE cont WITH .F.
   REPLACE baja WITH .F.
   REPLACE paga WITH VAL(SUBSTR(cbte[con],50,12))*(-1)
   REPLACE sact WITH 0
   REPLACE canc WITH .F.
   REPLACE comt WITH SUBSTR(cbte[con],4,2)
   REPLACE moro WITH vMoro
   UNLOCK
  ENDIF
 NEXT

 DBCOMMITALL()

ENDDO
RESTSCREEN(0,0,24,79,pan_ant)
CLOSE ALL

FERASE("&Ntxs1")

SETCOLOR(col_ant)
RETURN

FUNCTION sel_cbt1
PARAMETERS MOD,sub,pos
PRIVATE tcl,fil,pus,oriy,con,ydes
FIL=ROW()
IF LEN(cbte) < 7
 ydes = LEN(cbte)
ELSE
 ydes = 7
ENDIF
IF ydes = 1
 @10,10 SAY ''
ELSE
 pus = 7/LEN(cbte)*sub
 FOR oriy = 1 TO 7
    @ 9+oriy,10 SAY IF(pus <= oriy .and. pus >= oriy-.99999,IIF(sub=1,'',IIF(sub=LEN(cbte),'','')),'ณ')
 NEXT oriy
ENDIF
tcl = LASTKEY()
*Suma e imprime Sub_total
vtot_cbte = 0
vtot_nc = 0
FOR con = 1 TO LEN(cbte)
 IF SUBSTR(cbte[con],1,1) = 'ฑ'
  vtot_cbte = vtot_cbte + VAL(SUBSTR(cbte[con],35,12))
  vtot_nc = vtot_nc + VAL(SUBSTR(cbte[con],50,12))
 ENDIF
NEXT con
SETCOLOR("W+/R")
@18,46 SAY vtot_cbte PICTURE '@Z 999999999.99'
@18,61 SAY vtot_nc PICTURE '@Z 999999999.99'
*Si Completo el Importe de la NC Salimos automaticamente
IF vImpo = vTot_Nc
 MOD = 3
 tcl = 42
ENDIF
SETCOLOR(color3)
DO CASE
CASE MOD = 0
 RETURN(2)
CASE MOD = 1 .OR. MOD = 2
 TONO(2)
 RETURN(2)
CASE MOD = 3
 DO CASE
 CASE tcl = 27         &&Presiona [ESC] para terminar Pagos
  KEYBOARD CHR(27)
  RETURN(0)
 CASE tcl = 13
   IF SUBSTR(cbte[sub],1,1) # 'ฑ'
    Imp_Nc=vImpo - vTot_Nc
    IF VAL(SUBSTR(cbte[sub],35,12)) < Imp_Nc
     Imp_Nc=VAL(SUBSTR(cbte[sub],35,12))
    ENDIF
    SET CURSOR ON
    @FIL,61 GET imp_nc PICTURE "@K 999999999.99" VALID imp_nc<=VAL(SUBSTR(cbte[sub],35,12)).AND.;
                          vTot_Nc+Imp_Nc<=vImpo
    READ
    SET CURSOR OFF
    IF LASTKEY()#27
     cbte[sub] = STUFF(cbte[sub],1,1,'ฑ')
     cbte[sub] = STUFF(cbte[sub],50,12,TRANSFORM(imp_nc,"999999999.99"))
    ENDIF
   ELSE
    cbte[sub] = STUFF(cbte[sub],1,1,' ')
    cbte[sub] = STUFF(cbte[sub],50,12,SPACE(12))
   ENDIF
   KEYBOARD CHR(24)
   rel = pos
   RETURN(1)
 CASE tcl = 42
  vtot_cbte = 0
  vtot_nc=0
  FOR CON = 1 TO LEN(cbte)
   IF SUBSTR(cbte[con],1,1) = 'ฑ'
    vtot_cbte = vtot_cbte + VAL(SUBSTR(cbte[con],35,12))
    vtot_nc = vtot_nc + VAL(SUBSTR(cbte[con],50,12))
   ENDIF
  NEXT
  IF vtot_cbte = 0 .OR. vtot_nc = 0
   CARTEL("DEBE MARCAR ALGUN COMPROBANTE  [ OK ]",18,1,'N',color3)
   RETURN(1)
  ELSE
   RETURN(0)
  ENDIF
 OTHERWISE
  RETURN(2)
 ENDCASE
ENDCASE

*Control de Clientes
FUNCTION VAL_CLI7
PARAMETERS cli,fil,col,colo_sug,lon
IF LASTKEY()=27
 KEYBOARD CHR(27)
 RETURN .T.
ENDIF
PRIVATE colo_a,pan_ant
IF PCOUNT()=1
 ok1=0
ELSE
 ok1=1
ENDIF
antsel=ALLTRIM(STR(SELECT()))
colo_a=SETCOLOR()
SELECT CLIEN
SET ORDER TO 1
cli=CERO(VAL(cli),6)
vClie=cli
SEEK cli
IF .NOT. FOUND()
 pan_ant=savescreen(5,2,24,72)
 SETCOLOR(color3)
 SOMBRA(5,2,23,71,'V1',3,'',color3)
 @7,2 say'ฬออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน'
 @20,2 say'วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ'
 @17,2 say'ฬออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออน'
 SETCOLOR(INVER)
 @5,38 say' BUSCAR '
 SETCOLOR(color3)
 @18,3 SAY'BUSCAR:'
 @19,3 SAY SPACE(38)
 @21,13 say chr(24)+chr(25)+' PgUp PgDn Mover         '+chr(17)+'ฤู Acepta'
 @22,13 say'F2-Cambia Clave Busqueda  [Esc]-Salir'
 DECLARE cam[3],sep[3],ind[2],lar[2],nom[2]
 carga1(cam,"clie~nomb~domi")
 tit='Numero  Nombre  Cliente                Domicilio                           '
 carga2(sep,'5,2,0')
 ind[1]=1
 lar[1]=6
 nom[1]='Codigo Cliente  :    '
 ind[2]=2
 lar[2]=30
 nom[2]='Nombre del Cliente  :'
 ord_ant=STR(INDEXORD())
 SET ORDER TO 2
 GO TOP
 BUSCARE(6,4,16,70,cam,tit,sep,ind,lar,19,4,nom,18,14)
 regis=RECNO()
 SETCOLOR(colo_a)
 RESTSCREEN(5,2,24,72,pan_ant)
 SET ORDER TO &ord_ant
 IF LASTKEY()<>27
  vClie=clie
  vnom=nomb
  vciv=civa
  vsal=sact
  vCta="999"
  regcli=RECNO()
  IF ok1=1
   SETCOLOR(colo_sug)
   @fil,col SAY vClie+' '+SUBSTR(vnom,1,lon)
   SETCOLOR(colo_a)
  ENDIF
 ELSE
  SELECT &antsel
  RETURN .F.
 ENDIF
ELSE
 vnom=nomb
 vciv=civa
 vsal=sact
 vCta="999"
 regCli=RECNO()
 IF ok1=1
  SETCOLOR(colo_sug)
  @fil,col SAY vClie+' '+SUBSTR(vnom,1,lon)
  SETCOLOR(colo_a)
 ENDIF
ENDIF
*Buscar NC si Cbtes afectados
SELECT CTACTE
DBSETORDER(1)
p=0
SEEK vClie+'NC'
IF FOUND()
 DECLARE tabla[1]
 DO WHILE clie=vClie .AND. tcom='NC'
  IF ncom='********' .AND. paga#0 .AND. !BAJA
    carg=.T.
  ELSE
   carg=.F.
  ENDIF
  IF carg
   IF p>0
    ASIZE(tabla,p+1)
   ENDIF
   p=p+1
   tabla[p]=ltra+srie+nume+' '+DTOC(fech)+'     '+TRANSFORM(paga*(-1),'999,999,999.99')
  ENDIF
  SKIP
 ENDDO
ENDIF
IF p = 0
 CARTEL("CLIENTE no Tiene N.CREDITOS p/poner Nง de FACTURA [ OK ]",20,1,'N',color3)
 SET FILTER TO
 SETCOLOR(colo_a)
 SELECT &antsel
 RETURN .F.
ENDIF
SET ORDER TO 1
panant=SAVESCREEN(9,30,24,78)
SETCOLOR(colo3)
SOMBRA(9,30,23,77,'V1',3,'',colo3)
@11,30 say'ฬออออออออออออออออออออออออออออออออออออออออออออออน'
@21,30 say'ฬออออออออออออออออออออออออออออออออออออออออออออออน'
@10,31 SAY" Nฃmero NC       Fecha           Importe      "
*          "1234567890123 1234567890     123456789012
SETCOLOR(colo12)
@9,31 SAY 'NOTAS DE CREDITO SIN AFECTAR COMPROBANTES'
SETCOLOR(colo3)
@22,31 say chr(24)+chr(25)+' PgUp PgDn Mover   '+chr(17)+'ฤู Acepta   [Esc]-Salir'
*****************************************************************
elemento=SCR(tabla,9,12,31,LEN(tabla),9,30,23,77,1,color1,colo3)
*****************************************************************
RESTSCREEN(9,30,24,78,panant)
IF LASTKEY()=13
 vari=vClie+'NC'+SUBSTR(tabla[elemento],1,13)
 SEEK vari
 IF !FOUND()
  CARTEL("COMPROBANTE NO EXISTE  [ OK ]",22,1,'N',color3)
  SELECT &antsel
  SETCOLOR(colo_a)
  RETURN .F.
 ENDIF
ELSE
 IF LASTKEY()=27
  SELECT &antsel
  SETCOLOR(colo_a)
  RETURN .F.
 ENDIF
ENDIF
Reg_Nc=RECNO()
vnume=nume    &&Numero de la N.Credito
vLtra=ltra
vSrie=srie
vImpo=paga*(-1)
vFech=fech
vFvto=fvto
vMoro=moro
SELECT &antsel
RETURN .T.

PROCEDURE PAN_NCGRUcli
 SETCOLOR(color3)
 SOMBRA(5,10,21,75,'V2',3,'',color3)
 @ 3,10 SAY 'ฺฤฤฤฤฤฤฤฤฤฤฤฤASIGNACION N.CREDITO PENDIENTEฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ'
 @ 4,10 SAY 'ณCliente  :                                     ณNC Afecta a :   ณ'
 @ 5,10 SAY 'รฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤยฤฤมฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤด'
 @ 6,10 SAY 'ณNงN.Cred:               ณ Imp.:             ณ Fecha:            ณ'
 @ 7,10 SAY 'รฤฤฤยฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤยฤฤฤมฤฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤมฤฤฤยฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤด'
 @ 8,10 SAY 'ณMcaณ     Numero     ณ   Fecha    ณImporte c/IVA ณImporte Acred. ณ'
 @ 9,10 SAY 'รฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤด'
 @10,10 SAY 'ณ   ณ                ณ            ณ              ณ               ณ'
 @11,10 SAY 'ณ   ณ                ณ            ณ              ณ               ณ'
 @12,10 SAY 'ณ   ณ                ณ            ณ              ณ               ณ'
 @13,10 SAY 'ณ   ณ                ณ            ณ              ณ               ณ'
 @14,10 SAY 'ณ   ณ                ณ            ณ              ณ               ณ'
 @15,10 SAY 'ณ   ณ                ณ            ณ              ณ               ณ'
 @16,10 SAY 'ณ   ณ                ณ            ณ              ณ               ณ'
 @17,10 SAY 'รฤฤฤมฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤมฤฤฤฤฤฤฤฤฤฤฤฤมฤฤฤฤฤฤฤฤฤฤฤฤฤฤมฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤด'
 @18,10 SAY 'ณ                    Totales :                                   ณ'
 @19,10 SAY 'รฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤด'
 @20,10 SAY 'ณ                                                                ณ'
 @21,10 SAY 'ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู'
 @20,11 SAY CHR(24)+CHR(25)+' PgUp PgDn Mover   '+CHR(17)+'ฤู Marcar   [*]-Carga N.C.  [Esc]-Salir'
 @3,23 SAY "ASIGNACION N.CREDITO PENDIENTE" COLOR "W+/R"
 RETURN
