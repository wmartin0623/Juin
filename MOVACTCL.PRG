*Actualizacion del Arhivo CCCLIENT.DBF
*Se coloca un .T. en el campo CANC de los comprobantes Cancelados
*del Tipo FA, ND y CH como asi Todos sus comprobantes relacionados
LOCAL col_ant,pan_ant,vTot,vCan,vLtra,vSrie,vNum,vTip,vCan1,Reg,vClie,vRecno
LOCAL Pipi
col_ant=SETCOLOR()
pan_ant=SAVESCREEN(0,0,24,79)
*CUENTA CORRIENTE
AREA()
IF USAR('CCCLIENT','CTACTE',"E")
  IF !FILE('CCCLIENT.NTX')
   INDEX ON clie+tcom+ltra+srie+nume TO CCCLIENT
  ENDIF
  IF !FILE('CCCLIE01.NTX')
   INDEX ON clie+DTOS(fech) TO CCCLIE01
  ENDIF
  IF !FILE('CCCLIE04.NTX')
   INDEX ON tcom+ltra+srie+nume TO CCCLIE04
  ENDIF
  INDEX ON lcom+scom+ncom+comt TO CCCLIE05
  SET INDEX TO CCCLIENT,CCCLIE01,CCCLIE04,CCCLIE05
ELSE
 QUIT
ENDIF
SETCOLOR(color3)
SOMBRA(12,38,16,60,'H1',1,'',color3)
GO TOP
vtot=CERO(RECCOUNT(),5)
SETCOLOR("W+/R")
@13,40 SAY "Reg.Actual    Total "
vcan=1
REPLACE ALL canc WITH .F.
GO TOP
DO WHILE .NOT. EOF()
 IF TCOM='FA' .OR. TCOM='ND' .OR. TCOM='CH' .OR. TCOM="TK"
  IF sact=0
   reg=RECNO()
   vltra=ltra
   vsrie=srie
   vnum=nume
   vtip=tcom
   SET ORDER TO 4
   SEEK vltra+vsrie+vnum+vtip
   ok=0
   IF FOUND()
    vcan1=vcan
    DO WHILE ncom=vnum .AND. comt=vtip .AND. lcom=vltra .AND. scom=vsrie
     @14,43 SAY CERO(vcan1,5)+'   /  '+vtot
     RECLOCK(1)
     REPLACE canc WITH .T.
     ok=1
     UNLOCK
     vcan1=vcan1+1
     SKIP
    ENDDO
   ENDIF
   SET ORDER TO 1
   GO reg
   IF ok=1
    RECLOCK(1)
    REPLACE canc WITH .T.
    UNLOCK
   ELSE
    *Cancela los Cbtes. que se cargaron sin Importe, para reponer Gondolas
    IF paga=0 .AND. sact=0
     RECLOCK(1)
     REPLACE canc WITH .T.
     UNLOCK
    ENDIF
   ENDIF
  ENDIF
 ENDIF
 SKIP
 @14,43 SAY CERO(vcan,5)+'   /  '+vtot
 vcan=vcan+1
ENDDO
*Actualizar Cbtes. por N.Creditos de Grupos
SELECT CTACTE
GO TOP

DO WHILE .NOT. EOF()
 IF ncom = '--------' .AND. tcom = 'NC'
  des_fa = defa
  has_fa = hafa
  vltra = lcom
  vsrie = srie
  vclie = clie
  vrecno= RECNO()
  SET ORDER TO 1
  SEEK vclie+'FA'+vltra+vsrie+des_fa
  IF FOUND()
   DECLARE regh[1]
   p=0
   ok=0
   DO WHILE nume>=des_fa.AND.nume<=has_fa.AND.ltra=vltra;
                         .AND.srie=vsrie.AND.tcom='FA'
    IF p > 0
     ASIZE(regh,p+1)
    ENDIF
    p = p + 1
    IF sact=0.OR.sact=.01
     regh[LEN(regh)] = RECNO()
    ELSE
     ok=1
     EXIT
    ENDIF
    SKIP
   ENDDO
   IF ok=0
    FOR con = 1 TO LEN(regh)
     GO regh[con]
     RECLOCK(1)
     REPLACE canc WITH .T.
     UNLOCK
    NEXT
    GO vrecno
    RECLOCK(1)
    REPLACE canc WITH .T.
    UNLOCK
   ELSE
    GO vrecno
   ENDIF
  ELSE
   GO vrecno
  ENDIF
 ENDIF
 SKIP
ENDDO
SUM PAGA TO PIPI FOR CANC.AND.!BAJA.AND.!DELETED()
IF STR(pipi,20,2) # STR(0,20,2)
 CARTEL("LO CANCELADO NO SUMA CERO, VERIFIQUE....NO LIMPIE [ OK ]",12,1,'N',"*"+color3)
ENDIF
CLOSE ALL
DELETE FILE CCCLIE05.NTX
CARTEL("PROCESO TERMINADO  [  OK  ]",18,1,'N',color3)
RESTSCREEN(0,0,24,79,pan_ant)
SETCOLOR(col_ant)
RETURN
