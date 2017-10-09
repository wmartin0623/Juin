*MOVIMIENTO CLIENTES
posi = opc1+5
PRIVATE vm
DECLARE salir[1]
DO WHILE .T.
 opcion = 1
 vm=PADR(op,4,'0')
 Xsub_pan1 = SAVESCREEN(posi,30,posi+5,49)
 SOMBRA(posi,30,posi+4,48,'V2',1,'',INVER,'N+')
 SETCOLOR(color2)
 *-----------------------------------------
 DECLARE menu1[3],vmenss[3],fi_co[3]
 Menu1[1]="`1-Facturar "+CHR(26)
 Menu1[2]="`2-Pagos    "+CHR(26)
 Menu1[3]="`3-Cierre Z (LOCAL)"
 vmenss[1]='                 Permite Ingresar FACTURAS, N.CREDITO o N.DEBITO                 '
 vmenss[2]='                Permite Consultar FACTURAS, N.CREDITO o N.DEBITO                 '
 vmenss[3]='             Permite Hacer el Cierre Z Diario del Controlador FISCAL             '
 carga1(fi_co,'08,31~09,31~10,31')
 salir[1]=13
 opcion=PROMPT(menu1,fi_co,'I',opcion,salir,menu&vm,'N+/W',vmenss,24)
 vm=PADR(op+STR(opcion,1),4,'0')        &&Activa menu de Habilitacion
 *-----------------------------------------
 DO CASE
  CASE opcion = 0
   RESTSCREEN(posi,30,posi+5,49,Xsub_pan1)
   RETURN
  CASE opcion = 1
  *FACTURACION
   ypos=opcion+6
   DO WHILE .T.
    opc2 = 1
    Xsub_pan2 = SAVESCREEN(YPOS,41,YPOS+6,55)
    SOMBRA(YPOS,41,YPOS+5,54,'V1',1,'',INVER,'N+')
    SETCOLOR(color2)
    *-----------------------------------------
    DECLARE menu1[4],vmenss[4],fi_co[4]
    CARGA1(menu1,'`1-Ingresar ~`2-Consultar~`3-Actualiza~`4-Compras/Cl')    &&`7-N.C. Grupo')
    vmenss[1]='                 Permite Ingresar FACTURAS, N.CREDITO o N.DEBITO                 '
    vmenss[2]='                Permite Consultar FACTURAS, N.CREDITO o N.DEBITO                 '
    vmenss[3]='        Permite Actualizar COMPROBANTES CANCELADOS para Depurar Archivos         '
    vmenss[4]='  Permite Consultar las Compras realizadas por un CLiente en un Rango de Fechas  '
    carga1(fi_co,'08,42~09,42~10,42~11,42')
    salir[1]=13
    opc2=PROMPT(menu1,fi_co,'I',opc2,salir,menu&vm,'N+/W',vmenss,24)
    *-----------------------------------------
    IF opc2=0
     RESTSCREEN(YPOS,41,YPOS+6,55,Xsub_pan2)
     EXIT
    ENDIF
    CLOSE ALL
    DO CASE
     CASE opc2=1
      *Ingreso de Movimientos a la Cuenta Corriente
      DO movfaccl
     CASE opc2=2
      DO movcofac
     CASE opc2=3
      *Actualizacion de comprobantes cancelados
       DO MOVACTCL
     CASE opc2=4
      *Consulta de Compras Realizadas por un Cliente
      DO COMPRA_CL
    ENDCASE
    RESTSCREEN(YPOS,41,YPOS+6,55,Xsub_pan2)
   ENDDO
   RESTSCREEN(YPOS,41,YPOS+6,55,Xsub_pan2)
  CASE opcion=2
   *Pagos
   ypos=opcion+6
   DO WHILE .T.
    opc2 = 1
    Xsub_pan2 = SAVESCREEN(YPOS,41,YPOS+6,55)
    SOMBRA(YPOS,41,YPOS+5,54,'V1',1,'',INVER,'N+')
    SETCOLOR(color2)
    *-----------------------------------------
    DECLARE menu1[4],vmenss[4],fi_co[4]
    CARGA1(menu1,'`1-Ingresar ~`2-Consultar~`3-Baja    ~`4-Asignar ')
    vmenss[1]='                  Permite Ingresar PAGOS DE CUENTAS CORRIENTES                   '
    vmenss[2]='                  Permite Consultar PAGOS de CUENTAS CORRIENTES                  '
    vmenss[3]='                      Permite Dar de Baja PAGOS de Clientes                      '
    vmenss[4]='             Permite Asignar Pagos Cargados sin Afectar Comprobantes             '
    carga1(fi_co,'09,42~10,42~11,42~12,42')
    salir[1]=13
    opc2=PROMPT(menu1,fi_co,'I',opc2,salir,menu&vm,'N+/W',vmenss,24)
    *-----------------------------------------
    IF opc2=0
     RESTSCREEN(YPOS,41,YPOS+5,55,Xsub_pan2)
     EXIT
    ENDIF
    CLOSE ALL
    DO CASE
     CASE opc2=1
      DO movpacli
     CASE opc2=2
      DO movcopa1
     CASE opc2=3
      DO movbajpc
     CASE opc2=4
      DO movasicl
    ENDCASE
    RESTSCREEN(YPOS,41,YPOS+6,55,Xsub_pan2)
   ENDDO
   RESTSCREEN(YPOS,41,YPOS+6,55,Xsub_pan2)

  CASE opcion=3
   *Cierre Z
    CIERRE_Z(p_Com)

 ENDCASE
 RESTSCREEN(posi,30,posi+5,49,Xsub_pan1)
ENDDO

PROCEDURE COMPRA_CL
PRIVATE Ant_Pan,Ant_Col,bMes,bAno,vClie,Nom_Clie
Ant_Pan=SAVESCREEN(0,0,24,79)
Ant_Col=SETCOLOR()
bMes=SPACE(2)
bAno=SPACE(4)
vClie=SPACE(6)
Nom_Clie=SPACE(30)
*clientes
AREA()
OPEN_DBF("MAECLIEN","CLIENTES","C","MAECLIEN","clie","NOMCLIEN","nomb")
DO WHILE .T.
 *Solicitud de MES Y A¤o de la Consulta
 SOMBRA(7,23,21,58,"H5",1,'',colo3)
 CARTEL('MES A TRATAR   <mm> : [  ]',9,1,'E',colo3)
 CARTEL('A¥0 A TRATAR <aaaa> : [    ]',14,1,'E',colo3)
 SETCOLOR(color3)
 @8,32 SAY "CONSULTA VENTAS"
 SETCOLOR(colo3)
 @10,51 GET bmes PICTURE '@K 99' VALID VAL(bmes)>=1 .AND. VAL(bmes)<=12
 @15,50 GET bano PICTURE '@K 9999' VALID VAL(bano)>=1994 .AND. VAL(bano)<=2100
 READ
 IF LASTKEY()=27
  RESTSCREEN(0,0,24,79,Ant_PAn)
  SETCOLOR(Ant_Col)
  EXIT
 ENDIF
 bmes=CERO(VAL(bmes),2)
 bano=CERO(VAL(bano),4)
 vPer=CHR(VAL(bmes)+64)+bano
 IF .NOT.FILE("FCC&vper..DBF")
  CARTEL("PERIODO SIN REGISTRACION DE VENTAS  [ OK ]",18,1,'N',color3)
  LOOP
 ENDIF

 *FACTURAS
 AREA()
 IF USAR("FCC&vper","FACTURA","E")
  IF !FILE('FCC&vper..NTX')
   INDEX ON clie+tcom+ltra+srie+nume TO FCC&vPer
  ENDIF
  SET INDEX TO FCC&vPer
  SET RELATION TO clie INTO CLIENTES
 ELSE
  QUIT
 ENDIF
 *Solicitud del dia comienzo el final
 ult_dia=ULTIMO(VAL(bmes),VAL(bano))
 Dia_Des="01"
 Dia_Has=CERO(Ult_Dia,2)
 DO WHILE .T.
  SELECT FACTURA
  SETCOLOR(colo3)
  SOMBRA(11,30,14,46,'H1',1,'',colo3)
  @12,31 SAY 'Desde D¡a : '
  @13,31 SAY 'Hasta D¡a : '
  @12,43 GET dia_des PICTURE '@K 99' VALID VAL(dia_des)>=1.AND.VAL(dia_des)<=ult_dia
  @13,43 GET dia_has PICTURE '@K 99' VALID VAL(dia_has)>=VAL(dia_des).AND.VAL(dia_has)<=ult_dia
  READ
  IF LASTKEY()=27
   SELECT FACTURA
   CLOSE
   RESTSCREEN(0,0,24,79,Ant_Pan)
   SETCOLOR(Ant_Col)
   EXIT
  ENDIF
  dia_des=CERO(VAL(dia_des),2)
  dia_has=CERO(VAL(dia_has),2)
  SELECT FACTURA

  Fe_Des=CTOD(dia_des+'/'+bmes+'/'+bano)
  Fe_Has=CTOD(dia_has+'/'+bmes+'/'+bano)

  *Solicita Cliente
  SETCOLOR(colo3)
  cam="clie~nomb"
  vClie=SPACE(6)
  @18,24 SAY "Cliente: " GET vClie PICTURE "@K 999999" VALID EMPTY(vClie).OR.VALIDA(@vClie,@Nom_CLie,"CLIENTES",cam,6,30,"CLIE","NOMB")
  READ
  IF LASTKEY()=27
   SELECT FACTURA
   CLOSE
   EXIT
  ENDIF
  IF EMPTY(vClie)
   vClie="000000"
   @18,33 SAY vClie
   Nom_Clie="CONSUMIDOR OCASINAL"
  ENDIF
  @19,25 SAY Nom_Clie COLOR colo12

  bien = CARTEL("DATOS CORRECTOS [ SI ] [ NO ]",20,1,'N',colo3)

  IF bien .AND. LASTKEY() # 27
   SELECT FACTURA
   GO TOP
   vCount=0
   bBloq:={|| vCount := vCount + paga}
   bCondi:={|| clie=vClie.AND.fech>=Fe_Des.AND.fech<=Fe_Has.AND.!baja}
   bWhile:={|| clie=vClie}
   DBEVAL(bBloq,bCondi)    &&    ,bWhile)
   @20,27 SAY "Compra Realizada: "+STR(vCount,12,2) COLOR colo12
   SETCOLOR(color3)
   @21,24 SAY " ±Esc±-Sale  ±ÄÙ±-Otra Consulta"
   SETCOLOR(colo3)
   DIST_COLOR(21,26,'~W+/R~Esc')
   DIST_COLOR(21,38,'~W+/R~ÄÙ')
   tecla=65
   SET CURSOR OFF
   DO WHILE tecla#13 .AND. tecla#27
    tecla=INKEY(0)
   ENDDO
   IF tecla=27
    RESTSCREEN(0,0,24,79,Ant_Pan)
    SELECT FACTURA
    CLOSE
    EXIT
   ENDIF
  ENDIF
 ENDDO
ENDDO
CLOSE ALL
RESTSCREEN(0,0,24,79,Ant_Pan)
SETCOLOR(Ant_Col)
RETURN

PROCEDURE MOVCLFAC
@ 1,0 SAY "ÚClienteÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂIVAÄÂTpÄ¿ ÚComprobanteÄÄÄÄÄÄÄÄÄÄÄÄÄ¿ÚN§Remito¿"
@ 2,0 SAY "³                                ³    ³   ³ ÃTipoÂÄN£meroÄÂFechaÄÄÄÄÄ´ÃÄÄÄÄÄÄÄÄ´"
@ 3,0 SAY "³F.Pago:                         ÀÄÄÄÄÁÄÄÄ´ ³    ³        ³          ³³        ³"
@ 4,0 SAY "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ ÀÄÄÄÄÁÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÁÁÄÄÄÄÄÄÄÄÙ"
@05,0 SAY "ÚDetalle Art¡culoÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
@06,0 SAY "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´"
@07,0 SAY "³                                                                              ³"
@08,0 SAY "³                                                                              ³"
@09,0 SAY "³                                                                              ³"
@10,0 SAY "³                                                                              ³"
@11,0 SAY "³                                                                              ³"
@12,0 SAY "³                                                                              ³"
@13,0 SAY "³                                                                              ³"
@14,0 SAY "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
@15,0 SAY "ÚRubroÂMarcaÄÄÄÄÄÄÄÄÄÄÂMedidasÄÄÄÄÄÄÄÄÂUnidadÄÄÄÄÂCantidadÂP.Unit.ÄÄ¿ÚTotal ($)¿"
@16,0 SAY "³     ³               ³               ³          ³        ³         ³³         ³"
@17,0 SAY "ÀÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÙÀÄÄÄÄÄÄÄÄÄÙ"
@18,0 SAY "ÚÄÄSubTotalÄÄÄÂÄDescuentoÄÄÂÄNeto Grav.ÄÂÄI.V.A.ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄA PagarÄÄÄÄ¿"
@19,0 SAY "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÅÄIns.      ÄÂNo Ins.     ÅÄÄÄÄÄÄÄÄÄÄÄÄ´"
@20,0 SAY "³             ³            ³            ³            ³            ³            ³"
@21,0 SAY "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÅSaldo Clie.ÄÅÄÄÄÄÄÄÄÄÄÄÄÄÅValor DolarÄÅÄÄÄÄÄÄÄÄÄÄÄÄÙ"
@22,0 SAY "³             ³            ³            ³            ³            ³             "
@23,0 SAY "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÙ             "
RETURN

PROCEDURE MOVCONT
SOMBRA(17,13,20,64,"H2",1,'',color3)
@ 17,13 SAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄP A G O  de  C O N T A D OÄÄÄÄÄÄÄÄÄÄ¿"
@ 18,13 SAY "ÃEfectivoÄÄÄÄÄÄÄÄÅChequesÄÄÄÄÄÄÄÄÄÅDolaresÄÄÄÄÄÄÄÄÄ´"
@ 19,13 SAY "³                ³                ³                ³"
@ 20,13 SAY "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
RETURN

PROCEDURE MOVCPACL
@ 1,0 SAY "                                                                 ÉN§OficialÍÍÍ»"
@ 2,0 SAY " DETALLE COMPROBANTES PAGADOS                                    º            º"
@ 3,0 SAY "ÚÄDatos del ClienteÄÄÄÄÄÄÄÄÄÄÄÄÂDatos del Comprobante PAGADOÄÄÄÄÄÊÍÍÍÍÍÍÍÍÍÍÍÍ¹"
@ 4,0 SAY "³Codigo³Apellido y Nombre      ³Tp³N£mero       ³ Fecha    ³Imp.PAGADO  ³      "
@ 5,0 SAY "ÃÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄ´      "
@ 6,0 SAY "³      ³                       ³  ³             ³          ³            ³      "
@ 7,0 SAY "³      ³                       ³  ³             ³          ³            ³      "
@ 8,0 SAY "³      ³                       ³  ³             ³          ³            ³      "
@ 9,0 SAY "³      ³                       ³  ³             ³          ³            ³      "
@10,0 SAY "³      ³                       ³  ³             ³          ³            ³      "
@11,0 SAY "ÀÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÙ      "
@12,0 SAY "               DETALLES DEL PAGO                      Total:                   "
@13,0 SAY "     ÚDatos del PagoÄÄÄÄÄÄÄÄÄÄÄÄÂDatos del BancoÄÄÄÄÄÄÄÄÄÄÄÂImporteÄÄÄÄÄÄÄ¿    "
@14,0 SAY "     ÃN§InternoÄÂFechaÄÄÄÄÄÂTipoÅCod.ÂN§Bol/CheqÂFechaÄÄÄÄÄÅPagadoÄÄÄÄÄÄÄÄ´    "
@15,0 SAY "     ³          ³          ³    ³    ³          ³          ³              ³    "
@16,0 SAY "     ³          ³          ³    ³    ³          ³          ³              ³    "
@17,0 SAY "     ³          ³          ³    ³    ³          ³          ³              ³    "
@18,0 SAY "     ³          ³          ³    ³    ³          ³          ³              ³    "
@19,0 SAY "     ³          ³          ³    ³    ³          ³          ³              ³    "
@20,0 SAY "     ³          ³          ³    ³    ³          ³          ³              ³    "
@21,0 SAY "     ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÁÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ    "
@22,0 SAY "                                                      Total:                   "
RETURN

PROCEDURE MOVESCL
SOMBRA(7,17,18,58,"H2",.1,'',color7)
@ 7,17 SAY "ÚÄÄCLIENTEÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
@ 8,17 SAY "ÃCodigoÄÂÄApellido y NombreÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´"
@ 9,17 SAY "³       ³                                ³"
@10,17 SAY "ÀÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
@11,17 SAY "ÚÄÄZona VentaÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
@12,17 SAY "ÃCodigoÄÂÄDescripcionÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´"
@13,17 SAY "³       ³                                ³"
@14,17 SAY "ÀÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
@15,17 SAY "ÚÄÄCOMPROBANTEÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
@16,17 SAY "ÃTpÄÂN£meroÄÄÂÄFechaÄÄÄÄÄÄÂÄImporteÄÄÄÄÄÄ´"
@17,17 SAY "³   ³        ³            ³              ³"
@18,17 SAY "ÀÄÄÄÁÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
SETCOLOR(color3)
@20,20 SAY " ±Esc±-Sale  ±ÄÙ±-Ingresa "
SETCOLOR(color7)
DIST_COLOR(20,22,'~W+/R~Esc')
DIST_COLOR(20,34,'~W+/R~ÄÙ')
RETURN

PROCEDURE MOVPA1CL
@ 2,17 SAY "***** COMPROBANTES PENDIENTES *****"
@ 3,5 SAY "ÚÄClienteÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿ÉÍSaldoÍÍÍÍÍÍ»     "
@ 4,5 SAY "³                                                   ³º            º     "
@ 5,5 SAY "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙÈÍÍÍÍÍÍÍÍÍÍÍÍ¼     "
@ 6,5 SAY "ÚMca.ÂÄComprobanteÄ(-Cbte.Contado)ÄÄÄÄÄÄÂÄImportesÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
@ 7,5 SAY "ÃÄÄÄÄÅÄTipoÄÂÄN£meroÄÄÄÄÄÄÄÄÂÄFechaÄÄÄÄÄÄÅÄTotalÄÄÄÄÄÄÄÄÂÄSaldoÄÄÄÄÄÄÄÄ´"
@ 8,5 SAY "³    ³      ³               ³            ³              ³              ³"
@ 9,5 SAY "³    ³      ³               ³            ³              ³              ³"
@10,5 SAY "³    ³      ³               ³            ³              ³              ³"
@11,5 SAY "³    ³      ³               ³            ³              ³              ³"
@12,5 SAY "³    ³      ³               ³            ³              ³              ³"
@13,5 SAY "³    ³      ³               ³            ³              ³              ³"
@14,5 SAY "³    ³      ³               ³            ³              ³              ³"
@15,5 SAY "³    ³      ³               ³            ³              ³              ³"
@16,5 SAY "³    ³      ³               ³            ³              ³              ³"
@17,5 SAY "³    ³      ³               ³            ³              ³              ³"
@18,5 SAY "ÀÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄIva RI:      ÄÄRNI:      ÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
*            1     12    1234567890123   1234567890   123456789012   123456789012
@19,5 SAY "ÚÄNumero de PagosÄ¿                                ÚImp. a PAGARÄÄ¿     "
@20,5 SAY "ÃÄInternoÅÄOficial´                                ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´     "
@21,5 SAY "³        ³        ³                                ³              ³     "
@22,5 SAY "ÀFecha:ÄÄÁÄÄÄÄÄÄÄÄÙ                                ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ     "
SETCOLOR(color3)
@23,11 SAY " ±Esc±-Sale  ±ÄÙ±-Mca.Pag.  ±*±-Ing.Pag.  ±±-Mueve "
SETCOLOR(color7)
DIST_COLOR(23,13,'~W+/R~Esc')
DIST_COLOR(23,25,'~W+/R~ÄÙ')
DIST_COLOR(23,41,'~W+/R~*')
DIST_COLOR(23,56,'~W+/R~')
RETURN

PROCEDURE MOVPA2CL
@ 1,0 SAY "ÚN§ PagoÄ¿ÚClienteÄÄApellido y NombreÄÄÄÄÄÄÄÄÄÄÄÄ¿ÚSaldo Fact.ÄÂImp. PagadoÄ¿"
@ 2,0 SAY "³        ³³       ³                              ³³            ³            ³"
@ 3,0 SAY "ÀÄÄÄÄÄÄÄÄÙÀÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´ÀÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÂÄÄÄÄÄÙ"
@ 4,0 SAY "Zona Vta.:                                       ³ Dolar :          $ ³      "
@ 5,0 SAY "    ÚÄCPÄÂBancoÄÄÄÄÄÄÄÄÂPlazaÄÄÄÄÄÄÄÄÂBta./Cheq.ÂÁFechaÄÄÄÄÄÄÂÄImporteÄÄÄÄÄÄ¿"
@ 6,0 SAY "    ÃÄÄÄÄÅBanÅSiglaÄÄÄÄÅPzaÅSiglaÄÄÄÄÅN£meroÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´"
@ 7,0 SAY "    ³    ³   ³         ³   ³         ³          ³            ³              ³"
@ 8,0 SAY "    ³    ³   ³         ³   ³         ³          ³            ³              ³"
@ 9,0 SAY "    ³    ³   ³         ³   ³         ³          ³            ³              ³"
@10,0 SAY "    ³    ³   ³         ³   ³         ³          ³            ³              ³"
@11,0 SAY "    ³    ³   ³         ³   ³         ³          ³            ³              ³"
@12,0 SAY "    ³    ³   ³         ³   ³         ³          ³            ³              ³"
@13,0 SAY "    ³    ³   ³         ³   ³         ³          ³            ³              ³"
@14,0 SAY "    ³    ³   ³         ³   ³         ³          ³            ³              ³"
@15,0 SAY "    ³    ³   ³         ³   ³         ³          ³            ³              ³"
@16,0 SAY "    ÃÄÄÄÄÁÄÄÄÁÄÄÄÄÄÄÄÄÄÁÄÄÄÁÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´"
@17,0 SAY "    ³Ncta:                        Prov:                                     ³"
@18,0 SAY "    ÀTp.As:ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄObse.: ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
@19,0 SAY "    ÚÄReferencia (CP) Codigo de PagoÄÄÄÄÄCtas.: ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
@20,0 SAY "    ³   EF-Efectivo ³   RI-Ret.IVA      ³   AP-Ajuste (+)³                  ³"
@21,0 SAY "    ³   US-Dolares  ³   RB-Ret.Ing.Bru. ³   AN-Ajuste (-)³                  ³"
@22,0 SAY "    ³   CH-Cheque   ³   RG-Ret.Gananc.  ³                ³                  ³"
@23,0 SAY "    ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
SETCOLOR(color3)
@24,11 SAY " ±Esc±-Sale  ±ÄÙ±-Ingresa   ±*±-Finaliza  ±±-Mueve "
SETCOLOR(color7)
DIST_COLOR(24,13,'~W+/R~Esc')
DIST_COLOR(24,25,'~W+/R~ÄÙ')
DIST_COLOR(24,41,'~W+/R~*')
DIST_COLOR(24,56,'~W+/R~')
RETURN

PROCEDURE MOVVUEL
@ 19,13 SAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄV U E L T O  C O N T A D OÄÄÄÄÄÄÄÄÄÄ¿"
@ 20,13 SAY "ÃEfectivoÄÄÄÄÄÄÄÄÅChequesÄÄÄÄÄÄÄÄÄÅDolaresÄÄÄÄÄÄÄÄÄ´"
@ 21,13 SAY "³                ³                ³                ³"
@ 22,13 SAY "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
RETURN

PROCEDURE MOVCHFAC
SOMBRA(3,2,16,74,"H2",1,'',color7)
SETCOLOR(color9)
@ 3,2 SAY "ÚBancoÄÄÄÄÄÄÄÄÄÄÂPlazaÄÄÄÄÄÄÄÄÄÄÄÂN£meroÄÄÄÄÂÄFechaÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
@ 4,2 SAY "³Cod.³ Sigla    ³ Cod.³ Sigla    ³Tal¢n     ³ Vencimiento³  Importe     ³"
@ 5,2 SAY "ÆÍÍÍÍØÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍµ"
@ 6,2 SAY "³    ³          ³     ³          ³          ³            ³              ³"
@ 7,2 SAY "³    ³          ³     ³          ³          ³            ³              ³"
@ 8,2 SAY "³    ³          ³     ³          ³          ³            ³              ³"
@ 9,2 SAY "³    ³          ³     ³          ³          ³            ³              ³"
@10,2 SAY "³    ³          ³     ³          ³          ³            ³              ³"
@11,2 SAY "³    ³          ³     ³          ³          ³            ³              ³"
@12,2 SAY "³    ³          ³     ³          ³          ³            ³              ³"
@13,2 SAY "³    ³          ³     ³          ³          ³            ³              ³"
@14,2 SAY "ÀÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´"
@15,2 SAY "Cliente  :                                     TOTAL    ³              ³"
@16,2 SAY "Zona Vta.:                            N§Factura:         ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
RETURN

*Devuelve el nombre del mes
*Recibe el numero del mes como string
FUNCTION NOM_MES
PARAMETERS ydia
PRIVATE ymes
IF EMPTY(DTOC(ydia))
 ydia=DATE()
ENDIF
ymes=CERO(MONTH(ydia),2)
DECLARE mes1[12]
mes1[1]="Enero"
mes1[2]="Febrero"
mes1[3]="Marzo"
mes1[4]="Abril"
mes1[5]="Mayo"
mes1[6]="Junio"
mes1[7]="Julio"
mes1[8]="Agosto"
mes1[9]="Setiembre"
mes1[10]="Octubre"
mes1[11]="Noviembre"
mes1[12]="Diciembre"
RETURN SUBSTR(DTOC(ydia),1,2)+" de "+mes1[VAL(ymes)]+" de "+STR(YEAR(ydia),4)

PROCEDURE ASI_PA_CLI
 SOMBRA(3,0,22,78,"V5",1,'',color7)
 SETCOLOR(color9)
 @ 3,0 SAY "ÚClienteÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
 @ 4,0 SAY "³                                                                          ³"
 @ 5,0 SAY "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
 @ 6,0 SAY "ÚÄÂDetalle Comprobantes PendientesÄÄÄÄÄÄÄ¿ÚÄÂDetalle de Pagos a CuentaÄÄÄÄÄ¿"
 @ 7,0 SAY "ÃXÅTipo y N£meroÄÄÄÂFechaÄÄÄÂSaldoÄÄÄÄÄÄÄ´ÃXÅN£meroÄÄÂFechaÄÄÄÂImporteÄÄÄÄÄ´"
 @ 8,0 SAY "³ ³                ³        ³            ³³ ³        ³        ³            ³"
 @ 9,0 SAY "³ ³                ³        ³            ³³ ³        ³        ³            ³"
 @10,0 SAY "³ ³                ³        ³            ³³ ³        ³        ³            ³"
 @11,0 SAY "³ ³                ³        ³            ³³ ³        ³        ³            ³"
 @12,0 SAY "³ ³                ³        ³            ³³ ³        ³        ³            ³"
 @13,0 SAY "³ ³                ³        ³            ³³ ³        ³        ³            ³"
 @14,0 SAY "³ ³                ³        ³            ³³ ³        ³        ³            ³"
 @15,0 SAY "³ ³                ³        ³            ³³ ³        ³        ³            ³"
 @16,0 SAY "³ ³                ³        ³            ³³ ³        ³        ³            ³"
 @17,0 SAY "³ ³                ³        ³            ³³ ³        ³        ³            ³"
 @18,0 SAY "³ ³                ³        ³            ³³ ³        ³        ³            ³"
 @19,0 SAY "ÀÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÙÀÄÁÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÙ"
 @20,0 SAY "            Total en Cbtes.:                   Total en Pagos:              "
 @21,0 SAY "              ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
 @22,0 SAY "              ³                                                            ³"
 @23,0 SAY "              ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
RETURN

*Control de BANCOS
FUNCTION VAL_BAN
PARAMETERS ban,nom
IF LASTKEY() = 27
 KEYBOARD CHR(27)
 RETURN .T.
ENDIF
PRIVATE colo_a
antsel=ALLTRIM(STR(SELECT()))
colo_a=SETCOLOR()
SELECT BANCOS
SET ORDER TO 1
ban = CERO(VAL(ban),2)
SEEK ban
IF .NOT. FOUND()
 *Ayuda de bancos
 archivo='BANCOS'
 campi='banc~nomb+" "+sigl'
 ntx1=1
 ntx2=2
 l1=2
 l2=20
 camp1='BANC'
 IF ayudo(campi,ntx1,ntx2,l1,l2,camp1,archivo) = .T.
  ban=banc
  nom=sigl
  SELECT &antsel
  RETURN .T.
 ELSE
  SELECT &antsel
  RETURN .F.
 ENDIF
ELSE
 ban=banc
 nom=sigl
 SELECT &antsel
 RETURN .T.
ENDIF
RETURN .T.

*Control de PLAZAS
FUNCTION VAL_PZA
PARAMETERS pza,nom
IF LASTKEY() = 27
 KEYBOARD CHR(27)
 RETURN .T.
ENDIF
PRIVATE colo_a,antsel,ok2
antsel=ALLTRIM(STR(SELECT()))
colo_a=SETCOLOR()
ok2=0
IF SELECT("PLAZA")=0
 *Plazas
 AREA()
 OPEN_DBF("MAEPLAZA","PLAZA","C","MAEPLAZA","plaz","NOMPLAZA","nomb")
 ok2=1
ENDIF
SELECT PLAZA
SET ORDER TO 1
pza = CERO(VAL(pza),3)
SEEK pza
IF .NOT. FOUND()
 *Ayuda de Plazas
 archivo='PLAZA'
 campi='plaz~nomb+" "+sigl'
 ntx1=1
 ntx2=2
 l1=3
 l2=25
 camp1='plaz'
 IF ayudo(campi,ntx1,ntx2,l1,l2,camp1,archivo) = .T.
  pza=plaz
  nom=sigl
  IF ok2=1
   CLOSE
  ENDIF
  SELECT &antsel
  RETURN .T.
 ELSE
  IF ok2=1
   CLOSE
  ENDIF
  SELECT &antsel
  RETURN .F.
 ENDIF
ENDIF
pza=plaz
nom=sigl
IF ok2=1
 CLOSE
ENDIF
SELECT &antsel
RETURN .T.

FUNCTION CIERRE_Z(Com)
PUBLIC Handler, Port, n, Se
PRIVATE Pan_Cierre
Pan_Cierre = SAVESCREEN(,,,,)
   SETCOLOR(colo3)
   SOMBRA(1,2,22,77,"H2",1,'',colo3)
   SETCOLOR(color3)
   @  2,3 SAY CENTRAR("CIERRE Z DE CAJA DIARIA "+ DToC(Date()),3,76)
   if (DUDA("¨Est  seguro de Hacer Cirre 'Z'?", 2))
      CARTEL("COMUNICANDOSE CON EL CONTROLADOR FISCAL. Espere un momento...",12,1,'E',colo3)
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

*         CargarTexto()

*        s:= "]" + se + "3" + se + " "
*        enviar(s)
*        s:= "]" + se + "4" + se + " "
*        enviar(s)
*        s:= "]" + se + "11" + se + " "
*        enviar(s)
*        s:= "]" + se + "12" + se + " "
*        enviar(s)
*        s:= "]" + se + "13" + se + " "
*        enviar(s)
*        s:= "]" + se + "14" + se + " "
*        enviar(s)

         Enviar("9" + se + "Z")

         ClosePort(handler)
      ENDIF
   ENDIF
   RESTSCREEN(,,,,Pan_Cierre)
RETURN
