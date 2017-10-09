*============================================================================
* Cia. HASAR SAIC     Grupo HASAR - Dto. Software de Base
*                     por Ricardo D. Cardenes
*
* Ejemplo:            Clipper 5.02 - linkeditar con fiscal.lib
* Valido para:        Impresoras fiscales HASAR
* Modelos:            SMH/P-715F v1.00
* Consultar:          publtick.pdf - drivers.pdf
*
* Emision de:         Ticket Factura "A", y Reporte Z
*============================================================================
FUNCTION P715F
PARAMETERS Com
PUBLIC Handler, Port, n, Se

        * Separador de campos en los comandos a generar
        * ---------------------------------------------
        Se = CHR(28)

        * Apertura del puerto serie
        * -------------------------
        n = VAL(Com)

        if ( n < 0 .OR. n > 4 )
                return
        endif

        Handler = OpenPort (Com)

        * Sincronizar velocidad de la comunicacion serial. No es necesario llamar
        * a esta funcion toda vez que se arranque el programa.
        * Una vez establecida la velocidad de trabajo, utilizar la funcion
        * SetBaud para fijar la velocidad en la PC.
        * NOTA: otros modelos trabajan unicamente a 9600 baudios
        * -----------------------------------------------------------------------
        BaudRate = SearchPr (Handler)
        ? "Controlador Fiscal detectado a " + alltrim(str(BaudRate))

        * Sincronizarse con la impresora fiscal
        * -------------------------------------
        InitFiscal (Handler)

        * Si hay un documento abierto se lo cancela. Si no se pudo cancelar,
        * se intenta cerrarlo.
        * ------------------------------------------------------------------
        CancelDoc ()
        CloseFiscal ()

        * Se carga en la memoria de trabajo de la impresora fiscal el texto de
        * las lineas de fantasia, encabezamiento y pie del documento.
        * --------------------------------------------------------------------
        CargarTexto ()

    * Emision del Documento
        * ---------------------
    ImprimirDocumento ()

        * Reporte de acumulados
        * ---------------------
        *ReporteXZ ()

        * Cierre del Puerto serie
        * -----------------------
        ClosePort (Handler)


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
        for i=1 to 2
                linea = alltrim(str(i))
                s = "]" + Se + linea + Se + "Linea " + linea + " Fantasia..."
                Enviar (s)
        next

        * Carga lineas de encabezado
        * Genera comando: SetHeaderTrailer
        * --------------------------------
        for i=1 to 10
                linea = alltrim(str(i))
                s = "]" + Se + linea + Se + "Linea " + linea + " Header..."
                Enviar (s)
        next

        * Carga lineas de pie
        * Genera comando: SetHeaderTrailer
        * --------------------------------
        for i=11 to 20
                linea = alltrim(str(i))
                s = "]" + Se + linea + Se + "Linea " + linea + " Trailer..."
                Enviar (s)
        next
        wait "103"

RETURN 0

*----------------------------------------------------------------------------
* * Rutina de impresion del Documento.
* * El Documento a emitir dependera del comando de apertura. El comando de
* * cierre debera ser coherente al de apertura.
* * El cuerpo es valido cualquiera sea el Documento a emitir ( Fiscal o Ticket
* * Nota de Credito ).
* *----------------------------------------------------------------------------
 FUNCTION ImprimirDocumento

         * Se carga en la memoria de trabajo de la impresora fiscal un codigo de
         * barras a imprimir ( opcional ).
         * Genera comando: BarCode
         * ---------------------------------------------------------------------
         s = "Z" + Se + "1" + Se + "779123456789" + Se + "N" + Se + "G"
         Enviar (s)

         * Se cargan en la memoria de trabajo de la impresora fiscal, los datos
         * del comprador ( opcional en Documentos Fiscales, obligatorio en Notas
         * de Credito ).
         * Genera comando: SetCustomerData
         * ---------------------------------------------------------------------
         s = "b" + Se + "Razon Social..." + Se + "99999999995" + Se + "I" + Se + "C" + Se + "Domicilio..."
         Enviar (s)

         * Se carga en la memoria de trabajo de la impresora fiscal al relacion
         * del documento a emitir con un Remito o una factura, segun corresponda
         * ( opcional en Documentos Fiscales, obligatorio en Notas de Credito )
         * Genera comando: SetEmbarkNumber
         * ---------------------------------------------------------------------
         s = CHR(147) + Se + "1" + Se + "9998-00000123"
         Enviar (s)

         * Comando de apertura del Documento
         * ---------------------------------
         * Ticket Factura "A"
         * Genera comando: OpenFiscalReceipt
         s = "@" + Se + "A" + Se + "T"

         * Ticket Nota de Credito "A"
         * Genera comando:  OpenDNFH
         * s = CHR(128) + Se + "R" + Se + "T"
         Enviar (s)

         * Impresion Texto Fiscal - solamente previo al item
         * Genera comando: PrintFiscalText
         * -------------------------------------------------
         s = "A" + Se + "Texto Fiscal..." + Se + "0"
         Enviar(s)

         * Impresion de item
         * Genera comando: PrintLineitem
         * -----------------------------
         s = "B" + Se + "Articulo 1" + Se + "2.0" + Se + "10.0" + Se + "21.0" + Se + "M" + Se + "0.0" + Se + "0" + Se + "T"
         Enviar (s)

         * Descuento sobre ultima venta
         * Genera comando: LastItemDiscount
         * --------------------------------
         s = "U" + Se + "Oferta Ult. Venta..." + Se + "1.0" + Se + "m" + Se + "0" + Se + "T"
         Enviar(s)

         * Bonificacion a una alicuota de IVA
         * Genera comando: ReturnRecharge
         * ----------------------------------
         s = "m" + Se + "Bonif Iva21..." + Se + "1.0" + Se + "21.00" + Se + "m" + Se + "0.0" + Se + "0" + Se + "T" + Se + "B"
         Enviar(s)

         * Recargo General
         * Genera comando: GeneralDiscount
         * -------------------------------
         s = "T" + Se + "Financiero..." + Se + "10.0" + Se + "M" + Se + "0" + Se + "T"
         Enviar(s)

         * Percepciones a aplicar
         * Genera comando: Perception
         * --------------------------
         s = "`" + Se + "21.0" + Se + "Percep IVA 21..." + Se + "5.00"
         Enviar (s)
         s = "`" + Se + "**.**" + Se + "Percep Gral..." + Se + "5.00"
         Enviar (s)

         * Impresion del pago
         * Genera comando: TotalTender
         * ---------------------------
         s = "D" + Se + "Pago..." + Se + "10.0" + Se + "T" + Se + "0"
         Enviar(s)

         * Cierre del Documento
         * --------------------

         * Documento Fiscal
         * Genera comando: CloseFiscalReceipt
         * ----------------------------------
         s = "E"

         * Nota de Credito
         * Genera comando: CloseDNFH
         * -------------------------
         * s = CHR(129)

         Enviar (s)

 RETURN 0
*
* *----------------------------------------------------------------------------
* * Rutina que emite el Cierre Diario "X" o "Z".
* * Genera el comando: DailyClose
* *----------------------------------------------------------------------------
* FUNCTION ReporteXZ
*
*         * Cierre X - Reporte por cambio de turno
*         * --------------------------------------
*         Enviar ("9" + Se + "X")
*
*         * Cierre Z - Acumulados de la jornada
*         * -----------------------------------
*         * Enviar ("9" + Se + "Z")
*
* return 0
*
* *----------------------------------------------------------------------------
* * Rutina para enviar un comando a la impresora fiscal y atrapar la respuesta.
* * La inteligencia para el tratamiento de errores es responsabilidad del de-
* * sarrollador de software.
* *----------------------------------------------------------------------------
 FUNCTION Enviar
 PARAMETERS String
 PRIVATE Result, StatPrn

 ? "Enviando: " + String

 * Si la funcion MandaPaq() retorna un valor menor a cero, se retorna indi-
 * cando Error.
 * Consultar: drivers.pdf para el tratamiento de estos errores
 * ------------------------------------------------------------------------
 StatPrn = MandaPaq(Handler, String)
 * ? " StatPrn = " + alltrim (str(StatPrn))

 IF ( StatPrn < 0 )
         ? "Error enviando el comando"
         RETURN -1
 ENDIF

 * Se obtiene la respuesta de la impresora fiscal
 * ----------------------------------------------
 Result = Respuesta(Handler)
 ? "Recibido: " + Result

 * Tratamiento de la respuesta recibida
 * ------------------------------------
 GetErrors(Result)
 ? " "

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
  OffsetSep = AT ( CHR(28), Resp )

  * Convierte en hexa el campo de status de impresora
  * -------------------------------------------------
  PrinterStatus = HexaToInt (SUBSTR ( Resp, Origen, OffsetSep - 1))

  IF PrinterStatus < 0
          RETURN -1
  ENDIF

  Origen = OffsetSep

  * Analiza los bits comenzando del menos significativo
  * ---------------------------------------------------
  FOR i = 1 TO 16
          IF ( INT (PrinterStatus % 2) == 1 )
                  IF ( LEN (PrinterErrors[i]) > 0 )
                          ? "Status Impresora: " + PrinterErrors[i]
                          * Agregar la inteligencia de su tratamiento
                  ENDIF
          ENDIF
          PrinterStatus = PrinterStatus / 2
  NEXT

  OffsetSep = AT ( CHR(28), SUBSTR (Resp, Origen + 1) )

  IF OffsetSep == 0
          OffsetSep = LEN(Resp)
  ENDIF

  * Convierte en hexa el status fiscal
  * ----------------------------------
  FiscalStatus = HexaToInt (SUBSTR (Resp, Origen + 1, OffsetSep - 1))

  IF FiscalStatus < 0
          RETURN -1
  ENDIF

  * Analiza los bits comenzando del menos significativo
  * ---------------------------------------------------
  FOR i = 1 TO 16
          IF ( INT (FiscalStatus % 2) == 1 )
                  IF ( LEN (FiscalErrors[i]) > 0 )
                          ? "Status Fiscal: " + FiscalErrors[i]
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
          Value = ASC (S)

          IF ( Value >= ASC("A") .AND. Value <= ASC("F") )
                  Value = Value - ASC("A") + 10
          ELSEIF ( Value >= ASC("a") .AND. Value <= ASC("f") )
                  Value = Value - ASC("a") + 10
          ELSEIF ( Value >= ASC("0") .AND. Value <= ASC("9") )
                  Value = Value - ASC("0")
          ELSE
                  ? "HexaToInt: Digito hexadecimal incorrecto: " + HexValue
                  RETURN -1
          ENDIF

          Status = Status + Value * (16 ** ( 4 - i ))

  NEXT

  RETURN Status





*============================================================================
* Cia. HASAR SAIC     Grupo HASAR - Dto. Software de Base
*                     por Ricardo D. Cardenes
*
* Ejemplo:            Clipper 5.02 - linkeditar con fiscal.lib
* Valido para:        Impresoras fiscales HASAR
* Modelos:            SMH/P-715F v1.00
* Consultar:          publtick.pdf - drivers.pdf
*
* Emision de:         Ticket Factura "A", y Reporte Z
*============================================================================
FUNCTION FIS_FACT
PARAMETERS xCom
PUBLIC Handler, Port, n, Se
   * Separador de campos en los comandos a generar
   * ---------------------------------------------
     Se = CHR(28)

   * Apertura del puerto serie
   * -------------------------
     n = VAL(xCom)

     IF ( n < 0 .OR. n > 4 )
     ELSE
        Handler = OPENPORT( xCom)

      * Sincronizar velocidad de la comunicacion serial. No es necesario llamar
      * a esta funcion toda vez que se arranque el programa.
      * Una vez establecida la velocidad de trabajo, utilizar la funcion
      * SetBaud para fijar la velocidad en la PC.
      * NOTA: otros modelos trabajan unicamente a 9600 baudios
      * -----------------------------------------------------------------------
      SetBaud(9600)
        BaudRate = 9600  &&SearchPr(Handler)
        CARTEL("Controlador Fiscal detectado a " + alltrim(str(BaudRate)) ,12,1,'N',colo3)
      * Sincronizarse con la impresora fiscal
      * -------------------------------------
      IF (BaudRate < 1)
       Bandera = "ERROR1"
      ELSE
        NPROTOCOL(1)
        InitFiscal(Handler)

        * Si hay un documento abierto se lo cancela. Si no se pudo cancelar,
        * se intenta cerrarlo.
        * ------------------------------------------------------------------
        CancelDoc()
        CloseFiscal()

        * Se carga en la memoria de trabajo de la impresora fiscal el texto de
        * las lineas de fantasia, encabezamiento y pie del documento.
        * --------------------------------------------------------------------
        wait "aqui"
          CargarTexto()
       wait "pase"

        * Emision del Documento
        * ---------------------
          *ImprimirFa()
          ImprimirDocumento()

          nu_Fac = n_Resul

          s:= "™"
          enviar(s)

        * Reporte de acumulados
        * ---------------------
          *ReporteXZ ()

        * Cierre del Puerto serie
        * -----------------------
          ClosePort(Handler)
      ENDIF
     ENDIF
RETURN .T.



********************************
*FUNCTION ENVIAR
*
*  PARAMETERS string
*  PRIVATE result, statprn, pan_Enviar
*  pan_Enviar = SAVESCREEN(,,,,)
*  statprn:= MANDAPAQ( handler, string)
*  CARTEL(" StatPrn = " + alltrim(Str(statprn)),12,1,'E',colo3)
*  IF (statprn < 0)
*  ? "10"
*     DO WHILE (statprn = -9)
*        CARTEL("Impresora Ocupada",15,1,'E',color3)
*        string:= "¡"
*        statprn:= MANDAPAQ( handler, string)
*        IF (LASTKEY() == K_ESC)
*           CARTEL("Fin de la consulta intermedia [ OK ]",12,1,'N',color3)
*           RESTSCREEN(,,,,pan_Enviar)
*           RETURN -9
*        ENDIF
*        INKEY(1)
*     ENDDO
*     RESTSCREEN(,,,,pan_Enviar)
*     RETURN -1
*  ENDIF
*  result:= RESPUESTA( handler)
*  n_strin:= LEFT(STRING, 1)
*  wait "n_Strin= "+n_strin
*  wait result
*  IF (n_strin = "E" .OR. n_strin = "")
*     n_resul:= VAL(RIGHT( result, 8))
*  ELSE
*     n_Resul:=0
*  ENDIF
*  wait n_Resul
*  GETERRORS( result)
*  RESTSCREEN(,,,,pan_Enviar)
*  RETURN 0


FUNCTION IMPRIMIRFA
IF (n_iva = "C")
   h_cui:= vNDoc
   DO CASE
   CASE vTdoc = "LE "
      n_tip:= "0"
   CASE vTdoc = "LC "
      n_tip:= "1"
   CASE vTdoc = "DNI"
      n_tip:= "2"
   CASE vTdoc = "CE "
      n_tip:= "3"
   CASE vTdoc = "CI "
      n_tip:= "4"
   OTHERWISE
      n_tip:= " "
   ENDCASE
ELSE
   h_cui:= vCui
   n_tip:= "C"
ENDIF
IF (vFpag == 1)
   txt:= "CONTADO"
ELSE
   txt:= "CREDITO"
ENDIF
s:= "]" + se + "3" + se + "Condici¢n: " + txt
enviar(s)
wait "Uno 1"
    *   s:= "]" + se + "4" + se + "Vendedor: " + strzero(n_ven, 2) + " " ;
    *      + v_ven
    *   enviar(s)
    *   s:= "]" + se + "5" + se + "Operador: " + strzero(cod_opera, 2) + ;
    *      " " + nom_opera
    *   enviar(s)
    *   txt1:= " "
    *   txt2:= " "
    *   txt3:= " "
    *   txt4:= " "
    *   if (lis_tar = "S")
    *      if (can_tar >= 1)
    *         txt1:= "Pagado con Tarjeta " + Left(tarjdet[1], 16) + ;
    *            " Cup." + Str(tarjcup[1], 16) + " Cod." + ;
    *            Str(tarjaut[1], 16) + " $" + Str(tarjimp[1], 8, 2)
    *      endif
    *      if (can_tar >= 2)
    *         txt2:= "                   " + Left(tarjdet[2], 16) + ;
    *            " Cup." + Str(tarjcup[2], 16) + " Cod." + ;
    *            Str(tarjaut[2], 16) + " $" + Str(tarjimp[2], 8, 2)
    *      endif
    *      if (can_tar >= 3)
    *         txt3:= "                   " + Left(tarjdet[3], 16) + ;
    *            " Cup." + Str(tarjcup[3], 16) + " Cod." + ;
    *            Str(tarjaut[3], 16) + " $" + Str(tarjimp[3], 8, 2)
    *      endif
    *      if (can_tar >= 4)
    *         txt4:= "                   " + Left(tarjdet[4], 16) + ;
    *            " Cup." + Str(tarjcup[4], 16) + " Cod." + ;
    *            Str(tarjaut[4], 16) + " $" + Str(tarjimp[4], 8, 2)
    *      endif
    *   elseif (lis_che = "S")
    *      if (can_che >= 1)
    *         txt1:= "Bco." + j_bco[1] + " N§" + Str(j_nro[1], 8) + " $" ;
    *            + Str(j_imp[1], 8, 2) + " F:" + DToC(j_fec[1])
    *      endif
    *      if (can_che >= 2)
    *         txt1:= txt1 + "  Bco." + j_bco[2] + " N§" + Str(j_nro[2], ;
    *            8) + " $" + Str(j_imp[2], 8, 2) + " F:" + DToC(j_fec[2])
    *      endif
    *      if (can_che >= 3)
    *         txt2:= "Bco." + j_bco[3] + " N§" + Str(j_nro[3], 8) + " $" ;
    *            + Str(j_imp[3], 8, 2) + " F:" + DToC(j_fec[3])
    *      endif
    *      if (can_che >= 4)
    *         txt2:= txt2 + "  Bco." + j_bco[4] + " N§" + Str(j_nro[4], ;
    *            8) + " $" + Str(j_imp[4], 8, 2) + " F:" + DToC(j_fec[4])
    *      endif
    *      if (can_che >= 5)
    *         txt3:= "Bco." + j_bco[5] + " N§" + Str(j_nro[5], 8) + " $" ;
    *            + Str(j_imp[5], 8, 2) + " F:" + DToC(j_fec[5])
    *      endif
    *      if (can_che >= 6)
    *         txt3:= txt3 + "  Bco." + j_bco[6] + " N§" + Str(j_nro[6], ;
    *            8) + " $" + Str(j_imp[6], 8, 2) + " F:" + DToC(j_fec[6])
    *      endif
    *      equi_dol:= v_dolar * c_tot
    *      txt4:= " "
    *   else
    *      if (q > 1)
    *         txt1:= " "
    *      endif
    *      if (tarj = "S")
    *         txt2:= "Pagado con Tarjeta " + Left(tarjdet[1], 16) + ;
    *            " Cup." + Str(tarjcup[1], 16) + " Cod." + ;
    *            Str(tarjaut[1], 16) + " $" + Str(tarjimp[1], 8, 2)
    *      endif
    *      if (cheq = "S")
    *         txt3:= "Pagado con " + m_nota
    *      endif
    *      equi_dol:= v_dolar * c_tot
    *      txt4:= " "
    *   endif
    *   s:= "]" + se + "11" + se + txt1
    *   enviar(s)
    *   s:= "]" + se + "12" + se + txt2
    *   enviar(s)
    *   s:= "]" + se + "13" + se + txt3
    *   enviar(s)
    *   s:= "]" + se + "14" + se + txt4
    *   enviar(s)

* Enviar datos Cliente
  s:= "b" + se + vCli + " " + Trim(vNom) + se + ;
     Trim(h_cui) + se + n_iva + se + n_tip + se + Trim(vdom)
  enviar(s)
wait "Dos 2"
  if (n_iva = "I" .OR. n_iva = "N")
     s:= "@" + se + "A"
  else
     s:= "@" + se + "B"
  endif
  enviar(s)
wait "Tres 3"
for fg:= 1 to LEN(MItems)
   lc_mer:= " " + SUBSTR(MItems[fg],139,4) + "-" + SUBSTR(MItems[fg],143,5) + " "
   if (n_iva = "I" .OR. n_iva = "N")
      s:= "B" + se + lc_mer + Trim(SUBSTR(MItems[fg],1,78)) + se + ;
         SUBSTR(MItems[fg],79,8) + se + SUBSTR(MItems[fg],87,9) + ;
         se + strzero(iva, 5, 2) + se + "M" + se + "0" + se + ;
         "" + se + "B"
      enviar(s)
wait "Cuatro 4"
   else
      s:= "B" + se + lc_mer + Trim(SUBSTR(MItems[fg],1,78)) + se + ;
         SUBSTR(MItems[fg],79,8) + se + SUBSTR(MItems[fg],87,9) + ;
         se + strzero(iva, 5, 2) + se + "M" + se + "0" + se + "" + se + "T"

      enviar(s)
wait "Cinco 5"
   endif
next
if (vFpag == 1)
   if (tdes > 0)
      if (n_iva = "I" .OR. n_iva = "N")
         s:= "T" + se + "Descuento" + se + strzero(tdes, 8, 2) + ;
            se + "m" + se + "" + se + "B"
         enviar(s)
wait "seis 6"
      else
         s:= "T" + se + "Descuento" + se + strzero(tdes, 8, 2) + ;
            se + "m" + se + "" + se + "T"
         enviar(s)
wait "siete 7"
      endif
   endif
   *  if (n_rec > 0)
   *     if (n_iva = "I" .OR. n_iva = "N")
   *        s:= "T" + se + "Recargo" + se + strzero(n_rec, 8, 2) + ;
   *           se + "M" + se + "" + se + "B"
   *        enviar(s)
   *     else
   *        s:= "T" + se + "Recargo" + se + strzero(n_rec, 8, 2) + ;
   *           se + "M" + se + "" + se + "T"
   *        enviar(s)
   *     endif
   *  endif
else
* if (i_tot + s_lla > 0)
*  if (n_iva = "I" .OR. n_iva = "N")
*     s:= "T" + se + "Gtos. adm." + se + strzero(i_tot + s_lla, ;
*        8, 2) + se + "M" + se + "" + se + "B"
*     enviar(s)
*  else
*     s:= "T" + se + "Gtos. adm." + se + strzero(i_tot + s_lla, ;
*        8, 2) + se + "M" + se + "" + se + "T"
*     enviar(s)
*  endif
* endif
endif
if (vFpag != 1)
   t_pago:= 0  &&n_ant
else
   t_pago:= tgral
endif
s:= "D" + se + "Total" + se + strzero(t_pago, 12, 2) + se + "T" + ;
   se + "0"
enviar(s)
wait "ocho 8"
s:= "E"
enviar(s)
wait "nueve 9"
return 0
