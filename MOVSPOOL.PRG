*----------------------------------------------------------------------------
FUNCTION SpoolerFA(fDiscri)
   SET PRINTER TO "\&carpfis.\&tempofi3."
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
  s = "b" + Se + vNom + Se + vCui + Se + "I" + Se + "C" + Se + vDom
  ? s

  * Se carga en la memoria de trabajo de la impresora fiscal al relacion
  * del documento a emitir con un Remito o una factura, segun corresponda
  * ( opcional en Documentos Fiscales, obligatorio en Notas de Credito )
  * Genera comando: SetEmbarkNumber
  * ---------------------------------------------------------------------
  *Habilitar para cuando se utiliza un remito para el despacho
  IF !EMPTY(vRem)
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
  ENDIF

  * Cierre del Documento
  * --------------------

  * Documento Fiscal
  * Genera comando: CloseFiscalReceipt
  * ----------------------------------
  s = "E"

  ? s
  * Nota de Credito
  * Genera comando: CloseDNFH
  * -------------------------
  * s = CHR(129)

  ? s
RETURN 0

FUNCTION SpoolerNC
  SET PRINTER TO "\&carpfis.\&tempofi3."
  SET CONSOLE OFF
  SET DEVICE TO PRINTER
  SET PRINTER ON
  se:= CHR(28)
  s:= "D" + se + " " + se + "0.00" + se + "C" + se + "0"
  ? s
  s:= "E"
  ? s
  * Genera comando: SetCustomerData
  s = "b" + Se + vNom + Se + vCui + Se + "I" + Se + "C" + Se + vDom
  ? s

  s:= chr(147) + se + "1" + se + f_trans(Transform(VAL(fact_nc), "######"))
  ? s

  * Comando de apertura del Documento
  * ---------------------------------
  * Genera comando:  OpenDNFH
  IF vCiv="1" .OR. vCiv="2"
    * Nota de Credito "A"
    s = CHR(128) + Se + "R" + Se + "T" + Se + ""
  ELSE
    * Nota de Credito "B"
    * Genera comando: OpenFiscalReceipt
    s = CHR(128) + Se + "S" + Se + "T" + Se + ""
  ENDIF
  ? s

  * Impresion Texto Fiscal - solamente previo al item
  * Genera comando: PrintFiscalText
  * -------------------------------------------------
  s = "A" + Se + SUBSTR(MItems[1],1,30) + Se + "0"
  ? s

  ltx:= "S/FC " + ltra_nc + " " + srie_nc + "-" + fact_nc
  s:= "B" + se + ltx + se + "1.0" + se + StrZero(tgral, 9, 2) + se + ;
     StrZero(iva1, 5, 2) + se + "M" + se + "0.0" + se + "0" + se + "T"
  ? s

  * Cierre del Documento
  * --------------------
  * Nota de Credito
  * Genera comando: CloseDNFH
  * -------------------------
  s = CHR(129)

  ? s
RETURN 0

FUNCTION SpoolerND
  SET PRINTER TO "\&carpfis.\&tempofi3."
  SET CONSOLE OFF
  SET DEVICE TO PRINTER
  SET PRINTER ON
  se:= CHR(28)
  s:= "D" + se + " " + se + "0.00" + se + "C" + se + "0"
  ? s
  s:= "E"
  ? s
  * Genera comando: SetCustomerData
  s = "b" + Se + vNom + Se + vCui + Se + "I" + Se + "C" + Se + vDom
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
  ENDIF

  * Cierre del Documento
  * --------------------
  * Nota de Debito
  * Genera comando: CloseFiscalReceipt
  * -------------------------
  s = CHR(69)
  ? s
RETURN 0
