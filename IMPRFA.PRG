function IMPRIMIRFA

   if (n_iva = "C")
      h_cui:= strzero(Val(right(n_doc, 8)))
      do case
      case Left(n_doc, 2) = "LE"
         n_tip:= "0"
      case Left(n_doc, 2) = "LC"
         n_tip:= "1"
      case Left(n_doc, 2) = "DN"
         n_tip:= "2"
      case Left(n_doc, 2) = "CE"
         n_tip:= "3"
      case Left(n_doc, 2) = "CI"
         n_tip:= "4"
      otherwise
         n_tip:= " "
      endcase
   else
      h_cui:= Left(n_cui, 2) + SubStr(n_cui, 4, 8) + SubStr(n_cui, ;
         13, 1)
      n_tip:= "C"
   endif

   *Definir encabezado
   ************************************************************************
   if (q == 1)
      txt:= "CONTADO"
   else
      txt:= "CREDITO"
   endif
   if (lis_che = "S")
      txt:= "PAGO CON CHEQUE DIFERIDO"
   endif
   s:= "]" + se + "3" + se + "Condici¢n: " + txt
   enviar(s)
   s:= "]" + se + "4" + se + "Vendedor: " + strzero(n_ven, 2) + " " ;
      + v_ven
   enviar(s)
   s:= "]" + se + "5" + se + "Operador: " + strzero(cod_opera, 2) + ;
      " " + nom_opera
   enviar(s)
   **********************************************************************
   *Definir Pie
   **********************************************************************
   txt1:= " "
   txt2:= " "
   txt3:= " "
   txt4:= " "
   if (lis_tar = "S")
      if (can_tar >= 1)
         txt1:= "Pagado con Tarjeta " + Left(tarjdet[1], 16) + ;
            " Cup." + Str(tarjcup[1], 16) + " Cod." + ;
            Str(tarjaut[1], 16) + " $" + Str(tarjimp[1], 8, 2)
      endif
      if (can_tar >= 2)
         txt2:= "                   " + Left(tarjdet[2], 16) + ;
            " Cup." + Str(tarjcup[2], 16) + " Cod." + ;
            Str(tarjaut[2], 16) + " $" + Str(tarjimp[2], 8, 2)
      endif
      if (can_tar >= 3)
         txt3:= "                   " + Left(tarjdet[3], 16) + ;
            " Cup." + Str(tarjcup[3], 16) + " Cod." + ;
            Str(tarjaut[3], 16) + " $" + Str(tarjimp[3], 8, 2)
      endif
      if (can_tar >= 4)
         txt4:= "                   " + Left(tarjdet[4], 16) + ;
            " Cup." + Str(tarjcup[4], 16) + " Cod." + ;
            Str(tarjaut[4], 16) + " $" + Str(tarjimp[4], 8, 2)
      endif
   elseif (lis_che = "S")
      if (can_che >= 1)
         txt1:= "Bco." + j_bco[1] + " N§" + Str(j_nro[1], 8) + " $" ;
            + Str(j_imp[1], 8, 2) + " F:" + DToC(j_fec[1])
      endif
      if (can_che >= 2)
         txt1:= txt1 + "  Bco." + j_bco[2] + " N§" + Str(j_nro[2], ;
            8) + " $" + Str(j_imp[2], 8, 2) + " F:" + DToC(j_fec[2])
      endif
      if (can_che >= 3)
         txt2:= "Bco." + j_bco[3] + " N§" + Str(j_nro[3], 8) + " $" ;
            + Str(j_imp[3], 8, 2) + " F:" + DToC(j_fec[3])
      endif
      if (can_che >= 4)
         txt2:= txt2 + "  Bco." + j_bco[4] + " N§" + Str(j_nro[4], ;
            8) + " $" + Str(j_imp[4], 8, 2) + " F:" + DToC(j_fec[4])
      endif
      if (can_che >= 5)
         txt3:= "Bco." + j_bco[5] + " N§" + Str(j_nro[5], 8) + " $" ;
            + Str(j_imp[5], 8, 2) + " F:" + DToC(j_fec[5])
      endif
      if (can_che >= 6)
         txt3:= txt3 + "  Bco." + j_bco[6] + " N§" + Str(j_nro[6], ;
            8) + " $" + Str(j_imp[6], 8, 2) + " F:" + DToC(j_fec[6])
      endif
      equi_dol:= v_dolar * c_tot
      txt4:= " "
   else
      if (q > 1)
         txt1:= " "
      endif
      if (tarj = "S")
         txt2:= "Pagado con Tarjeta " + Left(tarjdet[1], 16) + ;
            " Cup." + Str(tarjcup[1], 16) + " Cod." + ;
            Str(tarjaut[1], 16) + " $" + Str(tarjimp[1], 8, 2)
      endif
      if (cheq = "S")
         txt3:= "Pagado con " + m_nota
      endif
      equi_dol:= v_dolar * c_tot
      txt4:= " "
   endif
   s:= "]" + se + "11" + se + txt1
   enviar(s)
   s:= "]" + se + "12" + se + txt2
   enviar(s)
   s:= "]" + se + "13" + se + txt3
   enviar(s)
   s:= "]" + se + "14" + se + txt4
   enviar(s)
   **********************************************************************

   *Pasar datos Comprador
   **********************************************************************
   s:= "b" + se + strzero(n_cli, 5) + " " + Trim(n_nom) + se + ;
      Trim(h_cui) + se + n_iva + se + n_tip + se + Trim(n_dom)
   enviar(s)
   if (n_iva = "I" .OR. n_iva = "N")
      *Factura Tipo A
      s:= "@" + se + "A"
   else
      *Factura Tipo B
      s:= "@" + se + "B"
   endif
   enviar(s)
   for fg:= 1 to w

      *Texto Fiscal
      if (br5[fg] != Space(30))
         s:= "A" + se + Trim(br5[fg]) + " "
         enviar(s)
      endif
      if (br6[fg] != Space(30))
         s:= "A" + se + Trim(br6[fg]) + " "
         enviar(s)
      endif

      *Detalar Articulos Comprados
      lc_mer:= " " + ar1[fg] + "-" + ar2[fg] + "-" + ar3[fg] + " "
      if (n_iva = "I" .OR. n_iva = "N")
         s:= "B" + se + lc_mer + Trim(ar5[fg]) + se + ;
            strzero(ar7[fg], 5, 2) + se + strzero(ar9[fg], 7, 2) + ;
            se + strzero(iva[fg], 5, 2) + se + "M" + se + "0" + se + ;
            "" + se + "B"
         enviar(s)
      else
         s:= "B" + se + lc_mer + Trim(ar5[fg]) + se + ;
            strzero(ar7[fg], 5, 2) + se + strzero(ar9[fg], 7, 2) + ;
            se + strzero(iva[fg], 5, 2) + se + "M" + se + "0" + se + ;
            "" + se + "T"
         enviar(s)
      endif
   next
   if (q == 1)

      *Descuento (Resta)
      if (n_des > 0)
         if (n_iva = "I" .OR. n_iva = "N")
            s:= "T" + se + "Descuento" + se + strzero(n_des, 8, 2) + ;
               se + "m" + se + "" + se + "B"
            enviar(s)
         else
            s:= "T" + se + "Descuento" + se + strzero(n_des, 8, 2) + ;
               se + "m" + se + "" + se + "T"
            enviar(s)
         endif
      endif

      *Recargos (Suma)
      if (n_rec > 0)
         if (n_iva = "I" .OR. n_iva = "N")
            s:= "T" + se + "Recargo" + se + strzero(n_rec, 8, 2) + ;
               se + "M" + se + "" + se + "B"
            enviar(s)
         else
            s:= "T" + se + "Recargo" + se + strzero(n_rec, 8, 2) + ;
               se + "M" + se + "" + se + "T"
            enviar(s)
         endif
      endif

   elseif (i_tot + s_lla > 0)
      *Gastos Administrativos (Suma)
      if (n_iva = "I" .OR. n_iva = "N")
         s:= "T" + se + "Gtos. adm." + se + strzero(i_tot + s_lla, ;
            8, 2) + se + "M" + se + "" + se + "B"
         enviar(s)
      else
         s:= "T" + se + "Gtos. adm." + se + strzero(i_tot + s_lla, ;
            8, 2) + se + "M" + se + "" + se + "T"
         enviar(s)
      endif

   endif
   if (q != 1)
      t_pago:= n_ant
   else
      t_pago:= c_tot
   endif

   *Imprime Total Pagado
   s:= "D" + se + "Total" + se + strzero(t_pago, 8, 2) + se + "T" + ;
      se + "0"
   enviar(s)
   s:= "E"
   enviar(s)

   return 0

********************************

*DEBITOS

procedure FIS_DEBI

   se:= ""
   n:= puerto_com
   if (n < 0 .OR. n > 4)
   else
      handler:= openport(puerto_com)
      baudrate:= searchpr(handler)
      tit2("Controlador Fiscal detectado a " + alltrim(Str(baudrate)))
      if (baudrate < 1)
         bandera:= "ERROR1"
      else
         nprotocol(1)
         initfiscal(handler)
         canceltick()
         closeticke()
         imprimirde()
         q_num:= n_resul
         closeport(handler)
      endif
   endif

function IMPRIMIRDE

   if (n_iva = "C")
      h_cui:= strzero(Val(right(n_doc, 8)))
      do case
      case Left(n_doc, 2) = "LE"
         n_tip:= "0"
      case Left(n_doc, 2) = "LC"
         n_tip:= "1"
      case Left(n_doc, 2) = "DN"
         n_tip:= "2"
      case Left(n_doc, 2) = "CE"
         n_tip:= "3"
      case Left(n_doc, 2) = "CI"
         n_tip:= "4"
      otherwise
         n_tip:= " "
      endcase
   else
      h_cui:= Left(n_cui, 2) + SubStr(n_cui, 4, 8) + SubStr(n_cui, ;
         13, 1)
      n_tip:= "C"
   endif

   *Definir encabezado
   if (bb_con = "071")
      txt:= "CONTADO"
   else
      txt:= "CREDITO"
   endif
   s:= "]" + se + "3" + se + "Condici¢n: " + txt
   enviar(s)
   s:= "]" + se + "4" + se + "Vendedor: " + strzero(n_ven, 2) + " " ;
      + v_ven
   enviar(s)
   s:= "]" + se + "5" + se + "Operador: " + strzero(cod_opera, 2) + ;
      " " + nom_opera
   enviar(s)

   *Definir Pie
   s:= "]" + se + "11" + se + m_nota
   enviar(s)
   s:= "]" + se + "12" + se + " "
   enviar(s)
   s:= "]" + se + "13" + se + " "
   enviar(s)
   s:= "]" + se + "14" + se + " "
   enviar(s)

   *Definir Datos Comprador
   s:= "b" + se + strzero(n_cli, 5) + " " + Trim(n_nom) + se + ;
      Trim(h_cui) + se + n_iva + se + n_tip + se + Trim(n_dom)
   enviar(s)

   if (n_iva = "I" .OR. n_iva = "N")
     *Tipo Nota Debito "A"
      s:= "@" + se + "D"
   else
      *Tipo Nota Debito Tipo B ¢ C
      s:= "@" + se + "E"
   endif
   enviar(s)

   if (bb_con = "070")
      ltx:= "D‚bito a la cuota N§ " + strzero(n_cuo, 2) + ;
         " de factura N§" + f_trans(Transform(n_fac, "######"))
   else
      ltx:= "D‚bito contado sobre la factura N§" + ;
         f_trans(Transform(n_fac, "######"))
   endif

   *Muestra Detalle de la Nota de Debito
   s:= "B" + se + ltx + se + "1" + se + strzero(c_lic, 8, 2) + se + ;
      strzero(i_va1, 5, 2) + se + "M" + se + "0" + se + "" + se + "T"
   enviar(s)

   *Manda Total de la Nota de debito
   if (bb_con = "070")
      s:= "D" + se + "Total" + se + "0" + se + "T" + se + "0"
      enviar(s)
   else
      s:= "D" + se + "Total" + se + strzero(c_lic, 8, 2) + se + "T" ;
         + se + "0"
      enviar(s)
   endif

   s:= "E"
   enviar(s)
   return 0



procedure FIS_CRED

   se:= CHR(28)
   n:= puerto_com
   if (n < 0 .OR. n > 4)
   else
      handler:= openport(puerto_com)
      baudrate:= searchpr(handler)
      tit2("Controlador Fiscal detectado a " + alltrim(Str(baudrate)))
      if (baudrate < 1)
         bandera:= "ERROR1"
      else
         nprotocol(1)
         initfiscal(handler)
         canceltick()
         closeticke()
         imprimircr()
         q_num:= n_resul
         closeport(handler)
      endif
   endif

function IMPRIMIRCR

   if (n_iva = "C")
      h_cui:= strzero(Val(right(n_doc, 8)))
      do case
      case Left(n_doc, 2) = "LE"
         n_tip:= "0"
      case Left(n_doc, 2) = "LC"
         n_tip:= "1"
      case Left(n_doc, 2) = "DN"
         n_tip:= "2"
      case Left(n_doc, 2) = "CE"
         n_tip:= "3"
      case Left(n_doc, 2) = "CI"
         n_tip:= "4"
      otherwise
         n_tip:= " "
      endcase
   else
      h_cui:= Left(n_cui, 2) + SubStr(n_cui, 4, 8) + SubStr(n_cui, ;
         13, 1)
      n_tip:= "C"
   endif

   *Define Cabecera de la Nota de Credito
   if (bb_con = "061")
      txt:= "CONTADO"
   else
      txt:= "CREDITO"
   endif
   s:= "]" + se + "3" + se + "Condici¢n: " + txt
   enviar(s)
   s:= "]" + se + "4" + se + "Vendedor: " + strzero(n_ven, 2) + " " ;
      + v_ven
   enviar(s)
   s:= "]" + se + "5" + se + "Operador: " + strzero(cod_opera, 2) + ;
      " " + nom_opera
   enviar(s)

   *Define Pie de la Nota de Credito
   s:= "]" + se + "11" + se + " "
   enviar(s)
   s:= "]" + se + "12" + se + " "
   enviar(s)
   s:= "]" + se + "13" + se + " "
   enviar(s)
   s:= "]" + se + "14" + se + " "
   enviar(s)

   *Define Datos del Comprador
   s:= "b" + se + strzero(n_cli, 5) + " " + Trim(n_nom) + se + ;
      Trim(h_cui) + se + n_iva + se + n_tip + se + Trim(n_dom)
   enviar(s)
   s:= chr(147) + se + "1" + se + strzero(n_fac, 6, 0)
   enviar(s)

   *Abre Nota de Credito
   if (n_iva = "I" .OR. n_iva = "N")
      *Nota de Credito Tipo A
      s:= CHR(128) + se + "R" + se + "T" + se + ""
      enviar(s)
   else
      *Nota de Credito Tipo B ¢ C
      s:= CHR(128) + se + "S" + se + "T" + se + ""
      enviar(s)
   endif

   *Detalle de la Nota de Credito
   if (bb_con = "060")
      ltx:= "Cr‚dito a la cuota N§ " + strzero(n_cuo, 2) + ;
         " de factura N§" + f_trans(Transform(n_fac, "######"))
   else
      ltx:= "Cr‚dito contado sobre la factura N§" + ;
         f_trans(Transform(n_fac, "######"))
   endif
   s:= "B" + se + ltx + se + "1" + se + strzero(c_lic, 8, 2) + se + ;
      strzero(i_va1, 5, 2) + se + "M" + se + "0" + se + "" + se + "T"
   enviar(s)

   *Cerrar Nota de Credito
   s:= CHR(129)
   enviar(s)
   return 0

********************************
function ENVIAR

   parameters string
   private result, statprn
   statprn:= mandapaq(handler, string)
   tit2(" StatPrn = " + alltrim(Str(statprn)))
   if (statprn < 0)
      do while (statprn = -9)
         tit2("Impresora Ocupada")
         string:= "¡"
         statprn:= mandapaq(handler, string)
         if (LastKey() == K_ESC)
            tit2("Fin de la consulta intermedia")
            return -9
         endif
         InKey(1)
      enddo
      return -1
   endif
   result:= respuesta(handler)
   n_strin:= Left(string, 1)

   *Busca una E ¢ una  en el string enviado
   *Si es una E sera una Factura
   *Si es una  sera una Nota de Credito
   if (n_strin = CHR(69) .OR. n_strin = chr(129))
      n_resul:= Val(right(result, 8))
   endif
   geterrors(result)
   return 0


