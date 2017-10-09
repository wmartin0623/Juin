*Pantalla de Rubros
PROCEDURE ingres01
SOMBRA(8,6,19,60,'H5',1,'',color7)
SETCOLOR(color9)
@09,8 SAY "C¢digo del Rubro :"
@10,8 SAY "Descripci¢n .... :"
@11,8 SAY "Usa Medidas ...  :"
@12,8 SAY "Usa Marcas ..... :"
@13,8 SAY "Usa Color ...... :"
@14,8 SAY "Usa Calidad .... :"
@15,8 SAY "Usa Utilizado en :"
@16,8 SAY "Usa Durezas .... :"
@17,8 SAY "Usa Referencias. :"
@18,8 SAY "Usa M2 x caja .. :"
RETURN

*Pantalla Listas de Precios
PROCEDURE ingres02
SOMBRA(5,6,19,66,'H5',1,'',color7)
SETCOLOR(color9)
* CLIE.FMT
@ 9,8 SAY "     ÚÄÄÄÄÄÄLISTA DE PRECIOSÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿   "
@10,8 SAY "     ÃCodigoÂÄDescripci¢nÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´   "
@11,8 SAY "     ³      ³                                       ³   "
@12,8 SAY "     ÃRubroÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´   "
@13,8 SAY "     ³                                              ³   "
@14,8 SAY "     ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ   "
RETURN

*Pantalla de Marcas
PROCEDURE ingres03
SOMBRA(8,15,16,64,'H5',1,'',color7)
SETCOLOR(color9)
@10,19 SAY "C¢digo ....... :"
@12,19 SAY "Descripci¢n .. :"
@14,19 SAY "Rubro asociado :"
RETURN

PROCEDURE PAN_ARTIC
* ARTICULOS.FMT
SOMBRA(5,4,21,69,'H5',1,'',color7)
SETCOLOR(color9)
@ 5,5 SAY "                ÚRubroÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿                  "
@ 6,5 SAY "                ³                           ³                  "
@ 7,5 SAY "ÚArt¡culoÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
@ 8,5 SAY "³                                                              ³"
@ 9,5 SAY "ÀObs.:ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
@10,5 SAY "   ÚLista de preciosÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂC¢digo Art.ÄÄÄÄÄÄÄÄÄÄÄ¿"
@11,5 SAY "   ÃC¢d.ÄÂDescripci¢nÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅEn ProveedorÄÄÄÄÄÄÄÄÄÄ´"
@12,5 SAY "   ³     ³                              ³                      ³"
@13,5 SAY "ÚDetalles de VentaÄÄÄÄÄÄÄÄÄÄÄÂAtributosÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´"
@14,5 SAY "³Unidad         :            ³Medidas   :                      ³"
@15,5 SAY "³Forma Vta.     :            ³Marca     :                      ³"
@16,5 SAY "³Unidades F.Vta :            ³Color     :                      ³"
@17,5 SAY "³Stock Actual   :            ³Calidad   :                      ³"
@18,5 SAY "ÀÂPrecio UnitarioÄÄÄÄÄÄÄÄÄÄÄÄ´Utilizado :                      ³"
@19,5 SAY " ÃContadoÄÄÄÄÂCta.Cte.ÄÄÄÂMon´Dureza    :                      ³"
@20,5 SAY " ³           ³           ³   ³Referencia:                      ³"
@21,5 SAY " ÀAct.: ÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÁM2 x Caja :ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
DIST_COLOR(05,22,'~W+/BG~Rubro')
DIST_COLOR(07,06,'~W+/BG~Art¡culo')
DIST_COLOR(10,09,'~W+/BG~Lista de Precios')
DIST_COLOR(11,09,'~W+/BG~C¢d.')
DIST_COLOR(11,15,'~W+/BG~Descripci¢n')
DIST_COLOR(10,46,'~W+/BG~C¢digo Art.')
DIST_COLOR(11,46,'~W+/BG~En Proveedor')
DIST_COLOR(13,06,'~W+/BG~Detalles de Venta')
DIST_COLOR(14,06,'~W+/BG~Unidad')
DIST_COLOR(15,06,'~W+/BG~Forma Vta.')
DIST_COLOR(16,06,'~W+/BG~Unidades F.Vta.')
DIST_COLOR(18,07,'~W+/BG~Precio Unitario')
DIST_COLOR(19,07,'~W+/BG~Contado')
DIST_COLOR(19,19,'~W+/BG~Cta.Cte.')
DIST_COLOR(19,31,'~W+/BG~Mon')
DIST_COLOR(13,35,'~W+/BG~Atributos')
RETURN

*Pantalla CLIENTES
PROCEDURE ingres05
SOMBRA(5,4,22,66,'H5',1,'',color7)
SETCOLOR(color9)
* CLIE.FMT
@ 5,5 SAY "      ÚÄÄÄÄÄÄC L I E N T EÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂTipoÄÄÄ¿ ÚEstado¿"
@ 6,5 SAY "      ÃCodigoÂÄApellido y NombreÄÄÄÄÄÄÄÄÄÄÄÄÅTpÂDesc´ ÃÄÄÄÄÄÄ´"
@ 7,5 SAY "      ³      ³                              ³  ³    ³ ³      ³"
@ 8,5 SAY "ÚDomicilioÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂUlt.CompraÂÄÁÄÄÁÄÄÄÄÁÄÁÄÄÄÄÄÄÙ"
@ 9,5 SAY "³                              ³          ³                   "
@10,5 SAY "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"
@11,5 SAY "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂC.PostalÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿              "
@12,5 SAY "³Pcia.:                ³                      ³              "
@13,5 SAY "³Dpto.:                ÃÄÄÄÄÄTelefonoÄÄÄÄÄÄÄÄÄ´              "
@14,5 SAY "³                      ³                      ³              "
@15,5 SAY "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
@16,5 SAY "ÚÄÄÄI.V.A.ÄÄÄÄÄÄÄÄÄ¿ÚTp./Nro.Docu¿ÚÄÄÄS A L D O SÄÄÄÄÄÄÄÄÄÄÄ´"
@17,5 SAY "ÃEstadoÂN§ CUITÄÄÄÄ´ÃÄÄÄÄÄÄÄÄÄÄÄÄ´ÃActualÄÄÄÄÄÄÂAcreditadoÄÄ´"
@18,5 SAY "³      ³           ³³            ³³            ³            ³"
@19,5 SAY "ÀÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÙÀÄÄÄÄÄÄÄÄÄÄÄÄÙÀÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÙ"
@20,5 SAY " E-MAIL:                                                     "
@21,5 SAY "                                                             "
RETURN

*Pantalla de bancos
PROCEDURE ingres06
SOMBRA(8,15,16,64,'H5',1,'',color7)
SETCOLOR(color9)
@10,19 SAY "Codigo ....... :"
@12,19 SAY "N o m b r e .. :"
@14,19 SAY "Sigla ........ :"
RETURN

*Pantalla Departamentos
PROCEDURE ingres09
SOMBRA(8,17,16,64,'H5',1,'',color7)
SETCOLOR(color9)
@10,21 SAY "Codigo ....... :"
@12,21 SAY "N o m b r e .. :"
@14,21 SAY "Sigla ........ :"
RETURN

*Pantalla de Provincias
PROCEDURE ingres08
SOMBRA(5,17,12,64,'H5',1,'',color7)
SETCOLOR(color9)
@06,21 SAY "Codigo ....... :"
@08,21 SAY "N o m b r e .. :"
@10,21 SAY "Sigla ........ :"
RETURN

@12,21 SAY "Conv.Multilat. :"
@14,21 SAY "Res.16/07 I.Bru:"
@14,44 SAY "Imp.M¡nimo:"
@15,21 SAY "I.Bruto 1,2,3,4:"
@17,21 SAY "Exenta IVa.... :"
@18,21 SAY "Percepcion ... :"
@19,21 SAY "Retencion .... :"
RETURN

*Pantalla de Plazas de Bancos
PROCEDURE ingres07
SOMBRA(8,6,18,60,'H5',1,'',color7)
SETCOLOR(color9)
@10,8 SAY "Codigo PLAZA ... :"
@12,8 SAY "Provincia ...... :"
@14,8 SAY "Descripci¢n Plaza:"
@16,8 SAY "Sigla .......... :"
RETURN

*Pantalla de Codigos Postales
PROCEDURE ingres14
SOMBRA(8,17,16,64,'H5',1,'',color7)
SETCOLOR(color9)
@10,21 SAY "Provincia..... :"
@12,21 SAY "Departamento.. :"
@14,21 SAY "Cod. Postal... :"
RETURN

*Pantalla de Condiciones de Pago en Remitos
PROCEDURE ingres11
SOMBRA(8,15,14,64,'H5',1,'',color7)
SETCOLOR(color9)
@10,17 SAY "Codigo ....... :"
@12,17 SAY "Condici•n .... :"
RETURN
