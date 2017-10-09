*Menu de movimientos
DO CASE
 CASE opc1=1
  *Articulos
  posi = opc1+5
  DO WHILE .T.
   Xsub_pan1 = SAVESCREEN(posi,30,posi+5,50)
   SOMBRA(posi,30,posi+4,49,'V2',1,'',INVER,'N+')
   SETCOLOR(color2)
   opcion = 0
   @posi+1,31 PROMPT '1- Consulta    'MESS'                       Permite Consultar Listas de Precios                       '
   @posi+2,31 PROMPT '2- Modif. P.Uni'MESS'                  Permite Modificar Lista de Precios Unitarios                   '
   @posi+3,31 PROMPT '3- Presupuestos'MESS'                   Permite Generar Presupuestos para Clientes                    '
   MENU TO opcion
   DO CASE
    CASE opcion = 0
     RESTSCREEN(posi,30,posi+5,50,Xsub_pan1)
     RETURN
    CASE opcion = 1
     *Consulta Lista de Precios
     DO MOVCONSU

    CASE opcion=2
     *Modificar Lista de Precios
     IF !Vusuario="VENTAS  "
      DO MOVMODPU
     ENDIF

    CASE opcion=3
     *Genera Presupuestos
     Xsub_pan2 = SAVESCREEN(11,46,15,62)
     SOMBRA(11,46,14,61,'V1',1,'',INVER,'N+')
     SETCOLOR(color2)
     @12,48 PROMPT '1- Ingresar  'MESS'                       Permite Generar Presupuestos Nuevos                       '
     @13,48 PROMPT '2- Modificar 'MESS'             Permite Consultar ¢ Modificar un Presupuesto Existente              '
     MENU TO p6
     IF p6#0
      DO MOVPRESU WITH p6
     ENDIF
     RESTSCREEN(11,46,15,62,Xsub_pan2)
   ENDCASE
   RESTSCREEN(posi,30,posi+5,50,Xsub_pan1)
  ENDDO
 CASE opc1=2
  *
ENDCASE
CLOSE ALL
RETURN

*     FUNCTION BERRE
*     PARA dato
*     RETURN RIGHT(dato,4)+SUBSTR(dato,4,2)+LEFT(dato,2)
*
*     FUNCTION AYUDO
*     PARAMETERS campi,ntx1,ntx2,l1,l2,camp1,archi
*     PRIVATE col_col
*     col_col=SETCOLOR()
*     IF RECCOUNT()=0
*      pan=SAVESCREEN(20,31,23,61)
*      SETCOLOR(color3)
*      @20,31 CLEAR TO 23,61
*      @20,31 TO 23,61 DOUBLE
*      @21,36 SAY '   PRESTE ATENCION  '
*      @22,32 SAY 'Ä ARCHIVO '+archi+' VACIO Ä'
*      SET CURSOR OFF
*      TONO(2)
*      INKEY(0)
*      SET CURSOR ON
*      SETCOLOR(col_col)
*      RESTSCREEN(20,31,23,61,pan)
*      RETURN .F.
*     ENDIF
*     SET ORDER TO 1
*     pan=SAVESCREEN(4,29,24,78)
*     SETCOLOR(color3)
*     SOMBRA(4,29,22,77,'H1',1,'',color3)
*     SETCOLOR(color3)
*     @ 6,29  SAY 'ÌÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹'
*     @ 19,29 SAY 'ÇÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¶'
*     @ 16,29 SAY 'ÌÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹'
*     @ 4,41 SAY ' BUSCAR '
*     @17,30 SAY 'BUSCAR:'
*     @ 20,30 SAY CHR(24)+CHR(25)+' PgUp PgDn mover          '+CHR(17)+'ÄÙ-ACEPTA'
*     @ 21,30 SAY "F2-cambia clave      ESC-salir"
*     DECLARE cam[2],sep[2],ind[2],lar[2],nom[2]
*     carga1(cam,campi)
*     carga2(sep,'6,0')
*     titul1=archi+'       NOMBRE              '
*     ind[1]=ntx1
*     lar[1]=l1
*     nom[1]=archi
*     ind[2]=ntx2
*     lar[2]=l2
*     nom[2]='NOMBRE :'
*     ord_ant=STR(INDEXORD())
*     SET ORDER TO 2
*     SETCOLOR(color3)
*     GO TOP
*     BUSCARE(5,31,15,75,cam,titul1,sep,ind,lar,18,31,nom,17,41)
*     SETCOLOR(col_col)
*     SET ORDER TO &ord_ant
*     RESTSCREEN(4,29,24,78,pan)
*     IF LASTKEY()=27
*      RETURN .F.
*     ENDIF
*     RETURN .T.
*
*     *scroll por pantalla
*     FUNCTION SCR
*     PARA menus,posvis,fila,col,largo,f1,c1,f2,c2,princ,tono_il,tono
*     PRIV ind,a,pos,n,maxtab,x,i,bb,col_ant
*     col_ant=SETCOLOR()
*     IF PCOUNT()>=5
*      maxtab=largo
*     ELSE
*      IF PCOUNT()<5
*       maxtab=len(menus)
*      ENDIF
*     ENDIF
*     bb='n+/r'
*     DO CASE
*      CASE PCOUNT()=4
*       f1=13
*       c1=39
*       f2=23
*       c2=78
*       princ=1
*       tono='w/n'
*       tono_il='n/w'
*      CASE PCOUNT()=5
*       f1=13
*       c1=39
*       f2=23
*       c2=78
*       princ=1
*       tono='w/n'
*       tono_il='n/w'
*      CASE PCOUNT()=8
*       princ=1
*       tono='w/n'
*       tono_il='n/w'
*      CASE PCOUNT()=9
*       tono_il='n/w'
*       tono='w/n'
*      CASE PCOUNT()=10
*       tono='w/n'
*     ENDCASE
*     ind=princ
*     a=0
*     IF maxtab<posvis
*      posvis=maxtab
*     ENDIF
*     IF ind>maxtab-posvis-1
*      pos=ind+posvis-maxtab
*      princ=maxtab-posvis+1
*     ELSE
*      pos=1
*     ENDIF
*     SAVE SCREEN
*     SET CURSOR OFF
*     SETCOLOR(tono)
*     FOR n=1 TO posvis
*      @fila+n-1,col SAY menus[n+princ-1]
*     NEXT
*     DO WHILE a<>13.AND.a<>27.AND.a<>-1.AND.a<>-2.AND.a<>-3.AND.a<>-4.AND.a<>-5.AND.;
*     a<>-6.AND.a<>-7.AND.a<>-8.AND.a<>-9.AND.a<>7.AND.a<>22
*      DO CASE
*       CASE a=5
*        DO sube
*       CASE a=24
*        DO cae
*       CASE a=1
*        DO comien
*       CASE a=6
*        DO fin
*       CASE a=3
*        DO avpag
*       CASE a=18
*        DO repag
*       CASE a>47.AND.a<123
*        DO buscar
*        LOOP
*      ENDCASE
*      DO escribe
*      a=INKEY(0)
*     ENDDO
*     SET CURSOR ON
*     SETCOLOR(col_ant)
*     RESTORE SCREEN
*     RETURN(ind)
*
*     PROCEDURE CAE
*     IF ind<maxtab
*      ind=ind+1
*      IF pos<posvis
*       pos=pos+1
*      ELSE
*       x=ind
*       FOR n=pos TO 1 step -1
*        @fila+n-1,col SAY menus[x]
*        x=x-1
*       NEXT
*      ENDIF
*      @fila+pos-2,col SAY menus[ind-1]
*     ENDIF
*     DO escribe
*     RETURN
*
*     PROCEDURE SUBE
*     IF ind>1
*      ind=ind-1
*      IF pos>1
*       pos=pos-1
*      ELSE
*       x=ind
*       FOR n=1 TO posvis
*        @fila+n-1,col SAY menus[x]
*        x=x+1
*       NEXT
*      ENDIF
*      @fila+pos,col SAY menus[ind+1]
*     ENDIF
*     DO escribe
*     RETURN
*
*     PROCEDURE COMIEN
*     FOR ind=1 TO posvis
*      @fila+ind-1,col SAY menus[ind]
*     NEXT
*     ind=1
*     pos=1
*     DO escribe
*     RETURN
*
*     PROCEDURE FIN
*     FOR n=1 TO posvis
*      @fila+n-1,col SAY menus[maxtab-posvis+n]
*     NEXT
*     ind=maxtab
*     pos=posvis
*     DO escribe
*     RETURN
*
*     PROCEDURE AVPAG
*     x=pos
*     IF ind+posvis-x<=maxtab-posvis
*      FOR n=1 TO posvis
*       x=x-1
*       @fila+n-1,col SAY menus[ind+posvis-x]
*      NEXT
*      ind=ind+posvis
*     ELSE
*      FOR n=1 TO posvis
*       @fila+n-1,col SAY menus[maxtab-posvis+n]
*      NEXT
*      ind=maxtab+pos-posvis
*     ENDIF
*     DO escribe
*     RETURN
*
*     PROCEDURE REPAG
*     x=pos
*     IF ind-x>=posvis
*      FOR n=1 TO posvis
*       x=x-1
*       @fila+n-1,col SAY menus[ind-x-posvis]
*      NEXT
*      ind=ind-posvis
*     ELSE
*      FOR ind=posvis TO 1 step-1
*       @fila+ind-1,col SAY menus[ind]
*      NEXT
*      ind=pos
*     ENDIF
*     DO ESCRIBE
*     RETURN
*
*     PROCEDURE BUSCAR
*     PRIV b,wcont,var,indice
*     b=a
*     wcont=0
*     var=""
*     DO WHILE (b>47.AND.b<123).OR.b=0.OR.b=32.OR.(b>128.AND.b<169)
*      var=var+CHR(b)
*      wcont=wcont+1
*      ctrl=0
*      FOR indice=1 TO maxtab
*       IF UPPER(var)=UPPER(SUBSTR(menus[indice],1,wcont))
*        IF indice>ind-pos.AND.indice<=ind-pos+posvis
*         pos=pos+indice-ind
*        ELSE
*         IF indice>maxtab-posvis
*          pos=indice+posvis-maxtab
*         ELSE
*          IF indice<=posvis
*           pos=indice
*          ENDIF
*         ENDIF
*        ENDIF
*        inicio=indice-pos+1
*        FOR n=1 TO posvis
*         @fila+n-1,col SAY menus[inicio+n-1]
*        NEXT
*        ind=indice
*        indice=maxtab
*        ctrl=1
*        var=SUBSTR(menus[ind],1,wcont)
*        DO ESCRIBE
*        SETCOLOR(bb)
*        @fila+pos-1,col SAY var
*        SETCOLOR(tono)
*       ENDIF
*      NEXT
*      IF ctrl=0
*       var=SUBSTR(var,1,LEN(var)-1)
*       wcont=wcont-1
*      ENDIF
*      b=0
*      DO WHILE b=0
*       b=INKEY(0)
*      ENDDO
*     ENDDO
*     a=b
*     RETURN
*
*     PROCEDURE ESCRIBE
*     SETCOLOR(tono_il)
*     @fila+pos-1,col SAY menus[ind]
*     SETCOLOR(tono)
*     RETURN
