*Parametros del Sistema
PRIVATE col_ant,pan_ant
col_ant=SETCOLOR()
pan_ant=SAVESCREEN(0,0,24,79)
PRIVATE vnomb,vdomi,vloca,vibru,vciva,nom_civa,vcuit,varet
PRIVATE vPu30,vPu60,vRG212
PRIVATE viva1,viva2

*Porcentaje de iva
AREA()
IF USAR('PORCEIVA','PIVAS')
 GO TOP
 viva1=IVAI
 viva2=IVANI
ELSE
 QUIT
ENDIF

*ARCHIVO AUXILIAR
AREA()
IF USAR('AUXILIAR','AUX')
 GO TOP
 vnomb=nomb
 vdomi=domi
 vloca=loca
 vibru=ibru
 vciva=civa
 vcuit=cuit
 varet=aret
 vPu30=pu30
 vPu60=pu60
 vrdbf=rdbf
ELSE
 QUIT
ENDIF

DO WHILE .T.
 SETCOLOR(color7)
 @1,0 CLEAR TO 24,79
 DO PAN_PARAM
 tecla=65
 DIST_COLOR(01,8,'~W+/R~**** PARAMETROS DEL SISTEMA ****')
 XPC=12
 XC=1
 SET KEY -4 TO VA_TRAS
 SETCOLOR(color1)
 *Imprime datos fijos
 @ 5, 5 SAY vnomb
 @ 5,39 SAY vdomi
 @ 7, 4 SAY vloca
 @ 7,31 SAY vibru
 @7,42 SAY vciva+' '+IIF(vciva='1','R.I. ',IIF(vciva='2','R.N.I',IIF(vciva='3','C.F. ',IIF(vciva='4','EXEN.',IIF(vciva='5','M.T. ',;
           IIF(vciva='6','N.A.',IIF(vciva='7','N.C.',SPACE(5))))))))
 @ 7,51 SAY vcuit
 @ 7,64 SAY varet
 @11,26 SAY vPu30 PICTURE "99.99"
 @11,34 SAY vPu60 PICTURE "99.99"
 @11,44 SAY viva1 PICTURE "99.99"
 @11,51 SAY viva2 PICTURE "99.99"
 @15,25 SAY vrdbf
 SET CURSOR OFF
 DO WHILE tecla#13 .AND. tecla#27
  tecla=INKEY(0)
 ENDDO
 IF tecla=27
  EXIT
 ENDIF
 SETCOLOR(colo3)
 @24,20 SAY SPACE(35)
 SET CURSOR ON
 DO WHILE XC<=XPC
  XD=1
  SET CURSOR ON
  SETCOLOR(color1)
  DO CASE
   CASE XC=1
    @5,5 GET vnomb PICTURE "@!"
    READ

   CASE XC=2
    @5,39 GET vdomi PICTURE "@!"
    READ

   CASE XC=3
    @7,4 GET vloca PICTURE "@!"
    READ

   CASE XC=4
    @7,31 GET vibru PICTURE "@K 9999999999"
    READ

   CASE XC=5
    @7,42 GET vciva PICTURE "9" VALID CIVA1(@vciva)
    READ
    @7,42 SAY vciva+' '+IIF(vciva='1','R.I. ',IIF(vciva='2','R.N.I',IIF(vciva='3','C.F. ',IIF(vciva='4','EXEN.',IIF(vciva='5','M.T. ',;
              IIF(vciva='6','N.A.',IIF(vciva='7','N.C.',SPACE(5))))))))

   CASE XC=6
    @7,51 GET vcuit PICTURE "@K 99999999999"
    READ

   CASE XC=7
    @7,64 GET varet PICTURE "@K 9999999"
    READ

   CASE XC=8
    @11,26 GET vpu30 PICTURE "@K 99.99"
    READ

   CASE XC=9
    @11,34 GET vpu60 PICTURE "@K 99.99"
    READ

   CASE XC=10
    @11,44 GET viva1 PICTURE "@K 99.99"
    READ

   CASE XC=11
    @11,51 GET viva2 PICTURE "@K 99.99"
    READ

   CASE XC=12
    @15,25 GET vrdbf PICTURE "@!"
    READ

  ENDCASE
  IF LASTKEY()=27
   CLEAR GETS
   XC=13
  ELSE
   XC=XC+XD
  ENDIF
 ENDDO
 *Gravar datos
 bien=CARTEL("DATOS CORRECTOS  [ SI ]  [ NO ]",18,1,'N',color3)
 IF !bien .OR.LASTKEY()=27
  LOOP
 ENDIF
 SELECT AUX
 GO TOP
 RECLOCK(1)
 *Datos empresa
 REPLACE nomb WITH vnomb
 REPLACE domi WITH vdomi
 REPLACE loca WITH vloca
 REPLACE ibru WITH vibru
 REPLACE civa WITH vciva
 REPLACE cuit WITH vcuit
 REPLACE aret WITH varet
 *Variables generales
 REPLACE pu30 WITH vpu30
 REPLACE pu60 WITH vpu60
 REPLACE rdbf WITH vrdbf
 UNLOCK
 SELECT PIVAS
 GO TOP
 RECLOCK(1)
 REPLACE ivai WITH viva1
 REPLACE ivani WITH viva2
 UNLOCK
ENDDO
CLOSE ALL
RESTSCREEN(0,0,24,79,pan_ant)
SETCOLOR(col_ant)
RETURN

PROCEDURE PAN_PARAM
SOMBRA(2,1,21,75,'H5',1,'',colo3)
SETCOLOR(color9)
@ 3,2 SAY " ÚDatos de La EMPRESAÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿  "
@ 4,2 SAY " ÃRaz¢n SocialÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂDomicilio LegalÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´  "
@ 5,2 SAY " ³                                 ³                                  ³  "
@ 6,2 SAY " ÃLocalidadÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂN§Ing.Bru.ÂCon.IVAÄÂN§ CUITÄÄÄÄÄÂN§Ag.Ret´  "
@ 7,2 SAY " ³                          ³          ³        ³            ³        ³  "
@ 8,2 SAY " ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÙ  "
@ 9,2 SAY "                      Ú% P.UnitariosÄÄ¿  ÚÄÄDatos IVAÄ¿                  "
@10,2 SAY "                      Ãa 30 DÄÂa 60 DÄ´  ³% RIÄÂ% RNIÄ´                  "
@11,2 SAY "                      ³       ³       ³  ³     ³      ³                  "
@12,2 SAY "                      ÀÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÙ  ÀÄÄÄÄÄÁÄÄÄÄÄÄÙ                  "
@13,2 SAY "                      ÚRuta de AccesosÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿                 "
@14,2 SAY "                      ÃArchivos de TrabajoÄÄÄÄÄÄÄÄÄÄÄÄÄ´                 "
@15,2 SAY "                      ³                                ³                 "
@16,2 SAY "                      ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ                 "
SETCOLOR(color3)
@19,20 SAY " ±Esc±-Sale  ±ÄÙ±-Ingresa     "
SETCOLOR(color7)
DIST_COLOR(19,22,'~W+/R~Esc')
DIST_COLOR(19,34,'~W+/R~ÄÙ')
RETURN

FUNCTION CIVA1(iva)
LOCAL m_iva,col_iva,pan_iva
col_iva=SETCOLOR()
pan_iva=SAVESCREEN(,,,,)
IF iva<"1".OR.iva>"7"
 SOMBRA(8,40,16,54,"H1",1,"",color3)
 m_iva=1
 SETCOLOR(color3)
 @9,41 PROMPT "1- R.I.   "
 @10,41 PROMPT "2- R.N.I  "
 @11,41 PROMPT "3- C.F.   "
 @12,41 PROMPT "4- EXENTO "
 @13,41 PROMPT "5- MonoT. "
 @14,41 PROMPT "6- No Alc."
 @15,41 PROMPT "7- No Cat."
 MENU TO m_iva
 RESTSCREEN(,,,,pan_iva)
 SETCOLOR(col_iva)
 IF LASTKEY()=27.OR.m_iva=0
  RETURN .F.
 ENDIF
 iva=STR(m_iva,1)
ENDIF
RETURN .T.
