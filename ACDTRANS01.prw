#Include "RwMake.ch"
#Include 'Protheus.ch'
#Include 'Topconn.ch'
#INCLUDE 'APVT100.CH'

#DEFINE P_ENDERECO		01
#DEFINE P_PRODUTO		02
#DEFINE P_QUANT			03
#DEFINE P_LOTE			04
#DEFINE P_LOCAL			05
#DEFINE P_QTD2UM		06

#define POS_TRANSP		01
#define POS_PLACA		02
#define POS_UFPLAC		03
#define POS_VOLUME		04
#define POS_PESLIQ		05
#define POS_PESBRU		06

//-----------------------------------------------------------------
/*/{Protheus.doc} ACDTRANS01
 Rotina de Menu do ACD para Transferência entre Filiais

@type		Function
@author		Gerson Schiavo
@since 		14/04/2016
/*/
//-----------------------------------------------------------------
User Function ACDTRANS01()

	Local aArea   				:= GetArea()
	Local aScreen 				:= NIL
	Local nOpcao  				:= 1

	private aDadosTemp			:= {}
	private aAreasTab			:= {}
	private aDadComplem			:= {}
	private cIdUserAtu			:= __cUserId
	private cBkpOperador		:= CBRetOpe()

	VTSAVE SCREEN TO aScreen

	If nOpcao != 0
		fTransfere(@nOpcao)
	EndIf
	VTRESTORE SCREEN FROM aScreen

	RestArea(aArea)

Return(.t.)

//-----------------------------------------------------------------
/*/{Protheus.doc} fTransfere
Transferência entre Filiais

@type		Function
@param		nOpcao,	Numerico,	Opção Selecionada
@author		Gerson Schiavo
@since 		14/04/2016
@Return		nil nulo
/*/
//-----------------------------------------------------------------
Static Function fTransfere(nOpcao)

	Local lRet			:= .T.
	Local aLista		:= {}
	Local aEmprx 		:= {'@@@@'} //aClone(aUser[2][6])
	Local aEmp
	Local aTotLote		:= {}
	Local nW			:= 0
	Local __lExit		:= .T.
	Local __cMensagem	:= ""
	Local __nPosTLote	:= 0
	Local __aTela
	Local cEmpBKP		:= cEmpAnt
	Local cFilBKP		:= cFilAnt
	Local nItem			:= 0

	Private cFunBkp 	:= FunName()
	Private lAbandona	:= .F.
	Private cEtiqueta	:= Space(35)
	Private cEndDest  	:= CriaVar("BE_LOCALIZ")
	Private cEndOrig  	:= CriaVar("BE_LOCALIZ")
	Private cFilDest	:= Space(12)
	Private cFilOri		:= cFilAnt
	Private cNota		:= ""
	Private nVolComp	:= 0
	Private nQtdVol		:= 0
	Private nPLiqComp	:= 0
	Private nPBrtComp	:= 0
	Private nLimVol		:= SuperGetMV("ZZ_LIMVOL",,150)

	While !lAbandona

		VTSetKey(09,{|| Consulta(aLista)},"Consulta Etiqueta")

		VTClear()
		@ 000,000 VTSAY "Transferencia Entre Filiais"
		@ 3,00 VTSAY "ENDERECO ORIGEM:"
		@ 4,00 VTGet cEndOrig  pict '@!' Valid VldEnd(cEndOrig)
		VTRead()

		If  !lAbandona .and. VTLastKey() == 27
			If VTYesNo("Abandona o Processo ?","Atencao",.T.)
				exit
			else
				loop
			endif
		endif
		nQtdVol		:= 0
		nVolComp	:= 0
		nPLiqComp	:= 0
		nPBrtComp	:= 0

		while  !lAbandona

			VTSetKey(24,{|| Estorna(aLista)},"Estorno")
			VTSetKey(09,{|| Consulta(aLista)},"Consulta Etiqueta")
			VTSetKey(05,{|| NovoEnder()},"Novo Endereço")

			VTClear()

			@ 000,000 VTSAY "Transferencia Entre Filiais"
			@ 1,00 VTSAY "Qtde Lotes:"
			@ 2,00 VTSAY nQtdVol
			VTGetRefresh(nQtdVol)
			If (nQtdVol < nLimVol)
				@ 3,00 VTSAY "ETIQUETA:"
				@ 4,00 VTGet cEtiqueta  pict '@!' Valid VldEti(cEtiqueta,@aLista,"I",cEndOrig)
				VTRead()
			else
				If (nQtdVol >= nLimVol)
					VTBEEP(2)
					VTAlert("   Limite Leitura Excedido!"+chr(13)+chr(10)+"Pressione ESC P/ Transferencia","Aviso")
				Endif
			Endif

			If VTLastKey() == 27

				aTotLote := {}

				__aTela			:= VTSave()

				for nItem := 1 to Len(aDadosTemp)
					__nPosTLote := aScan(aTotLote,{|x| x[1] == aDadosTemp[nItem,P_PRODUTO] })
					If __nPosTLote == 0
						AADD(aTotLote,{ aDadosTemp[nItem,P_PRODUTO] , aDadosTemp[nItem,P_QUANT] , 1 })
					Else
						aTotLote[__nPosTLote][2] := aTotLote[__nPosTLote][2] + aDadosTemp[nItem,P_QUANT]
						aTotLote[__nPosTLote][3] += 1
					Endif
					nVolComp += 1
					BuscaPeso( aDadosTemp[nItem,P_PRODUTO] ,SubStr(cEndOrig,1,2), aDadosTemp[nItem,P_LOTE] )
				next

				For nW := 1 to Len(aTotLote)

					// Montagem da variavel de mensagem
					__cMensagem := " Produto: " + Alltrim(aTotLote[nW][1]) + " Qtd: " + AllTrim(TransForm(aTotLote[nW][2],TM(aTotLote[nW][2],TamSX3("D2_QUANT")[1],TamSX3("D2_QUANT")[2]))) + " Volume: "+AllTrim(TransForm(aTotLote[nW][3],TM(aTotLote[nW][3],TamSX3("F2_VOLUME1")[1],TamSX3("F2_VOLUME1")[2])))

					// Pergunta exibindo ao operador as informações de produto e total separado
					// se ele confirmar indica que a quantidade separada é a desejada.
					If VtYesNo(__cMensagem,"ACDTRANS01")
						// Variavel que permitirá a saida do rotina com a consiencia do operador que a quantidade do
						// produto separado esta correta.
						__lExit := .T.
					Else
						// Variavel que permitirá a saida do rotina com a consiencia do operador que a quantidade do
						// produto separado não esta correta e assim voltaremos na tela de solicitação de novas etiquetas..
						__lExit := .F.

						// Alimentação da variavel para a saida do For.
						nW := Len(aTotLote)
					Endif  // VtYesNo(__cMensagem,"EmTrExp")
				Next //  nW := 1 to Len(aTotLote)

				If __lExit
					If VTYesNo("Finaliza Coleta de Dados ?","Atencao",.T.)
						if VTYesNo("Deseja Fazer a Transferencia ?","Atencao",.T.)

							aEmp 	:= VTNewEmpr(@aEmprx,.T.)
							cFildest:= aEmp[1]+aEmp[2]

							salvaAreas()
							trocaEmpFil(aEmp[1],aEmp[2])

							VTClear()
							@ 000,000 VTSAY "Transferencia Entre Filiais"
							@ 2,00 VTSAY "ENDERECO DESTINO:"
							@ 3,00 VTGet cEndDest  pict '@!' Valid VldEnd(cEndDest)
							VTRead()

							if VTYesNo("Confirma Transferencia ?","Atencao",.T.)
								VTClear()
								VtMsg("Aguarde... ")
								lRet:= Finaliza()
								if !lRet
									loop
								endif
							endif

							trocaEmpFil(cEmpBKP,cFilBKP)
							restAreas()

						endif
						lAbandona:= .t.
						exit
					else
						loop
					endif
				Else
					// Limpamos a tela do VTR
					VTCLEAR()

					// Restauramos a tela do VTR na tela de seleção de etiquetas.
					VtRestore(,,,,__aTela)
				Endif  // __lExit

			endif
		enddo
	enddo

Return

//-----------------------------------------------------------------
Static Function trocaEmpFil(cEmpAtu,cFilAtu)

	local aAllusers		:= {}
	Local cEmpDef		:= cEmpAnt
	Local cFilDef		:= cFilAnt
	local cOperAtual	:= ''

	Default cEmpAtu     := cEmpDef
	Default cFilAtu     := cFilDef

	geraLog( Replicate('-',60) )
	geraLog( "INICIO TROCA EMPRESA/FILIAL" )
	geraLog( "	>> EMPRESA ATUAL [" + alltrim(alltochar(cEmpAnt)) + "]" )
	geraLog( "	>> FILIAL ATUAL [" + alltrim(alltochar(cFilAnt)) + "]" )
	geraLog( " " )
	geraLog( "	>> EMPRESA NOVA [" + alltrim(alltochar(cEmpAtu)) + "]" )
	geraLog( "	>> FILIAL NOVA [" + alltrim(alltochar(cFilAtu)) + "]" )
	RpcClearEnv()
	If RpcSetEnv(cEmpAtu,cFilAtu,,,'ACD')
		geraLog( "	>> TROCA DE EMPRESA/FILIAL REALIZADA COM SUCESSO!" )
		geraLog( "	>> EMPRESA/FILIAL NOVA: [" + cEmpAnt + "/" + cFilAnt + "]" )
		geraLog( "	>> INICIO POSICIONAMENTO USUARIO..." )
		geraLog( "	>> OPERADOR ATUAL...:" + CBRetOpe()  )

		PswOrder(1)
		If PswSeek( cIdUserAtu )
			geraLog('	>> Posicionado no usuário ' + cIdUserAtu + ' com sucesso!')
			aAllusers	:= FWSFALLUSERS({cIdUserAtu})
			If len(aAllusers) > 0
				__cUserId		:= aAllusers[1,2]
				cUserName		:= aAllusers[1,4]
				cOperAtual		:= CBRetOpe() 
				geraLog('	>> Operador: ' + cOperAtual )
			else
				geraLog('	>> ERRO ao buscar o usuário ' + cIdUserAtu + '...')
			endif
		else
			geraLog('	>> ERRO ao posicionar no usuário ' + cIdUserAtu + '...')
		endif
		geraLog( "	>> OPERADOR NOVO....:" + CBRetOpe()  )
	else
		geraLog( "	>> ERRO NA TROCA DE EMPRESA/FILIAL!" )
		VTAlert("	>> Não foi possível entrar na empresa/filial: " + cEmpAtu + "/" + cFilAtu,"Aviso",.T.,4000)
	endif
	geraLog( "FIM TROCA EMPRESA/FILIAL" )
	geraLog( Replicate('-',60) )

	SetFunName(cFunBkp)
return


//-----------------------------------------------------------------
/*/{Protheus.doc} VldEnd
Funcao para validar Endereço

@type		Function
@param		cEndereco,	Caracter,	Endereço a ser validado
@author		Gerson Schiavo
@since 		14/04/2016
@Return		lRet Variavel logica, indicando se o endereço foi validado ou não (.T./.F.)
/*/
//-----------------------------------------------------------------
Static Function VldEnd(cEndereco)

	Local lRet:= .t.

	if !Empty(cEndereco)

		dbSelectArea("SBE")
		SBE->(DbSetOrder(1))
		If !dbSeek(xFilial("SBE")+cEndereco)
			VTBEEP(2)
			VTAlert("Endereço Invalido","Aviso",.T.,4000)
			VTClearGet("cEndOrig")
			VTClearBuffer()
			VTKeyBoard(chr(20))
			lRet:= .f.
		EndIf
	else
		VTBEEP(2)
		VTAlert("Endereço Invalido","Aviso",.T.,4000)
		VTClearGet("cEndOrig")
		VTClearBuffer()
		VTKeyBoard(chr(20))
		lRet:= .f.
	endif

//VTClearGet("cEndOrig")

Return lRet

//-----------------------------------------------------------------
/*/{Protheus.doc} VldEti
Funcao para validar Etiqueta

@type		Function
@param		cEtiqueta,	Caracter,	Etiqueta a ser validada
@param		aLista	,	Array	,	Array de Dados exibidos no Browse
@param		cOpcao	,	Caracter,	Tipo da Opção selecionada
@author		Gerson Schiavo
@since 		14/04/2016
@Return		lRet Variavel logica, indicando se a etiqueta foi validada ou não (.T./.F.)
/*/
//-----------------------------------------------------------------
Static Function VldEti(cEtiqueta,aLista,cOpcao,cEndOrig)
	Local cAlias 	:= GetNextAlias()
	Local lRet		:= .t.
	Local nQtdInt	:= GetNewPar("ZZ_TAMCBAR",5)											// Parte inteira da quantidade
	Local nQtdDec	:= GetNewPar("ZZ_DECCBAR",2)											// Parte decimal da quantidade
	Local cAuxEti	:= AllTrim(cEtiqueta)													// Etiqueta lida
	Local cProduto	:= Alltrim(SubStr(cAuxEti,1,11))										// Produto Lido
	Local cLote		:= AllTrim(SubStr(cAuxEti,12,10))										// Lote Lido
	Local cLocOri	:= Substr(cEndOrig,1,2)
	Local cEnder	:= Substr(cEndOrig,3,15)
	Local nQuant	:= Val(SubStr(cAuxEti,22,nQtdInt) + "." + SubStr(cAuxEti,27,nQtdDec))	// Fausto Costa - 27/09/2018

	local aTmp		:= {}
	local nPosicao	:= 0
	local nItem		:= 0

	dbSelectArea("SB8")
	SB8->(DbSetOrder(3))
	If SB8->(dbSeek(xFilial("SB8")+cProduto+"    "+cLocOri+cLote))
		If SB8->B8_SALDO == 0
			VTAlert("Saldo do Produto/Lote na SB8 zerado!","Aviso",.T.,4000)
		Else
			nQuant := SB8->B8_SALDO
		EndIf
	EndIf

	If Empty(cProduto) .or. nQuant = 0
		VTBEEP(2)
		VTAlert("Etiqueta Invalida","Aviso",.T.,4000)
		VTClearBuffer()
		VTKeyBoard(chr(20))
		VTClearGet("cEtiqueta")
		Return(.f.)
	EndIf

	dbSelectArea("SB1")
	If dbSeek(xFilial("SB1")+cProduto)

		If(Select(cAlias) > 0)
			(cAlias)->(DBCloseArea())
		EndIf

		cQuery := " SELECT " + CRLF
		cQuery += " 	COUNT(*) AS QTDREG " + CRLF
		cQuery += " FROM " + CRLF
		cQuery += " 	" + RetSqlName("SBF") + "   " + CRLF
		cQuery += " WHERE " + CRLF
		cQuery += "     BF_FILIAL  = '" + xFilial("SBF") +"'	AND "+ CRLF
		cQuery += "     BF_PRODUTO = '" + SB1->B1_COD + "'		AND	"+ CRLF
		cQuery += "     BF_LOCAL   = '" + cLocOri + "'			AND	"+ CRLF
		cQuery += "     BF_LOCALIZ = '" + cEnder + "'			AND	"+ CRLF
		cQuery += "     BF_LOTECTL = '" + cLote + "'			AND	"+ CRLF
		cQuery += " 	D_E_L_E_T_ = ' ' " + CRLF

		TcQuery cQuery Alias &cAlias New

		If (cAlias)->QTDREG = 0
			lRet:= .f.
		Endif
	Else
		lRet:= .f.
	Endif

	If lRet
		If cOpcao = "I"  //Inclusão
			nPosicao			:= aScan( aDadosTemp, { |x| AllTrim(x[P_PRODUTO]) == alltrim(cProduto) .and. AllTrim(x[P_LOTE]) == alltrim(cLote) } )
			If nPosicao > 0
				VTAlert("Etiqueta Ja foi Lida!","Aviso",.T.,4000)
				VTClearGet("cEtiqueta")
				lRet:= .f.
			else
				aTmp		:= {}
				aAdd( aTmp , substr(cEndOrig,3,15) ) //P_ENDERECO
				aAdd( aTmp , cProduto )
				aAdd( aTmp , nQuant )
				aAdd( aTmp , clote )
				aAdd( aTmp , SubStr(cEndOrig,1,2) )
				aAdd( aTmp , Iif(SB1->B1_TIPCONV = "M", (nQuant * SB1->B1_CONV), (nQuant / SB1->B1_CONV) ) )
				aAdd( aDadosTemp, aTmp )
				aTmp		:= {}
				nQtdVol 	+= 1
			endif
		Else
			nPosicao			:= aScan( aDadosTemp, { |x| AllTrim(x[P_PRODUTO]) == alltrim(cProduto) .and. AllTrim(x[P_LOTE]) == alltrim(cLote) } )
			If nPosicao > 0
				aTmp		:= {}
				nQtdVol 	-= 1
				for nItem := 1 to len(aDadosTemp)
					If !(nItem == nPosicao)
						aAdd( aTmp, aClone(aDadosTemp[nItem]) )
					endif
				next
				aDadosTemp	:= aClone(aTmp)
				aTmp		:= {}
			else
				VTAlert("Etiqueta Nao foi Lida!","Aviso",.T.,4000)
				VTClearGet("cEtiqueta")
				lRet:= .f.
			endif
		Endif
	Else
		VTBEEP(2)
		VTAlert("Etiqueta Invalida","Aviso",.T.,4000)
		VTClearBuffer()
		VTKeyBoard(chr(20))
	Endif

	VTClearGet("cEtiqueta")

Return lRet

//-----------------------------------------------------------------
/*/{Protheus.doc} VldFil
Funcao para validar Empresa/Filial

@type		Function
@param		cFilDest,	Caracter, Filial a ser validada
@author		Gerson Schiavo
@since 		14/04/2016
@Return		lRet Variavel logica, indicando se a empresa foi validada ou não (.T./.F.)
/*/
//-----------------------------------------------------------------
Static Function VldFil(cFilDest)

	Local lRet:= .t.

	if !Empty(cFilDest)

		dbSelectArea("SM0")
		dbGotop()
		if !dbSeek(cFilDest,.t.)
			lRet:= .f.
		EndIf
	else
		lRet:= .f.
	endif

	if !lRet
		VTBEEP(2)
		VTAlert("Empresa / Filial Invalida","Aviso",.T.,4000)
		VTClearGet("cFilDest")
		VTClearBuffer()
		VTKeyBoard(chr(20))
	endif

Return lRet

//-----------------------------------------------------------------
/*/{Protheus.doc} NOVOENDER
Funcao para Novo Endereço  [CTRL + E]

@type		Function
@param		aLista,	Array	,Array de Dados exibidos no Browse
@author		Gerson Schiavo
@since 		14/04/2016
@Return		nil nulo
/*/
//-----------------------------------------------------------------
Static Function NOVOENDER(aLista)

	Local aSave		:= VTSAVE()


	If DLVTAviso(FunDesc(),"Confirma Novo Endereco ?",{"Sim","Nao"}) == 1
		VTClear()
		@ 000,000 VTSAY "Transferencia Entre Filiais"
		@ 1,00 VTSAY "ENDERECO ORIGEM:"
		@ 2,00 VTGet cEndOrig  pict '@!' Valid VldEnd(cEndOrig)
		VTRead()
	endif

	VTClear()
	VTRestore(,,,,aSave)

Return

//-----------------------------------------------------------------
/*/{Protheus.doc} ESTORNA
Funcao para Estorno [CTRL + X]

@type		Function
@param		aLista,	Array	,Array de Dados exibidos no Browse
@author		Gerson Schiavo
@since 		14/04/2016
@Return		nil nulo
/*/
//-----------------------------------------------------------------
Static Function ESTORNA(aLista)

	Local aSave		:= VTSAVE()

	If VTYesNo("Confirma Estorno ?","Atencao",.T.)

		VTClear()
		@ 000,000 VTSAY "Estorno da Etiqueta"
		@ 2,00 VTSAY "ETIQUETA:"
		@ 3,00 VTGet cEtiqueta  pict '@!' Valid VldEti(cEtiqueta,aLista,"E",cEndOrig)
		VTRead()
	endif

	VTClearGet("cEtiqueta")

	VTClear()
	VTRestore(,,,,aSave)

Return

//-----------------------------------------------------------------
/*/{Protheus.doc} Consulta
Funcao para Consulta  [CTRL + I]

@type		Function
@param		aLista,	Array	,Array de Dados exibidos no Browse
@author		Gerson Schiavo
@since 		14/04/2016
@Return		nil nulo
/*/
//-----------------------------------------------------------------
Static Function Consulta(aLista)

	Local aCab			:= {"Produto","Lote","Qtd"}
	Local aTam			:= {15,10,12}
	Local aSave			:= VTSAVE()
	local nItem			:= 0

	aLista:= {}

	for nItem := 1 to len(aDadosTemp)
		aadd(aLista,{Alltrim( aDadosTemp[nItem,P_PRODUTO] ),Alltrim( aDadosTemp[nItem,P_LOTE] ), aDadosTemp[nItem,P_QUANT] })
	next

	VTCLEAR()
	VTaBrowse(0,0,7,40,aCab,aLista,aTam)
	VTRestore(,,,,aSave)

Return


//-----------------------------------------------------------------
Static Function fDadosCompl(aDadosTransf)

	Local aTela				:= {}

	Private cTransp			:= Space(06)
	Private cPlaca			:= Space(07)
	Private cUFPlaca		:= Space(02)
	Private nVolume			:= 00000
	Private nPesoLiq		:= 00000
	Private nPesoBrt		:= 00000

	RetDdCompl(aDadosTransf)

	aTela := VTSave()

	VTClear()
	@ 000,000 VTSAY "Dados Complementares da Nota Fiscal"
	@ 001,000 VTSAY "TRANSPORTADORA:"
	@ 002,000 VTGet cTransp  pict '@!' Valid fVldTransp(cTransp) .And. !Empty(cTransp) F3 'SA4'
	@ 003,000 VTSAY "PLACA:"
	@ 004,000 VTGet cPlaca pict '@!'  Valid fVldPlaca(cPlaca) .and. !Empty(cPlaca) // Fausto Costa - 15/10/2019 - Chamado 12617
	@ 005,000 VTSAY "UF:"
	@ 006,000 VTGet cUFPlaca pict '@!' Valid fVldUF(cUFPlaca) .and. !Empty(cUFPlaca)
	VTRead()

	If nVolume == 0 .OR. nPesoLiq == 0 .OR.  nPesoBrt == 0
		VTAlert("Volume ou peso estao zerados, favor informar esses dados:","Aviso",.T.,4000)
		VTClear()
		@ 000,000 VTSAY "Dados Complementares da Nota Fiscal"
		@ 001,000 VTSAY "VOLUME:"
		@ 002,000 VTGet nVolume  pict '@E 99999'
		@ 003,000 VTSAY "PESO LIQUIDO:"
		@ 004,000 VTGet nPesoLiq  pict '@E 99999.99'
		@ 005,000 VTSAY "PESO BRUTO:"
		@ 006,000 VTGet nPesoBrt  pict '@E 99999.99'
		VTRead()
	EndIf

	aDadComplem		:= {}
	aAdd( aDadComplem, cTransp ) 		//POS_TRANSP
	aAdd( aDadComplem, cPlaca )			//POS_PLACA
	aAdd( aDadComplem, cUFPlaca )		//POS_UFPLAC
	aAdd( aDadComplem, nVolume )		//POS_VOLUME
	aAdd( aDadComplem, nPesoLiq )		//POS_PESLIQ
	aAdd( aDadComplem, nPesoBrt )		//POS_PESBRU

Return


/*
Fausto Costa
08/07/2022
*/
Static Function RetDdCompl(aDadosTransf)
Local nX 		:= 1
Local cAlias	:= GetNextAlias()
Local cQry			:= ""

If(Select(cAlias) > 0)
	(cAlias)->(DBCloseArea())
EndIf

For nX := 1 To Len(aDadosTransf)
	cQry:= "SELECT SUM(SB8.B8_ZZPLIQ) AS PESOLIQ, SUM(SB8.B8_ZZPBRUT) AS PESOBRU, COUNT(*) AS QTDVOL, SUM(SB8.B8_SALDO2) AS SALDO2 " + CRLF
	cQry+= "FROM "+RetSqlName("SB8")+" SB8 WITH (NOLOCK) " + CRLF
	cQry+= "WHERE SB8.D_E_L_E_T_ = '' AND SB8.B8_FILIAL = '"+aDadosTransf[nX,1]+"' AND SB8.B8_PRODUTO = '"+aDadosTransf[nX,2]+"' AND SB8.B8_LOCAL = '"+aDadosTransf[nX,3]+"' AND SB8.B8_LOTECTL = '"+aDadosTransf[nX,17]+"' " + CRLF

	TcQuery cQry Alias &cAlias New

	nPesoBrt	+= (cAlias)->PESOBRU
	nPesoLiq	+= (cAlias)->PESOLIQ
	nVolume		+= (cAlias)->QTDVOL

	If(Select(cAlias) > 0)
		(cAlias)->(DBCloseArea())
	EndIf
Next

Return


//-----------------------------------------------------------------
/*/{Protheus.doc} Finaliza
Funcao para Finalizar Processo

@type		Function
@author		Gerson Schiavo
@since 		14/04/2016
@Return		lRet Variavel logica, indicando se possível realizar a transferencia (.T./.F.)
/*/
//-----------------------------------------------------------------
Static Function Finaliza()
	Local aParam310  	:= Array(30)
	Local aSeries		:= {}
	Local aNotas		:= {}
	Local aNFe			:= {}
	Local aDadosTransf	:= {}
	Local cLocDest		:= CRIAVAR("B1_LOCPAD")
	Local cTipo			:= CRIAVAR("B1_TIPO")
	Local cGrupo		:= CRIAVAR("B1_GRUPO")
	Local cNcm			:= CRIAVAR("B1_POSIPI")
	Local cCliente		:= CRIAVAR("A1_COD")
	Local cLojaCli		:= CRIAVAR("A1_LOJA")
	Local cFornece		:= CRIAVAR("A2_COD")
	Local cLojaFor		:= CRIAVAR("A2_LOJA")
	Local dDtValid		:= CRIAVAR("B8_DTVALID")
	Local cSerie		:= SuperGetMv("ZZ_SERIE",.f.)
	Local nTolPrc		:= SuperGetMv("ZZ_TOLPRCM",.F.)
	Local lRet			:= .F.
	Local lGravou		:= .T.
	Local oTMsg  		:= nil
	Local lValNeg		:= .F.
	Local lValZero		:= .F.
	Local lTabPrc		:= .F.
	Local lPrcMaior		:= .F.
	Local lProdBlq		:= .F.
	Local nX			:= 0
	Local nVlrPrv		:= 0
	Local nTotPrv		:= 0
	Local cTpVal		:= ""
	Local cMens			:= "Produtos com valores negativos:"+chr(13)+chr(10)
	Local cMens1		:= "Produtos com valores zerados:"+chr(13)+chr(10)
	Local cMens2		:= "Grupo de Produto nao encontrado na Tabela de Preco:"+chr(13)+chr(10)
	Local cMens3		:= "Produtos com valores maior que o permitido:"+chr(13)+chr(10)
	Local cMens4		:= "Produtos Bloqueados:"+chr(13)+chr(10)

	local nItem			:= 0

	Private nPBrtACD := 0
	Private nPLiqACD := 0
	Private nVolACD := 0

	Private cCodMot		:= Space(11)

	cSerie:= padR(allTrim(cSerie),tamSx3("F2_SERIE")[1])

	AADD(aSeries,{cFilOri,cSerie,''})

	aParam310[14]:= 2 //Nota Fiscal Classificada
	aParam310[16]:= SuperGetMv("ZZ_CONDPAG",.f.)
	aParam310[19]:= 2 //Não Trata Poder de Terceiro
	aParam310[20]:= SuperGetMv("ZZ_ESPECIE",.f.)

	For nItem := 1 to len(aDadosTemp)

		dbSelectArea("SB1")
		dbSetOrder(1)
		If MsSeek(xFilial("SB1")+ aDadosTemp[nItem,P_PRODUTO] )
			cLocDest:= Substr(cEndDest,1,2) //SB1->B1_LOCPAD
			cTipo	:= SB1->B1_TIPO
			cGrupo	:= SB1->B1_GRUPO
			cNcm	:= LEFT(SB1->B1_POSIPI,4)
		EndIf

		//Fausto Costa - 24/09/2020 - Chamado 15846
		If !Empty(SB1->B1_CC)
			VtAlert("Produto nao poder estar com Centro de Custo informado no cadastro!")
			Return(.F.)
		EndIf

		// Busca Cliente da Filial Destino para gerar Pedido de Venda e Nota Fiscal de Saída
		DBSelectArea("SM0")
		DBSetOrder(1)
		DBGoTop()
		SM0->(DBSeek(cFilDest))

		dbSelectArea("SA1")
		dbSetOrder(3)
		if dbSeek(xFilial("SA1")+SM0->M0_CGC)
			cCliente:= SA1->A1_COD
			cLojaCli:= SA1->A1_LOJA
		endif

//  Busca Fornecedor da Filial Corrente para gerar Nota Fiscal de Entrada
		DBSelectArea("SM0")
		DBSetOrder(1)
		DBGoTop()
		SM0->(DBSeek(cEmpAnt+cFilOri))

		dbSelectArea("SA2")
		dbSetOrder(3)
		if dbSeek(xFilial("SA2")+SM0->M0_CGC)
			cFornece:= SA2->A2_COD
			cLojaFor:= SA2->A2_LOJA
		endif

		cFilAnt:= cFilOri
		dDtValid:= Posicione("SB8",2,xFilial("SB8") + Space(TamSx3("B8_NUMLOTE")[1]) + aDadosTemp[nItem,P_LOTE] + aDadosTemp[nItem,P_PRODUTO] ,"B8_DTVALID")

		dbSelectArea("ZZA")
		dbSetOrder(3)  //NCM
		if dbSeek(xFilial("ZZA")+cFilOri+Substr(cFilDest,3,4)+cTipo+cNcm)
			cTesSai			:= ZZA->ZZA_TESSAI
			cTesEnt			:= ZZA->ZZA_TESENT
			aParam310[17]	:= VAL(ZZA->ZZA_PRECPV)
			cTpVal			:= ZZA->ZZA_PRECPV
		else
			dbSelectArea("ZZA")
			dbSetOrder(3) //TIPO
			if dbSeek(xFilial("ZZA")+cFilOri+Substr(cFilDest,3,4)+cTipo+"    ")
				cTesSai			:= ZZA->ZZA_TESSAI
				cTesEnt			:= ZZA->ZZA_TESENT
				aParam310[17]	:= VAL(ZZA->ZZA_PRECPV)
				cTpVal			:= ZZA->ZZA_PRECPV
			else
				VTAlert("Verifique Cadastro de TES X FILIAIS para o Tipo de Produto "+Alltrim(cTipo) ,"Aviso",.T.,4000)
				Return(.f.)
			endif
		endif

		// Fausto Costa - 16/12/2020 - Ajuste para que o Produto Recuperado busque o valor da Tabela de Preço
		If Alltrim(cGrupo) == "A004"
			aParam310[17]	:= 1
			cTpVal			:= "1"
		EndIf

	/* Fausto Costa - 06/02/2019 - 1 Tabela de Preço / 2 Custo STD / 3 Ultimo Preco / 4 Custo Unitario */
		If cTpVal == "4"
			DbSelectArea("SB2")
			SB2->(DbSetOrder(1))
			IF SB2->(DbSeek(cFilOri+SB1->B1_COD+SB1->B1_LOCPAD))
				nVlrPrv:= ROUND(SB2->B2_CM1,2)
				nTotPrv:= NOROUND( aDadosTemp[nItem,P_QUANT] * nVlrPrv,2)

				If nVlrPrv == 0
					lValZero := .T.
					cMens1 += Alltrim(SB1->B1_COD)+" - "+Alltrim(SUBSTR(SB1->B1_DESC,1,30))+": "+STR(nVlrPrv)+chr(13)+chr(10)
				ElseIf nVlrPrv < 0
					lValNeg := .T.
					cMens += Alltrim(SB1->B1_COD)+" - "+Alltrim(SUBSTR(SB1->B1_DESC,1,30))+": "+STR(nVlrPrv)+chr(13)+chr(10)
				EndIf
			Else
				VtAlert("PRODUTO SEM CUSTO MEDIO!"+chr(13)+chr(10)+"VERIFICAR COM DEPTO CONTROLADORIA","Aviso",.T.)
				Return(.f.)
			EndIf
		Else
			If SB1->B1_MSBLQL == "1"
				lProdBlq := .T.
				cMens4 += Alltrim(SB1->B1_COD)+" - "+Alltrim(SB1->B1_DESC)+chr(13)+chr(10)
			EndIf

			If Alltrim(SB1->B1_UM) == "KG"
				nVlrPrv:= fBuscaVlr(SA1->A1_TABELA,SB1->B1_GRUPO)
				nTotPrv:= NOROUND( aDadosTemp[nItem,P_QUANT] * nVlrPrv,2)

				If nVlrPrv == 0
					lTabPrc := .T.
					cMens2 += "Grupo: "+Alltrim(SB1->B1_GRUPO)+" - Tabela de Preco: "+Alltrim(SA1->A1_TABELA)+chr(13)+chr(10)
				EndIf
			Else
				nVlrPrv:= ROUND(CONVUM(SB1->B1_COD,fBuscaVlr(SA1->A1_TABELA,SB1->B1_GRUPO),0,2),2)
				nTotPrv:= NOROUND( aDadosTemp[nItem,P_QUANT] * nVlrPrv,2)

				If nVlrPrv == 0
					lTabPrc := .T.
					cMens2 += "Grupo: "+Alltrim(SB1->B1_GRUPO)+" - Tabela de Preco: "+Alltrim(SA1->A1_TABELA)+chr(13)+chr(10)
				EndIf
			EndIf
		EndIf

		If nVlrPrv > nTolPrc
			lPrcMaior := .T.
			cMens3 += Alltrim(SB1->B1_COD)+" - "+Alltrim(SUBSTR(SB1->B1_DESC,1,30))+": "+STR(nVlrPrv)+chr(13)+chr(10)
		EndIf

		AADD(aDadosTransf,{	cFilOri				, aDadosTemp[nItem,P_PRODUTO]	, aDadosTemp[nItem,P_LOCAL]	, aDadosTemp[nItem,P_QUANT]				,;
			aDadosTemp[nItem,P_QTD2UM]			, Substr(cFilDest,3,4)			, cLocDest		, cCliente					,;
			cLojaCli			, cFornece						, cLojaFor		, ""						,;
			""					, ""							, ""			, ""						,;
			Alltrim( aDadosTemp[nItem,P_LOTE] )	, ""							, dDtValid		, Alltrim( aDadosTemp[nItem,P_ENDERECO] )	,;
			""					, ""							, cTesSai		, cTesEnt					,;
			nVlrPrv				, nTotPrv} )

	next

	If lValNeg .OR. lValZero .OR. lTabPrc .OR. lProdBlq
		If lValNeg
			cMens += chr(13)+chr(10)+"VERIFICAR COM DEPTO CONTROLADORIA"
			VtAlert(cMens,"Aviso",.T.)
		EndIf

		If lValZero
			cMens1 += chr(13)+chr(10)+"VERIFICAR COM DEPTO CONTROLADORIA"
			VtAlert(cMens1,"Aviso",.T.)
		EndIf

		If lTabPrc
			cMens2 += chr(13)+chr(10)+"VERIFICAR COM DEPTO CONTROLADORIA"
			VtAlert(cMens2,"Aviso",.T.)
		EndIf

		If lPrcMaior
			cMens3 += chr(13)+chr(10)+"VERIFICAR COM DEPTO CONTROLADORIA"
			VtAlert(cMens3,"Aviso",.T.)
		EndIf

		If lProdBlq
			cMens4 += chr(13)+chr(10)+"VERIFICAR COM DEPTO PROCESSOS"
			Alert(cMens4)
		EndIf

		Return(.F.)
	EndIf

	fDadosCompl(aDadosTransf)

	Begin Transaction

		if Len(aDadosTransf) > 0

			nModuloOld  := nModulo
			nModulo     := 4

			VtMsg("Realizando Transferencia...")
			lRet := fGeraPedido(aDadosTransf,cCliente,cLojaCli,cFornece,cLojaFor, SA1->A1_TABELA)  //A310Proc(aDadosTransf,aParam310,aSeries,@aNotas)

			nModulo:= nModuloOld

			If lRet
				// Fausto Costa - 14/05/2020
				//u_RetPesoACD(cFilOri,cNota,cSerie,cCliente,cLojaCli)

				While .T.
					/* Fausto Costa - 14/05/2020
					If nPLiqACD == 0
						nVolume 	:= nVolComp
						nPesoLiq	:= nPLiqComp
						nPesoBrt	:= nPBrtComp
					Else
						nVolume 	:= nVolACD
						nPesoLiq	:= nPLiqACD
						nPesoBrt	:= nPBrtACD
					EndIf
					*/

					/* Fausto Costa - 08/07/2022
					If VTYesNo("Gravar Dados Complementares ?","Atencao",.T.)
					*/
						lGravou:= GravaDadosCompl( cNota,cSerie )

						if lGravou

							VTClear()
							VtMsg("Realizando Transferencia...")


							DBSelectArea("SM0")
							DBSetOrder(1)
							DBGoTop()
							SM0->(DBSeek(cEmpAnt+cFilOri))

							dbSelectArea("SF2")
							SF2->(DbSetOrder(1))
							If dbSeek(xFilial("SF2")+cNota+cSerie)
								oTMsg := FswTemplMsg():TemplMsg("S",SF2->F2_DOC,SF2->F2_SERIE,SF2->F2_CLIENTE,SF2->F2_LOJA)

								&& Grava as mensagens da nota fiscal
								oTMsg:carMsg(.f.)
								oTMsg:grvMsg()

								AADD(aNFe,{SF2->F2_FILIAL,SF2->F2_SERIE,SF2->F2_DOC,SF2->F2_FILIAL+SF2->F2_DOC+SF2->F2_SERIE+SF2->F2_CLIENTE+SF2->F2_LOJA})

							EndIf
							exit
						else
							loop
						endif
					/* Fausto Costa - 08/07/2022
					else
						loop
					endif
					*/
					VtRestore(,,,,aTela)
				enddo
			else
				VTAlert("Transferencia Nao Realizada","Aviso",.T.,3000)
				DisarmTransaction()
				Return(.f.)
			endif

			If lRet
				lRet:= fGeraNFE(aDadosTransf,cFornece,cLojaFor,cNota,cSerie,SA1->A1_TABELA,cCliente, cLojaCli)
				If lRet
					For nX:= 1 to Len(aNFe)
						AADD(aNotas, {Substr(cFilDest,3,4),cNota,cSerie})
					Next nX

					U_Endereca(aNotas,cFornece,cLojaFor,Substr(cEndDest,3,7),@lRet)
				Else
					VTAlert("Transferencia Nao Realizada","Endereco",.T.,3000)
					DisarmTransaction()
					Return(.f.)
				EndIf
			Else
				VTAlert("Transferencia Nao Realizada","Transmissao",.T.,3000)
				DisarmTransaction()
				Return(.f.)
			EndIf
		else
			VTAlert("Transferencia Nao Realizada","Aviso",.T.,3000)
			DisarmTransaction()
			Return(.f.)
		endif
		VTAlert("Transferencia Realizada Nota Fiscal: "+Alltrim(cNota)+"/"+Alltrim(cSerie),"Aviso",.T.,4000)
	End Transaction

/* Fausto Costa - 09/06/2021 - Descontinuado função AutoNfeEnv na release 12.1.27
For nNF:= 1 to Len(aNFe)
	DBSelectArea("SM0")
	DBSetOrder(1)
	DBGoTop()
	SM0->(DBSeek(cEmpAnt+cFilOri))
	
	dbSelectArea("SF2")
	SF2->(dbSetOrder(1))
	If SF2->(dbSeek(aNFe[nNF,4]))
		If "9090" $ GETMV("MV_SPEDURL") // Fausto Costa - Verificar que esta apontado para o TSS da Produção
			AutoNfeEnv(cEmpAnt,SF2->F2_FILIAL,"3","1",SF2->F2_SERIE,SF2->F2_DOC,SF2->F2_DOC)
		Else
			AutoNfeEnv(cEmpAnt,SF2->F2_FILIAL,"3","2",SF2->F2_SERIE,SF2->F2_DOC,SF2->F2_DOC)
		EndIf
	EndIf
Next nNF		
*/

Return(lRet)


//-----------------------------------------------------------------
/*/{Protheus.doc} BuscaPeso
Funcao para buscar Peso Liquido e Peso Bruto

@type		Function
@author		Fausto Costa
@since 		14/11/2018
@Return		lRet Variavel logica
/*/
//-----------------------------------------------------------------
Static Function BuscaPeso(cProd,cLocal,cLoteCtl)
	Local aArea   	:= GetArea()
	Local aAreaSB8	:= GetArea("SB8")

	DbSelectArea("SB8")
	SB8->(DbSetOrder(3))
	If SB8->(DbSeek(xFilial("SB8")+cProd+cLocal+cLoteCtl))
		nPLiqComp += SB8->B8_ZZPLIQ
		nPBrtComp += SB8->B8_ZZPBRUT
	EndIf

	RestArea(aAreaSB8)
	RestArea(aArea)
Return

//-----------------------------------------------------------------
/*/{Protheus.doc} fVldTransp
Funcao para validar Transportadora

@type		Function
@param		cTransp
@author		Gerson Schiavo
@since 		13/09/2017
@Return		lRet Variavel logica
/*/
//-----------------------------------------------------------------
Static Function fVldTransp(cTransp)
	Local lRetTrans:= .t.

	SA4->(dbSetOrder(1))
	if !SA4->(dbSeek(xFilial("SA4")+cTransp))
		VTAlert("Transportadora Não Existe !","Aviso",.T.,3000)
		lRetTrans:= .f.
	endif

Return(lRetTrans)


//-----------------------------------------------------------------
/*/{Protheus.doc} fVldCodMot
Funcao para validar Codigo do Motorista

@type		Function
@author		Fausto Costa
@since 		13/05/2020
@Return		lRet Variavel logica
/*/
//-----------------------------------------------------------------
Static Function fVldCodMot(cCodMot)
	Local lRetCMot:= .T.

	DbSelectArea("DHB")
	DHB->(DbSetOrder(1))
	If !DHB->(DbSeek(xFilial("DHB")+cCodMot))
		VTAlert("Cod. Motorista Não Existe !","Aviso",.T.,3000)
		lRetCMot:= .F.
	EndIf

Return(lRetCMot)

//-----------------------------------------------------------------
/*/{Protheus.doc} GravaDadosCompl
Funcao para preencher dados complementares

@type		Function
@author		Fausto Costa
@since 		13/09/2017
/*/
//-----------------------------------------------------------------
Static Function GravaDadosCompl(cNotaFis,cSerFis)
	Local lRetGrv:= .T.

	dbSelectArea("SF2")
	SF2->(DbSetOrder(1))
	If dbSeek(xFilial("SF2")+cNotaFis+cSerFis)

		SF2->(RecLock("SF2", .F.))
		SF2->F2_TRANSP	:= aDadComplem[POS_TRANSP]
		SF2->F2_ZZPLACA	:= UPPER( aDadComplem[POS_PLACA] )
		SF2->F2_ZZUFPLA	:= UPPER( aDadComplem[POS_UFPLAC] )
		SF2->F2_VOLUME1	:= aDadComplem[POS_VOLUME]
		SF2->F2_PLIQUI	:= aDadComplem[POS_PESLIQ]
		SF2->F2_PBRUTO	:= aDadComplem[POS_PESBRU]
		SF2->(MsUnLock())

	else
		VTAlert("Nota Fiscal Não Encontrada !","Aviso",.T.,3000)
		lRetGrv:= .f.
	EndIf

Return lRetGrv


//-----------------------------------------------------------------
/*/{Protheus.doc} fVldPlaca
Funcao para validar Placa

@type		Function
@author		Fausto Costa
@since 		13/09/2017
@Return		lRet Variavel logica
/*/
//-----------------------------------------------------------------

Static Function fVldPlaca(cConteudo)
	Local cLetras	:= "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
	Local lRetorno	:= .t.
	Local x			:= 1

	If !Empty(cConteudo)
		For x:= 1 to Len(cConteudo)
			if !Substr(cConteudo,x,1) $ cLetras
				lRetorno:= .f.
				VTAlert("Placa Invalida !","Aviso",.T.,3000)
			endif
		Next x
	Endif

	If Len(cConteudo) <> 7
		lRetorno:= .f.
		VTAlert("Placa Invalida !","Aviso",.T.,3000)
	EndIf

return(lRetorno)


//-----------------------------------------------------------------
/*/{Protheus.doc} fVldUF
Funcao para validar UF da Placa

@type		Function
@author		Fausto Costa
@since 		13/09/2017
@Return		lRet Variavel logica
/*/
//-----------------------------------------------------------------
Static Function fVldUF(cUf)

	Local lRetorno:= .t.

	SX5->(DbSetOrder(1))
	If !SX5->(DbSeek(xFilial("SX5")+"12"+cUf))
		VTAlert("UF da Placa Invalido !","Aviso",.T.,3000)
		lRetorno:= .f.
	endif

Return(lRetorno)



	******************************************************************************************************************
	******************************************************************************************************************
//Função: fGeraPedido() - Gera Pedido de Venda de Beneficiamento                                                //
	******************************************************************************************************************
	******************************************************************************************************************
Static Function fGeraPedido(aDadosTransf,cCliente, cLojaCli, cFornece, cLojaFor, cTabPrc)
	Local aCab		:= {}
	Local aItens	:= {}
	Local cItemPed	:= "00"
	Local lRetPed	:= .t.
	Local lLiberOk 	:= .T.
	Local cTes		:= ""
	Local x			:= 1

	Private lMSHelpAuto := .t.
	Private lMSErroAuto := .f.


	aCab :={{"C5_TIPO"   ,"N"		,Nil},;
		{"C5_CLIENTE",cCliente	,Nil},;
		{"C5_LOJACLI",cLojaCli	,Nil},;
		{"C5_EMISSAO",dDataBase	,Nil},;
		{"C5_CONDPAG","001"		,Nil},;
		{"C5_MOEDA"  ,1 	    ,Nil},;
		{"C5_TIPOCLI","R"		,Nil},;
		{"C5_TIPLIB","2"		,Nil},;
		{"C5_INDPRES","1"		,Nil},;
		{"C5_TPFRETE","C"		,Nil} }

	For x:= 1 to Len(aDadosTransf)

		IncProc()

		SB1->(dbSetOrder(1))
		SB1->(dbSeek(xFilial("SB1")+aDadosTransf[x,2]))

		cItemPed := Soma1( cItemPed , 2 )

		AAdd(aItens,{	{"C6_ITEM"   ,cItemPed					,Nil},; // Numero do Item no Pedido
		{"C6_PRODUTO",aDadosTransf[x,2]			,Nil},; // Codigo do Produto
		{"C6_QTDVEN" ,aDadosTransf[x,4]			,Nil},; // Quantidade Vendida
		{"C6_QTDLIB" ,aDadosTransf[x,4]			,Nil},; // Quantidade Liberada
		{"C6_PRCVEN" ,aDadosTransf[x,25]		,Nil},; // Preco Unitario Liquido
		{"C6_PRUNIT" ,aDadosTransf[x,25]		,Nil},; // Preco Unitario
		{"C6_TES"    ,aDadosTransf[x,23]		,Nil},; // Tipo de Entrada/Saida do Item
		{"C6_LOCAL"  ,aDadosTransf[x,3]			,Nil},; // Almoxarifado
		{"C6_LOTECTL",aDadosTransf[x,17]		,Nil},; // Lote
		{"C6_LOCALIZ",aDadosTransf[x,20]		,Nil},; // Lote
		{"C6_CLI"    ,aDadosTransf[x,8]			,Nil},; // Cliente
		{"C6_LOJA"   ,aDadosTransf[x,9]			,Nil}}) // Loja do Cliente

		cTes:= aDadosTransf[x,23]

	Next x

	MSExecAuto({|x,y,z| Mata410(x,y,z)},aCab,aItens,3)

	if lMsErroAuto
		//MostraErro()
		VTAlert("Problema ao Incluir o Pedido!")
		DisarmTransaction()
		VTDispFile(NomeAutoLog(),.t.)
		lRetPed:= .f.
	else
		ConfirmSx8()
		//Executa a Liberação do Pedido de Venda
		MaAvLibPed(SC5->C5_NUM,.F.,.F.,@lLiberOk)
		If ( lLiberOk )
			SC5->(MaLiberOK({SC5->C5_NUM}, .F.))

			// Fausto Costa - 03/06/2020 - Criado tratativa para verificar se a liberação de estoque do Pedido de Venda ocorreu para o Lote Selecionado.
			If VerifSC9(xFilial("SC9"), SC5->C5_NUM)
				lRetPed:= fGeraNota(SC5->C5_NUM,cTes,aDadosTransf, cFornece, cLojaFor,cTabPrc,cCliente, cLojaCli)
			Else
				VTAlert("Problema na Liberação de Estoque do Pedido!")
				DisarmTransaction()
				lRetPed:= .F.
			EndIf
		else
			lRetPed:= .f.
		endIf
	endif

Return(lRetPed)


	******************************************************************************************************************
	******************************************************************************************************************
//Função: VerifSC9() - Verifica se a Liberação de Estoque ocorreu igual ao Pedido de Venda                                                               //
	******************************************************************************************************************
	******************************************************************************************************************
Static Function VerifSC9(cFilPV, cNumPV)
	Local lRet 		:= .T.
	Local cAlias	:= GetNextAlias()
	Local cQuery	:=	""

	If Select(cAlias) <> 0
		(cAlias)->(dbCloseArea())
	EndIf

//cQuery :=	"SELECT SC9.C9_FILIAL, SC9.C9_CLIENTE, SC9.C9_LOJA, SC9.C9_PEDIDO, SC9.C9_ITEM, SC9.C9_PRODUTO, SC6.C6_LOTECTL, SC9.C9_LOTECTL "+CRLF
	cQuery :=	"SELECT COUNT(*) AS QTD  "+CRLF
	cQuery +=	"FROM "+RetSqlName("SC6")+" SC6 "+CRLF
	cQuery +=	"LEFT JOIN "+RetSqlName("SB1")+" SB1 ON SB1.D_E_L_E_T_ = '' AND SC6.C6_PRODUTO = SB1.B1_COD "+CRLF
	cQuery +=	"LEFT JOIN "+RetSqlName("SC9")+" SC9 ON SC9.D_E_L_E_T_ = '' AND SC9.C9_FILIAL+SC9.C9_CLIENTE+SC9.C9_LOJA+SC9.C9_PEDIDO+SC9.C9_ITEM = SC6.C6_FILIAL+SC6.C6_CLI+SC6.C6_LOJA+SC6.C6_NUM+SC6.C6_ITEM "+CRLF
	cQuery +=	"WHERE SC6.D_E_L_E_T_ = '' AND SC9.C9_FILIAL = '"+cFilPV+"' AND SC9.C9_PEDIDO = '"+cNumPV+"' AND SC6.C6_LOTECTL <> SC9.C9_LOTECTL "+CRLF

	TcQuery cQuery New Alias &cAlias

	If (cAlias)->QTD == 0
		lRet := .T.
	Else
		lRet := .F.
	EndIf

Return lRet


	******************************************************************************************************************
	******************************************************************************************************************
//Função: fGeraNota() - Gera Nota Fiscal de Saída                                                               //
	******************************************************************************************************************
	******************************************************************************************************************
Static Function fGeraNota(cPedido,cTes,aDadosTransf, cFornece, cLojaFor,cTabPrc,cCliente, cLojaCli)
	Local cAlias		:= GetNextAlias()
	Local cQuery		:=	""
	Local aPvlNfs 		:= {}
	Local cSerie  		:= "55 "
	Local lRetNota		:= .t.

	If Select(cAlias) <> 0
		(cAlias)->(dbCloseArea())
	Endif

	cQuery :=	"SELECT 		 "+CRLF
	cQuery +=		"C9_PEDIDO	,"+CRLF
	cQuery +=		"C9_ITEM	,"+CRLF
	cQuery +=		"C9_SEQUEN	,"+CRLF
	cQuery +=		"C9_QTDLIB	,"+CRLF
	cQuery +=		"C9_PRODUTO	,"+CRLF
	cQuery +=		"C9_PRCVEN	,"+CRLF
	cQuery +=		"C9_LOCAL	,"+CRLF
	cQuery +=		"R_E_C_N_O_  "+CRLF
	cQuery += 	" FROM " + RetSqlName( "SC9" ) + "  "+CRLF
	cQuery +=	"WHERE "+CRLF
	cQuery +=		"D_E_L_E_T_ = '' "+CRLF
	cQuery +=		"AND C9_FILIAL  = '" + xFilial("SC9") + "'"+CRLF
	cQuery +=		"AND C9_PEDIDO  = '" + cPedido + "' "+CRLF
	cQuery += "ORDER BY C9_ITEM, C9_PRODUTO "+CRLF

	TcQuery cQuery New Alias &cAlias

	(cAlias)->(dbGotop())
	While (cAlias)->(!Eof())

		SC5->(dbSeek(xFilial("SC5") + (cAlias)->C9_PEDIDO ))
		SC6->(dbSeek(xFilial("SC6") + (cAlias)->C9_PEDIDO+(cAlias)->C9_ITEM ))
		SE4->(dbSeek(xFilial("SE4") + "010"))
		SB1->(dbSeek(xFilial("SB1") + (cAlias)->C9_PRODUTO ))
		SB2->(dbSeek(xFilial("SB2") + (cAlias)->C9_PRODUTO+(cAlias)->C9_LOCAL ))
		SF4->(dbSeek(xFilial("SF4") + cTes))

		Aadd(aPvlNfs,{ 	(cAlias)->C9_PEDIDO,;
			(cAlias)->C9_ITEM,;
			(cAlias)->C9_SEQUEN,;
			(cAlias)->C9_QTDLIB,;
			(cAlias)->C9_PRCVEN,;
			(cAlias)->C9_PRODUTO,;
			.f.,;
			(cAlias)->R_E_C_N_O_,;
			SC5->(RecNo()),;
			SC6->(RecNo()),;
			SE4->(RecNo()),;
			SB1->(RecNo()),;
			SB2->(RecNo()),;
			SF4->(RecNo())})

		(cAlias)->(dbSkip())
	EndDo

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³Retorna o pergunte da MATA461                         ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	Pergunte("MT460A",.F.)

	If Len(aPvlNfs) > 0
		While .T.
			If LockByName("EMBRASA",.F.,.F.,.T.)
				cNota := MaPvlNfs(aPvlNfs,cSerie,.F.,.F.,.F.,.T.,.F.,0,0,.F.,.F.)
			/* cNota := MaPvlNfs(aPvlNfs,cSerie,.F.,.F.,.F.,.T.,.F.,0,0,.T.,.F.) // Ajuste para não gerar SR7 - Fausto Costa - 23/08/2018 */
				UnLockByName("A461NUMNF",.T.,!Empty(xFilial("SD9")),.T.)

				//Destrava Numeração da Nota Fiscal//
				SX5->(MsUnlock())

				aPvlNfs := {}

				Exit
			ElseIf LockByName("ACDTRANS01",.F.,.F.,.T.)
				cNota := MaPvlNfs(aPvlNfs,cSerie,.F.,.F.,.F.,.T.,.F.,0,0,.F.,.F.)
			/* cNota := MaPvlNfs(aPvlNfs,cSerie,.F.,.F.,.F.,.T.,.F.,0,0,.T.,.F.) // Ajuste para não gerar SR7 - Fausto Costa - 23/08/2018 */
				UnLockByName("A461NUMNF",.T.,!Empty(xFilial("SD9")),.T.)

				//Destrava Numeração da Nota Fiscal//
				SX5->(MsUnlock())

				aPvlNfs := {}

				Exit
			Else
				VtMsg("SEMÁFORO! Nota fiscal sendo faturada, aguarde!")
			EndIf
		EndDo
	EndIf

	if !Empty(cNota)
		lRetNota:= .t. //fGeraNFE(aDadosTransf,cFornece,cLojaFor,cNota,cSerie,cTabPrc,cCliente, cLojaCli)
	else
		lRetNota:= .f.
	endif

Return(lRetNota)


	************************************************************************************************************
	************************************************************************************************************
//Função: fGeraNFE() - Gera Nota Fiscal de Entrada 
	************************************************************************************************************
	************************************************************************************************************
Static Function fGeraNFE(aDadosTransf,cFornece,cLojaFor,cNota,cSerie,cTabPrc,cCliente, cLojaCli)

	Local aSB8  	:= SB8->(GetArea())
	Local aAux		:= {}
	Local aCab		:= {}
	Local aItens	:= {}
	Local lRetNfe	:= .t.
	Local cFilOld	:= cFilAnt
	Local nLtGAP	:= ""
	Local nPLiq		:= 0
	Local nPBruto	:= 0
	Local nSldOri	:= 0
	Local cDocOri	:= ""
	Local nAux		:= 1
	Local y			:= 1

	Private lMSHelpAuto := .t.
	Private lMSErroAuto := .f.

	nModuloOld  := nModulo
	nModulo     := 2

	cFilAnt:= Substr(cFildest,3,4)

	DBSelectArea("SM0")
	DBSetOrder(1)
	DBGoTop()
	SM0->(DBSeek(cEmpAnt+cFilAnt))

	SA2->(dbSetOrder(1))
	SA2->(dbSeek( xFilial("SA2") + cFornece+cLojaFor ))

	aCab	 := {	{"F1_DOC"		, cNota						,NIL},;
		{"F1_SERIE"		, cSerie					,NIL},;
		{"F1_FORNECE"	, cFornece					,NIL},;
		{"F1_LOJA"	    , cLojaFor					,NIL},;
		{"F1_EMISSAO"	, dDataBase					,NIL},;
		{"F1_EST"		, SA2->A2_EST				,NIL},;
		{"F1_TIPO"		, "N"						,NIL},;
		{"F1_DTDIGIT"	, dDataBAse					,Nil},;
		{"F1_FILORIG"	, cFilOri					,Nil},;
		{"F1_CLIORI"	, cCliente					,Nil},;
		{"F1_LOJAORI"	, cLojaCli					,Nil},;
		{"F1_FORMUL"	, ""						,NIL},;
		{"F1_ESPECIE"	, "SPED"		 			,NIL} }

	For y := 1 to Len(aDadosTransf)

		SF4->(dbSetOrder(1))
		SF4->(dbSeek( xFilial("SF4") + aDadosTransf[y,24] ))

		SB1->(dbSetOrder(1))
		SB1->(dbSeek( xFilial("SB1") + aDadosTransf[y,2] ))

		aAux:= {}
	/*
	aAdd(aAux, 	{'D1_COD'    	, Alltrim(aDadosTransf[y,2])		, NIL} ) 
	aAdd(aAux, 	{'D1_UM'    	, SB1->B1_UM						, NIL} ) 
	aAdd(aAux, 	{'D1_QUANT'  	, aDadosTransf[y,4]					, NIL} )
	aAdd(aAux, 	{'D1_VUNIT'  	, aDadosTransf[y,25]				, NIL} )
	aAdd(aAux, 	{'D1_TOTAL'  	, aDadosTransf[y,26]				, NIL} )
	aAdd(aAux, 	{'D1_TES'    	, aDadosTransf[y,24]				, NIL} )
	aAdd(aAux, 	{'D1_CF'    	, SF4->F4_CF						, NIL} )
	aAdd(aAux, 	{'D1_FORNECE'  	, cFornece							, NIL} )
	aAdd(aAux, 	{'D1_LOJA'  	, cLojaFor							, NIL} )
	aAdd(aAux, 	{'D1_LOCAL'  	, aDadosTransf[y,7]					, NIL} )
	aAdd(aAux, 	{'D1_LOTECTL'  	, Alltrim(aDadosTransf[y,17])		, NIL} )
	aAdd(aAux, 	{'D1_DTVALID'  	, aDadosTransf[y,19]				, NIL} )
	*/
		aAdd(aAux, 	{'D1_COD'    	, Alltrim(aDadosTransf[y,2])		, NIL} )
		aAdd(aAux, 	{'D1_UM'    	, SB1->B1_UM						, NIL} )
		aAdd(aAux, 	{'D1_QUANT'  	, aDadosTransf[y,4]					, NIL} )
		aAdd(aAux, 	{'D1_VUNIT'  	, aDadosTransf[y,25]				, NIL} )
		aAdd(aAux, 	{'D1_TOTAL'  	, aDadosTransf[y,26]				, NIL} )
		aAdd(aAux, 	{'D1_TES'    	, aDadosTransf[y,24]				, NIL} )
		aAdd(aAux, 	{'D1_FORNECE'  	, cFornece							, NIL} )
		aAdd(aAux, 	{'D1_LOJA'  	, cLojaFor							, NIL} )
		aAdd(aAux, 	{'D1_LOCAL'  	, aDadosTransf[y,7]					, NIL} )
		aAdd(aAux, 	{'D1_LOTECTL'  	, Alltrim(aDadosTransf[y,17])		, NIL} )
		aAdd(aAux, 	{'D1_DTVALID'  	, aDadosTransf[y,19]				, NIL} )

		aAdd(aItens, aAux )

	Next y


	if Len(aCab) > 0 .and. Len(aItens) > 0

		lMsErroAuto:= .f.

		MsExecAuto({| x, y, z| mata103( x, y, z)}, aCab, aItens, 3)

		If lMsErroAuto
			lRetNfe:= .f.
			VTDispFile(NomeAutoLog(),.t.)
			DisarmTransaction()
		Else

			For nAux:= 1 to Len(aDadosTransf)
				//Informações Origem
				DbSelectArea("SB8")
				SB8->(DbSetOrder(3))
				If SB8->(DbSeek(Alltrim(aDadosTransf[nAux,1])+Alltrim(aDadosTransf[nAux,2])+"    "+aDadosTransf[nAux,3]+Alltrim(aDadosTransf[nAux,17])))
					nLtGAP	:= SB8->B8_ZZLTGAP
					nPLiq	:= SB8->B8_ZZPLIQ
					nPBruto	:= SB8->B8_ZZPBRUT
					nSldOri	:= SB8->B8_ZZSORI
					cDocOri	:= SB8->B8_ZZDOCOR
				EndIf

				//Informações Destino
				DbSelectArea("SB8")
				SB8->(DbSetOrder(3))
				If SB8->(DbSeek(Alltrim(aDadosTransf[nAux,6])+Alltrim(aDadosTransf[nAux,2])+"    "+aDadosTransf[nAux,7]+Alltrim(aDadosTransf[nAux,17])))
					RecLock("SB8",.F.)
					SB8->B8_ZZPBRUT	:= nPBruto
					SB8->B8_ZZPLIQ	:= nPLiq
					SB8->B8_ZZLTGAP := nLtGAP
					SB8->B8_ZZSORI	:= nSldOri
					SB8->B8_ZZDOCOR := cDocOri
					SB8->(MsUnlock())
				EndIf
			Next nAux
		Endif

	endif

	cFilAnt := cFilOld
	nModulo := nModuloOld

	SB8->(RestArea(aSB8))

Return(lRetNfe)


	***************************************************************************************
Static Function fBuscaVlr(cTabPrc,cGrupo)

	Local cAlias 	:= GetNextAlias()
	Local nVlrDA1	:= 0

	if(Select(cAlias) > 0)
		(cAlias)->(DBCloseArea())
	endIf

	cQuery := " SELECT DA1_PRCVEN " + CRLF
	cQuery += " FROM " + CRLF
	cQuery += " 	" + RetSqlName("DA1") + "   " + CRLF
	cQuery += " WHERE " + CRLF
	cQuery += "     DA1_FILIAL = '" + xFilial("DA1") +"'	AND "+ CRLF
	//cQuery += "     DA1_FILIAL = '" + cFilOri +"'	AND "+ CRLF
	cQuery += "     DA1_CODTAB = '" + cTabPrc + "'			AND	"+ CRLF
	cQuery += "     DA1_GRUPO  = '" + cGrupo + "'			AND	"+ CRLF
	cQuery += " 	D_E_L_E_T_ = ' ' " + CRLF

	TcQuery cQuery Alias &cAlias New

	nVlrDA1:= (cAlias)->DA1_PRCVEN

Return(nVlrDA1)


user function MonitNFE( Serie, NotaIni, NotaFim )
	local cURL          	:= alltrim(PadR(GetNewPar("MV_SPEDURL","http://"),250))
	local aParam          	:= { Serie, NotaIni, NotaFim }
	local nTpMonitor     	:= 1 // por intervalo de notas
	local cModelo          	:= "55" // NFe
	local lCte          	:= .F.
	local cAviso          	:= ""
	local lUsaColab         := .F.
	local aRetorno
	local cIdEnt, cError

	If CTIsReady(,,,lUsaColab)
		//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
		//³Obtem o codigo da entidade                                              ³
		//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
		cIdEnt := getCfgEntidade(@cError)

		if !empty(cError)
			Alert(cError)
			return
		endif
	Endif

	aRetorno := procMonitorDoc(cIdEnt, cUrl, aParam, nTpMonitor, cModelo, lCte, @cAviso, lUsaColab)

	if empty(cAviso) // tudo certo
		// faz a varredura no aRetorno

	else // ocorreu alguma falha
		Alert(cAviso)
	endif
return aRetorno


/*
User Function RetPesoACD(_cFilial,_cDoc,_cSerie,_cCliente,_cLoja)

	Local cAlias := GetNextAlias()
	Local cAlias2 := GetNextAlias()
	
	If _cCliente <> "000135"
	// Adicionado Tratativa para buscar Peso Liquido, Peso Bruto e Volume da tabela SB8 - Fausto Costa - 10/01/2019
		BeginSql Alias cAlias
			SELECT SB8.B8_ZZPLIQ AS PESOLIQ, SB8.B8_ZZPBRUT AS PESOBRU, 1 AS QTDVOL FROM SB8010 SB8
			WHERE SB8.D_E_L_E_T_ = ' ' AND SB8.B8_FILIAL+SB8.B8_PRODUTO+SB8.B8_LOTECTL+SB8.B8_LOCAL IN 
			(SELECT  SD31.D3_FILIAL + SD31.D3_COD + SD31.D3_LOTECTL + SD31.D3_LOCAL
						 FROM SD3010 SD31 WHERE SD31.D_E_L_E_T_ = ' ' AND SD31.D3_ESTORNO <> 'S' AND SD31.D3_LOTECTL NOT LIKE ('EXP%') AND SD31.D3_NUMSEQ IN
						(SELECT SD30.D3_NUMSEQ FROM SD3010 SD30 WHERE SD30.D_E_L_E_T_ = ' ' AND SD30.D3_COD + SD30.D3_LOTECTL IN 
						(SELECT D2_COD + D2_LOTECTL FROM SD2010 SD2 WHERE D2_FILIAL = '0105' AND D2_DOC = '000007851' AND D2_SERIE = '55' AND D2_CLIENTE = '000474' AND D2_LOJA = '28' AND SD2.D_E_L_E_T_ = ' ')))
		EndSql

		(cAlias)->(dbGoTop())
		While (cAlias)->(!Eof())
			nPBrtACD += (cAlias)->PESOBRU
			nPLiqACD += (cAlias)->PESOLIQ
			nVolACD	 += (cAlias)->QTDVOL
			(cAlias)->(dbSkip())
		EndDo

		(cAlias)->(DbCloseArea())
		
		//Caso o Peso Liquido, Peso Bruto ou Volume esteja zerado será executado a busca dos dados da forma antiga - Fausto Costa - 10/01/2019
		If nPBrtACD == 0 .OR. nPLiqACD == 0 .OR. nVolACD == 0
		
			BeginSql Alias cAlias
				SELECT SD32.D3_ZZPELIQ AS PESOLIQ, SD32.D3_ZZPEBRU AS PESOBRU, 1 AS QTDVOL FROM SD3010 SD32 WHERE SD32.%NotDel% AND SD32.D3_ESTORNO <> 'S' AND SD32.D3_DOC <> 'INVENT' AND (SD32.D3_CF IN ('DE0','PR0') OR SD32.D3_TM = '490') AND
				SD32.D3_COD + SD32.D3_LOTECTL IN
				(SELECT SD31.D3_COD + SD31.D3_LOTECTL FROM SD3010 SD31 WHERE SD31.%NotDel% AND SD31.D3_ESTORNO <> 'S' AND SD31.D3_LOTECTL NOT LIKE ('EXP%') AND SD31.D3_NUMSEQ IN
				(SELECT SD30.D3_NUMSEQ FROM SD3010 SD30 WHERE SD30.%NotDel% AND SD30.D3_COD + SD30.D3_LOTECTL IN 
				(SELECT D2_COD + D2_LOTECTL FROM SD2010 SD2 WHERE D2_FILIAL = %Exp:_cFilial% AND D2_DOC = %Exp:_cDoc% AND D2_SERIE = %Exp:_cSerie% AND D2_CLIENTE = %Exp:_cCliente% AND D2_LOJA = %Exp:_cLoja% AND SD2.%NotDel%)))
			EndSql
			
			(cAlias)->(dbGoTop())
			While (cAlias)->(!Eof())
				nPBrtACD += (cAlias)->PESOBRU
				nPLiqACD += (cAlias)->PESOLIQ
				nVolACD	 += (cAlias)->QTDVOL
				(cAlias)->(dbSkip())
			EndDo
	
			(cAlias)->(DbCloseArea())
		EndIf
	Else
		BeginSql Alias cAlias
			SELECT D2_FILIAL + D2_COD + D2_LOTECTL + D2_LOCAL AS FILCODLOTE, D2_TP FROM SD2010 SD2 WHERE D2_FILIAL = %Exp:_cFilial% AND D2_DOC = %Exp:_cDoc% AND D2_SERIE = %Exp:_cSerie% AND D2_CLIENTE = %Exp:_cCliente% AND D2_LOJA = %Exp:_cLoja% AND SD2.%NotDel%
			// FAUSTO COSTA - 07-01-2019 - SELECT D2_COD + D2_LOTECTL AS FILCODLOTE, D2_TP FROM SD2010 SD2 WHERE D2_FILIAL = %Exp:_cFilial% AND D2_DOC = %Exp:_cDoc% AND D2_SERIE = %Exp:_cSerie% AND D2_CLIENTE = %Exp:_cCliente% AND D2_LOJA = %Exp:_cLoja% AND SD2.%NotDel%
		EndSql
		(cAlias)->(dbGoTop())
		While (cAlias)->(!Eof())
			If (cAlias)->D2_TP == "SP"
				BeginSql Alias cAlias2
					SELECT ZZF_PESOBR - ZZF_PESOAC AS PESOLIQ, ZZF_PESOBR AS PESOBRU, 1 AS QTDVOL FROM ZZF010 ZZF WHERE ZZF.D_E_L_E_T_ = ' ' AND
					ZZF_FILIAL + ZZF_IDAPA IN (SELECT BC_FILIAL + BC_ZZIDAP FROM SBC010 SBC WHERE SBC.D_E_L_E_T_ = ' ' AND 
					BC_FILIAL + BC_CODDEST + BC_LOTECTL + BC_LOCAL =  %Exp:(cAlias)->FILCODLOTE%)
				EndSql
			Else
				BeginSql Alias cAlias2
					SELECT SB8.B8_ZZPLIQ AS PESOLIQ, SB8.B8_ZZPBRUT AS PESOBRU, 1 AS QTDVOL FROM SB8010 SB8
					WHERE SB8.D_E_L_E_T_ = ' ' AND SB8.B8_FILIAL+SB8.B8_PRODUTO+SB8.B8_LOTECTL+SB8.B8_LOCAL = %Exp:(cAlias)->FILCODLOTE%
				EndSql
			EndIf
			
			nPBrtACD += (cAlias2)->PESOBRU
			nPLiqACD += (cAlias2)->PESOLIQ
			nVolACD	 += (cAlias2)->QTDVOL
			(cAlias2)->(DbCloseArea())
			(cAlias)->(dbSkip())
		EndDo
		
		If nPBrtACD == 0 .OR. nPLiqACD == 0 .OR. nVolACD == 0
			(cAlias)->(dbGoTop())
			While (cAlias)->(!Eof())
				If (cAlias)->D2_TP == "SP"
					BeginSql Alias cAlias2
						SELECT ZZF_PESOBR - ZZF_PESOAC AS PESOLIQ, ZZF_PESOBR AS PESOBRU, 1 AS QTDVOL FROM ZZF010 ZZF WHERE ZZF.D_E_L_E_T_ = ' ' AND
						ZZF_FILIAL + ZZF_IDAPA IN (SELECT BC_FILIAL + BC_ZZIDAP FROM SBC010 SBC WHERE SBC.D_E_L_E_T_ = ' ' AND 
						BC_FILIAL + BC_CODDEST + BC_LOTECTL =  %Exp:LEFT((cAlias)->FILCODLOTE,29)%)
					EndSql
				Else
					BeginSql Alias cAlias2
						//SELECT SD32.D3_ZZPELIQ AS PESOLIQ, SD32.D3_ZZPEBRU AS PESOBRU, 1 AS QTDVOL FROM SD3010 SD32 WHERE SD32.D_E_L_E_T_ = ' ' AND SD32.D3_ESTORNO <> 'S' AND 
						//(SD32.D3_CF IN ('DE0','PR0') OR SD32.D3_TM = '490')  AND	SD32.D3_FILIAL + SD32.D3_COD + SD32.D3_LOTECTL =  %Exp:(cAlias)->FILCODLOTE%
						
						SELECT SD32.D3_ZZPELIQ AS PESOLIQ, SD32.D3_ZZPEBRU AS PESOBRU, 1 AS QTDVOL FROM SD3010 SD32 WHERE SD32.D_E_L_E_T_ = ' ' AND SD32.D3_ESTORNO <> 'S' AND 
						(SD32.D3_CF IN ('DE0','PR0') OR SD32.D3_TM = '490')  AND SD32.D3_COD + SD32.D3_LOTECTL =  %Exp:LEFT((cAlias)->FILCODLOTE,29)%
						
					EndSql
				EndIf
				nPBrtACD += (cAlias2)->PESOBRU
				nPLiqACD += (cAlias2)->PESOLIQ
				nVolACD  += (cAlias2)->QTDVOL
				(cAlias2)->(DbCloseArea())
				(cAlias)->(dbSkip())
			EndDo
		EndIf
		
		(cAlias)->(DbCloseArea())
	EndIf

Return
*/

Static Function CLote() //ContaLote
	nQtdVol		:= Len(aDadosTemp)
Return


//----------------------------------------------------------------------------------------------------------
static Function salvaAreas()

	local nAreas		:= 0
	local cTabela		:= ''

	for nAreas := 1 to len(cFOpened) step 3
		cTabela		:=  Substr( cFOpened, nAreas, 3 )
		aAdd( aAreasTab, (cTabela)->(GetArea()) )
	next

return

//----------------------------------------------------------------------------------------------------------
static Function restAreas()

	local nAreas		:= 0
	local cTabela		:= ''

	for nAreas := 1 to len(aAreasTab)
		cTabela		:=  aAreasTab[nAreas,1]
		(cTabela)->(DbSetOrder(1))
		RestArea(aAreasTab[nAreas])
	next

return

//----------------------------------------------------------------------------------------------------------
static function geraLog(cMensagem)
	ConOut('[ACDTRANS01]' + cMensagem )
return
