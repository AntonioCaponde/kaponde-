inteiro codigo,contador,numero_de_clientes,total_no_ficheiro,numero_da_conta[2]
real valor,saldo_da_conta[2]
logico f<-FALSO
literal caminho,digitado
fficheiro backup
	
registo pessoa
	inteiro codigo,telefone
	literal nome
	literal numero_bilhete
	literal nif
	
fimregisto

registo conta
	inteiro numero_Conta
	real saldo
	inteiro codigo_Cliente
	literal estado
fimregisto


//	funçoes adicionadas

// função para evitar erro de digitação de uma letra em vez de numero
funcao inteiro caracter_invalido(string digitado)
	
	caracter matriz[]<-strParaArray (digitado)
	inteiro tamanho,contador,codigo_Ascii,numero
	literal formar_numero
	
	se strIsVazia (digitado) entao
		retorne -1
	fimse
	
	tamanho<-atamanho(matriz,0)
	
	para contador de 0 ate tamanho-1
		codigo_Ascii<-carObtASCII(matriz[contador])

		se codigo_Ascii>=48 e codigo_Ascii<=57 entao
			formar_numero<-formar_numero+matriz[contador]
			numero<-strvalorInt(formar_numero)
		senao
			retorne -1
		fimse
	fimpara

	retorne numero
	
fimfuncao

// função para evitar erro de digitação de uma letra em vez de numero
funcao inteiro caracter_invalido_saldo(string digitado)
	caracter matriz[]<-strParaArray (digitado)
	inteiro tamanho,contador,codigo_Ascii,numero
	literal formar_numero
	se strIsVazia (digitado) entao
		retorne 0
	fimse
	tamanho<-atamanho(matriz,0)
	para contador de 0 ate tamanho-2
		codigo_Ascii<-carObtASCII(matriz[contador])
		se codigo_Ascii>=48 e codigo_Ascii<=57 entao	
			formar_numero<-formar_numero+matriz[contador]
		fimse
	fimpara
	numero<-strvalorint(formar_numero)
	retorne numero
fimfuncao

funcao logico validar_bilhete(string digitado)
	caracter matriz[]<-strParaArray (digitado)
	inteiro tamanho,contador
	se strIsVazia (digitado) entao
		mostre "vazio\n"
		 retorne falso
	fimse
	tamanho<-atamanho(matriz,0)
	se tamanho=14 entao
	para contador de 0 ate tamanho-6
		se nao carIsnumero (matriz[contador]) entao
			mostre"\n Erro!"
			retorne falso
		fimse
	fimpara
	se nao carIsAlfabeto(matriz[tamanho-5])  entao
			mostre"\n Erro!"
			retorne falso
	fimse
	para contador de tamanho-3 ate tamanho-1
		se nao carIsnumero (matriz[contador]) entao
			mostre"\n Erro!"
			retorne falso
		fimse
	fimpara
	retorne verdadeiro
	fimse
	retorne falso
	
fimfuncao

funcao logico validar_numero_telefone(inteiro telefone)
caracter matriz[]<-strParaArray (strIntParaStr(telefone))
inteiro tamanho,numero
tamanho<-atamanho(matriz,0)
numero<-strvalorint(matriz[0])

se tamanho=9 entao
	se numero=9 entao
		retorne verdadeiro
	fimse
fimse
mostre"\n Erro!"
retorne falso	
fimfuncao
funcao inteiro Evita_erro_de_escrita()
	literal digitado
	inteiro caracter_certo
	faca
		leia(digitado)
	caracter_certo<-caracter_invalido(digitado)
	se caracter_certo<0 entao
		mostre"\n Erro! digite um numero:"
		fimse
	
	enquanto caracter_certo<0
	retorne caracter_certo
	
fimfuncao

funcao inteiro Evita_erro_de_escrita_saldo()
	literal digitado
	inteiro caracter_certo
	faca
		leia(digitado)
	caracter_certo<-caracter_invalido_saldo(digitado)
	enquanto caracter_certo<0
	retorne caracter_certo
fimfuncao

funcao inteiro busca_binaria(conta p[],inteiro numero_buscado,inteiro total)

	inteiro inicial<-0, final<-total, media
	
	faca
		mostre media
		media<-(inicial+final)/2
		se media=total entao
			mostre"\nnao existe\n"
			senao
		se numero_buscado>p[media].numero_conta entao
			final<-media-1
		senao
			se numero_buscado<p[media].numero_conta entao
				inicial<-media+1
			fimse
		fimse
		fimse
	enquanto (p[media].numero_conta<>numero_buscado e inicial<=final)
	
	se p[media].numero_conta=numero_buscado entao 
			mostre "numero encontrado"
			retorne media
	senao
		mostre"numero nao encontrado"
	fimse
	retorne -media
fimfuncao

funcao inteiro busca_binaria_codigo(pessoa p[],inteiro numero_buscado,inteiro total)
	
	inteiro inicial<-0, final<-total, media
	
	faca
		media<-(inicial+final)/2
		mostre "\nmedia=",media
		tpause(2000)
		se numero_buscado<p[media].codigo entao
			final<-media
		senao
			se numero_buscado>p[media].codigo entao
				inicial<-media+1
			fimse
		fimse
	enquanto p[media].codigo<>numero_buscado e inicial<=final
	
	se p[media].codigo=numero_buscado entao 
		mostre "numero encontrado"
		retorne media 
	senao
		mostre"numero nao encontrado"
	fimse
	retorne -media
	
fimfuncao

funcao logico cadastro_de_cliente(pessoa p)
	inteiro opcao, numero
	literal bi
	mostre"Escolha:\t 0-Pessoa \t1-Empresa"
	leia digitado
	opcao<-caracter_invalido(digitado)
	escolha opcao
	caso 0:
		escreva "Digite BI/NIF: "
		leia bi
		se validar_bilhete(bi) entao
		p.numero_bilhete<-bi
		senao 
			retorne falso
			fimse
	caso 1:
		escreva "Digite o NIF: "
		leia bi
		se validar_bilhete(bi) entao
		p.nif<-bi
		senao 
			retorne falso
			fimse
	fimescolha
	se opcao=0 ou opcao=1 entao
	escreva"Digite o nome: "
	leia(p.nome)
	escreva"Digite o numero de telefone: "
	leia digitado
	numero<-caracter_invalido(digitado)
		se validar_numero_telefone(numero) entao
			p.telefone<-numero
			retorne verdadeiro
		senao
			retorne falso
			fimse
	senao
		retorne falso
	fimse
	retorne falso
fimfuncao
	
funcao logico abertura_de_conta(pessoa p, inteiro tipo_de_conta)
	 literal verificador,copia
	 inteiro tipo<-tipo_de_conta
	escolha tipo_de_conta
	caso 0:
		mostre p.numero_bilhete
			escreva"digite o numero do Bilhete:"
			leia(verificador)
			copia<-strAjustar (p.numero_bilhete)
			se verificador=copia entao
				mostre"igual\n"
			retorne verdadeiro
			fimse
	caso 1:
		mostre p.nif
			escreva"Informe o NIF:\n"
			leia(verificador)
			copia<-strAjustar (p.nif)
			se verificador=copia entao
				mostre"igual\n"
			retorne verdadeiro
			fimse
		defeito:
			escreva"Erro||| \n escolha entre 0 e 1"
			retorne falso
		fimescolha
		retorne verdadeiro
		fimfuncao
			
		funcao logico depositar_dinheiro(inteiro num_de_conta, real valor)
			
			se num_de_conta=numero_da_conta[0] entao
				mostre valor
				saldo_da_conta[0]<-saldo_da_conta[0]+valor
				mostre "saldo_da_conta[0]:",saldo_da_conta[0]
				retorne verdadeiro
				senao
				escreva"Esta conta nao existe"
				retorne falso
				fimse
	 	retorne verdadeiro
		fimfuncao
		funcao logico levantar_dinheiro(inteiro num_de_conta, real valor)
			
								
			se num_de_conta=numero_da_conta[0] entao
				saldo_da_conta[0]<-saldo_da_conta[0]-valor
				retorne verdadeiro
				senao
				retorne falso
				fimse
	 	retorne verdadeiro
		fimfuncao
		funcao real consultar_saldo(inteiro num_da_conta)
			se num_da_conta=numero_da_conta[0] entao
				retorne saldo_da_conta[0]
				senao
			retorne -1
			fimse
			retorne saldo_da_conta[0]
		fimfuncao

		funcao logico transferir_dinheiro(inteiro num_da_conta_enviar, inteiro num_da_conta_receber, real valor)
			 se num_da_conta_enviar=numero_da_conta[0] entao
			se num_da_conta_receber=numero_da_conta[1] entao
			saldo_da_conta[0]<-saldo_da_conta[0]-valor
			saldo_da_conta[1]<-saldo_da_conta[1]+valor
			retorne verdadeiro
			senao
				escreva"conta de destino nao existe"
				retorne falso
			fimse
			 senao
			 	escreva"conta de origem nao existe"
			 	retorne falso
			 	fimse
				retorne verdadeiro
		fimfuncao
		funcao logico actualizar_conta(pessoa p, inteiro codigo_cliente)
			literal nbn 
			inteiro telefone, opcao
			se p.codigo=codigo_cliente entao
			escreva"O que deseja actualizar\n Digite:\n 0-nome\t 1-BI\t 2-NIF\t 3-Telefone"
			leia(opcao)
			escolha opcao
				caso 0: 
					escreva" Digite o nome: "
					leia (nbn)
					p.nome<-nbn
					retorne verdadeiro
				caso 1:
					escreva"Digite BI\NIF: "
					leia(nbn)
					se	validar_bilhete(nbn) entao
					p.nif<-nbn
					fimse
					retorne verdadeiro
				caso 2:
						escreva" Digite o NIF: "
						leia(nbn)
						se validar_bilhete(nbn) entao
						p.nif<-nbn
						fimse
						retorne verdadeiro
				caso 3:
						escreva" Digite o numero de telefone: "
						leia(telefone)
						se validar_numero_telefone(telefone) entao
						p.telefone<-telefone
						fimse
						retorne verdadeiro

				defeito:
						escreva"erro! verifique as opções "
						retorne falso
			fimescolha
			senao
				mostre"Erro dados incorretos"
				fimse
			retorne verdadeiro
			fimfuncao


			//Funções e procedimentos adicionais
		
			funcao inteiro gerar_codigo(inteiro codigo)
				inteiro x<-1
				codigo<-codigo+x
				retorne codigo
			fimfuncao
			funcao inteiro gerar_numero_da_conta(inteiro x)
			literal juntar,contadorl,codigogerado, fixo<-"92000",numero
			inteiro codigo,contador,m
			codigo<-gerar_codigo(x)	
				contador<-parteinteira(aleatorio()*1000)
				codigogerado<-strIntParaStr(codigo)
				contadorl<-strIntParaStr(contador)
				juntar<-fixo+codigogerado+contadorl
				contador<-strValorInt(juntar)
				retorne contador
			fimfuncao
			procedimento menu()
				escreva "\t\t\"SISTEMA DE GESTAO E CONTROLO DE CLIENTES BAI-SGCB\"\n\n"
					escreva"1-Cadastrar cliente\t\t 2-Listar cliente\t 3-Abertura de conta\n4-Depositar dinheiro\t\t 5-Levantar dinheiro\t 6-Consultar saldo\t\n7-Transferir dinheiro\t\t 8-Actualizar dados\t 9-terminar\nOpcao: " 
			fimprocedimento
			
			funcao literal criar_ficheiro()
    			string caminho <- fPastaUtilizador() + "\\Documents\\projecto\\backup.txt"
    			retorne caminho
    		fimfuncao
procedimento actualizar_ficheiro(pessoa cliente_inicial[],conta conta_cliente[])
	
					fAbrir(backup,caminho,verdadeiro)
							fescrever(backup,"")
							ffechar(backup)
							fAbrir(backup,caminho,f)
							inteiro a<-total_no_ficheiro
								para contador de 0 ate a-1
						fescrever(backup,"Codigo: "+strIntparastr(cliente_inicial[contador].codigo)+";Nome: "+cliente_inicial[contador].nome+";\tTelefone: "+strIntparastr(cliente_inicial[contador].telefone)+";\tBilhete: "+cliente_inicial[contador].numero_bilhete+";\tNIF: "+cliente_inicial[contador].nif+";\tNumero de conta:"+strIntparastr(conta_cliente[contador].numero_conta)+";\tSaldo:"+strRealparastr(conta_cliente[contador].saldo)+";\tEstado:"+conta_cliente[contador].estado                                                       )
						MOSTRE "\N OPCAO 3",cliente_inicial[contador].codigo
						MOSTRE "\N A=",A
						TPAUSE(2000)
						fNovaLinha(backup)
								fimpara
								total_no_ficheiro<-total_no_ficheiro-a
	fimprocedimento
	procedimento listar_clientes()
						fRecuar (backup)				 	
				    enquanto ftemlinha(backup) faca
        		mostre "",fLerTudo(backup)
				    fimenquanto 
				    fimprocedimento









			
procedimento existencia_de_ficheiro()
	
    se nao  fExiste(caminho) entao 
    	caminho<-criar_ficheiro()
        se nao fcriar(caminho) entao
        	mostre "existe ficheiro\n"
        
        fimse
    senao
 	 		mostre "o ficheiro foi criado!\n"
   	fimse	
	fimprocedimento
	procedimento contar_linha_no_ficheiro()
			enquanto ftemlinha(backup) faca
        		fLerLinha(backup)
        		
        		total_no_ficheiro<-total_no_ficheiro+1 
        	fimenquanto
        	mostre total_no_ficheiro 
        	codigo<-total_no_ficheiro
	fimprocedimento
	procedimento copia_de_seguranca(pessoa cliente[] )
		para contador de 0 ate numero_de_clientes-1
			
						fescrever(backup,"Codigo: "+strIntparastr(cliente[contador].codigo)+";Nome: "+cliente[contador].nome+";\tTelefone: "+strIntparastr(cliente[contador].telefone)+";\tBilhete: "+cliente[contador].numero_bilhete+";\tNIF: "+cliente[contador].nif)
						fNovaLinha(backup)
						mostre"escreveu"
						tpause(2000)
		fimpara
		fimprocedimento
	procedimento restaurar_copia_de_seguranca(pessoa cliente_inicial[],conta conta_cliente[])
					inteiro contador1,tamanho1,contador2,tamanho2,formarLinha<-0
						ffechar(backup)
						fAbrir(backup,caminho,f)
						enquanto ftemlinha(backup) faca
        					literal matriz_virgula[]<-strRepartir(fLerLinha(backup),";")
        		 			tamanho1<-atamanho(matriz_virgula,0)
        		 			
      	 					 para contador1 de 0 ate tamanho1-1
        		 			literal matriz_dois_ponto[]<-strRepartir(matriz_virgula[contador1],":")
        		 			tamanho2<-atamanho(matriz_dois_ponto,0)
        		 			para contador2 de 1 ate tamanho2-1
        		 			escolha contador1
        		 		caso 0:
        		 			cliente_inicial[formarLinha].codigo<-caracter_invalido(strAjustar(matriz_dois_ponto[tamanho2-1]))
        		 		caso 1:
        		 			cliente_inicial[formarLinha].nome<-matriz_dois_ponto[tamanho2-1]
        		 			
        		 		caso 2:
        		 			cliente_inicial[formarLinha].telefone<-caracter_invalido(strAjustar(matriz_dois_ponto[tamanho2-1]))
        		 			
        		 		caso 3:
        		 			cliente_inicial[formarLinha].numero_bilhete<-matriz_dois_ponto[tamanho2-1]
        		 			caso 4:
        		 				cliente_inicial[formarLinha].nif<-matriz_dois_ponto[tamanho2-1]
        		 		caso 5: 
        		 			conta_cliente[formarLinha].numero_conta<-caracter_invalido(strAjustar(matriz_dois_ponto[tamanho2-1]))
        		 			caso 6:
        		 			conta_cliente[formarLinha].saldo<-caracter_invalido_saldo(strAjustar(matriz_dois_ponto[tamanho2-1]))		
        		 			defeito:
        		 				mostre "erro"
        		 		fimescolha
        		 	fimpara
      	 		 fimpara
      	 		 formarLinha<-formarLinha+1
						fimenquanto	
	fimprocedimento
	
inicio	
	
	inteiro sair<-1,opcao,total_clientes,tipo_de_conta,numero_buscado,numero_encontrado
	existencia_de_ficheiro()
	fAbrir(backup,caminho,f)
	faca
		menu()    
		contar_linha_no_ficheiro()
		numero_de_clientes<-0
		conta conta_cliente[total_no_ficheiro+numero_de_clientes]
		pessoa cliente_inicial[total_no_ficheiro+numero_de_clientes]
		restaurar_copia_de_seguranca(cliente_inicial,conta_cliente)
		leia digitado
		opcao<-caracter_invalido(digitado)        
		escolha opcao
			caso 1:
				logico verificador<-falso
				limpatela
				escreva"Cadastrar quantos clientes?: "
				numero_de_clientes<-Evita_erro_de_escrita()
				pessoa cliente[numero_de_clientes]  
				
				para contador de 0 ate numero_de_clientes-1
					verificador<-cadastro_de_cliente(cliente[contador])
					se verificador entao		
						codigo<-gerar_codigo(codigo)
						cliente[contador].codigo<-codigo
						escreva"\nCadastrado com sucesso\n"
						limpatela
					senao
						escreva"\nerro ao cadastrar!\n"
							limpatela
					fimse
				fimpara
				se verificador entao
				copia_de_seguranca(cliente)
				fimse
					caso 2:
						listar_clientes()	 
					caso 3:
						se total_no_ficheiro=0 entao
							mostre "Erro! nao possui clientes cadastrados"
						senao
							mostre"digite o codigo de cliente: "
							total_clientes<-total_no_ficheiro+numero_de_clientes	
							numero_buscado<-Evita_erro_de_escrita()
							numero_encontrado<-busca_binaria_codigo(cliente_inicial,numero_buscado,total_clientes)
							se numero_encontrado<0 ENTAO
								MOSTRE "cliente nao encontrado\n",numero_encontrado
							SENAO
								se numero_buscado=cliente_inicial[numero_encontrado].codigo entao
								mostre "Escolha o tipo de conta: \t 0-Individual \t 1 Empresa"
								tipo_de_conta<-Evita_erro_de_escrita()
									se conta_cliente[numero_encontrado].numero_conta<=0 entao
									se abertura_de_conta(cliente_inicial[numero_encontrado], tipo_de_conta) entao
										
										conta_cliente[numero_encontrado].numero_conta<-gerar_numero_da_conta(numero_encontrado)
										conta_cliente[numero_encontrado].saldo<-0
										actualizar_ficheiro(cliente_inicial,conta_cliente)
									mostre conta_cliente[numero_encontrado].numero_conta
									senao
							mostre "cliente nao existe"
						fimse
						fimse
								fimse
								mostre"\nJa possui conta\n"
							FIMSE
							fimse
					caso 4:		
						se total_no_ficheiro=0 entao
							mostre"nao possui clientes cadastrados"
							senao
						mostre "digite o numero da conta:"
						total_clientes<-total_no_ficheiro+numero_de_clientes
						numero_buscado<-Evita_erro_de_escrita()
						mostre "Digite o valor: "
						leia(valor)
						numero_encontrado<-busca_binaria(conta_cliente,numero_buscado,total_clientes)
						se numero_encontrado<0 entao
							mostre"nao exite"
							senao
						mostre conta_cliente[numero_encontrado].numero_conta,"\n",numero_encontrado
						se conta_cliente[numero_encontrado].numero_conta=numero_buscado entao
						numero_da_conta[0]<-numero_buscado
						saldo_da_conta[0]<-conta_cliente[numero_encontrado].saldo
						se depositar_dinheiro(numero_buscado,valor) entao
							conta_cliente[numero_encontrado].saldo<-saldo_da_conta[0]
							mostre "depositado",conta_cliente[numero_encontrado].saldo
							actualizar_ficheiro(cliente_inicial,conta_cliente)
						senao 
							mostre"erro ao depositar"
						fimse
						fimse
						fimse
						fimse
					caso 5:
						se total_no_ficheiro=0 entao
							mostre"nao possui clintes cadastrados"
							senao
						mostre conta_cliente[0].numero_conta
						mostre "digite o numero da conta:"
						total_clientes<-total_no_ficheiro+numero_de_clientes
						numero_buscado<-Evita_erro_de_escrita()
						mostre "Digite o valor: "
						leia valor
						numero_encontrado<-busca_binaria(conta_cliente,numero_buscado,total_clientes)
						se numero_encontrado<0 entao
							mostre "conta nao exite"
							senao
						mostre conta_cliente[numero_encontrado].numero_conta,"\n",numero_encontrado
						se conta_cliente[numero_encontrado].numero_conta=numero_buscado entao
						numero_da_conta[0]<-numero_buscado
						saldo_da_conta[0]<-conta_cliente[numero_encontrado].saldo
						se levantar_dinheiro(numero_buscado,valor) entao
							conta_cliente[numero_encontrado].saldo<-saldo_da_conta[0]
							mostre "Levantar",conta_cliente[numero_encontrado].saldo
							actualizar_ficheiro(cliente_inicial,conta_cliente)
						senao 
							mostre"erro ao Levantar"
						fimse
						fimse
						fimse	
						fimse
					caso 6:
						se total_no_ficheiro=0 entao 
							mostre "Nao possui clientes cadastrados"
							senao
						mostre "digite o numero da conta:"
						numero_buscado<-Evita_erro_de_escrita()
						numero_encontrado<-busca_binaria(conta_cliente,numero_buscado,total_no_ficheiro+numero_de_clientes)
						se numero_encontrado<0 entao
							mostre "nao existe"
						senao
							
						se conta_cliente[numero_encontrado].numero_conta=numero_buscado entao
							mostre consultar_saldo(numero_buscado) 
						senao 
							mostre"erro ao consultar"
						fimse
						fimse
						fimse
					caso 7:
						se total_no_ficheiro=0 entao
							mostre"nao possui clinetes cadastra"
							senao
						inteiro num_da_conta_enviar
						mostre conta_cliente[0].numero_conta
						mostre "digite o numero da conta:"
						numero_buscado<-Evita_erro_de_escrita()
						numero_encontrado<-busca_binaria(conta_cliente,numero_buscado,total_no_ficheiro+numero_de_clientes)
						se numero_encontrado>=0 entao
						se conta_cliente[numero_encontrado].numero_conta=numero_buscado entao
						numero_da_conta[0]<-numero_buscado
						num_da_conta_enviar<-numero_buscado
						saldo_da_conta[0]<-conta_cliente[numero_encontrado].saldo
						senao
							mostre "erro numero incorreto"
						fimse
						mostre "digite o numero da conta:"
						numero_buscado<-Evita_erro_de_escrita()
						numero_encontrado<-busca_binaria(conta_cliente,numero_buscado,total_no_ficheiro+numero_de_clientes)
						se numero_encontrado>=0 entao
						se conta_cliente[numero_encontrado].numero_conta=numero_buscado entao
						numero_da_conta[1]<-numero_buscado
						saldo_da_conta[1]<-conta_cliente[numero_encontrado].saldo
						mostre "digite o valor"
						leia valor
						se transferir_dinheiro(num_da_conta_enviar, numero_buscado,valor) entao
							actualizar_ficheiro(cliente_inicial,conta_cliente)
							fimse
						senao
							mostre "erro numero incorreto"
						fimse
						fimse
						fimse
						fimse
						
						
					caso 8:
					se total_no_ficheiro=0 entao
							mostre"nao possui clinetes cadastra"
							senao
						mostre conta_cliente[0].numero_conta
						mostre "digite o numero da conta:"
						numero_buscado<-Evita_erro_de_escrita()
						numero_encontrado<-busca_binaria(conta_cliente,numero_buscado,total_no_ficheiro+numero_de_clientes)
						se conta_cliente[numero_encontrado].numero_conta=numero_buscado entao
							actualizar_conta(cliente_inicial[numero_encontrado],cliente_inicial[numero_encontrado].codigo)
							fimse
						actualizar_ficheiro(cliente_inicial,conta_cliente)
						fimse
					caso 9:
						limpatela
						escreva"Deseja sair?\t para sair:0\t continuar:1\n"
					leia (sair)
					limpatela
					escreva"Encerrado..."
					defeito:
					mostre"opcao errada|"
					tpause(1000)
					fimescolha
						
							
enquanto sair=1
				ffechar(backup)
			fimalgoritmo
			