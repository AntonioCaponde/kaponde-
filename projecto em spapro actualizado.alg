inteiro codigo,contador,numero_de_clientes,total_no_ficheiro,numero_da_conta[2]
inteiro codigo_do_cliente
real valor,saldo_da_conta[2]
logico f<-FALSO
literal caminho,digitado
fficheiro backup
registo data
	inteiro dia
	inteiro mes
	inteiro ano
fimregisto
registo conta
	inteiro numero_Conta
	real saldo
	inteiro codigo_Cliente
	literal estado<-"desativada"
	data data_de_abertura
fimregisto

registo pessoa
	inteiro codigo,telefone
	literal nome
	literal numero_bilhete
	literal nif
	conta conta_do_cliente
	data data_de_nascimento 	
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

// função para evitar erro de um inteiro em vez de real, o tamanho tem menos duas posicoes
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


// função para validar_bilhete
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
				retorne falso
			fimse
		fimpara
		
		se nao carIsAlfabeto(matriz[tamanho-5])  entao
			retorne falso
		fimse
		
		para contador de tamanho-3 ate tamanho-1
			se nao carIsnumero (matriz[contador]) entao
				retorne falso
			fimse
		fimpara
		retorne verdadeiro
	fimse
	retorne falso
	
fimfuncao
// função para validar_numero_telefone
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
	mostre"\n Erro! Numero de telefone invalido"
	tpause(2000)
retorne falso	
fimfuncao
// função que obriga a digitar um numero 
funcao inteiro Evita_erro_de_escrita()
	literal digitado
	inteiro caracter_certo
	faca
		leia(digitado)
		//chamei a funcao caracter_invalido para verificar se foi digitado um numero ou letra 
	caracter_certo<-caracter_invalido(digitado)
	se caracter_certo<0 entao
		mostre"\n Erro! digite um numero:"
		fimse
	
	enquanto caracter_certo<0
	retorne caracter_certo
fimfuncao
// função para
funcao inteiro Evita_erro_de_escrita_saldo()
	literal digitado
	inteiro caracter_certo
		leia(digitado)
	caracter_certo<-caracter_invalido_saldo(digitado)
	retorne caracter_certo
fimfuncao
// função para
funcao inteiro busca_binaria(pessoa p[],inteiro numero_buscado,inteiro total)

	inteiro inicial<-0, final<-total-1, media
	se numero_buscado=0 entao
	retorne -1
	fimse
	
	faca
		media<-(inicial+final)/2
		se numero_buscado>p[media].conta_do_cliente.numero_conta entao
			final<-media-1
			
		senao
			se numero_buscado<p[media].conta_do_cliente.numero_conta entao
				inicial<-media+1
				
			fimse
		fimse
	enquanto (p[media].conta_do_cliente.numero_conta<>numero_buscado e inicial<=final)
	se p[media].conta_do_cliente.numero_conta=numero_buscado entao
			retorne media
	fimse
	retorne media-1
fimfuncao
// função para
funcao inteiro busca_binaria_codigo(pessoa p[],inteiro numero_buscado,inteiro total)
	se numero_buscado=0 entao
	retorne -1
	fimse
	
	inteiro inicial<-0, final<-total-1, media
	
	faca
		media<-(inicial+final)/2
		se numero_buscado<p[media].codigo entao
			final<-media
		senao
			se numero_buscado>p[media].codigo entao
				inicial<-media+1
			fimse
		fimse
	enquanto p[media].codigo<>numero_buscado e inicial<=final
	
	se p[media].codigo=numero_buscado entao 
		retorne media 
	fimse
	retorne -media
fimfuncao

//Funcao para gerar o codigo do cliente
funcao inteiro gerar_codigo(inteiro codigo)
	inteiro x<-1
	codigo<-codigo+x
	retorne codigo

fimfuncao

//Funcao para gerar_numero_da_conta
funcao inteiro gerar_numero_da_conta(inteiro codigo_do_cliente)

	literal juntar,contadorl,codigogerado, fixo<-"92000",numero
	inteiro codigo,contador,m
	
	codigo<-gerar_codigo(codigo_do_cliente)	
	contador<-parteinteira(aleatorio()*1000)
	codigogerado<-strIntParaStr(codigo)
	contadorl<-strIntParaStr(contador)
	juntar<-fixo+codigogerado+contadorl
	contador<-strValorInt(juntar)
	retorne contador
fimfuncao
//Funçao para efectuar o cadastro do cliente
funcao logico cadastro_de_cliente(pessoa p)
	inteiro opcao, numero
	literal validar
	mostre"\nEscolha: 0-Pessoa \t1-Empresa\n:"
	leia digitado
	opcao<-caracter_invalido(digitado)
	escolha opcao
	caso 0:
		escreva "Digite BI/NIF: "
		leia validar
		se validar_bilhete(validar) entao
		p.numero_bilhete<-validar
		senao 
			mostre"Erro Bilhete invalido"
			tpause(2000)
			retorne falso
			fimse
	caso 1:
		escreva "Digite o NIF: "
		leia validar
		se validar_bilhete(validar) entao
		p.nif<-validar
		senao
			mostre"\nErro NIF invalido"
			tpause(2000)
			retorne falso
			fimse
	fimescolha
	se opcao=0 ou opcao=1 entao
	escreva"Digite o nome: "
	leia(p.nome)
	escreva"Digite o numero de telefone: "
	numero<-Evita_erro_de_escrita()
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
	// função para
funcao logico abertura_de_conta(pessoa p, inteiro tipo_de_conta)
	
	escolha tipo_de_conta
	caso 0:
			se p.numero_bilhete="" entao
				mostre"nao e cliente individual"
			retorne falso
			fimse
	caso 1:
			se p.nif="" entao
				mostre "nao e uma empresa"
				retorne falso
			fimse
		defeito:
			escreva"Erro||| \n escolha entre 0 e 1"
			retorne falso
	fimescolha
	inteiro a
	mostre p.codigo
	leia a
	se p.conta_do_cliente.numero_Conta="0" entao
		p.conta_do_cliente.saldo<-0
		p.conta_do_cliente.numero_Conta<-gerar_numero_da_conta(p.codigo)
		p.conta_do_cliente.codigo_cliente<-p.codigo
		p.conta_do_cliente.estado<-"Ativo"
		retorne verdadeiro
		fimse
	retorne falso
fimfuncao
// função para
funcao logico depositar_dinheiro(inteiro num_de_conta, real valor)
	se num_de_conta=numero_da_conta[0] entao
		saldo_da_conta[0]<-saldo_da_conta[0]+valor
		mostre "deposito"
		retorne verdadeiro
	fimse
	retorne falso
fimfuncao
// função para
funcao logico levantar_dinheiro(inteiro num_de_conta, real valor)								
	se num_de_conta=numero_da_conta[0] entao
		se valor>0 entao
			se saldo_da_conta[0]>valor entao
				saldo_da_conta[0]<-saldo_da_conta[0]-valor
				retorne verdadeiro
			fimse
		fimse
	fimse
	retorne falso

fimfuncao
// função para
funcao real consultar_saldo(inteiro num_da_conta)
	se num_da_conta<>numero_da_conta[0] entao
		retorne -1
	fimse
	retorne saldo_da_conta[0]

fimfuncao
// função para
funcao logico transferir_dinheiro(inteiro num_da_conta_enviar, inteiro num_da_conta_receber, real valor)
	se num_da_conta_enviar<>numero_da_conta[0] entao
		escreva"conta de origem nao existe"
		retorne falso
	fimse
	se num_da_conta_receber<>numero_da_conta[1] entao		
		escreva"conta de destino nao existe"
		retorne falso
	fimse	
	saldo_da_conta[0]<-saldo_da_conta[0]-valor
	saldo_da_conta[1]<-saldo_da_conta[1]+valor
	retorne verdadeiro

fimfuncao
// função para
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
					retorne verdadeiro
				fimse
				retorne falso
			caso 2:
				escreva" Digite o NIF: "
				leia(nbn)
				se validar_bilhete(nbn) entao
					p.nif<-nbn
					retorne verdadeiro
				fimse
				retorne falso
			caso 3:
				escreva" Digite o numero de telefone: "
				leia(telefone)
				se validar_numero_telefone(telefone) entao
					p.telefone<-telefone
					retorne verdadeiro
				fimse
				retorne falso
			defeito:
				escreva"erro! verifique as opções "
				retorne falso
		fimescolha
	senao
		mostre"Erro dados incorretos"
	fimse
	retorne verdadeiro

fimfuncao
		// função para


procedimento menu()
	escreva "\t\t\"SISTEMA DE GESTAO E CONTROLO DE CLIENTES BAI-SGCB\"\n\n"
	escreva"1-Cadastrar cliente\t\t 2-Listar cliente\t 3-Abertura de conta\n4-Depositar dinheiro\t\t 5-Levantar dinheiro\t 6-Consultar saldo\t\n7-Transferir dinheiro\t\t 8-Actualizar dados\t 9-terminar\nOpcao: " 

fimprocedimento
			
funcao literal criar_ficheiro()
	string caminho <- fPastaUtilizador() + "\\Documents\\projecto\\backup.txt"
	retorne caminho

fimfuncao

procedimento actualizar_ficheiro(pessoa cliente_inicial[])
	
	fAbrir(backup,caminho,verdadeiro)
	fescrever(backup,"")
	ffechar(backup)
	fAbrir(backup,caminho,f)
	inteiro a<-total_no_ficheiro
	para contador de 0 ate a-1
		fescrever(backup,"Codigo:"+strIntparastr(cliente_inicial[contador].codigo)+";Nome:"+cliente_inicial[contador].nome+";Telefone:"+strIntparastr(cliente_inicial[contador].telefone)+";Bilhete:"+cliente_inicial[contador].numero_bilhete+";NIF:"+cliente_inicial[contador].nif+";Numero de conta:"+strIntparastr(cliente_inicial[contador].conta_do_cliente.numero_conta)+";Saldo:"+strRealparastr(cliente_inicial[contador].conta_do_cliente.saldo)+";Estado:"+cliente_inicial[contador].conta_do_cliente.estado)
		fNovaLinha(backup)
	fimpara
	total_no_ficheiro<-total_no_ficheiro-a

fimprocedimento

procedimento listar_clientes()
	limpatela
	ffechar(backup)
	fAbrir(backup,caminho,f)
	mostre"__________________________________________________LISTA DE CLIENTES______________________________________________________\n\n"			 	
	enquanto ftemlinha(backup) faca
		inteiro tamanho,contador
		literal dados[]<-strRepartir(fLerLinha(backup),";")
        tamanho<-atamanho(dados,0)
      	mostre"__________________________________________________________________________________________________________________________\n\n"
      	para contador de 0 ate tamanho-1
      		mostre dados[contador],"\n"
      	fimpara
	fimenquanto
	mostre"__________________________________________________________________________________________________________________________\n\n"
		 
	string a
		mostre"Digite qualquer tecla para continuar"
		leia(a)
		limpatela
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
	codigo<-total_no_ficheiro
	
fimprocedimento

procedimento copia_de_seguranca(pessoa cliente[] )
	para contador de 0 ate numero_de_clientes-1
		fescrever(backup,"Codigo:"+strIntparastr(cliente[contador].codigo)+";Nome:"+cliente[contador].nome+";Telefone:"+strIntparastr(cliente[contador].telefone)+";Bilhete:"+cliente[contador].numero_bilhete+";NIF:"+cliente[contador].nif)
		fNovaLinha(backup)
	fimpara
	mostre"Cadastrado com sucesso\n"
	tpause(2000)
fimprocedimento
	
procedimento restaurar_copia_de_seguranca(pessoa cliente_inicial[])
	
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
        		 			cliente_inicial[formarLinha].nome<-strAjustar(matriz_dois_ponto[tamanho2-1])	
        		 		caso 2:
        		 			cliente_inicial[formarLinha].telefone<-caracter_invalido(strAjustar(matriz_dois_ponto[tamanho2-1]))	
        		 		caso 3:
        		 			cliente_inicial[formarLinha].numero_bilhete<-strAjustar(matriz_dois_ponto[tamanho2-1])
        		 		caso 4:
        		 			cliente_inicial[formarLinha].nif<-strAjustar(matriz_dois_ponto[tamanho2-1])
        		 		caso 5: 
        		 			cliente_inicial[formarLinha].conta_do_cliente.numero_conta<-caracter_invalido(strAjustar(matriz_dois_ponto[tamanho2-1]))
        		 		caso 6:
        		 			cliente_inicial[formarLinha].conta_do_cliente.saldo<-caracter_invalido_saldo(strAjustar(matriz_dois_ponto[tamanho2-1]))		
        		 		caso 7:
        		 			cliente_inicial[formarLinha].conta_do_cliente.estado<-strAjustar(matriz_dois_ponto[tamanho2-1])		
        		 		
        		 		defeito:
        		 			mostre "Nao foi possivel restaurar os ficheiros"
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
		pessoa cliente_inicial[total_no_ficheiro+numero_de_clientes]
		restaurar_copia_de_seguranca(cliente_inicial)
		opcao<-Evita_erro_de_escrita()        
		escolha opcao
			caso 1:
				logico verificador<-falso
				limpatela
				escreva"Cadastrar quantos clientes?: "
				numero_de_clientes<-Evita_erro_de_escrita()
				pessoa cliente[numero_de_clientes]  			
				para contador de 0 ate numero_de_clientes-1
					verificador<-cadastro_de_cliente(cliente[contador])
					se nao verificador entao
						contador<-contador-1
					senao
						mostre"Ja cadastrou ",contador+1," Deseja continuar?\nescolha 0-Para cancelar \t 1-Para continuar\n:"
						inteiro cancelar
						cancelar<-Evita_erro_de_escrita()
						se cancelar=0 entao
							numero_de_clientes<-contador+1
							mostre"Cancelando...\n" 
						fimse
					fimse
					se verificador entao		
						codigo<-gerar_codigo(codigo)
						cliente[contador].codigo<-codigo
						escreva"Cadastrado com sucesso\n"
					
					senao
						escreva"\nerro ao cadastrar!\n"
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
						MOSTRE "cliente nao encontrado\n"
						tpause(2000)
					senao
						se numero_buscado=cliente_inicial[numero_encontrado].codigo entao
							mostre "Escolha o tipo de conta: \t 0-Individual \t 1 Empresa"
							tipo_de_conta<-Evita_erro_de_escrita()
							se cliente_inicial[numero_encontrado].conta_do_cliente.numero_conta<=0 entao
								se abertura_de_conta(cliente_inicial[numero_encontrado], tipo_de_conta) entao
									mostre"operacao efectuada com sucesso"
									actualizar_ficheiro(cliente_inicial)
									mostre cliente_inicial[numero_encontrado].conta_do_cliente.numero_conta
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
					numero_encontrado<-busca_binaria(cliente_inicial,numero_buscado,total_clientes)
					se numero_encontrado<0 entao
						mostre"nao exite"
					senao
						mostre cliente_inicial[numero_encontrado].conta_do_cliente.numero_conta,"\n",numero_encontrado
						se cliente_inicial[numero_encontrado].conta_do_cliente.numero_conta=numero_buscado entao
							numero_da_conta[0]<-numero_buscado
							saldo_da_conta[0]<-cliente_inicial[numero_encontrado].conta_do_cliente.saldo
							se depositar_dinheiro(numero_buscado,valor) entao
								cliente_inicial[numero_encontrado].conta_do_cliente.saldo<-saldo_da_conta[0]
								mostre "depositado",cliente_inicial[numero_encontrado].conta_do_cliente.saldo
								actualizar_ficheiro(cliente_inicial)
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
					mostre cliente_inicial[0].conta_do_cliente.numero_conta
					mostre "digite o numero da conta:"
					total_clientes<-total_no_ficheiro+numero_de_clientes
					numero_buscado<-Evita_erro_de_escrita()
					mostre "Digite o valor: "
					leia valor
					numero_encontrado<-busca_binaria(cliente_inicial,numero_buscado,total_clientes)
					se numero_encontrado<0 entao
						mostre "conta nao exite"
					senao
						mostre cliente_inicial[numero_encontrado].conta_do_cliente.numero_conta,"\n",numero_encontrado
						se cliente_inicial[numero_encontrado].conta_do_cliente.numero_conta=numero_buscado entao
							numero_da_conta[0]<-numero_buscado
							saldo_da_conta[0]<-cliente_inicial[numero_encontrado].conta_do_cliente.saldo
							se levantar_dinheiro(numero_buscado,valor) entao
								cliente_inicial[numero_encontrado].conta_do_cliente.saldo<-saldo_da_conta[0]
								mostre "Levantar",cliente_inicial[numero_encontrado].conta_do_cliente.saldo
								actualizar_ficheiro(cliente_inicial)
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
					numero_encontrado<-busca_binaria(cliente_inicial,numero_buscado,total_no_ficheiro+numero_de_clientes)
					se numero_encontrado<0 entao
						mostre "nao existe"
					senao
						se cliente_inicial[numero_encontrado].conta_do_cliente.numero_conta=numero_buscado entao
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
					mostre cliente_inicial[0].conta_do_cliente.numero_conta
					mostre "digite o numero da conta:"
					numero_buscado<-Evita_erro_de_escrita()
					numero_encontrado<-busca_binaria(cliente_inicial,numero_buscado,total_no_ficheiro+numero_de_clientes)
					se numero_encontrado>=0 entao
						se cliente_inicial[numero_encontrado].conta_do_cliente.numero_conta=numero_buscado entao
							numero_da_conta[0]<-numero_buscado
							num_da_conta_enviar<-numero_buscado
							saldo_da_conta[0]<-cliente_inicial[numero_encontrado].conta_do_cliente.saldo
						senao
							mostre "erro numero incorreto"
						fimse
						mostre "digite o numero da conta:"
						numero_buscado<-Evita_erro_de_escrita()
						numero_encontrado<-busca_binaria(cliente_inicial,numero_buscado,total_no_ficheiro+numero_de_clientes)
						se numero_encontrado>=0 entao
							se cliente_inicial[numero_encontrado].conta_do_cliente.numero_conta=numero_buscado entao
							numero_da_conta[1]<-numero_buscado
							saldo_da_conta[1]<-cliente_inicial[numero_encontrado].conta_do_cliente.saldo
							mostre "digite o valor"
							leia valor
							se transferir_dinheiro(num_da_conta_enviar, numero_buscado,valor) entao
								actualizar_ficheiro(cliente_inicial)
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
				mostre cliente_inicial[0].conta_do_cliente.numero_conta
				mostre "digite o codigo de cliente:"
				numero_buscado<-Evita_erro_de_escrita()
				numero_encontrado<-busca_binaria_codigo(cliente_inicial,numero_buscado,total_no_ficheiro+numero_de_clientes)
				se numero_encontrado<0 entao
					mostre"Cliente nao existe"
				senao
				se cliente_inicial[numero_encontrado].conta_do_cliente.numero_conta=numero_buscado entao
					actualizar_conta(cliente_inicial[numero_encontrado],cliente_inicial[numero_encontrado].codigo)
				fimse
				actualizar_ficheiro(cliente_inicial)
				fimse
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
			