;Define a estrutura de datas no programa.
(defstruct data
dia
mes
ano)

;Define a estrutura dos clientes no programa.
(defstruct cliente
nome 
sobrenome 
cpf 
sexo 
data 
endereco
contacorrente
(aplicacoes ())
historico
)

;Define a estrutura de aplicações.
(defstruct aplicacao
tipo
rendimento
valorinicial
datainicial
prazo
id)

;Define a estrutura de conta corrente
(defstruct corrente
idagencia
numero ;número único da conta.
abertura
encerramento
status
(saldo 0)
(historico ())
)

;Armazena todos os clientes cadastrados no banco.
(defvar *clientes* ())

;Valor inicial para a criação de números únicos. 
(defvar numero-unico 5000) 

;Valor inicial para determinar os id's das aplicações.
(setf idUnico 900)

;Instancia uma data. Função usada para passar como parâmetro uma data em outras funções.
(defun cria-data (dia mes ano) (
    make-data :dia dia :mes mes :ano ano))

;Instancia um cliente de acordo com os parâmetros e armazena na lista *clientes*.
(defun cadastrar-cliente (nome sobrenome cpf sexo data endereco idAgencia dataAbertura)
(
    ;Se a busca for bem-sucedida, significa que este cliente ja foi cadastrado.
    if (buscar-cliente cpf) 
        (format t "CPF ~a ja registrado." cpf)
    (progn
        ;Adiciona o novo cliente no início de *clientes*.
        (push (make-cliente :nome nome 
                            :sobrenome sobrenome 
                            :cpf cpf 
                            :sexo sexo 
                            :data data 
                            :endereco endereco) *clientes*)

        ;Associa a ele uma conta-corrente.
        (criar-contaCorrente (first *clientes*) idAgencia dataAbertura)

        ;Exibe a confirmação do cadastro e atualiza o relatório da agência.
        (format t "Cliente cadastrado com sucesso.")
        (atualiza-relatorio_agencia :novo_cliente t))))

;Exibe as informações do cliente.
(defun exibe-info (cliente)
    (format t "--Dados cadastrais--~%Nome: ~a ~a ~%CPF: ~a ~%Sexo: ~a ~%Data de nascimento: ~a/~a/~a ~%Endereco: ~a ~%" 
    (cliente-nome cliente) (cliente-sobrenome cliente) (cliente-cpf cliente) 
    (cliente-sexo cliente) 
    (data-dia (cliente-data cliente)) (data-mes (cliente-data cliente)) (data-ano (cliente-data cliente))
    (cliente-endereco cliente))

    ;Exibe informações da conta corrente.
    (exibe-conta cliente)
)

;Busca por um cpf na lista de *clientes*. 
;Se encontrar retorna a estrutura do cliente em específico.
;Se não encontrar retorna nil.
(defun buscar-cliente(cpf &optional (clientes *clientes*))
(
    cond
    ((endp clientes) nil)
    ((equal (cliente-cpf (first clientes)) cpf) (first clientes)) ;Compara o cpf buscado com o cpf do primeiro cliente da lista.
    (t (buscar-cliente cpf (rest clientes))) ;Chamada recursiva onde o parâmetro opcional clientes é sobreposto.
)
)

;Atualiza os dados do cliente de acordo com as chaves passadas.
(defun att-info (cliente &key (nome nil) (sobrenome nil) (cpf nil) (sexo nil) (data nil) (endereco nil) )
    (when nome (setf (cliente-nome cliente) nome)
    (print "Nome alterado.")
    t)

    (when sobrenome (setf (cliente-sobrenome cliente) sobrenome)
    (print "Sobrenome alterado.")
    t)

    (when cpf (setf (cliente-cpf cliente) cpf)
    (print "CPF alterado.")
    t)

    (when sexo (setf (cliente-sexo cliente) sexo)
    (print "Sexo alterado.")
    t)

    (when data (setf (cliente-data cliente) data)
    (print "Data alterada.")
    t)

    (when endereco (setf (cliente-endereco cliente) endereco)
    (print "Endereco alterado.")
    t)

)

;Exibe todas as informações da conta-corrente de um cliente.
(defun exibe-conta (cliente)

    (let((conta (cliente-contacorrente cliente))) ;Acessa o campo de conta corrente do cliente e o armazena em conta.

    (format t "---Dados da conta corrente---~%Agencia: ~a ~%IdUnico: ~a ~%Data da abertura: ~a/~a/~a ~%Saldo: ~a ~%Status: ~a ~%"
    (corrente-idagencia conta)
    (corrente-numero conta)
    (data-dia (corrente-abertura conta))
    (data-mes (corrente-abertura conta))
    (data-ano (corrente-abertura conta))
    (corrente-saldo conta)
    (corrente-status conta))

    ;Quando a conta estiver inativa também exibe a sua data de encerramento.
    (when (equal (corrente-status conta) 'Inativo) (format t "Data de encerramento: ~a/~a/~a ~%" 
    (data-dia (corrente-encerramento conta))
    (data-mes (corrente-encerramento conta))
    (data-ano (corrente-encerramento conta)))))
)

;Cria a conta corrente.
(defun criar-contaCorrente(cliente id abertura)
(   ;Se o campo de conta corrente do cliente não está como nil, o sistema assume que já existe uma conta.
    if (cliente-contacorrente cliente) (format t "Este cliente ja tem uma conta-corrente.") 
    (progn
        (let ((nova-conta (make-corrente :idagencia id :numero (setf numero-unico (+ numero-unico 1)) ;cria a nova conta.
        :abertura abertura :status 'Ativo)))
        (setf (cliente-contacorrente cliente) nova-conta)) ;Associa a nova conta ao campo de conta corrente do cliente.
        t)))


;Exibe o saldo atual da conta corrente de um cliente.
(defun consultar-saldo (cliente)(
    let ((conta (cliente-contacorrente cliente) ))

    (if conta ;Se houver uma conta corrente
        (format t "Saldo atual: ~a"(corrente-saldo conta))  
        (format t "Este cliente nao tem uma conta-corrente."))))

;Verifica se a conta do cliente está encerrada.
(defun encerradop (cliente) (
    if  (equal(corrente-status (cliente-contacorrente cliente)) 'ATIVO) ;Se o status da conta estiver ativo.
        nil 
        (progn 
        (format t "Conta corrente de ~a ~a esta inativa." (cliente-nome cliente) (cliente-sobrenome cliente) ) 
        t)))

;Registra uma string no histórico do cliente.
(defun registra-historico (texto cliente)
    (let* (
            (conta (cliente-contacorrente cliente))
            (historico (corrente-historico conta)))

        (setf (corrente-historico conta) (append historico (list texto))) ;Adiciona o texto no histórico.
))

;Exibe o historico do cliente.
(defun exibe-historico (cliente)
    (let*   ((conta (cliente-contacorrente cliente))
            (historico (corrente-historico conta))
            (tamHistorico (length historico)))
        
        (format t "---Historico de movimentacoes na conta corrente---~%")

        (dotimes (i tamHistorico) ;Percorre por historico exibindo cada elemento.
            (format t "~a ~%" (nth i historico))
        )

    ))

;Deposita um saldo na conta corrente do cliente, podendo ser passada uma descrição do depósito afim de detalhar no histórico.
(defun depositar-saldo (cliente deposito data hora minuto &optional (descricao (format nil "Deposito realizado")))

    ;Se a conta estiver encerrada nil será retornado.
    (when (encerradop cliente) (return-from depositar-saldo nil)) 

    ;Acessa a conta
    (let ((conta  (cliente-contacorrente cliente) ))

    ;Incrementa o deposito no saldo da conta.
    (setf (corrente-saldo conta) (+ (corrente-saldo conta) deposito))

    ;Registra o depósito no histórico.
    (registra-historico (format nil "~a -> ~a | Novo saldo: ~a" 
    descricao deposito (corrente-saldo conta)) 
    cliente)

    ;Registra no relatório de transacao.    
    (registrar-relatorio 'TRANSACAO (list (cliente-cpf cliente) data "Deposito" 
    deposito hora minuto nil))

    ;Atualiza o relatório da agência.
    (atualiza-relatorio_agencia :novo_deposito t)

    t))

;Retira um valor do saldo do cliente
(defun tirar-saldo (cliente valor)  
    ;Se a conta estiver encerrada a operação não é realizada.
    (when (encerradop cliente) (return-from depositar-saldo nil))

    (let ((conta (cliente-contacorrente cliente)))
        (if (> valor (corrente-saldo conta)) ;Se o valor for maior do que o saldo 
        (format t "Saldo indisponivel")
        (setf (corrente-saldo conta) (- (corrente-saldo conta) valor) )) ;Decrementa o valor do saldo.
    )
)

;Alterar o status da conta do cliente para inativo.
(defun encerrar-conta (cliente data_atual)
   (setf (corrente-status (cliente-contacorrente cliente)) 'INATIVO) ;Muda o status
   (setf (corrente-encerramento (cliente-contacorrente cliente)) data_atual) ;Registra a data de encerramento.
   (format t "Conta encerrada.")
   t
)

;Transfere um valor de uma conta a outra.
(defun transferir (cliente_origem cliente_destino valor data hora minuto)
    ;Verifica o status das contas
    (when (encerradop cliente_origem) (return-from transferir nil))
    (when (encerradop cliente_destino) (return-from transferir nil))

    ;Acessa as contas
    (let ((conta_origem (cliente-contacorrente cliente_origem))
            (conta_destino (cliente-contacorrente cliente_destino)))

    ;Verifica se o saldo é suficiente para a transação.
    (if (> valor (corrente-saldo conta_origem)) 
        (progn (format t "Valor insuficiente.") nil)
        (progn
            ;Atualiza os saldos das contas.
            (setf (corrente-saldo conta_destino) (+ (corrente-saldo conta_destino) valor) )
            (setf (corrente-saldo conta_origem) (- (corrente-saldo conta_origem) valor))

            ;Registra nos históricos das duas contas.
            (registra-historico (format nil "Transferencia realizada para ~a ~a -> ~a | Novo saldo: ~a"
                                (cliente-nome cliente_destino) (cliente-sobrenome cliente_destino) 
                                valor (corrente-saldo conta_origem)) cliente_origem)

            (registra-historico (format nil "Transferencia recebida de ~a ~a -> ~a | Novo saldo: ~a"
                                (cliente-nome cliente_origem) (cliente-sobrenome cliente_origem) 
                                valor (corrente-saldo conta_destino)) cliente_destino)
            
            ;Atualiza o relatório de transações.
            (registrar-relatorio 
            'TRANSACAO (list (cliente-cpf cliente_origem) data "Transferencia" valor hora minuto (cliente-cpf cliente_destino)))
            (atualiza-relatorio_agencia :nova_transacao t)
            t
        ))))

;Cliente pode fazer um pagamento especificando um tipo (boleto ou conta)
(defun pagar (cliente valor destinatario tipo data hora minuto)
    ;Se a conta estiver encerrada não realiza a operação.
    (when (encerradop cliente) (return-from transferir nil))

    (let ((conta (cliente-contacorrente cliente)))

    ;Verifica se o saldo é suficiente para o pagamento.
    (if (< (corrente-saldo conta) valor) (progn (format t "Valor insuficiente.") nil) 
    (progn 
        ;Decrementa o saldo da conta.
        (tirar-saldo cliente valor) 

        ;Registra no histórico da conta.
        (registra-historico (format nil "Pagamento realizado ao destinatario ~a com o valor de ~a" 
        destinatario valor) cliente)
        
        ;Registra no relatório de pagamentos.
        (registrar-relatorio 
                        'PAGAMENTO (list (cliente-cpf cliente) data tipo 
                        valor hora minuto destinatario))
        t))))


;Permite que o cliente saque seu saldo.
(defun sacar (cliente valor data hora minuto)
    ;Verifica se a conta está encerrada.
    (when (encerradop cliente) (return-from transferir nil))

    ;Acessa a conta e define o valor limite do saque (90% do saldo atual).
    (let*   ((conta (cliente-contacorrente cliente))
            (valor-limite (* (corrente-saldo conta) 0.90)))
    
    (cond 
        ((< (corrente-saldo conta) valor) (progn (format t "Valor indisponivel.") nil)) ;Se não tiver saldo suficiente.
        ((> valor valor-limite) (progn (format t "Valor de saque ultrapassa o limite (90% do saldo atual da conta).") nil))
        (t (progn
                    (setf (corrente-saldo conta) (- (corrente-saldo conta) valor) ) ;Decrementa o valor do saldo da conta.
                    
                    ;Registra no histórico da conta.
                    (registra-historico (format nil "Saque realizado -> ~a | Novo saldo: ~a" 
                                        valor (corrente-saldo conta) ) cliente)
                    
                    ;Registra no relatório de transações.
                    (registrar-relatorio 
                    'TRANSACAO (list (cliente-cpf cliente) data "Saque" 
                    valor hora minuto nil))

                    ;Atualiza o relatório da agencia.
                    (atualiza-relatorio_agencia :nova_transacao t)

                    t) ))
    )
)

;Permite criar aplicações.
(defun cadastrar-aplicacao (cliente tipo valor datainicial dataprazo hora minuto)
    ;Instancia a nova aplicação e define o texto que será guardado no histórico.
    (let (
            (aplicacao (make-aplicacao :tipo tipo 
                                    :valorinicial valor
                                    :datainicial datainicial
                                    :prazo dataprazo
                                    :id idUnico
                                    ))
            (historico (format nil "Aplicacao em ~a cadastrada em ~a/~a/~a com valor inicial de ~a | Novo saldo: ~a"
                        tipo (data-dia datainicial) (data-mes datainicial) (data-ano datainicial) 
                        valor (- (corrente-saldo (cliente-contacorrente cliente)) valor) )))

(setf idUnico (+ idUnico 1)) ;Atualiza a referência de id unico.

;Definem o rendimento da aplicação de acordo com o tipo.
(cond
    ((equal 'POUPANCA tipo) (setf (aplicacao-rendimento aplicacao) 0.01))
    ((equal 'TITULO tipo) (setf (aplicacao-rendimento aplicacao) 0.02))
    ((equal 'FUNDO tipo) (setf (aplicacao-rendimento aplicacao) 0.03))
    (t (return-from cadastrar-aplicacao nil)))

;O processo de tirar-saldo foi concluido registra a aplicação.
(if (tirar-saldo cliente valor)
(progn 
    (push aplicacao (cliente-aplicacoes cliente)) ;Adiciona a aplicação na lista de aplicações.
    (registra-historico historico cliente ) ;Registra a nova aplicação no histórico.
    (registrar-relatorio 
                'APLICACAO (list (cliente-cpf cliente) datainicial tipo 
                valor nil hora minuto)) ;Registra no relatório de aplicação.
    (atualiza-relatorio_agencia :nova_aplicacao t) t) ;Atualiza o relatório da agência.            

    nil)))


(defun buscar-aplicacao (cliente id)
;Acessa a quantidade de aplicações nas aplicações do cliente.
(let ((nAplicacoes (length (cliente-aplicacoes cliente) ) ))
        ;Percorre as aplicações e retorna a buscada
        (dotimes (i nAplicacoes) 
            (when (equal (aplicacao-id (nth i (cliente-aplicacoes cliente))) id) 
            (return-from buscar-aplicacao (nth i (cliente-aplicacoes cliente)))))
nil))

;Exibe na tela as informações da aplicação.
(defun exibe-aplicacao (aplicacao) 
(   format t "Tipo: ~a ~%Valor Inicial: ~a ~%Data inicial: ~a/~a/~a ~%Prazo: ~a/~a/~a ~%ID-Unico: ~a ~%"
    (aplicacao-tipo aplicacao) 
    (aplicacao-valorinicial aplicacao) 
    (data-dia (aplicacao-datainicial aplicacao)) 
    (data-mes (aplicacao-datainicial aplicacao)) 
    (data-ano (aplicacao-datainicial aplicacao))
    (data-dia (aplicacao-prazo aplicacao))
    (data-mes (aplicacao-prazo aplicacao)) 
    (data-ano (aplicacao-prazo aplicacao))
    (aplicacao-id aplicacao)))

;Exibe todas as aplicações do cliente.
(defun exibe-aplicacoes (cliente) 
    ;Acessa a quantidade de aplicações que já foram cadastradas.
(   let ((nAplicacoes (length (cliente-aplicacoes cliente) ) ))

    (if (zerop nAplicacoes) 
        (format t "Nenhuma aplicacao registrada.") 
        (dotimes (i nAplicacoes) ;Percorre a lista de aplicações exibindo-as.
            (format t "---Aplicacao--- ~%" i)
            (exibe-aplicacao (nth i (cliente-aplicacoes cliente)))
        )
    )
))


;Calcula a diferença de meses entre uma data e outra.
;Cálculo: 12 - mesinicial + ((anoprazo-(anoinicial+1)) * 12) + mesprazo
(defun calcula-meses (datainicial dataatual)
(let (
        (total-meses (+ (- 12 (data-mes datainicial)) (data-mes dataatual) ) )
        (diferenca-anos (- (data-ano dataatual) (+ (data-ano datainicial) 1)))
    )
    (setf total-meses (+ total-meses (* diferenca-anos 12) ))
    
    (if (> (data-dia datainicial) (data-dia dataatual)) (- total-meses 1) total-meses)
)
)

;Verifica se o prazo de uma aplicação foi alcançado.
(defun prazo-alcancado (aplicacao data_atual)
(
    let ((prazo (aplicacao-prazo aplicacao)))  

    (cond 
        ((> (data-ano data_atual) (data-ano prazo)) t)
        ((< (data-ano data_atual) (data-ano prazo)) nil)    
        ((> (data-mes data_atual) (data-mes prazo)) t)
        ((< (data-mes data_atual) (data-mes prazo)) nil)
        ((>= (data-dia data_atual) (data-dia prazo)) t)
        (t nil))
))

;Calcula o rendimento da aplicação.
(defun calcula-rendimento (aplicacao data_atual)
    ;Se o prazo foi alcançado então o sistema calcula de acordo com a data do prazo.
    (when (prazo-alcancado aplicacao data_atual) (setf data_atual (aplicacao-prazo aplicacao) ))
   

    (let* (
        (data-inicial (aplicacao-datainicial aplicacao))
        (diferenca-meses (calcula-meses data-inicial data_atual))
        (rendimento (aplicacao-rendimento aplicacao))
        (valor-inicial (aplicacao-valorinicial aplicacao))
        )

    ;Retorna o rendimento 
    (* (* rendimento diferenca-meses) valor-inicial)
    )

)

;Exibe o saldo atual da aplicação.
(defun exibe-saldoAplicacao (aplicacao data_atual)

   (format t "Saldo atual: ~2,2f" (+ (calcula-rendimento aplicacao data_atual) (aplicacao-valorinicial aplicacao) ))
)

;Exibe o rendimento da aplicação.
(defun exibe-rendimento (aplicacao data_atual)

    (format t "Rendimento atual: ~2,2f" (calcula-rendimento aplicacao data_atual) )

)

;Calcula o saldo total da aplicação.
(defun calcula-saldo (aplicacao data_atual)
    (+ (calcula-rendimento aplicacao data_atual) (aplicacao-valorinicial aplicacao) )
)

;Verifica se o rendimento da aplicação será perdido.
(defun rendimento-perdido (aplicacao data_atual)
;Se o prazo não foi alcançado e a aplicação é do tipo fundo de investimento ou título.
(if (and (not (prazo-alcancado aplicacao data_atual)) 
    (or (equal (aplicacao-tipo aplicacao) 'FUNDO ) (equal (aplicacao-tipo aplicacao) 'TITULO )))
    t
    nil
))

;Exclui a aplicação da lista de aplicações do cliente.
(defun excluir-aplicacao (aplicacoes id)
    (cond 
        ((endp aplicacoes) nil)
        ((equal (aplicacao-id (first aplicacoes)) id) (rest aplicacoes) )
        (t (cons (first aplicacoes) (excluir-aplicacao (rest aplicacoes) id) ))))

;Resgata o saldo da aplicação. 
;Se resgate for passado por parâmetro, então o sistema assume que é um resgate parcial. 
(defun resgatar-aplicacao (cliente id data_atual &optional (resgate nil))
    (let(
            (aplicacao (buscar-aplicacao cliente id) )
            (deposito 0))
    

    (when (not aplicacao) (progn
    (format t "Aplicacao nao encontrada.")
    (return-from resgatar-aplicacao nil)
    ))
    
    
    (cond 
        (resgate (setf deposito resgate)) ;Se resgate foi passado por parâmetro, o deposito a ser feito na conta será esse valor.
        ((rendimento-perdido aplicacao data_atual) (setf deposito (aplicacao-valorinicial aplicacao) )) ;Se o rendimento for perdido o valor inicial da aplicação será o depósito. 
        (t (setf deposito (calcula-saldo aplicacao data_atual)))) ;Senão o depósito será completo.


    (cond 
        ((rendimento-perdido aplicacao data_atual) (format t "Prazo da aplicacao nao foi alcancado. Rendimento perdido.~%"))
        ((not resgate) (setf (cliente-aplicacoes cliente) (excluir-aplicacao (cliente-aplicacoes cliente) id)))) ;Exclui a aplicação se o resgate for total.
    

    ;Deposita o saldo na conta do cliente.
    (depositar-saldo cliente deposito (format nil "Aplicacao em ~a de id ~a resgatada em ~a/~a/~a"
                        (aplicacao-tipo aplicacao) id
                        (data-dia data_atual) (data-mes data_atual) (data-ano data_atual)) )

    t))


;Lista de colunas para os relatórios de transações.
(defvar *colunas_transacoes* (list "Cliente" "Data" "Tipo" "Valor" "Hora" "Minuto" "Destino"))
;Lista de relatórios de transações.
(defvar *relatorios_transacoes* ())

;Lista de colunas para os relatórios de aplicações.
(defvar *colunas_aplicacoes* (list "Cliente" "Data" "Tipo" "Valor" "Rendimento" "Data_resgate" "ID"))
;Lista de relatórios de aplicações.
(defvar *relatorios_aplicacoes* ())

;Lista de colunas para os relatórios de pagamentos.
(defvar *colunas_pagamentos* (list "Cliente" "Data" "Tipo" "Valor" "Hora" "Minuto" "Destino"))
;Lista de relatórios de pagamentos.
(defvar *relatorios_pagamentos* ())

;Estrutura de chave-valor para o relatório da agência.
(defvar *relatorio_agencia* (list (list 'TRANSACOES 0) (list 'CLIENTES 0) (list 'DEPOSITOS 0) (list 'APLICACOES 0)))

;Espera por alguma chave que recebe um valor diferente de nil. E incrementa 1 na informação correspondente no relatório da agência.
(defun atualiza-relatorio_agencia (&key (nova_transacao nil) (novo_cliente nil) (novo_deposito nil) (nova_aplicacao nil))
    (when nova_transacao (incf (cadr (assoc 'TRANSACOES *relatorio_agencia*))))
    (when novo_cliente (incf (cadr (assoc 'CLIENTES *relatorio_agencia*))))
    (when novo_deposito (incf (cadr (assoc 'DEPOSITOS *relatorio_agencia*))))
    (when nova_aplicacao (incf (cadr (assoc 'APLICACOES *relatorio_agencia*))))
    nil
)

;Registra os dados no relatório específico de acordo com o tipo-relatorio.
(defun registrar-relatorio (tipo-relatorio lista-dados)
    (cond 
            ((equal tipo-relatorio 'TRANSACAO) (setf *relatorios_transacoes* (append *relatorios_transacoes* (list lista-dados)) ) )
            ((equal tipo-relatorio 'APLICACAO) (setf *relatorios_aplicacoes* (append *relatorios_aplicacoes* (list lista-dados)) ))
            ((equal tipo-relatorio 'PAGAMENTO) (setf *relatorios_pagamentos* (append *relatorios_pagamentos* (list lista-dados)) ))
            (t nil))
)

;Retorna a lista de relatórios dependendo do tipo.
(defun lista-relatorios (tipo)
    (cond 
        ((equal tipo 'TRANSACAO) *relatorios_transacoes*)
        ((equal tipo 'APLICACAO) *relatorios_aplicacoes*)
        ((equal tipo 'PAGAMENTO) *relatorios_pagamentos*)
        ((equal tipo 'AGENCIA) (mapcar (lambda (x) (car (cdr x) )) *relatorio_agencia*)) ;Retorna apenas os valores sem suas chaves.
        (t nil))
)

;Retorna a lista de colunas de relatórios dependendo do tipo.
(defun lista-colunas (tipo)
    (cond 
        ((equal tipo 'TRANSACAO) *colunas_transacoes*)
        ((equal tipo 'APLICACAO) *colunas_aplicacoes*)
        ((equal tipo 'PAGAMENTO) *colunas_pagamentos*)
        ((equal tipo 'AGENCIA) (mapcar #'car *relatorio_agencia*) )
        (t nil))
)

;Retorna t se data_a for anterior de data_b  
(defun data-anterior (data_a data_b)
    (cond 
        ((> (data-ano data_b) (data-ano data_a)) t)
        ((< (data-ano data_b) (data-ano data_a)) nil)    
        ((> (data-mes data_b) (data-mes data_a)) t)
        ((< (data-mes data_b) (data-mes data_a)) nil)
        ((>= (data-dia data_b) (data-dia data_a)) t)
        (t nil)) 
)

;Filtra a lista de relatórios de acordo com os filtros diferentes de nil.
(defun filtragem (relatorios cliente data_min data_max tipo)

    (when (not (null cliente)) (setf relatorios (remove-if-not (lambda (x) (equal (first x) cliente) ) relatorios)) )

    (when (not (null data_min)) (setf relatorios (remove-if (lambda (x) (data-anterior (second x) data_min ) ) relatorios)))

    (when (not (null data_max)) (setf relatorios (remove-if (lambda (x) (data-anterior  data_max (second x)) ) relatorios)))

    (when (not (null tipo)) (setf relatorios (remove-if-not (lambda (x) (equal (third x) tipo) ) relatorios)))

    relatorios
)

;Recebe um tipo de relatório a ser gerado entre: 'TRANSACAO, 'APLICACAO, 'PAGAMENTO e 'AGENCIA.
;De forma opcional recebe os filtros: Cliente em específico, intervalo de data e tipo.
;Se necessário realiza a filtragem e exporta em arquivo CSV. 
(defun gerar-relatorio (tipo-relatorio &key (cliente nil) (data_min nil) (data_max nil) (tipo nil))
    (let ((relatorios (lista-relatorios tipo-relatorio)) ;Acessa a lista de relatório.
            (colunas (lista-colunas tipo-relatorio))) ;Acessa a lista de colunas.

        (when (not (equal tipo-relatorio 'AGENCIA)) ;Para relatório de agencia não são aceitados filtros.
        (setf relatorios (filtragem relatorios cliente data_min data_max tipo))) ;Processo de filtragem.

        ;Exporta em CSV.
        (relatorio_csv (format nil "Relatorio de ~a" tipo-relatorio) colunas relatorios tipo-relatorio)
    ))

;Exporta os relatórios para CSV.
(defun relatorio_csv (nome-arquivo colunas dados tipo)
    ;Abertura/Criação do arquivo para escrita.
    (with-open-file (stream nome-arquivo 
                                        :direction :output
                                        :if-exists :supersede
                                        :if-does-not-exist :create)
    ;Passa pro arquivo as colunas na primeira linha.
    (format stream "~{~a~^,~}~%" colunas)
    
    (if (equal tipo 'AGENCIA) (format stream "~{~a~^,~}~%" dados) ;Se for um relatório de agência apenas exibe seus valores separados por vírgulas.
    (dolist (lista dados)   ;Percorre pela lista de dados exibindo cada valor de cada relatório em uma linha, separados por vírgulas.    
        (format stream "~{~a~^,~}~%" lista))))
    t)