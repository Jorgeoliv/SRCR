%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI/3

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% TP1 - Programacao em logica e invariantes
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: definicoes iniciais

:- op( 900,xfy,'::' ).
:- dynamic utente/4.
:- dynamic prestador/4.
:- dynamic cuidado/6.
:- dynamic instituicao/4.


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado utente: IdUt, Nome, Idade, Morada -> {V,F}

utente( 1,ana,14,morada( 'Rua do Louro','Caldas das Taipas','Guimaraes' ) ).
utente( 2,luis,13,morada( 'Rua do Azevinho','Braga','Braga' ) ).
utente( 3,joao,13,morada( 'Rua do Rual','São João de Ponte','Guimaraes' ) ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado prestador: IdPrest, Nome, Especialidade, Instituição -> {V,F}

prestador( 1,jose,'ortopedia',['Hospital de Braga'] ).
prestador( 2,joao,'urologia',['Hospital de Guimaraes'] ).
prestador( 3,jorge,'neurologia',['Hospital de Guimaraes'] ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado cuidado: Data, IdUt, IdPrest, Descrição, Custo, Instituição -> {V,F}

cuidado( data( 2018,3,14,20 ),1,1,'curativo',23,'Hospital de Braga'  ).
cuidado( data( 2018,3,10,20 ),2,3,'rotina',23,'Hospital de Guimaraes' ).
cuidado( data( 2018,3,10,20 ),3,3,'rotina',23,'Hospital de Guimaraes' ).
cuidado( data( 2018,2,10,20 ),2,1,'emergência',23,'Hospital de Braga' ).
cuidado( data( 2018,3,15,20 ),3,3,'curativo',34,'Hospital de Guimaraes' ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado instituição: Nome, Cidade, Tipo, ListaEspecialidade -> {V,F}

instituicao( 'Hospital de Braga','Braga','Hospital',['ortopedia'] ).
instituicao( 'Hospital de Guimaraes','Guimaraes','Hospital',['urologia','neurologia'] ).

%----------------------------------Data do cuidado é válida---------------------------------------

+cuidado( Data,U,P,Descricao,C,I ) :: (dataValida(Data)).


%----------------- ----- --- -- -
% Extensao do predicado dataValida: Data -> {V,F}
dataValida( data(A,M,D,H) ) :- 	natural(A),
							natural(M),
							natural(D),
							natural(H),
							H < 25,
							M < 13,
							diasValidos( A,M,D ).

diasValidos( A,M,D ) :- R is M mod 2,
							R \= 0,
							D < 32,
							D > 0.
diasValidos( A,M,D ) :- R is M mod 2,
							R == 0,
							M \= 2,
							D < 30,
							D > 0.
diasValidos( A,2,D ) :- R is A mod 4,
							R \= 0,
							D < 29,
							D > 0.
diasValidos( A,2,D ) :- R is A mod 4,
							R == 0,
							D < 30,
							D > 0. 

%----------------------------------Não pode haver mais do que 3 cuidados à mesma hora tanto para o utente como o profissional-----------------

+cuidado(D,U,P,Des,C,I) :: (solucoes((D,P),cuidado(D,Ut,P,Descr,Cus,Ins),L),
							comprimento(L,X),
							X=<3
							).

+cuidado(D,U,P,Des,C,I) :: (solucoes((D,U),cuidado(D,U,Pr,Descr,Cus,Ins),L),
							comprimento(L,X),
							X=<3
							).


%-------- Não podem haver ids repetidos -------------

+utente( IdU,_,_,_ ) :: (solucoes( IdU,utente( IdU,_,_,_ ),L ),
						comprimento( L,X ),
						X =< 1).

+prestador( IdP,_,_,_ ) :: (solucoes( IdP,prestador( IdP,_,_,_ ),L ),
						comprimento( L,X ),
						X =< 1).

%--------- Não pode ter uma idade inválida ----------
+utente( IdU,_,I,_ ) :: ( validadeIdade( I ) ).

%--------------- --- -- -
% Extensao do predicado validaIdade: Idade -> {V,F}
validaIdade( A ) :- R is A+1,
					natural(R).

%--------------- Não se podem adicionar cuidados para os quais não existam utentes/prestadores -------------------

+cuidado( _,IdU,_,_,_,_ ) :: (solucoes( IdU,utente( IdU,_,_,_ ),L ),
						comprimento( L,X ),
						X == 1).

+cuidado( _,_,IdP,_,_,_ ) :: (solucoes( IdP,prestador( IdP,_,_,_ ),L ),
						comprimento( L,X ),
						X == 1).

%--------------- Não se pode adicionar um cuidado prestado numa instituição da qual o prestador não faça parte ---------------

+cuidado( _,_,IdP,_,_,I ) :: (solucoes( LI,( prestador( IdP,_,_,LI ),pertence( I,LI ) ),L ),
						comprimento( L,X ),
						X == 1).

%--------------- Não se pode adicionar um cuidado cujo custo seja negativo ------------------------

+cuidado( _,_,_,_,C,_ ) :: (C >= 0).

%----------------- Não se pode adicionar um cuidado com os campos todos iguais --------------------
+cuidado( D,IdU,IdP,Desc,C,I ) :: (solucoes( I,cuidado( D,IdU,IdP,Desc,C,I ),L ),
						comprimento( L,X ),
						X == 1).

%--------------- Não se pode remover um utente/prestador para o qual existam cuidados relativos ---------------

-utente( IdU,_,_,_ ) :: (solucoes( D,cuidado(D,IdU,_,_,_,_ ),L ),
						comprimento( L,X ),
						X == 0).


-prestador( IdP,_,_,_ ) :: (solucoes( D,cuidado(D,_,IdP,_,_,_ ),L ),
						comprimento( L,X ),
						X == 0).

%--------------- Garantir que o prestador tem a especialidade que pertença à lista de especialidades da instituição a que pertença -----

+prestador( IDp,_,Esp,LInst ) :: (validaEspecialidadeInstituicao( Esp,LInst )).

%------------------------- ---- -- -
% Extensao do predicado validaEspecialidadeInstituicao: Especialidade, ListaInstituicao -> {V,F}
validaEspecialidadeInstituicao( Esp,[] ).
validaEspecialidadeInstituicao( Esp,[C|L] ) :- instituicao( C,_,_,E ),
												pertence( Esp,E ),
												validaEspecialidadeInstituicao( Esp,L ). 

%----------------- Não se pode adicionar prestadores que prestem serviços em instituições que não estejam registadas no sistema ----------
+prestador(ID, _, _, LI) :: validaInstituicao(LI).

%------------------------- ---- -- -
% Extensao do predicado validaInstituicao: ListaInstituicao -> {V,F}
validaInstituicao([]).
validaInstituicao([C|L]) :-
	instituicao(C, _, _, _),
	validaInstituicao(L).

%--------------- Não se pode remover uma instituicao para a qual existam prestadores ---------------------
-instituicao( I,C,T,LE ) :: (solucoes( P,( prestador( P,_,_,LI ),pertence( I,LI ) ),L),
							comprimento( L,X ),
							X==0).

%--------------- Não se pode remover uma instituicao para a qual existam cuidados ---------------------
-instituicao( I,C,T,LE ) :: (solucoes( P,prestador( _,P,_,_,_,I ),L),
							comprimento( L,X ),
							X==0).

%------------------------- Predicados de inserção -----------------------------------

%---------------- Adicionar utente ------------------------------------
% Extensao do predicado adicionarUtente: Id,Nome,Idade,Morada -> {V,F}

adicionarUtente( IdUtente,Nome,Idade,Morada ) :-
											evolucao( utente( IdUtente,Nome,Idade,Morada ) ).


%---------------- Adicionar prestador ------------------------------------
% Extensao do predicado adicionarPrestador: Id,Nome,Especialidade,ListaInstituicao -> {V,F}

adicionarPrestador( IdPrestador,Nome,Especialidade,ListaI ) :-
											evolucao( prestador( IdPrestador,Nome,Especialidade,ListaI ) ).

%---------------- Adicionar cuidado ------------------------------------
% Extensao do predicado adicionarCuidado: Data,IdUtente,IdPrestador,Descricao,Custo,Instituicao -> {V,F}

adicionarCuidado( Data,IdUtente,IdPrestador,Descricao,Custo,Instituicao ) :-
											evolucao( cuidado( Data,IdUtente,IdPrestador,Descricao,Custo,Instituicao ) ).

%---------------- Adicionar instituicao ------------------------------------
% Extensao do predicado adicionarInstituicao: Nome,Cidade,Tipo,ListaEspecialidade -> {V,F}

adicionarInstituicao( Nome,Cidade,Tipo,ListaEspecialidade ) :-
											evolucao( instituicao( Nome,Cidade,Tipo,ListaEspecialidade ) ).

%------------------------- Predicados de remocao -----------------------------------

%---------------- Remover utente ------------------------------------
% Extensao do predicado retirarUtente: Id -> {V,F}

retirarUtente( IdUtente ) :-
					inevolucao( utente( IdUtente,_,_,_ ) ).


%---------------- Remover prestador ------------------------------------
% Extensao do predicado retirarPrestador: Id -> {V,F}

retirarPrestador( IdPrestador ) :-
							inevolucao( prestador( IdPrestador,_,_,_ ) ).

%---------------- Remover cuidado ------------------------------------
% Extensao do predicado retirarCuidado: Data,IdUtente,IdPrestador,Descricao,Custo,Instituiao -> {V,F}
retirarCuidado( Data,IdUtente,IdPrestador,Descricao,Custo,Instituiao ) :-
											inevolucao( cuidado( Data,IdUtente,IdPrestador,Descricao,Custo,Instituiao ) ).

%---------------- Remover instituicao ------------------------------------
% Extensao do predicado retirarInstituicao: Nome -> {V,F}
retirarInstituicao( Instituicao ) :-
							inevolucao( instituicao( Instituicao,_,_,_ ) ).

%--------------------------Identificar utentes ---------------------------------------

%---------- Por Id ---------------------
% Extensao do predicado utenteId: Id,Resposta(Utente) -> {V,F}

utenteID( Id,R ) :- solucoes( utente( Id,_,_,_ ),utente( Id,_,_,_ ),R ).


%------------- Por Nome -----------
% Extensao do predicado utenteNome: Nome,Resposta(ListaUtente) -> {V,F}
utenteNome( N,R ) :- solucoes( utente( _,N,_,_ ),utente( _,N,_,_ ),R ).

%----------- Por Morada --------------
% Extensao do predicado utenteMorada: Tipo(Rua,Localidade ou Cidade), Morada, Resposta(ListaUtente) -> {V,F}

utenteMorada( rua,M,R ) :- solucoes( utente( _,_,_,morada( M,_,_ ) ),utente( _,_,_,morada( M,_,_ ) ),R ).
utenteMorada( localidade,M,R ) :- solucoes( utente( _,_,_,morada( M,_,_ ) ),utente( _,_,_,morada( _,M,_ ) ),R ).
utenteMorada( cidade,M,R ) :- solucoes( utente( _,_,_,morada( M,_,_ ) ),utente( _,_,_,morada( _,_,M ) ),R ).

%------------ Por idade --------------------
% Extensao do predicado utentePorIdade: Relacao(>,<,=,=<,>=), Idade, Resposta(ListaUtente) -> {V,F}

utentePorIdade( =,I,X ) :- solucoes( (Id,N),utente(Id,N,I,M),X ).
utentePorIdade( <,I,X ) :- solucoes( (Id,N),(utente(Id,N,Idade,M), Idade < I),X ).
utentePorIdade( >,I,X ) :- solucoes( (Id,N),(utente(Id,N,Idade,M), Idade > I),X ).
utentePorIdade( =<,I,X ) :- solucoes( (Id,N),(utente(Id,N,Idade,M), Idade =< I),X ).
utentePorIdade( >=,I,X ) :- solucoes( (Id,N),(utente(Id,N,Idade,M), Idade >= I),X ).

%----------------------------------------- Identificar prestadores a que um utente recorreu
% Extensao do predicado prestadoresRecorridosUtente: IdUtente,R -> {V,F}
prestadoresRecorridosUtente( IdU,R ) :- solucoes( ( IdP,N ),( cuidado( _,IdU,IdP,_,_ ),prestador(IdP,N,_,_) ),L ),
											multiConjunto( L,R ).
%--------------------------------> VER ISTO >---------------------------------------

%------------------------------- Identificar instituicoes prestadoras de cuidados ----------------------------
% Extensao do predicado instituicoesPrestadoresCuidados: Resposta(ListaInstituicao) -> {V,F}

instituicoesPrestadoresCuidados( R ) :- solucoes( I,cuidado( D,U,P,DE,C,I ),Aux ), multiConjunto( Aux,R ).

%-------------------> VER ISTO <---------------------------------------
utentesPrestador( P,R ) :- solucoes( (U,N),(cuidado(D,U,P,DE,C),utente(U,N,I,M)),Aux ),  multiConjunto(Aux,R).


%-------------------------- Cuidados de saude prestados -------------------------------------------------

%%----------------------------- Por instituicao
% Extensao do predicado cuidadosSaudeInstituicao: Instituicao, ListaCuidado -> {V,F}

cuidadosSaudeInstituicao(Inst, R) :-
	solucoes((D, IDU, IDP, Desc, Cus), cuidado(D, IDU, IDP, Desc, Cus, Inst), R).

%%-------------------------------------------- Por cidade
% Extensao do predicado cuidadosSaudeCidade: Cidade, ListaCuidado -> {V,F}
cuidadosSaudeCidade(Cid, R) :-
	solucoes((D, IDU, IDP, Desc, Cus, I), ( cuidado(D, IDU, IDP, Desc, Cus, I),instituicao( I,Cid,_,_ ) ), R).

%%----------------------------------------------- Por Data
% Extensao do predicado cuidadosSaudeData: Criterio(maior,igual,menor), Data, ListaCuidado -> {V,F}
cuidadosSaudeData(igual, Data, R) :-
	solucoes((D, IDU, IDP, Desc, Cus, Inst), (cuidado(D, IDU, IDP, Desc, Cus, Inst) , (comparaData(igual,D, Data, D))), R).
cuidadosSaudeData(maior, Data, R) :-
	solucoes((D, IDU, IDP, Desc, Cus, Inst), (cuidado(D, IDU, IDP, Desc, Cus, Inst) , (comparaData(maior,D, Data, D))), R).
cuidadosSaudeData(menor, Data, R) :-
	solucoes((D, IDU, IDP, Desc, Cus, Inst), (cuidado(D, IDU, IDP, Desc, Cus, Inst) , (comparaData(menor,D, Data, D))), R).

%------------
% Extensao do predicado cuidadosSaudeData: Criterio(entre), Data, Data, ListaCuidado -> {V,F}
cuidadosSaudeData(entre, Data1, Data2, R) :-
	solucoes((D, IDU, IDP, Desc, Cus, Inst), (cuidado(D, IDU, IDP, Desc, Cus, Inst) , (comparaData(maior,D, Data1, D)),(comparaData(menor,D, Data2, D))), R).

%------------------------------------------------------------
% Extensao do predicado comparaData: Criterio(maior,menor), Data, Data -> {V,F}

comparaData(igual,data(A1, M1, D1, H1), data(A2, M2, D2, H2), data(A1, M1, D1, H1)) :-
	A1 == A2,
	M1 == M2,
	D1 == D2,
	H1 == H2.
comparaData(maior,data(A1, M1, D1, H1), data(A2, M2, D2, H2), data(A1, M1, D1, H1)) :-
	A1 > A2.
comparaData(maior,data(A1, M1, D1, H1), data(A2, M2, D2, H2), data(A1, M1, D1, H1)) :-
	A1 == A2,
	M1 > M2.
comparaData(maior,data(A1, M1, D1, H1), data(A2, M2, D2, H2), data(A1, M1, D1, H1)) :-
	A1 == A2,
	M1 == M2,
	D1 > D2.
comparaData(maior,data(A1, M1, D1, H1), data(A2, M2, D2, H2), data(A1, M1, D1, H1)) :-
	A1 == A2,
	M1 == M2,
	D1 == D2,
	H1 > H2.
comparaData(menor,data(A1, M1, D1, H1), data(A2, M2, D2, H2), data(A1, M1, D1, H1)) :-
	A1 < A2.
comparaData(menor,data(A1, M1, D1, H1), data(A2, M2, D2, H2), data(A1, M1, D1, H1)) :-
	A1 == A2,
	M1 < M2.
comparaData(menor,data(A1, M1, D1, H1), data(A2, M2, D2, H2), data(A1, M1, D1, H1)) :-
	A1 == A2,
	M1 == M2,
	D1 < D2.
comparaData(menor,data(A1, M1, D1, H1), data(A2, M2, D2, H2), data(A1, M1, D1, H1)) :-
	A1 == A2,
	M1 == M2,
	D1 == D2,
	H1 < H2.


% -------------------------------- Identificar os utentes de um prestador/especialidade/instituição ---------------------------------
%---------------------------------------------------
% Extensao do predicado utentesDoPrestador: IdPrestador, ListaUtente -> {V,F}
utentesDoPrestador( IdP,R ) :- solucoes( ( IdU,N ), ( cuidado(_,IdU,IdP,_,_,_), utente(IdU,N,_,_) ), L ),
                               multiConjunto( L,R ).

%---------------------------------------------------
% Extensao do predicado utentesDaEspecialidade: Especialidade, ListaUtente -> {V,F}
utentesDaEspecialidade( Esp,R ) :- solucoes( ( IdU,N ), ( prestador(IdP,_,Esp,_), cuidado(_,IdU,IdP,_,_,_), utente(IdU,N,_,_) ), L ),
                                   multiConjunto( L,R ).

%---------------------------------------------------
% Extensao do predicado utentesDaInstituicao: Instituicao, ListaUtente -> {V,F}
utentesDaInstituicao( Inst,R ) :- solucoes( ( IdU,N ), ( cuidado(_,IdU,_,_,_,I), utente(IdU,N,_,_) ), L ),
                                  multiConjunto( L,R ).											


%------------------------------ Identificar cuidados de utente/prestador/instituicao ---------------------------------------------------------

%---------------------------------------------------
% Extensao do predicado cuidadosUtente: IdUtente, ListaCuidado -> {V,F}
cuidadosUtente(IDU,R) :- solucoes((D,IDU,PREST,DESC,CUSTO) , (cuidado(D,IDU,PREST,DESC,CUSTO)),R).

%---------------------------------------------------
% Extensao do predicado cuidadosInstituicao: Instituicao, ListaCuidado -> {V,F}
cuidadosInstituicao(Inst,R) :- solucoes(  (D,IDU,PREST,DESC,CUSTO)  , ( prestador(PREST,_,_,Inst) , cuidado(D,IDU,PREST,DESC,CUSTO)), R).

%---------------------------------------------------
% Extensao do predicado cuidadosPrestador: IdPrestador, ListaCuidado -> {V,F}
cuidadosPrestador(Prest,R) :- solucoes( (D,IDU,PREST,DESC,CUSTO)  , cuidado((D,IDU,Prest,DESC,CUSTO))  , R) .

%-------------------------------- Determinar todas as instituicoes/prestadores a que um utente já recorreu----------------------

%---------------------------------------------------
% Extensao do predicado instituicoesRecorridasUtente: IdUtente, ListaInstituicao -> {V,F}
instituicoesRecorridasUtente( IdU,R ) :- solucoes( I,cuidado(_,IdU,_,_,_,I),L ),
											multiConjunto( L,R ).

%---------------------------------------------------
% Extensao do predicado prestadoresRecorridosUtente: IdUtente, ListaPrestador -> {V,F}
prestadoresRecorridosUtente( IdU,R ) :- solucoes( ( P,NP ),( cuidado( _,IdU,P,_,_,_ ),prestador( P,NP,_,_ ) ),L ),
											multiConjunto( L,R ).

% --------------------------------Total Custo -----------------------------------------------

%---------------------------------------------------
% Extensao do predicado totalCustoUtente: IdUtente, Custo -> {V,F}
totalCustoUtente( IdU,C ) :- solucoes( Custo,cuidado(_,IdU,_,_,Custo,_),L ),
							somaC( L,C ).

%---------------------------------------------------
% Extensao do predicado totalCustoEspecialidade: Especialidade, Custo -> {V,F}
totalCustoEspecialidade( Esp,C ) :- solucoes( Custo,( cuidado(_,_,P,_,Custo,_),prestador(P,_,Esp,_) ),L ),
							somaC( L,C ).


% ------------------------------------------ PREDICADOS EXTRA -----------------------------------------------------

% ------------------------------------------ Utente Com Mais Custos -----------------------------------------------

%---------------------------------------------------
% Extensao do predicado utentesMaisCusto: Numero, ListaUtentes -> {V,F}
utentesMaisCusto(N , R) :-
	solucoes( ( C,ID,N ),( utente(ID, N, _, _),totalCustoUtente(ID,C) ),L ),
	ordena(L, Rs),
	take(Rs, N, R).

% -------------------------------------------- PREDICADOS AUXILIARES ----------------------------------------------

%---------------------------------------------------
% Extensao do predicado natural: Numero -> {V,F}
natural( 1 ).
natural( X ) :- X < 1, !, fail.
natural( N ) :- R is N-1,
            natural(R).

%---------------------------------------------------
% Extensao do predicado somaC: Lista,Soma -> {V,F}
somaC( [],0 ).
somaC( [C|L],R ) :- somaC( L,A ),
				R is C+A.

%---------------------------------------------------
% Extensao do predicado multiConjunto: Lista, Lista com pares (ocorrencia,nrOcorrencia) -> {V,F}
multiConjunto( [],[] ).
multiConjunto( [C|L], R ) :- multiConjunto(L,A),
							insereMultiConjunto(C,A,R).

%---------------------------------------------------
% Extensao do predicado insereMultiConjunto: Elemento, Lista, Lista com pares (ocorrencia,nrOcorrencia) -> {V,F}
insereMultiConjunto( E,[],[(E,1)] ).
insereMultiConjunto( E,[(E,N)|L],[(E,M)|L] ) :- M is N+1.
insereMultiConjunto( E,[(C,N)|L],[(C,N)|R] ) :- E \= C,
								insereMultiConjunto(E,L,R).

%---------------------------------------------------
% Extensao do predicado solucoes: Termo,Questao,Solucao(ListaQuestao) -> {V,F}
solucoes(X,Y,Z) :- findall(X,Y,Z).
%---------------------------------------------------
% Extensao do predicado construir: ListaInicial,ListaFinal -> {V,F}
construir( L,[F|R] ) :- retract( tmp(F) ),
						construir( L,R ).
construir( S,S ).


%% --------------------------
%% Ordena uma lista de pares por ordem decrescente
% Extensao do predicado ordena: Lista,ListaOrdenada -> {V,F}
ordena([],[]).
ordena([H|T],L) :- ordena(T,T1), insereOrdenado(H,T1,L).

%------------ Insere um elemento ordenado numa lista
% Extensao do predicado insereOrdenado: Elemento,Lista,ListaComElemento -> {V,F}
insereOrdenado(X,[],[X]).
insereOrdenado(X,[Y|Ys],[Y|Zs]) :- X @> Y, ins(X,Ys,Zs).
insereOrdenado(X,[Y|Ys],[X,Y|Ys]) :- X @=< Y.

%% --------------------------
%% Fica os primeiros n elementos de uma lista
% Extensao do predicado take: Lista,NumeroElementos,ListaComOsNElementos -> {V,F}
take( [],_,[] ).
take( C,0,[] ).
take( [C|L],N,[C|R] ) :-
	M is N-1,
	take( L,M,R ).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado pertence: Elemento, Lista -> {V,F}
pertence( X,[X|L] ).
pertence( X,[C|L] ) :- pertence( X,L ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado comprimento: Lista, Resultado -> {V,F}
comprimento( [],0 ).
comprimento( [C|L],R ) :- comprimento( L,A ),
					R is A+1.



%---------------------------- Predicados de Evolucao -------------------

%---------------------------------------------------
% Extensao do predicado evolucao: Termo -> {V,F}
evolucao( Termo ) :- solucoes( Inv,+Termo::Inv,S ),
					inserir( Termo ),
					testar( S ).

%---------------------------------------------------
% Extensao do predicado inserir: Predicado -> {V,F}
inserir( P ) :- assert( P ).
inserir( P ) :- retract( P ), !, fail.


%---------------------------------------------------
% Extensao do predicado involucao: Termo -> {V,F}
inevolucao( Termo ) :- solucoes(Inv,-Termo::Inv,S),
					remover(Termo),
					testar(S).


%---------------------------------------------------
% Extensao do predicado remover: Predicado -> {V,F}
remover( P ) :- retract( P ).
remover( P ) :- assert( P ), !, fail.

%---------------------------------------------------
% Extensao do predicado testar: ListaPredicado -> {V,F}
testar( [] ).
testar( [X|R] ) :- X,
				testar( R ).

%-------------------------- Predicado não - negacao por falha na prova ----------------
% Extensao do predicado nao: Termo -> {V,F}

nao( T ) :- T, !, fail.
nao( T ). 
