%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI/3

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% TP1 - Programacao em logica e inveriantes
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


%------------------------INVARIANTES-----------------------

% Não pode existir mais do que um utente com o mesmo Id
% mesmo para prestador
% mesmo talvez para cuidado, com utente e data, prestador e data (se colocarmos horas) num limite de 3
% nao se pode adicionar cuidados para os quais não existam utentes e prestadores

% não se pode remover utentes e prestadores para os quais existam cuidados

%----------------------------------Data do cuidado é válida---------------------------------------

+cuidado( Data,U,P,Descricao,C,I ) :: (dataValida(Data)).

%----------------------------------Não pode haver mais do que 3 cuidados à mesma hora tanto para o utente como o profissional-----------------

+cuidado(D,U,P,Des,C,I) :: (solucoes((D,P),cuidado(D,Ut,P,Descr,Cus,Ins),L),
							comprimento(L,X),
							X<4
							).

+cuidado(D,U,P,Des,C,I) :: (solucoes((D,U),cuidado(D,U,Pr,Descr,Cus,Ins),L),
							comprimento(L,X),
							X<4
							).


validaIdade( A ) :- R is A+1,
					natural(R).


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


%-------- Não podem haver ids repetidos -------------

+utente( IdU,_,_,_ ) :: (solucoes( IdU,utente( IdU,_,_,_ ),L ),
						comprimento( L,X ),
						X =< 1).

+prestador( IdP,_,_,_ ) :: (solucoes( IdP,prestador( IdP,_,_,_ ),L ),
						comprimento( L,X ),
						X =< 1).

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

%--------------- Não se pode remover um utente para o qual existam cuidados relativos ---------------

-utente( IdU,_,_,_ ) :: (solucoes( D,cuidado(D,IdU,_,_,_,_ ),L ),
						comprimento( L,X ),
						X == 0).


-prestador( IdP,_,_,_ ) :: (solucoes( D,cuidado(D,_,IdP,_,_,_ ),L ),
						comprimento( L,X ),
						X == 0).

%--------------- Garantir que o prestador tem a especialidade que pertença à lista de especialidades da instituição a que pertença -----

+prestador( IDp,_,Esp,LInst ) :: (validaEspecialidadeInstituicao( Esp,LInst )).

validaEspecialidadeInstituicao( Esp,[] ).
validaEspecialidadeInstituicao( Esp,[C|L] ) :- instituicao( C,_,_,E ),
												pertence( Esp,E ),
												validaEspecialidadeInstituicao( Esp,L ). 



%------------------------- Predicados de inserção -----------------------------------

%---------------- Adicionar utente ------------------------------------
% adicionarUtente: Id,Nome,Idade,Morada -> {V,F}

adicionarUtente( IdUtente,Nome,Idade,Morada ) :-
											evolucao( utente( IdUtente,Nome,Idade,Morada ) ).


%---------------- Adicionar prestador ------------------------------------
% adicionarPrestador: Id,Nome,Especialidade,ListaInstituicao -> {V,F}

adicionarPrestador( IdPrestador,Nome,Especialidade,ListaI ) :-
											evolucao( prestador( IdPrestador,Nome,Especialidade,ListaI ) ).

%---------------- Adicionar cuidado ------------------------------------
% adicionarCuidado: Data,IdUtente,IdPrestador,Descricao,Custo,Instituicao -> {V,F}

adicionarCuidado( Data,IdUtente,IdPrestador,Descricao,Custo,Instituicao ) :-
											evolucao( cuidado( Data,IdUtente,IdPrestador,Descricao,Custo,Instituicao ) ).

%------------------------- Predicados de remocao -----------------------------------

%---------------- Remover utente ------------------------------------
% retirarUtente: Id -> {V,F}

retirarUtente( IdUtente ) :-
					inevolucao( utente( IdUtente,_,_,_ ) ).


%---------------- Remover prestador ------------------------------------
% retirarPrestador: Id -> {V,F}

retirarPrestador( IdPrestador ) :-
							inevolucao( prestador( IdPrestador,_,_,_ ) ).

%---------------- Remover cuidado ------------------------------------
% ratirarCuidado: Data,IdUtente,IdPrestador -> {V,F}

retirarCuidado( Data,IdUtente,IdPrestador ) :-
											inevolucao( cuidado( Data,IdUtente,IdPrestador,_,_,_ ) ).  % ver isto, pois podemos ter vários na mesma data



%--------------------------Identificar utentes ---------------------------------------

%---------- Por Id ---------------------
% utenteId: Id,R -> {V,F}

utenteID( Id,R ) :- solucoes( utente( Id,_,_,_ ),utente( Id,_,_,_ ),R ).


%------------- Por Nome -----------
% utenteNome: Nome,R -> {V,F}
utenteNome( N,R ) :- solucoes( utente( _,N,_,_ ),utente( _,N,_,_ ),R ).

%----------- Por Morada --------------
% utenteMorada: Tipo(Rua,Localidade ou Cidade), Morada -> {V,F}

utenteMorada( rua,M,R ) :- solucoes( utente( _,_,_,morada( M,_,_ ) ),utente( _,_,_,morada( M,_,_ ) ),R ).
utenteMorada( localidade,M,R ) :- solucoes( utente( _,_,_,morada( M,_,_ ) ),utente( _,_,_,morada( _,M,_ ) ),R ).
utenteMorada( cidade,M,R ) :- solucoes( utente( _,_,_,morada( M,_,_ ) ),utente( _,_,_,morada( _,_,M ) ),R ).


utentePorIdade( =,I,X ) :- solucoes( (Id,N),utente(Id,N,I,M),X ).
utentePorIdade( <,I,X ) :- solucoes( (Id,N),(utente(Id,N,Idade,M), Idade < I),X ).
utentePorIdade( >,I,X ) :- solucoes( (Id,N),(utente(Id,N,Idade,M), Idade > I),X ).
utentePorIdade( =<,I,X ) :- solucoes( (Id,N),(utente(Id,N,Idade,M), Idade =< I),X ).
utentePorIdade( >=,I,X ) :- solucoes( (Id,N),(utente(Id,N,Idade,M), Idade >= I),X ).

%----------------------------------------- Identificar prestadores a que um utente recorreu
% prestadoresRecorridosUtente: IdUtente,R -> {V,F}
prestadoresRecorridosUtente( IdU,R ) :- solucoes( ( IdP,N ),( cuidado( _,IdU,IdP,_,_ ),prestador(IdP,N,_,_) ),L ),
											multiConjunto( L,R ).


instituicoesPrestadoresCuidados( R ) :- solucoes( I,(cuidado(D,U,P,DE,C),prestador(P,N,E,I)),Aux ), multiConjunto(Aux,R).

utentesPrestador( P,R ) :- solucoes( (U,N),(cuidado(D,U,P,DE,C),utente(U,N,I,M)),Aux ),  multiConjunto(Aux,R).


%-------------------------- Ponto 5 -------------------------------------------------

%% cuidados de saude prestados

%%instituicao
cuidadosSaudeInstituicao(Inst, R) :-
	solucoes((D, IDU, IDP, Desc, Cus), cuidado(D, IDU, IDP, Desc, Cus, Inst), R).

%%cidade
cuidadosSaudeCidade(Cid, R) :-
	solucoes((D, IDU, IDP, Desc, Cus, I), (utente(IDU, _, _, morada(_, Cid, _)) ,  cuidado(D, IDU, IDP, Desc, Cus, I)), R).

%%Data

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


cuidadosSaudeData(igual, Data, R) :-
	solucoes((D, IDU, IDP, Desc, Cus, Inst), (cuidado(D, IDU, IDP, Desc, Cus, Inst) , (comparaData(igual,D, Data, D))), R).
cuidadosSaudeData(maior, Data, R) :-
	solucoes((D, IDU, IDP, Desc, Cus, Inst), (cuidado(D, IDU, IDP, Desc, Cus, Inst) , (comparaData(maior,D, Data, D))), R).
cuidadosSaudeData(menor, Data, R) :-
	solucoes((D, IDU, IDP, Desc, Cus, Inst), (cuidado(D, IDU, IDP, Desc, Cus, Inst) , (comparaData(menor,D, Data, D))), R).
cuidadosSaudeData(entre, Data1, Data2, R) :-
	solucoes((D, IDU, IDP, Desc, Cus, Inst), (cuidado(D, IDU, IDP, Desc, Cus, Inst) , (comparaData(maior,D, Data1, D)),(comparaData(menor,D, Data2, D))), R).



% -------------------------------- Ponto 6 -> Identificar os utentes de um prestador/especialidade/instituição ---------------------------------

utentesDoPrestador( IdP,R ) :- solucoes( ( IdU,N ), ( cuidado(_,IdU,IdP,_,_,_), utente(IdU,N,_,_) ), L ),
                               multiConjunto( L,R ).

utentesDaEspecialidade( Esp,R ) :- solucoes( ( IdU,N ), ( prestador(IdP,_,Esp,_), cuidado(_,IdU,IdP,_,_,_), utente(IdU,N,_,_) ), L ),
                                   multiConjunto( L,R ).

utentesDaInstituicao( Inst,R ) :- solucoes( ( IdU,N ), ( cuidado(_,IdU,_,_,_,I), utente(IdU,N,_,_) ), L ),
                                  multiConjunto( L,R ).											


%------------------------------ Ponto 7 ---------------------------------------------------------

cuidadosUtente(IDU,R) :- solucoes((D,IDU,PREST,DESC,CUSTO) , (cuidado(D,IDU,PREST,DESC,CUSTO)),R).

cuidadosInstituicao(Inst,R) :- solucoes(  (D,IDU,PREST,DESC,CUSTO)  , ( prestador(PREST,_,_,Inst) , cuidado(D,IDU,PREST,DESC,CUSTO)), R).

cuidadosPrestador(Prest,R) :- solucoes( (D,IDU,PREST,DESC,CUSTO)  , cuidado((D,IDU,Prest,DESC,CUSTO))  , R) .



% predicado Data poderia ser Data(Ano,Mes,Dia,Hora)
% predicado Morada poderia ser Morada(Rua,Freguesia,Cidade) também se podia por vila e distrito mas acho demais

% -------------------------------- Ponto 8 -> Determinar todas as instituicoes/prestadores a que um utente já recorreu----------------------

instituicoesRecorridasUtente( IdU,R ) :- solucoes( I,cuidado(_,IdU,_,_,_,I),L ),
											multiConjunto( L,R ).

% -------------------------------- Ponto 9 -> Total Custo -----------------------------------------------

totalCustoUtente( IdU,C ) :- solucoes( Custo,cuidado(_,IdU,_,_,Custo,_),L ),
							somaC( L,C ).

totalCustoEspecialidade( Esp,C ) :- solucoes( Custo,( cuidado(_,_,P,_,Custo,_),prestador(P,_,Esp,_) ),L ),
							somaC( L,C ).


% ------------------------------------------ PREDICADOS EXTRA -----------------------------------------------------

% ------------------------------------------ Utente Com Mais Custos -----------------------------------------------

utentesMaisCusto(N , R) :-
	solucoes( ( C,ID ),( utente(ID, _, _, _),totalCustoUtente(ID,C) ),L ),
	ordenaPar(L, Rs),
	take(Rs, N, R).

% -------------------------------------------- PREDICADOS AUXILIARES ----------------------------------------------


natural( 1 ).
natural( X ) :- X < 1, !, fail.
natural( N ) :- R is N-1,
            natural(R).


somaC( [],0 ).
somaC( [C|L],R ) :- somaC( L,A ),
				R is C+A.

multiConjunto( [],[] ).
multiConjunto( [C|L], R ) :- multiConjunto(L,A),
							insereMultiConjunto(C,A,R).


insereMultiConjunto( E,[],[(E,1)] ).
insereMultiConjunto( E,[(E,N)|L],[(E,M)|L] ) :- M is N+1.
insereMultiConjunto( E,[(C,N)|L],[(C,N)|R] ) :- E \= C,
								insereMultiConjunto(E,L,R).


solucoes( X,Y,Z ) :- findall( X,Y,Z ).


%% --------------------------
%% Ordena uma lista de pares por ordem decrescente

ordenaPar(L, L).

%% --------------------------
%% Fica os primeiros n elementos de uma lista

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

evolucao( Termo ) :- solucoes( Inv,+Termo::Inv,S ),
					inserir( Termo ),
					testar( S ).

inserir( P ) :- assert( P ).
inserir( P ) :- retract( P ), !, fail.



inevolucao( Termo ) :- solucoes(Inv,-Termo::Inv,S),
					remover(Termo),
					testar(S).


remover( P ) :- retract( P ).
remover( P ) :- assert( P ), !, fail.

testar( [] ).
testar( [X|R] ) :- X,
				testar( R ).

%-------------------------- Predicado não ----------------
%-------- nao: Termo -> {V,F}

nao( T ) :- T, !, fail.
nao( T ). 
