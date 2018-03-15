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
:- dynamic cuidado/6.


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado utente: IdUt, Nome, Idade, Morada -> {V,F}

utente(1,ana,14,rua).
utente(2,luis,13,tres).
utente(3,joao,13,lll).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado prestador: IdPrest, Nome, Especialidade, Instituição -> {V,F}

prestador(1,jose,nano,chhc).
prestador(2,joao,filo,rethc).
prestador(3,jorge,tilo,chhc).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado cuidado: Data, IdUt, IdPrest, Descrição, Custo, Instituição -> {V,F}

cuidado(11,1,1,err,23,chhc).
cuidado(11,2,3,err,23,chhc).
cuidado(12,3,3,err,23,chhc).
cuidado(14,2,1,err,23,chhc).
cuidado(15,3,3,trrr,34,chhc).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado instituição: Nome, Cidade, Tipo, ListaEspecialidade -> {V,F}

instituição(chhc, tonios, nada, [nano, filo, tilo]).


%------------------------INVARIANTES-----------------------

% Não pode existir mais do que um utente com o mesmo Id
% mesmo para prestador
% mesmo talvez para cuidado, com utente e data, prestador e data (se colocarmos horas) num limite de 3
% nao se pode adicionar cuidados para os quais não existam utentes e prestadores

% não se pode remover utentes e prestadores para os quais existam cuidados

% ----------------------------------Data do cuidado é válida---------------------------------------

+cuidado(Data,U,P,Descricao,C,I) :: (dataValida(Data)).

% ----------------------------------Não pode haver mais do que 3 cuidados à mesma hora tanto para o utente como o profissional-----------------

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


dataValida( A,M,D,H ) :- 	natural(A),
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

+utente( IdU,_,_,_ ) :: solucoes( IdU,utente( IdU,_,_,_ ),L ),
						comprimento( L,X ),
						X <= 1.

+prestador( IdP,_,_,_ ) :: solucoes( IdP,prestador( IdP,_,_,_ ),L ),
						comprimento( L,X ),
						X <= 1.

%--------------- Não se podem adicionar cuidados para os quais não existam utentes/prestadores -------------------

+cuidado( _,IdU,_,_,_,_ ) :: solucoes( IdU,utente( IdU,_,_,_ ),L ),
						comprimento( L,X ),
						X == 1.

+cuidado( _,_,IdP,_,_,_ ) :: solucoes( IdP,prestador( IdP,_,_,_ ),L ),
						comprimento( L,X ),
						X == 1.

%--------------- Não se pode adicionar um cuidado prestado numa instituição da qual o prestador não faça parte ---------------

+cuidado( _,_,IdP,_,_,_ ) :: solucoes( LI,( prestador( IdP,_,_,_ ),pertence(I,LI) ),L ),
						comprimento( L,X ),
						X == 1.

%--------------- Não se pode adicionar um cuidado cujo custo seja negativo ------------------------

+cuidado( _,_,_,_,C,_ ) :- C >= 0.

%--------------- Não se pode remover um utente para o qual existam cuidados relativos ---------------

-utente( IdU,_,_,_ ) :: solucoes( D,cuidado(D,IdU,_,_,_,_ ),L ),
						comprimento( L,X ),
						X == 0.


-prestador( IdP,_,_,_ ) :: solucoes( D,cuidado(D,_,IdP,_,_,_ ),L ),
						comprimento( L,X ),
						X == 0.

%--------------- Garantir que o prestador tem a especialidade que pertença à lista de especialidades da instituição a que pertença -----

+prestador( IDp,_,Esp,LInst) :: validaEspecialidadeInstituicao( Esp,LInst ).

validaEspecialidadeInstituicao( Esp,[] ).
validaEspecialidadeInstituicao( Esp,[C|L] ) :- instituicao( C,_,_,E ),
												pertence( Esp,E ),
												validaEspecialidadeInstituicao( Esp,L ). 



%------------------------- Predicados de inserção -----------------------------------

%---------------- Adicionar utente ------------------------------------
% adicionarUtente: Id,Nome,Idade,Morada -> {V,F}

adicionarUtente( IdUtente,Nome,Idade,morada(R,L,C) ) :-
											evolucao( utente( IdUtente,Nome,Idade,Morada( R,L,C ) ) ).


%---------------- Adicionar prestador ------------------------------------
% adicionarPrestador: Id,Nome,Especialidade,ListaInstituicao -> {V,F}

adicionarPrestador( IdPrestador,Nome,Espacialidade,ListaI ) :-
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


%----------------------------------------- Identificar prestadores a que um utente recorreu
% prestadoresRecorridosUtente: IdUtente,R -> {V,F}
prestadoresRecorridosUtente( IdU,R ) :- solucoes( ( IdP,N ),( cuidado( _,IdU,IdP,_,_ ),prestador(IdP,N,_,_) ),L ),
											multiConjunto( L,R ).

utentePorIdade( =,I,X ) :- solucoes( (Id,N),utente(Id,N,I,M),X ).
utentePorIdade( <,I,X ) :- solucoes( (Id,N),(utente(Id,N,Idade,M), Idade < I),X ).
utentePorIdade( >,I,X ) :- solucoes( (Id,N),(utente(Id,N,Idade,M), Idade > I),X ).
utentePorIdade( =<,I,X ) :- solucoes( (Id,N),(utente(Id,N,Idade,M), Idade =< I),X ).
utentePorIdade( >=,I,X ) :- solucoes( (Id,N),(utente(Id,N,Idade,M), Idade >= I),X ).

instituicoesPrestCuidados( R ) :- solucoes( I,(cuidado(D,U,P,DE,C),prestador(P,N,E,I)),Aux ), multiConjunto(Aux,R).

utentesPrestador( P,R ) :- solucoes( (U,N),(cuidado(D,U,P,DE,C),utente(U,N,I,M)),Aux ),  multiConjunto(Aux,R).



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



