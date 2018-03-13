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
% Extensao do predicado cuidado: Data, IdUt, IdPrest, Descrição, Custo -> {V,F}

cuidado(11,1,1,err,23,chhc).
cuidado(11,2,3,err,23,chhc).
cuidado(12,3,3,err,23,chhc).
cuidado(14,2,1,err,23,chhc).
cuidado(15,3,3,trrr,34,chhc).


solucoes( X,Y,Z ) :- findall( X,Y,Z ).

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