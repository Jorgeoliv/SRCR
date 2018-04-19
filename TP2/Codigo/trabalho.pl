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
+cuidado( D,IdU,IdP,Desc,C,I ) :: (solucoes( LI,cuidado( D,IdU,IdP,Desc,C,I ),L ),
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


%-------------------- Invariantes de conhecimento imperfeito ----------------------------------------

% ----- Não pode haver conhecimento negativo igual aquele cuidado se queremos adicionar positivo -------------
+cuidado( Id,Data,U,P,D,C,I ) :: (solucoes( A,-cuidado( Id,Data,U,P,D,C,I ),L )
									comprimento( L,N ),
									N == 0). 

% ----- Não pode haver conhecimento positivo igual aquele cuidado se queremos adicionar negativo -------------
+( -cuidado( Id,Data,U,P,D,C,I ) ) :: (solucoes( A,cuidado( Id,Data,U,P,D,C,I ),L )
									comprimento( L,N ),
									N == 0). 

% ----- Não pode haver conhecimento negativo igual aquele cuidado se queremos adicionar negativo -------------
+( -cuidado( Id,Data,U,P,D,C,I ) ) :: (solucoes( A,-cuidado( Id,Data,U,P,D,C,I ),L )
									comprimento( L,N ),
									N == 0). 


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


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado demo: Questao,Resposta -> {V,F}

demo( Questao,verdadeiro ) :-
    Questao.
demo( Questao,falso ) :-
    -Questao.
demo( Questao,desconhecido ) :-
    nao( Questao ),
    nao( -Questao ).


demoTotal( A+B,R ) :- demoTotal( A,T ),
						demoTotal( B,Q ),
						ou( T,Q,R ).
demoTotal( A^B,R ) :- demoTotal( A,T ),
						demoTotal( B,Q ),
						e( T,Q,R ).
demoTotal( A,R ) :- demo( A,R ).

%----------- ou: Termo,Termo,Resposta -> {V,F}
ou( verdadeiro,_,verdadeiro ).
ou( _,verdadeiro,verdadeiro ).
ou( desconhecido,desconhecido,desconhecido ).
ou( desconhecido,falso,desconhecido ).
ou( falso,desconhecido,desconhecido ).
ou( falso,falso,falso ). 

%----------- e: Termo,Termo,Resposta -> {V,F}
e( falso,_,falso ).
e( _,falso,falso ).
e( desconhecido,desconhecido,desconhecido ).
e( desconhecido,verdadeiro,desconhecido ).
e( verdadeiro,desconhecido,desconhecido ).
e( verdadeiro,verdadeiro,verdadeiro ). 



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
