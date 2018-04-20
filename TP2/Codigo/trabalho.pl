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
:- dynamic cuidado/7.
:- dynamic instituicao/4.


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado utente: IdUt, Nome, Idade, Morada -> {V,F}

utente( 1,ana,20,morada( 'Rua do Louro','Caldelas','Guimaraes' ) ).


excecao( utente( 2,bruno,25,morada( 'Rua do Louro' , 'Caldelas' , 'Guimaraes' ) ) ).
excecao( utente( 2,bruno,35,morada( 'Rua do Louro' , 'Caldelas' , 'Guimaraes' ) ) ).



utente( 3,carlos,xpto497,xpto336 ).
nuloD( xpto497 ).
excecao( utente( A,_,_,_ ) ) :- utente( A,_,B,_ ),
                                nuloD( B ).
nuloI( xpto336 ).
excecao( utente( A,_,_,_ ) ) :- utente( A,B,C,D ),
								nuloI( C ).

+utente( O,B,C,D ) :: ( solucoes( N,( utente( 3,N,X,T ),nao( nuloI( X ) ) ),L ),
					comprimento( L,Aux ),
					Aux == 0
					).


utente( 4,duarte,35,morada( 'Rua dos Loiros' , 'Caldelas' , 'Guimaraes' ) ).


utente( 5,elisabete,26,morada( 'Rua da Ajuda' , 'Vila Nova' , 'Guimaraes' ) ).



excecao(utente(9,irene,30,morada('Rua dos tolos','Briteiros','Guimaraes'))).
excecao(utente(9,irene,30,morada('Rua dos tolos','S.Clemente','Guimaraes'))).
excecao(utente(9,irene,50,morada('Rua dos tolos','Briteiros','Guimaraes'))).
excecao(utente(9,irene,50,morada('Rua dos tolos','S.Clemente','Guimaraes'))).
utente(10,jacinto,48,morada('Rua da Rua','Braga','Braga')).

excecao(utente(11,marta,34,morada('Rua do Pinheiro','São Lourenço','Braga'))).
excecao(utente(11,marta,34,morada('Rua do Pinheiro','São Lourenço','Guimaraes'))).
excecao(utente(11,mara,34,morada('Rua do Pinheiro','São Lourenço','Braga'))).
excecao(utente(11,marta,34,morada('Rua do Pinheiro','São Lourenço','Guimaraes'))).

excecao( utente( 12,joaquim,I,morada( 'Rua do Limoeiro' , 'Amais' , 'Viana do Castelo' ) ) ) :-
	I >= 60,
	I =< 80.

utente( 13,marcelo,45,xpto115 ).
excecao( utente( ID,N,I,L ) ) :-
	utente( ID,N,I,xpto115 ).
nulo(xpto115).

+utente( ID,N,I,L ) :: ( solucoes( ID,( utente( 13,N,I,M ),nao( nulo(M) ) ),R ),
						comprimento( R,N ),
						N==0
						).

excecao( utente( 14,diana,I,morada( 'Rua dos Loiros' , 'Caldelas' , 'Guimaraes' ) ) ) :-
	I >= 8,
	I =< 12.




%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado prestador: IdPrest, Nome, Especialidade, Instituição -> {V,F}

prestador( 1,antonio,urologia,'Hospital de Guimarães' ).


prestador( 2,bernardo,ortopedia,'Hospital Privado de Guimarães' ).


prestador( 3,carla,xpto789,'Hospital de Guimarães' ).
nuloD( xpto789 ).
excecao( prestador( A,B,C,D ) ) :- prestador( A,_,B,_ ),
                                    nuloD( B ).
                                    
prestador( 4,dalila,neurologia,xpto123 ).
nuloI( xpto123 ).
excecao( prestador( A,_,_,_ ) ) :- prestador( A,B,C,D ),
								nuloI( D ).


+prestador( O,B,C,D ) :: ( solucoes( N,( prestador( 4,N,X,T ),nao( nuloI( X ) ) ),L ),
					comprimento( L,Aux ),
					Aux == 0
					).


prestador( 5,ermelinda,enfermeira,'Hospital de Braga' ).

excecao(prestador(iglesias,urologia,'Hospital Privado de Braga')).
excecao(prestador(iglesias,patologia,'Hospital Privado de Braga')).

excecao(prestador(A,B,C,D)) :- prestador(A,B,C,xpto423).
prestador(josefina,patologia,xpto423).

prestador(nueria,dermatologia,'Hospital do Porto').


prestador( 12,joao,xpto171,'Hospital do Porto' ).
excecao( prestador( ID,N,E,L ) ) :-
	utente( ID,N,xpto171,L ).
nulo(xpto171).

+prestador( ID,N,E,L ) :: ( solucoes( ID,( prestador( 12,N,I,L ),nao( nulo(I) ) ),R ),
							comprimento( R,N ),
							N==0
						  ).

excecao( prestador( 13,julia,E,L ) ) :-
	E == neurocirurgia,
	L == 'Hospital do Porto'.
excecao( prestador( 13,julia,E,L ) ) :-
	E == neurocirurgia,
	L == 'Hospital de Guimaraes'.
excecao( prestador( 13,julia,E,L ) ) :-
	E == neurologia,
	L == 'Hospital do Porto'.
excecao( prestador( 13,julia,E,L ) ) :-
	E == neurologia,
	L == 'Hospital de Guimaraes'.

prestador( 14,renato,xpto145,xpto167 ).
excecao( prestador( ID,N,E,L ) ) :-
	prestador( ID,N,xpto145,xpto167 ).




%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado cuidado: Data, IdUt, IdPrest, Descrição, Custo, Instituição -> {V,F}

excecao( cuidado( 1,D,1,5,'curativo',10,'Hospital de Braga' ) ) :- 
													comparaData( >=,D,data( 1,1,2018 ) ),
													comparaData( <=,D,data( 5,1,2018 ) ).


cuidado( 2,data( 2,1,2018 ),2,6,'investigação',xpto444,{'Hospital Privado de Braga', 'Hospital de Braga'} ).
nuloI( xpto444 ).
excecao( cuidado( 2,data( 2,1,2018 ),2,6,'investigação',xpto444,'Hospital Privado de Braga' ) ).
excecao( cuidado( 2,data( 2,1,2018 ),2,6,'investigação',xpto444,'Hospital de Braga') ).
excecao( cuidado( A,_,_,_,_,_,_ ) ) :- cuidado( A,B,C,D,E,F,G ),
								nuloI( F ).
%-------- Invariante de nulo interdito ---------------
+cuidado( O,A,B,C,D,E,F ) :: ( solucoes( N,( cuidado( 2,N,_,T,_,X,_ ),nao( nuloI( X ) ) ),L ),
					comprimento( L,Aux ),
					Aux == 0
					).


cuidado( 3,data( 1,2,2018 ),3,7,xpto908,50,{'Hospital de Braga', 'Hospital de Guimaraes'} ).
nuloD( xpto908 ).
excecao( cuidado( A,B,C,D,E,F,G ) ) :- cuidado( A,_,_,_,B,_,_ ),
                                        nuloD( B ).
excecao( cuidado( 3,data( 1,2,2018 ),3,7,xpto732,50,'Hospital de Braga' ) ).
excecao( cuidado( 3,data( 1,2,2018 ),3,7,xpto732,50,'Hospital de Guimaraes') ).


cuidado( 4,data( 2,2,2018 ),4,8,xpto007,15489,'Hospital de Braga' ).
nuloI( xpto007 ).
excecao( cuidado( A,_,_,_,_,_,_ ) ) :- cuidado( A,B,C,D,E,F,G ),
								nuloI( E ).
%-------- Invariante de nulo interdito ---------------
+cuidado( O,A,B,C,D,E,F ) :: ( solucoes( N,( cuidado( 4,N,_,T,X,_,_ ),nao( nuloI( X ) ) ),L ),
					comprimento( L,Aux ),
					Aux == 0
					).


excecao( cuidado( 5,data( 3,3,2018 ),5,9,'rotina',25,'Hospital Privado de Braga' ) ).
excecao( cuidado( 5,data( 3,3,2018 ),5,9,'exame',25,'Hospital Privado de Braga' ) ).



nulo(xpto111).
nulo(xpto222).
cuidado(9,data(28,5,2018),0,3,xpto11,xpto222,'Hospital de Guimares').
excecao(cuidado(A,B,C,D,E,F,G)) :- cuidado(_,_,_,xpto111,xpto222,_).


nuloI(xpto400).
nuloI(xpto123).
cuidado(10,xpto400,10,4,rotina,333,xpto123).
excecao(cuidado(A,B,C,D,E,F,G)) :- cuidado(_,xpto400,_,_,_,_,xpto123).

+cuidado( O,A,B,C,D,E,F ) :: ( solucoes( N,( cuidado( 10,X,_,_,_,_,Y ),nao( nuloI( X ),nao(nuloI(Y) ) ) ),L ),
					comprimento(L,Aux),
					Aux == 0
					).


nulo(xpto113).
exececao(cuidado(11,data(20,5,2018),11,12,cirurgia,100,xpto113)).
exececao(cuidado(11,data(20,5,2018),11,12,cirurgia,110,xpto113)).
exececao(cuidado(A,B,C,D,E,F,G)) :- cuidado(_,_,_,_,_,_,xpto113).


cuidado( 12,data( 1,6,2018 ),12,14,'exame',50,xpto168 ).
excecao( cuidado( ID,D,U,P,Desc,C,I ) ) :-
	cuidado( ID,D,U,P,Desc,C,xpto168 ).

cuidado( 13,data( 1,7,2018 ),13,12,'rotina',10,'Hospital do Porto' ).

cuidado( 14,data( 4,7,2018 ),14,11,xpto112,xpto999,'Hospital do Porto' ).
excecao( cuidado( ID,D,U,P,Desc,C,I ) ) :-
	cuidado( ID,D,U,P,xpto112,xpto999,I ).
nulo(xpto112).

+cuidado( ID,D,U,P,Desc,C,I ) :: ( solucoes( ID,( cuidado( 14,D,U,P,Des,C,I ),nao( nulo(Des) ) ),R ),
								   comprimento( R,N ),
								   N==0
						  	     ).





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



%-------------------- Invariantes de conhecimento imperfeito ----------------------------------------

% ----- Não pode haver conhecimento negativo igual aquele cuidado se queremos adicionar positivo -------------
+cuidado( Id,Data,U,P,D,C,I ) :: (solucoes( A,-cuidado( Id,Data,U,P,D,C,I ),L ),
									comprimento( L,N ),
									N == 0). 

% ----- Não pode haver conhecimento positivo igual aquele cuidado se queremos adicionar negativo -------------
+( -cuidado( Id,Data,U,P,D,C,I ) ) :: (solucoes( A,cuidado( Id,Data,U,P,D,C,I ),L ),
									comprimento( L,N ),
									N == 0). 

% ----- Não pode haver conhecimento negativo igual aquele cuidado se queremos adicionar negativo -------------
+( -cuidado( Id,Data,U,P,D,C,I ) ) :: (solucoes( A,-cuidado( Id,Data,U,P,D,C,I ),L ),
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
