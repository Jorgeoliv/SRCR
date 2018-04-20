utente( 6,filipa,xpto001,morada('Rua do Emigrante','Azurém','Braga') ).
nuloD( xpto001 ).
excecao( utente(A,B,_,D) ) :- utente( A,C,B,D ),
							  nuloD( B ).


utente( 7,gisela,33,xpto002 ).
nuloD( xpto002 ).
excecao( utente(A,B,C,_) ) :- utente(A,B,C,D),
							  nuloD( D ).


excecao( utente( 8,helder,Idade,morada('Rua do Azevinho','Braga','Braga') ) ) :-
	Idade >= 15,
	Idade =< 18.




excecao( prestador( 6,fausto,neurologia,'Hospital Privado de Braga' ) ).
excecao( prestador( 6,fausto,neurologia,'Hospital de Braga' ) ).


excecao( prestador( 7,gabriel,A,'Hospital de Braga' ) ).
excecao( prestador( 7,gabriel,A,'Hospital de Guimarães' ) ).
nuloI(xpto456).
prestador(7,gabriel,xpto456,X).
excecao( prestador( A,B,_,D ) ) :-	prestador( A,B,C,D).
									nuloI( C ).

+prestador( O,B,C,D ) :: ( solucoes( N,( prestador( 6,N,X,T ),nao( nuloI( X ) ) ),L ),
					comprimento( L,Aux ),
					Aux == 0
					).


prestador( 8,henriqueta,cardiologia,'Hospital de Braga' ).




cuidado( 6,data(3,4,2018),6,10,'medição',70,xpto424 ).
nuloD( xpto424 ).
excecao( cuidado( A,B,C,D,E,F,_ ) ) :- cuidado( A,B,C,D,E,F,G ),
									   nudoD( G ).


excecao( cuidado( 7,data(4,4,2018),7,1,'exame',69,'Hospital de Guimarães' ) ).
excecao( cuidado( 7,data(5,4,2018),7,1,'exame',69,'Hospital de Guimarães' ) ).


excecao( cuidado( 8,data(1,1,2018),8,2,'cirurgia',Custo,'Hospital Privado de Guimarães' ) ) :-
	Custo >= 100,
	Custo =< 500.

