------------Utente-------------------
utente( 1,ana,20,morada( 'Rua do Louro','Caldelas','Guimaraes' ) ).


excecao( utente( 2,bruno,25,morada( 'Rua do Louro' , 'Caldelas' , 'Guimaraes' ) ) ).
excecao( utente( 2,bruno,35,morada( 'Rua do Louro' , 'Caldelas' , 'Guimaraes' ) ) ).



utente( 3,carlos,xpto732,xpto336 ).
excecao( utente( A,_,_,_ ) ) :- utente( A,_,xpto732,_ ).
nuloI( xpto336 ).
excecao( utente( A,_,_,_ ) ) :- utente( A,B,C,D ),
								nuloI( C ).

-------- Invariante de nulo interdito ---------------
+utente( O,B,C,D ) :: ( solucoes( N,( utente( 3,N,X,T ),nao( nuloI( X ) ) ),L ),
					comprimento( L,Aux ),
					Aux == 0
					).


utente( 4,duarte,35,morada( 'Rua dos Loiros' , 'Caldelas' , 'Guimaraes' ) ).


utente( 5,elisabete,26,morada( 'Rua da Ajuda' , 'Vila Nova' , 'Guimaraes' ) ).


-----------------------Prestador-----------------
prestador( 1,antonio,urologia,'Hospital de Guimarães' ).


prestador( 2,bernardo,ortopedia,'Hospital Privado de Guimarães' ).


prestador( 3,carla,xpto732,'Hospital de Guimarães' ).
excecao( prestador( A,B,C,D ) ) :- prestador( A,_,xpto732,_ ).

prestador( 4,dalila,neurologia,xpto123 ).
nuloI( xpto123 ).
excecao( prestador( A,_,_,_ ) ) :- prestador( A,B,C,D ),
								nuloI( D ).

-------- Invariante de nulo interdito ---------------
+prestador( O,B,C,D ) :: ( solucoes( N,( prestador( 4,N,X,T ),nao( nuloI( X ) ) ),L ),
					comprimento( L,Aux ),
					Aux == 0
					).


prestador( 5,ermelinda,enfermeira,'Hospital de Braga' ).


---------------------------Cuidado------------------------------
excecao( cuidado( 1,D,1,5,'curativo',10,'Hospital de Braga' ) ) :- 
													comparaData( >=,D,data( 1,1,2018 ) ),
													comparaData( <=,D,data( 5,1,2018 ) ).


cuidado( 2,data( 2,1,2018 ),2,6,'investigação',xpto444,{'Hospital Privado de Braga', 'Hospital de Braga'} ).
nuloI( xpto444 ).
excecao( cuidado( 2,data( 2,1,2018 ),2,6,'investigação',xpto444,'Hospital Privado de Braga' ) ).
excecao( cuidado( 2,data( 2,1,2018 ),2,6,'investigação',xpto444,'Hospital de Braga') ).
excecao( cuidado( A,_,_,_,_,_,_ ) ) :- cuidado( A,B,C,D,E,F,G ),
								nuloI( F ).
-------- Invariante de nulo interdito ---------------
+cuidado( O,A,B,C,D,E,F ) :: ( solucoes( N,( cuidado( 2,N,_,T,_,X,_ ),nao( nuloI( X ) ) ),L ),
					comprimento( L,Aux ),
					Aux == 0
					).


cuidado( 3,data( 1,2,2018 ),3,7,xpto732,50,{'Hospital de Braga', 'Hospital de Guimaraes'} ).
excecao( cuidado( A,B,C,D,E,F,G ) ) :- cuidado( A,_,_,_,xpto732,_,_ ).
excecao( cuidado( 3,data( 1,2,2018 ),3,7,xpto732,50,'Hospital de Braga' ) ).
excecao( cuidado( 3,data( 1,2,2018 ),3,7,xpto732,50,'Hospital de Guimaraes') ).


cuidado( 4,data( 2,2,2018 ),4,8,xpto007,15489,'Hospital de Braga' ).
nuloI( xpto007 ).
excecao( cuidado( A,_,_,_,_,_,_ ) ) :- cuidado( A,B,C,D,E,F,G ),
								nuloI( E ).
-------- Invariante de nulo interdito ---------------
+cuidado( O,A,B,C,D,E,F ) :: ( solucoes( N,( cuidado( 4,N,_,T,X,_,_ ),nao( nuloI( X ) ) ),L ),
					comprimento( L,Aux ),
					Aux == 0
					).


excecao( cuidado( 5,data( 3,3,2018 ),5,9,'rotina',25,'Hospital Privado de Braga' ) ).
excecao( cuidado( 5,data( 3,3,2018 ),5,9,'exame',25,'Hospital Privado de Braga' ) ).





--------------- Penso que na remocao de prestadores e utentes temos de ter atencao em eles estarem também em cuidados mas n se saber se são mm eles------------------------
-utente( A,N,I,M ) :: ( solucoes( D,( excecao( cuidado( ID,Data,A,P,D,C,I ),nao(cuidado( ID,Data,xpto732,P,D,C,I ) ) ),L ),
						comprimento( L,Aux ),
						Aux == 0
						).