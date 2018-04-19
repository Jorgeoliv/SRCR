
%%UTENTE:


%Alinea 9
excecao(utente(9,irene,30,morada('Rua dos tolos','Briteiros','Guimaraes')).
excecao(utente(9,irene,30,morada('Rua dos tolos','S.Clemente','Guimaraes')).
excecao(utente(9,irene,50,morada('Rua dos tolos','Briteiros','Guimaraes')).
excecao(utente(9,irene,50,morada('Rua dos tolos','S.Clemente','Guimaraes')).

%Alinea 10
utente(10,jacinto,48,morada('Rua da Rua','Braga','Braga')).

%Alinea 11
excecao(utente(11,marta,34,morada('Rua do Pinheiro','São Lourenço','Braga'))).
excecao(utente(11,marta,34,morada('Rua do Pinheiro','São Lourenço','Guimaraes'))).
excecao(utente(11,mara,34,morada('Rua do Pinheiro','São Lourenço','Braga'))).
excecao(utente(11,marta,34,morada('Rua do Pinheiro','São Lourenço','Guimaraes'))).


%PRESTADOR%

%Alinea 9
excecao(prestador(iglesias,urologia,'Hospital Privado de Braga')).
excecao(prestador(iglesias,patologia,'Hospital Privado de Braga')).

%Alinea 10
excecao(prestador(A,B,C,D)) :- prestador(A,B,C,xpto423).
prestador(josefina,patologia,xpto423).

%Alina 11
prestador(nueria,dermatologia,'Hospital do Porto').


%CUIDADO%

%alinea9
nulo(xpto111).
nulo(xpto222).
cuidado(9,data(28,5,2018),0,3,xpto11,xpto222,'Hospital de Guimares').
excecao(cuidado(A,B,C,D,E,F,G)) :- cuidado(_,_,_,xpto111,xpto222,_).




%alinea10
nuloI(xpto400).
nuloI(xpto123).
cuidado(10,xpto400,10,4,rotina,333,xpto123).
excecao(cuidado(A,B,C,D,E,F,G)) :- cuidado(_,xpto400,_,_,_,_,xpto123).

+cuidado( O,A,B,C,D,E,F ) :: ( solucoes( N,( cuidado( 10,X,_,_,_,_,Y ),nao( nuloI( X ),nao(nuloI(Y) ) ),L ),
					comprimento(L,Aux),
					Aux == 0
					).


%alinea11
nulo(xpto113).
exececao(cuidado(11,data(20,5,2018),11,12,cirurgia,100,xpto113)).
exececao(cuidado(11,data(20,5,2018),11,12,cirurgia,110,xpto113)).
exececao(cuidado(A,B,C,D,E,F,G)) :- cuidado(_,_,_,_,_,_,xpto113).