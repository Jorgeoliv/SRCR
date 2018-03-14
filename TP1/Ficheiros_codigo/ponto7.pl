cuidadosUtente(IDU,R) :- solucoes((D,IDU,PREST,DESC,CUSTO) , (cuidado(D,IDU,PREST,DESC,CUSTO)),R).

cuidadosInstituicao(Inst,R) :- solucoes(  (D,IDU,PREST,DESC,CUSTO)  , ( prestador(PREST,_,_,Inst) , cuidado(D,IDU,PREST,DESC,CUSTO)), R).

cuidadosPrestador(Prest,R) :- solucoes( (D,IDU,PREST,DESC,CUSTO)  , cuidado((D,IDU,Prest,DESC,CUSTO))  , R) .
