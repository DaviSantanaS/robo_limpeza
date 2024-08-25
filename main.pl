/*
 * 
 * Implementação de um robo de limpeza
 * Ponto final: destino(ponto(X, Y)).
 * Ponto bloqueado: bloqueado(ponto(X, Y)).
 * Ponto sujo: sujo(ponto(X, Y)).
 * Estado atual do robo: robo(ponto(X, Y)).
 * Os pontos limpos será reconhecido por não estar sujo.
 * 
 * */

/*
 *
 * Declaração do mapa
 * O mapa será 8x8, os pontos variam entre ponto(0, 0) e ponto(7, 7)
 *
 * */

ponto(0, 0).
ponto(0, 1).
ponto(0, 2).
ponto(0, 3).
ponto(0, 4).
ponto(0, 5).
ponto(0, 6).
ponto(0, 7).
ponto(1, 0).
ponto(1, 1).
ponto(1, 2).
ponto(1, 3).
ponto(1, 4).
ponto(1, 5).
ponto(1, 6).
ponto(1, 7).
ponto(2, 0).
ponto(2, 1).
ponto(2, 2).
ponto(2, 3).
ponto(2, 4).
ponto(2, 5).
ponto(2, 6).
ponto(2, 7).
ponto(3, 0).
ponto(3, 1).
ponto(3, 2).
ponto(3, 3).
ponto(3, 4).
ponto(3, 5).
ponto(3, 6).
ponto(3, 7).
ponto(4, 0).
ponto(4, 1).
ponto(4, 2).
ponto(4, 3).
ponto(4, 4).
ponto(4, 5).
ponto(4, 6).
ponto(4, 7).
ponto(5, 0).
ponto(5, 1).
ponto(5, 2).
ponto(5, 3).
ponto(5, 4).
ponto(5, 5).
ponto(5, 6).
ponto(5, 7).
ponto(6, 0).
ponto(6, 1).
ponto(6, 2).
ponto(6, 3).
ponto(6, 4).
ponto(6, 5).
ponto(6, 6).
ponto(6, 7).
ponto(7, 0).
ponto(7, 1).
ponto(7, 2).
ponto(7, 3).
ponto(7, 4).
ponto(7, 5).
ponto(7, 6).
ponto(7, 7).

/*
 *
 * Declaração dos pontos sujos
 * sujo(ponto(X, Y)).
 *
 * */

:-dynamic sujo/1.

sujo(ponto(0, 0)).
sujo(ponto(0, 4)).
sujo(ponto(1, 2)).
sujo(ponto(2, 3)).
sujo(ponto(2, 4)).
sujo(ponto(2, 5)).
sujo(ponto(3, 4)).
sujo(ponto(4, 5)).
sujo(ponto(4, 6)).
sujo(ponto(5, 1)).
sujo(ponto(5, 2)).
sujo(ponto(6, 1)).
sujo(ponto(6, 7)).
sujo(ponto(7, 0)).
sujo(ponto(7, 1)).
sujo(ponto(7, 2)).
sujo(ponto(7, 4)).
sujo(ponto(7, 5)).
sujo(ponto(7, 6)).
sujo(ponto(7, 7)).

/*
 *
 * Declaração dos pontos bloqueados
 * bloqueado(ponto(X, Y)).
 *
 * */

bloqueado(ponto(1, 6)).
bloqueado(ponto(1, 7)).
bloqueado(ponto(2, 7)).
bloqueado(ponto(4, 0)).
bloqueado(ponto(4, 1)).
bloqueado(ponto(4, 2)).
bloqueado(ponto(4, 7)).
bloqueado(ponto(5, 5)).
bloqueado(ponto(6, 2)).
bloqueado(ponto(6, 3)).

/* Declarando o estado inicial */
:- dynamic robo/1.

robo(ponto(3, 3)).

/* Declarando o ponto final */
destino(ponto(7, 3)).

/* Declarando objetivo */
:-dynamic objetivo/1.

objetivo(ponto(7, 3)).

/* Mostra possíveis caminhos a partir de um ponto */

cima(ponto(X, Y), ponto(X, Z)) :-
  Z is Y + 1,
  ponto(X, Z),
  not(bloqueado(ponto(X, Z))).

baixo(ponto(X, Y), ponto(X, Z)) :-
  Z is Y - 1,
  ponto(X, Z),
  not(bloqueado(ponto(X, Z))).

esquerda(ponto(X, Y), ponto(Z, Y)) :-
  Z is X - 1,
  ponto(Z, Y),
  not(bloqueado(ponto(Z, Y))).

direita(ponto(X, Y), ponto(Z, Y)) :-
  Z is X + 1,
  ponto(Z, Y),
  not(bloqueado(ponto(Z, Y))).

possiveis_caminhos(P, X) :-
    direita(P, X).

possiveis_caminhos(P, X) :-
    esquerda(P, X).

possiveis_caminhos(P, X) :-
    cima(P, X).

possiveis_caminhos(P, X) :-
    baixo(P, X).

%distância de manhattan

distancia(ponto(X1, Y1), ponto(X2, Y2), D):-
    D is abs(X1-X2) + abs(Y1-Y2).

%///////////////////////////////////////////////////
%///////////////////////////////////////////////////
%Manipulação de Lista

%Concatenação

concatena([], A, A).

concatena([X| Cauda],A, [X | Cauda2]):-
    concatena(Cauda, A, Cauda2).

%Comparação entre elementos para ordenação

pointCheck(X):-
    X = ponto(_,_).

menor([_, _, H1, Ponto1|Lista], [_, _, H2, _|_], [_, _, H1, Ponto1|Lista]):-
    not(pointCheck(H1); pointCheck(H2)), H1<H2.

menor([_, _, H1, _|_], [_, _, H2, Ponto|Lista], [_, _, H2, Ponto|Lista]):-
    not(pointCheck(H1); pointCheck(H2)), H1>=H2.

menor([Custo1, Ponto|Resto], [Custo2,_|_], [Custo1, Ponto|Resto]):-
    Custo1<Custo2.

menor([Custo1, _|_], [Custo2,Ponto|Resto], [Custo2, Ponto|Resto]):-
    Custo1>=Custo2.

%Ordenação

insertOrd([], X, [X]).

insertOrd([Prim|Cauda], X, [X, Prim | Cauda]):-
    menor(X, Prim, X).

insertOrd([Prim|Cauda], X, [Prim |Cauda2]):-
    insertOrd(Cauda, X, Cauda2).

ordena([], []).

ordena([X|Cauda], Out):-
    ordena(Cauda, CaudaOrd),
    insertOrd(CaudaOrd, X, Out).

%//////////////////////////////////////
%//////////////////////////////////////
% Heurísticas para decidir próxima sujeira

%sujeira mais próxima

sujeiraMaisProx(X, Suj):-
    setof([Dist, Y], 
          	(sujo(Y), distancia(X, Y, Dist)),
         [[_, Suj]|_]).

%melhor sujeira por quadrante

tamanho([], 0).
tamanho([_|Cauda], Tam):-
    tamanho(Cauda, Tam1),
    Tam is Tam1+1.

vect(ponto(X1, Y1), ponto(X2, Y2), vec(A, B)):-
     A is X2-X1, B is Y2-Y1.

mesmoQuadrante(vec(A, B), vec(C, D)):-
    Q1 is A*C,
    Q2 is B*D,
    Q1>=0, Q2>=0.

bloqueiosNoQuadrante(Pos, Suj, Num):-
    findall(Y, 
            (
            	bloqueado(Y), 
            	distancia(Suj, Y, Dist),
                distancia(Suj, Pos, Dist2),
                Dist<Dist2,
            	vect(Suj, Y, V1),vect(Suj,Pos, V2),
                mesmoQuadrante(V1, V2)),
            Lista),
	tamanho(Lista, Num).

melhorSujeira(MelhorSuj):-
	findall([A, Y],
          	(robo(X),sujo(Y), 
            distancia(X, Y, Dist),
            bloqueiosNoQuadrante(X, Y, Bloq),
            A is Dist+Bloq),
         Suj),
    ordena(Suj, [[_, MelhorSuj]|_]).

%melhor sujeira por caminho

caminhoR(ponto(X1, Y1),ponto(X2, Y2), caminho(Dx1, Dx2, Dy1, Dy2)):-
    (	(X1<X2, Dx1=X1, Dx2=X2);(Dx2=X1, Dx1=X2)),
    (   (Y1<Y2, Dy1=Y1, Dy2=Y2); (Dy2=Y1, Dy1 = Y2)), !.

noCaminho(caminho(A, B, C, D), ponto(X, Y)):-
    X>=A, X=<B, Y>=C, Y=<D.

bloqueiosNoCaminho(Pos, Suj, Num):-
	findall(Y, 
            (
            	bloqueado(Y),
                caminhoR(Pos, Suj, Caminho),
            	noCaminho(Caminho, Y)
            ),
            Lista),
	tamanho(Lista, Num).

melhorSujeira2(MelhorSuj):-
	findall([A, Y],
          	(robo(X),sujo(Y), 
            distancia(X, Y, Dist),
            bloqueiosNoCaminho(X, Y, Bloq),
            A is Dist+Bloq),
         Suj),
    ordena(Suj, [[_, MelhorSuj]|_]).

%///////////////////////////////////////////////////
%///////////////////////////////////////////////////
% predicados do A Estrela

%Estende

estendeA([_,Dist,_,X | Caminho], NovosCaminhos):-
    findall([Av, Dist2, Soma, Y, X | Caminho],
            (
            	possiveis_caminhos(X, Y),
                objetivo(Z),
                distancia(Z, Y, Av),
                Dist2 is Dist + 1,
                Soma is Dist2 + Av,
                not(member(Y, Caminho))
            ),NovosCaminhos).

% A*

aEstrela([[_, _, _, X| Caminho]|_], Solucao):-
    objetivo(X),
    reverse([X|Caminho], Solucao).

aEstrela([PrimeiroCaminho|Caminhos], Solucao):-
    estendeA(PrimeiroCaminho, NovosCaminhos),
    concatena(Caminhos, NovosCaminhos, CaminhosTotal),
    ordena(CaminhosTotal, CaminhosOrd),
    aEstrela(CaminhosOrd, Solucao).

%///////////////////////////////////////////////////
%///////////////////////////////////////////////////
% predicados do Branch and Bound

%Estende

estendeBB([Custo,X | Caminho], NovosCaminhos):-
    findall([CustoNovo, Y, X | Caminho],
            (
            	possiveis_caminhos(X, Y),
                CustoNovo is Custo + 1,
                not(member(Y, Caminho))
            ),NovosCaminhos).

% branch and bound

branchNBound([[_,X|Caminho]|_], Solucao):-
        objetivo(X),
    	reverse([X|Caminho], Solucao).

branchNBound([PrimeiroCaminho|Caminhos], Solucao):-
    estendeBB(PrimeiroCaminho, NovosCaminhos),
    concatena(NovosCaminhos, Caminhos, CaminhosTotais),
    ordena(CaminhosTotais, CaminhosOrd),
    branchNBound(CaminhosOrd, Solucao).

%///////////////////////////////////////////////////
%///////////////////////////////////////////////////
% predicados do Hill Climb

% Estende

estendeH([_, X|Caminho], NovosCaminhos):-
    findall([Aval1, Y, X|Caminho],
            (
            	possiveis_caminhos(X, Y),
            	objetivo(Z),
                distancia(Y,Z,Aval1),
                not(member(Y, Caminho))
            ), NovosCaminhos).

% Hill Climb

hillClimb([[_, X |Caminho]|_], Solucao):-
    objetivo(X),
    reverse([X|Caminho],Solucao).

hillClimb([PrimeiroCaminho| Caminhos], Solucao):-
    estendeH(PrimeiroCaminho, NovosCaminhos),
    ordena(NovosCaminhos, CaminhosOrd),
    concatena(CaminhosOrd, Caminhos, CaminhosTotais),
    hillClimb(CaminhosTotais, Solucao).

%///////////////////////////////////////////////////
%///////////////////////////////////////////////////
% predicados do Best First

% Estende

estendeB([_, X|Caminho], NovosCaminhos):-
    findall([Aval, Y, X|Caminho],
            (
            	possiveis_caminhos(X, Y),
                objetivo(Z),
                distancia(Y, Z, Aval),
                not(member(Y, Caminho))
            ), NovosCaminhos).

% Best First

bestFirst([[_, X | Caminho]|_], Solucao):-
    objetivo(X),
    reverse([X|Caminho], Solucao).

bestFirst([PrimeiroCaminho| Caminhos], Solucao):-
    estendeB(PrimeiroCaminho, NovosCaminhos),
    concatena(NovosCaminhos, Caminhos, CaminhosTotais),
    ordena(CaminhosTotais, CaminhosOrd),
    bestFirst(CaminhosOrd, Solucao).

%//////////////////////////////////////////
%//////////////////////////////////////////
%predicado para Limpar

%andaLimpa

andaLimpa([]):-!.

andaLimpa([X|Resto]):-
    retract(sujo(X)),
    retract(robo(_)),
    assert(robo(X)),
    andaLimpa(Resto).

andaLimpa([X|Resto]):-
    retract(robo(_)),
    assert(robo(X)),
    andaLimpa(Resto).

%limpar caso base

limpar(Tabuleiro, [], _):-
    not(sujo(_)),
    robo(X),
    destino(X),
    desenhar(7, Tabuleiro).
    
%limpar A*

limpar(Tabuleiro, Caminho, aEstrela):-
	melhorSujeira(Suj),
    retract(objetivo(_)),
    assert(objetivo(Suj)),
    robo(X),
    aEstrela([[0, 0, 0, X]], Solucao),
    andaLimpa(Solucao),
    limpar(Tabuleiro, Caminho1, aEstrela),
    concatena(Solucao, [limpa(Suj)], Caminho0),
    concatena(Caminho0, Caminho1, Caminho).

limpar(Tabuleiro, Caminho, aEstrela):-
	destino(D),
    retract(objetivo(_)),
    assert(objetivo(D)),
    robo(X),
    aEstrela([[0, 0, 0, X]], Solucao),
    andaLimpa(Solucao),
    limpar(Tabuleiro, Caminho1, aEstrela),
    concatena(Solucao, Caminho1, Caminho).

%Limpar Branch and Bound

limpar(A, Caminho, branchNBound):-
	melhorSujeira(Suj),
    retract(objetivo(_)),
    assert(objetivo(Suj)),
    robo(X),
    branchNBound([[0, X]], Solucao),
    andaLimpa(Solucao),
    limpar(A, Caminho1, branchNBound),
    concatena(Solucao, [limpa(Suj)], Caminho0),
    concatena(Caminho0, Caminho1, Caminho).

limpar(A, Caminho, branchNBound):-
	destino(D),
    retract(objetivo(_)),
    assert(objetivo(D)),
    robo(X),
    branchNBound([[0, X]], Solucao),
    andaLimpa(Solucao),
    limpar(A, Caminho1, branchNBound),
    concatena(Solucao, Caminho1, Caminho).

%limpar best first

limpar(A, Caminho, bestFirst):-
	melhorSujeira(Suj),
    retract(objetivo(_)),
    assert(objetivo(Suj)),
    robo(X),
    bestFirst([[0, X]], Solucao),
    andaLimpa(Solucao),
    limpar(A, Caminho1, bestFirst),
    concatena(Solucao, [limpa(Suj)], Caminho0),
    concatena(Caminho0, Caminho1, Caminho).

limpar(A, Caminho, bestFirst):-
	destino(D),
    retract(objetivo(_)),
    assert(objetivo(D)),
    robo(X),
    bestFirst([[0, X]], Solucao),
    andaLimpa(Solucao),
    limpar(A, Caminho1, bestFirst),
    concatena(Solucao, Caminho1, Caminho).

%limpar hill Climb

limpar(A, Caminho, hillClimb):-
	melhorSujeira(Suj),
    retract(objetivo(_)),
    assert(objetivo(Suj)),
    robo(X),
    hillClimb([[0, X]], Solucao),
    andaLimpa(Solucao),
    limpar(A, Caminho1, hillClimb),
    concatena(Solucao, [limpa(Suj)], Caminho0),
    concatena(Caminho0, Caminho1, Caminho).

limpar(A, Caminho, hillClimb):-
	destino(D),
    retract(objetivo(_)),
    assert(objetivo(D)),
    robo(X),
    hillClimb([[0, X]], Solucao),
    andaLimpa(Solucao),
    limpar(A, Caminho1, hillClimb),
    concatena(Solucao, Caminho1, Caminho).

%///////////////////////////////
%///////////////////////////////
%predicados para desenhar a sala

desenharLinha(_, 8, []):-!.

desenharLinha(I, J, ["S"|Caminho]):-
    sujo(ponto(I, J)),
    J1 is J + 1,
    desenharLinha(I, J1, Caminho).

desenharLinha(I, J, ["B"|Caminho]):-
    bloqueado(ponto(I, J)),
    J1 is J + 1,
    desenharLinha(I, J1, Caminho).

desenharLinha(I, J, ["R"|Caminho]):-
    robo(ponto(I, J)),
    J1 is J + 1,
    desenharLinha(I, J1, Caminho).

desenharLinha(I, J, ["D"|Caminho]):-
    destino(ponto(I, J)),
    J1 is J + 1,
    desenharLinha(I, J1, Caminho).

desenharLinha(I, J, ["L"|Caminho]):-
    J1 is J + 1,
    desenharLinha(I, J1, Caminho).

desenhar(-1, []).

desenhar(I, [Atual|Linhas]):-
    desenharLinha(I, 0, Atual),
    I1 is I-1,
    desenhar(I1, Linhas).




calcular_tempo_e_nos_expandidos(Codigo, Tempo, NosExpandidos) :-
    statistics(cputime, Inicio),
    contar_nos_expandidos(Codigo, 0, NosExpandidos),
    statistics(cputime, Fim),
    Tempo is Fim - Inicio.

contar_nos_expandidos(Codigo, NosAnteriores, TotalNos) :-
    retractall(contador_nos_expandidos(_)),
    assert(contador_nos_expandidos(NosAnteriores)),
    call(Codigo),
    retract(contador_nos_expandidos(Nos)),
    TotalNos is Nos.

incrementar_nos_expandidos :-
    retract(contador_nos_expandidos(N)),
    N1 is N + 1,
    assert(contador_nos_expandidos(N1)).