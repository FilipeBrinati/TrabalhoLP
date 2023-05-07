/*
Primeiro Trabalho
Grupo:
  Filipe Brinati Furtado  -  201865563C
  Lucca Oliveira Schroder - 201765205C
*/

:- dynamic occupied/3.
:- dynamic boardSize/2.
:- dynamic boardDistance/3.
:- dynamic lastPosition/2.
:- dynamic minValue/3.
:- dynamic open/2.


% Criacao da Mesa -------------------------------------
createBoardSimp(X) :-
    (   X < 0 -> write("Tabela vazia"));!,
    (   G is X+1,
        assertz(boardSize(X,G)),
        write("Board Simplificado Criado \n"),
        write("Use a funcao 'insertXSimp(X)' para inserir na coluna X\n"),
        write("Use a funcao 'checkFinish(x)' para ver se ganhou\n"),
        write("Use a funcao 'checkFinish(o)' para ver se perdeu\n"),
        write("Use a funcao 'empate(0,0)' para ver se deu velha\n"),
        write("Se der apenas true, o jogo acabou"),
        write("Use 'destroy()' para apagar todos os dados e comecar um novo jogo")
    ).

createBoardNormal(X,Y) :-
    (   X =< 0, write("Tabela vazia"),!);
    (   Y =< 0, write("Tabela vazia"),!);

    (   assertz(boardSize(X,Y)),
        write("Board Normal Criado \n"),
        write("Use a funcao 'insertXNormal(X,Y)' para inserir na posicao (X,Y)\n"),
        write("Use a funcao 'checkFinish(x)' para ver se ganhou\n"),
        write("Use a funcao 'checkFinish(o)' para ver se perdeu\n"),
        write("Use a funcao 'empate(0,0)' para ver se deu velha\n"),
        write("Se der apenas true, o jogo acabou"),
        write("Use 'destroy()' para apagar todos os dados e comecar um novo jogo")
    ).
%------------------------------------------------------

% Criando distancias e pesos --------------------------
/*pesoDistancia(X,Y):-
    (   boardSize(M,_),
        X > M,
        write("X maximo"),
        !
    );
    (   boardSize(_,N),
        Y > N,
        write("Y maximo"),
        !
    );
    (   (boardDistance(X,Y,_))->(
        F is X+1,
        D is Y+1,
        pesoDistancia(F, Y),
        pesoDistancia(X, D),
        pesoDistancia(F, D));(
        (   X >= Y ->(Z is X-Y); Z is Y-X),
        assertz(boardDistance(X,Y,Z)),
        F is X+1,
        D is Y+1,
        pesoDistancia(F, Y),
        pesoDistancia(X, D),
        pesoDistancia(F, D))
    ).
Pesos agora sao criados de acordo com a distancia dos nos com
o ultimo espaco ocupado por o.

*/
%------------------------------------------------------

% Achando melhor lugar para inserir O -----------------

findBestPlaceNormal() :-
    (   lastPosition(N,M) ->
    ( findBestPlaceNormalAux(N,M,0,0),
      minValue(X,Y,_),
      insertONormal(X,Y),
      retract(lastPosition(_,_)),
      assertz(lastPosition(X,Y)),
      retract(minValue(_,_,_)),
      !
    );
    ( findEmptySpace(0,0),
      open(X1,Y1),
      insertONormal(X1,Y1),
      assertz(lastPosition(X1,Y1)),
      retract(open(_,_)),
      !
    )
    ).

findBestPlaceSimp() :-
    (   lastPosition(N,M) ->
    ( findBestPlaceNormalAux(N,M,0,0),
      minValue(X,Y,_),
      insertOSimp(X),
      retract(lastPosition(_,_)),
      assertz(lastPosition(X,Y)),
      retract(minValue(_,_,_)),
      !
    );
    ( findEmptySpace(0,0),
      open(X1,Y1),
      insertOSimp(X1),
      assertz(lastPosition(X1,Y1)),
      retract(open(_,_)),
      !
    )
    ).


findBestPlaceNormalAux(N,M,X,Y) :-
    (   (   boardSize(M1,_),
        X > M1,
        %write("X maximo"),
        !
    );
    (   boardSize(_,N1),
        Y > N1,
        %write("Y maximo"),
        !
    );
    (
        occupied(X,Y,_),
        %write("Espaco ocupado"),
        X1 is X+1,
        Y1 is Y+1,
        findBestPlaceNormalAux(N,M,X1,Y),
        findBestPlaceNormalAux(N,M,X,Y1),
        findBestPlaceNormalAux(N,M,X1,Y1),
        !
    );
    (   minValue(_,_,I)->(
            (   N >= X ->(D is N-X); D is X-N),
            (   M >= Y ->(F is M-Y); F is Y-M),
            (   I1 is D+F),
            (   I1 < I ->
            (retract(minValue(_,_,_)), assertz(minValue(X,Y,I1)));
            (   X1 is X+1,
                Y1 is Y+1,
                findBestPlaceNormalAux(N,M,X1,Y),
                findBestPlaceNormalAux(N,M,X,Y1),
                findBestPlaceNormalAux(N,M,X1,Y1)
            )
            )
        );
    (
                             (   N >= X ->(D is N-X); D is X-N),
                             (   M >= Y ->(F is M-Y); F is Y-M),
                             (   I1 is D+F),
                             assertz(minValue(X,Y,I1))


                         )
    )
).

findEmptySpace(X,Y) :-
    (   boardSize(M,_),
        X > M,
        %write("X maximo"),
        !
    );
    (   boardSize(_,N),
        Y > N,
        %write("Y maximo"),
        !
    );

    ( occupied(X,Y,_) ->
    (   X1 is X+1,
        Y1 is Y+1,
        findEmptySpace(X1,Y),
        findEmptySpace(X,Y1),
        findEmptySpace(X1,Y1)
    );
    (
       assertz(open(X,Y)),!
    )
).


% Checa se o jogo terminou ----------------------------
checkFinish(Simb):-
    boardSize(M,N),
    (
        (   checkFinishHorizontal(M,N,Simb), !);
        (   checkFinishVertical(M,N,Simb), !);
        (   checkFinishDiagonalRL(M,N,0,Simb,M,N), !);
        (   checkFinishDiagonalLR(0,N,0,Simb,M,N), !)
    ),
    write("Jogo Concluido").

%Checa se a horizontal esta preenchida
checkFinishHorizontal(X,Y,Simb):-
    (   Y >= 0,
        (   checkFinishHorizontalAux(X,Y,Simb);(
            F is Y-1,
            checkFinishHorizontal(X,F,Simb)))
    ).
checkFinishHorizontalAux(X, Y, Simb):-
    (X >= 0,
     occupied(X,Y,Simb),
     F is X-1,
     checkFinishHorizontalAux(F,Y,Simb)
    );
    (X =:= 0,
     occupied(X,Y,Simb)
    ).

%Checa se a vertical esta preenchida com um so simbolo
checkFinishVertical(X, Y, Simb):-
    (   X >= 0,
        (   checkFinishVerticalAux(X,Y,Simb);(
            F is X-1,
            checkFinishVertical(F,Y,Simb)))
    ).
checkFinishVerticalAux(X, Y, Simb):-
    (Y >= 0,
     occupied(X,Y,Simb),
     F is Y-1,
     checkFinishVerticalAux(X,F,Simb)
    );
    (Y =:= 0,
     occupied(X,Y,Simb)
    ).

%Checa se a diagonal da direita para esquerda esta preenchida
checkFinishDiagonalRL(X,Y,Z,Simb,M,N) :-
    ( D is M-N,
      Z =< D,
      (   checkFinishDiagonalRLAux(X,Y,Simb);(
              F is Z+1,
              G is X-1,
              checkFinishDiagonalRL(G,Y,F,Simb,M,N)))
    ).

checkFinishDiagonalRLAux(X, Y,Simb):-
    (   X>0,
        Y>=0,
        occupied(X,Y,Simb),
        I is X-1,
        J is Y-1,
        checkFinishDiagonalRLAux(I,J,Simb)
    );
    (
        Y =:= 0,
        occupied(X,Y,Simb)
    ).

%Checa se a diagonal da esquerda para direita esta preenchida
checkFinishDiagonalLR(X,Y,Z,Simb,M,N) :-
    ( D is M-N,
      Z =< D,
      (   checkFinishDiagonalLRAux(X,Y,Simb,M);(
              F is Z+1,
              G is X+1,
              checkFinishDiagonalLR(G,Y,F,Simb,M,N)))
    ).

checkFinishDiagonalLRAux(X, Y,Simb,M):-
    (   X<M,
        Y>0,
        occupied(X,Y,Simb),
        I is X+1,
        J is Y-1,
        checkFinishDiagonalLRAux(I,J,Simb,M)
    );
    (
        Y =:= 0,
        occupied(X,Y,Simb)
    ).


%------------------------------------------------------

%Checa se ouve empate ---------------------------------

empate(X,Y):-
    (   boardSize(M,_),
        X > M,
        %write("X maximo"),
        !
    );
    (   boardSize(_,N),
        Y > N,
        %write("Y maximo"),
        !
    );
    (   (occupied(X,Y,_))->(
        F is X+1,
        D is Y+1,
        empate(F, Y),
        empate(X, D),
        empate(F, D));(
        write("Jogo nao acabado"),!)
    ).

%------------------------------------------------------


%Funcaopara inserir X --------------------------------
insertXNormal(X,Y):-
    (
    boardSize(M,_),
    X > M,
    %write("Fora dos Limites"),
    !
);
    (
    boardSize(_,N),
    Y > N,
    %write("Fora dos Limites"),
    !
);

    (
    occupied(X,Y,x),
    write("Espaco ocupado\n"),
    !
);
       (
        assertz(occupied(X,Y,x)),
        write("X inserido\n"),
        findBestPlaceNormal()
       ).

insertXSimp(X):-
    (  boardSize(M,_),
       X > M ,
       %write("Fora da Mesa\n"),
       !
    );
    insertXAux(X,0),
    findBestPlaceSimp().


insertXAux(X,Y):-
    (
    boardSize(_,N),
    Y > N,
    write("Coluna preenchida"),
    !
);
    (
        occupied(X,Y,x)->(
        F is Y+1,
        insertXAux(X,F))
    ;
    (
        assertz(occupied(X,Y,x)),
        write("X inserido")
    )).

%------------------------------------------------------


%Funcao para inserir O --------------------------------
insertONormal(X,Y):-
    (
    boardSize(M,_),
    X > M,
    write("Fora dos Limites"),
    !
);
    (
        boardSize(_,N),
        Y > N,
        write("Fora dos Limites"),
        !
    );

    (
        occupied(X,Y,o),
        write("Espaco ocupado\n"),
        !
);
    (
        assertz(occupied(X,Y,o)),
        write("O inserido\n")
    ).

insertOSimp(X):-
    (  boardSize(M,_),
       X > M ,
       %write("Fora da Mesa\n"),
       !
    );
    insertOAux(X,0).


insertOAux(X,Y):-
    (
    boardSize(_,N),
    Y > N,
    %write("Coluna preenchida"),
    !
);
    (
        occupied(X,Y,o)->(
        F is Y+1,
        insertXAux(X,F))
    ;
    (
        assertz(occupied(X,Y,o)),
        write("O inserido")
    )).

%------------------------------------------------------

%Check Best Position ----------------------------------


%Destruir os dados criados ----------------------------
destroy():-
    retract(occupied(_,_,_));
    retract(boardSize(_,_));
    retract(boardDistance(_,_,_)),
    retract(lastPosition(_,_)),
    retract(minValue(_,_,_)),
    retract(open(_,_)).
%------------------------------------------------------

