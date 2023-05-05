/*
Primeiro Trabalho
Grupo:
  Filipe Brinati Furtado  -  201865563C
*/

:- dynamic occupied/3.
:- dynamic boardSize/2.


% Criação da Mesa -------------------------------------
createBoardSimp(X) :-
    (   X < 0 -> write("Tabela vazia"));!,
    (   G is X+1,
        assertz(boardSize(X,G)),
        write("Board Simplificado Criado \n"),
        write("Use a funcao 'insertXSimp(X)' para inserir\n")
    ).

createBoardNormal(X,Y) :-
    (   X =< 0 -> write("Tabela vazia"));!,
    (   Y =< 0 -> write("Tabela vazia"));!,

    (   assertz(boardSize(X,Y)),
        write("Board Normal Criado \n"),
        write("Use a funcao 'insertXNormal(X)' para inserir\n")

    ).
%------------------------------------------------------


% Checa se o jogo terminou ----------------------------
checkFinish(Simb):-
    boardSize(M,N),
    (
        (   checkFinishDirectAux(M,N,Simb),!);
        (   checkFinishIndirectAux(0, 0, Simb),!)
    ),
    write("Jogo Concluido").


%Checa do máximo para zero.
checkFinishIndirectAux(X,Y,Simb):-
    boardSize(M,N),(
    (   (   X < M,
        (
                (
                 checkFinishDiagonalLR(X, Y, Simb),
                !
                );
                (  F is X+1,
                   checkFinishIndirectAux(F,Y,Simb)
                )
        )
    ),!);
    (   Y < N,
        (
                (
                 checkFinishDiagonalLR(X, Y, Simb),
                !
                );
                (  D is Y+1,
                   checkFinishIndirectAux(X,D,Simb)
                )
        )
    )).

%Checa do máximo para zero.
checkFinishDirectAux(X,Y,Simb):-
    (   X > 0,
        (
                (
                (   checkFinishVertical(X, Y, Simb),!);
                (   checkFinishHorizontal(X, Y, Simb),!);
                (   checkFinishDiagonalRL(X, Y, Simb),!)
                );
                (  F is X-1,
                   checkFinishDirectAux(F,Y,Simb)
                )
        )
    );
    (   Y > 0,
        (
                (
                (   checkFinishVertical(X, Y, Simb),!);
                (   checkFinishHorizontal(X, Y, Simb),!);
                (   checkFinishDiagonalRL(X, Y, Simb),!)
                );
                (  D is Y-1,
                   checkFinishDirectAux(X,D,Simb)
                )
        )
    ).


%Checa se a horizontal esta preenchida
checkFinishHorizontal(X, Y, Simb):-
    (   X > 0,
        occupied(X,Y,Simb),
        F is X-1,
        checkFinishHorizontal(F, Y, Simb),
        !
    );
    (   X = 0,
        occupied(X,Y,Simb)
    ).

%Checa se a vertical esta preenchida com um so símbolo
checkFinishVertical(X, Y, Simb):-
    (   Y > 0,
        occupied(X,Y,Simb),
        F is Y-1,
        checkFinishVertical(X, F, Simb),
        !
    );
    (   Y = 0,
        occupied(X,Y,Simb)
    ).

%checa diagonal da direita para a esquerda
checkFinishDiagonalRL(X, Y,Simb):-
    (   X>0,
        Y>0,
        occupied(X,Y,Simb),
        I is X-1,
        J is Y-1,
        checkFinishDiagonalRL(I,J,Simb),
        !
    );
    (
        (   X = 0 ; Y = 0),
        occupied(X,Y,Simb)
    ).

%Checa diagonal da esquerda para a direita
checkFinishDiagonalLR(X, Y,Simb):-
    (   boardSize(M,N)
    ),(
    (   X < M,
        Y < N,
        occupied(X,Y,Simb),
        I is X+1,
        J is Y+1,
        checkFinishDiagonalLR(I,J,Simb),
        !
    );
    (
        (   X = M ; Y = N),
        occupied(X,Y,Simb)
    )).
%------------------------------------------------------


%Função para inserir X --------------------------------
insertXNormal(X,Y):-
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
    occupied(X,Y,x),
    write("Espaco ocupado\n"),
    !
);
       (
           assertz(occupied(X,Y,x)),
           write("X inserido\n")
       ).

insertXSimp(X):-
    (  boardSize(M,_),
       X > M ,
       write("Fora da Mesa\n"),
       !
    );
    insertXAux(X,0).


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


%Função para inserir X --------------------------------
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
       write("Fora da Mesa\n"),
       !
    );
    insertOAux(X,0).


insertOAux(X,Y):-
    (
    boardSize(_,N),
    Y > N,
    write("Coluna preenchida"),
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



%Destruir os dados criados ----------------------------
destroy():-
    retract(occupied(_,_,_));
    retract(boardSize(_,_)).
%------------------------------------------------------

