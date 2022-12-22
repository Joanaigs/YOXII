initialBoard([
    [invalid,invalid,empty,empty,token,invalid, invalid],
    [invalid,empty,empty,empty,empty,empty,invalid],
    [empty,empty,empty,empty,empty,empty,empty],
    [empty,empty,empty,empty,empty,empty,empty],
    [empty,empty,empty,empty,empty,empty,empty],
    [invalid,empty,empty,empty,empty,empty,invalid],
    [invalid,invalid,empty,empty,empty,invalid, invalid]
    ]).






symbol(invalid,'-', _).
symbol(empty,' ', _).
symbol(blackOne,'O', black).
symbol(blackTwo,'=', black).
symbol(blackThree,'Y', black).
symbol(blackFour,'X', black).
symbol(redOne,'o', red).
symbol(redTwo,'"', red).
symbol(redThree,'y', red).
symbol(redFour,'x', red).
symbol(token,'#', _).

letter(1, 'a').
letter(2, 'b').
letter(3, 'c').
letter(4, 'd').
letter(5, 'e').
letter(6, 'f').
letter(7, 'g').

points(redOne, 1).
points(redTwo, 2).
points(redThree, 3).
points(redFour, 4).
points(blackOne, 1).
points(blackTwo, 2).
points(blackThree, 3).
points(blackFour, 4).
points(token, 0).
points(empty, 0).
points(invalid, 0).



/*
piecesred(['0', '"', 'y', 'x']).
initialPiecesred([5, 5, 5, 3]).
piecesBlack(['O', '=', 'Y', 'X']).
initialPiecesBlack([5, 5, 5, 3]).
*/

initialPiecesRed([redOne-5, redTwo-5, redThree-5, redFour-3]).
initialPiecesBlack([blackOne-5, blackTwo-5, blackThree-5, blackFour-3]).

drawGame(Board, Player, RedPieces, BlackPieces) :-
    nl,
    write('          Player '),
    write(Player),
    nl,nl,
    write('          red Pieces:'),nl,
    write('           '),
    drawSymbols(RedPieces), nl,
    write('           '),
    drawPieces(RedPieces),
    nl,
    printBoard(Board),
    nl,
    write('          Black Pieces:'),nl,
    write('           '),
    drawSymbols(BlackPieces), nl,
    write('           '),
    drawPieces(BlackPieces).


drawSymbols([]).
drawSymbols([X-_n|XS]) :- 
                symbol(X, S, _),
                write(S),
                write('  '),
                drawSymbols(XS).
drawPieces([]).
drawPieces([_x-N|XS]) :- 
                write(N),
                write('  '),
                drawPieces(XS).
                    
                    


printBoard(X) :-
    nl,
    write('   | 1 | 2 | 3 | 4 | 5 | 6 | 7 \n'),
    write('---|---|---|---|---|---|---|---\n'),
    printMatrix(X, 1).

printMatrix([], 8).

printMatrix([Head|Tail], N) :-
    letter(N, L),
    write(' '),
    write(L),
    N1 is N + 1,
    write(' | '),
    printLine(Head),
    write('\n---|---|---|---|---|---|---|---|\n'),
    printMatrix(Tail, N1).

printLine([]).

printLine([Head|Tail]) :-
    symbol(Head, S, _),
    write(S),
    write(' | '),
    printLine(Tail).