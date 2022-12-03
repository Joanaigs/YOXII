initialBoard([
    [invalid,invalid,empty,empty,empty,invalid, invalid],
    [invalid,empty,empty,empty,empty,empty,invalid],
    [empty,empty,empty,empty,empty,empty,empty],
    [empty,empty,empty,token,empty,empty,empty],
    [empty,empty,empty,empty,empty,empty,empty],
    [invalid,empty,empty,empty,empty,empty,invalid],
    [invalid,invalid,empty,empty,empty,invalid, invalid]
    ]).




symbol(invalid,S) :- S='-'.
symbol(empty,S) :- S=' '.
symbol(blackOne,S) :- S='O'.
symbol(blackTwo,S) :- S='='.
symbol(blackThree,S) :- S='Y'.
symbol(blackFour,S) :- S='X'.
symbol(whiteOne,S) :- S='o'.
symbol(whiteTwo,S) :- S='"'.
symbol(whiteThree,S) :- S='y'.
symbol(whiteFour,S) :- S='x'.
symbol(token,S) :- S='#'.

letter(1, L) :- L='A'.
letter(2, L) :- L='B'.
letter(3, L) :- L='C'.
letter(4, L) :- L='D'.
letter(5, L) :- L='E'.
letter(6, L) :- L='F'.
letter(7, L) :- L='G'.
letter(8, L) :- L='H'.
letter(9, L) :- L='I'.
letter(10, L) :- L='J'.
letter(11, L) :- L='K'.

piecesWhite(['o', '"', 'y', 'x']).
initialPiecesWhite([5, 5, 5, 3]).
piecesBlack(['O', '=', 'Y', 'X']).
initialPiecesBlack([5, 5, 5, 3]).


drawGame(Board, WhitePieces, BlackPieces) :-
    nl,
    write('          White Pieces:'),nl,
    piecesWhite(WhiteSymbols),
    write('           '),
    drawPieces(WhiteSymbols), nl,
    write('           '),
    drawPieces(WhitePieces),
    nl,
    printBoard(Board),
    nl,
    write('          Black Pieces:'),
    nl,
    piecesBlack(BlackSymbols),
    write('           '),
    drawPieces(BlackSymbols), nl,
    write('           '),
    drawPieces(BlackPieces).


drawPieces([]).
drawPieces([X|XS]) :- 
                write(X),
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
    symbol(Head, S),
    write(S),
    write(' | '),
    printLine(Tail).