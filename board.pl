/**
 * initialBoard( -Board)
 *     @param Board is the initial Board.
 *
 *  Returns the initial board
*/
initialBoard([
    [invalid,invalid,empty,empty,empty,invalid, invalid],
    [invalid,empty,empty,empty,empty,empty,invalid],
    [empty,empty,empty,empty,empty,empty,empty],
    [empty,empty,empty,token,empty,empty,empty],
    [empty,empty,empty,empty,empty,empty,empty],
    [invalid,empty,empty,empty,empty,empty,invalid],
    [invalid,invalid,empty,empty,empty,invalid, invalid]
    ]).





/**
 * symbol( ?Piece, ?Symbol, ?Color)
 *      @param Piece is a piece of the game.
 *      @param Symbol is the symbol that represents the piece.
 *      @param Color is the color of the piece.
 * 
 *      Converts a piece to a symbol or a symbol to a piece.
 *      if only color is given, returns all pieces of that color.
*/
symbol(empty,' ', _).
symbol(invalid,'@', _).
symbol(whiteOne,'O', white).
symbol(whiteTwo,'T', white).
symbol(whiteThree,'Y', white).
symbol(whiteFour,'X', white).
symbol(redOne,'o', red).
symbol(redTwo,'t', red).
symbol(redThree,'y', red).
symbol(redFour,'x', red).
symbol(token,'#', _).

/**
 * letter( ?Number, ?Letter)
 *     @param Number.
 *     @param Letter.
 * 
 *   Converts a number to a letter and vice versa.
*/
letter(1, 'a').
letter(2, 'b').
letter(3, 'c').
letter(4, 'd').
letter(5, 'e').
letter(6, 'f').
letter(7, 'g').

/**
* points( +Piece, -Points)
*       @param Piece is a piece of the game.
*       @param Points is the number of points that the piece is worth.
*
*       Returns the points of a piece.
*/
points(redOne, 1).
points(redTwo, 2).
points(redThree, 3).
points(redFour, 4).
points(whiteOne, 1).
points(whiteTwo, 2).
points(whiteThree, 3).
points(whiteFour, 4).
points(token, 0).
points(empty, 0).
points(invalid, 0).

/**
 * initialPiecesRed( -Pieces)
 *      @param Pieces is a list of pieces and the number of pieces of that type. [Piece-Number, Piece-Number, ...]
 * 
 *      Returns the initial pieces of the red player.
*/
initialPiecesRed([redOne-5, redTwo-5, redThree-5, redFour-3]).

/**
 * initialPiecesWhite( -Pieces)
 *      @param Pieces is a list of pieces and the number of pieces of that type. [Piece-Number, Piece-Number, ...]
 * 
 *      Returns the initial pieces of the white player.
*/
initialPiecesWhite([whiteOne-5, whiteTwo-5, whiteThree-5, whiteFour-3]).

/**
 * display_game( +GameState, +Player, +RedPieces, +WhitePieces)
 *     @param GameState is the current GameState.
 *     @param Player is the current player.
 *     @param RedPieces is a list of pieces and the number of pieces of that type. [Piece-Number, Piece-Number, ...]
 *     @param WhitePieces is a list of pieces and the number of pieces of that type. [Piece-Number, Piece-Number, ...]   
 *
 *      Draws the current game board, and the pieces of each player.
*/
display_game(GameState, Player, RedPieces, WhitePieces) :-
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
    printBoard(GameState),
    nl,
    write('          white Pieces:'),nl,
    write('           '),
    drawSymbols(WhitePieces), nl,
    write('           '),
    drawPieces(WhitePieces).


/**
 * drawSymbols( +Pieces)
 *      @param Pieces is a list of pieces and the number of pieces of that type. [Piece-Number, Piece-Number, ...]
 * 
 *      Draws the symbols of the list pieces.
*/
drawSymbols([]).
drawSymbols([X-_n|XS]) :- 
                symbol(X, S, _),
                write(S),
                write('  '),
                drawSymbols(XS).

/**
 * drawPieces( +Pieces)
 *      @param Pieces is a list of pieces and the number of pieces of that type. [Piece-Number, Piece-Number, ...]
 * 
 *      Draws the number of pieces of the list pieces.
*/
drawPieces([]).
drawPieces([_x-N|XS]) :- 
                write(N),
                write('  '),
                drawPieces(XS).
                    
                    

/**
 * printBoard( +Board)
 *      @param Board is a list of lines of the board.
 * 
 *      Prints the board.
 */
printBoard(X) :-
    nl,
    write('   | 1 | 2 | 3 | 4 | 5 | 6 | 7 \n'),
    write('---|---|---|---|---|---|---|---\n'),
    printMatrix(X, 1).

/**
 * printMatrix( +Matrix, +N)
 *      @param Matrix is a list of lines of the board.
 *      @param N is the number of the line that it is printing.
 * 
 *      Prints the  Board line by line.
 *      It prints the line letter and then the symbols of each cell.
*/
printMatrix([], 8).
printMatrix([Head|Tail], N) :-
    letter(N, L),
    write(' '),
    write(L),
    N1 is N + 1,
    write(' |'),
    printLine(Head),
    write('\n---|---|---|---|---|---|---|---|\n'),
    printMatrix(Tail, N1).


/**
 * printLine( +Line)
 *      @param Line is a list of pieces.
 *
 *      Prints the symbols of each cell in the line.
*/
printLine([]).
printLine([Head|Tail]) :-
    symbol(Head, S, _),
    write(' '),
    write(S),
    write(' |'),
    printLine(Tail).
