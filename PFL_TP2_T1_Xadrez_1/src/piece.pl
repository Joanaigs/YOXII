:- use_module(library(random)).

/** existPiece(+Pieces, -Piece)
 *      @param Pieces - list of pieces
 *      @param Piece - piece to check
 *      
 *      This predicate is used to check if a piece exists in the list of pieces.
 *      It returns true if the piece exists, false otherwise.
 *
 * */
existPiece([Piece-Number | _], Piece) :- Number > 0.
existPiece([_Symbol-_ | List], Piece) :- existPiece(List, Piece).

/** usePiece(+Pieces, +Piece, -NewPieces)
 *      @param Pieces - list of pieces
 *      @param Piece - piece to use
 *      @param NewPieces - list of pieces with the piece used
 *      
 *      This predicate is used to subtract the number of that piece from the list of pieces.
 *      It returns the list of pieces with the piece used.
 *
 * */
usePiece([Piece-Number | List], Piece, [Piece-N | List])  :- N is Number-1.
usePiece([Symbol-Number| List], Piece, [Symbol-Number| NewList]) :- usePiece(List, Piece, NewList).

/** readPiece(+Pieces, -Piece, +Player)
 *      @param Pieces - list of pieces
 *      @param Piece - piece to read
 *      @param Player - current player
 *      
 *      This predicate is used to read a piece from the input.
 *      It returns the piece read.
 *
 * */
readPiece(Pieces, Piece, Player) :- get_char(Symbol), skip_line,
                                    symbol(Piece, Symbol, Player), existPiece(Pieces, Piece).    


/**
 *choose_move_piece(+GameState, +Player, -NewGameState, +Mode)
 *      @param GameState - current GameState
 *      @param Player - current player
 *      @param NewGameState - new GameState with the token moved
 *      Mode - 'H' for human, 'PC' for computer
 *  
 *      This predicate is used to choose the move of a piece of the Player choosing.
 *          If the mode is 'H' the player chooses the move (from input), 
 *          if the mode is 'PC' the computer chooses the move using diferent algoritms .
 *              level 1 - random move 
 *  
*/
choose_move_piece(GameState, Player, NewGameState, Pieces, NewPieces, 'H'):- nl,
                                write('Choose Piece to add to the board: '), 
                                readPiece(Pieces, Piece, Player),
                                write('Possible moves'), nl,
                                valid_moves_piece(GameState, Moves), change_number_letter(Moves, Result), 
                                write(Result),
                                nl,
                                movePiece(GameState, NewGameState, Moves, Piece),
                                usePiece(Pieces, Piece, NewPieces).
choose_move_piece(GameState, Player, NewGameState, Pieces, NewPieces, 'H') :- write('Invalid Piece'), nl, choose_move_piece(GameState, Player, NewGameState, Pieces, NewPieces).


choose_move_piece(GameState, _, NewGameState, Pieces, NewPieces, 'PC'-1):- 
    setof(Piece, existPiece(Pieces, Piece), PiecesList),
    random_member(Piece, PiecesList),
    valid_moves_piece(GameState, Moves),
    random_member(Line-Column, Moves),
    move(GameState, Line, Column, Piece, NewGameState),
    usePiece(Pieces, Piece, NewPieces),
    letter(Line, CharLine),
    symbol(Piece, Symbol, _),
    nl,write('Add piece '), write(Symbol) , write(' to ') , write(CharLine-Column), nl.

    

/**
 * movePiece(+GameState, -NewGameState, +Player)
 *      @param GameState - current GameState
 *      @param NewGameState - new GameState with the token moved
 *      @param Player - current player
 *      
 *      This predicate is used to move the token.
 *      It shows the possible moves and asks the player to choose one.
 *       If the move is not valid, it asks again.
 *      Then it moves the token to the chosen position, and stores the play in NewGameState.
 *
 * */
movePiece(GameState, NewGameState, Moves, Piece) :-
                                    write('Choose the piece position (Ex: d-3): '),
                                    getPosition(Line-Column),
                                    select_move(Line-Column, Moves),
                                    move(GameState, Line, Column, Piece, NewGameState).
movePiece(GameState, NewGameState, Moves, Piece) :- write('Invalid Position'), nl, movePiece(GameState, NewGameState, Moves, Piece). 


/**
 * select_move(+Line-Column, +Moves)
 *      @param Line-Column - position to check
 *      @param Moves - list of valid moves
 *      
 *      This predicate is used to check if a move is valid.
 *      It returns true if the position is empty and it is around the token, false otherwise.
 *
 * */

/**
 * check_move_piece(+GameState, -Line-Column)
 *      @param GameState - current GameState
 *      @param Line-Column - position to check
 *      
 *      This predicate is used to check if a move is valid.
 *      It returns true if the position is empty and it is around the token, false otherwise.
 */
check_move_piece(GameState, Line-Column) :- getPiece(GameState, Line-Column, Piece), 
                                                Piece == empty,
                                                getPiece(GameState, TLine-TColumn, token),  DiffLine is abs(TLine - Line), DiffCol is abs(TColumn - Column),                                       
                                                ((DiffLine==0 , DiffCol==1) ; (DiffLine==1, DiffCol==0) ; (DiffLine==1, DiffCol==1)). 


/**
 * valid_moves_piece(+GameState, -Moves)
 *      @param GameState - current GameState
 *      @param Moves - list of valid moves
 *      
 *      This predicate is used to get the valid moves of a piece.
 *      It returns a list of empty cells around the token.
 *      If there are no valid moves, it returns a list of all empty positions.
 *
 * */

valid_moves_piece(GameState, Moves):-
    setof(Line-Column, check_move_piece(GameState, Line-Column), Moves).
valid_moves_piece(GameState, Moves):-
    setof(Line-Column, getPiece(GameState, Line-Column, empty), Moves).


