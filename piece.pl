:- use_module(library(random)).
:- use_module(library(system)).

existPiece([Piece-Number | _], Piece) :- Number > 0.
existPiece([_Symbol-_ | List], Piece) :- existPiece(List, Piece).

usePiece([Piece-Number | List], Piece, [Piece-N | List])  :- N is Number-1.
usePiece([Symbol-Number| List], Piece, [Symbol-Number| NewList]) :- usePiece(List, Piece, NewList).

readPiece(Pieces, Piece, Player) :- get_char(Symbol), skip_line,
                                    symbol(Piece, Symbol, Player), existPiece(Pieces, Piece).    


/**
 *choose_move_piece(+Board, +Player, -NewBoard, +Mode)
 *      @param Board - current board
 *      @param Player - current player
 *      @param NewBoard - new board with the token moved
 *      Mode - 'H' for human, 'PC' for computer
 *  
 *      This predicate is used to choose the move of a piece of the Player choosing.
 *          If the mode is 'H' the player chooses the move (from input), 
 *          if the mode is 'PC' the computer chooses the move using diferent algoritms .
 *              level 1 - random move
 *              level 2 - greedy algorithm    
 *  
*/
choose_move_piece(Board, Player, NewBoard, Pieces, NewPieces, 'H'):- nl,
                                write('Choose Piece to add to the board: '), 
                                readPiece(Pieces, Piece, Player),
                                write('Possible moves'), nl,
                                valid_moves_piece(Board, Moves), change_number_letter(Moves, Result), 
                                write(Result),
                                nl,
                                movePiece(Board, NewBoard, Moves, Piece),
                                usePiece(Pieces, Piece, NewPieces).
choose_move_piece(Board, Player, NewBoard, Pieces, NewPieces, 'H') :- write('Invalid Piece'), nl, choose_move_piece(Board, Player, NewBoard, Pieces, NewPieces).


choose_move_piece(Board, _, NewBoard, Pieces, NewPieces, 'PC'-1):- 
    setof(Piece, existPiece(Pieces, Piece), PiecesList),
    random_member(Piece, PiecesList),
    valid_moves_piece(Board, Moves),
    random_member(Line-Column, Moves),
    setPiece(Board, NewBoard, Line, Column, Piece),
    usePiece(Pieces, Piece, NewPieces),
    letter(Line, CharLine),
    symbol(Piece, Symbol, _),
    nl,write('Add piece '), write(Symbol) , write(' to ') , write(CharLine-Column), nl,
    sleep(1).

choose_move_piece(Board, _, NewBoard, Pieces, NewPieces, 'PC'-2):- 
    setof(Piece, existPiece(Pieces, Piece), PiecesList),
    random_member(Piece, PiecesList),
    valid_moves_piece(Board, Moves),
    random_member(Line-Column, Moves),
    setPiece(Board, NewBoard, Line, Column, Piece),
    usePiece(Pieces, Piece, NewPieces),
    letter(Line, CharLine),
    symbol(Piece, Symbol, _),
    nl,write('Add piece '), write(Symbol) , write(' to ') , write(CharLine-Column), nl,
    sleep(1).
    

/**
 * movePiece(+Board, -NewBoard, +Player)
 *      @param Board - current board
 *      @param NewBoard - new board with the token moved
 *      @param Player - current player
 *      
 *      This predicate is used to move the token.
 *      It shows the possible moves and asks the player to choose one.
 *       If the move is not valid, it asks again.
 *      Then it moves the token to the chosen position, and stores the play in NewBoard.
 *
 * */
movePiece(Board, NewBoard, Moves, Piece) :-
                                    write('Choose the piece position (Ex: d-3): '),
                                    getPosition(Line-Column),
                                    select_move(Line-Column, Moves),
                                    setPiece(Board, NewBoard, Line, Column, Piece).
movePiece(Board, NewBoard, Moves, Piece) :- write('Invalid Position'), nl, movePiece(Board, NewBoard, Moves, Piece). 


/**
 * select_move(+Line-Column, +Moves)
 *      @param Line-Column - position to check
 *      @param Moves - list of valid moves
 *      
 *      This predicate is used to check if a move is valid.
 *      It returns true if the position is empty and it is around the token, false otherwise.
 *
 * */
check_move_piece(Board, Line-Column) :- getPiece(Board, Line-Column, Piece), 
                                                Piece == empty,
                                                getPiece(Board, TLine-TColumn, token),  DiffLine is abs(TLine - Line), DiffCol is abs(TColumn - Column),                                       
                                                ((DiffLine==0 , DiffCol==1) ; (DiffLine==1, DiffCol==0) ; (DiffLine==1, DiffCol==1)). 


/**
 * valid_moves_piece(+Board, -Moves)
 *      @param Board - current board
 *      @param Moves - list of valid moves
 *      
 *      This predicate is used to get the valid moves of a piece.
 *      It returns a list of empty cells around the token.
 *      If there are no valid moves, it returns a list of all empty positions.
 *
 * */

valid_moves_piece(Board, Moves):-
    setof(Line-Column, check_move_piece(Board, Line-Column), Moves).
valid_moves_piece(Board, Moves):-
    setof(Line-Column, getPiece(Board, Line-Column, empty), Moves).


