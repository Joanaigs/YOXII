:- use_module(library(random)).
:- use_module(library(system)).

/**
 *choose_move_token(+Board, +Player, -NewBoard, +Mode)
 *      @param Board - current board
 *      @param Player - current player
 *      @param NewBoard - new board with the token moved
 *      @param Mode - 'H' for human, 'PC' for computer
 *  
 *      This predicate is used to choose the move of the token.
 *          If the mode is 'H' the player chooses the move (from input), 
 *          if the mode is 'PC' the computer chooses the move using diferent algoritms .
 *              level 1 - random move
 *              level 2 - greedy algorithm    
 *  
*/

choose_move_token(Board, Player, NewBoard, 'H'):-
    moveToken(Board, NewBoard, Player).  
choose_move_token(Board, Player, NewBoard, 'H'):- write('wrong Input'), nl, choose_move_token(Board, Player, NewBoard).    

choose_move_token(Board, Player, NewBoard, 'PC'-1):-
    valid_moves_token(Board, Player, Moves),
    random_member(Line-Column, Moves),
    getPiece(Board, TokenL-TokenC, token),
    move(Board, NewBoard, TokenL-TokenC, Line-Column),
    letter(Line,CharLine),
    nl, write('Move token to ') , write(CharLine-Column), nl.
    sleep(1).

choose_move_token(_Board, _Player, _NewBoard, 'PC'-2).

    
/**
 * moveToken(+Board, -NewBoard, +Player)
 *      @param Board - current board
 *      @param NewBoard - new board with the token moved
 *      @param Player - current player
 *      
 *      This predicate is used to move the token.
 *      It shows the possible moves and asks the player to choose one.
 *      If the move is not valid, it asks again.
 *      Then it moves the token to the chosen position, and stores the play in NewBoard.
 *
 * */

moveToken(Board, NewBoard, Player) :- nl,
                            write('Possible moves'), 
                            valid_moves_token(Board, Player, Moves), change_number_letter(Moves, Result), 
                            write(Result),
                            nl,
                            write('Choose the token position (Ex: d-3): '),
                            getPosition(Line-Column),
                            select_move(Line-Column, Moves),
                            
                            getPiece(Board, TokenL-TokenC, token),
                            move(Board, NewBoard, TokenL-TokenC, Line-Column).

/**
 *  get_direction(+Line-Column, +TokenL-TokenC, -Direction)
 *      @param Line-Column - position of the piece
 *      @param TokenL-TokenC - position of the token
 *      @param Direction - direction of the piece
 * 
 *      This predicate is used to get the direction of the piece in relation to the token.
 * 
 * */
get_direction(Line-Column, TokenL-TokenC, Direction) :- 
    DiffLine is (TokenL - Line), DiffCol is (TokenC - Column),
    
    ((DiffCol==0, DiffLine>0, Direction=down);(DiffCol==0, DiffLine<0, Direction=up);
    (DiffCol>0, DiffLine==0, Direction=right);(DiffCol<0, DiffLine==0, Direction=left);
    (DiffCol<0, DiffLine<0,DiffLine==DiffCol, Direction=upLeft);
    (DiffCol<0, DiffLine>0,AbsDiffLine is abs(DiffLine),  AbsDiffCol is abs(DiffCol), AbsDiffLine==AbsDiffCol, Direction=downLeft);
    (DiffCol>0, DiffLine<0,AbsDiffLine is abs(DiffLine),  AbsDiffCol is abs(DiffCol), AbsDiffLine==AbsDiffCol, Direction=upRight);
    (DiffCol>0, DiffLine>0,DiffLine==DiffCol, Direction=downRight)).


/**
 * gotoToken(+Board, +Line-Column, +Direction, +Player, +TokenL-TokenC)
 *      @param Board - current board
 *      @param Line-Column - position
 *      @param Direction - direction of the move
 *      @param Player - current player
 *      @param TokenL-TokenC - position of the token
 *      
 *      This predicate returns if it has a straight line path (in the defined direction)
 *          between the token and the position, can only pass through its own pieces. 
*/
gotoToken(_Board, TokenL-TokenC, _Direction, _Player, TokenL-TokenC).
gotoToken(Board, Line-Column, Direction, Player, TokenL-TokenC) :- getPiece(Board, Line-Column, Piece),
                                    Piece \== empty,
                                    symbol(Piece, _, Player),
                                    previous_cell(Line-Column, Direction,  PLine-PColumn),
                                    gotoToken(Board, PLine-PColumn, Direction, Player, TokenL-TokenC).
                                        
/**
 * check_move(+Board, +Player, ?Line-Column)
 *      @param Board - current board    
 *      @param Player - current player
 *      @param Line-Column - position of the piece
 * 
 *      This predicate returns if the piece in the position Line-Column is a valid move, or returns the possible moves.
 *      First it checks if the position is empty, then if it has a straight line path between the token and the position.
 * */

check_move(Board, Player, Line-Column) :-
                   getPiece(Board, Line-Column, Piece), 
                   Piece == empty, 
                   getPiece(Board, TokenL-TokenC, token), 
                   get_direction(Line-Column, TokenL-TokenC, Direction), 
                   previous_cell(Line-Column, Direction,  PLine-PColumn),
                   gotoToken(Board, PLine-PColumn, Direction, Player, TokenL-TokenC).

/**
 * valid_moves_token(+Board, +Player, -Moves)
 *      @param Board - current board
 *      @param Player - current player
 *      @param Moves - list of possible moves
 *      
 *      Returns the list of all possible token moves for a given player.
 * */
valid_moves_token(Board, Player,Moves):-
    setof(Line-Column, check_move(Board, Player, Line-Column), Moves).

/**
 * move(+Board, -NewBoard, +OldLine-OldColumn, +NewLine-NewColumn)
 *      @param Board - current board
 *      @param NewBoard - new board with the piece moved
 *      @param OldLine-OldColumn - position of the piece
 *      @param NewLine-NewColumn - position of the piece after the move
 *  
 *      This predicate is used to move a piece from OldLine-OldColumn to NewLine-NewColumn.
 * */
move(Board, NewBoard, OldLine-OldColumn, NewLine-NewColumn) :- 
                    getPiece(Board, OldLine-OldColumn, Piece),
                    setPiece(Board, TmpBoard, NewLine, NewColumn, Piece),
                    setPiece(TmpBoard, NewBoard, OldLine, OldColumn, empty).