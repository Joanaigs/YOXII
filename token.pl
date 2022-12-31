:- use_module(library(random)).

/**
 *choose_move_token(+GameState, +Player, -NewGameState, +Mode)
 *      @param GameState - current GameState
 *      @param Player - current player
 *      @param NewGameState - new GameState with the token moved
 *      @param Mode - 'H' for human, 'PC' for computer
 *  
 *      This predicate is used to choose the move of the token.
 *          If the mode is 'H' the player chooses the move (from input), 
 *          if the mode is 'PC' the computer chooses the move using diferent algoritms .
 *              level 1 - random move
 *              level 2 - greedy algorithm    
 *  
*/

choose_move_token(GameState, Player, NewGameState, 'H'):-
    moveToken(GameState, NewGameState, Player).  
choose_move_token(GameState, Player, NewGameState, 'H'):- write('wrong Input'), nl, choose_move_token(GameState, Player, NewGameState, 'H').    

choose_move_token(GameState, Player, NewGameState, 'PC'-1):-
    valid_moves_token(GameState, Player, Moves),
    random_member(Line-Column, Moves),
    getPiece(GameState, TokenL-TokenC, token),
    move(GameState, TokenL-TokenC, Line-Column, NewGameState),
    letter(Line,CharLine),
    nl, write('Move token to ') , write(CharLine-Column), nl.

choose_move_token(_GameState, _Player, _NewGameState, 'PC'-2).

    
/**
 * moveToken(+GameState, -NewGameState, +Player)
 *      @param GameState - current GameState
 *      @param NewGameState - new GameState with the token moved
 *      @param Player - current player
 *      
 *      This predicate is used to move the token.
 *      It shows the possible moves and asks the player to choose one.
 *      If the move is not valid, it asks again.
 *      Then it moves the token to the chosen position, and stores the play in NewGameState.
 *
 * */

moveToken(GameState, NewGameState, Player) :- nl,
                            write('Possible moves'), 
                            valid_moves_token(GameState, Player, Moves), change_number_letter(Moves, Result), 
                            write(Result),
                            nl,
                            write('Choose the token position (Ex: d-3): '),
                            getPosition(Line-Column),
                            select_move(Line-Column, Moves),
                            
                            getPiece(GameState, TokenL-TokenC, token),
                            move(GameState, TokenL-TokenC, Line-Column, NewGameState).

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
 * gotoToken(+GameState, +Line-Column, +Direction, +Player, +TokenL-TokenC)
 *      @param GameState - current GameState
 *      @param Line-Column - position
 *      @param Direction - direction of the move
 *      @param Player - current player
 *      @param TokenL-TokenC - position of the token
 *      
 *      This predicate returns if it has a straight line path (in the defined direction)
 *          between the token and the position, can only pass through its own pieces. 
*/
gotoToken(_GameState, TokenL-TokenC, _Direction, _Player, TokenL-TokenC).
gotoToken(GameState, Line-Column, Direction, Player, TokenL-TokenC) :- getPiece(GameState, Line-Column, Piece),
                                    Piece \== empty,
                                    symbol(Piece, _, Player),
                                    previous_cell(Line-Column, Direction,  PLine-PColumn),
                                    gotoToken(GameState, PLine-PColumn, Direction, Player, TokenL-TokenC).
                                        
/**
 * check_move(+GameState, +Player, ?Line-Column)
 *      @param GameState - current GameState    
 *      @param Player - current player
 *      @param Line-Column - position of the piece
 * 
 *      This predicate returns if the piece in the position Line-Column is a valid move, or returns the possible moves.
 *      First it checks if the position is empty, then if it has a straight line path between the token and the position.
 * */

check_move(GameState, Player, Line-Column) :-
                   getPiece(GameState, Line-Column, Piece), 
                   Piece == empty, 
                   getPiece(GameState, TokenL-TokenC, token), 
                   get_direction(Line-Column, TokenL-TokenC, Direction), 
                   previous_cell(Line-Column, Direction,  PLine-PColumn),
                   gotoToken(GameState, PLine-PColumn, Direction, Player, TokenL-TokenC).

/**
 * valid_moves_token(+GameState, +Player, -Moves)
 *      @param GameState - current GameState
 *      @param Player - current player
 *      @param Moves - list of possible moves
 *      
 *      Returns the list of all possible token moves for a given player.
 * */
valid_moves_token(GameState, Player,Moves):-
    setof(Line-Column, check_move(GameState, Player, Line-Column), Moves).

