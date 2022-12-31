:- use_module(library(lists)).


convertToInt(StringNumber, Number) :-atom_chars(StringNumber, Z), number_chars(Number, Z).


/**
 * startGame(+ThisTurn, +NextTurn)
 *      @param ThisTurn
 *      @param NextTurn
 * 
 *      Prints the board and the players remaning pieces and starts the game
 */
startGame(ThisTurn, NextTurn) :-
    initialBoard(Board),
    initialPiecesRed(RedPieces),
    initialPiecesWhite(WhitePieces),
    game_cycle(ThisTurn, NextTurn, Board, white, RedPieces, WhitePieces).
    

/**
 * next_player(+Player1, +Player2)
 *      @param Player1
 *      @param Player2
 * 
 *      Predicate used to change the player playing 
 */
next_player(red, white).
next_player(white, red).


/**
 * get_players_pieces(+Player, +RedPieces, +WhitePieces, -PlayerPieces)
 *      @param Player
 *      @param RedPieces
 *      @param WhitePieces
 *      @param PlayerPieces
 * 
 *      Returns the pieces of the player
 */
get_players_pieces(red, RedPieces, _WhitePieces, RedPieces).
get_players_pieces(white, _RedPieces, WhitePieces, WhitePieces).

/**
 * update_players_pieces(+Player, +OldRedPieces, +OldWhitePieces, +Pieces, -NewRedPieces, -NewWhitePieces)
 *      @param Player
 *      @param OldRedPieces
 *      @param OldWhitePieces
 *      @param Pieces
 *      @param NewRedPieces
 *      @param NewWhitePieces
 * 
 *      Updates the pieces of the player after a move
 */
update_players_pieces(red, _, OldWhitePieces, Pieces,  Pieces, OldWhitePieces).
update_players_pieces(white, OldRedPieces, _, Pieces,  OldRedPieces, Pieces).


/**
 * game_over(+Board, +Player)
 *      @param Board
 *      @param Player
 * 
 *      Checks if the game is over by seing if the player has any moves left
 */
game_over(Board, Player) :- 
    \+ valid_moves_token(Board, Player, _Moves).


/**
 * getPoints(+Board, +TokenPosition, +Direction, +Player, +Points, -NewPoints)
 *      @param Board
 *      @param TokenPosition (TokenLine-TokenCol)
 *      @param Direction
 *      @param Player
 *      @param Points
 *      @param NewPoints
 * 
 * Calculates the points of the player in a certain direction of the token and returns them in NewPoints
 */
getPoints(Board, TLine-TCol, Direction, Player, Points, NewPoints) :- 
                                                        previous_cell(TLine-TCol, Direction, Line-Col),
                                                        getPiece(Board, Line-Col, Piece),
                                                        symbol(Piece, _, Player), 
                                                        points(Piece, PiecePoints), 
                                                        NewPoints is Points + PiecePoints.
getPoints(_, _, _, _, Points, Points).



/** 
 * countPoints(+Board, +Player, -Points)
 *      @param Board
 *      @param Player
 *      @param Points
 * 
 *      Calculates the points of the player by summing the values of their pieces that suround the token 
 */
countPoints(Board, Player, Points, Pieces) :-
        getPiece(Board, TLine-TCol, token),
        getPoints(Board, TLine-TCol, up,        Player, 0      , Points1), (Points1 > 0 -> Pieces1 = Pieces + 1; Pieces1 = 0), 
        getPoints(Board, TLine-TCol, down,      Player, Points1, Points2), (Points1 == Points2 -> Pieces2 = Pieces1 + 1; Pieces2 = Pieces1), 
        getPoints(Board, TLine-TCol, right,     Player, Points2, Points3), (Points2 == Points3 -> Pieces3 = Pieces2 + 1; Pieces3 = Pieces2), 
        getPoints(Board, TLine-TCol, left,      Player, Points3, Points4), (Points3 == Points4 -> Pieces4 = Pieces3 + 1; Pieces4 = Pieces3),  
        getPoints(Board, TLine-TCol, upLeft,    Player, Points4, Points5), (Points4 == Points5 -> Pieces5 = Pieces4 + 1; Pieces5 = Pieces4),  
        getPoints(Board, TLine-TCol, upRight,   Player, Points5, Points6), (Points5 == Points6 -> Pieces6 = Pieces5 + 1; Pieces6 = Pieces5),  
        getPoints(Board, TLine-TCol, downLeft,  Player, Points6, Points7), (Points6 == Points7 -> Pieces7 = Pieces6 + 1; Pieces7 = Pieces6),  
        getPoints(Board, TLine-TCol, downRight, Player, Points7, Points),  (Points7 == Points ->  Pieces = Pieces7 + 1; Pieces = Pieces7).


/** 
 * congratulations(+RedPoints, +WhitePoints)
 *      @param RedPoints
 *      @param WhitePoints
 * 
 *      Checks witch player was the winner
 */
congratulations(RedPoints, WhitePoints, _NumPiecesRed, _NumPiecesWhite) :- RedPoints > WhitePoints, !, write('Congratulations red you won!'), nl.
congratulations(RedPoints, WhitePoints,  _NumPiecesRed, _NumPiecesWhite) :- WhitePoints > RedPoints, !, write('Congratulations white you won!'), nl.
congratulations(_RedPoints, _WhitePoints,  NumPiecesRed, NumPiecesWhite) :- NumPiecesRed > NumPiecesWhite, !, write('Congratulations red you won! You had more Pieces surrounding the token'), nl.
congratulations(_RedPoints, _WhitePoints,  NumPiecesRed, NumPiecesWhite) :- NumPiecesRed < NumPiecesWhite, !, write('Congratulations white you won! You had more Pieces surrounding the token'), nl.
congratulations(_RedPoints, _WhitePoints,  _NumPiecesRed, _NumPiecesWhite) :- write('You tied'), nl.

/**
 * game_cycle(+ThisTurn, +NextTurn, +Board, +Player, +RedPieces, WhitePieces)
 *      @param ThisTurn
 *      @param NextTurn
 *      @param Board
 *      @param Player
 *      @param RedPieces
 *      @param WhitePieces
 * 
 *      The cycle of the game, where we start by seeing if the game has enough conditions to finish, and if not then the current player chooses their moves and give turn to the other player to do the same.
 */
game_cycle(_, _, Board, Player, RedPieces, WhitePieces):- 
                    draw_game(Board, Player, RedPieces, WhitePieces),
                    game_over(Board, Player), !, nl,
                    write('Game Over!'),nl,
                    countPoints(Board, red, RedPoints, NumRedPieces),
                    write('Red: '), write(RedPoints),nl,
                    countPoints(Board, white, WhitePoints, NumWhitePieces ),
                    write('White: '), write(WhitePoints),nl,nl,
                    congratulations(RedPoints, WhitePoints, NumRedPieces,  NumWhitePieces).

game_cycle(ThisTurn, NextTurn ,Board, Player, RedPieces, WhitePieces):-
                    choose_move(Board, Player, ThisTurn, RedPieces, WhitePieces, NewBoard, NewPieces),
                    % update players pieces
                    update_players_pieces(Player, RedPieces, WhitePieces, NewPieces, NewRedPieces, NewWhitePieces),
                    next_player(Player, NextPlayer),
                    nl,nl,
                    game_cycle(NextTurn, ThisTurn, NewBoard, NextPlayer, NewRedPieces, NewWhitePieces).



/** 
 * change_number_letter(+ValidMoves, -ValidMovesBoard)
 *      @param ValidMoves [(Line-Column))]
 *      @param ValidMovesBoard [(Line-Column))]
 * 
 *      Receives a list of Moves of type Number-Number and changes it to an array of Moves of type Char-Number, for example '1-1' turns to 'a-1'
 */
change_number_letter([], []).
change_number_letter([X-Y|L1], [X1-Y|L2]) :- letter(X, Z), X1 = Z, change_number_letter(L1, L2).

/** 
 * select_move(+Move, +ValidMoves)
 *      @param Move (Line-Column)
 *      @param ValidMoves [(Line-Column))]
 * 
 *      Checks if the Move received is in the List of valid moves
 */
select_move(_, []) :- fail.              
select_move(Line-Column, [Line-Column|_]) .
select_move(Line-Column, [_|Moves]) :- select_move(Line-Column, Moves).


/** 
 * getPosition(-Move)
 *      @param Move (line-column)
 * 
 *      Reads the move from terminal of type 'char-number' and turns it into number-number, for example 'a-1' becomes 1-1
 */              
getPosition(Line-Column) :- get_char(CharLine), get_char(Sep), get_char(CharCol), skip_line,
                            Sep == '-',
                            letter(Line, CharLine),
                            convertToInt(CharCol, Column).


/** 
 * replace(+List1, +Index, +New, -List2)
 *      @param List1
 *      @param Index
 *      @param New
 *      @param List2
 * 
 *      Replaces a element of a list with the new one at the given position
 */
replace([_|L], 1, New, [New|L]).
replace([X|List1], Index, New, [X|List2]) :-  Index>1,
                                            N1 is Index-1,
                                            replace(List1,N1, New, List2).


/** 
 * setPiece(+Board, -NewBoard, +Line, +Column, +NewElem)
 *      @param Board
 *      @param NewBoard
 *      @param Line
 *      @param Column
 *      @param NewElem
 * 
 *      Selects a line of the board where a element of a certain column is changed with the help of the predicate replace
 */
setPiece([Line|Board], [NewLine|Board], 1, Column, Piece) :- replace(Line, Column, Piece, Result), NewLine=Result.
setPiece([Line|Board], [Line|NewBoard], IndexLine, Column, Piece) :- Temp is IndexLine-1, setPiece(Board, NewBoard, Temp, Column, Piece).


/** 
 * getPiece(+Board, ?Position, ?Piece)
 *      @param Board
 *      @param Position (line-column)
 *      @param Piece
 * 
 *      Returns the element in the position given of the board or returns the position of an element on the board
 */
getPiece(Board, Line-Column, Piece) :-
    nth1(Line, Board, BoardLine),
    nth1(Column, BoardLine, Piece).

/**
 * previous_cell(+PositionElement, +Direction, -PositionDiretion)
 *      @param PositionElement (line-column)
 *      @param Direction
 *      @param PositionDiretion (line-column)
 * 
 *      Returns the position of the element next to them in that direction
 */
previous_cell(Line-Column, up, PLine-PColumn) :- PLine is Line-1, PColumn is Column.
previous_cell(Line-Column, down, PLine-PColumn) :- PLine is Line+1, PColumn is Column.
previous_cell(Line-Column, left, PLine-PColumn) :- PLine is Line, PColumn is Column-1.
previous_cell(Line-Column, right, PLine-PColumn) :- PLine is Line, PColumn is Column+1.
previous_cell(Line-Column, upLeft, PLine-PColumn) :- PLine is Line-1, PColumn is Column-1.
previous_cell(Line-Column, upRight, PLine-PColumn) :- PLine is Line-1, PColumn is Column+1.
previous_cell(Line-Column, downLeft, PLine-PColumn) :- PLine is Line+1, PColumn is Column-1.
previous_cell(Line-Column, downRight, PLine-PColumn) :- PLine is Line+1, PColumn is Column+1.

