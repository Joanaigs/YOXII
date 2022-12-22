:- use_module(library(lists)).

convertToInt(StringNumber, Number) :-atom_chars(StringNumber, Z), number_chars(Number, Z).

startGame(_player, _player2) :-
    initialBoard(Board),
    initialPiecesRed(RedPieces),
    initialPiecesBlack(BlackPieces),
    game_cycle(Board, red, RedPieces, BlackPieces).
    
next_player(red, black).
next_player(black, red).


getPlayersPieces(red, RedPieces, _BlackPieces, RedPieces).
getPlayersPieces(black, _RedPieces, BlackPieces, BlackPieces).

% updatePlayersPieces(red, OldRedPieces, OldBlackPieces, Pieces,  NewRedPieces, NewBlackPieces).
updatePlayersPieces(red, _, OldBlackPieces, Pieces,  Pieces, OldBlackPieces).
updatePlayersPieces(black, OldRedPieces, _, Pieces,  OldRedPieces, Pieces).

game_over(Board, Player) :- 
    \+ valid_moves_token(Board, Player, _Moves).

getPoints(Board, TLine-TCol, Direction, Player, Points, NewPoints) :- 
                                                        previous_cell(TLine-TCol, Direction, Line-Col),
                                                        getPiece(Board, Line-Col, Piece),
                                                        symbol(Piece, _, Player),
                                                        points(Piece, PiecePoints),
                                                        NewPoints is Points + PiecePoints.
getPoints(_, _, _, _, Points, Points).

countPoints(Board, Player, Points) :-
        getPiece(Board, TLine-TCol, token),
        getPoints(Board, TLine-TCol, up,        Player, 0      , Points1),
        getPoints(Board, TLine-TCol, down,      Player, Points1, Points2),
        getPoints(Board, TLine-TCol, right,     Player, Points2, Points3),
        getPoints(Board, TLine-TCol, left,      Player, Points3, Points4),
        getPoints(Board, TLine-TCol, upLeft,    Player, Points4, Points5),
        getPoints(Board, TLine-TCol, upRight,   Player, Points5, Points6),
        getPoints(Board, TLine-TCol, downLeft,  Player, Points6, Points7),
        getPoints(Board, TLine-TCol, downRight, Player, Points7, Points).

congratulations(Player, RedPoints, BlackPoints) :- RedPoints > BlackPoints, !, write('Congratulations '), write(Player), write(' you won!'), nl.
congratulations(Player, RedPoints, BlackPoints) :- BlackPoints > RedPoints, !, write('Congratulations '), write(Player), write(' you won!'), nl.
congratulations(Player, RedPoints, BlackPoints) :- write('You tied'), nl.


game_cycle(Board, Player, _, _):- 
                    game_over(Board, Player), !,
                    write('Game Over!'),nl,
                    countPoints(Board, red, RedPoints),
                    write('Red: '), write(RedPoints),nl,
                    countPoints(Board, black, BlackPoints),
                    write('Black: '), write(BlackPoints),nl
                    .

game_cycle(Board, Player, RedPieces, BlackPieces):-
                    drawGame(Board, Player, RedPieces, BlackPieces),
                    choose_move_token(Board, Player, TempBoard ),
                    drawGame(TempBoard, Player, RedPieces, BlackPieces),
                    getPlayersPieces(Player, RedPieces, BlackPieces, Pieces),
                    choosePiece(TempBoard, Player, NewBoard,Pieces, NewPieces),
                    updatePlayersPieces(Player, RedPieces, BlackPieces, NewPieces, NewRedPieces, NewBlackPieces),
                    next_player(Player, NextPlayer),
                    game_cycle(NewBoard, NextPlayer, NewRedPieces, NewBlackPieces).

% The user choose the next move   
change_number_letter([], []).
change_number_letter([X-Y|L1], [X1-Y|L2]) :- letter(X, Z), X1 = Z, change_number_letter(L1, L2).

select_move(_, []) :- fail.              
select_move(Line-Column, [Line-Column|_]) .
select_move(Line-Column, [_|Moves]) :- select_move(Line-Column, Moves).
                    
getPosition(Line-Column) :- get_char(CharLine), get_char(Sep), get_char(CharCol), skip_line,
                            Sep == '-',
                            letter(Line, CharLine),
                            convertToInt(CharCol, Column).

replace([_|L], 1, New, [New|L]).
replace([X|List1], Index, New, [X|List2]) :-  Index>1,
                                            N1 is Index-1,
                                            replace(List1,N1, New, List2).

setPiece([Line|Board], [NewLine|Board], 1, Column, Piece) :- replace(Line, Column, Piece, Result), NewLine=Result.
setPiece([Line|Board], [Line|NewBoard], IndexLine, Column, Piece) :- Temp is IndexLine-1, setPiece(Board, NewBoard, Temp, Column, Piece).

getPiece(Board, Line-Column, Piece) :-
    nth1(Line, Board, BoardLine),
    nth1(Column, BoardLine, Piece).


previous_cell(Line-Column, up, PLine-PColumn) :- PLine is Line-1, PColumn is Column.
previous_cell(Line-Column, down, PLine-PColumn) :- PLine is Line+1, PColumn is Column.
previous_cell(Line-Column, left, PLine-PColumn) :- PLine is Line, PColumn is Column-1.
previous_cell(Line-Column, right, PLine-PColumn) :- PLine is Line, PColumn is Column+1.
previous_cell(Line-Column, upLeft, PLine-PColumn) :- PLine is Line-1, PColumn is Column-1.
previous_cell(Line-Column, upRight, PLine-PColumn) :- PLine is Line-1, PColumn is Column+1.
previous_cell(Line-Column, downLeft, PLine-PColumn) :- PLine is Line+1, PColumn is Column-1.
previous_cell(Line-Column, downRight, PLine-PColumn) :- PLine is Line+1, PColumn is Column+1.


 /*   
    % interaction to select move
choose_move_token(GameState, computer-Level, Move):-
    valid_moves_token(GameState, Moves),
    choose_move_token(Level, GameState, Moves, Move).

valid_moves_token(GameState, Moves):-
    findall(Move, move(GameState, Move, NewState), Moves).



choose_move_token(1, _GameState, Moves, Move):-
    random_select(Move, Moves, _Rest).
choose_move_token(2, GameState, Moves, Move):-
    setof(Value-Mv, NewState^( member(Mv, Moves),
    move(GameState, Mv, NewState),
    evaluate_board(NewState, Value) ), [_V-Move|_]).*/