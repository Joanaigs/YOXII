:- use_module(library(lists)).

convertToInt(StringNumber, Number) :-atom_chars(StringNumber, Z), number_chars(Number, Z).

startGame(_player, _player2) :-
    initialBoard(Board),
    initialPiecesWhite(WhitePieces),
    initialPiecesBlack(BlackPieces),
    drawGame(Board,WhitePieces, BlackPieces),

    game_cycle(Board, red, WhitePieces, BlackPieces).
    
/*
game_cycle(GameState-Player):-
                    game_over(GameState, Winner), !,
                    congratulate(Winner).*/
next_player(red, black).
next_player(black, red).

game_cycle(Board, Player, WhitePieces, BlackPieces):-
                    choose_move(Board, Player, NewBoard ),
                    next_player(Player, NextPlayer),
                    drawGame(NewBoard,WhitePieces, BlackPieces), !,
                    game_cycle(NewBoard, NextPlayer, WhitePieces, BlackPieces).

% The user choose the next move   
change_number_letter([], []).
change_number_letter([X-Y|L1], [X1-Y|L2]) :- letter(X, Z), X1 = Z, change_number_letter(L1, L2).

choose_move(Board, Player, NewBoard):-
                    moveToken(Board, NewBoard, Player).  
choose_move(Board, Player, NewBoard):- write('wrong Input'), nl, choose_move(Board, Player, NewBoard).          

select_move(_, []) :- fail.              
select_move(Line-Column, [Line-Column|_]) .
select_move(Line-Column, [_|Moves]) :- select_move(Line-Column, Moves).
                    
moveToken(Board, NewBoard, Player) :- nl,
                            write('Possible moves'), 
                            valid_moves(Board, Player, Moves), change_number_letter(Moves, Result), 
                            write(Result),
                            nl,
                            write('Choose the token position (Ex: d-3)'),
                            getPosition(Line-Column),!,
                            select_move(Line-Column, Moves),
                            
                            getPiece(Board, TokenL-TokenC, token),
                            move(Board, NewBoard, TokenL-TokenC, Line-Column).


getPosition(Line-Column) :-  read(CharLine-Column), letter(Line, CharLine).

replace([_|L], 1, New, [New|L]).
replace([X|List1], Index, New, [X|List2]) :-  Index>1,
                                            N1 is Index-1,
                                            replace(List1,N1, New, List2).

setPiece([Line|Board], [NewLine|Board], 1, Column, Piece) :- replace(Line, Column, Piece, Result), NewLine=Result.
setPiece([Line|Board], [Line|NewBoard], IndexLine, Column, Piece) :- Temp is IndexLine-1, setPiece(Board, NewBoard, Temp, Column, Piece).

move(Board, NewBoard, OldLine-OldColumn, NewLine-NewColumn) :- 
        getPiece(Board, OldLine-OldColumn, Piece),
        setPiece(Board, TmpBoard, NewLine, NewColumn, Piece),
        setPiece(TmpBoard, NewBoard, OldLine, OldColumn, empty).

getPiece(Board, Line-Column, Piece) :-
    nth1(Line, Board, BoardLine),
    nth1(Column, BoardLine, Piece).


get_direction(Line-Column, TokenL-TokenC, Direction) :- 
                                                     DiffLine is (TokenL - Line), DiffCol is (TokenC - Column),
                                                    
                                                    ((DiffCol==0, DiffLine>0, Direction=down);(DiffCol==0, DiffLine<0, Direction=up);
                                                     (DiffCol>0, DiffLine==0, Direction=right);(DiffCol<0, DiffLine==0, Direction=left);
                                                     (DiffCol<0, DiffLine<0,DiffLine==DiffCol, Direction=upLeft);
                                                     (DiffCol<0, DiffLine>0,AbsDiffLine is abs(DiffLine),  AbsDiffCol is abs(DiffCol), AbsDiffLine==AbsDiffCol, Direction=downLeft);
                                                     (DiffCol>0, DiffLine<0,AbsDiffLine is abs(DiffLine),  AbsDiffCol is abs(DiffCol), AbsDiffLine==AbsDiffCol, Direction=upRight);
                                                     (DiffCol>0, DiffLine>0,DiffLine==DiffCol, Direction=downRight)).

previous_cell(Line-Column, up, PLine-PColumn) :- PLine is Line-1, PColumn is Column.
previous_cell(Line-Column, down, PLine-PColumn) :- PLine is Line+1, PColumn is Column.
previous_cell(Line-Column, left, PLine-PColumn) :- PLine is Line, PColumn is Column-1.
previous_cell(Line-Column, right, PLine-PColumn) :- PLine is Line, PColumn is Column+1.
previous_cell(Line-Column, upLeft, PLine-PColumn) :- PLine is Line-1, PColumn is Column-1.
previous_cell(Line-Column, upRight, PLine-PColumn) :- PLine is Line-1, PColumn is Column+1.
previous_cell(Line-Column, downLeft, PLine-PColumn) :- PLine is Line+1, PColumn is Column-1.
previous_cell(Line-Column, downRight, PLine-PColumn) :- PLine is Line+1, PColumn is Column+1.

gotoToken(_Board, TokenL-TokenC, _Direction, _Player, TokenL-TokenC).
gotoToken(Board, Line-Column, Direction, Player, TokenL-TokenC) :- getPiece(Board, Line-Column, Piece), Piece == Player,
                                                        previousCell(Line-Column, Direction,  PLine-PColumn),
                                                        gotoToken(Board, PLine-PColumn, Direction, Player, TokenL-TokenC).
                                                             

check_move(Board, Player, Line-Column) :-
                                        getPiece(Board, Line-Column, Piece), 
                                        Piece == empty, 
                                        getPiece(Board, TokenL-TokenC, token), 
                                        get_direction(Line-Column, TokenL-TokenC, Direction), 
                                        previous_cell(Line-Column, Direction,  PLine-PColumn),
                                        gotoToken(Board, PLine-PColumn, Direction, Player, TokenL-TokenC).




valid_moves(Board, Player,Moves):-
    findall(Line-Column, check_move(Board, Player, Line-Column), Moves).


 /*   
    % interaction to select move
choose_move(GameState, computer-Level, Move):-
    valid_moves(GameState, Moves),
    choose_move(Level, GameState, Moves, Move).

valid_moves(GameState, Moves):-
    findall(Move, move(GameState, Move, NewState), Moves).



choose_move(1, _GameState, Moves, Move):-
    random_select(Move, Moves, _Rest).
choose_move(2, GameState, Moves, Move):-
    setof(Value-Mv, NewState^( member(Mv, Moves),
    move(GameState, Mv, NewState),
    evaluate_board(NewState, Value) ), [_V-Move|_]).*/