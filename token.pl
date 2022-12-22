choose_move_token(Board, Player, NewBoard):-
    moveToken(Board, NewBoard, Player).  
choose_move_token(Board, Player, NewBoard):- write('wrong Input'), nl, choose_move_token(Board, Player, NewBoard).    


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


get_direction(Line-Column, TokenL-TokenC, Direction) :- 
    DiffLine is (TokenL - Line), DiffCol is (TokenC - Column),
    
    ((DiffCol==0, DiffLine>0, Direction=down);(DiffCol==0, DiffLine<0, Direction=up);
    (DiffCol>0, DiffLine==0, Direction=right);(DiffCol<0, DiffLine==0, Direction=left);
    (DiffCol<0, DiffLine<0,DiffLine==DiffCol, Direction=upLeft);
    (DiffCol<0, DiffLine>0,AbsDiffLine is abs(DiffLine),  AbsDiffCol is abs(DiffCol), AbsDiffLine==AbsDiffCol, Direction=downLeft);
    (DiffCol>0, DiffLine<0,AbsDiffLine is abs(DiffLine),  AbsDiffCol is abs(DiffCol), AbsDiffLine==AbsDiffCol, Direction=upRight);
    (DiffCol>0, DiffLine>0,DiffLine==DiffCol, Direction=downRight)).



gotoToken(_Board, TokenL-TokenC, _Direction, _Player, TokenL-TokenC).
gotoToken(Board, Line-Column, Direction, Player, TokenL-TokenC) :- getPiece(Board, Line-Column, Piece),
                                    Piece \== empty,
                                    symbol(Piece, _, Player),
                                    previous_cell(Line-Column, Direction,  PLine-PColumn),
                                    gotoToken(Board, PLine-PColumn, Direction, Player, TokenL-TokenC).
                                        

check_move(Board, Player, Line-Column) :-
                   getPiece(Board, Line-Column, Piece), 
                   Piece == empty, 
                   getPiece(Board, TokenL-TokenC, token), 
                   get_direction(Line-Column, TokenL-TokenC, Direction), 
                   previous_cell(Line-Column, Direction,  PLine-PColumn),
                   gotoToken(Board, PLine-PColumn, Direction, Player, TokenL-TokenC).

valid_moves_token(Board, Player,Moves):-
    setof(Line-Column, check_move(Board, Player, Line-Column), Moves).


move(Board, NewBoard, OldLine-OldColumn, NewLine-NewColumn) :- 
                    getPiece(Board, OldLine-OldColumn, Piece),
                    setPiece(Board, TmpBoard, NewLine, NewColumn, Piece),
                    setPiece(TmpBoard, NewBoard, OldLine, OldColumn, empty).