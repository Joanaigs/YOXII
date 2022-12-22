existPiece([Piece-Number | _], Piece) :- Number > 0.
existPiece([_Symbol-_ | List], Piece) :- existPiece(List, Piece).

usePiece([Piece-Number | List], Piece, [Piece-N | List])  :- N is Number-1.
usePiece([Symbol-Number| List], Piece, [Symbol-Number| NewList]) :- usePiece(List, Piece, NewList).

readPiece(Pieces, Piece, Player) :- get_char(Symbol), skip_line,
                                    symbol(Piece, Symbol, Player), existPiece(Pieces, Piece).    

choosePiece(Board, Player, NewBoard, Pieces, NewPieces):- nl,
                                write('Choose Piece to add to the board: '), 
                                readPiece(Pieces, Piece, Player),
                                write('Possible moves'), nl,
                                valid_moves_piece(Board, Moves), change_number_letter(Moves, Result), 
                                write(Result),
                                nl,
                                movePiece(Board, NewBoard, Moves, Piece),
                                usePiece(Pieces, Piece, NewPieces).
choosePiece(Board, Player, NewBoard, Pieces, NewPieces) :- write('Invalid Piece'), nl, choosePiece(Board, Player, NewBoard, Pieces, NewPieces).

movePiece(Board, NewBoard, Moves, Piece) :-
                                    write('Choose the piece position (Ex: d-3): '),
                                    getPosition(Line-Column),
                                    select_move(Line-Column, Moves),
                                    setPiece(Board, NewBoard, Line, Column, Piece).
movePiece(Board, NewBoard, Moves, Piece) :- write('Invalid Position'), nl, movePiece(Board, NewBoard, Moves, Piece). 



check_move_piece(Board, Line-Column) :- getPiece(Board, Line-Column, Piece), 
                                                Piece == empty,
                                                getPiece(Board, TLine-TColumn, token),  DiffLine is abs(TLine - Line), DiffCol is abs(TColumn - Column),                                       
                                                ((DiffLine==0 , DiffCol==1) ; (DiffLine==1, DiffCol==0) ; (DiffLine==1, DiffCol==1)). 


valid_moves_piece(Board, Moves):-
    setof(Line-Column, check_move_piece(Board, Line-Column), Moves).
% se o setof retornar false, entao mostra todas as posicoes vazias
valid_moves_piece(Board, Moves):-
    setof(Line-Column, getPiece(Board, Line-Column, empty), Moves).


