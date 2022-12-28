:- consult('token.pl').
:- consult('piece.pl').
:- use_module(library(lists)).


escolher_posicao_peca(Board, Player,  RedPieces, WhitePieces, Pieces, Moves, Line-Column, Piece, Points) :-
    existPiece(Pieces, Piece),
    select_move(Line-Column, Moves),
    setPiece(Board, NewBoard, Line, Column, Piece), 
    countPoints(NewBoard, Player, Points).



escolher_peca(Board, Player,  RedPieces, WhitePieces, Line-Column, Piece, Points) :-
    valid_moves_piece(Board,Moves),
    get_players_pieces(Player, RedPieces, WhitePieces, Pieces),

    setof(Points-Line-Column-Piece, 
        NewBoard^escolher_posicao_peca(Board, Player, RedPieces, WhitePieces, Pieces, Moves, Line-Column, Piece, Points),
            Final),
    last(Final, Points-Line-Column-Piece).

escolher_token(Board, Player,  RedPieces, WhitePieces, TMove, Piece, PMove) :-
    valid_moves_token(Board, Player, Moves),
    getPiece(Board, TokenL-TokenC, token),
    setof(Points-(TLine-TColumn)-(PLine-PColumn)-Piece, 
        TempNewBoard^(
                select_move(TLine-TColumn, Moves),
                move(Board, TempNewBoard, TokenL-TokenC, TLine-TColumn),
                escolher_peca(TempNewBoard, Player,  RedPieces, WhitePieces, PLine-PColumn, Piece, Points)),
            Final),
    write(Final), nl,
    last(Final, Points-TMove-PMove-Piece).

choose_move(Board, Player, 'PC'-2, RedPieces, WhitePieces, NewBoard, NewPieces) :- !,
    escolher_token(Board, Player,  RedPieces, WhitePieces, (TLine-TColumn), Piece, (PLine-PColumn)),

    getPiece(Board, TokenL-TokenC, token),
    move(Board, NewBoard, TokenL-TokenC, TLine-TColumn),
    setPiece(NewBoard, NewBoard2, PLine, PColumn, Piece), nl,nl,
    printBoard(Board), nl,
    printBoard(NewBoard2).

choose_move(Board, Player, ThisTurn, RedPieces, WhitePieces, NewBoard, NewPieces) :-
    % choose token move
    choose_move_token(Board, Player, TempBoard , ThisTurn),
    draw_game(TempBoard, Player, RedPieces, WhitePieces),

    % choose piece move
    get_players_pieces(Player, RedPieces, WhitePieces, Pieces),
    choose_move_piece(TempBoard, Player, NewBoard,Pieces, NewPieces, ThisTurn).
