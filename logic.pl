:- consult('token.pl').
:- consult('piece.pl').

/*
escolher_posicao_peca(Board, Player,  RedPieces, WhitePieces, Line-Col) :-
    % percorrer as jogadas da peca
    % findall
    escolher_token(Board, Player, RedPieces, WhitePieces)

escolher_peca(Board, Player,  RedPieces, WhitePieces, Line-Col) :-
    % percorrer as possiveis pecas
    %findall

escolher_token(Board, Player,  RedPieces, WhitePieces, Line-Col) :-
    % percorrer as possiveis pecas
    %findall

choose_move(Board, Player, 'PC'-2, RedPieces, WhitePieces, NewBoard, NewPieces) :-
    escolher_token(Board, Player, RedPieces, WhitePieces, Line-Col),
    % percorrer as jogadas possiveis do token
*/
choose_move(Board, Player, ThisTurn, RedPieces, WhitePieces, NewBoard, NewPieces) :-
    % choose token move
    choose_move_token(Board, Player, TempBoard , ThisTurn),
    draw_game(TempBoard, Player, RedPieces, WhitePieces),

    % choose piece move
    get_players_pieces(Player, RedPieces, WhitePieces, Pieces),
    choose_move_piece(TempBoard, Player, NewBoard,Pieces, NewPieces, ThisTurn).
