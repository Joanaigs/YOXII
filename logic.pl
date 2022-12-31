
:- use_module(library(lists)).
:- use_module(library(system)).


escolher_posicao_peca(Board, Player,  Pieces, Moves, Line-Column, Piece, Points) :-
    existPiece(Pieces, Piece),
    select_move(Line-Column, Moves),
    setPiece(Board, NewBoard, Line, Column, Piece), 
    countPoints(NewBoard, Player, Points, _).


escolher_peca(Board, Player,  RedPieces, WhitePieces, Line-Column, Piece, Points) :-
    valid_moves_piece(Board,Moves),
    get_players_pieces(Player, RedPieces, WhitePieces, Pieces),

    setof(Points-Line-Column-Piece, 
        escolher_posicao_peca(Board, Player, Pieces, Moves, Line-Column, Piece, Points),
            Final),
    last(Final, Points-Line-Column-Piece).


blabla(Points1, Move1, Points2, _Move2, Move) :-  Points1 >= Points2, !, Move = Move1.
blabla(Points1, _Move1, Points2, Move2, Move) :- Points1 < Points2, !, Move = Move2.

% encontrar a melhor jogada
jogar_brancas(_,_,[],Points1-(TLine1-TColumn1)-(PLine1-PColumn1)-Piece1,_,_) :- !, Points1 = 0, TLine1 = 0, TColumn1= 0,PLine1= 0,PColumn1= 0,Piece1= 0 , nl.
jogar_brancas(Board, Player, [Points-(TLine-TColumn)-(PLine-PColumn)-Piece | Resto], BPoints-BTMove-BPMove-BPiece,  RedPieces, WhitePieces):-
    
    getPiece(Board, TokenL-TokenC, token),
    move(Board, Temp, TokenL-TokenC, TLine-TColumn),
    setPiece(Temp, NewBoard, PLine, PColumn, Piece),
    
    next_player(Player, NextPlayer),
    game_over(NewBoard, NextPlayer),!,
    
    jogar_brancas(Board, Player, Resto, Points1-(TLine1-TColumn1)-(PLine1-PColumn1)-Piece1, RedPieces, WhitePieces),
    
    countPoints(NewBoard, NextPlayer, OtherPoints, _),

    blabla(Points, Points-(TLine-TColumn)-(PLine-PColumn)-Piece, OtherPoints, Points1-(TLine1-TColumn1)-(PLine1-PColumn1)-Piece1, BPoints-BTMove-BPMove-BPiece).
    
jogar_brancas(Board, Player, [_Points-(TLine-TColumn)-(PLine-PColumn)-Piece | Resto], BPoints-BTMove-BPMove-BPiece,  RedPieces, WhitePieces) :-
    % jogamos nos
    jogar_brancas(Board, Player, Resto, Points1-(TLine1-TColumn1)-(PLine1-PColumn1)-Piece1, RedPieces, WhitePieces),
    
    Move = Points1-(TLine1-TColumn1)-(PLine1-PColumn1)-Piece1,

    getPiece(Board, TokenL-TokenC, token),
    move(Board, Temp, TokenL-TokenC, TLine-TColumn),
    setPiece(Temp, NewBoard1, PLine, PColumn, Piece),
    get_players_pieces(Player, RedPieces, WhitePieces, Pieces),
    usePiece(Pieces, Piece, NewPieces),
    update_players_pieces(Player, RedPieces, WhitePieces, NewPieces, NewRedPieces, NewWhitePieces),

    % joga o adversario
    next_player(Player, NextPlayer),
    escolher_token(NewBoard1, NextPlayer, NewRedPieces, NewWhitePieces, (WTLine-WTColumn), WPiece, (WPMove-WPColumn), last_move), 
    getPiece(NewBoard1, WTokenL-WTokenC, token),
    
    move(NewBoard1, TNewBoard, WTokenL-WTokenC, WTLine-WTColumn),
    setPiece(TNewBoard, NewBoard, WPMove, WPColumn, WPiece),

    % conta pontos
    countPoints(NewBoard, Player, ThisPoints, _),  
    ThisMove = ThisPoints-(TLine-TColumn)-(PLine-PColumn)-Piece,
    % ver o resto da lista
    blabla(ThisPoints, ThisMove, Points1, Move, BPoints-BTMove-BPMove-BPiece).


escolher_token(Board, Player,  RedPieces, WhitePieces, TMove, Piece, PMove, last_move) :- !,
    valid_moves_token(Board, Player, Moves),
    getPiece(Board, TokenL-TokenC, token),
    setof(Points-(TLine-TColumn)-(PLine-PColumn)-Piece, 
        TempNewBoard^(
                select_move(TLine-TColumn, Moves),
                move(Board, TempNewBoard, TokenL-TokenC, TLine-TColumn),
                escolher_peca(TempNewBoard, Player,  RedPieces, WhitePieces, PLine-PColumn, Piece, Points)),
            Final),
    last(Final, Points-TMove-PMove-Piece).

escolher_token(Board, Player,  RedPieces, WhitePieces, TMove, Piece, PMove) :-
    valid_moves_token(Board, Player, Moves),
    getPiece(Board, TokenL-TokenC, token),
    setof(Points-(TLine-TColumn)-(PLine-PColumn)-Piece, 
        TempNewBoard^(
                select_move(TLine-TColumn, Moves),
                move(Board, TempNewBoard, TokenL-TokenC, TLine-TColumn),
                escolher_peca(TempNewBoard, Player,  RedPieces, WhitePieces, PLine-PColumn, Piece, Points)),
            Final),
    jogar_brancas(Board, Player, Final, Points-TMove-PMove-Piece, RedPieces, WhitePieces).

choose_move(Board, Player, 'PC'-2, RedPieces, WhitePieces, NewBoard, NewPieces) :- !,
    escolher_token(Board, Player,  RedPieces, WhitePieces, (TLine-TColumn), Piece, (PLine-PColumn)),

    % move token
    getPiece(Board, TokenL-TokenC, token),
    move(Board, TempBoard, TokenL-TokenC, TLine-TColumn),
    letter(TLine,CharLineT),
    nl, write('Move token to ') , write(CharLineT-TColumn), nl,
    sleep(1),
    
    draw_game(TempBoard, Player, RedPieces, WhitePieces),

    % place piece
    setPiece(TempBoard, NewBoard, PLine, PColumn, Piece),
    get_players_pieces(Player, RedPieces, WhitePieces, Pieces),
    usePiece(Pieces, Piece, NewPieces),
    
    letter(PLine, CharLine),
    symbol(Piece, Symbol, _),
    nl,write('Add piece '), write(Symbol) , write(' to ') , write(CharLine-PColumn), nl,
    sleep(1).


choose_move(Board, Player, ThisTurn, RedPieces, WhitePieces, NewBoard, NewPieces) :-
    % choose token move
    choose_move_token(Board, Player, TempBoard , ThisTurn),
    sleep(1),
    draw_game(TempBoard, Player, RedPieces, WhitePieces),
    % choose piece move
    get_players_pieces(Player, RedPieces, WhitePieces, Pieces),
    choose_move_piece(TempBoard, Player, NewBoard,Pieces, NewPieces, ThisTurn),
    sleep(1).
