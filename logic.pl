
:- use_module(library(lists)).
:- use_module(library(system)).

/**
 * find_piece_position(+Board, +Player, +Pieces, +Moves, -Line-Column, +Piece, -Points)
 *      @param Board - Board
 *      @param Player - Player
 *      @param Pieces - Pieces
 *      @param Moves - Moves
 *      @param Line-Column - Line-Column
 *      @param Piece - Piece
 *      @param Points - Points
 * 
 *      It choose a piece in the list of pieces and choose a move in the list of moves and returns the points and Line-Column of the move.
 * */
find_piece_position(Board, Player,  Pieces, Moves, Line-Column, Piece, Points) :-
    existPiece(Pieces, Piece),
    select_move(Line-Column, Moves),
    setPiece(Board, NewBoard, Line, Column, Piece), 
    countPoints(NewBoard, Player, Points, _).

/**
 * find_best_piece(+Board, +Player, +RedPieces, +WhitePieces, -Line-Column, -Piece, -Points)
 *      @param Board - Board
 *      @param Player - Player
 *      @param RedPieces - Red Pieces
 *      @param WhitePieces - White Pieces
 *      @param Line-Column - Line-Column
 *      @param Piece - Piece
 *      @param Points - Points
 * 
 *      It finds the best piece to move and the best move for the player, based on the points of the move.
 *
 */
find_best_piece(Board, Player,  RedPieces, WhitePieces, Line-Column, Piece, Points) :-
    valid_moves_piece(Board,Moves),
    get_players_pieces(Player, RedPieces, WhitePieces, Pieces),

    setof(Points-Line-Column-Piece, 
        find_piece_position(Board, Player, Pieces, Moves, Line-Column, Piece, Points),
            Final),
    last(Final, Points-Line-Column-Piece).

/**
 * compares_choose(+Points1, +Move1, +Points2, +Move2, -Move)
 *      @param Points1 - Points1
 *      @param Move1 - Move1
 *      @param Points2 - Points2
 *      @param Move2 - Move2
 * 
 *      It compares the points of the two moves and returns the move with the highest points.
 */
compares_choose(Points1, Move1, Points2, _Move2, Move) :-  Points1 >= Points2, !, Move = Move1.
compares_choose(Points1, _Move1, Points2, Move2, Move) :- Points1 < Points2, !, Move = Move2.

% encontrar a melhor jogada
/**
 * find_best_move(+Board, +Player, +Moves, -BPoints-BTMove-BPMove-BPiece, +RedPieces, +WhitePieces)
 *      @param Board - Board
 *      @param Player - Player
 *      @param Moves - Moves
 *      @param BPoints-BTMove-BPMove-BPiece - Best Points-Best Token Move-Best Piece Move-Best Piece
 *      @param RedPieces - Red Pieces
 *      @param WhitePieces - White Pieces
 * 
 * @description
 *      This predicate finds the best move for the player.
 *      It simulates the move in list of moves and then checks if it is gameover,
 *                  if is gameover, it checks the other player points and compares with the current player points, 
 *                          if player points are greater, it is the best move, if not, it checks the next move in the list.
 *                  if is not gameover, it returns the move with the highest  points difference.
 * 
 */
find_best_move(_,_,[],Points1-(TLine1-TColumn1)-(PLine1-PColumn1)-Piece1,_,_) :-  !, Points1 = 0, TLine1 = 0, TColumn1= 0,PLine1= 0,PColumn1= 0,Piece1= 0 , nl.
find_best_move(Board, Player, [Points-(TLine-TColumn)-(PLine-PColumn)-Piece | Resto], BPoints-BTMove-BPMove-BPiece,  RedPieces, WhitePieces):-
    
    getPiece(Board, TokenL-TokenC, token),
    move(Board, Temp, TokenL-TokenC, TLine-TColumn),
    setPiece(Temp, NewBoard, PLine, PColumn, Piece),
    
    next_player(Player, NextPlayer),
    game_over(NewBoard, NextPlayer),!,
    
    find_best_move(Board, Player, Resto, Points1-(TLine1-TColumn1)-(PLine1-PColumn1)-Piece1, RedPieces, WhitePieces),
    
    countPoints(NewBoard, NextPlayer, OtherPoints, _),

    compares_choose(Points, Points-(TLine-TColumn)-(PLine-PColumn)-Piece, OtherPoints, Points1-(TLine1-TColumn1)-(PLine1-PColumn1)-Piece1, BPoints-BTMove-BPMove-BPiece).
    
find_best_move(Board, Player, [_Points-(TLine-TColumn)-(PLine-PColumn)-Piece | Resto], BPoints-BTMove-BPMove-BPiece,  RedPieces, WhitePieces) :-
    % not gameover
    find_best_move(Board, Player, Resto, ListPointsDiff-(TLine1-TColumn1)-(PLine1-PColumn1)-Piece1, RedPieces, WhitePieces),
    
    % player plays
    ListMove = ListPointsDiff-(TLine1-TColumn1)-(PLine1-PColumn1)-Piece1,

    getPiece(Board, TokenL-TokenC, token),
    move(Board, Temp, TokenL-TokenC, TLine-TColumn),
    setPiece(Temp, NewBoard1, PLine, PColumn, Piece),
    get_players_pieces(Player, RedPieces, WhitePieces, Pieces),
    usePiece(Pieces, Piece, NewPieces),
    update_players_pieces(Player, RedPieces, WhitePieces, NewPieces, NewRedPieces, NewWhitePieces),

    % other player plays
    next_player(Player, NextPlayer),
    choose_move_greedy(NewBoard1, NextPlayer, NewRedPieces, NewWhitePieces, (WTLine-WTColumn), WPiece, (WPMove-WPColumn), last_move), 
    getPiece(NewBoard1, WTokenL-WTokenC, token),
    
    move(NewBoard1, TNewBoard, WTokenL-WTokenC, WTLine-WTColumn),
    setPiece(TNewBoard, NewBoard, WPMove, WPColumn, WPiece),

    % count player points
    countPoints(NewBoard, Player, ThisPoints, _),  
    ThisMove = ThisPoints-(TLine-TColumn)-(PLine-PColumn)-Piece,

    % count other player points
    countPoints(NewBoard, NextPlayer, OtherPoints, _),

    PointsDiff is ThisPoints - OtherPoints,

    % return the best move this or the list move
    compares_choose(PointsDiff, ThisMove, ListPointsDiff, ListMove, BPoints-BTMove-BPMove-BPiece).

/**
 * choose_move_greedy(+Board, +Player, +RedPieces, +WhitePieces, -TMove, -Piece, -PMove, +last_move)
 *      @param Board - Board
 *      @param Player - Player
 *      @param RedPieces - Red Pieces
 *      @param WhitePieces - White Pieces
 *      @param TMove - Token Move
 *      @param Piece - Piece
 *      @param PMove - Piece Move
 *      @param last_move - check if it is to calculate the next player move or not
 * 
 *     Choose the best move for the greedy player.
 *     If the last_move is not given, it will calculate his possibles moves and then calcules the other player best play, and choose the best move using that play
 *     If the last_move is given, it will return the best move  based on the points of the player
 * 
 * 
 */
choose_move_greedy(Board, Player,  RedPieces, WhitePieces, TMove, Piece, PMove, last_move) :- !,
    valid_moves_token(Board, Player, Moves),
    getPiece(Board, TokenL-TokenC, token),
    setof(Points-(TLine-TColumn)-(PLine-PColumn)-Piece, 
        TempNewBoard^(
                select_move(TLine-TColumn, Moves),
                move(Board, TempNewBoard, TokenL-TokenC, TLine-TColumn),
                find_best_piece(TempNewBoard, Player,  RedPieces, WhitePieces, PLine-PColumn, Piece, Points)),
            Final),
    last(Final, Points-TMove-PMove-Piece).

choose_move_greedy(Board, Player,  RedPieces, WhitePieces, TMove, Piece, PMove) :-
    valid_moves_token(Board, Player, Moves),
    getPiece(Board, TokenL-TokenC, token),
    setof(Points-(TLine-TColumn)-(PLine-PColumn)-Piece, 
        TempNewBoard^(
                select_move(TLine-TColumn, Moves),
                move(Board, TempNewBoard, TokenL-TokenC, TLine-TColumn),
                find_best_piece(TempNewBoard, Player,  RedPieces, WhitePieces, PLine-PColumn, Piece, Points)),
            Final),
    find_best_move(Board, Player, Final, _-TMove-PMove-Piece, RedPieces, WhitePieces).

/**
 * choose_move(+Board, +Player, +Mode, +RedPieces, +WhitePieces, -NewBoard, -NewPieces)
 *      @param Board - current board
 *      @param Player - current player
 *      @param Mode    H - human, 'PC'-1 - random Move, 'PC'-2 - greedy Move
 *      @param RedPieces - list of red pieces
 *      @param WhitePieces - list of white pieces
 *      @param NewBoard - new board
 *      @param NewPieces - new pieces
 * 
 *      This predicate is used to choose the move to be made by the player, 
 *             first it chooses where to move the token and then choose the piece to be placed and where.
 *      If the mode is H, it will ask the user for the move, if it is 'PC'-1 or 'PC'-2 it will choose a random or greedy move.
 */ 

choose_move(Board, Player, 'PC'-2, RedPieces, WhitePieces, NewBoard, NewPieces) :- !,
    choose_move_greedy(Board, Player,  RedPieces, WhitePieces, (TLine-TColumn), Piece, (PLine-PColumn)),

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
