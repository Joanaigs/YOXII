
:- use_module(library(lists)).
:- use_module(library(system)).

/**
 * find_piece_position(+GameState, +Player, +Pieces, +Moves, -Line-Column, +Piece, -Points)
 *      @param GameState - GameState
 *      @param Player - Player
 *      @param Pieces - Pieces
 *      @param Moves - Moves
 *      @param Line-Column - Line-Column
 *      @param Piece - Piece
 *      @param Points - Points
 * 
 *      It choose a piece in the list of pieces and choose a move in the list of moves and returns the points and Line-Column of the move.
 * */
find_piece_position(GameState, Player,  Pieces, Moves, Line-Column, Piece, Points) :-
    existPiece(Pieces, Piece),
    select_move(Line-Column, Moves),
    move(GameState, Line, Column, Piece, NewGameState), 
    countPoints(NewGameState, Player, Points, _).

/**
 * find_best_piece(+GameState, +Player, +RedPieces, +WhitePieces, -Line-Column, -Piece, -Points)
 *      @param GameState - GameState
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
find_best_piece(GameState, Player,  RedPieces, WhitePieces, Line-Column, Piece, Points) :-
    valid_moves_piece(GameState,Moves),
    get_players_pieces(Player, RedPieces, WhitePieces, Pieces),

    setof(Points-Line-Column-Piece, 
        find_piece_position(GameState, Player, Pieces, Moves, Line-Column, Piece, Points),
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
 * find_best_move(+GameState, +Player, +Moves, -BPoints-BTMove-BPMove-BPiece, +RedPieces, +WhitePieces)
 *      @param GameState - GameState
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
find_best_move(GameState, Player, [Points-(TLine-TColumn)-(PLine-PColumn)-Piece | Resto], BPoints-BTMove-BPMove-BPiece,  RedPieces, WhitePieces):-
    
    getPiece(GameState, TokenL-TokenC, token),
    move(GameState, TokenL-TokenC, TLine-TColumn, Temp),
    move(Temp, PLine, PColumn, Piece, NewGameState),
    
    next_player(Player, NextPlayer),
    game_over(NewGameState, NextPlayer),!,
    
    find_best_move(GameState, Player, Resto, Points1-(TLine1-TColumn1)-(PLine1-PColumn1)-Piece1, RedPieces, WhitePieces),
    
    countPoints(NewGameState, NextPlayer, OtherPoints, _),

    compares_choose(Points, Points-(TLine-TColumn)-(PLine-PColumn)-Piece, OtherPoints, Points1-(TLine1-TColumn1)-(PLine1-PColumn1)-Piece1, BPoints-BTMove-BPMove-BPiece).
    
find_best_move(GameState, Player, [_Points-(TLine-TColumn)-(PLine-PColumn)-Piece | Resto], BPoints-BTMove-BPMove-BPiece,  RedPieces, WhitePieces) :-
    % not gameover
    find_best_move(GameState, Player, Resto, ListPointsDiff-(TLine1-TColumn1)-(PLine1-PColumn1)-Piece1, RedPieces, WhitePieces),
    
    % player plays
    ListMove = ListPointsDiff-(TLine1-TColumn1)-(PLine1-PColumn1)-Piece1,

    getPiece(GameState, TokenL-TokenC, token),
    move(GameState, TokenL-TokenC, TLine-TColumn, Temp),
    move(Temp, PLine, PColumn, Piece, NewGameState1),

    get_players_pieces(Player, RedPieces, WhitePieces, Pieces),
    usePiece(Pieces, Piece, NewPieces),
    update_players_pieces(Player, RedPieces, WhitePieces, NewPieces, NewRedPieces, NewWhitePieces),

    % other player plays
    next_player(Player, NextPlayer),
    choose_move_greedy(NewGameState1, NextPlayer, NewRedPieces, NewWhitePieces, (WTLine-WTColumn), WPiece, (WPMove-WPColumn), last_move), 
    getPiece(NewGameState1, WTokenL-WTokenC, token),
    
    move(NewGameState1, WTokenL-WTokenC, WTLine-WTColumn, TNewGameState),
    move(TNewGameState, WPMove, WPColumn, WPiece, NewGameState),

    % count player points
    countPoints(NewGameState, Player, ThisPoints, _),  
    ThisMove = ThisPoints-(TLine-TColumn)-(PLine-PColumn)-Piece,

    % count other player points
    countPoints(NewGameState, NextPlayer, OtherPoints, _),

    PointsDiff is ThisPoints - OtherPoints,

    % return the best move this or the list move
    compares_choose(PointsDiff, ThisMove, ListPointsDiff, ListMove, BPoints-BTMove-BPMove-BPiece).

/**
 * choose_move_greedy(+GameState, +Player, +RedPieces, +WhitePieces, -TMove, -Piece, -PMove, +last_move)
 *      @param GameState - GameState
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
choose_move_greedy(GameState, Player,  RedPieces, WhitePieces, TMove, Piece, PMove, last_move) :- !,
    valid_moves_token(GameState, Player, Moves),
    getPiece(GameState, TokenL-TokenC, token),
    setof(Points-(TLine-TColumn)-(PLine-PColumn)-Piece, 
        TempNewGameState^(
                select_move(TLine-TColumn, Moves),
                move(GameState, TokenL-TokenC, TLine-TColumn, TempNewGameState),
                find_best_piece(TempNewGameState, Player,  RedPieces, WhitePieces, PLine-PColumn, Piece, Points)),
            Final),
    last(Final, Points-TMove-PMove-Piece).

choose_move_greedy(GameState, Player,  RedPieces, WhitePieces, TMove, Piece, PMove) :-
    valid_moves_token(GameState, Player, Moves),
    getPiece(GameState, TokenL-TokenC, token),
    setof(Points-(TLine-TColumn)-(PLine-PColumn)-Piece, 
        TempNewGameState^(
                select_move(TLine-TColumn, Moves),
                move(GameState, TokenL-TokenC, TLine-TColumn, TempNewGameState),
                find_best_piece(TempNewGameState, Player,  RedPieces, WhitePieces, PLine-PColumn, Piece, Points)),
            Final),
    find_best_move(GameState, Player, Final, _-TMove-PMove-Piece, RedPieces, WhitePieces).

/**
 * choose_move(+GameState, +Player, +Mode, +RedPieces, +WhitePieces, -NewGameState, -NewPieces)
 *      @param GameState - current GameState
 *      @param Player - current player
 *      @param Mode    H - human, 'PC'-1 - random Move, 'PC'-2 - greedy Move
 *      @param RedPieces - list of red pieces
 *      @param WhitePieces - list of white pieces
 *      @param NewGameState - new GameState
 *      @param NewPieces - new pieces
 * 
 *      This predicate is used to choose the move to be made by the player, 
 *             first it chooses where to move the token and then choose the piece to be placed and where.
 *      If the mode is H, it will ask the user for the move, if it is 'PC'-1 or 'PC'-2 it will choose a random or greedy move.
 */ 

choose_move(GameState, Player, 'PC'-2, RedPieces, WhitePieces, NewGameState, NewPieces) :- !,
    choose_move_greedy(GameState, Player,  RedPieces, WhitePieces, (TLine-TColumn), Piece, (PLine-PColumn)),

    % move token
    getPiece(GameState, TokenL-TokenC, token),
    move(GameState, TokenL-TokenC, TLine-TColumn, TempGameState),
    letter(TLine,CharLineT),
    nl, write('Move token to ') , write(CharLineT-TColumn), nl,
    sleep(1),
    
    display_game(TempGameState, Player, RedPieces, WhitePieces),

    % place piece
    move(TempGameState, PLine, PColumn, Piece, NewGameState),
    get_players_pieces(Player, RedPieces, WhitePieces, Pieces),
    usePiece(Pieces, Piece, NewPieces),
    
    letter(PLine, CharLine),
    symbol(Piece, Symbol, _),
    nl,write('Add piece '), write(Symbol) , write(' to ') , write(CharLine-PColumn), nl,
    sleep(1).


choose_move(GameState, Player, ThisTurn, RedPieces, WhitePieces, NewGameState, NewPieces) :-
    % choose token move
    choose_move_token(GameState, Player, TempGameState , ThisTurn),
    sleep(1),
    display_game(TempGameState, Player, RedPieces, WhitePieces),
    % choose piece move
    get_players_pieces(Player, RedPieces, WhitePieces, Pieces),
    choose_move_piece(TempGameState, Player, NewGameState,Pieces, NewPieces, ThisTurn),
    sleep(1).
