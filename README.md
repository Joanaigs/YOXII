# YOXII
Grupo Yoxii_5:
- up202004598: Afonso Castanheira de Abreu Nabais Baldo - contribution 50%
-  up202006279: Joana Inês Gonçalves Santos - contribution 50%

## Table of contents
- [Installation and Execution](#installation-and-execution)
- [Game description](#game-description)
    - [Board](#board)
    - [How to Play](#how-to-play)


## Installation and Execution
Start by opening a SicStus terminal and by consulting the file main.pl either by using the command `consult('.../main.pl')` or going to `file`->`Consult`-> Choose file `main.pl`
Then you can simply type `play.` to start the game.

## Game description
https://cdn.1j1ju.com/medias/0d/84/35-yoxii-rulebook.pdf
https://www.philibertnet.com/en/cosmoludo/106008-yoxii-3770015431041.htmlhttps://boardgamegeek.com/boardgame/361084/yoxii
Yoxii is a powerful abstract game, which mixes chess and go but which is neither like one nor the other. To win, you will have to surround the *token* with your strongest pieces. Wins the player with the highest points around the token.

### Board
The game board has 37 squares and resembles a plus sign. The pieces are a *token*(#) and 37 pieces, white and red. Every player has 18 pieces total, five each valued 1(o, O), 2(t, T), and 3(y, Y) and three valued 4(x, X).
![](https://i.imgur.com/c5VjYKM.png)
### How to Play
#### Move the token
Each player in turn moves the Totem one square directly around it in any direction as long as the destination square is free.
Then they place one of their pieces on another free square around the new position of the Totem before handing over to the opponent.
Players can move the Totem several squares by jumping over their own pieces, only if they form a continuous line (diagonal or orthogonal) leading to a free square (it is therefore also possible to jump over only one piece further if it leads to a free square).
It is forbidden to jump over the pieces of the opponent’s color.

#### Put a Piece
Each time a player has moved the Totem, they must place one of their pieces (of any value) on one of the free squares directly around it (8 squares maximum).
The value of the pieces played is chosen by the players according to the strategy they choose to adopt.
**Exception**
When a player has just moved the Totem and all the squares around it are occupied, they must play one of their pieces on any other available square of the board.

#### Game Over
The game ends when a player can no longer move the Totem (at this point, the Totem must be encircled).Then for each player it is calculated the total value of the pieces surrounding the Totem (8 pieces in total). The player with the most points wins the game.
If both players have the same score when the Totem is encircled, the player with the most pawns of his color wins the game. If both score and colors are equal, the game ends in a tie.

## Game Logic
### Internal representation of the state of the game:
The board is represented by a list of lists of different atoms, that are change for the respective symbols when drawing the game, with the help of the function symbol (`symbol( ?Piece, ?Symbol, ?Color)`). 
```prolog
initialBoard(B).
B=[
    [invalid,invalid,empty,empty,empty,invalid, invalid],
    [invalid,empty,empty,empty,empty,empty,invalid],
    [empty,empty,empty,empty,empty,empty,empty],
    [empty,empty,empty,token,empty,empty,empty],
    [empty,empty,empty,empty,empty,empty,empty],
    [invalid,empty,empty,empty,empty,empty,invalid],
    [invalid,invalid,empty,empty,empty,invalid, invalid]
    ]
symbol(empty,' ', _).
symbol(invalid,'@', _).
symbol(whiteOne,'O', white).
symbol(whiteTwo,'T', white).
symbol(whiteThree,'Y', white).
symbol(whiteFour,'X', white).
symbol(redOne,'o', red).
symbol(redTwo,'t', red).
symbol(redThree,'y', red).
symbol(redFour,'x', red).
symbol(token,'#', _).
```
The Pieces that the user still has to play are represent of a list of *atom-number_of_pices_left*
```prolog
initialPiecesRed(RedPieces)
RedPieces=[redOne-5, redTwo-5, redThree-5, redFour-3].

initialPiecesWhite(WhitePieces)
WhitePieces=[whiteOne-5, whiteTwo-5, whiteThree-5, whiteFour-3].
```
After playing for a bit the Board and Pieces will have now this look
```prolog
B=[
    [invalid,invalid,empty,empty,empty,invalid,invalid],
    [invalid,redTwo,empty,redFour,whiteThree,whiteThree,invalid],
    [whiteTwo,empty,whiteThree,empty,empty,redThree,whiteTwo],
    [redTwo,token,empty,whiteThree,empty,redOne,empty],
    [empty,empty,empty,empty,empty,empty,redOne],
    [invalid,whiteTwo,empty,empty,empty,empty,invalid],
    [invalid,invalid,empty,empty,empty,invalid,invalid]
    ]

RedPieces=[redOne-3, redTwo-3, redThree-4, redFour-2].

WhitePieces=[whiteOne-5, whiteTwo-3, whiteThree-1, whiteFour-3].
``` 

At the end of one game the Board and Pieces might have this look
```prolog
B=[
    [invalid,invalid,token,whiteTwo,empty,invalid,invalid],
    [invalid,redTwo,redThree,redFour,whiteThree,whiteThree,invalid],
    [whiteTwo,whiteOne,whiteThree,empty,whiteOne,redThree,whiteTwo],
    [redTwo,redOne,empty,whiteThree,empty,redOne,empty,
    [redOne,empty,empty,whiteFour,empty,empty,redOne],
    [invalid,whiteTwo,redOne,whiteFour,redThree,empty,invalid],
    [invalid,invalid,empty,empty,empty,invalid,invalid]
    ]

RedPieces=[redOne-0, redTwo-3, redThree-2, redFour-2].

WhitePieces=[whiteOne-3, whiteTwo-1, whiteThree-1, whiteFour-1].
``` 
Which results in this board:
![](https://i.imgur.com/8dtVDTq.png)

### Game state view
We start by displaying the **menu** with the help of the predicate **printMainMenu/0** which results in this amazing output:
![](https://i.imgur.com/uqjQh7o.png)
In this **menu** we are presented with 4 options besides the Exit one. If we **type 1 and then click enter** we will immediately start the game. If we choose the other 3 options we will be confronted with another menu to **choose the level of difficulty**. After choosing the level the game will immediately start.
![](https://i.imgur.com/edpF90x.png)
After the game starts you will be displayed with the **Player** whose move it is, the **Board** and the **pieces** available to **each player**, with  the help of the function **display_game( +GameState, +Player, +RedPieces, +WhitePieces)**. We will also display the possible moves for each play for the player to choose from. The input of the move if of the type **line-column** and then you just need to click enter
![](https://i.imgur.com/fSh44uY.png)


### Moves Execution
For a player to move his pieces, he has to execute the **choose_move**. 
If the player is human or PC-1 (IA level 1), it calls the perdicates **choose_move_token** and then **choose_move_piece**, to move the *token* and the piece respectively. 
If it is PC-2, it calls the perdicate **choose_move_greedy**.
If the player is human, he will receive as input where to move and which piece to move, otherwise he will choose the position to move the *token* to, the piece to place, and its position using different algorithms.

The perdicate **move** is used to move the *token* and place the piece in the board. It receives different parameters depending on if we want to move or to place a piece in the board.
The perdicate with 4 parameters move along the board's lines until it reaches the row of the given piece and then replaces this position with a piece. 

The other perdicate with 5 parameters, first get the piece that is in the Old position, then places that piece in the New Position and change the oldposition to empty. Then 

```prolog
% move(+Board, +Line, +Column, +NewElem, -NewBoard)
move([Line|Board], 1, Column, Piece, [NewLine|Board]) :- replace(Line, Column, Piece, Result), NewLine=Result.
move([Line|Board], IndexLine, Column, Piece, [Line|NewBoard]) :- Temp is IndexLine-1, move(Board, Temp, Column, Piece, NewBoard).

move(Board,  OldLine-OldColumn, NewLine-NewColumn,NewBoard) :- 
    getPiece(Board, OldLine-OldColumn, Piece),
    move(Board, NewLine, NewColumn, Piece, TmpBoard),
    move(TmpBoard, OldLine, OldColumn, empty, NewBoard).
```

To verify if the move is valid we check if this move is in the list of valid moves using the predicate **select_move**.



### List of Valid Moves
During the game we obtain to different valid moves, the valid moves of the *token* and the pieces. 

When talking about the *token*, the player can move it to the squares directly arround it in any direction or it can move several quares by jumping over the player pieces. To find this moves we use the predicate **valid_moves_token(+GameState, +Player, -ListOfMoves)**  which calls the predicate **setof** to find all the possible options with the help of the function **check_move(+GameState, +Player, ?Line-Column)** which checks if the move given is valid or not.
```prolog
valid_moves_token(Board, Player,Moves):-
    setof(Line-Column, check_move(Board, Player, Line-Column), Moves).

```

When talking about the valid moves of a piece, if there is space arround the *token*, we place the piece in one of the spaces, if not, it can go in any empty space in the board . We do this with the help of **valid_moves_piece(+GameState, -ListOfMoves)**.
```prolog
valid_moves_piece(Board, Moves):-
    setof(Line-Column, check_move_piece(Board, Line-Column), Moves).
valid_moves_piece(Board, Moves):-
    setof(Line-Column, getPiece(Board, Line-Column, empty), Moves).
```
If their is space arround the *token*, it will saty in the first predicate, that calls the predicate **check_move_piece(GameState, Line-Column)** that check if that the position given is arround the position given is arround the *token*. However if their isn't any space  arround the *token* it will enter the second predicate that returns all the empty spaces with the help of the function **getPiece(+GameState, ?Position, ?Piece)**.

### End of Game

In each game cycle we check if the game is over or not, to do this we use the help of the predicate **game_over(+GameSate, +Player)** where we check if their are any valid moves for the *token* left, with the help of **valid_moves_token(+GameState, +Player, -ListOfMoves)**.If there aren't the game ends an we calculate the points and the number of pieces arround the *token* of each player and check who is the winner. 

### Board Evaluation
To evaluate the board we use the predicate **value(+GameSate, +Player, -Points, -Pieces)** that calculates the number of points that the player has. To calculate the points, we sum the value of the pieces that the player has arround the *token*. In this predicate we also calculate the number of pieces that the player has arround the *token*.

### Computer move
For the computer to choose which move to make, we use the perdicate **choose_move(+GameState, +Player, +Mode, +RedPieces, +WhitePieces, -NewGameState, -NewPieces)**.
*  If the Mode is **'PC'-1**, in **choose_move_token**, the computer will choose from the list returned in **valid_moves_token** a random move for the token, and make that move, then in **choose_move_piece** will choose a random piece and an random valid position from **valid_moves_piece** and put that piece in that position 
* If the Mode is **'PC'-2**, the predicate **choose_move_greedy(+GameState, +Player, +RedPieces, +WhitePieces, -TMove, -Piece, -PMove, +last_move)** will choose a Move for the token, a piece, and the position to put the piece.  
    1. First it will get the lists of valid moves for token, and then using **setof** for each token move, using the **find_best_piece** predicate, we find the best piece to play and the best position to put it in. This way, we create a list with the points of the move and the move (the new token position, the piece to be placed and its position) sorted from lowest to highest points (Final list).
    2. Then using **find_best_move(+GameState, +Player, +Moves, -BPoints-BTMove-BPMove-BPiece, +RedPieces, +WhitePieces)**, we will choose the best move from that list. 
    ```prolog
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
    ```
    The predicate **find_best_move(+GameState, +Player, +Moves, -BPoints-BTMove-BPMove-BPiece, +RedPieces, +WhitePieces)** choose the best move from Moves for the player, it will try to guess the opponent next move, and choose the move with largest Points diferent.
    For each Move in Moves:
    * Simulate that play
    * Then checks if it is gameover
        * If is gameover, it calcule the opponent points and compare with the player points. If player points are greater, it is the best move, otherwise, it checks the next move in the list.
        * If is not game over, it try to guess the opponent best play, using **choose_move_greedy(GameState, Player,  RedPieces, WhitePieces, TMove, Piece, PMove, last_move)** (with last_move so it does the same thing that **choose_move_greedy** did earlier, but then it doesn't call **find_best_move** , but returns the last element of the Final list (with the highest number of points). <br> Then simulates that play, and calcule and compare the opponent points with the player points and returns the move with the highest  points difference in the list.
    ```prolog
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
    ```
        
## Conclusions 
The board game YOXII was successfully implemented in the Prolog language, our main dificulty being the creation of the algoritm for the second level of dificulty. Some improvements would be having the ai trying to gess more future plays when deciding witch move would be the best.

## Bibliography
https://boardgamegeek.com/boardgame/361084/yoxii
https://www.philibertnet.com/en/cosmoludo/106008-yoxii-3770015431041.html
https://boardgamearena.com/gamepanel?game=yoxii