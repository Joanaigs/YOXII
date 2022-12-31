:- consult('board.pl').
:- consult('game.pl').
:- consult('token.pl').
:- consult('piece.pl').
:- consult('logic.pl').

/**
 * manageInput( +Input )
 *      @param Input - The option chosen by the user.     
 * 
 *      Checks if the Input is a valid option and starts the game. 
 *      If the option is Computer vs Computer or Human vs Computer or Computer vs Human, it asks for the level.
 *      If the option is Exit, it fails.
 *      If the option is invalid, it prints an error message.
*/
manageInput('1') :- 
    startGame('H', 'H').
manageInput('2') :- 
    printLevel,
    askLevel(Level),
    startGame('H', 'PC'-Level).
manageInput('3') :- 
    printLevel,
    askLevel(Level),
    startGame('PC'-Level, 'H').
manageInput('4') :-
    printLevel,
    askLevel(Level), 
    startGame('PC'-Level, 'PC'-Level).
manageInput('0') :- !, fail.

manageInput(_) :-
    write('Invalid option!'),nl.
    

/**
 * play
 *      Prints the main menu and asks for the option.
 *      If the option is Exit, it fails.
 *      If the option is invalid, it prints an error message.
*/
play :-
    printMainMenu,
    write('> Insert your option: '),
    get_char(Input),
    skip_line,
    manageInput(Input), !,
    play.

/**
 * askLevel( -Level)
 *     @param Level - The level chosen by the user.
 * 
 *      Read the level from the user.
 *      If the level isnt 1 or 2, it prints an error message and asks for the level again .
 *      otherwise, it returns the level.
*/
askLevel(Level) :-
    get_char(Input),
    skip_line,
    convertToInt(Input, Level),
    (Level = 1; Level = 2).

askLevel(Level) :-
    write('Invalid Level!'),nl,
    askLevel(Level).

/**
 * printMainMenu
 * 
 *      Prints the main menu.
*/
printMainMenu :-
    nl,nl,
    write(' _______________________________________________________________________ '),nl,
    write('|                                                                       |'),nl,
    write('|                                                                       |'),nl,
    write('|                __   __   ___   __  __    ___     ___                  |'),nl,
    write('|                \\ \\ / /  / _ \\  \\ \\/ /   |_ _|   |_ _|                 |'),nl,
    write('|                 \\ V /  | (_) |  >  <     | |     | |                  |'),nl,
    write('|                 _|_|_   \\___/  /_/\\_\\   |___|   |___|                 |'),nl,
    write('|               _| """ |_|"""""|_|"""""|_|"""""|_|"""""|                |'),nl,
    write('|               "`-0-0-\'"`-0-0-\'"`-0-0-\'"`-0-0-\'"`-0-0-\'                |'),nl,
    write('|               -----------------------------------------               |'),nl,
    write('|                                                                       |'),nl,
    write('|                                                                       |'),nl,
    write('|                                                                       |'),nl,
    write('|                          1. Player vs Player                          |'),nl,
    write('|                                                                       |'),nl,
    write('|                          2. Player vs Computer                        |'),nl,
    write('|                                                                       |'),nl,
    write('|                          3. Computer vs Player                        |'),nl,
    write('|                                                                       |'),nl,
	write('|                          4. Computer vs Computer                      |'),nl,
    write('|                                                                       |'),nl,
    write('|                          0. Exit                                      |'),nl,
    write('|                                                                       |'),nl,
    write('|                                                                       |'),nl,
    write(' _______________________________________________________________________ '),nl,nl,nl.

/**
 * printLevel
 * 
 *      Prints the level menu.
*/
printLevel :-
    nl,nl,
    write(' _______________________________________________________________________ '),nl,
    write('|                                                                       |'),nl,
    write('|                                                                       |'),nl,
    write('|                __   __   ___   __  __    ___     ___                  |'),nl,
    write('|                \\ \\ / /  / _ \\  \\ \\/ /   |_ _|   |_ _|                 |'),nl,
    write('|                 \\ V /  | (_) |  >  <     | |     | |                  |'),nl,
    write('|                 _|_|_   \\___/  /_/\\_\\   |___|   |___|                 |'),nl,
    write('|               _| """ |_|"""""|_|"""""|_|"""""|_|"""""|                |'),nl,
    write('|               "`-0-0-\'"`-0-0-\'"`-0-0-\'"`-0-0-\'"`-0-0-\'                |'),nl,
    write('|               -----------------------------------------               |'),nl,
    write('|                                                                       |'),nl,
    write('|                          1. Nivel 1                                   |'),nl,
    write('|                                                                       |'),nl,
    write('|                          2. Nivel 2                                   |'),nl,
    write('|                                                                       |'),nl,
    write('|                          0. Exit                                      |'),nl,
    write('|                                                                       |'),nl,
    write('|                                                                       |'),nl,
    write(' _______________________________________________________________________ '),nl,nl,nl.

