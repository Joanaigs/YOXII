:- consult('board.pl').
:- consult('game.pl').

manageInput(1) :- 
    startGame('P','P'),
    mainMenu.

mainMenu :-
    printMainMenu,
    askMenuOption,
    read(Input),
    manageInput(Input).


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
	write('|                          3. Computer vs Computer                      |'),nl,
    write('|                                                                       |'),nl,
    write('|                          0. Exit                                      |'),nl,
    write('|                                                                       |'),nl,
    write('|                                                                       |'),nl,
    write(' _______________________________________________________________________ '),nl,nl,nl.

askMenuOption :-
    write('> Insert your option: ').