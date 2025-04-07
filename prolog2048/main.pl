:- use_module(library(pce)).
:- dynamic tabuleiro/1.

cell_size(100). % tamanho das células
grid_size(4). % tamanho inicial do game

% Classe customizada para tratar eventos de teclado
:- pce_begin_class(game_window, picture).

event(GameWindow, Ev:event) :->
    get(Ev, key, Key),
    ( member(Key, [cursor_left, cursor_right, cursor_up, cursor_down]) ->
        handle_key(GameWindow, Key)
    ; true
    ).


:- pce_end_class(game_window).

% inicia o joguinho my friends
start :-
    retractall(tabuleiro(_)),
    initial_board(Board),
    assertz(tabuleiro(Board)),
    new(Window, game_window('2048')),
    send(Window, size, size(400, 400)),
    send(Window, open),
    new(FocusBox, box(1,1)),
    send(FocusBox, pen, 0),
    send(Window, display, FocusBox, point(-10, -10)),
    send(Window, focus, FocusBox),
    draw_board(Window, []).


% tabuleiro inicial
initial_board(Board) :-
    empty_board(Empty),
    place_random_piece(Empty, Board).

% limpa o tabuleiro
empty_board(Board) :-
    grid_size(N),
    length(Row, N), maplist(=(0), Row),
    length(Board, N), maplist(=(Row), Board).

% gera uma peça aleatória e adiciona ela no tabuleiro
place_random_piece(Board, NewBoard) :-
    findall((R,C),
        (nth0(R, Board, Row), nth0(C, Row, 0)),
        EmptyCells),
    ( EmptyCells \= [] ->
        random_member((R,C), EmptyCells),
        set_cell(Board, R, C, 2, NewBoard)
    ; NewBoard = Board
    ).

set_cell(Board, R, C, Value, NewBoard) :-
    nth0(R, Board, Row),
    replace_nth(C, Row, Value, NewRow),
    replace_nth(R, Board, NewRow, NewBoard).

replace_nth(Idx, List, Elem, NewList) :-
    same_length(List, NewList),
    append(Before, [_|After], List),
    length(Before, Idx),
    append(Before, [Elem|After], NewList).

% move as peças de uma linha para a esquerda
move_row_left(Row, NewRow) :-
    exclude(=(0), Row, NoZeros),          % remove os zeros
    combine(NoZeros, Combined),           % funde os valores iguais
    length(Row, N),
    length(Combined, L),
    M is N - L,
    length(Padding, M),
    maplist(=(0), Padding),
    append(Combined, Padding, NewRow).    % completa com zeros à direita

% move todas as linhas do tabuleiro para a esquerda
move_left(Board, NewBoard) :-
    maplist(move_row_left, Board, NewBoard).

% tranpõe a matriz para poder reaproveitar o a regra mover_esquerda
transpose([[]|_], []) :- !.
transpose(Matrix, [Row|Rest]) :-
    maplist(nth0(0), Matrix, Row),
    maplist(remove_head, Matrix, TailMatrix),
    transpose(TailMatrix, Rest).

remove_head([_|T], T).

% mover para direita
move_right(Board, NewBoard) :-
    maplist(reverse, Board, Rev),
    move_left(Rev, Moved),
    maplist(reverse, Moved, NewBoard).

% mover para cima
move_up(Board, NewBoard) :-
    transpose(Board, T),
    move_left(T, Moved),
    transpose(Moved, NewBoard).

% mover para baixo
move_down(Board, NewBoard) :-
    transpose(Board, T),
    move_right(T, Moved),
    transpose(Moved, NewBoard).

% mescla
combine([], []).
combine([X], [X]).
combine([X,X|T], [Y|Rest]) :-
    Y is X + X,
    combine(T, Rest).
combine([X,Y|T], [X|Rest]) :-
    X \= Y,
    combine([Y|T], Rest).

% pega os eventos do teclado, ou seja, setinhas
handle_key(Window, Key) :-
    tabuleiro(OldBoard),
    move_board(Key, OldBoard, MovedBoard),
    ( MovedBoard \= OldBoard ->
        place_random_piece(MovedBoard, NewBoard),
        get_merged_positions(OldBoard, MovedBoard, Merged),
        retractall(tabuleiro(_)),
        assertz(tabuleiro(NewBoard)),
        send(Window, clear),
        draw_board(Window, Merged)
    ; true).

% logica de movimento
move_board(cursor_left,  B, NB) :- move_left(B, NB).
move_board(cursor_right, B, NB) :- move_right(B, NB).
move_board(cursor_up,    B, NB) :- move_up(B, NB).
move_board(cursor_down,  B, NB) :- move_down(B, NB).

% desenhao tabuleiro
draw_board(Window, MergedPositions) :-
    tabuleiro(Board),
    draw_grid(Window),
    draw_tiles(Window, Board, 0, MergedPositions).

% desenha a grade
draw_grid(Window) :-
    grid_size(N),
    End is N - 1,
    forall(between(0, End, Row),
           forall(between(0, End, Col),
                  draw_cell(Window, Row, Col))).

% desenha cada peça
draw_cell(Window, Row, Col) :-
    cell_size(S),
    X is Col * S,
    Y is Row * S,
    new(Box, box(S, S)),
    send(Box, fill_pattern, colour(white)),
    send(Box, pen, 1),
    send(Box, colour, black),
    send(Window, display, Box, point(X, Y)).

% desenha a linha com as peças
draw_row(_, [], _, _, _).
draw_row(Window, [Val|T], Row, Col, Merged) :-
    ( member((Row, Col), Merged) ->
        draw_tile(Window, Row, Col, Val, fused)
    ; draw_tile(Window, Row, Col, Val, none)
    ),
    NextCol is Col + 1,
    draw_row(Window, T, Row, NextCol, Merged).


% 'preenche' todas as peças
draw_tiles(_, [], _, _).
draw_tiles(Window, [Row|Rest], RowIndex, Merged) :-
    draw_row(Window, Row, RowIndex, 0, Merged),
    NextRow is RowIndex + 1,
    draw_tiles(Window, Rest, NextRow, Merged).


% caso base
draw_tile(_, _, _, 0, _) :- !.  % não desenha se for 0
% preenche uma peça
draw_tile(Window, Row, Col, Value) :-
    draw_tile(Window, Row, Col, Value, false).
% caso pro Value =:= none
draw_tile(Window, Row, Col, Value, none) :-
    cell_size(S),
    X is Col * S,
    Y is Row * S,
    tile_color(Value, Color),
    new(Box, box(S, S)),
    send(Box, fill_pattern, colour(Color)),
    send(Box, pen, 1),
    send(Box, colour, black),
    send(Window, display, Box, point(X, Y)),
    atom_string(Value, Str),
    new(Txt, text(Str)),
    send(Txt, font, font(helvetica, bold, 20)),
    Xtxt is X + S // 2 - 10,
    Ytxt is Y + S // 2 - 10,
    send(Window, display, Txt, point(Xtxt, Ytxt)).
% versão com animação pra os outros valores caso tenham se fundido
draw_tile(Window, Row, Col, Value, fused) :-
    Value > 0,  % só anima valores válidos
    cell_size(S),
    X is Col * S,
    Y is Row * S,
    tile_color(Value, Color),
    new(Box, box(S, S)),
    send(Box, fill_pattern, colour(Color)),
    send(Box, pen, 1),
    send(Box, colour, black),
    send(Window, display, Box, point(X, Y)),

    animate_grow(Box, X, Y, S + 20),  % chama a animação!

    atom_string(Value, Str),
    new(Txt, text(Str)),
    send(Txt, font, font(helvetica, bold, 20)),
    Xtxt is X + S // 2 - 10,
    Ytxt is Y + S // 2 - 10,
    send(Window, display, Txt, point(Xtxt, Ytxt)).


% faz a animação da peça crescendo aos poucos
animate_grow(Box, X, Y, FinalSize) :-
    Steps = 10,
    MaxSize is FinalSize,
    StepSize is MaxSize // Steps,
    animate_step(Box, X, Y, StepSize, 1, Steps).
    
% gera cada 'frame' da animação da peça crescendo
animate_step(_, _, _, _, N, Max) :- N > Max, !.
animate_step(Box, X, Y, StepSize, N, Max) :-
    Size is StepSize * N,
    send(Box, size, size(Size, Size)),
    Offset is (StepSize * Max - Size) // 2,
    send(Box, position, point(X + Offset, Y + Offset)),
    send(Box, flush),  % força update da interface
    sleep(0.01),       % pausa pequena pra dar o efeito de "frame"
    N1 is N + 1,
    animate_step(Box, X, Y, StepSize, N1, Max).

% 'salva' as peças mescladas
get_merged_positions(OldBoard, NewBoard, MergedPositions) :-
    findall((R, C),
        ( nth0(R, OldBoard, OldRow),
          nth0(R, NewBoard, NewRow),
          nth0(C, OldRow, OldVal),
          nth0(C, NewRow, NewVal),
          OldVal \= NewVal,
          NewVal \= 0,
          NewVal =:= OldVal * 2  % só se dobrou!
        ),
        MergedPositions).

% adicionando os frufrus
tile_color(0,    white).
tile_color(2,    '#eee4da').
tile_color(4,    '#ede0c8').
tile_color(8,    '#f2b179').
tile_color(16,   '#f59563').
tile_color(32,   '#f67c5f').
tile_color(64,   '#f65e3b').
tile_color(128,  '#edcf72').
tile_color(256,  '#edcc61').
tile_color(512,  '#edc850').
tile_color(1024, '#edc53f').
tile_color(2048, '#edc22e').
tile_color(_,    '#3c3a32').  % outros valores
