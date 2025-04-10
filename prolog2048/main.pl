:- use_module(library(pce)).
:- use_module(draw).

:- dynamic tabuleiro/1.
:- dynamic score/1.

cell_size(150). % tamanho das células
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
    retractall(tabuleiro(_)),          % limpa o tabuleiro
    retractall(score(_)),              % limpa score antigo
    assertz(score(0)),                 % score inicial
    initial_board(Board),              % chama o tabuleiro inicial
    assertz(tabuleiro(Board)),
    new(Window, game_window('2048')),
    send(Window, size, size(1920, 1080)),   % ajusta ao formato da tela
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
        valor_nova_peca(Board, R, C, 2, NewBoard)
    ; NewBoard = Board
    ).

valor_nova_peca(Board, R, C, Value, NewBoard) :-
    nth0(R, Board, Row),
    subistitue_peca_index(C, Row, Value, NewRow),
    subistitue_peca_index(R, Board, NewRow, NewBoard).

subistitue_peca_index(Idx, List, Elem, NewList) :-
    same_length(List, NewList),
    append(Before, [_|After], List),
    length(Before, Idx),
    append(Before, [Elem|After], NewList).

% move as peças de uma linha para a esquerda
mover_linha_esquerda(Row, NewRow, Points) :-
    exclude(=(0), Row, NoZeros),          % remove os zeros
    combine(NoZeros, Combined, Points),    % funde os valores iguais
    length(Row, N),
    length(Combined, L),
    M is N - L,
    length(Padding, M),
    maplist(=(0), Padding),
    append(Combined, Padding, NewRow).    % completa com zeros à direita

% move todas as linhas do tabuleiro para a esquerda
mover_esquerda(Board, NewBoard, Points) :-
    maplist(mover_linha_esquerda_acc, Board, NewBoard, PointList),
    sum_list(PointList, Points).

% helper pra maplist que aceita 3 args
mover_linha_esquerda_acc(Row, NewRow, Points) :-
    mover_linha_esquerda(Row, NewRow, Points).

% tranpõe a matriz para poder reaproveitar o a regra mover_esquerda
traspor([[]|_], []) :- !.
traspor(Matrix, [Row|Rest]) :-
    maplist(nth0(0), Matrix, Row),
    maplist(remove_head, Matrix, TailMatrix),
    traspor(TailMatrix, Rest).

remove_head([_|T], T).

% mover para direita
move_direita(Board, NewBoard, Points) :-
    maplist(reverse, Board, Rev),
    mover_esquerda(Rev, Moved, Points),
    maplist(reverse, Moved, NewBoard).

% mover para cima
move_up(Board, NewBoard, Points) :-
    traspor(Board, T),
    mover_esquerda(T, Moved, Points),
    traspor(Moved, NewBoard).
    
% mover para baixo
move_down(Board, NewBoard, Points) :-
    traspor(Board, T),
    move_direita(T, Moved, Points),
    traspor(Moved, NewBoard).

% mescla
combine([], [], 0).
combine([X], [X], 0).
combine([X, X | T], [Y | Rest], Score) :-
    Y is X + X,
    combine(T, Rest, RestScore),
    Score is RestScore + Y.
combine([X, Y | T], [X | Rest], Score) :-
    X \= Y,
    combine([Y | T], Rest, Score).

% pega os eventos do teclado, ou seja, setinhas
handle_key(Window, Key) :-
    tabuleiro(OldBoard),
    move_board(Key, OldBoard, MovedBoard, Points),
    ( MovedBoard \= OldBoard ->
        place_random_piece(MovedBoard, NewBoard),
        update_score(Points),
        get_merged_positions(OldBoard, MovedBoard, Merged), 
        retractall(tabuleiro(_)),
        assertz(tabuleiro(NewBoard)),
        send(Window, clear),
        draw_board(Window, Merged),
        (game_over(NewBoard) -> 
        desenha_game_over(Window)
        ; true)
    ; true).

game_over(Board) :-
    \+ (mover_esquerda(Board, NewBoard, _), Board \= NewBoard),
    \+ (move_direita(Board, NewBoard, _), Board \= NewBoard),
    \+ (move_up(Board, NewBoard, _), Board \= NewBoard),
    \+ (move_down(Board, NewBoard, _), Board \= NewBoard).

% logica de movimento
move_board(cursor_left,  B, NB, P) :- mover_esquerda(B, NB, P).
move_board(cursor_right, B, NB, P) :- move_direita(B, NB, P).
move_board(cursor_up,    B, NB, P) :- move_up(B, NB, P).
move_board(cursor_down,  B, NB, P) :- move_down(B, NB, P).

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

% Atualiza a pontuacao
update_score(Points) :-
    score(Old),
    New is Old + Points,
    retractall(score(_)),
    assertz(score(New)).

restart_game :-
    get(@display, frame, Window),
    send(Window, clear),
    start.  % reinicia o jogo

:- initialization(start).