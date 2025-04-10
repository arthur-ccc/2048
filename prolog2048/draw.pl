:- module(draw, [draw_board/2, draw_grid/1, draw_cell/3, draw_row/5, draw_tile/5, draw_tiles/4, draw_score/1, desenha_game_over/1,tile_color/2]).

% desenhao tabuleiro
draw_board(Window, MergedPositions) :-
    tabuleiro(Board),
    draw_score(Window),
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
    grid_size(N),
    TotalSize is N * S,                % Tamanho total do tabuleiro (N x N células)
    WinWidth = 1920, WinHeight = 1080, % Dimensões da janela
    OffsetX is (WinWidth - TotalSize) // 2,  % Offset para centralizar horizontalmente
    OffsetY is (WinHeight - TotalSize) // 2, % Offset para centralizar verticalmente
    X is OffsetX + Col * S,            % Posição X com offset
    Y is OffsetY + Row * S,            % Posição Y com offset
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

% desenha a pontuacao
draw_score(Window) :-
    score(Score),
    cell_size(S),
    grid_size(N),
    TotalSize is N * S,
    WinWidth = 1920,
    OffsetX is (WinWidth - TotalSize) // 2,
    OffsetY is (1080 - TotalSize) // 2,
    ScoreY is OffsetY - 50,  % Posiciona 50 pixels acima do tabuleiro
    format(atom(ScoreText), 'Score: ~w', [Score]),
    new(Text, text(ScoreText)),
    send(Text, font, font(helvetica, bold, 20)),
    send(Window, display, Text, point(OffsetX, ScoreY)).  % Alinha com o tabuleiro

% caso base
draw_tile(_, _, _, 0, _) :- !.  % não desenha se for 0
% caso pro Value =:= none
draw_tile(Window, Row, Col, Value, _) :-
    cell_size(S),
    grid_size(N),
    TotalSize is N * S,
    WinWidth = 1920, WinHeight = 1080,
    OffsetX is (WinWidth - TotalSize) // 2,
    OffsetY is (WinHeight - TotalSize) // 2,
    X is OffsetX + Col * S + 2,       % +2 para margem interna
    Y is OffsetY + Row * S + 2,
    EffectiveSize is S - 4,
    tile_color(Value, Color),
    new(Box, box(EffectiveSize, EffectiveSize)),
    send(Box, fill_pattern, colour(Color)),
    send(Box, pen, 1),
    send(Box, colour, black),
    send(Window, display, Box, point(X, Y)),
    (Value \= 0 ->
        atom_string(Value, Str),
        new(Txt, text(Str)),
        send(Txt, font, font(helvetica, bold, 20)),
        Xtxt is X + EffectiveSize // 2 - 10,
        Ytxt is Y + EffectiveSize // 2 - 10,
        send(Window, display, Txt, point(Xtxt, Ytxt))
    ; true).

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
% preenche uma peça
draw_tile(Window, Row, Col, Value) :-
    draw_tile(Window, Row, Col, Value, false).

% desenha a tela de GAME OVER
desenha_game_over(Picture) :-
    send(Picture, clear),
    get(Picture, width, Width),
    get(Picture, height, Height),

    new(Fundo, box(Width, Height)),
    send(Fundo, colour, grey),
    send(Picture, display, Fundo, point(0, 0)),

    new(Texto, text('Game Over')),
    send(Texto, font, font(helvetica, bold, 40)),
    get(Texto, width, TW),
    get(Texto, height, TH),
    TX is (Width - TW) // 2,
    TY is (Height - TH) // 2 - 40,
    send(Picture, display, Texto, point(TX, TY)),

    new(Botao, button('Reiniciar', message(@prolog, restart_game))),
    get(Botao, width, BW),
    BX is (Width - BW) // 2,
    BY is TY + 60,
    send(Picture, display, Botao, point(BX, BY)).

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
