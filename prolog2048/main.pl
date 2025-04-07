:- use_module(library(pce)).
:- dynamic tabuleiro/1.

% Configurações dinâmicas
:- dynamic cell_size/1, window_size/1, grid_size/1, min_grid_size/1.
cell_size(80). % Tamanho base de cada célula
grid_size(4).  % Tamanho inicial da grade
min_grid_size(4). % Tamanho minimo da grade
window_size(size(600, 600)). % Tamanho inicial da janela

% Classe principal da janela do jogo
:- pce_begin_class(game_window, picture).

% Inicializa a janela do jogo
initialise(Window, Name:name) :->
    send_super(Window, initialise, Name), % Chama o inicializador da superclasse
    send(Window, kind, top_level), % Define como janela de nível superior
    send(Window, recogniser, new(resize_gesture)), % Permite redimensionamento
    send(Window, size, size(600, 600)), % Define tamanho inicial
    send(Window, open). % Abre a janela

% Trata redimensionamento da janela
resize(Window) :->
    get(Window, size, NewSize), % Obtém novo tamanho
    retractall(window_size(_)), % Remove tamanho antigo
    asserta(window_size(NewSize)), % Armazena novo tamanho
    adjust_grid_and_cell_size(Window), % Ajusta grade e células
    (tabuleiro(Board) -> % Se existir tabuleiro
        send(Window, clear), % Limpa janela
        draw_board(Window, Board, []) % Redesenha tabuleiro
    ; true).

% Trata eventos (teclado e redimensionamento)
event(Window, Ev:event) :->
    (get(Ev, kind, resize) -> % Se for evento de redimensionamento
        send(Window, resize) % Redimensiona
    ; get(Ev, key, Key), % Se for evento de teclado
      (member(Key, [cursor_left, cursor_right, cursor_up, cursor_down]) -> % Tecla de seta
          handle_key(Window, Key) % Trata movimento
      ; Key == 27 ->  % Tecla ESC
          send(Window, destroy) % Fecha janela
      ; send_super(Window, event, Ev) % Outros eventos para superclasse
      ).

:- pce_end_class.

% Ajusta tamanho da grade e células baseado no tamanho da janela
adjust_grid_and_cell_size(_) :-
    window_size(size(W, H)), % Obtém tamanho atual
    MinDimension is min(W, H), % Pega menor dimensão
    min_grid_size(MinGrid), % Obtém tamanho minimo
    % Calcula novo tamanho da grade (minimo 4x4)
    NewGridSize is max(MinGrid, MinDimension // 150),
    retractall(grid_size(_)), % Remove tamanho antigo
    asserta(grid_size(NewGridSize)), % Armazena novo tamanho
    % Calcula novo tamanho das células
    NewCellSize is (MinDimension - 40) // NewGridSize,
    retractall(cell_size(_)), % Remove tamanho antigo
    asserta(cell_size(NewCellSize)). % Armazena novo tamanho

% Inicia o jogo
start :-
    retractall(tabuleiro(_)), % Limpa tabuleiro anterior
    initial_board(Board), % Cria novo tabuleiro
    assertz(tabuleiro(Board)), % Armazena tabuleiro
    new(Window, game_window('2048 Game')), % Cria nova janela
    adjust_grid_and_cell_size(Window), % Ajusta tamanhos
    draw_board(Window, Board, []). % Desenha tabuleiro

% Inicializa tabuleiro
initial_board(Board) :-
    empty_board(Empty), % Cria tabuleiro vazio
    place_random_piece(Empty, Board). % Adiciona peça aleatória

% Cria tabuleiro vazio
empty_board(Board) :-
    grid_size(N), % Obtém tamanho da grade
    length(Row, N), maplist(=(0), Row), % Cria linha vazia
    length(Board, N), maplist(=(Row), Board). % Cria grade com linhas vazias

% Coloca peça aleatória (valor 2) no tabuleiro
place_random_piece(Board, NewBoard) :-
    % Encontra todas as células vazias
    findall((R,C),
        (nth0(R, Board, Row), nth0(C, Row, 0)),
        EmptyCells),
    (EmptyCells \= [] -> % Se houver células vazias
        random_member((R,C), EmptyCells), % Escolhe aleatoriamente
        set_cell(Board, R, C, 2, NewBoard) % Coloca valor 2 na célula
    ; NewBoard = Board % Se não houver células vazias, mantém tabuleiro
    ).

% Define valor de uma célula
set_cell(Board, R, C, Value, NewBoard) :-
    nth0(R, Board, Row), % Obtém linha
    replace_nth(C, Row, Value, NewRow), % Substitui valor na linha
    replace_nth(R, Board, NewRow, NewBoard). % Substitui linha no tabuleiro

% Substitui elemento em uma posição da lista
replace_nth(Idx, List, Elem, NewList) :-
    same_length(List, NewList), % Garante mesmo tamanho
    append(Before, [_|After], List), % Divide lista
    length(Before, Idx), % Verifica posição
    append(Before, [Elem|After], NewList). % Reconstrói lista com novo elemento

% Lógica de movimento para esquerda
move_row_left(Row, NewRow) :-
    exclude(=(0), Row, NoZeros), % Remove zeros
    combine(NoZeros, Combined), % Combina valores iguais
    length(Row, N), % Tamanho original
    length(Combined, L), % Tamanho após combinação
    M is N - L, % Número de zeros para adicionar
    length(Padding, M), % Cria lista de zeros
    maplist(=(0), Padding), % Preenche com zeros
    append(Combined, Padding, NewRow). % Junta tudo

% Move todo o tabuleiro para esquerda
move_left(Board, NewBoard) :-
    maplist(move_row_left, Board, NewBoard).

% Transposição para movimentos verticais
transpose([[]|_], []) :- !.
transpose(Matrix, [Row|Rest]) :-
    maplist(nth0(0), Matrix, Row), % Pega primeiro elemento de cada linha
    maplist(remove_head, Matrix, TailMatrix), % Remove primeiro elemento
    transpose(TailMatrix, Rest). % Transpõe o resto

remove_head([_|T], T). % Remove primeiro elemento

% Move para direita (usando movimento para esquerda)
move_right(Board, NewBoard) :-
    maplist(reverse, Board, Rev), % Inverte linhas
    move_left(Rev, Moved), % Move para esquerda
    maplist(reverse, Moved, NewBoard). % Reverte novamente

% Move para cima (usando transposição)
move_up(Board, NewBoard) :-
    transpose(Board, T), % Transpõe
    move_left(T, Moved), % Move para esquerda
    transpose(Moved, NewBoard). % Transpõe de volta

% Move para baixo (usando transposição)
move_down(Board, NewBoard) :-
    transpose(Board, T), % Transpõe
    move_right(T, Moved), % Move para direita
    transpose(Moved, NewBoard). % Transpõe de volta

% Combina peças adjacentes iguais
combine([], []).
combine([X], [X]).
combine([X,X|T], [Y|Rest]) :-
    Y is X + X, % Dobra o valor
    combine(T, Rest).
combine([X,Y|T], [X|Rest]) :-
    X \= Y, % Valores diferentes
    combine([Y|T], Rest).

% Funções de desenho
draw_board(Window, Board, MergedPositions) :-
    get_offsets(Window, XOffset, YOffset), % Calcula offsets para centralizar
    draw_grid(Window, XOffset, YOffset), % Desenha grade
    draw_tiles(Window, Board, 0, MergedPositions, XOffset, YOffset). % Desenha peças

% Calcula posição para centralizar o tabuleiro
get_offsets(Window, XOffset, YOffset) :-
    cell_size(S), % Tamanho da célula
    grid_size(G), % Tamanho da grade
    TotalSize is S * G, % Tamanho total
    get(Window, size, size(W, H)), % Tamanho da janela
    XOffset is (W - TotalSize) // 2, % Offset X para centralizar
    YOffset is (H - TotalSize) // 2. % Offset Y para centralizar

% Desenha a grade vazia
draw_grid(Window, XOffset, YOffset) :-
    grid_size(G),
    End is G - 1,
    forall(between(0, End, Row), % Para cada linha
           forall(between(0, End, Col), % Para cada coluna
                  draw_cell(Window, Row, Col, XOffset, YOffset))). % Desenha célula

% Desenha uma célula vazia
draw_cell(Window, Row, Col, XOffset, YOffset) :-
    cell_size(S),
    X is Col * S + XOffset, % Posição X
    Y is Row * S + YOffset, % Posição Y
    new(Box, box(S, S)), % Cria caixa
    send(Box, fill_pattern, colour(white)), % Preenchimento branco
    send(Box, pen, 1), % Borda preta
    send(Box, colour, black),
    send(Window, display, Box, point(X, Y)). % Mostra na janela

% Desenha uma linha de peças
draw_row(_, [], _, _, _, _, _).
draw_row(Window, [Val|T], Row, Col, Merged, XOffset, YOffset) :-
    (member((Row, Col), Merged) -> % Se foi combinada
        draw_tile(Window, Row, Col, Val, fused, XOffset, YOffset) % Desenha com animação
    ; draw_tile(Window, Row, Col, Val, none, XOffset, YOffset)), % Desenha normal
    NextCol is Col + 1,
    draw_row(Window, T, Row, NextCol, Merged, XOffset, YOffset).

% Desenha todas as peças do tabuleiro
draw_tiles(_, [], _, _, _, _).
draw_tiles(Window, [Row|Rest], RowIndex, Merged, XOffset, YOffset) :-
    draw_row(Window, Row, RowIndex, 0, Merged, XOffset, YOffset), % Desenha linha
    NextRow is RowIndex + 1,
    draw_tiles(Window, Rest, NextRow, Merged, XOffset, YOffset).

% Desenha célula vazia (especialização)
draw_tile(Window, Row, Col, 0, _, XOffset, YOffset) :- !,
    cell_size(S),
    X is Col * S + XOffset,
    Y is Row * S + YOffset,
    new(Box, box(S, S)),
    send(Box, fill_pattern, colour(white)),
    send(Box, pen, 1),
    send(Box, colour, black),
    send(Window, display, Box, point(X, Y)).

% Desenha peça com valor
draw_tile(Window, Row, Col, Value, Anim, XOffset, YOffset) :-
    Value > 0,
    cell_size(S),
    X is Col * S + XOffset,
    Y is Row * S + YOffset,
    tile_color(Value, Color), % Obtém cor baseada no valor
    new(Box, box(S, S)),
    send(Box, fill_pattern, colour(Color)), % Preenche com cor
    send(Box, pen, 1),
    send(Box, colour, black),
    send(Window, display, Box, point(X, Y)),
    (Anim == fused -> animate_grow(Box, X, Y, S) ; true), % Animação se combinada
    % Tamanho de fonte escalável
    ( Value >= 1000 -> FontSize is max(10, S // 4)
    ; Value >= 100  -> FontSize is max(12, S // 3)
    ;                 FontSize is max(14, S // 2)
    ),
    atom_string(Value, Str),
    new(Txt, text(Str)),
    send(Txt, font, font(helvetica, bold, FontSize)), % Define fonte
    get(Txt, size, size(TW, TH)),
    Xtxt is X + (S - TW) // 2, % Centraliza texto
    Ytxt is Y + (S - TH) // 2,
    send(Window, display, Txt, point(Xtxt, Ytxt)).

% Animação para peças combinadas
animate_grow(Box, X, Y, CellSize) :-
    Steps = 5,
    InitialSize is CellSize // 2, % Começa pequeno
    FinalSize is CellSize, % Tamanho final
    animate_step(Box, X, Y, InitialSize, FinalSize, Steps).

animate_step(Box, X, Y, CurrentSize, FinalSize, StepsLeft) :-
    StepsLeft > 0,
    StepSize is (FinalSize - CurrentSize) // StepsLeft,
    NewSize is CurrentSize + StepSize,
    Offset is (FinalSize - NewSize) // 2, % Calcula offset para centralizar
    send(Box, size, size(NewSize, NewSize)), % Redimensiona
    send(Box, position, point(X + Offset, Y + Offset)), % Reposiciona
    send(Box, flush),
    sleep(0.05), % Pequena pausa
    NextSteps is StepsLeft - 1,
    animate_step(Box, X, Y, NewSize, FinalSize, NextSteps).
animate_step(_, _, _, _, _, 0).

% Tratamento de entrada
handle_key(Window, Key) :-
    tabuleiro(OldBoard), % Obtém tabuleiro atual
    move_board(Key, OldBoard, MovedBoard), % Move conforme tecla
    (MovedBoard \= OldBoard -> % Se houve movimento
        place_random_piece(MovedBoard, NewBoard), % Adiciona nova peça
        get_merged_positions(OldBoard, MovedBoard, Merged), % Obtém posições combinadas
        retractall(tabuleiro(_)), % Remove tabuleiro antigo
        assertz(tabuleiro(NewBoard)), % Armazena novo tabuleiro
        send(Window, clear), % Limpa janela
        draw_board(Window, NewBoard, Merged) % Redesenha
    ; true). % Se não houve movimento, não faz nada

% Mapeia teclas para movimentos
move_board(cursor_left, B, NB) :- move_left(B, NB).
move_board(cursor_right, B, NB) :- move_right(B, NB).
move_board(cursor_up, B, NB) :- move_up(B, NB).
move_board(cursor_down, B, NB) :- move_down(B, NB).

% Encontra posições onde peças foram combinadas
get_merged_positions(OldBoard, NewBoard, MergedPositions) :-
    findall((R, C),
        (nth0(R, OldBoard, OldRow),
         nth0(R, NewBoard, NewRow),
         nth0(C, OldRow, OldVal),
         nth0(C, NewRow, NewVal),
         OldVal \= NewVal, % Valor mudou
         NewVal \= 0, % Não é zero
         NewVal =:= OldVal * 2), % Novo valor é o dobro
        MergedPositions).

% Cores das peças baseadas no valor
tile_color(0,    white).        % Vazio
tile_color(2,    '#eee4da').    % 2
tile_color(4,    '#ede0c8').    % 4
tile_color(8,    '#f2b179').    % 8
tile_color(16,   '#f59563').    % 16
tile_color(32,   '#f67c5f').    % 32
tile_color(64,   '#f65e3b').    % 64
tile_color(128,  '#edcf72').    % 128
tile_color(256,  '#edcc61').    % 256
tile_color(512,  '#edc850').    % 512
tile_color(1024, '#edc53f').    % 1024
tile_color(2048, '#edc22e').    % 2048
tile_color(_,    '#3c3a32').    % Outros valores

% Inicia o jogo automaticamente
:- initialization(start).