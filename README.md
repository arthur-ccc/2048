# 2048

> Projeto desenvolvido para a disciplina de Paradigmas de Linguagens de Programação, ministrada pelo professor Ricardo Santos de Oliveira.

## DESCRIÇÃO
&nbsp;&nbsp;&nbsp;&nbsp;"2048" é um jogo em que peças numeradas estão dispostas em uma grade (matriz), e o objetivo é mesclar pares de peças até que uma alcance o valor 2048.<br>
&nbsp;&nbsp;&nbsp;&nbsp;Os movimentos possíveis são: direita, esquerda, cima e baixo. Em cada jogada, as peças deslizam na direção escolhida, e pares vizinhos com o mesmo valor se somam, formando uma nova peça. Peças com valores diferentes apenas se movem.<br>
&nbsp;&nbsp;&nbsp;&nbsp;Após cada movimento, uma nova peça surge aleatoriamente em um espaço vazio. O <font color="red">GAME OVER</font> acontece quando o tabuleiro está cheio e nenhuma fusão é possível.

## LISTA DE FUNCIONALIDADES:
|ID   |FUNCIONALIDADE                       |DESCRIÇÃO                                |
|-----| ----------------------------------- |-----------------------------------------|
|1    |Jogadas                              |O jogo basicamente gira em torno delas, onde cada estado da aplicação depende da jogada escolhida pelo jogador.<br>São elas: Direita, Esquerda, Cima, Baixo.|
|2    |Movimento                            |Uma jogada faz com que todas as peças do tabuleiro<br>se movam no sentido da jogada.|
|3    |Mescla                               |Ao fazer uma jogada, peças adjacentes de acordo com o sentido da jogada podem se mesclar, caso tenham o mesmo valor, em uma única peça que passa a ter a soma das duas que se somaram.<br>Detalhe: a mescla não ocorre indefinidamente, uma peça recém mesclada, não se mescla com outra novamente na mesma jogada.
|4    |Geração                              |A cada jogada surge uma nova peça em um espaço vazio com um valor potencialmente baixo.|
|5    |GAME OVER                            |O jogador dá um GAME OVER quando não existem espaços vazios no tabuleiro e peças vizinhas não podem mais se mesclar.|
|6    |Níveis de dificuldade                |Serão disponibilizados 3 níveis de dificuldade:<br>Fácil<br>Médio<br>Difícil<br>O que muda de um para o outro é apenas a dimensão do tabuleiro, 4x4(Fácil), 5x5(Médio), 6x6(Difícil).|
|7    |HUB inicial                          |Tela onde o jogador escolhe a dificuldade do jogo e poderá ver o ranking dos outros jogadores(caso seja implementado).|
|8    |Pontuação                            |Cada jogada pode gerar um acúmulo de pontos para o jogador. Você verá a sua pontuação ao fim do jogo.|
|EXTRA|Integração com SQLite<br>para ranking|Semelhante aos fliperamas, você pode deixar seu recorde registrado no história do jogo ou disputar pelo pódio no ranking. O SQLite seria usado para armazenar essas informações.|

## ANEXOS
```
Esboço simples do funcionamento do jogo:

+---+---+---+---+
| 2 | 4 |   |   |
+---+---+---+---+
|   | 2 | 4 |   |
+---+---+---+---+               ESTADO DO JOGO EM ALGUM MOMENTO
|   |   |   |   |
+---+---+---+---+
|   |   | 2 |   |
+---+---+---+---+

após JOGADA PARA DIREITA:

+---+---+---+---+
|   |   | 2 | 4 |
+---+---+---+---+
|   |   | 2 | 4 |
+---+---+---+---+		TODAS AS PEÇAS SE DESLOCAM E UMA NOVA SURGE
| 2 |   |   |   |				(SEM MESCLAS)
+---+---+---+---+
|   |   |   | 2 |
+---+---+---+---+

após JOGADA PARA CIMA:

+---+---+---+---+
| 2 | 2 | 4 | 8 |
+---+---+---+---+
|   |   |   | 2 |
+---+---+---+---+		TODAS AS PEÇAS SE DESLOCAM E UMA NOVA SURGE
|   |   |   |   |			(4s somados, 2s somados)
+---+---+---+---+
|   |   |   |   |
+---+---+---+---+

após JOGADA PARA A DIREITA:

+---+---+---+---+
|   | 4 | 4 | 8 |
+---+---+---+---+
|   |   |   | 2 |
+---+---+---+---+		TODAS AS PEÇAS SE DESLOCAM E UMA NOVA SURGE
|   |   |   |   |				(2s somados)
+---+---+---+---+
| 4 |   |   |   |
+---+---+---+---+
```