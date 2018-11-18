# Cabeçalho
  > __(Igual ao relatório intercalar)__
# 1-Introdução
  This project's objective is for us to implement a board game (for us is Pente) using Prolog, with 3 different modes: Player vs Player, Player vs Bot and Bot vs Bot, with the Bot having 2 different difficulties.
  It is needed for us to implement all the game's rules and use the best suiting compounds to represent the game, with the board, pieces, player's turn, etc...
  This will all be discussed below.
  
# 2- O Jogo XXX
  > __(Tópicos 1 e 2 do relatório intercalar)__
  
# 3- Lógica do jogo
## 3.1- Representação do Estado do Jogo
  Representing the game’s board is fairly simple. Every position in the SizexSize board is in one of three states: white piece, black piece, or empty. We’ll represent the board using a SizexSize matrix (list of lists), whose elements are w, b or c, for each state respectively. This matrix will be called Board.
  Now, each player has captured a certain number of pieces (an integer) and only plays pieces of a certain color (white or black). These will be Wc or Bc, representing the white player captures and the black player captures, respectively.
  The overall game state will be represented by a game/5 compound. This compound is game(Board, P, [Wc, Bc], Turn, Options), where P is w or b according to who will play next, Turn is an integer thst represents the current game turn number, starting at 0. Lastly, Options is a list that represents the game's options. These option include (use 'help' to get same information): 

     - board_size(S) or size(S), S in {7,9,11,...}
      Defines the board size (SxS), must be odd.
      Default: 19

    - difficulty(D), D in {1,2,3,4,5}
       Sets the bot difficulty (with 5 being the most
      difficult) by selecting a set of predefined values
      for depth, padding and width. If given values for
      depth, padding and width, these override those selected
      by the difficulty.
      Default: 3

    - depth(D), D in {0,1,2,3,...}
       Depth of the bot search tree.
       Suggestion: Choose an even depth (2 or 4).
      Default (difficulty 3): 5

    - padding(P), P in {0,1,2,3,...}
       Padding of the active subboard used by the search tree.
      Default (difficulty 3): 2

    - width([W0,W1,...]), Wi in {1,2,3,...}
       For each search node on depth i (top is depth 0)
       recurse the search tree only for the Wi best moves.
       If the width is not defined for all the depths, the
       last width in the list will be used repeatedly.
    - width(W), W in {1,2,3,...}
       Use the width W for all depths.
      Default (difficulty 3): [4,3,2]

    - flip_board(B) or flip(B), B is true or false
       Flip the board print on the console for Blacks turn.
      Default: false

    - tournament_rule(B) or rule(B), B is true or false
       Use the tournament rule.
      Default: true

They can be set by the player when starting the game with play, like the example below.
 Example: play([size(13), difficulty(1), rule(false)])
If an option is not given the default value will be used.

## 3.2- Visualização do Tabuleiro
  If the user decide to flip the board (display only, not representation) we will do it when it is Black’s turn to play, as if the two players were playing face-to-face on a physical Goboard. Naturally we adjust the identification of rows and columns. To draw the board with text (on the console) we used unicode box-drawing characters (range u+2500–u+257f). The white and black pieces become filled and empty unicode circles, respectively.

  > __(atualizar imagens devido à forma como são chamadas)__
  > __(adicionar imagem 13x13)__
  > __(adicionar imagem com a comentação da função display_game)__

## 3.3- Lista de Jogadas Válidas
  In pente, the player's possible moves does not depend on which player is going to play. The available moves are the same for each one.
  With this being said we implemented several predicates that give us a list with all the possible moves a player can make.
  
  > __(imagem valid_moves/2)__
  
  This gives the player all the empty positions on the board, does not take in consideration the game's turn or if the user wants to play using the tournament rule.
  
  > __(imagem valid_moves/3)__
  
  Being more especific than valid_moves/2 it takes a given turn making sure that at the turn number 0 the only available move for the player is the center piece.
  By default it uses the Tournament rule at turn 2.
  
 >  __(imagem valid_moves/4)__
 
  Takes into account the game turn and if the tournament rule is active or not. At turn 0 the only valid move is still the center one, and at turn 2 if the Tournament rule is being used the valid moves are the empty positions at least 3 intersections away from the center. 

## 3.4- Execução de Jogadas
  When a player makes a move it needs to be checked. This analisis is made seeing if the player move is in the list of valid_moves returned from the predicates described above.
  In the case that the player move is valid we need to do some things: 
  1- Place the player's corresponding colored stone at the given game coords.
  2 - Check if that stone bracketed the opponent. If yes add 2 captures to the player's captures and remove those 2 stones from the board.
  3- Update the next player playing (from w to b and from b to w).
  4- Increment the game's turn by 1.
  
  This is all done in move/3.
  > __(imagem move/3)__

## 3.5- Final do Jogo
  The are two winning conditions: a) capture 10 pieces, b) make a row of 5 consecutive friendly pieces in any direction.
  In the end of every game iteration tests are made to see if any player (w or b) passes and of the winning condition. If so a victory message is displayed for that player and the game cycle is stopped, finishing the game.
  
  The winning conditions are checked in game_over/2 where you give a Game and the player to check for.
  > __(imagem game_over/2)__ 

  The end iteration check is done in the following piece of code.
 >  __(image game_loop_aux/2)__

## 3.6- Avaliação do Tabuleiro
  The game evalution is done based on patterns. All the possible patterns are defined with a corresponding value (base 2) to each one (bigger value = better pattern).
  A pattern value is not only defined by its value but also by the sum of all the inner pattern in it.
 ========= corrige isto ==============
  When evaluating the game all the patterns presents in it are added (the opponents patterns have a complementary value) and considers both player captures.
 =====================================
> __(imagem value/2)__

## 3.7- Jogada do Computador
Vou deixar isto para ti 😅
falar das optimizacoes, N melhores filhos, P posicoes dentro do padding, a diferença do evaluate_board e do reevaluate_board, etc...

# 4- Conclusões
🙃🙃🙃🙃🙃🙃🙃 ==== help here ==== 🙃🙃🙃🙃🙃🙃🙃
This project required a lot of work but yet rewarding.
We achieved everything that we stipulated from the beggining and even more. In our opinion we took what was asked for and elevated it to another level giving the player a lot more options to customize both bot and the game.
The biggest difficulties when developing this game was, definitely, the making of the Bot's evaluation tree. Although, we conquered this and are pretty happy with how it turned out.
The one thing that we would like to improve are the values for each pattern, in our opinion these aren't perfectly tuned and would require more time to perfect these.
In conclusion, with Prolog being a language that we are not used to use we are really proud with what we accomplished.

 
# Bibliografia
__(igual ao relatório intercalar)__

