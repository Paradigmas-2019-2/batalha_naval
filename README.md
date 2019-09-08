# Batalha Naval

<p> O jogo <strong>Batalha Naval</strong> será desenvolvido utilizando a linguagem funcional Haskell, 
para a disciplina de Paradigmas de Programação UNB-2019-2.</p>

## Sobre o Jogo
##### Batalha Naval é um jogo de tabuleiro no qual o objetivo consiste em advinhar a posição do navio do jogador adversário, este projeto consiste em uma adaptação simplificada do jogo tradicional onde cada jogador posicionará apenas um navio.
### Regras: 
    (1) - É necessário ter dois jogadores.
    (2) - Ganha o jogador que acertar primeiro as três partes do navio adverdário.
    (3) - Cada jogador possui um navio e após o cadastro os jogadores devem posicioná-los no tabuleiro.
    (4) - O tamanho do tabuleiro é de 10x10 com coordenadas de A até J e de 0 até 9.
    (5) - O navio ocupará 3 posições consecutivas tanto na horizontal (H) como na vertical (V).
    (6) - O jogador deve indicar a posição e a orientação central do seu navio.
          Exemplo: 
            LETRA(A-J): D
            NUMERO(0-9): 5
            ORIENTACAO(H ou V): H.
          
    (7) - Para realizar o disparo no navio inimigo o jogador deve indicar as coordenadas compostas por uma letra(A-J) e um numero(0-9).
          Exemplo: 
            LETRA(A-J): C
            NUMERO(0-9): 5.
            
    (8) - Caso o disparo atinja o navio inimigo será exibido o caracter # na posição indicada, caso o disparo atinja a água será exibido o caracter '*'.
    (9) - SIMBOLOS E SEUS SIGNIFICADOS:
          '~' - Posição não revelada.
          '*' - Jogador acertou a água.
          '#' - Jogador acertou o navio inimigo.
    (10) - TECLAS: "
            Orientação vertical: A,B,C,D,E,F,G,H,I,J.
            Orientação horizontal: 0,1,2,3,4,5,6,7,8,9.
            Navio Orientação: V e H.
### Como utilizar o jogo
#### Requisitos
    1. Ter o compilador de Haskell em sua máquina, se não tiver instalado utilize o seguinte comando "sudo apt-get install haskell-platform"
    2. Ter o projeto clonado ou realizar download, para clonar o projeto utilize o seguinte comando "git clone https://github.com/Paradigmas-2019-2/batalha_naval.git"
#### Como rodar
    1. Pelo terminal entre no diretório que contenha os aquivos do projeto
    2. Rodar o GHCI pelo seguinte comando "ghci"
    3. Compile os arquivos com o comando ":l inicio.hs"
    4. Chame a funcão "inicio"
