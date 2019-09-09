module Biblioteca (
    menu,
    defineOpcao,
    rodada,
    jogo,
    executarJogo,
    vencedor,
    imprimeRegras,
    imprimeDupla,
    validaDisparo,
    converteEntradas,
    verificaDisparo,
    atualizaCampo,
    atualizaTabela,
    armazenaPosicaoNavio,
    horizontal,
    vertical,
    imprimeMar,
    removePosicao,

)where

import System.IO
import System.Process
import Data.Char
import System.Random
import Data.Function
import Data.List
import Control.Exception



type Jogadores = [Jogador]
type Navio = [Int]
type Pontos = Int
type Nome = String
type Vez = Int
data Jogador =  Jogador Navio Pontos Vez
                    deriving (Show, Read)

mar = replicate 100 '~'
-- menu para inicio
menu::Jogadores ->IO Jogadores
menu dados = do
    system "clear"
    putStrLn "----------------------------Batalha Naval----------------------------"
    putStrLn "\nDigite 1 Jogar: "
    putStrLn "\nDigite 2 ver o Histórico: "
    putStrLn "\nDigite 0 para sair: "
    putStrLn "\n-------------------------------------------------------------------"
    opcao <- getChar
    getChar
    defineOpcao dados opcao

-- função para o usuario decidir opções do menu
defineOpcao :: Jogadores->Char->IO Jogadores
defineOpcao dados '1' = rodada dados
defineOpcao dados '2' = do 
                            
                            history <- readFile "historico.txt"
                            putStr(history)
                            putStr "\n Precione enter para voltar ao menu"
                           
                            getChar
                            menu dados
defineOpcao dados '0' = do
                            putStrLn "FIM"
                            return dados

defineOpcao dados _ = do            
                            putStrLn ("\n Opção invalida")
                            
                            getChar
                            menu dados

-- LER O ARQUIVO HISTORICO
historico:: IO ()
historico = do  
            
            
            --getChar
            return ()

-- INICIA UMA RODADA SALVADO OS JOGADORES
rodada::Jogadores ->IO Jogadores
rodada dados = do
    system "clear"
    putStr "Jogador 1 digite seu nome: "
    jogador1 <- getLine
    system "clear"
    putStr "Jogador 2 digite seu nome: "
    jogador2 <- getLine 
    jogo dados jogador1 jogador2

--INICIA O JOGO 
jogo::Jogadores -> Nome ->Nome ->IO Jogadores
jogo dados jogador1 jogador2 = do
    system "clear"
    putStrLn ("---------------------" ++ jogador1 ++ " X " ++ jogador2 ++ "---------------------")
    ---adcionar as regras do jogo e como funciona
    imprimeRegras
    putStrLn "precione Enter para pular ..."
    getChar
    system "clear"
    let oceano = mar
    --Deve fazer o posicionamento dos navios agora
    putStrLn (show(jogador1)++" posicione seu navio (NAO DEIXE O ADVERSARIO VER): ")
    putStr "Letra(A-J): "
    y1 <- getChar
    getChar
    putStr "Numero(0-9): "
    x1 <- getChar
    getChar
    putStr "Orientacao(H ou V): "
    z1 <- getChar
    getChar
    let res1 = validaDisparo x1 y1 z1
    if not(res1) then do
        putStrLn "ENTRADA(S) INVALIDAS.....\nPRESSIONE ENTER PARA CONTINUAR....."
        getChar
        jogo dados jogador1 jogador2
    else do
        system "clear"
        putStrLn (show(jogador2)++" posicione seu navio (NAO DEIXE O ADVERSARIO VER): ")
        putStr "Letra(A-J): "
        y2 <- getChar
        getChar
        putStr "Numero(0-9): "
        x2 <- getChar
        getChar
        putStr "orientacao(H ou V): "
        z2 <- getChar
        getChar
        if not(res1 == validaDisparo y2 x2 z2) then do
            system "clear"
            putStrLn "ENTRADA(S) INVALIDAS.....\nPRESSIONE ENTER PARA CONTINUAR....."
            getChar
            jogo dados jogador1 jogador2
        else do
            ---SE CHEGOU AQUI AS ENTRADAS SÃO VALIDAS
            --converte as entradas em um int para usar a função armazenaNavio
            let a = ((((converteEntradas y1)-65)*10)+((converteEntradas x1)-48))
            let navio1 = armazenaPosicaoNavio a z1
            let b = ((((converteEntradas y2)-65)*10)+((converteEntradas x2)-48))
            let navio2 = armazenaPosicaoNavio b z2
            ---FALTA VARIFICAR SE ALGUM NAVIO É [] SE FOR CHAME A FUNÇÃO JOGO NOVAMENTE
            executarJogo dados oceano oceano jogador1 jogador2 navio1 navio2 1
        getChar
        menu dados

    menu dados
    --executarJogo dados mar jogador1 jogador2 1

--função responsavel por coordenar as jogadas
executarJogo::Jogadores->[Char]->[Char]->Nome->Nome->Navio->Navio->Vez->IO Jogadores
executarJogo dados oceano oceano2 jogador1 jogador2 navio1 navio2 vez = do
        --VERIFICAR SE ALGUM JOGADOR VENCEU
        print navio1
        print navio2
        getChar
        if (navio1 == []) then do
            system "clear"
            vencedor jogador2 jogador1
            putStrLn "Precione ENTER para sair ...."
            getChar
            menu dados 
        else do
            if(navio2 == []) then do
                system "clear"
                vencedor jogador1 jogador2
                putStrLn "Precione ENTER para sair ...."
                getChar
                menu dados
            else do 
                --SE CHEGOU AQ É QUE O JOGO NÃO TERMINOU HORA DE VERIFICAR DE QUEM É A VEZ
                if (vez == 1 ) then do
                    system "clear"
                    putStrLn ("Vez do " ++ jogador1 ++ "...")
                    imprimeMar oceano2
                    putStrLn "\nDISPARO....."
                    putStr "LETRA(A-J): "
                    tentativay <- getChar
                    getChar
                    putStr "NUMERO(0-9): "
                    tentativax <- getChar
                    getChar
                    let o = validaDisparo tentativay tentativax 'A'
                    if (o ==False) then do
                        putStrLn ("\nEntrada invalida")
                        putStrLn "Pressione ENTER para jogar novamente..."
                        getChar
                        executarJogo dados oceano oceano2 jogador1 jogador2 navio1 navio2 1
                        putStrLn ("\n")
                        
                    else do {
                        putStrLn ("certo")   
                    }
                    ----VERIFICA SE A JÁ ACONTECEU O DISPARO
                    let a2= ((((converteEntradas tentativay)-65)*10)+((converteEntradas tentativax)-48))
                    let resultadodisparo = verificaDisparo a2 oceano2
                    if ((resultadodisparo == 1) || (resultadodisparo == 10)) then do
                        system "clear"
                        putStrLn "Já foi efetuado disparo nessa posição.Por favor tente outra...."
                        putStrLn "precione ENTER para continuar...."
                        getChar
                        executarJogo dados oceano oceano2 jogador1 jogador2 navio1 navio2 1
                    else do
                        ---SE CHEGOU AQ QUER DIZER QUE AINDA NÃO FOI DISPARADO NO CAMPO PODE PROCEGUIR
                        --VERIFICAR SE ACERTOU AGUA OU NAVIO
                        let ver = verificaDisparo a2 oceano2
                        if(elem a2 navio2) then do
                            system "clear"
                            
                            putStrLn "ACERTOU O NAVIO..."
                            let novomar = atualizaCampo a2 '#'oceano2
                            imprimeMar novomar
                            putStrLn "Precione ENTER ..."
                            let novonavio =removePosicao a2 navio2
                            getChar
                            --passa a vez para o outro jogador
                            executarJogo dados oceano novomar jogador1 jogador2 navio1 novonavio 1
                        else do 
                            ---ACERTOU A AGUA
                            system "clear"
                            putStrLn "AGUAAAAA....."
                            let novomar = atualizaCampo a2 '*'oceano2
                            imprimeMar novomar
                            putStrLn "Precione ENTER ..."
                            getChar
                            --passa a vez para o outro jogador
                            executarJogo dados oceano novomar jogador1 jogador2 navio1 navio2 2

                        getChar
                        menu dados
                    getChar
                    menu dados
                else do
                    system "clear"
                    putStrLn ("Vez do " ++ jogador2 ++ "...")
                    imprimeMar oceano
                    putStrLn "\nDISPARO....."
                    putStr "LETRA(A-J): "
                    tentativay <- getChar
                    getChar
                    putStr "NUMERO(0-9): "
                    tentativax <- getChar
                    getChar
                    let o = validaDisparo tentativay tentativax 'A'
                    if (o ==False) then do
                        putStrLn ("\nEntrada invalida")
                        putStrLn "Pressione ENTER para jogar novamente..."
                        getChar
                        executarJogo dados oceano oceano2 jogador1 jogador2 navio1 navio2 2
                        putStrLn ("\n")
                        
                    else do {
                        putStrLn ("certo")   
                    }
                    ----VERIFICA SE A JÁ ACONTECEU O DISPARO
                    let a2= ((((converteEntradas tentativay)-65)*10)+((converteEntradas tentativax)-48))
                    let resultadodisparo = verificaDisparo a2 oceano
                    if ((resultadodisparo == 1) || (resultadodisparo == 10)) then do
                        system "clear"
                        putStrLn "Já foi efetuado disparo nessa posição.Por favor tente outra...."
                        putStrLn "precione ENTER para continuar...."
                        getChar
                        executarJogo dados oceano oceano2 jogador1 jogador2 navio1 navio2 2
                    else do
                        ---SE CHEGOU AQ QUER DIZER QUE AINDA NÃO FOI DISPARADO NO CAMPO PODE PROCEGUIR
                        --VERIFICAR SE ACERTOU AGUA OU NAVIO
                        let ver = verificaDisparo a2 oceano
                        if(elem a2 navio1) then do
                            system "clear"
                            
                            putStrLn "ACERTOU O NAVIO..."
                            let novomar = atualizaCampo a2 '#'oceano
                            imprimeMar novomar
                            putStrLn "Precione ENTER ..."
                            let novonavio =removePosicao a2 navio1
                            getChar
                            --passa a vez para o outro jogador
                            executarJogo dados novomar oceano2 jogador1 jogador2 novonavio navio2 2
                        else do 
                            ---ACERTOU A AGUA
                            system "clear"
                            putStrLn "AGUAAAAA....."
                            let novomar = atualizaCampo a2 '*'oceano
                            imprimeMar novomar
                            putStrLn "Precione ENTER ..."
                            getChar
                            --passa a vez para o outro jogador
                            executarJogo dados novomar oceano2 jogador1 jogador2 navio1 navio2 1

                       
                        menu dados


            

                getChar
                menu dados
            menu dados 

        getChar
        jogo dados jogador1 jogador2

vencedor::Nome->Nome->IO ()
vencedor jogador jogador2 = do
    system "clear"
    putStrLn "################################################################################################"
    putStrLn "                                              O VENCEDOR É"     
    putStrLn ("                                          "++ (show(jogador)))
    putStrLn "################################################################################################"
    -- ESCREVE NO ARQUIVO O JOGADOR VENCEDOR.
    --ler o arquivo
    arq <- openFile "historico.txt" ReadMode
    no_arquivo <- hGetLine arq
    hClose arq

    -- abre o arquivo para escrita para atualizá-lo
    arq <- openFile "historico.txt" WriteMode
    hPutStrLn arq (jogador++"-Ganhou-do(a)"++jogador2++","++no_arquivo)
    hClose arq

    

imprimeRegras::IO ()
imprimeRegras = do
    system "clear"
    putStrLn "---------------------------------- Regras --------------------------------------"
    putStrLn "É necessário ter dois jogadores."
    putStrLn "Ganha o jogador que acertar primeiro as três partes do navio adverdário"
    putStrLn "Após o Cadastro, os jogadores devem posicionar seus navios no tabuleiro."
    putStrLn "O tamanho do tabuleiro é de 10x10 com coordenadas de A até J e de 0 até 9."
    putStrLn "O navio ocupará 3 posições consecutivas tanto na horizontal (H) como na vertical (V)."
    putStrLn "O jogador deve indicar a posição e a orientação central do seu navio."
    putStrLn "\n\tExemplo: \nLETRA(A-J): D\nNUMERO(0-9): 5\nORIENTACAO(H ou V): H."
    putStrLn "\nPara realizar o disparo no navio inimigo o jogador deve indicar as coordenadas compostas por uma letra(A-J) e um numero(0-9)."
    putStrLn "\n\tExemplo: \nLETRA(A-J): C\nNUMERO(0-9): 5\n"
    putStrLn "Caso o disparo atinja o navio inimigo será exibido o caracter '#' na posição indicada,"
    putStrLn "Caso o disparo atinja a água será exibido o caracter '*'."
    putStrLn "\n\tSIMBOLOS E SEUS SIGNIFICADOS: "
    putStrLn "'~' - Posição não revelada."
    putStrLn "'*' - Jogador acertou a água."
    putStrLn "'#' - Jogador acertou o navio inimigo."
    putStrLn "\n\tTECLAS: "
    putStrLn "Orientação vertical: A,B,C,D,E,F,G,H,I,J"
    putStrLn "Orientação horizontal: 0,1,2,3,4,5,6,7,8,9"

    putStrLn "Não é possível apagar, digite apenas quando tiver certeza!!"

-------Alterar os tipos de todas as funçoes abaixo
imprimeDupla::String->String->IO ()
imprimeDupla nome1 nome2 = do
    putStrLn (nome1 ++ "  X  " ++ nome2)

validaDisparo:: Char-> Char->Char-> Bool
validaDisparo x y z
                    |((elem x a) == (elem y b)&&(elem z a)) = True
                    |otherwise = False
                    where
                        a = ['A','B','C','D','E','F','G','H','I','J','V']
                        b = ['0','1','2','3','4','5','6','7','8','9']
--funcao responsavel por converter as entradas em inteiros
converteEntradas :: Char->Int
converteEntradas entrada = ord entrada

--verifica se o disparo acertou agua(0), navio(#) ou se já aconteceu(*)
verificaDisparo::Int->[Char]->Int
verificaDisparo posicao tabela
                    | (tabela!!posicao)== '*' = 1
                    | (tabela!!posicao)== '#' = 10
                    |otherwise = 0

--atualiza o campo com disparo na agua(*) ou navio(#) 
atualizaCampo::Int->Char->[Char]->[Char]
atualizaCampo _ _ [] = []
atualizaCampo disparo caracter (x:xs)
                    | disparo == 0 = caracter:xs
                    |otherwise = x:atualizaCampo (disparo-1) caracter xs
--funçao responsavel de atualizar a tabela com 
atualizaTabela::Int->[Int]->[Char]->[Char]
atualizaTabela posicao barco tabela 
                    | (elem posicao barco) =  atualizaCampo posicao '#' tabela --se a posicao tentada existir na lista com as posições do barco então subistitua por ##
                    |((verificaDisparo posicao tabela)==0) = atualizaCampo posicao '*' tabela
--a partir do centro e a orientaçao posiciona o navio na matriz
armazenaPosicaoNavio::Int->Char->[Int]
armazenaPosicaoNavio centro orientacao
                    | (orientacao == 'h' || orientacao == 'H' ) = horizontal centro
                    | (orientacao == 'v' || orientacao == 'V' ) = vertical centro
--não verifica se o navio nao extrapola os limites se acontecer retorna uma lista vazia 
horizontal :: Int->[Int]
horizontal centro
                    |(elem centro restricao) = []
                    |otherwise = [centro-1,centro,centro+1]
                    where restricao= [0,9,10,19,20,29,30,39,40,49,50,59,60,69,70,79,80,89,90,90]
--não verifica se o navio nao extrapola os limites se acontecer retorna uma lista vazia 
vertical :: Int->[Int]
vertical centro
                    |(elem centro restricao) = []
                    |otherwise = [centro-10,centro,centro+10]
                    where restricao= [0,1,2,3,4,5,6,7,8,9,90,91,92,93,94,95,96,97,98,99]

imprimeMar::String->IO () 
imprimeMar tabela = do
    putStr("   0   1   2   3   4   5   6   7   8  9\n" ++
                "A "++(show(tabela!!0))++" "++(show(tabela!!1))++" "++(show(tabela!!2))++" "++(show(tabela!!3))++" "++(show(tabela!!4))++" "++(show(tabela!!5))++" "++(show(tabela!!6))++" "++(show(tabela!!7))++" "++(show(tabela!!8))++" "++(show(tabela!!9))++"\n"++
                "B "++(show(tabela!!10))++" "++(show(tabela!!11))++" "++(show(tabela!!12))++" "++(show(tabela!!13))++" "++(show(tabela!!14))++" "++(show(tabela!!15))++" "++(show(tabela!!16))++" "++(show(tabela!!17))++" "++(show(tabela!!18))++" "++(show(tabela!!19))++"\n"++
                "C "++(show(tabela!!20))++" "++(show(tabela!!21))++" "++(show(tabela!!22))++" "++(show(tabela!!23))++" "++(show(tabela!!24))++" "++(show(tabela!!25))++" "++(show(tabela!!26))++" "++(show(tabela!!27))++" "++(show(tabela!!28))++" "++(show(tabela!!29))++"\n"++
                "D "++(show(tabela!!30))++" "++(show(tabela!!31))++" "++(show(tabela!!32))++" "++(show(tabela!!33))++" "++(show(tabela!!34))++" "++(show(tabela!!35))++" "++(show(tabela!!36))++" "++(show(tabela!!37))++" "++(show(tabela!!38))++" "++(show(tabela!!39))++"\n"++
                "E "++(show(tabela!!40))++" "++(show(tabela!!41))++" "++(show(tabela!!42))++" "++(show(tabela!!43))++" "++(show(tabela!!44))++" "++(show(tabela!!45))++" "++(show(tabela!!46))++" "++(show(tabela!!47))++" "++(show(tabela!!48))++" "++(show(tabela!!49))++"\n"++
                "F "++(show(tabela!!50))++" "++(show(tabela!!51))++" "++(show(tabela!!52))++" "++(show(tabela!!53))++" "++(show(tabela!!54))++" "++(show(tabela!!55))++" "++(show(tabela!!56))++" "++(show(tabela!!57))++" "++(show(tabela!!58))++" "++(show(tabela!!59))++"\n"++
                "G "++(show(tabela!!60))++" "++(show(tabela!!61))++" "++(show(tabela!!62))++" "++(show(tabela!!63))++" "++(show(tabela!!64))++" "++(show(tabela!!65))++" "++(show(tabela!!66))++" "++(show(tabela!!67))++" "++(show(tabela!!68))++" "++(show(tabela!!69))++"\n"++
                "H "++(show(tabela!!70))++" "++(show(tabela!!71))++" "++(show(tabela!!72))++" "++(show(tabela!!73))++" "++(show(tabela!!74))++" "++(show(tabela!!75))++" "++(show(tabela!!76))++" "++(show(tabela!!77))++" "++(show(tabela!!78))++" "++(show(tabela!!79))++"\n"++
                "I "++(show(tabela!!80))++" "++(show(tabela!!81))++" "++(show(tabela!!82))++" "++(show(tabela!!83))++" "++(show(tabela!!84))++" "++(show(tabela!!85))++" "++(show(tabela!!86))++" "++(show(tabela!!87))++" "++(show(tabela!!88))++" "++(show(tabela!!89))++"\n"++
                "J "++(show(tabela!!90))++" "++(show(tabela!!91))++" "++(show(tabela!!92))++" "++(show(tabela!!93))++" "++(show(tabela!!94))++" "++(show(tabela!!95))++" "++(show(tabela!!96))++" "++(show(tabela!!97))++" "++(show(tabela!!98))++" "++(show(tabela!!99))++"\n")

removePosicao:: Int->Navio->Navio
removePosicao _ [] = []
removePosicao x (y:xs) | x == y    = removePosicao x xs
                       | otherwise = y : removePosicao x xs
