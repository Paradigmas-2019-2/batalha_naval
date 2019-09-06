import System.IO
import System.Process
import Data.Char
import System.Random
mar = replicate 100 '~'


menu::IO String
menu = do
    putStrLn "-----------------------------------Batalha-Naval----------------------------"
    putStrLn "Opções:"
    putStrLn "\t1 - Iniciar o jogo:"
    putStrLn "\t2 - Historico:"
    putStrLn "\t3 - Sair:"
    opcao <- getChar
    getChar
    opcoes opcao

opcoes::Char->IO String
opcoes '1' = do
            iniciaJogo
opcoes '2' = do 
            putStrLn "sair..."
            return"saiu"
opcoes '3' = do 
            putStrLn "sair..."
            return"saiu"

iniciaJogo::IO String
iniciaJogo =do 
    system "clear"
    system "clear"
    putStr "\tNome Jogador1: "
    nome1 <- getLine
    system "clear"
    putStr "\tNome Jogador2: "
    nome2 <- getLine
    system "clear"
    imprimeDupla nome1 nome2
    let oceano = replicate 100 '~'
    imprimeMar oceano
    getChar
    jogo nome1 nome2 1

    return nome1

jogo::String->String->Int->IO Char
jogo jogador1 jogador2 vez = do
    system "clear"
    if(vez==1) then do
        putStrLn ("É a vez do "++jogador1++"...")
        putStrLn "------------------Coordenadas----------------"
        putStr "\nY(letra): "
        y <-getChar
        getChar
        putStr "X(numero): "
        x <-getChar
        getChar
        let res = validaDisparo x y
        print(res)
        return 'a'

    else do
        putStr ("a")
        return 'a'


imprimeDupla::String->String->IO ()
imprimeDupla nome1 nome2 = do
    putStrLn (nome1 ++ "  X  " ++ nome2)








validaDisparo:: Char-> Char-> Bool
validaDisparo x y 
                    |((elem x a) &&(elem y b)) = True
                    |otherwise = False
                    where
                        a = ['A','B','C','D','E','a','b','c','d','e']
                        b = ['1','2','3','4','5','6','7','8','9']
--funcao responsavel por converter as entradas em inteiros
converteEntradas :: Char->Int
converteEntradas entrada = digitToInt entrada

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
disparoComputador :: IO Int
disparoComputador = do
    number <- randomRIO (0,99)
    return (number)


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
