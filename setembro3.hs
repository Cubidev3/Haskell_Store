-- Questão: Dada uma lista de códigos de barra
--          e baseada num banco de dados com
--          com cada produto,
-- 					crie uma função que mostre na tela
--          a nota de compra dessa lista.

-- Exemplo: <Banco de dados> =  [ (4719, "Fish Fingers", 121),
--                                (5643, "Nappies", 1010),
--                                (3814, "Orange Jelly", 56),
--                                (1111, "Hula Hoops", 21),
--                                (1112, "Hula Hoops (Giant)", 133) ]


--          <funcao> [1234, 4719, 3814,1112, 1113, 1234]

-- 					        Haskell Stores
   
--          Dry Sherry, 1lt...........5.40
--          Fish Fingers..............1.21
--          Orange Jelly..............0.56
--          Hula Hoops (Giant)........1.33
--          Unknown Item..............0.00
--          Dry Sherry, 1lt...........5.40
--          
--          Total....................13.90

-- Vou começar da função principal e ir criando funções
-- para auxilia-la

nomeDaLoja = "Haskell Store"
tamanhoDaLinha = 30

type Codigo = Int
type Nome = String
type Preco = Int -- Em Centavos

type Produto = (Nome, Preco)
bancoDeDados :: [(Codigo, Produto)]
bancoDeDados = [ (4719, ("Fish Fingers", 121)),
                 (5643, ("Nappies", 1010)),
                 (3814, ("Orange Jelly", 56)),
                 (1111, ("Hula Hoops", 21)),
                 (1112, ("Hula Hoops (Giant)", 133)) ]

mostrarNotaDeCompra :: [Codigo] -> IO()
mostrarNotaDeCompra codigos = putStr notaDeCompra 
  where notaDeCompra = montarNotaDeCompra codigos

montarNotaDeCompra :: [Codigo] -> String
montarNotaDeCompra codigos = cabecalho ++ "\n" ++
                             tabela ++ "\n" ++
                             total ++ "\n"
 where
  tabela = montarTabela codigos
  total = montarTotal codigos

cabecalho :: String
cabecalho = espacoVazio ++ nomeDaLoja ++ "\n"
 where 
  espacoVazio = replicate quantidadeDeEspacos ' '
  quantidadeDeEspacos = (tamanhoDaLinha - length nomeDaLoja) `div` 2

montarTabela :: [Codigo] -> String
montarTabela codigos = concat [ linha ++ "\n" | codigo <- codigos, let linha = montarLinha codigo ]

montarTotal :: [Codigo] -> String
montarTotal codigos = "Total" ++ pontinhos ++ preco
 where 
  preco = formatarPreco precoTotal
  precoTotal = sum [ preco | codigo <- codigos, let (_,preco) = encontrarProduto codigo ]
  pontinhos = replicate quantidadeDePontinhos '.'
  quantidadeDePontinhos = tamanhoDaLinha - 5 - length preco

montarLinha :: Codigo -> String
montarLinha codigo = nomeDoProduto ++ pontinhos ++ preco
  where
   (nomeDoProduto, precoEmCentavos) = encontrarProduto codigo
   preco = formatarPreco precoEmCentavos
   pontinhos = replicate quantidadeDePontinhos '.'
   quantidadeDePontinhos = tamanhoDaLinha - length nomeDoProduto - length preco

formatarPreco :: Preco -> String
formatarPreco preco = reais ++ "." ++ centavos
  where 
  reais = show (preco `div` 100)
  centavos 
    | length str <= 1 = str ++ "0"
    | otherwise = str
    where str = show (preco `mod` 100)

encontrarProduto :: Codigo -> Produto
encontrarProduto codigo
  | tentativa /= [] = head tentativa
  | otherwise = ("Unknown Product", 0)
  where tentativa = [ produto | (cod, produto) <- bancoDeDados, cod == codigo]
 
