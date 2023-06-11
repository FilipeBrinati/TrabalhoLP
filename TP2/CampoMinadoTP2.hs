--  Autores:
-- Filipe Brinati Furtado - 201865563C 
-- Lucca Oliveira Schröder - 201765205C



import System.Random
import Data.Char (isDigit)

-- Definição dos tipos de dados

type Coord = (Int, Int)

data Cell = Empty | Number Int | Bomb | MarkedBomb | ActualBomb deriving (Eq)

data Action = Reveal | Mark | UnMark 

type Board = [[Cell]]

-- Funções de criação do tabuleiro

createEmptyBoard :: Int -> Int -> Board
createEmptyBoard rows cols = replicate rows (replicate cols Empty)

placeBombs :: Int -> Int -> Int -> Board -> IO Board
placeBombs rows cols numBombs board = do
  bombCoords <- generateBombCoords rows cols numBombs
  return (foldr (\c acc -> updateCell c Bomb acc) board bombCoords)

generateBombCoords :: Int -> Int -> Int -> IO [Coord]
generateBombCoords rows cols numBombs = do
  gen <- newStdGen
  let allCoords = [(x, y) | x <- [0 .. rows - 1], y <- [0 .. cols - 1]]
      shuffledCoords = shuffleBombs gen allCoords
  return (take numBombs shuffledCoords)

shuffleBombs :: StdGen -> [a] -> [a]
shuffleBombs gen xs = go gen xs []
  where
    go _ [] acc = acc
    go g xs acc =
      let (n, newG) = randomR (0, length xs - 1) g
          elem = xs !! n
          remaining = take n xs ++ drop (n + 1) xs
       in go newG remaining (elem : acc)

-- Funções de manipulação do tabuleiro

updateCell :: Coord -> Cell -> Board -> Board
updateCell (x, y) cell board =
  take x board
    ++ [take y (board !! x) ++ [cell] ++ drop (y + 1) (board !! x)]
    ++ drop (x + 1) board

getCell :: Board -> Coord -> Cell
getCell board (x, y) = board !! x !! y

isBomb :: Cell -> Bool
isBomb cell = cell == Bomb

isActualBomb :: Cell -> Bool
isActualBomb cell = cell == ActualBomb

isNumbered :: Cell -> Bool
isNumbered cell = case cell of
  Number _ -> True
  _ -> False

coordenadasAdjacentes :: Coord -> Int -> Int -> [Coord]
coordenadasAdjacentes (x, y) rows cols =
  let coords = [(x + 1, y),(x - 1, y), (x, y + 1), (x, y - 1)]
   in filter (\(x', y') -> x' >= 0 && x' < rows && y' >= 0 && y' < cols) coords

-- Função principal de jogo

playGame :: Int -> Int -> Int -> IO ()
playGame rows cols numBombs = do
  let maxBombs = rows * cols `div` 2
      adjustedBombs = min numBombs maxBombs
  putStrLn $ "Número de Bombas: " ++ show adjustedBombs
  emptyBoard <- return $ createEmptyBoard rows cols
  board <- placeBombs rows cols adjustedBombs emptyBoard
  putStrLn "Tabuleiro Inicial:"
  printBoard board
  playTurn rows cols board

playTurn :: Int -> Int -> Board -> IO ()
playTurn rows cols board = do
  putStrLn "Digite as coordenadas para revelar uma célula ou marcar/desmarcar uma célula (linha coluna ação):"
  input <- getLine
  case parseInput input of
    Just (coord, action) ->
      case action of
        Reveal -> do
          let newBoard = revealCell coord rows cols board
          if isBomb (getCell newBoard coord)
            then do
              putStrLn "Você perdeu! O jogo acabou."
              putStrLn "Tabuleiro Final:"
              printBoard newBoard
            else do
              putStrLn "Tabuleiro Atual:"
              printBoard newBoard
              if checkWin newBoard
                then do
                  putStrLn "Parabéns! Você venceu o jogo."
                  putStrLn "Tabuleiro Final:"
                  printBoard newBoard
                else playTurn rows cols newBoard
        Mark -> do
          let newBoard = markCell coord board
          putStrLn "Tabuleiro Atual:"
          printBoard newBoard
          playTurn rows cols newBoard
        UnMark -> do
          let newBoard = unmarkCell coord board
          putStrLn "Tabuleiro Atual:"
          printBoard newBoard
          playTurn rows cols newBoard
    Nothing -> do
      putStrLn "Entrada inválida. Tente novamente."
      playTurn rows cols board

parseInput :: String -> Maybe (Coord, Action)
parseInput input =
  case words input of
    [xStr, yStr, actionStr] -> do
      x <- parseCoord xStr
      y <- parseCoord yStr
      action <- parseAction actionStr
      return ((x, y), action)
    _ -> Nothing

parseCoord :: String -> Maybe Int
parseCoord str =
  if all isDigit str then Just (read str) else Nothing

parseAction :: String -> Maybe Action
parseAction str =
  case str of
    "-" -> Just UnMark
    "+" -> Just Mark
    "r" -> Just Reveal
    _ -> Nothing

revealCell :: Coord -> Int -> Int -> Board -> Board
revealCell coord@(x, y) rows cols board =
  case getCell board coord of
    Empty ->
      if isRevealed coord board
        then board -- A célula já foi revelada, não faz nada
        else
          let newBoard = updateEmptyWithNumbers coord rows cols board
          in newBoard
    _ -> updateCell coord (getCell board coord) board

updateEmptyWithNumbers :: Coord -> Int -> Int -> Board -> Board
updateEmptyWithNumbers coord@(x, y) rows cols board =
  let bombCount = countAdjacentBombs coord rows cols board
      newBoard = updateCell coord (Number bombCount) board
  in newBoard

countAdjacentBombs :: Coord -> Int -> Int -> Board -> Int
countAdjacentBombs (x, y) rows cols board =
  let adjacentCoords = coordenadasAdjacentes (x, y) rows cols
      cells = map (getCell board) adjacentCoords
  in length $ filter isBomb cells


isRevealed :: Coord -> Board -> Bool
isRevealed coord board = case getCell board coord of
  Empty -> False
  Number _ -> True
  Bomb -> True
  MarkedBomb -> True
  ActualBomb -> True



markCell :: Coord -> Board -> Board
markCell coord@(x, y) board =
  let cell = getCell board coord
   in case cell of
        Empty -> updateCell coord MarkedBomb board
        Bomb -> updateCell coord ActualBomb board
        _ -> board

unmarkCell :: Coord -> Board -> Board
unmarkCell coord@(x, y) board =
  let cell = getCell board coord
   in case cell of
        MarkedBomb -> updateCell coord Empty board
        ActualBomb -> updateCell coord Bomb board
        _ -> board

checkWin :: Board -> Bool
checkWin board =
  all (\row -> all (\cell -> isBomb cell || isNumbered cell || isActualBomb cell) row) board

-- Funções de impressão do tabuleiro

printBoard :: Board -> IO ()
printBoard board = do
  let rows = length board
      cols = length (head board)
  putStrLn $ "    " ++ unwords (map show [0 .. cols - 1])
  putStrLn $ "   " ++ replicate (cols * 2 + 1) '-'
  mapM_ (printRow cols) (zip [0 ..] board)

printRow :: Int -> (Int, [Cell]) -> IO ()
printRow cols (rowIdx, cells) = do
  putStr $ show rowIdx ++ " | "
  mapM_ (putStr . cellToChar) cells
  putStrLn ""

cellToChar :: Cell -> String
cellToChar cell =
  case cell of
    Empty -> ". "
    Number n -> show n ++ " "
    Bomb -> ". "
    MarkedBomb -> "+ "
    ActualBomb -> "+ "

-- Execução do jogo

main :: IO ()
main = do
  putStrLn "Bem-vindo ao jogo Campo Minado!"
  putStrLn "Digite o número de linhas do tabuleiro:"
  rows <- readLn
  putStrLn "Digite o número de colunas do tabuleiro:"
  cols <- readLn
  putStrLn "Digite o número de bombas:"
  numBombs <- readLn
  playGame rows cols numBombs
