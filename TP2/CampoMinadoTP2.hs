import Data.Char
import Data.List
import System.Random
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

-- Definição de tipos

data Cell = Empty | Number Int | Bomb deriving (Eq)

type Coord = (Int, Int)

type Board = [[Cell]]

-- Função principal

main :: IO ()
main = do
  putStrLn "Bem-vindo ao Campo Minado!"
  (rows, cols) <- getSize
  numBombs <- getNumBombs
  let board = generateBoard rows cols numBombs
  playGame rows cols board

-- Funções de interação com o usuário

getSize :: IO (Int, Int)
getSize = do
  putStrLn "Digite o número de linhas do tabuleiro:"
  rows <- getIntInput
  putStrLn "Digite o número de colunas do tabuleiro:"
  cols <- getIntInput
  if rows > 0 && cols > 0
    then return (rows, cols)
    else do
      putStrLn "Tamanho inválido. Tente novamente."
      getSize

getNumBombs :: IO Int
getNumBombs = do
  putStrLn "Digite o número de bombas:"
  numBombs <- getIntInput
  if numBombs >= 0
    then return numBombs
    else do
      putStrLn "Número inválido. Tente novamente."
      getNumBombs

getIntInput :: IO Int
getIntInput = do
  input <- getLine
  case readMaybe input of
    Just n -> return n
    Nothing -> do
      putStrLn "Entrada inválida. Tente novamente."
      getIntInput

-- Funções de geração e manipulação do tabuleiro

generateBoard :: Int -> Int -> Int -> Board
generateBoard rows cols numBombs =
  let board = replicate rows (replicate cols Empty)
      gen = mkStdGen seed
      seed = rows + cols + numBombs
      indices = take numBombs $ nub $ randomRs (0, rows * cols - 1) gen
      bombs = map (\idx -> (idx `div` cols, idx `mod` cols)) indices
   in foldr (\coord acc -> updateCell coord Bomb acc) board bombs


updateCell :: Coord -> Cell -> Board -> Board
updateCell (x, y) cell board =
  let (before, row : after) = splitAt x board
      (left, _ : right) = splitAt y row
   in before ++ [left ++ [cell] ++ right] ++ after

getCell :: Board -> Coord -> Cell
getCell board (x, y) = board !! x !! y

isHiddenCell :: Cell -> Bool
isHiddenCell cell = cell == Empty

isBomb :: Cell -> Bool
isBomb cell = cell == Bomb

countAdjacentBombs :: Coord -> Board -> Int
countAdjacentBombs (x, y) board =
  let neighbors = getNeighbors (x, y) (length board)
   in length $ filter isBomb $ map (getCell board) neighbors

getNeighbors :: Coord -> Int -> [Coord]
getNeighbors (x, y) size =
  let neighbors = [(x - 1, y), (x, y - 1), (x, y + 1), (x + 1, y)]
   in filter (isValidCoord size) neighbors

isValidCoord :: Int -> Coord -> Bool
isValidCoord size (x, y) = x >= 0 && x < size && y >= 0 && y < size

revealCell :: Coord -> Board -> Board
revealCell coord@(x, y) board
  | not (isValidCoord (length board) coord) = board
  | not (isHiddenCell cell) = board
  | isBomb cell = revealAllCells board
  | x == 0 || y == 0 = updateCell coord (Number count) board
  | count == 0 = revealEmptyCells [(x, y)] (updateCell coord (Number count) board)
  | otherwise = updateCell coord (Number count) board
  where
    cell = getCell board coord
    count = countAdjacentBombs coord board


revealEmptyCells :: [Coord] -> Board -> Board
revealEmptyCells [] board = board
revealEmptyCells (coord : coords) board =
  let updatedBoard = revealCell coord board
      newEmptyNeighbors = filter (\neighbor -> isHiddenCell (getCell updatedBoard neighbor)) (getNeighbors coord (length board))
      newCoords = coords ++ newEmptyNeighbors
   in revealEmptyCells newCoords updatedBoard

revealAllCells :: Board -> Board
revealAllCells board = map revealRow board
  where
    revealRow = map revealCell'
    revealCell' cell
      | isHiddenCell cell = Number $ countAdjacentBombs (0, 0) board
      | otherwise = cell

-- Função principal do jogo

playGame :: Int -> Int -> Board -> IO ()
playGame rows cols board = do
  putStrLn $ "\n" ++ renderBoard board
  putStrLn "Digite as coordenadas para revelar uma célula (linha e coluna):"
  coord <- getCoordInput rows cols
  let updatedBoard = revealCell coord board
  if isBomb (getCell updatedBoard coord)
    then do
      putStrLn $ "\n" ++ renderBoard (revealAllCells updatedBoard)
      putStrLn "Você perdeu! Fim de jogo."
    else if isGameOver updatedBoard
           then do
             putStrLn $ "\n" ++ renderBoard (revealAllCells updatedBoard)
             putStrLn "Parabéns! Você venceu!"
           else playGame rows cols updatedBoard


getCoordInput :: Int -> Int -> IO Coord
getCoordInput rows cols = do
  putStrLn "Digite a linha da célula:"
  row <- getValidCoordInput rows
  putStrLn "Digite a coluna da célula:"
  col <- getValidCoordInput cols
  return (row, col)

getValidCoordInput :: Int -> IO Int
getValidCoordInput maxVal = do
  input <- getIntInput
  if input >= 0 && input < maxVal
    then return input
    else do
      putStrLn "Coordenada inválida. Tente novamente."
      getValidCoordInput maxVal


readMaybeCoord :: String -> Maybe Coord
readMaybeCoord input =
  case mapMaybe readMaybeInt (words input) of
    [x, y] -> Just (x, y)
    _ -> Nothing

readMaybeInt :: String -> Maybe Int
readMaybeInt s = case reads s of
  [(n, "")] -> Just n
  _ -> Nothing


-- Funções auxiliares

isGameOver :: Board -> Bool
isGameOver board = all (not . isHiddenCell) (concat board)

renderBoard :: Board -> String
renderBoard = unlines . map renderRow
  where
    renderRow = intercalate " " . map renderCell
    renderCell Empty = "."
    renderCell (Number n) = show n
    renderCell Bomb = "X"
-- modificar aqui quando acabar de testar