type Coordinate = (Int, Int)
type GameProgress = [Coordinate]
type Result = String -- for game board 

pp :: Result -> IO ()
pp x = putStr (concat (map (++ "\n") (lines x)))

initializeBoard :: (Int, Int) -> Result
initializeBoard (cols, rows) = unlines (replicate rows (replicate cols ' '))

updateBoard :: Result -> GameProgress -> Result
updateBoard board [] = board
updateBoard board ((x, y):progress) = updateBoard updatedBoard progress
  where
    updatedBoard = unlines (updateRow (lines board) x y 'x')  -- 'x' is just an example, you should pass the actual player here

    updateRow :: [String] -> Int -> Int -> Char -> [String]
    updateRow rows x y player =
      let (before, current:after) = splitAt (length rows - y) rows
          updatedRow = before ++ [updateRowStr current x player] ++ after
      in updatedRow

    updateRowStr :: String -> Int -> Char -> String
    updateRowStr row x player = take (x - 1) row ++ player : drop x row

addBoundaries :: Result -> Result
addBoundaries board =
  let rows = lines board
      cols = length (head rows)
      topBoundary = replicate (cols + 2) '-' ++ "\n"
      bottomBoundary = topBoundary
      middle = map (\row -> '|' : row ++ "|") rows
  in unlines (topBoundary : middle ++ [bottomBoundary])

generateTicTacToeBoard :: (Int, Int) -> GameProgress -> IO ()
generateTicTacToeBoard (cols, rows) progress = pp $ addBoundaries (updateBoard (initializeBoard (cols, rows)) progress)

parseMoves :: [String] -> GameProgress
parseMoves [] = []
parseMoves (x:y:moves) = (read x, read y) : parseMoves moves