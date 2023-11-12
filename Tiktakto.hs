
type Coordinate = (Int, Int)
type GameProgress = [Coordinate]
type Result = String -- for game board 

pp :: Result -> IO ()
pp x = putStrLn x

initializeBoard :: (Int, Int) -> Result
initializeBoard (cols, rows) = unlines (replicate rows (replicate cols ' '))

updateBoard :: Result -> GameProgress -> Result
updateBoard board [] = board
updateBoard board ((x, y):progress) = updateBoard updatedBoard progress
  where
    player = if even (length progress) then 'x' else 'o'
    updatedBoard = unlines (updateRow (lines board) x y player)

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

ticktack :: (Int, Int) -> GameProgress -> Result
ticktack (cols, rows) progress = addBoundaries (updateBoard (initializeBoard (cols, rows)) progress)
