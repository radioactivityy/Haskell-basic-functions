--Comment to run:  pp(ticktack (8,8) [(1,1),(8,8),(2,2),(3,3),(4,2),(3,2)])
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

ticktackInteractive :: IO ()
ticktackInteractive = do
  putStrLn "Enter the number of columns: "
  cols <- readLn
  putStrLn "Enter the number of rows: "
  rows <- readLn
  putStrLn "Enter the list of coordinates (e.g., [(1,1),(8,8),(2,2),(3,3),(4,2),(3,2)]): "
  coordinates <- readLn
  let result = addBoundaries (updateBoard (initializeBoard (cols, rows)) coordinates)
  pp result
