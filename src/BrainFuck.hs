module BrainFuck
where
  -- Список команд :(
  -- >	i++;	перейти к следующей ячейке
  -- <	i--;	перейти к предыдущей ячейке
  -- +	arr[i]++;	увеличить значение в текущей ячейке на 1
  -- -	arr[i]--;	уменьшить значение в текущей ячейке на 1
  -- .	putchar(arr[i]);	напечатать значение из текущей ячейки
  -- ,	arr[i] = getchar();	ввести извне значение и сохранить в текущей ячейке
  -- [	while(arr[i]){	если значение текущей ячейки ноль, перейти вперёд по тексту программы на ячейку, следующую за соответствующей ] (с учётом вложенности)
  -- ]	}	если значение текущей ячейки не нуль, перейти назад по тексту программы на символ [ (с учётом вложенности)
  --
  import Data.Char (ord, chr)
  -- Прости господи, список инструкций
  -- В названиях могла быть ваша реклама
  data Instruction = Forward
                   | Backward
                   | Increment
                   | Decrement
                   | Output
                   | Input
                   | Loop [Instruction]
                   deriving (Show)

  data Memory = Memory [Int] [Int] deriving (Show)

  emptyMemory :: Memory
  emptyMemory = Memory [] []

  goForward :: Memory -> Memory
  goForward (Memory left []) = Memory (0:left) []
  goForward (Memory left (x:xs)) = Memory (x:left) xs

  goBackward :: Memory -> Memory
  goBackward (Memory [] right) = Memory [] (0:right)
  goBackward (Memory (x:xs) right) = Memory xs (x:right)

  readMemory :: Memory -> Int
  readMemory (Memory _ []) = 0
  readMemory (Memory _ (x:_)) = x

  modifyMemory :: (Int -> Int) -> Memory -> Memory
  modifyMemory f (Memory l []) = Memory l [f 0]
  modifyMemory f (Memory l (x:xs)) = Memory l (f x : xs)

  inc :: Memory -> Memory
  inc m = modifyMemory (+1) m

  dec :: Memory -> Memory
  dec m = modifyMemory (+(-1)) m

  writeToMemory :: Memory -> Int -> Memory
  writeToMemory (Memory left (x:xs)) num = Memory left (num:xs)

  readFromMemory :: Memory -> Int
  readFromMemory (Memory _ (x:_)) = x

  run_parsed :: [Instruction] -> IO Memory
  run_parsed instructions = exec emptyMemory instructions
    where
      exec m [] = return m
      exec m (x:xs) = case x of
        Forward -> exec (goForward m) xs
        Backward -> exec (goBackward m) xs
        Increment -> exec (inc m) xs
        Decrement -> exec (dec m) xs
        Input -> do
          c <- getChar
          exec (writeToMemory m (ord c)) xs
          return m
        
