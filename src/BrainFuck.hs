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

  decr :: Memory -> Memory
  decr m = modifyMemory (+(-1)) m

  writeToMemory :: Memory -> Int -> Memory
  writeToMemory (Memory left (x:xs)) num = Memory left (num:xs)

  readFromMemory :: Memory -> Int
  readFromMemory (Memory _ (x:_)) = x
