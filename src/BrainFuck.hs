module BrainFuck
where
  import Data.Char (ord, chr)
  import qualified Text.Megaparsec as P
  import qualified Text.Megaparsec.Char as P'
  import Data.Text (Text)
  import Data.Void
  import System.IO
  import Control.Monad
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
                   -- | Forward_opt (Int)
                   | Backward
                   -- | Backward_opt (Int)
                   | Increment
                   -- | Increment_opt (Int)
                   | Decrement
                   -- | Decrement_opt (Int)
                   | Output
                   | Input
                   | Loop [Instruction]
                   deriving (Show, Eq)


  -- !Parser
  type Parser = P.Parsec Void String

  parseBf :: Parser Instruction
  parseBf = P.choice
                  [ Increment <$ P'.char '+'
                  , Decrement <$ P'.char '-'
                  , Backward <$ P'.char '<'
                  , Forward <$ P'.char '>'
                  , Output <$ P'.char '.'
                  , Input <$ P'.char ','
                  , Loop <$> (P'.char '[' *> P.many parseBf <* P'.char ']')
                  ]

  data Program = Program [Instruction] deriving (Show)

  parseProgram :: Parser Program
  parseProgram = Program <$> P.many parseBf

  bfPreprocessor :: [Char] -> [Char]
  bfPreprocessor str = filter (\a -> a `elem` "><.,[]+-") str

  evalProgram :: String -> IO ()
  evalProgram s = case P.parse parseProgram "" (bfPreprocessor s) of
    Left e -> do
      print "error"
      print e
    Right prog -> do
      mem <- run_parsed prog
      print mem

  -- !Interpretator
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
  writeToMemory m num = modifyMemory (const num) m

 -- для исполнение кода, который был распарсен используется парадигма
 -- state-машины
  run_parsed :: Program -> IO Memory
  run_parsed (Program instructions) = exec emptyMemory instructions
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
        Output -> do
          putChar (chr (readMemory m))
          exec (m) xs
        Loop body -> if (readMemory m) /= 0
                     then do
                       m' <- exec m body
                       exec m' (x:xs)
                     else exec m xs

  readAndExecBf :: [Char] -> IO ()
  readAndExecBf path = do
    contents <- readFile path
    evalProgram contents



  helloWorldProgram :: [Char]
  helloWorldProgram = "++++[>+++++<-]>[<+++++>-]+<+[>[>+>+<<-]++>>[<<+>>-]>>>[-]++>[-]+>>>+[[-]++++++>>>]<<<[[<++++++++<++>>-]+<.<[>----<-]<]<<[>>>>>[>>>[-]+++++++++<[>-<-]+++++++++>[-[<->-]+[<<<]]<[>+<-]>]<<-]<<-]"

  c :: [Char]
  c = ",[.-]"

  constantPath :: [Char]
  constantPath = "C:\\Users\\AnarCom\\Desktop\\BrainFuck\\squares.bf"
