module BWMachine where

import Control.Monad
import Data.Array
import Data.List
import Code
import MyUtil

data MachineState =
  MState { dp :: Int, memory :: Array Int Int, code :: [Inst] }

newtype Machine pc =
  Machine { runMachine :: MachineState ->
                          IO (Either String (pc, MachineState)) }

instance Monad Machine where
  return a = Machine $ \s -> return $ Right (a, s)
  m >>= cont = Machine $ \s ->
    runMachine m s >>= \v -> case v of
    (Left err) -> return $ Left err
    (Right (a, s')) -> runMachine (cont a) s'

instance Functor Machine where
  fmap = liftM

instance Applicative Machine where
  pure = return
  (<*>) = ap

throwError :: String -> Machine a
throwError msg = Machine $ \_ -> return $ Left msg

type BWMachine = Machine Int

memSize = 30000

initialState =
  MState { dp = 0,
           memory = array (0, memSize) [(i, 0) | i <- [0..memSize]],
           code = [] }

exit = Machine $ \ms -> return $ Right (0, ms)

getCurInst pc =
  Machine $ \ms ->
  let cs = code ms in
    if Data.List.length cs <= pc
    then return $ Left $ "Out of Program (pc: " ++ show pc ++ ")"
    else return $ Right (cs!!pc, ms)

getCurPointer =
  Machine $ \ms -> return $ Right $ ((dp ms), ms)

putCurPointer p =
  Machine $ \ms -> return $ Right $ ((), ms { dp = p })

mvCurPointer diff = do
  idx <- getCurPointer
  putCurPointer (idx + diff)

getCurValue =
  Machine $ \ms ->
  let mem = memory ms
      idx = dp ms
      (s, e) = Data.Array.bounds mem
  in
    if s <= idx && idx < e
    then return $ Right $ (mem!idx, ms)
    else return $ Left $ "Out of Memory (dp: " ++ show idx ++ ")"

putCurValue v =
  Machine $ \ms ->
  let mem = memory ms
      idx = dp ms
      (s, e) = Data.Array.bounds mem
  in
    if s <= idx && idx < e
    then return $ Right $ ((), ms {memory = mem//[(idx, v)]})
    else return $ Left $ "Out of Memory (dp: " ++ show idx ++ ")"

mvCurValue diff = do
  v <- getCurValue
  putCurValue (v + diff)

loadProgram program =
  Machine $ \ms -> return $ Right (0, ms { code = program })

nextR pc =
  Machine $ \ms ->
  let cs = code ms
      rest = Data.List.drop pc cs
  in
    case MyUtil.fstOccurrence ((==) JumpR) rest of
      Nothing -> return $ Left "There is no corresponding R to L"
      Just idx -> return $ Right (idx + pc, ms)

prevL pc =
  Machine $ \ms ->
  let cs = code ms
      rest = Data.List.take (pc + 1) cs
      rrest = Data.List.reverse rest
      llen = Data.List.length cs
  in
    case MyUtil.fstOccurrence ((==) JumpL) rrest of
      Nothing -> return $ Left "There is no corresponding L to R"
      Just idx -> return $ Right (pc - idx, ms)
putStdOut =
  Machine $ \ms ->
  let idx = dp ms
      mem = memory ms
      v = mem!idx
  in
    do
      putStrLn $ show v
      return $ Right ((), ms)

getStdIn =
  Machine $ \ms ->
  let idx = dp ms
      mem = memory ms
  in
    do
      l <- getLine
      return $ Right ((), ms {memory = mem//[(idx, (read l) :: Int)]})

dbgPrint str =
  Machine $ \ms -> do
  putStrLn $ "DebugPrint: " ++ str
  return $ Right ((), ms)


eval :: Int -> BWMachine
eval pc = do
  code <- getCurInst pc
  case code of
    Exit -> do
      exit
    PInc -> do
      mvCurPointer 1
      eval $ pc + 1
    PDec -> do
      mvCurPointer (-1)
      eval $ pc + 1
    VInc -> do
      mvCurValue 1
      eval $ pc + 1
    VDec -> do
      mvCurValue (-1)
      eval $ pc + 1
    PutChar -> do
      putStdOut
      eval $ pc + 1
    GetChar -> do
      getStdIn
      eval $ pc + 1
    JumpL -> do
      v <- getCurValue
      if v == 0
        then do
        idx <- nextR pc
        -- dbgPrint $ "nextR = " ++ show idx
        eval $ idx + 1
        else eval $ pc + 1
    JumpR -> do
      v <- getCurValue
      if v /= 0
        then do
        idx <- prevL pc
        -- dbgPrint $ "prevL = " ++ show idx
        eval $ idx + 1
        else eval $ pc + 1

evalProgram :: [Inst] -> BWMachine
evalProgram program = do
  loadProgram program
  eval 0

runProgram :: [Inst] -> IO ()
runProgram program =
  runMachine (evalProgram program) initialState >>= \res ->
  case res of
    Right _ -> return ()
    Left msg -> putStrLn $ "Runtime error: " ++ msg
