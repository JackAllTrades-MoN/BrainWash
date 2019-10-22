module Code where

import MyUtil
import Data.Text
import Data.Text.IO
import Data.List

data Inst = PInc
          | PDec
          | VInc
          | VDec
          | PutChar
          | GetChar
          | JumpL
          | JumpR
          | Comment Text
          | Exit
          deriving(Show, Eq)

tokenizeCode :: Text -> [Text]
tokenizeCode = split $ \c -> c == '。' || c == '、' || c == '「' || c == '」'

parseCode :: Text -> Inst
parseCode str
  | str == pack "ありがとう" = PInc
  | str == pack "バカヤロウ" = PDec
  | str == pack "名勝の水" = VInc
  | str == pack "水道水" = VDec
  | str == pack "波動を出力" = PutChar
  | str == pack "波動を測定" = GetChar
  | str == pack "量" = JumpL
  | str == pack "子" = JumpR
  | str == pack "アセンション" = Exit
  | otherwise = Comment str

readFile :: FilePath -> IO [Inst]
readFile filename = do
  x <- Data.Text.IO.readFile filename
  Prelude.putStrLn $ Data.Text.unpack x
  let codes = Data.List.map parseCode
        $ tokenizeCode
        $ stripAll x
  return $ codes ++ [Exit]

isNotComment (Comment _) = False
isNotComment _ = True

rmComment = Data.List.filter isNotComment
