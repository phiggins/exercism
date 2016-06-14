module ETL where
  import qualified Data.Map as M
  import Data.Char (toLower)

  transform :: M.Map Int [String] -> M.Map String Int
  transform = M.fromList . buildPairs . M.toList
    where buildPairs xs = concat $ map (\(x,y) -> zip (map (map toLower) y) (repeat x)) xs
