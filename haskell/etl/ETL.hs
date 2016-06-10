module ETL where
  import qualified Data.Map as M
  import Data.Char (toLower)

  transform :: M.Map Int [String] -> M.Map String Int
  transform = M.foldrWithKey (\k xs acc -> foldr (\x a -> M.insert (map toLower x) k a) acc xs) M.empty
