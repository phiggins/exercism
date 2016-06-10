module ETL where
  import qualified Data.Map as M
  import Data.Char (toLower)

  transform :: M.Map Int [String] -> M.Map String Int
  transform = M.fromList . (M.foldrWithKey (\k x acc -> map (\y -> (map toLower y,k)) x ++ acc) [])
