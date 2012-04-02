{-# LANGUAGE ViewPatterns #-}

import Data.List
import System.Environment
import Control.Arrow
import Control.Monad

-- Split list on the most equal parts
f :: Int -> [a] -> [[a]]
f 0 _  = []
f n xs = uncurry (:) . (id *** f (n - 1)) $ splitAt (length xs `div` n) xs

main = do
    fns@(length -> len) <- getArgs
    (zip fns . map unlines . f len . lines -> xs) <- getContents
    forM_ xs $ uncurry writeFile
