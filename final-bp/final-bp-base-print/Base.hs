{-# LANGUAGE TypeApplications #-}
module Base where

lit :: Int -> String
lit = show

add :: String -> String -> String
add x y = "(" ++ x ++ ") + (" ++ y ++ ")"
