{-------------------------------------------
  Show the capabilities of Haskell with only
  the standard library.

  Milslav Ciz, 2017
  WTFPL license
-------------------------------------------}

import System.IO
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time
import Data.Time.Clock.POSIX

factorialOf = 10

header :: String -> Char -> String
header message separator =
  "\n" ++ message ++ "\n" ++ [separator | i <- [1..length(message)]] ++ "\n"

introduce :: String
introduce =
  "I am Haskell, a compiled, purely-functional language.\n" ++
  "I am not much used outside of the academia world, but\n" ++
  "I will bring you the most beautiful programs. I am the\n" ++
  "zen itself.\n"

factorial :: Int -> Int
factorial x =
  if x < 2 then 1 else x * factorial (x - 1)

main =
  do
    putStr
      (
        header "Showing off the power of Haskell!" '~' ++
        introduce ++
        header "general" '-'
      )
    time <- getZonedTime
    putStr ("It is " ++ (show time) ++ ".\n")
    t1 <- getCurrentTime
    putStr ("Factorial of " ++ (show factorialOf) ++ " is " ++ show (factorial 10) ++ " (it took ")
    t2 <- getCurrentTime
    putStr (show (diffUTCTime t2 t1) ++ " seconds to compute).\n")
