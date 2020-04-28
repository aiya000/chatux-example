module Main where

import ChatUxExample.Main (app)
import RIO
import Tonatona (run)

main :: IO ()
main = run app
