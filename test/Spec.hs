{-
-- EPITECH PROJECT, 2025
-- Glados-On-Top
-- File description:
-- Spec
-}

module Main where

import Test.Hspec
import Lib (someFunc)

main :: IO ()
main = hspec $ do
  describe "someFunc" $ do
    it "returns the expected output" $ do
      someFunc `shouldReturn` "someFunc"
