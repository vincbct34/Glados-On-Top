{-
-- EPITECH PROJECT, 2025
-- Glados-On-Top
-- File description:
-- Spec
-}

module Main (main) where

import Test.Hspec

import Lib (someFunc)

main :: IO ()
main = hspec $ do
  describe "someFunc" $ do
    it "returns ()" $ do
      someFunc `shouldReturn` ()