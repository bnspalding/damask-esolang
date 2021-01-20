{-# LANGUAGE OverloadedStrings #-}

module DamaskSpec
  ( spec,
  )
where

import Damask
import Test.Hspec

spec :: Spec
spec = do
  describe "fundamentals"
    $ it "returns an identical text when no operators are present"
    $ runPoem "hello world" `shouldBe` "hello world"
  describe "operators" $ do
    describe "comma (,) - FLIP" $ do
      it "flips the left side and right side of a line" $
        runPoem "hello, my world" `shouldBe` "my world, hello"
      it "associates left" $
        runPoem "green, blue, and red" `shouldBe` "and red, blue, green"
    describe "colon (:) - PUSH" $ do
      it "moves the left side of the line to the end of the poem (new line)" $
        runPoem "hello: world\ngoodbye" `shouldBe` "hello\ngoodbye\nworld"
      it "associates left" $
        runPoem "hello: world: I love you" `shouldBe` "hello\nworld\nI love you"
      it "trims whitespace beginning a new line" $
        runPoem "hello:  world:\tgoodbye" `shouldBe` "hello\nworld\ngoodbye"
    describe "em dash (—) - SHIFT" $ do
      it "shifts the letters of the left word by the values of the right word" $
        runPoem "aaa—abc" `shouldBe` "bcd"
      it "uses whitespace before the second word as zeros" $
        runPoem "aaa—  c" `shouldBe` "aad"
      it "treats whitespace after the first word as zeroes" $
        runPoem "a  — abc" `shouldBe` "bbc"
      it "wraps from z to a (including zero as space)" $
        runPoem "zzz—abc" `shouldBe` " ab"
      it "operates on the first n characters of the left word" $
        runPoem "aaaa—aa" `shouldBe` "bbaa"
      it "pads right with zeros on the left word to the length of the right word" $
        runPoem "aa—aabb" `shouldBe` "bbbb"
