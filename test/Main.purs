module Test.Main where

import Prelude

import Test.Unit (suite, test)
import Test.Unit.Assert (assert, assertFalse, equal, expectFailure)
import Test.Unit.Main (runTest)

main :: _
main = runTest do
  suite "sync code" do
    test "arithmetic" do
      assert "2 + 2 should be 4" $ (2 + 2) == 4
      assertFalse "2 + 2 shouldn't be 5" $ (2 + 2) == 5
      equal 4 (2 + 2)
      expectFailure "2 + 2 shouldn't be 5" $ equal 5 (2 + 2)
