module Main (main) where

import System.IO ( hGetContents, openFile, IOMode(ReadMode) )
import LKML
import Test.HUnit
    ( assertEqual, runTestTT, Test(TestLabel, TestCase, TestList), Counts )

testSingleMessage :: IO Counts
testSingleMessage = do
  handle <- openFile "test/single.mbox" ReadMode
  contents <- System.IO.hGetContents handle
  let mbox = parse contents
  let test1 = TestCase (assertEqual "test1.mbox tree test1" [("000000000000236b8d061cf510dc@google.com",Nothing)] (getTree mbox))
  let test2 = TestCase (assertEqual "test1.mbox map test1" ("fromList [(\"000000000000236b8d061cf510dc@google.com\",\"mboxrd@z Thu Jan  1 00:00:00 1970\\nMessage-ID: <000000000000236b8d061cf510dc@google.com>\\nSubject: test\\nFrom: syzbot <syzbot+96c36598b73a9d3f3e25@syzkaller.appspotmail.com>\\nTo: linux-bluetooth@vger.kernel.org\\nContent-Type: text/plain; charset=\\\"UTF-8\\\"\\n\\nHello,\\n\\nasfsadf\\nsyzkallerfdsa\\nfsa\\n\\n\\nasdfsaf\")]") (show $ getMap mbox))
  runTestTT $ TestList [
    TestLabel "test1.mbox tree test" test1
    , TestLabel "test1.mbox tree test" test2
    ]

main :: IO Counts
main = do
  testSingleMessage
  -- dumpTree mails
  