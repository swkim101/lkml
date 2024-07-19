module LKML (parse, dumpTree, dumpMap, getTree, getMap) where

import Data.Map as Map (Map, lookup, (!), fromList)
import Control.Applicative (liftA)
import Text.Regex as Reg
import Data.Char (isSpace)

type Message = String
type MessageId = String
type MessageMap = Map.Map MessageId Message
type Edge = (MessageId, Maybe MessageId) -- child, parent
type Tree = [Edge]

data Header = MessageId | InReplyTo

data MBox = MBox Tree MessageMap

dumpTree :: MBox -> IO ()
dumpTree (MBox tree _) = putStrLn $ show tree

dumpMap :: MBox -> IO ()
dumpMap (MBox _ m) = putStrLn $ show m


getTree :: MBox -> Tree
getTree (MBox tree _) = tree

getMap :: MBox -> MessageMap
getMap (MBox _ m) = m

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

regMessageId :: Reg.Regex
regMessageId = Reg.mkRegexWithOpts "^Message-ID:[ \t\n\r\f\v]+<(.+)>$" True False -- multiline, case insensitive

regInReplyTo :: Reg.Regex
regInReplyTo = Reg.mkRegexWithOpts "^In-Reply-To:[ \t\n\r\f\v]+<(.+)>$" True False

regFrom :: Reg.Regex
regFrom = Reg.mkRegexWithOpts "^From " True True

getHeader :: Message -> String
getHeader a = unlines $ takeWhile (/="") $ lines a

getBody :: Message -> String
getBody a = unlines $ takeWhile (/="") $ lines a

getMust' :: Reg.Regex -> String -> Message -> String
getMust' reg regStr m = case matchRegex reg (getHeader m) of
  Nothing -> error $ "find " ++ regStr ++ " failed in \n" ++ m
  Just x -> last x

getMust :: Header -> Message -> String
getMust MessageId = getMust' regMessageId "message ID"
getMust InReplyTo = getMust' regInReplyTo "in reply to"

get' :: Reg.Regex -> Message -> Maybe String
get' reg m = liftA last $ matchRegex reg (getHeader m)

get :: Header -> Message -> Maybe String
get MessageId = get' regMessageId
get InReplyTo = get' regInReplyTo

-- dumpHeader :: String -> IO ()
-- dumpHeader a = do
--   let msgs = filter ((/=) 0 . length . trim ) $ Reg.splitRegex regFrom a
--   mapM_ (putStrLn . getHeader) msgs

parse :: String -> MBox
parse a = do
  let msgs = filter ((/=) 0 . length . trim ) $ Reg.splitRegex regFrom a
  let tree = liftA (\x -> (getMust MessageId x, get InReplyTo x)) msgs
  let messageMap = Map.fromList $ liftA (\x -> (getMust MessageId x, x)) msgs
  MBox tree messageMap
  