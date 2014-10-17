module Merc.Parser (
  nickname,
  username,
  host,
  hostmask,
  channelName,
  message
) where

import Control.Applicative
import qualified Data.Attoparsec.Text as A
import Data.Monoid
import qualified Data.Text as T
import qualified Merc.Types.Channel as C
import qualified Merc.Types.Message as M
import qualified Merc.Types.User as U

nameHeadChars :: [Char]
nameHeadChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz[]{}\\|`_-"

nameTailChars :: [Char]
nameTailChars = nameHeadChars ++ "0123456789"

name :: A.Parser T.Text
name = mappend <$> (T.singleton <$> A.satisfy (`elem` nameHeadChars))
               <*> A.takeWhile (`elem` nameTailChars)

word :: A.Parser T.Text
word = A.takeWhile1 (not . A.isHorizontalSpace)

spaces :: A.Parser ()
spaces = A.skip A.isHorizontalSpace *> A.skipWhile A.isHorizontalSpace

nickname :: A.Parser U.Nickname
nickname = U.Nickname <$> name

username :: A.Parser T.Text
username = name

host :: A.Parser T.Text
host = word

hostmask :: A.Parser U.Hostmask
hostmask = U.Hostmask <$> nickname <* A.char '!'
                      <*> username <* A.char '@'
                      <*> host

channelName :: A.Parser C.ChannelName
channelName = A.char '#' *>
             (C.ChannelName <$> A.takeWhile1 (`elem` nameTailChars))

messagePrefix :: A.Parser M.Prefix
messagePrefix = A.char ':' *> ((M.HostmaskPrefix <$> hostmask)
                           <|> (M.ServerPrefix <$> host))

messageCommand :: A.Parser M.Command
messageCommand = do
  commandName <- T.toUpper <$> A.takeWhile1 (not . A.isHorizontalSpace)
  return $ case M.fromCommandName commandName of
    Just command -> command
    Nothing -> M.UnknownCommand commandName

message :: A.Parser M.Message
message = do
  prefix <- A.option Nothing (Just <$> messagePrefix <* spaces)
  command <- messageCommand

  params <- A.option [] (spaces *>
                         A.takeWhile1 (\c -> not (A.isHorizontalSpace c) &&
                                             c /= ':')
                         `A.sepBy` spaces)

  A.option () spaces
  trailing <- A.option [] ((:[]) <$> (A.char ':' *> A.takeText))

  A.endOfInput

  return M.Message {
    M.prefix = prefix,
    M.command = command,
    M.params = params ++ trailing
  }
