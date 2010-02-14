#!/usr/bin/env runhaskell

import System.IO (stderr, stdin, stdout)
import Prelude hiding (interact, getContents, unlines)
import Data.Char
import Data.Either
import Data.ByteString.Lazy hiding (count, pack)
import Data.ByteString.Lazy.Char8 hiding (count)
import Data.ParserCombinators.Attoparsec.Char8
import Control.Applicative
import Control.Monad




main                         =  handled stdin stdout stderr

handled i o e                =  do
  (b, result)               <-  parse (repeated separated) <$> hGetContents i
  case result of
    Right timezones         ->  hPutStr o (unlines timezones)
    Left err -> hPutStr e (unlines [pack err, pack "  :: at ::", b])



data TriState t              =  Done | Continue | Found t

repeated p                   =  repeatedR
 where
  repeatedR                  =  do
    res <- choice [eof >> return Done, Found <$> p, anyChar >> return Continue]
    case res of
      Done                  ->  return []
      Continue              ->  repeatedR
      Found x               ->  (x:) <$> repeatedR

separated                    =  do
  result                    <-  match compound
  spacers <|> eof
  return result
 where
  spacers                    =  satisfy (`Prelude.elem` "., \t\n") >> return ()

compound                     =  match (simple >> sep >> simple)
 where
  sep                        =  match (some digit) <|> string "/"

simple                       =  (match . range 2 3) (satisfy isUpper)

range n m p                  =  do
  required                  <-  count n p
  optional                  <-  rights <$> count m (eitherF p)
  return (required ++ optional)

