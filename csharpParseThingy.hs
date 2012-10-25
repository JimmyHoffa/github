{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings #-}
import Control.Applicative
import Data.Attoparsec hiding (take, string, inClass)
import Data.Attoparsec.Combinator
import Data.Attoparsec.ByteString hiding (take, inClass)
import Data.Attoparsec.ByteString.Char8 hiding (take, string)

import Data.List
import System.IO.MMap
import Control.Monad (forM_, (>>))
import System.IO
import Control.Exception (bracket)
import System.Environment
import qualified Data.ByteString.Char8 as B
data CSharp a =
         File [CSharp a]
       | Class [CSharp a]
       | Property [CSharp a]
       | Method [CSharp a]
       | Comment a
       | Namespace [CSharp a]
       | Using (CSharp a)
       | Block [CSharp a]
       | Unidentified a
       | Expr a
       | Empty
       deriving (Show, Ord, Eq)
anyTill = manyTill anyChar
anyTillS = anyTill . string
anyBetween start end = start *> anyTill end
anyBetweenS start end = anyBetween (string start) (string end)
oneLineComment = anyBetween (string "//" *> many1 space) endOfLine
multiLineComment = anyBetweenS "/*" "*/"

using = do
  p <- string "using" *> (many1 space *> notChar '(') *> anyTill (string ";" <|> string " ")
  return $ Using $ Expr p

namespace = do
  p <- string "namespace" *> many1 space *> anyTill (string " " <|> string "{" <|> string "\r")
  return $ Namespace [Expr p]

comment = do
  p <- oneLineComment <|> multiLineComment
  return $ Comment p

csharp = comment <|> using <|> namespace <|> block <|> unidentified
 
block = do
  p <- string "{" *> manyTill csharp (string "}")
  return $ Block p
 
unidentified = do
  p <- manyTill anyChar anyChar
  return $ Unidentified p

parseAllOf = parseOnly . many1

dropCS t [] = []
dropCS t (Block xs:rest) =
  (Block $ dropCS t xs) : dropCS t rest
dropCS t (x:xs)
  | t /= x = x : dropCS t xs
  | otherwise = dropCS t xs
 
test = do {
  fsIO <- mmapFileByteString "C:\\awesome.cs" Nothing;
    case parseAllOf csharp fsIO of
      Left err -> print err
      Right xs -> foldl1 (>>) $ printCsharp (dropCS (Unidentified "") xs)
  }
  where printCsharp [] = []
        printCsharp (x:xs) = case x of
          Block ys -> (intersperse (putStr "  ") $ printCsharp ys) ++ printCsharp xs
          otherwise -> print x : printCsharp xs