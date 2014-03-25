-- | Parses a string of the form @123.123.123.123:54321@ to a "To".

-- TODO: Allow arbitrary hostnames, in particular IPv6 and DNS

{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}

module Config.AddressParser where

import Text.Parsec hiding (many, (<|>))
import Text.Parsec.String
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)
import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Text.Printf
import Data.Foldable (asum)

import Types



-- | Haskell grammar based lexer.
lexer :: P.GenTokenParser String u Identity
lexer = P.makeTokenParser haskellDef


-- | General "Int" parser.
intP :: Parser Int
intP = fromIntegral <$> P.integer lexer



-- | Parse an Int between 0 and 2^16-1 = 65536 (inclusive).
portP :: Parser Int
portP = do p <- intP
           if | p < 0      -> parserFail "Port < 0"
              | p > 2^16-1 -> parserFail "Port > 65535"
              | otherwise  -> return p



-- TODO: This function is entirely untested.
ipv6P :: Parser ([Int], [Int])
ipv6P = do
      ip@(l,r) <- (asum . map try)
            [ ([],) <$> connectedPart -- Full address without omitted zeros
            , ([],) <$>                 (doubleColon *> connectedPart) -- ::1
            , (,[]) <$> connectedPart <* doubleColon                   -- 1::
            , (,)   <$> connectedPart <* doubleColon <*> connectedPart -- 1::2
            ]
      if length (l ++ r) > 8
            then parserFail "IPv6 too long"
            else return ip

      where
            connectedPart :: Parser [Int]
            connectedPart = (:) <$> ipv6NumberP <*> many (colon *> ipv6NumberP)

            doubleColon :: Parser ()
            doubleColon = (void . try) (colon *> colon)



ipv6NumberP :: Parser Int
ipv6NumberP = do p <- intP
                 if | p < 0      -> parserFail "IPv6 part < 0"
                    | p > 0xffff -> parserFail "IPv6 part > 0xffff"
                    | otherwise  -> return p


-- | Parser for an IPv4 address.
ipv4P :: Parser (Int, Int, Int, Int)
ipv4P = (,,,) <$> ipv4NumberP <* dot
              <*> ipv4NumberP <* dot
              <*> ipv4NumberP <* dot
              <*> ipv4NumberP



-- | Parse a literal \".\"
dot :: Parser Char
dot = char '.'



-- | Parse a literal \":\"
colon :: Parser Char
colon = char ':'



-- | Parse an Int between 0 and 255 (inclusive).
ipv4NumberP :: Parser Int
ipv4NumberP = do p <- intP
                 if | p < 0     -> parserFail "IPv4 part < 0"
                    | p > 255   -> parserFail "IPv4 part > 255"
                    | otherwise -> return p


-- | Parser for "To" data.
toP :: Parser To
toP = try toP' <?> errMsg where
      toP' = do (a,b,c,d) <- ipv4P
                let host = printf "%d.%d.%d.%d" a b c d
                colon
                port <- portP
                return (To (Node host port))
      errMsg = "IPv4+port address of the form 127.0.0.1:12345"



parseAddress :: String -> Either ParseError To
parseAddress = parse toP ""