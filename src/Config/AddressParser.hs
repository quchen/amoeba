-- | Parses a string of the form @123.123.123.123:54321@ to a "To".

-- TODO: Allow arbitrary hostnames, in particular IPv6 and DNS

{-# LANGUAGE MultiWayIf #-}

module Config.AddressParser where

import Text.Parsec hiding (many, (<|>))
import Text.Parsec.String
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)
import Control.Applicative
import Text.Printf

import Types



-- | Haskell grammar based lexer.
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



-- | Parser for an IPv4 address.
ipv4P :: Parser (Int, Int, Int, Int)
ipv4P = (,,,) <$> ipv4PartP <* dot
              <*> ipv4PartP <* dot
              <*> ipv4PartP <* dot
              <*> ipv4PartP



-- | Parse a literal \".\"
dot :: Parser Char
dot = char '.'



-- | Parse a literal \":\"
colon :: Parser Char
colon = char ':'



-- | Parse an Int between 0 and 255 (inclusive).
ipv4PartP :: Parser Int
ipv4PartP = do p <- intP
               if | p < 0     -> parserFail "IP part < 0"
                  | p > 255   -> parserFail "IP part > 255"
                  | otherwise -> return p


-- | Parser for "To" data.
toP :: Parser To
toP = do (a,b,c,d) <- ipv4P
         let host = printf "%d.%d.%d.%d" a b c d
         colon
         port <- portP
         return (To (Node host port))



parseAddress :: String -> Either ParseError To
parseAddress = parse toP ""