-- | Parses a string of the form @123.123.123.123:54321@ to a 'To'.

-- TODO: Allow arbitrary hostnames, in particular IPv6 and DNS

{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_HADDOCK show-extensions #-}

module Config.AddressParser (parseAddress) where

import Text.Parsec hiding (many, (<|>))
import Text.Parsec.String
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)
import Control.Applicative
import Control.Monad
import Control.Monad.Identity
import Text.Printf

import Types



-- | Haskell grammar based lexer.
lexer :: P.GenTokenParser String u Identity
lexer = P.makeTokenParser haskellDef


-- | General 'Int' parser.
intP :: Parser Int
intP = fromIntegral <$> P.integer lexer



-- | Parse an Int between 0 and 2^16-1 = 65536 (inclusive).
portP :: Parser Int
portP = do p <- intP
           let minPort = 0
               maxPort = 2^(16::Int)-1
           if | p < minPort -> parserFail "Port < 0"
              | p > maxPort -> parserFail "Port > 65535"
              | otherwise  -> return p



-- | Parser for an IPv4 address.
ipv4P :: Parser (Int, Int, Int, Int)
ipv4P = (,,,) <$> ipv4NumberP <* dot
              <*> ipv4NumberP <* dot
              <*> ipv4NumberP <* dot
              <*> ipv4NumberP



-- | Discard a literal \".\"
dot :: Parser ()
dot = void (char '.')



-- | Discard a literal \":\"
colon :: Parser ()
colon = void (char ':')



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