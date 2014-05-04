{-# LANGUAGE TemplateHaskell #-}

-- For makeFields
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_HADDOCK show-extensions #-}


-- | Defines optics (lens library) for various Amoeba types. Intended to be
--   imported qualified.
module Types.Lens where

import Control.Lens
import Types


-- Various types
makeLenses ''BootstrapConfig
makeLenses ''Client
makeLenses ''DrawingConfig
makeLenses ''Environment
makeLenses ''MultiConfig
makeLenses ''Node
makeLenses ''NodeConfig
makeLenses ''PChan
makeLenses ''PoolConfig


-- Types with overloaded fields, e.g. "has node config subfield"
makeFields ''BootstrapConfig
makeFields ''DrawingConfig
makeFields ''MultiConfig

-- Isomorphisms (newtypes)
timestamp :: Iso' Double Timestamp
timestamp = iso f b where
      f t = Timestamp t
      b (Timestamp t) = t

makeIso ''Microseconds