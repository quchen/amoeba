{-# LANGUAGE TemplateHaskell #-}

-- For makeFields
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_HADDOCK show-extensions #-}


-- | Defines optics (lens library) for various Amoeba types. Intended to be
--   imported qualified.
--
--   This module is predominantly generated automatically by Template Haskell.
--   For documentation on what the different fields do, have a look at the
--   documentation in the other 'Types' modules.
module Types.Lens where

import Control.Lens
import Types
import Utilities.IOQueue



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
makeIso ''IOQueue
makeIso ''Microseconds
makeIso ''Timestamp