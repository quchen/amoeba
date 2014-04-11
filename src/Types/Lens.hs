{-# LANGUAGE TemplateHaskell #-}


-- | Defines optics (lens library) for various Amoeba types. Intended to be
--   imported qualified.
module Types.Lens where

import Control.Lens
import Types

$(makeLenses ''Environment    )
$(makeLenses ''NodeConfig     )
$(makeLenses ''PoolConfig     )
$(makeLenses ''BootstrapConfig)
$(makeLenses ''MultiConfig    )
$(makeLenses ''DrawingConfig  )