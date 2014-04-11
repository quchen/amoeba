{-# LANGUAGE TemplateHaskell #-}

-- For makeFields
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}


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


$(makeFields ''PoolConfig     )
$(makeFields ''BootstrapConfig)
$(makeFields ''MultiConfig    )
$(makeFields ''DrawingConfig  )