-- | Types and typeclasses to modify a given configuration

module Config.OptionModifier where

import Types
import Data.Monoid



-- | Represents a modification of a configuration type.
newtype OptionModifier a = OptionModifier { applyOptionModifier :: a -> a }

-- mappend applies the modifiers from left to right, i.e. the rightmost
-- modifier has the final say. Equivalent to `Dual (Endo a)`.
instance Monoid (OptionModifier a) where
      mempty = OptionModifier id
      mappend (OptionModifier x) (OptionModifier y) = OptionModifier (y . x)



class HasNodeConfig a where

      -- | Accessor to the contained "NodeConfig"
      _nodeConfig :: a -> NodeConfig

      -- | Lift a modifier for the contained "NodeConfig" to a modifier of the
      --   container
      liftNodeModifier :: OptionModifier NodeConfig -> OptionModifier a

-- There's intentionally no instance for Config itself, because all calls to it
-- would be redundant and noise up the code.

instance HasNodeConfig BootstrapConfig where
      _nodeConfig = _bootstrapNodeConfig
      liftNodeModifier (OptionModifier x) = OptionModifier
            ( \c -> c { _bootstrapNodeConfig = x (_bootstrapNodeConfig c) } )

instance HasNodeConfig MultiConfig where
      _nodeConfig = _multiNodeConfig
      liftNodeModifier (OptionModifier x) = OptionModifier
            ( \c -> c { _multiNodeConfig = x (_multiNodeConfig c) } )

instance HasNodeConfig DrawingConfig where
      _nodeConfig = _drawingNodeConfig
      liftNodeModifier (OptionModifier x) = OptionModifier
            ( \c -> c { _drawingNodeConfig = x (_drawingNodeConfig c) } )



class HasPoolConfig a where

      -- | Accessor to the contained "PoolConfig"
      _poolConfig :: a -> PoolConfig

      -- | Lift a modifier for the contained "PoolConfig" to a modifier of the
      --   container
      liftPoolModifier :: OptionModifier PoolConfig -> OptionModifier a

instance HasPoolConfig BootstrapConfig where
      _poolConfig = _bootstrapPoolConfig
      liftPoolModifier (OptionModifier x) = OptionModifier
            ( \c -> c { _bootstrapPoolConfig = x (_bootstrapPoolConfig c) } )

instance HasPoolConfig MultiConfig where
      _poolConfig = _multiPoolConfig
      liftPoolModifier (OptionModifier x) = OptionModifier
            ( \c -> c { _multiPoolConfig = x (_multiPoolConfig c) } )

instance HasPoolConfig DrawingConfig where
      _poolConfig = _drawingPoolConfig
      liftPoolModifier (OptionModifier x) = OptionModifier
            ( \c -> c { _drawingPoolConfig = x (_drawingPoolConfig c) } )