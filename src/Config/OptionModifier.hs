-- | Types and typeclasses to modify a given configuration

module Config.OptionModifier where

import Types
import Data.Monoid

import qualified Types.Lens as L
import Control.Lens


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
      _nodeConfig      = _bootstrapNodeConfig
      liftNodeModifier = liftModifier L.bootstrapNodeConfig

instance HasNodeConfig MultiConfig where
      _nodeConfig      = _multiNodeConfig
      liftNodeModifier = liftModifier L.multiNodeConfig

instance HasNodeConfig DrawingConfig where
      _nodeConfig      = _drawingNodeConfig
      liftNodeModifier = liftModifier L.drawingNodeConfig



class HasPoolConfig a where

      -- | Accessor to the contained "PoolConfig"
      _poolConfig :: a -> PoolConfig

      -- | Lift a modifier for the contained "PoolConfig" to a modifier of the
      --   container
      liftPoolModifier :: OptionModifier PoolConfig -> OptionModifier a

instance HasPoolConfig BootstrapConfig where
      _poolConfig      = _bootstrapPoolConfig
      liftPoolModifier = liftModifier L.bootstrapPoolConfig

instance HasPoolConfig MultiConfig where
      _poolConfig      = _multiPoolConfig
      liftPoolModifier = liftModifier L.multiPoolConfig

instance HasPoolConfig DrawingConfig where
      _poolConfig      = _drawingPoolConfig
      liftPoolModifier = liftModifier L.drawingPoolConfig



-- | Lift a modifier of a subfield to a modifier of the entire field.
--
--   For example, a 'BootstrapConfig' contains a 'NodeConfig'.
--   @liftModifier nodeConfig@ will then be a modifier for a 'BootstrapConfig'
--   defined by modifying its 'NodeConfig' subfield.
liftModifier :: Setting (->) b b a a
             -> OptionModifier a
             -> OptionModifier b
liftModifier l (OptionModifier f) = OptionModifier (l %~ f)