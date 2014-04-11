-- | Types and typeclasses to modify a given configuration

module Config.OptionModifier where

import Data.Monoid
import Control.Lens

import Types.Lens


-- | Represents a modification of a configuration type.
newtype OptionModifier a = OptionModifier { applyOptionModifier :: a -> a }

-- mappend applies the modifiers from left to right, i.e. the rightmost
-- modifier has the final say. Equivalent to `Dual (Endo a)`.
instance Monoid (OptionModifier a) where
      mempty = OptionModifier id
      mappend (OptionModifier x) (OptionModifier y) = OptionModifier (y . x)



-- | Lift a modifier of a subfield to a modifier of the entire field.
--
--   For example, a 'BootstrapConfig' contains a 'NodeConfig'.
--   @liftModifier nodeConfig@ will then be a modifier for a 'BootstrapConfig'
--   defined by modifying its 'NodeConfig' subfield.
liftModifier :: Setting (->) b b a a
             -> OptionModifier a
             -> OptionModifier b
liftModifier l (OptionModifier f) = OptionModifier (l %~ f)