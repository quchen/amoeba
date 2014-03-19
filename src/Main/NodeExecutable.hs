-- | Main module for the single client executable.
--
--   Tiny wrapper to isolate the Amoeba library and its dependencies from the
--   Amoeba executables, which then only depend on Base and the Amoeba library,
--   while the latter contains all the transitive dependencies.

module Main (main) where

import qualified Main.Node as M

main = M.main