-- | Main module for the multi client executable.
--
--   Tiny wrapper to isolate the Amoeba library and its dependencies from the
--   Amoeba executables, which then only depend on Base and the Amoeba library,
--   while the latter contains all the transitive dependencies.

module Main.MultiExecutable (main) where

import qualified Main.Multi as M

main = M.main