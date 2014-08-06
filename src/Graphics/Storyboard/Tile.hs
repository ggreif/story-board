{-# LANGUAGE KindSignatures, TemplateHaskell, GADTs, GeneralizedNewtypeDeriving, InstanceSigs, OverloadedStrings #-}

module Graphics.Storyboard.Tile where

import qualified Data.Text as Text
import Data.Text(Text)
import Data.List as List
import Control.Applicative
import Control.Monad (liftM2)
import Data.Semigroup
import Data.Text(Text)
import Graphics.Blank (Canvas)
import Control.Monad.IO.Class
import Control.Lens (makeLenses)

import GHC.Exts (IsString(fromString))

import Graphics.Storyboard.Types.Basic
import Graphics.Storyboard.Literals

import Graphics.Blank

-- | A Tile has a specific, fixed size.
-- When rendered, it is given a specific size to operate inside of,
-- that typically would be *at least* the size of the original fixed size.
-- The tile can choose to put any extra space on the inside or outside
-- of any border, etc.

data Tile a = Tile (Size Float) (Size Float -> Canvas a)

instance Show (Tile a) where
  show (Tile sz _) = show sz

-- | tile requests a specific (minimum) size, and provides
-- a paint routine that takes the *actual* size.
-- The paint routine can assume the canvas starts at (0,0),
-- and is the given size. No masking is done by default.

tile :: Size Float -> (Size Float -> Canvas a) -> Tile a
tile = Tile

tileWidth :: Tile a -> Float
tileWidth (Tile (w,_) _) = w

tileHeight :: Tile a -> Float
tileHeight (Tile (_,h) _) = h

tileSize :: Tile a -> Size Float
tileSize (Tile sz _) = sz

blank :: Size Float -> Tile ()
blank sz = tile sz $ const $ return ()

-- nudge the tile into a specific corner of its enclosure
nudge :: Horizontal -> Vertical -> Tile a -> Tile a
nudge hor ver (Tile (w,h) f) = Tile (w,h) $ \ (w',h') ->
    let w'' = case hor of
                HL -> 0
                HC -> (w' - w) / 2
                HR -> w' - w
        h'' = case ver of
                VT -> 0
                VC -> (h' - h) / 2
                VB -> h' - h
    in
       saveRestore $ do
         translate (w'',h'') -- nudge
         f (w,h)             -- and pretend there is no extra space



instance Semigroup a => Semigroup (Tile a) where
  (Tile (x1,y1) c1) <> (Tile (x2,y2) c2) = Tile (max x1 x2,max y1 y2) $ \ sz ->
        do r1 <- c1 sz
           r2 <- c2 sz -- overlay is the default monoid
           return (r1 <> r2)

instance Monoid a => Monoid (Tile a) where
  mempty = Tile (0,0) (return mempty)
  (Tile (x1,y1) c1) `mappend` (Tile (x2,y2) c2) = Tile (max x1 x2,max y1 y2) $ \ sz ->
      do r1 <- c1 sz
         r2 <- c2 sz -- overlay is the default monoid
         return (r1 `mappend` r2)