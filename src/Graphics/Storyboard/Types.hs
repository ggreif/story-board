module Graphics.Storyboard.Types where

import Control.Applicative
import Data.Semigroup
import Graphics.Blank (Canvas)

-----------------------------------------------------------------------------

type Size  f = (f,f)
type Coord f = (f,f)

-----------------------------------------------------------------------------

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

-----------------------------------------------------------------------------

data Cavity f = Cavity
  { cavityCorner :: Coord f
  , cavitySize   :: Size f
  , cavitySpacer :: Size f  -- take height *or* width, not both
  }
  deriving Show

data Spacing'
  = Alloc Float    -- take up space
  | AtLeast Float  -- be at least this wide
  | Space'         -- space Filler
  deriving (Eq, Ord, Show)

-----------------------------------------------------------------------------

-- Anchored ??
-- Placed ??

data Filler a = Filler
  { fillerSpace :: [(Spacing',Spacing')]
  , runFiller   :: Cavity Float -> Canvas (a,Cavity Float)
  }

instance Functor Filler where
 fmap f m = pure f <*> m

instance Applicative Filler where
 pure a = Filler [] $ \ sz0 -> return (a,sz0)
 Filler fs f <*> Filler xs x = Filler (fs ++ xs) $ \ sz0 -> do
                    (f',sz1) <- f sz0
                    (x',sz2) <- x sz1
                    return (f' x',sz2)

instance Semigroup a => Semigroup (Filler a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (Filler a) where
  mempty = pure mempty
  mappend = liftA2 mappend

-----------------------------------------------------------------------------