{- |

    This module allows you to use general literals like 'left' and 'center',
    and the overloading will resove to the appropreate type.
 -}
module Graphics.Storyboard.Literals where

-----------------------------------------------------------------------------

-- Short cut literal/DSL-keyword classes
class LR a where
  left :: a
  right :: a

class TB a where
  top :: a
  bottom :: a

class Center a where
  center :: a

-----------------------------------------------------------------------------

data Side       = T | B | L | R
  deriving (Eq,Ord,Show)

instance LR Side where
  left = L
  right = R

instance TB Side where
  top = T
  bottom = B

-----------------------------------------------------------------------------

data Vertical   = VT | VC | VB
  deriving (Eq,Ord,Show)

instance TB Vertical where
  top = VT
  bottom = VB

instance Center Vertical where
  center = VC

-----------------------------------------------------------------------------

data Horizontal = HL | HC | HR
  deriving (Eq,Ord,Show)

instance LR Horizontal where
  left = HL
  right = HR

instance Center Horizontal where
  center = HC

-----------------------------------------------------------------------------
-- http://en.wikipedia.org/wiki/Typographic_alignment

data Alignment = JustLeft | JustCenter | JustRight | Justified | Truncated
  deriving (Eq,Ord,Show)

instance LR Alignment where
  left = JustLeft
  right = JustRight

instance Center Alignment where
  center = JustCenter

justified :: Alignment
justified = Justified

truncated :: Alignment
truncated = Truncated
