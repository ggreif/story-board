{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Diagrams.Backend.Canvas
import           Diagrams.Prelude as Dia

import qualified Graphics.Storyboard as SB
import           Graphics.Storyboard.Diagrams()

main :: IO ()
--main = SB.storyBoard 3000 [ slide, slide2 ]
main = SB.storyBoard 3000 [ slide2 ]

slide :: SB.Slide ()
slide = do
      SB.align SB.center $ SB.p $ "Diagrams Plugin Example"

      let t = SB.drawTile (200,200) dig

      SB.place SB.top (SB.nudge SB.top SB.center t)

      let t' = SB.drawMovieTile (200,200) (pad 1.05 . tournament . succ. (`mod` 10) . (floor :: (Double -> Int)))

      SB.place SB.top (SB.nudge SB.top SB.center t')

      return ()

dig :: Diagram B R2
dig = tournament 6

node :: Int -> Diagram B R2
node n = text (show n) # fontSizeN 0.1 # fc white
      <> circle 0.2 # fc green # named n

arrowOpts :: ArrowOpts
arrowOpts = with & gaps  .~ small
                 & headLength .~ Global 0.2

tournament :: Int -> Diagram B R2
tournament n = decorateTrail (regPoly n 1) (map node [1..n])
  # applyAll [connectOutside' arrowOpts j k | j <- [1 .. n-1], k <- [j+1 .. n]]

--default (Double)

slide2 :: SB.Slide ()
slide2 = do
      SB.align SB.center $ SB.p $ "Diagrams Plugin Example2"

      let t = SB.drawTile (200,200) (pic # applyAll [connectOutside' arrowOpts (0 :: Int) (1 :: Int)])
          pic :: Diagram B R2
          pic = (r <> vrule 1 <> (c1 === strutY 1 === c0)) ||| hrule 1
          r = square 5
          c = circle 0.2 # fc red
          c0 = c # named (0 :: Int)
          c1 = c # named (1 :: Int)

      SB.place SB.top (SB.nudge SB.top SB.center t)

      return ()
