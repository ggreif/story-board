{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Graphics.Storyboard.Images where

import           Data.Monoid ((<>))
import           Data.Text (pack)

import           Graphics.Blank as Blank
import           Graphics.Storyboard.Tile
import           Graphics.Storyboard.Types


-- | Load an image; do not place it anywhere yet.
imageTile :: MonadCanvas m => FilePath -> m (Tile ())
imageTile filePath = liftCanvas $ do
    -- From the canvas' point of view, we need to fix the absolute path with "/"
    img <- newImage ("/" <> pack filePath)
    return $ tile (width img, height img) $ \ (Cavity _ _) -> drawImage (img,[0,0])

-- | Load an scaled image; do not place it anywhere yet.
scaledImageTile :: MonadCanvas m => FilePath -> Double -> m (Tile ())
scaledImageTile filePath s = liftCanvas $ do
    -- From the canvas' point of view, we need to fix the absolute path with "/"
    img <- newImage ("/" <> pack filePath)
    return $ tile (width img * s, height img * s) $ \ (Cavity _ _) -> saveRestore $ do
          scale (s,s)
          drawImage (img,[0,0])
