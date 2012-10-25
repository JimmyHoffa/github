{-# LANGUAGE OverloadedStrings #-}
module Plane where

type X = Float
type Y = Float
type Direction = Float
type Location = (X, Y)
type Size = (Float, Float)
type TopLeftCorner = Location
type TopRightCorner = Location
type BottomLeftCorner = Location
type BottomRightCorner = Location

data Shape = Rectangle deriving (Eq, Show)

data Corner = RectangleCorners {
    topLeftCorner :: TopLeftCorner,
    topRightCorner :: TopRightCorner,
    bottomRightCorner :: BottomRightCorner,
    bottomLeftCorner :: BottomLeftCorner}

data Artifact = Artifact {
    shape :: Shape,
    location :: Location,
    size :: Size } deriving (Eq, Show)

type Plane = [Artifact]

moveArtifact :: Plane -> Artifact -> Location -> Artifact
moveArtifact plane originalArtifact (moveToX, moveToY)
    | artifactCanGoToLoc = Artifact Rectangle (moveToX, moveToY) $ size originalArtifact
    | otherwise = originalArtifact
    where artifactCorners = corners originalArtifact 
          artifactCanGoToLoc = not $
            topLeftCorner artifactCorners `inside` plane ||
            topRightCorner artifactCorners `inside` plane ||
            bottomRightCorner artifactCorners `inside` plane ||
            bottomLeftCorner artifactCorners `inside` plane

corners :: Artifact -> Corner
corners (Artifact Rectangle (artifactX, artifactY) (artifactW,artifactH)) =
    RectangleCorners 
        ((-) artifactX $ artifactW / 2, (+) artifactY $ artifactH / 2)
        ((+) artifactX $ artifactW / 2, (+) artifactY $ artifactH / 2)
        ((+) artifactX $ artifactW / 2, (-) artifactY $ artifactH / 2)
        ((-) artifactX $ artifactW / 2, (-) artifactY $ artifactH / 2)

inside :: Location -> Plane -> Bool
inside x y = insideAcc False x y

insideAcc :: Bool -> Location -> Plane -> Bool
insideAcc False (locToCheckX, locToCheckY) (Artifact Rectangle (artifactX, artifactY) (artifactW,artifactH):artifacts) =
    insideAcc
    (upperRightX > locToCheckX && 
    lowerLeftX < locToCheckX &&
    upperRightY > locToCheckY &&
    lowerLeftY < locToCheckY)
    (locToCheckX, locToCheckY) artifacts
    where
        upperRightX = (+) artifactX $ artifactW / 2
        upperRightY = (+) artifactY $ artifactH / 2
        lowerLeftX = (-) artifactY $ artifactH / 2
        lowerLeftY = (-) artifactX $ artifactW / 2

insideAcc _ _ _ = True
