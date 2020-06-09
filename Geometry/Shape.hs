module Geometry.Shape
( Point(..)
, Shape(..)
, baseCircle
, baseRectangle
, area
, nudge
) where

data Point = Point Float Float deriving (Show)

data Shape = Circle Point Float | Rectangle Point Point
    deriving (Show)


baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRectangle :: Float -> Float -> Shape
baseRectangle width height = Rectangle (Point 0 0) (Point width height)

area :: Shape -> Float
area (Circle (Point _ _) r) = pi * r ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) xd yd = Circle (Point (x + xd) (y + yd)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) xd yd = Rectangle (Point (x1 + xd) (y1 + yd)) (Point (x2 + xd) (y2 + yd))
