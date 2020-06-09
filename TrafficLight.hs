module TrafficLight
(TrafficLight(..)
) where

import YesNo

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False

instance Show TrafficLight where
    show Red = "Red light"
    show Green = "Green light"
    show Yellow = "Yellow light"

instance YesNo TrafficLight where
    yesno Red = False
    yesno _ = True
