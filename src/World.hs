{-# LANGUAGE OverloadedStrings #-}
module World where

import qualified Data.Map.Strict as M
import Types 

frontHallDescription :: Description
frontHallDescription =
    "Light shines down on you through lace curtained window. The oaken floor\
    \creaks as you adjust your weight. Dust piles up in the corners and you\
    \sense a great tragedy occured here once, many years ago\
    \To the north you see a door to the kitchen."

frontHall :: Room
frontHall = Room
    { roomName = "The Fronthall"
    , roomDescription = frontHallDescription
    , roomRoomId = 1
    , roomAdjacent = M.fromList [(N, 2)]
    }

kitchenDescription :: Description
kitchenDescription = 
    "You are standing in kitchen. The floor is tiled with black and white\
    \linoleum. There is a propane stove, and a Kitchenmade Refrigerator."

kitchen :: Room
kitchen = Room 
    { roomName = "The Kitchen"
    , roomDescription = kitchenDescription
    , roomRoomId = 2
    , roomAdjacent = M.fromList [(S, 1)]
    }

world :: World
world = M.fromList [(1, kitchen), (2, frontHall)]
