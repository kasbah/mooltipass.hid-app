module DeveloperTab where

import Graphics.Element (..)
import Signal (..)
import Signal
import Text (..)

import Color
import Layout (..)
import CustomGraphics (..)
import Actions (..)
import CommonState (..)

developerTab = flow right [ button (send commonActions (CommonNoOp)) "import"
                          , button (send commonActions (CommonNoOp)) "export"
                          , button (send commonActions (CommonNoOp)) "erase"
                          ]
