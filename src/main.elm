import Text(..)
import Graphics.Element(..)
import Graphics.Collage(..)
import Color(..)
import Signal(..)
import Mouse
import Window

background (w',h') = let (w,h) = (toFloat w', toFloat h')
                     in collage w' h' [filled green <| square 64]

main = background <~ Window.dimensions
