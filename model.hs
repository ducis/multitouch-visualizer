#!runghc
import Graphics.Gloss
--model = 	<width> <height> <number of LEDs horizontally> <number of LEDs vertically>
--			<left offset> <right offset> <bottom offset>
main = getContents>>=display (InWindow "M" (1280,720) (10,10)) white.f.map read.words
f::[Double]->Picture
f _ = Circle 80
