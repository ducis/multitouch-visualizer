#!runghc
{-# LANGUAGE ScopedTypeVariables #-}
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL.GL
import Graphics.Rendering.OpenGL.Raw.ARB.Compatibility
import Graphics.Rendering.OpenGL.Raw
import Data.Word
import Data.Bits
import Data.List
import GHC.IO.Handle
import GHC.IO.Handle.FD
import Control.Monad
--import Control.Concurrent
main = do
	(_, (confFile:axesFile:flags)) <- getArgsAndInitialize
	initialDisplayMode $= [DoubleBuffered, RGBMode]
	initialWindowSize $= Size 1280 720
	initialWindowPosition $= Position 50 50
	createWindow ("animate"++unwords flags)
	--(nx::GLdouble,ny::GLdouble)<-readFile confFile>>=return.(\(a:b:_)->(read a,read b)).words
	matrixMode $= Projection
	ortho 0 4095 0 4095 (-1) 1
	ax<-readFile axesFile>>=return.read
--	m ax
	displayCallback $= do
		m flags ax
		swapBuffers
		postRedisplay Nothing
	keyboardMouseCallback $= Just (\k _ _ _->when (k==MouseButton RightButton) (clear[ ColorBuffer ]))
	mainLoop
--	getContents>>=mapM_ (\l->xx ax l>>vis ax l).map (init.map read.words).lines
	where
	m flags a = sequence (take 3 $ repeat getLine)>>=vis flags a -- xx a.init.map read.words

vis::[String]->[[[Double]]]->[String]->IO ()
vis flags [pxs,pys,ds_x,ds_y] [bnd',quad',polygon'] = 
	(if "-accum" `elem` flags then do
		renderPrimitive Points $ mapM_ (\[x,y,_,_,c]->let
			f=glColor3f
			cc	| c<1 = f 0 0 1
				| c<10000 = f 0 1 0
				| otherwise = f 1 0 0
			in
			cc>>vertex (Vertex2 (x::GLfloat) y) ) quads
	else do
		clearColor $= (Color4 0 0 0.3 0)
		when (not $ "-accum" `elem` flags) $ clear [ ColorBuffer ]
		glEnable gl_BLEND
		glBlendFunc gl_SRC_ALPHA gl_DST_ALPHA
		glColor4f 1 1 1 0.1
	--	let act = sequence_ $ map (f.disassemble) $ init ( map read (words bnd')::[Word32] ) in
	--		renderPrimitive Lines act >> glDisable gl_BLEND >> glColor3f 1 0 0 >>renderPrimitive Lines act
		glColor4f 0 1 0 1
		mapM_ (renderPrimitive Polygon . mapM_ (\(x,y)->vertex (Vertex2 (x::GLfloat) y))) polygons
		glColor4f 1 1 1 1
		(renderPrimitive Lines 
			. mapM_ (\(x,y)->vertex (Vertex2 (x::GLfloat) y)) 
			. concat . map ((\[a,b,c,d]->[a,b,b,c,c,d,d,a])
			.(\[x0,y0,x1,y1]->[(x0,y0),(x1,y0),(x1,y1),(x0,y1)])
			.(\[x,y,xr,yr,_]->[x-xr,y-yr,x+xr,y+yr])))
			quads
		print (length polygons, length quads)
		hFlush stdout) >>postRedisplay Nothing>>flush>>finish
	where
	polygons = read polygon'
	quads::[[GLfloat]] = (if "-magic2" `elem` flags then magic2 else id) $ g $ map read $ words quad' where 
		g (a:b:c:d:e:ss) = [a,b,c,d,e]:g ss
		g [] = []
		magic2 [[x0,y0,xr0,yr0,_],[x1,y1,xr1,yr1,_]] = [[x0,y0-yr0+xr0',xr0,xr0'],[x1,y1+yr1-xr1',xr1,xr1']] where { xr0'=xr0*16/9 ; xr1'=xr1*16/9 }
		magic2 x = x
	pds::[([[Double]],[Double])]
	pds = (uncurry zip) $ (\(ps,ds)->(ps, map g ds)) $ unzip
		( zip (repeat pxs) ds_x ++ zip (repeat pys) ds_y )
		where
		g::[Double]->[Double]
		g v = map (/(sqrt $ sum $ map (^2) v)) v
	disassemble x = (x.&.0xFF, (x `shiftR` 8) .&. 0xFFF, x `shiftR` 20)
	f (a,b,e) = mapM_ (\[x,y]->vertex (Vertex2 (realToFrac x::GLfloat) (realToFrac y)))
		(seg b++reverse (seg e))
		where
		seg s = [p'0,p'1]
			where 
			p'0 = ((p1 .* (fromIntegral s/4095)) .+ (p0 .* (1-fromIntegral s/4095)))
			p'1 = p'0 .+ (d.*40000)
		v .* s = map (*s) v
		v1 .+ v2 = zipWith (+) v1 v2
		([p0,p1],d) = pds !! fromIntegral a
	
