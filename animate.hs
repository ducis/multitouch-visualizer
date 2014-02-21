#!runghc
{-# LANGUAGE ScopedTypeVariables #-}
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL.GL
import Data.Word
import Data.Bits
--import Control.Concurrent
main = do
	(_, [confFile,axesFile]) <- getArgsAndInitialize
	initialDisplayMode $= [DoubleBuffered, RGBMode]
	initialWindowSize $= Size 720 720
	initialWindowPosition $= Position 50 50
	createWindow "animate"
	--(nx::GLdouble,ny::GLdouble)<-readFile confFile>>=return.(\(a:b:_)->(read a,read b)).words
	matrixMode $= Projection
	ortho 0 4095 0 4095 (-1) 1
	ax<-readFile axesFile>>=return.read
--	m ax
	displayCallback $= do
		m ax
		swapBuffers
		postRedisplay Nothing
	mainLoop
--	getContents>>=mapM_ (\l->xx ax l>>vis ax l).map (init.map read.words).lines
	where
	m a = getLine>>=xx a.init.map read.words
	xx::[[[Double]]]->[Word32]->IO ()
	xx a l = {- print l >> -} vis a l


vis::[[[Double]]]->[Word32]->IO ()
vis [pxs,pys,ds_x,ds_y] s = do
	clearColor $= (Color4 0.0 0.0 0.0 0.0)
	clear [ ColorBuffer ]
	renderPrimitive Lines $ sequence_ $ map (f.disassemble) s
--	renderPrimitive Lines
--		(vertex (Vertex2 (-0.5::GLdouble) 0)>>vertex (Vertex2 (0.5::GLdouble) 0))
--	flush
	postRedisplay Nothing
--	swapBuffers
	--print pns
	flush>>finish
	where
	pds::[([[Double]],[Double])]
	pds = (uncurry zip) $ (\(ps,ds)->(ps, map g ds)) $ unzip
		( zip (repeat pxs) ds_x ++ zip (repeat pys) ds_y )
		where
		g::[Double]->[Double]
		g v = map (/(sqrt $ sum $ map (^2) v)) v
	disassemble x = (x.&.0xFF, (x `shiftR` 8) .&. 0xFFF, x `shiftR` 20)
	f (a,b,e) = mapM_ (\[x,y]->vertex (Vertex2 (realToFrac x::GLfloat) (realToFrac y)))
		(seg b++seg e)
		where
		seg s = [p'0,p'1]
			where 
			p'0 = ((p1 .* (fromIntegral s/4095)) .+ (p0 .* (1-fromIntegral s/4095)))
			p'1 = p'0 .+ (d.*40000)
		v .* s = map (*s) v
		v1 .+ v2 = zipWith (+) v1 v2
		([p0,p1],d) = pds !! fromIntegral a
