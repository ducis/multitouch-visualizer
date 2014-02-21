#!runghc
main=interact (show.f.filter (/=[]).parse)
parse = map (map read.words).lines
f::[[Int]]->[[[Int]]]
f [[phy_x,n_x,margin_x_in_pu],[phy_y,n_y,margin_y_in_pu],as_XBar,as_YBar] = 
	[
		[[0,t],[sx,t]],
		[[l,0],[l,sy]],
		map (\dx->[cx dx, -(sy+2*exty)]) as_XBar,
		map (\dy->[sx+2*extx, cy dy]) as_YBar
	]
	where 
	cx x = round (fI x*fI sx/fI n_x)
	cy y = round (fI y*fI sy/fI n_y)
	fI = fromIntegral
	sx = 4095
	sy = 4095
	extx = round $ fI margin_y_in_pu/fI phy_x*fI sx
	exty = round $ fI margin_x_in_pu/fI phy_y*fI sy
	l = -extx
	r = sx+extx
	b = -exty
	t = sy+exty
