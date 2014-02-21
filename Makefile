axes: axesgen.hs conf
	./axesgen.hs <conf >axes
vis_sample: axes
	./animate.hs conf axes <bound.txt
vis_sample_rt: axes slow_print.hs
	./slow_print.hs 17 <bound.txt | ./animate.hs conf axes
vis_test_play:
	./animateX.hs conf axes < cooked
