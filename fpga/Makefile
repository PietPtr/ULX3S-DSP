

bitstream: config
	ecppack ulx3s_out.config ulx3s.bit

config: json
	nextpnr-ecp5 --85k --json project.json \
		--lpf ulx3s_v20.lpf \
		--textcfg ulx3s_out.config 

json: clash
	yosys-ulx project.ys 

clash:
	clash -isrc --verilog src/Main.hs