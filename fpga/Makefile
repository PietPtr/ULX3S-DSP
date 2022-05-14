

ecppack:
	ecppack ulx3s/ulx3s_out.config ulx3s/bitstream.bit

nextpnr:
	nextpnr-ecp5 --85k --json ulx3s/project.json \
		--lpf ulx3s/ulx3s_v20.lpf \
		--textcfg ulx3s/ulx3s_out.config 

yosys:
	yosys ulx3s/project.ys 

clash:
	clash -isrc -outputdir .hs -fclash-hdldir hdl --verilog src/Main.hs

all: clash yosys nextpnr ecppack

clean:
	-rm .hs -r
	-rm ulx3s/ulx3s_out.config
	-rm ulx3s/project.json
	-rm ulx3s/bitstream.bit

flash:
	fujprog ulx3s/bitstream.bit