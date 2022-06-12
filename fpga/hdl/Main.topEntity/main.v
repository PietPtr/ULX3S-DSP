/* AUTOMATICALLY GENERATED VERILOG-2001 SOURCE CODE.
** GENERATED BY CLASH 1.4.6. DO NOT MODIFY.
*/
`timescale 100fs/100fs
module main
    ( // Inputs
      input  clk_25mhz // clock
    , input  reset // reset
    , input  enable // enable
    , input  adc_miso

      // Outputs
    , output wire  adc_csn
    , output wire  adc_mosi
    , output wire  adc_sclk
    , output wire [3:0] led
    , output wire  adc_miso_monitor
    );
  // src/ADC.hs:(18,1)-(19,57)
  reg [7:0] ctr = 8'd0;
  wire  c$bindCsr;
  // src/ADC.hs:(43,1)-(44,57)
  reg [39:0] c$ds1_app_arg = {2'b10,38'bxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx};
  // src/ADC.hs:(43,1)-(44,57)
  wire signed [63:0] c$wild2_app_arg;
  wire [15:0] result_0;
  // src/ADC.hs:(43,1)-(44,57)
  wire [16:0] c$ds1_case_alt;
  // src/ADC.hs:(43,1)-(44,57)
  wire [16:0] c$ds1_app_arg_0;
  // src/ADC.hs:(43,1)-(44,57)
  wire [1:0] c$setting_case_alt;
  // src/ADC.hs:(43,1)-(44,57)
  wire [1:0] c$setting_app_arg;
  wire  c$app_arg;
  wire [15:0] c$app_arg_0;
  reg [15:0] result_1 = 16'd0;
  wire [14:0] result_2;
  // src/ADC.hs:(43,1)-(44,57)
  wire [15:0] a1;
  // src/ADC.hs:(43,1)-(44,57)
  wire signed [63:0] wild2;
  // src/ADC.hs:(43,1)-(44,57)
  wire  ready;
  // src/ADC.hs:(43,1)-(44,57)
  reg [1:0] setting = 2'd0;
  // src/ADC.hs:(43,1)-(44,57)
  wire [16:0] iM;
  wire [60:0] c$case_alt;
  reg [16:0] c$app_arg_1;
  reg [16:0] c$case_alt_0;
  reg  c$app_arg_2;
  wire signed [63:0] c$app_arg_3;
  reg  c$app_arg_4;
  reg  c$case_alt_1;
  reg  c$case_alt_2;
  reg  c$case_alt_3;
  reg  c$app_arg_5;
  reg  c$case_alt_4;
  reg  c$app_arg_6;
  reg [39:0] c$app_arg_7;
  wire [39:0] c$case_alt_5;
  reg [39:0] c$case_alt_6;
  reg [39:0] c$case_alt_7;
  // src/ADC.hs:100:1-9
  wire [15:0] w;
  reg [39:0] c$case_alt_8;
  wire signed [63:0] c$app_arg_8;
  wire [47:0] c$vecFlat;
  wire [3:0] c$case_alt_selection_1;
  wire [63:0] c$i_14;
  wire [15:0] c$i_17;
  wire [1:0] c$case_alt_selection_4;
  wire [3:0] c$case_alt_selection_7;
  wire [1:0] c$case_alt_selection_10;
  wire [1:0] c$case_alt_selection_13;
  wire [3:0] c$case_alt_selection_17;
  wire [3:0] c$case_alt_selection_20;
  wire [1:0] c$case_alt_selection_23;
  wire [11:0] c$bv;
  wire [7:0] result;

  // register begin
  always @(posedge clk_25mhz or  posedge  reset) begin : ctr_register
    if ( reset) begin
      ctr <= 8'd0;
    end else if (enable) begin
      ctr <= (ctr + 8'd1);
    end
  end
  // register end

  assign c$bindCsr = ctr == 8'd0;

  // register begin
  always @(posedge clk_25mhz or  posedge  reset) begin : c$ds1_app_arg_register
    if ( reset) begin
      c$ds1_app_arg <= {2'b10,38'bxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx};
    end else if (c$bindCsr) begin
      c$ds1_app_arg <= c$case_alt[60:21];
    end
  end
  // register end

  assign c$wild2_app_arg = $unsigned({{(64-2) {1'b0}},setting});

  assign c$vecFlat = {16'd32772,   16'd0,
                      16'd32767};

  // index begin
  wire [15:0] vecArray [0:3-1];
  genvar i;
  generate
  for (i=0; i < 3; i=i+1) begin : mk_array
    assign vecArray[(3-1)-i] = c$vecFlat[i*16+:16];
  end
  endgenerate
  assign result_0 = vecArray[(wild2)];
  // index end

  assign c$ds1_case_alt = (setting == 2'd3) ? {1'b1,16'd2730} : {1'b1,result_0};

  assign c$ds1_app_arg_0 = ready ? c$ds1_case_alt : {1'b0,16'bxxxxxxxxxxxxxxxx};

  assign c$setting_case_alt = (setting < 2'd3) ? (setting + 2'd1) : setting;

  assign c$setting_app_arg = ready ? c$setting_case_alt : setting;

  assign c$app_arg = iM[16:16] ? 1'b1 : 1'b0;

  assign c$app_arg_0 = iM[16:16] ? a1 : ({16 {1'bx}});

  // register begin
  always @(posedge clk_25mhz or  posedge  reset) begin : result_1_register
    if ( reset) begin
      result_1 <= 16'd0;
    end else if (c$bindCsr & c$app_arg) begin
      result_1 <= c$app_arg_0;
    end
  end
  // register end

  assign result_2 = {c$case_alt[18:18],
                     c$case_alt[17:17],   c$case_alt[19:19],
                     result_1[0+:12]};

  assign a1 = iM[15:0];

  assign wild2 = $signed(c$wild2_app_arg);

  assign ready = c$case_alt[20:20];

  // register begin
  always @(posedge clk_25mhz or  posedge  reset) begin : setting_register
    if ( reset) begin
      setting <= 2'd0;
    end else if (c$bindCsr) begin
      setting <= c$setting_app_arg;
    end
  end
  // register end

  assign iM = c$case_alt[16:0];

  assign c$case_alt = {c$app_arg_7,
                       {c$app_arg_6,   c$app_arg_5,   c$app_arg_4,
                        c$app_arg_2,   c$app_arg_1}};

  always @(*) begin
    case(c$ds1_app_arg[39:38])
      2'b00 : c$app_arg_1 = c$case_alt_0;
      default : c$app_arg_1 = {1'b0,16'bxxxxxxxxxxxxxxxx};
    endcase
  end

  assign c$case_alt_selection_1 = c$ds1_app_arg[35:32];

  always @(*) begin
    case(c$case_alt_selection_1)
      4'd0 : c$case_alt_0 = {1'b1,c$ds1_app_arg[15:0]};
      default : c$case_alt_0 = {1'b0,16'bxxxxxxxxxxxxxxxx};
    endcase
  end

  assign c$i_14 = 64'd0;

  always @(*) begin
    case(c$ds1_app_arg[39:38])
      2'b00 : c$app_arg_2 = c$i_14[0] ? 1'bx : c$app_arg_3[0];
      default : c$app_arg_2 = 1'b0;
    endcase
  end

  assign c$i_17 = (c$ds1_app_arg[31:16] >> (64'sd15));

  assign c$app_arg_3 = $unsigned({{(64-16) {1'b0}},c$i_17});

  always @(*) begin
    case(c$ds1_app_arg[39:38])
      2'b00 : c$app_arg_4 = c$case_alt_1;
      2'b01 : c$app_arg_4 = 1'b1;
      default : c$app_arg_4 = 1'b1;
    endcase
  end

  assign c$case_alt_selection_4 = c$ds1_app_arg[37:36];

  always @(*) begin
    case(c$case_alt_selection_4)
      2'd3 : c$case_alt_1 = c$case_alt_2;
      default : c$case_alt_1 = c$case_alt_3;
    endcase
  end

  assign c$case_alt_selection_7 = c$ds1_app_arg[35:32];

  always @(*) begin
    case(c$case_alt_selection_7)
      4'd15 : c$case_alt_2 = 1'b1;
      default : c$case_alt_2 = c$case_alt_3;
    endcase
  end

  assign c$case_alt_selection_10 = c$ds1_app_arg[37:36];

  always @(*) begin
    case(c$case_alt_selection_10)
      2'd2 : c$case_alt_3 = c$ds1_app_arg[35:32] == 4'd15;
      default : c$case_alt_3 = 1'b0;
    endcase
  end

  always @(*) begin
    case(c$ds1_app_arg[39:38])
      2'b00 : c$app_arg_5 = c$case_alt_4;
      default : c$app_arg_5 = 1'b0;
    endcase
  end

  assign c$case_alt_selection_13 = c$ds1_app_arg[37:36];

  always @(*) begin
    case(c$case_alt_selection_13)
      2'd0 : c$case_alt_4 = 1'b0;
      2'd1 : c$case_alt_4 = 1'b1;
      2'd2 : c$case_alt_4 = 1'b1;
      2'd3 : c$case_alt_4 = 1'b0;
      default : c$case_alt_4 = {1 {1'bx}};
    endcase
  end

  always @(*) begin
    case(c$ds1_app_arg[39:38])
      2'b00 : c$app_arg_6 = 1'b0;
      2'b01 : c$app_arg_6 = 1'b0;
      default : c$app_arg_6 = 1'b1;
    endcase
  end

  always @(*) begin
    case(c$ds1_app_arg[39:38])
      2'b00 : c$app_arg_7 = c$case_alt_7;
      2'b01 : c$app_arg_7 = c$case_alt_6;
      default : c$app_arg_7 = c$case_alt_5;
    endcase
  end

  assign c$case_alt_5 = c$ds1_app_arg_0[16:16] ? {2'b00,2'd3,4'd15,w,16'd0} : {2'b10,38'bxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx};

  assign c$case_alt_selection_17 = c$ds1_app_arg[37:34];

  always @(*) begin
    case(c$case_alt_selection_17)
      4'd0 : c$case_alt_6 = {2'b10,38'bxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx};
      default : c$case_alt_6 = {2'b01,c$ds1_app_arg[37:34] - 4'd1,34'bxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx};
    endcase
  end

  assign c$case_alt_selection_20 = c$ds1_app_arg[35:32];

  always @(*) begin
    case(c$case_alt_selection_20)
      4'd0 : c$case_alt_7 = {2'b01,4'd15,34'bxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx};
      default : c$case_alt_7 = c$case_alt_8;
    endcase
  end

  assign w = c$ds1_app_arg_0[15:0];

  assign c$case_alt_selection_23 = c$ds1_app_arg[37:36];

  always @(*) begin
    case(c$case_alt_selection_23)
      2'd0 : c$case_alt_8 = {2'b00,2'd3,c$ds1_app_arg[35:32] - 4'd1,c$ds1_app_arg[31:16] << (64'sd1),(c$ds1_app_arg[15:0] << (64'sd1)) | ($unsigned(c$app_arg_8[0+:16]))};
      default : c$case_alt_8 = {2'b00,c$ds1_app_arg[37:36] - 2'd1,c$ds1_app_arg[35:32],c$ds1_app_arg[31:16],c$ds1_app_arg[15:0]};
    endcase
  end

  assign c$app_arg_8 = (adc_miso == (1'b0)) ? 64'sd0 : 64'sd1;

  assign c$bv = (result_2[11:0] >> (64'sd8));

  assign result = {result_2[14:14],
                   result_2[13:13],   result_2[12:12],
                   c$bv[0+:4],   adc_miso};

  assign adc_csn = result[7:7];

  assign adc_mosi = result[6:6];

  assign adc_sclk = result[5:5];

  assign led = result[4:1];

  assign adc_miso_monitor = result[0:0];


endmodule

