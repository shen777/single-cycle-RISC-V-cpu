// Please include verilog file if you write module in other file

module CPU(
    input         clk,
    input         rst,
    output        instr_read,
    output [31:0] instr_addr,
    input  [31:0] instr_out,
    output        data_read,
    output        data_write,
    output [31:0] data_addr,
    output [31:0] data_in,
    input  [31:0] data_out
);
wire [31:0]pc_out,pc_in,instr,pcadd4,rd_data,rs1data,rs2data,imm,add2out,pc_4,alu2,alu_out,pc_to_reg,pc_or_alu_out,mem_out;
wire regwrite,pctoregsrc,alusrc,rdsrc,memtoreg,zeroflag;
wire [3:0] ctoalu;
wire[1:0] brancgctrl;
/* Add your design */
pc pc(.clk(clk),.rst(rst),.pcinput(pc_in),.pcoutput(pc_out));

assign instr_addr[31:0]=pc_out[31:0];
assign instr_read=1'b1;
assign instr[31:0]=instr_out[31:0];
add1 add1(.pcout(pc_out),.pc4(pcadd4));
contral contral(.clk(clk),.opcode(instr[6:0]),.funct3(instr[14:12]),.funct7(instr[31:25]),.alucout(ctoalu),.memreadout(data_read),.memwriteout(data_write),.regwriteout(regwrite),.branchcontralout(brancgctrl),.pctoregsrcout(pctoregsrc),.alusrcout(alusrc),.rdsrcout(rdsrc),.memtoregout(memtoreg));
regfile regfile(.clk(clk),.rst(rst),.rs1(instr[19:15]),.rs2(instr[24:20]),.rd(instr[11:7]),.rddata(rd_data),.regwrite(regwrite),.rs1data(rs1data),.rs2data(rs2data));
immediate immediate(.instr(instr),.outimm(imm));
add2 add2(.pcout(pc_out),.imm(imm),.pcaddimm(add2out));
add3 add3(.pcout(pc_out),.pc4(pc_4));
mux2 ALUSRC(.upordown(alusrc),.up(rs2data),.down(imm),.result(alu2));
assign data_in[31:0]=rs2data[31:0];
alu alu(.data1(rs1data),.data2(alu2),.alucon(ctoalu),.zeroflg(zeroflag),.alurslt(alu_out));
mux3 mux3(.clk(clk),.branchcontral(brancgctrl),.pc4(pcadd4),.branch(zeroflag),.mid(add2out),.aluresult(alu_out),.pcin(pc_in));
mux2 PCTOREGSRC(.upordown(pctoregsrc),.up(add2out),.down(pc_4),.result(pc_to_reg));
mux2 RDSRC(.upordown(rdsrc),.up(pc_to_reg),.down(alu_out),.result(pc_or_alu_out));
assign data_addr[31:0]=alu_out[31:0];
assign mem_out[31:0]=data_out[31:0];
mux2 MEMTOREG(.upordown(memtoreg),.up(pc_or_alu_out),.down(mem_out),.result(rd_data));


module pc(
input clk,
input rst,
input[31:0] pcinput,
output[31:0] pcoutput
);
reg [31:0] pc;

always@(posedge clk or posedge rst)begin
if(rst)begin
pc=32'b0;
end
else begin
pc[31:0]=pcinput[31:0];
end
end
assign pcoutput[31:0]=pc[31:0];
endmodule

module alu(
input[31:0] data1,
input[31:0] data2,
input[3:0] alucon,
output zeroflg,// true==1
output[31:0] alurslt
);
reg[31:0]aluresult;
reg [3:0]aluc;
reg zeroflag;
reg [31:0] temp;

initial zeroflag = 1'b0;			// zeroflag 

always@(*)begin
aluc[3:0]=alucon[3:0];

case(aluc)//begin
4'b0000:begin// add lw addi sw jalr
aluresult<=data1+data2;
end

4'b0001:begin//sub 
aluresult<=$signed(data1)-$signed(data2);
end

4'b0010:begin// shift left logical rd=rs1<<rs2[4:0] slli
aluresult<=($unsigned(data1)<<data2[4:0]);			// put unsigned for data1
end

4'b0011:begin//show less than slt slti blt
if($signed(data1)<$signed(data2))begin 
aluresult<=32'b1;
zeroflag<=1'b1;
end
else begin//data2>=data1
aluresult<=32'b0;
zeroflag=1'b0;
end
end

4'b0100:begin// show less than unsign sltu sltiu bltu		// may have problems
	if($unsigned(data1)<data2) zeroflag=1'b1;
	else zeroflag=1'b0;
	
	aluresult[31:1]=31'b0;
	aluresult[0]=zeroflag;
end

4'b0101:begin//xor xori
aluresult<=data1^data2;
end

4'b0110:begin//shift right logical srl srli
aluresult<=($unsigned(data1)>>data2[4:0]);
end

4'b0111:begin//算術右移 有號延伸 sra srai
aluresult<=($signed(data1)>>>data2[4:0]);
end

4'b1000:begin//or ori
aluresult<=data1|data2;
end

4'b1001:begin// and andi 
aluresult<=data1&data2;
end

4'b1010:begin// be equal beq
if(data1==data2)begin
zeroflag=1'b1;
end
else zeroflag=1'b0;
end

4'b1011:begin//be not equal bne
if(data1!=data2)begin
zeroflag=1'b1;
end
else zeroflag=1'b0;
end

4'b1100:begin//bigger or equal bge
if($signed(data1)>=$signed(data2))begin
zeroflag=1'b1;
end
else zeroflag=1'b0;
end

4'b1101:begin// bigger or equal unsign bgeu
if($unsigned(data1)>=$unsigned(data2))begin
zeroflag=1'b1;
end
else zeroflag=1'b0;
end

default:    //lui rd = imm lui 4'b1110
aluresult<=data2;
endcase

//end
end
assign alurslt=aluresult;
assign zeroflg=zeroflag;
endmodule

module regfile(
input clk,
input rst,
input [4:0] rs1,
input [4:0] rs2,
input [4:0] rd,
input[31:0] rddata,
input regwrite,
output [31:0] rs1data,
output [31:0] rs2data
);
reg [31:0] regfile2[31:0];
integer k;

always@(posedge clk)begin

if(rst==1'b1)begin

for(k=0;k<32;k=k+1) begin
regfile2[k]<=32'd0;
end


end

else begin
if((regwrite==1'b1)&&(rd!=5'b0))begin
regfile2[rd]<=rddata;
end
end

end
assign rs1data[31:0]=regfile2[rs1];
assign rs2data[31:0]=regfile2[rs2];
endmodule

module contral(//total contral
input clk,
input [6:0] opcode,
input [2:0] funct3,
input [6:0] funct7,
output [3:0] alucout,// toalu
output memreadout,
output memwriteout,
output regwriteout,
output [1:0]branchcontralout,
output pctoregsrcout,
output alusrcout,
output rdsrcout,
output memtoregout
);
reg [3:0] aluc;
reg memread,memwrite,regwrite,pctoregsrc,alusrc,rdsrc,memtoreg;

reg [2:0] test;

reg [1:0]branchcontral;
always@(opcode or funct3 or funct7)begin

case(opcode)

7'b0110011:begin//rtype
memread=1'b0;
memwrite=1'b0;
regwrite=1'b1;
branchcontral[1:0]=2'b00;
pctoregsrc=1'b0;// dosnt matter
alusrc=1'b1;
rdsrc=1'b0;	// instruction h170 sub error
memtoreg=1'b1;
case(funct3)

3'b000:begin

case(funct7)
7'b0000000:begin
//add
aluc=4'b0000;
end

7'b0100000:begin
//sub
aluc=4'b0001;
end

endcase
end

3'b001:begin
//sll
aluc=4'b0010;
end

3'b010:begin
//slt
aluc=4'b0011;
end

3'b011:begin
//sltu
aluc=4'b0100;
end

3'b100:begin
//xor
aluc=4'b0101;
end

3'b101:begin
case(funct7)
7'b0000000:begin
//srl
aluc=4'b0110;
end
7'b0100000:begin
//sra
aluc=4'b0111;
end
endcase
end

3'b110:begin
//or
aluc=4'b1000;
end

3'b111:begin
//and
aluc=4'b1001;
end

endcase
end

7'b0000011:begin// lw
memread=1'b1;
memwrite=1'b0;
regwrite=1'b1;
branchcontral[1:0]=2'b00;
pctoregsrc=1'b0;// dosnt matter
alusrc=1'b0;
rdsrc=1'b1;// dosen't matter
memtoreg=1'b0;
alusrc=4'b0000;
end

7'b0010011:begin
//itype
test = 3'b111;

memread=1'b0;
memwrite=1'b0;
regwrite=1'b1;
branchcontral[1:0]=2'b00;
pctoregsrc=1'b0;// dosnt matter
alusrc=1'b0;
rdsrc=1'b0;
memtoreg=1'b1;
case(funct3)//funct3
3'b000:begin
//addi
aluc=4'b0000;
end
3'b010:begin
//slti
aluc=4'b0011;
end
3'b011:begin
//sltiu
aluc=4'b0100;
end
3'b100:begin
//xori
aluc=4'b0101;
end
3'b110:begin
//ori
aluc=4'b1000;
end
3'b111:begin
//andi
aluc=4'b1001;
end
3'b001:begin
//slli  has shamt
aluc=4'b0010;
end
3'b101:begin
case(funct7)
7'b0000000:begin
//srli
aluc=4'b0110;
end
7'b0100000:begin
//srai
aluc=4'b0111;
end
endcase
end
endcase//funct3

end

7'b1100111:begin
//jalr
memread=1'b0;
memwrite=1'b0;
regwrite=1'b1;
branchcontral[1:0]=2'b11;
pctoregsrc=1'b0;
alusrc=1'b0;
rdsrc=1'b1;
memtoreg=1'b1;
aluc=4'b0000;
end

7'b0100011:begin
//sw
memread=1'b0;
memwrite=1'b1;
regwrite=1'b0;
branchcontral[1:0]=2'b00;
pctoregsrc=1'b0;//dosen't matter
alusrc=1'b0;
rdsrc=1'b1;//dosen't matter
memtoreg=1'b1;//dosen't matter
aluc=4'b0000;
end

7'b1100011:begin//btype
memread=1'b0;
memwrite=1'b0;
regwrite=1'b0;
branchcontral[1:0]=2'b10;
pctoregsrc=1'b0;//dosen't matter
alusrc=1'b1;
rdsrc=1'b1;//dosen't matter
memtoreg=1'b1;//dosen't matter
case(funct3)

3'b000:begin
//beq
aluc=4'b1010;
end
3'b001:begin
//bne
aluc=4'b1011;
end
3'b100:begin
//blt
aluc=4'b0011;
end
3'b101:begin
//bge
aluc=4'b1100;
end
3'b110:begin
//bltu
aluc=4'b0100;
end
3'b111:begin
//bgeu
aluc=4'b1101;
end
endcase
end//btype

7'b0010111:begin
//auipc
memread=1'b0;
memwrite=1'b0;
regwrite=1'b1;
branchcontral[1:0]=2'b00;
pctoregsrc=1'b1;
alusrc=1'b1;//dosen't matter
rdsrc=1'b1;
memtoreg=1'b1;
aluc=4'b0000;//dosen't matter
end
7'b0110111:begin
//lui
memread=1'b0;
memwrite=1'b0;
regwrite=1'b1;
branchcontral[1:0]=2'b00;
pctoregsrc=1'b1;//dosen't matter
alusrc=1'b0;
rdsrc=1'b0;
memtoreg=1'b1;
aluc=4'b1110;
end
7'b1101111:begin
//jal
memread=1'b0;
memwrite=1'b0;
regwrite=1'b1;
branchcontral[1:0]=2'b01;
pctoregsrc=1'b0;
alusrc=1'b0;//dosen't matter
rdsrc=1'b1;
memtoreg=1'b1;
aluc=4'b1110;//dosen't matter
end

endcase


end
assign alucout=aluc;
assign memreadout=memread;
assign memwriteout=memwrite;
assign regwriteout=regwrite;
assign branchcontralout=branchcontral;
assign pctoregsrcout=pctoregsrc;
assign alusrcout=alusrc;
assign rdsrcout=rdsrc;
assign memtoregout=memtoreg;
endmodule

module immediate(
input [31:0] instr,
output[31:0] outimm//out
);
reg[6:0] opcode;
reg[31:0] out;

always@(instr)begin
opcode[6:0]=instr[6:0];
out=32'b0;
case(opcode)
7'b1101111:begin//jal out[20]=instr[31];out[10:1]=instr[30:21];out[11]=instr[20];out[19:12]=instr[19:12];
out={{12{instr[31]}},instr[19:12],instr[20],instr[30:25],instr[24:21],1'b0};
end
7'b0110111:begin//lui out[19:0]=instr[31:12];
out={{1{instr[31]}},instr[30:20],instr[19:12],12'b0};
end
7'b0010111:begin//auipc out[19:0]=instr[31:12];
out={{1{instr[31]}},instr[30:20],instr[19:12],12'b0};
end
7'b1100011:begin//btype out[12]=instr[31]; out[10:5]=instr[30:25]; out[4:1]=instr[11:8]; out[11]=instr[7];
out={{20{instr[31]}},instr[7],instr[30:25],instr[11:8],1'b0};
end
7'b0100011:begin//sw out[11:5]=instr[31:25]; out[4:0]=instr[11:7];
out={{21{instr[31]}},instr[30:25],instr[11:7]};
end
7'b0000011:begin//itype out[11:0]=instr[31:20];
out ={{21{instr[31]}},instr[30:25],instr[24:20]};
end
7'b0010011:begin//itype out[11:0]=instr[31:20];
out ={{21{instr[31]}},instr[30:25],instr[24:20]};
end
7'b1100111:begin//itype out[11:0]=instr[31:20];
out ={{21{instr[31]}},instr[30:25],instr[24:20]};
end

default:begin
out =32'b0;// dosent matter
end

endcase
end

assign outimm = out;

endmodule

module add1(
input[31:0] pcout,
output reg [31:0]pc4
);
/*reg [31:0] add;
always@(*)begin
add[31:0]=pcout[31:0];
add=add+32'd4;
end */
//assign pc4=pcout+32'd4;
always@(pcout) begin
	pc4 = pcout + 32'd4;
	end

endmodule

module add2(
input[31:0] pcout,
input[31:0] imm,
output[31:0] pcaddimm
);
//reg [31:0] add;
//always@(*)begin
//add=pcout+imm;
//end
assign pcaddimm=pcout+imm;
endmodule

module add3(
input[31:0] pcout,
output [31:0]pc4
);
reg [31:0] add;
always@(*)begin
add=pcout+32'd4;
end
assign pc4[31:0]=add[31:0];
endmodule

module mux3(
input clk,
input [1:0] branchcontral,
input [31:0] pc4,
input branch,
input[31:0] mid,
input[31:0] aluresult,
output [31:0] pcin
);
reg [31:0] outreg;
always@(branchcontral or pc4 or branch or aluresult)begin

if(branchcontral==2'b00) outreg=pc4;
else if(branchcontral==2'b01) outreg=mid;
else if(branchcontral==2'b10) begin
	if(branch==1'b1) outreg=mid;
	else outreg=pc4;
	end
else if(branchcontral==2'b11) begin
	outreg[31:1]<=aluresult[31:1];
	outreg[0]<=1'b0;
	end

end
assign pcin=outreg;
endmodule

module mux2(
input upordown,//1 up 0 down
input[31:0] up,
input[31:0] down,
output[31:0] result
);
reg [31:0]res;
always@(*)begin
if(upordown==1'b1)begin
res[31:0]=up[31:0];
end
else begin
res[31:0]=down[31:0];
end
end
assign result[31:0]=res[31:0];
endmodule

/*module mux2(
input pctoregsrc,//1 pcaddimm 0 pc4
input[31:0] pcaddimm,
input[31:0] pc4,
output[31:0] pctoreg
);
always@(*)begin
if(pctoregsrc)begin
pctoreg[31:0]=pcaddimm[31:0];
end
else begin
pctoreg[31:0]=pc4[31:0];
end
end
endmodule

module mux3(
input alusrc,//1 reg2 0 imm
input[31:0] reg2,
input[31:0] imm,
output[31:0] data2
);
always@(*)begin
if(alusrc)begin
data2[31:0]=reg2[31:0];
end
else begin
data2[31:0]=imm[31:0];
end
end
endmodule

module mux4(
input rdsrc,//1 pctoreg 0 aluout
input [31:0] pctoreg,
input [31:0] aluout,
output [31:0] pcoraluout
);
always@(*)begin
if(rdsrc)begin
pcoraluout[31:0]=pctoreg[31:0];
end
else begin
pcoraluout[31:0]=aluout[31:0];
end
end
endmodule */







endmodule
