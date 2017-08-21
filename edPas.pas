(* minimal pascal compiler
* (c) 2014 by ir. Marc Dendooven *)

program edPas; 

(*****************************************************************
* system parameters. you can make adjustments.
******************************************************************)
// compiler
const	maxId = 1024; 

// virtual machine

		maxProg = 1023;
		maxStack = 1023;
		maxDivLev = 15; // max difference between static levels

(*****************************************************************
* general stuff
******************************************************************)
var f: text; // input file

procedure error(s: string);
begin
	writeln;
	writeln(s);writeln;
	halt
end;

(*****************************************************************
* virtual machine
******************************************************************)

type 	mnemonic = (LIT,NEG,ADD,SUB,MUL,DIVi,EQL,NEQ,LESS,LEQ,GTR,GEQ,
					ODDi,LOAD,STORE,JMP,JPZ,CALL,RET,ISP,WRITEi,WRITEC,READi,HALT);
					
		instruction = 	record
							m: mnemonic;
							dl: 0..maxDivLev;
							val : integer 
						end;
						
var pm: array[0..maxProg] of instruction; //program memory

	
procedure interpret;
	var PC,SP,LV: cardinal;
		IR: instruction;
		s: array[0..maxStack] of longInt; //stack
	
	function base : cardinal;
	var i: cardinal;
	begin
		base := LV;
		for i := 1 to IR.dl do base := s[base]
	end;
		
begin
	PC := 0; SP:=0; LV := 0;
	while true do begin
	
		if PC > maxProg then error('RUNTIME ERROR: program memory size exceeded.'); // should not be necessary ...
		if SP > maxStack then error('RUNTIME ERROR: stack size exceeded.');
		
		IR := pm[PC];
		PC := PC+1;
		
		case IR.m of
			LIT: begin s[SP] := IR.val; inc(SP) end;
			NEG:s[SP-1] := -s[SP-1];
			ADD:begin dec(SP);s[SP-1] := s[SP-1] + s[SP] end;
			SUB:begin dec(SP);s[SP-1] := s[SP-1] - s[SP] end;
			MUL:begin dec(SP);s[SP-1] := s[SP-1] * s[SP] end;
			DIVi:begin dec(SP);s[SP-1] := s[SP-1] div s[SP] end;
			EQL: begin dec(SP);s[SP-1] := ord(s[SP-1] = s[SP]) end;
			NEQ: begin dec(SP);s[SP-1] := ord(s[SP-1] <> s[SP]) end;
			LESS:begin dec(SP);s[SP-1] := ord(s[SP-1] < s[SP]) end;
			LEQ: begin dec(SP);s[SP-1] := ord(s[SP-1] <= s[SP]) end;
			GTR: begin dec(SP);s[SP-1] := ord(s[SP-1] > s[SP]) end;
			GEQ: begin dec(SP);s[SP-1] := ord(s[SP-1] >= s[SP]) end;
			ODDi: s[SP-1]:=ord(odd(s[SP-1]));
			LOAD: begin s[SP] := s[base+IR.val]; inc(SP) end;
			STORE: begin dec(SP); s[base+IR.val] := s[SP] end;
			JMP: PC := IR.val;
			JPZ: begin dec(SP); if s[SP]=0 then PC := IR.val end;
			CALL: begin s[SP] := base; s[SP+1] := LV; s[SP+2] := PC; LV := SP; PC := IR.val end;
			RET: begin PC := s[LV+2]; SP := LV; LV := s[LV+1] end;
			ISP: SP := SP + IR.val;
			WRITEi: begin dec(SP); write(s[SP]) end;
			WRITEc: begin dec(SP); write(chr(s[SP])) end;
			READi: begin read(s[SP]); inc(SP) end;
			HALT: break
		end
	end
end;

(*****************************************************************
* code generator
******************************************************************)

var address : cardinal = 0;

procedure gen(m0: mnemonic; dl0: cardinal; val0: integer);
begin
	if address > maxProg then error('INTERNAL ERROR: program memory size exceeded.');
	if dl0 > maxDivLev then error('INTERNAL ERROR: max difference between static levels exceeded.');
	with pm[address] do begin 
		m := m0;
		dl := dl0;
		val := val0
	end;
	inc(address);
end;

function nextAdr: cardinal;
begin
	nextAdr := address
end;

procedure fix(a: cardinal);
begin
	pm[a].val := address
end;

procedure showCode;
var a: cardinal;
begin
	for a := 0 to address-1 do
		with pm[a] do writeln(a,': ',m,' ',dl,' ',val)
end;

(*****************************************************************
* identifiers 
******************************************************************)

type 	idKind = 	(id_const, id_var, id_proc, id_func);
		idDescr = 	record
						Name: string;
						level: cardinal;
						kind: idKind;
						data: cardinal;
					end;
					
var		idList : array[1..maxId] of idDescr;
		fip : cardinal = 1; // free id pointer
		currentLevel : cardinal = 0;
		
procedure checkForDouble(name: string);
var i: cardinal;
begin
	i := fip-1;
	while (i > 0) and (idList[i].level = currentLevel) do
	begin
		if (name = idList[i].name)  then error('ERROR: Duplicate identifier "'+name+'"');
		i:=i-1
	end
end;

procedure addId(name: string; kind: idKind; data : cardinal);
begin
	checkForDouble(name);
	if fip > maxId then error('INTERNAL ERROR: max objects exceeded.');
	idList[fip].name := name;
	idList[fip].level := CurrentLevel;
	idList[fip].kind := kind;
	idList[fip].data := data;
	fip:=fip+1;
//	writeln;writeln;writeln('*** ',name,' ',kind,' ',data,' ***');writeln
end;

function getId(name: string): idDescr;
var i : cardinal;
	notfound: boolean = true;
begin
	i := fip-1;
	while (i > 0) do
	begin
		if (name = idList[i].name)  then begin getId := idList[i]; notfound := false; break end;
		i:=i-1
	end;
	if notfound then  error('ERROR: identifier not found "'+name+'"')
end;


(*****************************************************************
* scanner
******************************************************************)

type Symbols = (s_number, s_ident, s_program, s_begin, s_end, s_const,
				s_var, s_proc, s_func, s_if, s_then, s_else, s_while, s_do, s_for, s_to, s_repeat, s_until, s_write,
				s_writeln, s_read, s_odd, s_integer, 
				s_times, s_div, s_plus, s_minus, s_eql, s_neq, s_less,
				s_leq, s_gtr, s_geq, s_comma, s_rparen, s_lparen,
				s_becomes, s_semi, s_colon, s_period, s_eof, s_string, s_other);

const symText: array[s_number .. s_other] of string = ('NUMBER',
				'IDENTIFIER','PROGRAM', 'BEGIN', 'END', 'CONST',
				'VAR', 'PROCEDURE', 'FUNCTION', 'IF', 'THEN', 'ELSE', 'WHILE', 'DO', 'FOR', 'TO', 'REPEAT', 'UNTIL', 'WRITE',
				'WRITELN', 'READ', 'ODD', 'INTEGER',
				'*','/','+','-','=','<>','<',
				'<=','>','>=',',',')','(',
				':=',';',':','.','END OF FILE','STATIC STRING','SOMETHING ELSE'); 
								 

var ch : char; // lookahead character
	symbol : Symbols; // lookahead symbol
	val: cardinal; // value of a number
	name: string; // name of an identifier

procedure getCh;
begin
	if eof(f) 	then ch := #0  // ascii eof value
				else 	begin 
							read(f,ch);
							write(ch)
						end		
end;

procedure getSym;

	procedure number;
	begin
		symbol := s_number;
		val := 0;
		while ch in ['0'..'9'] do begin
			val := 10*val+ord(ch)-ord('0');
			getCh
		end
	end;
	
	procedure identifier;
	var i: Symbols;
	begin
		symbol := s_ident;
		name := '';
		while ch in ['a'..'z','A'..'Z','0'..'9'] do begin
			name := name + upcase(ch);
			getCh
		end;
		for i := s_program to s_integer do if name = symText[i] then symbol := i
	end;
	
	procedure other;
	var i: Symbols;
	
		procedure comment;
		begin
			getCh;
			repeat 
				while ch <> '*' do getCh;
				getCh
			until ch = ')';	
			getCh
		end;
	
	begin //other
		symbol := s_other;
		name := ch;
		case ch of
		#0 : symbol := s_eof;
		'(': begin 	getCh; 
					if ch = '*' then begin comment; getSym end
			 end;
		'<','>',':': 	begin 
							getCh; 
							if ch in ['=','>'] then begin name := name + ch; getCh end
						end;
		else getCh
		end;
		for i := s_times to s_period do if name = symText[i] then symbol := i
	end;
	
	procedure aString;
	begin
		symbol := s_string;
		name := '';
		getCh;
		while ch<>'''' do begin name := name+ch; getCh end;
		getCh
	end;

begin //getSym
	while ch in [#1..#32] do getCh; //skip whiteSpace
	case ch of
		'0'..'9': number;
		'a'..'z','A'..'Z': identifier;
		'''': aString;
		else other
	end
//	writeln('***',symbol,'***',val,'***',name,'***')
end;

(********************************************************
* parser
*********************************************************)

procedure nyi;
begin
	error('INTERNAL ERROR: '+symtext[symbol]+' : this feature is not yet implemented')
end;

procedure expect(s: Symbols);
begin
	if s = symbol 	then getSym
					else error('SYNTAX ERROR: "'+symtext[s]+'" expected but "'+symtext[symbol]+'" found.')
end;

procedure aProgram;

	procedure block(paramCount: cardinal);
	var oldfip: cardinal;
		varcount: cardinal = 3; // 0,1 and 2 are static link, dyn link and old PC
		lab0: cardinal;
		procedure const_decl;
		
			procedure define_constant;
			var cname : string;
			begin
				cname := name;
				expect(s_ident);
				expect(s_eql);
				addId(cname, id_const, val);
				expect(s_number);
				expect(s_semi)
			end;
		
		begin // const_decl
			getSym;
			define_constant;
			while symbol = s_ident do define_constant
		end;
		
		procedure var_decl;
		
			procedure define_var;
			begin
				getSym;
				addId(name,id_var,varCount+paramCount); // var relative address should be added
				inc(varCount);
				expect(s_ident);
			end;
		
		begin // var_decl
			define_var;
			while symbol = s_comma do define_var;
			expect(s_colon);expect(s_integer);
			expect(s_semi)
		end;
		
		procedure proc;
		var formalParamCount: integer = 0;
			id: idKind;
		
			procedure formalParamList;
			
				procedure define_param;
				begin
					getSym;
					addId(name,id_var,formalParamCount+3);
					inc(formalParamCount); 
					expect(s_ident);
				end;
			
			begin //formalParamList
				define_param;
				while symbol = s_comma do define_param;
				expect(s_colon);expect(s_integer);
				expect(s_rparen)
			end;
		
		
		begin //proc
			if symbol = s_proc then id := id_proc else id := id_func;
			getSym;
			addId(name,id,nextAdr); 
			expect(s_ident); 
(**)		currentLevel := currentLevel+1; oldfip := fip;
			if symbol = s_lparen then formalParamList;
			if id = id_func then begin expect(s_colon); expect(s_integer) end;
			expect(s_semi);	
			block(formalParamCount); expect(s_semi);
(**)		currentLevel := currentLevel-1; fip := oldfip;
(**)		gen(RET,0,0) 
		end;
		
		procedure statement; forward;
		
		procedure compound_Statement;
		begin 
			expect(s_begin);
			statement;
			while symbol = s_semi do begin getSym; statement end;
			expect(s_end)
		end;
		
		procedure statement;
		var lab0,lab1: cardinal;
		
			procedure expression;
			var s0 : Symbols;
			
				procedure term;
				var s0 : Symbols;
				
					procedure factor;
					var id: idDescr;
						actualParamCount: cardinal = 0;
					
						procedure actualParamList;
						begin
							expect(s_lparen);
							expression; inc(ActualParamCount);
							while symbol = s_comma do begin expect(s_comma); expression; inc(actualParamCount) end;
						//	expect(s_rparen);
						end;
					
					begin // factor
						case symbol of
							s_ident:begin 
										id := getId(name);
										case id.kind of
											id_const: gen(LIT,0,id.data);
											id_var: gen(LOAD,currentLevel-id.level,id.data);
											id_func: 	begin 
															getSym;
															gen(ISP,0,4); // make room for return value and SL, DL and old PC
															if symbol = s_lparen then actualParamList;
															gen(ISP,0,-actualParamCount-3);
															gen(CALL,currentLevel-id.level,id.data)
														end
										else
											error('ERROR: constant, variable or function expected')
										end;
										getSym 
									end;
							s_number: begin gen(LIT,0,val);getSym end;
							s_lparen: begin expect(s_lparen); expression; expect(s_rparen) end
							else error('SYNTAX ERROR: One of "IDENTIFIER", "NUMBER" or "(" expected but "'+symtext[symbol]+'" found.')
						end
					end;
				
				begin //term
					factor;
					while symbol in [s_times,s_div] do 	begin 	s0 := symbol; getSym; 
																factor;
																if s0 = s_times then gen(MUL,0,0) else gen(DIVi,0,0) 
														end
				end;
			
			begin //expression
				s0 := symbol;
				if symbol in [s_minus,s_plus] then getSym;
				term; if s0 = s_minus then gen(NEG,0,0);
				while symbol in [s_minus,s_plus] do begin 	s0:= symbol;getSym; 
															term;
															if s0 = s_minus then gen(SUB,0,0) else gen(ADD,0,0)
													end
			end;
		
			procedure assign_or_call;
			var id: idDescr;
					actualParamCount: cardinal = 0;
			
				procedure actualParamList;
				begin
					expect(s_lparen);
					expression; inc(ActualParamCount);
					while symbol = s_comma do begin expect(s_comma); expression; inc(actualParamCount) end;
					expect(s_rparen);
				end;
			
			begin
				id := getId(name);
				expect(s_ident);
				if symbol = s_becomes
					then //assign
						begin 
							expect(s_becomes);
							case id.kind of
								id_var : begin expression; gen(STORE,currentLevel-id.level,id.data) end;
								id_func: begin expression; gen(STORE,currentLevel-id.level-1,-1) end;
							else error('error: variable or function name expected')
							end	 
						end 
					else 
						begin //call procedure
							if id.kind <> id_proc then error('error: procedure expected'); 
							gen(ISP,0,3);
							if symbol = s_lparen then actualParamList;
							gen(ISP,0,-actualParamCount-3);
							gen(CALL,currentLevel-id.level,id.data)
						end
			end;
			
			procedure condition;
			
				procedure diatomic;
				var s0: Symbols;
				begin
					expression;
					s0 := symbol;
					getSym;
					expression;
					case s0 of
						s_eql: gen(EQL,0,0);
						s_neq: gen(NEQ,0,0);
						s_less: gen(LESS,0,0);
						s_leq: gen(LEQ,0,0);
						s_gtr: gen(GTR,0,0);
						s_geq: gen(GEQ,0,0);
						else error('SYNTAX ERROR: One of "ODD", "=", "<>", "<", ">=", ">", ">=" expected but "'+symtext[symbol]+'" found.')
					end
				end;
			
			begin //condition
				case symbol of
					s_odd : begin expect(s_odd); expect(s_lparen); expression; expect(s_rparen);gen(ODDi,0,0) end
					// other buildin functions come here
					else diatomic;
				end
			end;
			
			procedure if_stat;
			begin
				expect(s_if); condition; lab0 := nextAdr; gen(JPZ,0,0); expect(s_then); statement; lab1 := nextAdr; gen(JMP,0,0); fix(lab0);
				if symbol = s_else then begin expect(s_else); statement end;
				fix(lab1)
			end;
			
			procedure while_stat;
			begin
				expect(s_while); lab0 := nextAdr; condition; lab1 := nextAdr ;gen(JPZ,0,0); expect(s_do); statement; gen(JMP,0,lab0); fix(lab1)
			end;
			
			procedure repeat_stat;
			begin
				expect(s_repeat); lab0 := nextAdr; statement; expect(s_until); condition; gen(JPZ,0,lab0)
			end;
			
			procedure for_stat;
			var id: idDescr;
			begin
				expect(s_for); id := getId(name); if id.kind <> id_var then error('error: variable identifier expected'); 
				expect(s_ident); expect(s_becomes); expression; gen(STORE,currentLevel-id.level,id.data); //assign loopvar
				expect(s_to); lab1 := nextAdr; gen(LOAD,currentLevel-id.level,id.data); //reload loopvar
				expression; ; gen(LEQ,0,0); lab0 := nextAdr; // compare to expression
				gen(JPZ,0,0); //jump to end if loopvar > expression
				expect(s_do); statement; 
				gen(LOAD,currentLevel-id.level,id.data); gen(LIT,0,1); 
				gen(ADD,0,0);gen(STORE,currentLevel-id.level,id.data); gen(JMP,0,LAB1); // increment loopvar and jump back
				fix(lab0) 
			end;
			
			procedure write_stat;

			
				procedure doWrite;
				var i: cardinal;
				begin
					getSym;
					if symbol = s_string 
					then begin 
							for i := 1 to length(name) do begin gen(LIT,0,ord(name[i]));gen(WRITEc,0,0) end;
							expect(s_string) 
						 end
					else begin expression;gen(WRITEi,0,0) end
				end;
			
			begin
				getSym;
				if symbol = s_lparen then 
				begin
					doWrite;
					while symbol = s_comma do doWrite;
					expect(s_rparen)
				end
			end;
			
			procedure writeln_stat;
			begin
				write_stat;
				gen(LIT,0,10);gen(WRITEc,0,0) // newline... for windows CR should be added
			end;
			
			procedure read_stat;
			var id: idDescr;
			begin
				expect(s_read);expect(s_lparen);
				id := getId(name); expect(s_ident);
				if id.Kind <> id_var then error('ERROR: variable expected');
				gen(READi,0,0);gen(STORE,currentLevel-id.level,id.data);
				expect(s_rparen)
			end;
		
		begin //statement
			case symbol of
				s_ident: assign_or_call; 
				s_if: if_stat;
				s_while: while_stat;
				s_repeat: repeat_stat;
				s_for: for_stat;
				s_begin: compound_Statement;
				s_write: write_stat;
				s_writeln: writeln_stat;
				s_read: read_stat;
				else error('SYNTAX ERROR: Statement expected but "'+symtext[symbol]+'" found.')
			end
		end;
	
	begin //block
//		currentLevel := currentLevel+1; oldfip := fip;
		if symbol = s_const then const_decl;
		if symbol = s_var then var_decl;
		lab0 := nextAdr; gen(JMP,0,0);
		while symbol in [s_proc,s_func] do proc;
//		while symbol = s_func do func;
		fix(lab0);
		gen(ISP,0,varCount+paramCount);
		compound_Statement;
//		currentLevel := currentLevel-1; fip := oldfip;
//		if currentLevel > 0 then gen(RET,0,0) else gen(HALT,0,0)
	end;

begin //aProgram
	expect(s_program);expect(s_ident);expect(s_semi); //header
	block(0); expect(s_period);
(**)gen(HALT,0,0)
end;

                          
begin // edPas
	writeln('**************************************');
	writeln('* Minimal pascal compiler            *');
	writeln('* (c) 2014 by ir. Marc Dendooven     *');
	writeln('**************************************');
	writeln;
	
	if paramStr(1)='' then error('ERROR: sourcefile missing');
	{$I-}
	assign (f,paramStr(1));
	reset(f);
	{$I+}
	if IOresult <> 0 then error('ERROR: no such file');
	
	
	getCh;
	getSym;
	
	aProgram;
	
	writeln;
	writeln('**************************************');
	writeln('* Compilation has succesfully ended  *');
	writeln('* P-code is:                         *');
	writeln('**************************************');
	writeln;
	
	showCode;
	
	writeln;
	writeln('**************************************');
	writeln('* Executing program:                 *');
	writeln('*                                    *');
	writeln('**************************************');
	writeln;
	
	interpret;
	
	writeln;
	writeln('**************************************');
	writeln('* Execution has succesfully ended    *');
	writeln('* bye                                *');
	writeln('**************************************');
	writeln;
	
	close(f)
end.
