{
	const OPERATORS = {
		"&": "BitwiseAnd",
		"|": "BitwiseOr",
		"^": "ExclusiveOr",
		"=": "Equal",
		"==": "Equal",
		"!=": "NotEqual",
		"<>": "NotEqual",
		">=": "GreaterEqual",
		"<=": "LessEqual",
		">": "GreaterThan",
		"<": "LessThan",
		">>>": "ArithmaticRight",
		">>": "ShiftRight",
		"<<": "ShiftLeft",
		"+": "Addition",
		"-": "Subtraction",
		"*": "Multiplication",
		"/": "Division",
		"%": "Modulo",
		"..": "Concat"
	}

	function associate(term, left, right) {
        while(right.length > 0) {
			let top = right.shift();
			top[term] = left;
			left = top;
		}
		return left;
	}
}

Body
	= _ body:Statement*
		{ return body }

Statement
	= ZeuxStatement
	/ UsingStatement
	/ FunctionStatement
	/ WhileStatement
	/ DoWhileStatement
	/ IfStatement
	/ SwitchStatement
	/ GlobalStatement
	/ LocalStatement
	/ ConstStatement
	/ AssignmentStatement
	/ LineBreak
	/ CommentStatement
	/ ReturnStatement
	/ value:Expression
		{ return { type: "ExpressionStatement", value } }

CommentStatement
	= "//" body:$(![\n\r] .)* LineBreak
		{ return { type: "Comment", location: location(), body } }
	/ "/*" body:$(!"*/" .)* "*/"
		{ return { type: "Comment", location: location(), body } }


ZeuxStatement
	= body:ZeuxCommand LineBreak
		{ return { type:"ZeuxStatement", location: location(), body } }

ZeuxCommand
	= $("ABORT"i WB) $("LOOP"i WB) 
	/ $("ASK"i WB) StringValue
	/ $("AVALANCHE"i WB) 
	/ $("BECOME"i WB) ColorValue ThingValue ParameterValue
	/ $("BECOME"i WB) $("NONLAVAWALKER"i WB) 
	/ $("BECOME"i WB) $("NONPUSHABLE"i WB) 
	/ $("BECOME"i WB) $("LAVAWALKER"i WB) 
	/ $("BECOME"i WB) $("PUSHABLE"i WB) 
	/ $("BLIND"i WB) NumberValue
	/ $("BOARD"i WB) DirectionValue StringValue
	/ $("BOARD"i WB) DirectionValue $("NONE"i WB) 
	/ $("BULLETCOLOR"i WB) ColorValue
	/ $("BULLETE"i WB) NumberValue
	/ $("BULLETN"i WB) NumberValue
	/ $("BULLETS"i WB) NumberValue
	/ $("BULLETW"i WB) NumberValue
	/ $("CENTER"i WB) $("MESG"i WB) 
	/ $("CHANGE"i WB) ColorValue ThingValue ParameterValue ColorValue ThingValue ParameterValue
	/ $("CHANGE"i WB) $("CHAR"i WB) $("ID"i WB) NumberValue NumberValue
	/ $("CHANGE"i WB) $("OVERLAY"i WB) ColorValue NumberValue ColorValue NumberValue
	/ $("CHANGE"i WB) $("OVERLAY"i WB) ColorValue ColorValue
	/ $("CHANGE"i WB) $("SFX"i WB) NumberValue StringValue
	/ $("CHANGE"i WB) $("THICK"i WB) $("ARROW"i WB) $("CHAR"i WB) DirectionValue NumberValue
	/ $("CHANGE"i WB) $("THIN"i WB) $("ARROW"i WB) $("CHAR"i WB) DirectionValue NumberValue
	/ $("CHAR"i WB) NumberValue
	/ $("CHAR"i WB) $("EDIT"i WB) NumberValue NumberValue NumberValue NumberValue NumberValue NumberValue NumberValue NumberValue NumberValue NumberValue NumberValue NumberValue NumberValue NumberValue NumberValue
	/ $("CLEAR"i WB) $("MESG"i WB) 
	/ $("CLIP"i WB) $("INPUT"i WB) 
	/ $("COLOR"i WB) ColorValue
	/ $("COLOR"i WB) $("FADE"i WB) $("OUT"i WB) 
	/ $("COLOR"i WB) $("FADE"i WB) $("IN"i WB) 
	/ $("COLOR"i WB) $("INTENSITY"i WB) NumberValue $("PERCENT"i WB) 
	/ $("COLOR"i WB) $("INTENSITY"i WB) NumberValue NumberValue $("PERCENT"i WB) 
	/ $("COPY"i WB) DirectionValue DirectionValue
	/ $("COPY"i WB) NumberValue NumberValue NumberValue NumberValue
	/ $("COPY"i WB) $("BLOCK"i WB) NumberValue NumberValue NumberValue NumberValue NumberValue NumberValue
	/ $("COPY"i WB) $("CHAR"i WB) NumberValue NumberValue
	/ $("COPY"i WB) $("OVERLAY"i WB) $("BLOCK"i WB) NumberValue NumberValue NumberValue NumberValue NumberValue NumberValue
	/ $("COPYROBOT"i WB) DirectionValue
	/ $("COPYROBOT"i WB) StringValue
	/ $("COPYROBOT"i WB) NumberValue NumberValue
	/ $("CYCLE"i WB) NumberValue
	/ $("DIE"i WB) 
	/ $("DIE"i WB) $("ITEM"i WB) 
	/ $("DISABLE"i WB) $("MESG"i WB) $("EDGE"i WB) 
	/ $("DISABLE"i WB) $("SAVING"i WB) 
	/ $("DUPLICATE"i WB) $("SELF"i WB) NumberValue NumberValue
	/ $("DUPLICATE"i WB) $("SELF"i WB) DirectionValue
	/ $("STOP"i WB) 
	/ $("STOP"i WB) $("MOD"i WB) 
	/ $("STOP"i WB) $("PLAY"i WB) 
	/ $("STOP"i WB) $("SAM"i WB) 
	/ $("ENDGAME"i WB) 
	/ $("ENDLIFE"i WB) 
	/ $("ENABLE"i WB) $("MESG"i WB) $("EDGE"i WB) 
	/ $("ENABLE"i WB) $("SAVING"i WB) 
	/ $("ENABLE"i WB) $("SENSORONLY"i WB) $("SAVING"i WB) 
	/ $("ENEMY"i WB) $("BULLETCOLOR"i WB) ColorValue
	/ $("ENEMY"i WB) $("BULLETE"i WB) NumberValue
	/ $("ENEMY"i WB) $("BULLETN"i WB) NumberValue
	/ $("ENEMY"i WB) $("BULLETS"i WB) NumberValue
	/ $("ENEMY"i WB) $("BULLETW"i WB) NumberValue
	/ $("EXCHANGE"i WB) $("PLAYER"i WB) $("POSITION"i WB) 
	/ $("EXCHANGE"i WB) $("PLAYER"i WB) $("POSITION"i WB) NumberValue
	/ $("EXCHANGE"i WB) $("PLAYER"i WB) $("POSITION"i WB) NumberValue $("DUPLICATE"i WB) $("SELF"i WB) 
	/ $("EXPLODE"i WB) NumberValue
	/ $("FIREWALKER"i WB) NumberValue
	/ $("FILLHEALTH"i WB) 
	/ $("FLIP"i WB) $("CHAR"i WB) NumberValue DirectionValue
	/ $("FREEZETIME"i WB) NumberValue
	/ $("GIVE"i WB) NumberValue ThingValue
	/ $("GIVEKEY"i WB) ColorValue
	/ $("GIVEKEY"i WB) ColorValue StringValue
	/ $("GO"i WB) DirectionValue NumberValue
	/ $("GOTO"i WB) StringValue
	/ $("GOTOXY"i WB) NumberValue NumberValue
	/ $("INPUT"i WB) $("STRING"i WB) StringValue
	/ $("JUMP"i WB) $("MOD"i WB) $("ORDER"i WB) NumberValue
	/ $("LAYBOMB"i WB) DirectionValue
	/ $("LAYBOMB"i WB) $("HIGH"i WB) DirectionValue
	/ $("LAZERWALL"i WB) DirectionValue NumberValue
	/ $("LOAD"i WB) $("CHAR"i WB) $("SET"i WB) StringValue
	/ $("LOAD"i WB) $("PALETTE"i WB) StringValue
	/ $("LOCKPLAYER"i WB) 
	/ $("LOCKPLAYER"i WB) $("ATTACK"i WB) 
	/ $("LOCKPLAYER"i WB) $("EW"i WB) 
	/ $("LOCKPLAYER"i WB) $("NS"i WB) 
	/ $("LOCKSCROLL"i WB) 
	/ $("LOCKSELF"i WB) 
	/ $("LOOP"i WB) NumberValue
	/ $("LOOP"i WB) $("START"i WB) 
	/ $("MESSAGE"i WB) $("ROW"i WB) NumberValue
	/ $("MISSILECOLOR"i WB) ColorValue
	/ $("MOD"i WB) StringValue
	/ $("MOD"i WB) $("FADE"i WB) NumberValue NumberValue
	/ $("MOD"i WB) $("FADE"i WB) $("IN"i WB) StringValue
	/ $("MOD"i WB) $("FADE"i WB) $("OUT"i WB) 
	/ $("MOD"i WB) $("SAM"i WB) NumberValue NumberValue
	/ $("MOVE"i WB) $("ALL"i WB) ColorValue ThingValue ParameterValue DirectionValue
	/ $("MOVE"i WB) $("PLAYER"i WB) DirectionValue
	/ $("MOVE"i WB) $("PLAYER"i WB) DirectionValue StringValue
	/ $("NEUTRAL"i WB) $("BULLETCOLOR"i WB) ColorValue
	/ $("NEUTRAL"i WB) $("BULLETE"i WB) NumberValue
	/ $("NEUTRAL"i WB) $("BULLETN"i WB) NumberValue
	/ $("NEUTRAL"i WB) $("BULLETS"i WB) NumberValue
	/ $("NEUTRAL"i WB) $("BULLETW"i WB) NumberValue
	/ $("OPEN"i WB) DirectionValue
	/ $("OVERLAY"i WB) $("ON"i WB) 
	/ $("OVERLAY"i WB) $("STATIC"i WB) 
	/ $("OVERLAY"i WB) $("TRANSPARENT"i WB) 
	/ $("PERSISTENT"i WB) $("GO"i WB) StringValue
	/ $("PLAY"i WB) StringValue
	/ $("PLAY"i WB) $("SFX"i WB) StringValue
	/ $("PLAYER"i WB) $("BULLETCOLOR"i WB) ColorValue
	/ $("PLAYER"i WB) $("BULLETE"i WB) NumberValue
	/ $("PLAYER"i WB) $("BULLETN"i WB) NumberValue
	/ $("PLAYER"i WB) $("BULLETS"i WB) NumberValue
	/ $("PLAYER"i WB) $("BULLETW"i WB) NumberValue
	/ $("PLAYER"i WB) $("CHAR"i WB) DirectionValue NumberValue
	/ $("PLAYER"i WB) $("CHAR"i WB) NumberValue
	/ $("PLAYERCOLOR"i WB) ColorValue
	/ $("PUSH"i WB) DirectionValue
	/ $("PUT"i WB) ColorValue ThingValue ParameterValue DirectionValue $("PLAYER"i WB) 
	/ $("PUT"i WB) ColorValue ThingValue ParameterValue DirectionValue
	/ $("PUT"i WB) ColorValue ThingValue ParameterValue NumberValue NumberValue
	/ $("PUT"i WB) ColorValue NumberValue $("OVERLAY"i WB) NumberValue NumberValue
	/ $("PUT"i WB) $("PLAYER"i WB) DirectionValue
	/ $("PUT"i WB) $("PLAYER"i WB) NumberValue NumberValue
	/ $("REL"i WB) $("COUNTERS"i WB) 
	/ $("REL"i WB) $("PLAYER"i WB) 
	/ $("REL"i WB) $("SELF"i WB) 
	/ $("REL"i WB) $("COUNTERS"i WB) $("FIRST"i WB) 
	/ $("REL"i WB) $("PLAYER"i WB) $("FIRST"i WB) 
	/ $("REL"i WB) $("SELF"i WB) $("FIRST"i WB) 
	/ $("REL"i WB) $("COUNTERS"i WB) $("LAST"i WB) 
	/ $("REL"i WB) $("PLAYER"i WB) $("LAST"i WB) 
	/ $("REL"i WB) $("SELF"i WB) $("LAST"i WB) 
	/ $("RESETVIEW"i WB) 
	/ $("RESTORE"i WB) StringValue NumberValue
	/ $("RESTORE"i WB) $("PLAYER"i WB) $("POSITION"i WB) 
	/ $("RESTORE"i WB) $("PLAYER"i WB) $("POSITION"i WB) NumberValue
	/ $("RESTORE"i WB) $("PLAYER"i WB) $("POSITION"i WB) NumberValue $("DUPLICATE"i WB) $("SELF"i WB) 
	/ $("ROTATECW"i WB) 
	/ $("ROTATECCW"i WB) 
	/ $("SAM"i WB) NumberValue StringValue
	/ $("SAVE"i WB) $("PLAYER"i WB) $("POSITION"i WB) 
	/ $("SAVE"i WB) $("PLAYER"i WB) $("POSITION"i WB) NumberValue
	/ $("SCROLL"i WB) $("CHAR"i WB) NumberValue DirectionValue
	/ $("SCROLLARROW"i WB) $("COLOR"i WB) ColorValue
	/ $("SCROLLBASE"i WB) $("COLOR"i WB) ColorValue
	/ $("SCROLLCORNER"i WB) $("COLOR"i WB) ColorValue
	/ $("SCROLLPOINTER"i WB) $("COLOR"i WB) ColorValue
	/ $("SCROLLTITLE"i WB) $("COLOR"i WB) ColorValue
	/ $("SCROLLVIEW"i WB) DirectionValue NumberValue
	/ $("SCROLLVIEW"i WB) $("POSITION"i WB) NumberValue NumberValue
	/ $("SEND"i WB) DirectionValue $("PLAYER"i WB) StringValue
	/ $("SEND"i WB) NumberValue NumberValue StringValue
	/ $("SEND"i WB) DirectionValue StringValue
	/ $("SEND"i WB) StringValue StringValue
	/ $("SET"i WB) $("COLOR"i WB) NumberValue NumberValue NumberValue NumberValue
	/ $("SET"i WB) $("EDGE"i WB) $("COLOR"i WB) ColorValue
	/ $("SET"i WB) $("MAXHEALTH"i WB) NumberValue
	/ $("SET"i WB) $("MESG"i WB) $("COLUMN"i WB) NumberValue
	/ $("SFX"i WB) NumberValue
	/ $("SHOOT"i WB) DirectionValue
	/ $("SHOOTMISSILE"i WB) DirectionValue
	/ $("SHOOTSEEKER"i WB) DirectionValue
	/ $("SLOWTIME"i WB) NumberValue
	/ $("SPITFIRE"i WB) DirectionValue
	/ $("STATUS"i WB) $("COUNTER"i WB) NumberValue StringValue
	/ $("SWAP"i WB) $("WORLD"i WB) StringValue
	/ $("SWITCH"i WB) DirectionValue DirectionValue
	/ $("TAKE"i WB) NumberValue ThingValue StringValue
	/ $("TAKE"i WB) NumberValue ThingValue
	/ $("TAKEKEY"i WB) ColorValue StringValue
	/ $("TAKEKEY"i WB) ColorValue
	/ $("TELEPORT"i WB) $("PLAYER"i WB) StringValue NumberValue NumberValue
	/ $("TRADE"i WB) NumberValue ThingValue NumberValue ThingValue StringValue
	/ $("TRY"i WB) DirectionValue StringValue
	/ $("UNLOCKPLAYER"i WB) 
	/ $("UNLOCKSCROLL"i WB) 
	/ $("UNLOCKSELF"i WB) 
	/ $("VIEWPORT"i WB) $("SIZE"i WB) NumberValue NumberValue
	/ $("VIEWPORT"i WB) NumberValue NumberValue
	/ $("VOLUME"i WB) NumberValue
	/ $("WAIT"i WB) $("MOD"i WB) $("FADE"i WB) 
	/ $("WAIT"i WB) $("PLAY"i WB) 
	/ $("WAIT"i WB) $("PLAY"i WB) StringValue
	/ $("WAIT"i WB) NumberValue
	/ $("WALK"i WB) DirectionValue
	/ $("WIND"i WB) NumberValue
	/ $("WRITE"i WB) $("OVERLAY"i WB) ColorValue StringValue NumberValue NumberValue
	/ $("ZAP"i WB) StringValue NumberValue

StringValue
	= value:Expression
		{ return { type: "StringValue", location: location(), value } }

NumberValue
	= value:Expression

ColorValue
	= value:$("c"i [0-9A-F?]i [0-9A-F?]i WB)
    	{ return value }
	/ Expression

ParameterValue
	= value:$("p"i [0-9A-F?]i [0-9A-F?]i WB)
    	{ return value }
	/ Expression

ThingValue
	= value:$([A-Z]i+ WB)
    	{ return value }

DirectionValue
	= value:$DirectionWord WB
    	{ return value }

DirectionWord
	= "NORTH"i
	/ "N"i
	/ "UP"i
	/ "SOUTH "i
	/ "S"i
	/ "DOWN"i
	/ "EAST"i
	/ "E"i
	/ "RIGHT"i
	/ "WEST"i
	/ "W"i
	/ "LEFT"i
	/ "IDLE"i
	/ "NODIR"i
	/ "ANYDIR"i
	/ "RANDNS"i
	/ "RANDEW"i
	/ "RANDNE"i
	/ "RANDNB"i
	/ "RANDB"i
	/ "SEEK"i
	/ "FLOW"i
	/ "RANDANY"i
	/ "UNDER"i
	/ "BENEATH"i

UsingStatement
	= "IMPORT"i WB file:String name:("AS"i WB name:Identifier { return name })?
		{ return { type: "ImportStatement", file, name } }

FunctionStatement
	= "DEFINE"i WB name:Identifier "(" _ first:(p:Identifier "," _ { return p })* last:Identifier? ")" _
		body: Statement*
	  "END"i WB
		{ return { type: "FunctionStatement", name, parameters:last && first.concat(last), location: location(), body } }

WhileStatement
	= "WHILE"i WB expression:Expression
		body:Statement*
	  "END"i WB 
		{ return { type: "WhileStatement", location: location(), expression, body } }

DoWhileStatement
	= "DO"i WB
		body:Statement*
	  "WHILE"i WB expression:Expression
		{ return { type: "DoWhileStatement", location: location(), expression, body } }

IfStatement
	= "IF"i WB expression:Expression "THEN"i WB
		body:Statement*
 		elseifs:ElseIfStatement*
 		otherwise:ElseStatement?
	  "END"i WB 
		{ return { type: "IfStatement", location: location(), expression, body, elseifs, otherwise } }

ElseIfStatement
	= "ELSE"i WB "IF"i WB expression:Expression "THEN"i WB
		body: Statement*
		{ return { type: "ElseIfStatement", location: location(), body, expression } }

ElseStatement
	= "ELSE"i WB
		body: Statement*
		{ return { type: "ElseStatement", location: location(), body } }

SwitchStatement
	= "SWITCH"i WB match:Expression cases:SwitchCaseStatement* "END"i WB
		{ return { type: "SwitchStatement", location: location(), match, cases }}

SwitchCaseStatement
	= match:(term:(Number/String) "," _ { return term })* last:(Number/String) ":" _ body:Statement*
		{ return { type: "SwitchCondition", match: match.concat(last), location: location(), body }}
	/ "DEFAULT"i WB ":" _ body:Statement*
		{ return { type:"SwitchDefault", location: location(), body }}
	/ LineBreak

GlobalStatement
	= "GLOBAL"i WB name:Identifier value:("=" _ v:Expression { return v })?
		{ return { type: "GlobalStatement", location: location(), name, value } }

LocalStatement
	= "LOCAL"i WB name:Identifier value:("=" _ v:Expression { return v })?
		{ return { type: "LocalStatement", location: location(), name, value } }

ConstStatement
	= "CONST"i WB name:Identifier "=" _ value:Expression
		{ return { type: "ConstStatement", const: location(), name, value } }

AssignmentStatement
	= target:UnaryExpression op:("+"/"-"/"**"/"*"/"/"/"%"/"&"/"|"/"^"/">>>"/">>"/"<<") "=" _ right:Expression
		{ 
			return { 
				type:"Assignment", 
				location: location(), 
				value: {
					type: "BinaryOperation",
					operation: OPERATORS[op],
					location: location(),
					left: target,
					right
				},
				target
			}
		}
	/ target:UnaryExpression "=" _ value:Expression
		{ return { type:"AssignmentStatement", location: location(), target, value } }

ReturnStatement
	= "RETURN"i WB value:Expression
		{ return { type:"ReturnStatement", location: location(), value } }

Expression
	= ConcatinationExpression

ConcatinationExpression
	= left:ConditionalExpression ".." _ right:ConcatinationExpression
		{ return { type: "BinaryOperation", operation: "Concat", location: location(), left, right } }
	/ ConditionalExpression

ConditionalExpression
	= test:BitwiseExpression "?" _ onTrue:ConditionalExpression _ ":" onFalse:ConditionalExpression
		{ return { type: "ConditionalOperation", location: location(), test, onTrue, onFalse } }
	/ BitwiseExpression

BitwiseExpression
	= left:ComparisonExpression right:(op:("&"/"|"/"^") _ right:ComparisonExpression { return { type:"BinaryOperation", operation: OPERATORS[op], location: location(), right } })*
		{ return associate("left", left, right) }

ComparisonExpression
	= left:ShiftExpression right:(op:("=="/"="/"!="/"<>"/">="/"<="/">"/"<") _ right:ShiftExpression { return { type:"BinaryOperation", operation: OPERATORS[op], location: location(), right } })*
		{ return associate("left", left, right) }

ShiftExpression
	= left:AdditionExpression right:(op:(">>>"/">>"/"<<") _ right:AdditionExpression { return { type:"BinaryOperation", operation: OPERATORS[op], location: location(), right } })*
		{ return associate("left", left, right) }

AdditionExpression
	= left:MultiplicationExpression right:(op:("+"/"-") _ right:MultiplicationExpression { return { type:"BinaryOperation", operation: OPERATORS[op], location: location(), right } })*
		{ return associate("left", left, right) }

MultiplicationExpression
	= left:PowerExpression right:(op:("*" !"*"/"/"/"%") _ right:PowerExpression { return { type:"BinaryOperation", operation: OPERATORS[op], location: location(), right } })*
		{ return associate("left", left, right) }

PowerExpression
	= left:UnaryExpression "**" _ right:PowerExpression
		{ return { type: "BinaryOperation", operation: "Power", location: location(), left, right } }
	/ UnaryExpression

UnaryExpression
	= "-" _ value:UnaryExpression
		{ return { type: "UnaryOperation", operation: "Negation", location: location(), value } }
	/ "~" _ value:UnaryExpression
		{ return { type: "UnaryOperation", operation: "Complement", location: location(), value } }
	/ "&" _ value:UnaryExpression
		{ return { type: "UnaryOperation", operation: "Reference", location: location(), value } }
	/ "*" _ value:UnaryExpression
		{ return { type: "UnaryOperation", operation: "Dereference", location: location(), value } }
	/ TopExpression

TopExpression
	= "(" _ e:Expression ")" _
		{ return e }
	/ String
	/ Number
	/ Variable

Variable
	= identifier:Identifier ops:VariableOperation*
		{ return associate("value", identifier, ops) }

VariableOperation
	= "[" _ index:Expression "]" _
		{ return { type: "Index", location: location(), index } }
	/ "." _ name:Identifier
		{ return { type: "Property", location: location(), name } }
	/ "(" _ args:(value:Expression "," _ { return value })* last:Expression? ")" _
		{ return { type: "FunctionCall", location: location(), args: last && args.concat(last) } }

String
	= '"' value:$(!'"' .)* '"' _
		{ return { type: "String", location: location(), value }}

Number
	= "0x"i value:$([A-F0-9]i+) _
		{ return { type: "Number", value: parseInt(value, 16), location: location() } }
	/ "0b"i value:$([01]+) _
		{ return { type: "Number", value: parseInt(value, 2), location: location() } }
	/ "'" char:. "'"
		{ return { type: "Number", value: char.charCodeAt(0), location: location() } }
	/ value:$[0-9]+ _
		{ return { type: "Number", value: parseInt(value, 10), location: location() } }

Identifier
	= !ReservedWord name:$("$"? [A-Z_]i [A-Z0-9_]i*) _
		{ return { type: "Identifier", location: location(), name } }

ReservedWord
	= name:ReservedWords ![A-Z]i

ReservedWords
	= "using"i
	/ "global"i
	/ "local"i
	/ "define"i
	/ "default"i
	/ "switch"i
	/ "if"i
	/ "then"i
	/ "else"i
	/ "do"i
	/ "while"i
	/ "const"i
	/ "end"i
	/ "return"i

	// These are the bullshit that megazeux uses
	/ "SHOOT"i
	/ "ENABLE"i
	/ "SPITFIRE"i
	/ "SHOOTMISSILE"i
	/ "MOVE"i
	/ "DUPLICATE"i
	/ "ORDER"i
	/ "LAVAWALKER"i
	/ "LAST"i
	/ "SAM"i
	/ "FIREWALKER"i
	/ "TAKEKEY"i
	/ "FLIP"i
	/ "FADE"i
	/ "NONPUSHABLE"i
	/ "GOTO"i
	/ "RESTORE"i
	/ "CLEAR"i
	/ "ROTATECCW"i
	/ "ALL"i
	/ "SCROLLVIEW"i
	/ "EXPLODE"i
	/ "ARROW"i
	/ "GO"i
	/ "MISSILECOLOR"i
	/ "VIEWPORT"i
	/ "FIRST"i
	/ "ON"i
	/ "SFX"i
	/ "EDIT"i
	/ "SEND"i
	/ "WRITE"i
	/ "ITEM"i
	/ "ATTACK"i
	/ "ABORT"i
	/ "COPYROBOT"i
	/ "THIN"i
	/ "LOCKPLAYER"i
	/ "SAVE"i
	/ "LOOP"i
	/ "SAVING"i
	/ "GOTOXY"i
	/ "MAXHEALTH"i
	/ "VOLUME"i
	/ "STATIC"i
	/ "BOARD"i
	/ "BLOCK"i
	/ "LAYBOMB"i
	/ "BLIND"i
	/ "NONE"i
	/ "CENTER"i
	/ "COLOR"i
	/ "NEUTRAL"i
	/ "AVALANCHE"i
	/ "SCROLLBASE"i
	/ "POSITION"i
	/ "SCROLLPOINTER"i
	/ "TRANSPARENT"i
	/ "ROW"i
	/ "LOAD"i
	/ "ENEMY"i
	/ "TRY"i
	/ "GIVEKEY"i
	/ "INPUT"i
	/ "EW"i
	/ "FILLHEALTH"i
	/ "SIZE"i
	/ "TELEPORT"i
	/ "START"i
	/ "SCROLLTITLE"i
	/ "ZAP"i
	/ "WIND"i
	/ "MOD"i
	/ "ENDLIFE"i
	/ "CLIP"i
	/ "GIVE"i
	/ "SLOWTIME"i
	/ "STOP"i
	/ "CHAR"i
	/ "REL"i
	/ "RESETVIEW"i
	/ "OPEN"i
	/ "SET"i
	/ "COLUMN"i
	/ "TRADE"i
	/ "SWITCH"i
	/ "DISABLE"i
	/ "SWAP"i
	/ "ASK"i
	/ "WORLD"i
	/ "ENDGAME"i
	/ "NS"i
	/ "OUT"i
	/ "SCROLLCORNER"i
	/ "PLAYERCOLOR"i
	/ "EXCHANGE"i
	/ "PERCENT"i
	/ "JUMP"i
	/ "PUSH"i
	/ "LOCKSELF"i
	/ "PUSHABLE"i
	/ "THICK"i
	/ "WAIT"i
	/ "BULLETE"i
	/ "SHOOTSEEKER"i
	/ "BULLETN"i
	/ "UNLOCKSCROLL"i
	/ "PERSISTENT"i
	/ "BULLETW"i
	/ "INTENSITY"i
	/ "BULLETS"i
	/ "PUT"i
	/ "LAZERWALL"i
	/ "PALETTE"i
	/ "SCROLLARROW"i
	/ "UNLOCKPLAYER"i
	/ "DIE"i
	/ "COUNTER"i
	/ "WALK"i
	/ "ROTATECW"i
	/ "CHANGE"i
	/ "NONLAVAWALKER"i
	/ "OVERLAY"i
	/ "FREEZETIME"i
	/ "UNLOCKSELF"i
	/ "SENSORONLY"i
	/ "BECOME"i
	/ "CYCLE"i
	/ "STATUS"i
	/ "PLAY"i
	/ "STRING"i
	/ "BULLETCOLOR"i
	/ "PLAYER"i
	/ "IN"i
	/ "COPY"i
	/ "MESG"i
	/ "ID"i
	/ "HIGH"i
	/ "SELF"i
	/ "EDGE"i
	/ "TAKE"i
	/ "LOCKSCROLL"i
	/ "MESSAGE"i
	/ "SCROLL"i
	/ "COUNTERS"i

LineBreak
	= ("\r\n" / "\n" / "\r") _
		{ return { type: "LineBreak", location: location() } }

WB
	= ![A-Z0-9_] _

_
	= [ \t]*
