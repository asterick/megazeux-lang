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
	= UsingStatement
	/ FunctionStatement
	/ WhileStatement
	/ DoWhileStatement
	/ IfStatement
	/ SwitchStatement
	/ GlobalStatement
	/ LocalStatement
	/ ConstStatement
	/ AssignmentStatement
	/ Expression
	/ ZeuxTerm
	/ LineBreak
	/ CommentStatement
	/ ReturnStatement

CommentStatement
	= "//" body:$(![\n\r] .)* LineBreak
		{ return { type: "Comment", location: location(), body } }
	/ "/*" body:$(!"*/" .)* "*/"
		{ return { type: "Comment", location: location(), body } }

UsingStatement
	= "IMPORT"i WB file:String name:("AS"i WB name:Identifier { return name })?
		{ return { type: "ImportModule", file, name } }

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
	= "IF"i WB expression:Expression
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
	= "GLOBAL"i WB name:Identifier initializer:("=" _ value:Expression { return value })?
		{ return { type: "GlobalStatement", name, initializer } }

LocalStatement
	= "LOCAL"i WB name:Identifier initializer:("=" _ value:Expression { return value })?
		{ return { type: "LocalStatement", name, initializer } }

ConstStatement
	= "CONST"i WB name:Identifier initializer:"=" _ value:Expression
		{ return { type: "LocalStatement", name, initializer } }

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
	/ value:$[0-9]+ _
		{ return { type: "Number", value: parseInt(value, 10), location: location() } }

Identifier
	= !ZeuxTerm !ReservedWord name:$("$"? [A-Z_]i [A-Z0-9_]i*) _
		{ return { type: "Identifier", location: location(), name } }

ReservedWord
	= name:ReservedWords ![A-Z]i

ZeuxTerm
	= name:ZeuxTerms ![A-Z]i _
		{ return { type: "ZeuxTerm", location: location(), name } }

ReservedWords
	= "using"i
	/ "global"i
	/ "local"i
	/ "define"i
	/ "default"i
	/ "switch"i
	/ "if"i
	/ "else"i
	/ "do"i
	/ "while"i
	/ "const"i
	/ "end"i
	/ "return"i

ZeuxTerms
	= $("C"i [0-9?][0-9?])
	/ $("P"i [0-9?][0-9?])
	/ "SHOOT"i
	/ "ENABLE"i
	/ "SPITFIRE"i
	/ "SHOOTMISSILE"i
	/ "MOVE"i
	/ "DUPLICATE"i
	/ "ORDER"i
	/ "LAVAWALKER"i
	/ "SLOWTIME"i
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
	/ "RANDOM"i
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
	/ "DOUBLE"i
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
	/ "STRING"i
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
	/ "STOP"i
	/ "TRY"i
	/ "GIVEKEY"i
	/ "INPUT"i
	/ "EW"i
	/ "FILLHEALTH"i
	/ "SIZE"i
	/ "TELEPORT"i
	/ "START"i
	/ "MATCHES"i
	/ "LAYBOMB"i
	/ "SCROLLTITLE"i
	/ "ZAP"i
	/ "NOT"i
	/ "ANY"i
	/ "WIND"i
	/ "ENDLIFE"i
	/ "CLIP"i
	/ "GIVE"i
	/ "HALF"i
	/ "CHAR"i
	/ "REL"i
	/ "RESETVIEW"i
	/ "OPEN"i
	/ "SET"i
	/ "SPRITE_COLLIDING"i
	/ "NO"i
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
	/ "IMAGE_FILE"i
	/ "LOCKSELF"i
	/ "PUSHABLE"i
	/ "THICK"i
	/ "WAIT"i
	/ "BULLETE"i
	/ "SHOOTSEEKER"i
	/ "ALIGNEDROBOT"i
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
	/ "ROTATECCW"i
	/ "SPRITE"i
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
	/ "MOD"i
	/ "SCROLL"i
	/ "COUNTERS"i

LineBreak
	= ("\r\n" / "\n" / "\r") _
		{ return { type: "LineBreak", location: location() } }

WB
	= ![A-Z0-9_] _

_
	= [ \t]*
