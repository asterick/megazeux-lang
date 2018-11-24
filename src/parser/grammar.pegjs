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
		"%": "Modulo"
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
	= body:Statement* _
		{ return { type: "ProgramBody", body } }

Statement
	= UsingStatement
	/ FunctionStatement
	/ SwitchStatement
	/ GlobalStatement
	/ LocalStatement
	/ AssignmentStatement
	/ ReturnStatement
	/ ZeuxTerm
	/ Variable // For function calls
	/ LineBreak

UsingStatement
	= _ "IMPORT"i file:String name:(_ "AS"i name:Identifier { return name })?
		{ return { type: "ImportModule", file, name } }

FunctionStatement
	= _ "DEFINE"i name:Identifier _"(" first:(p:Identifier _ "," { return p })* last:Identifier? _ ")"
		body: Statement*
	  _ "END"i
		{ return { type: "FunctionStatement", name, parameters:last && first.concat(last), location: location(), body } }

SwitchStatement
	= _ "SWITCH"i match:Expression cases:SwitchCaseStatement* _ "END"i
		{ return { type: "SwitchStatement", location: location(), match, cases }}

SwitchCaseStatement
	= match:(term:(Number/String) _ "," { return term })* last:(Number/String) _ ":" body:Statement*
		{ return { type: "SwitchCondition", match: match.concat(last), location: location(), body }}
	/ _ "DEFAULT"i _ ":" body:Statement*
		{ return { type:"SwitchDefault", location: location(), body }}
	/ LineBreak

GlobalStatement
	= _ "GLOBAL"i name:Identifier initializer:(_ "=" value:Expression { return value })?
		{ return { type: "GlobalStatement", name, initializer } }

LocalStatement
	= _ "LOCAL"i name:Identifier initializer:(_ "=" value:Expression { return value })?
		{ return { type: "LocalStatement", name, initializer } }

AssignmentStatement
	= target:Variable _ op:("+"/"-"/"*"/"/"/"%"/"&"/"|"/"^") "=" right:Expression
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
	/ target:Variable _ "=" value:Expression
		{ return { type:"AssignmentStatement", location: location(), target, value } }

ReturnStatement
	= _ "RETURN"i value:Expression
		{ return { type:"ReturnStatement", location: location(), value } }

Expression
	= ConcatinationExpression

ConcatinationExpression
	= left:ConditionalExpression _ ".." right:ConcatinationExpression
		{ return { type: "BinaryOperation", operation: "Concat", location: location(), left, right } }
	/ ConditionalExpression

ConditionalExpression
	= test:BitwiseExpression _ "?" onTrue:ConditionalExpression _ ":" onFalse:ConditionalExpression
		{ return { type: "ConditionalOperation", location: location(), test, onTrue, onFalse } }
	/ BitwiseExpression

BitwiseExpression
	= left:ComparisonExpression _ right:(_ op:("&"/"|"/"^") right:ComparisonExpression { return { type:"BinaryOperation", operation: OPERATORS[op], location: location(), right } })*
		{ return associate("left", left, right) }

ComparisonExpression
	= left:ShiftExpression _ right:(_ op:("=="/"="/"!="/"<>"/">="/"<="/">"/"<") right:ShiftExpression { return { type:"BinaryOperation", operation: OPERATORS[op], location: location(), right } })*
		{ return associate("left", left, right) }

ShiftExpression
	= left:AdditionExpression _ right:(_ op:(">>>"/">>"/"<<") right:AdditionExpression { return { type:"BinaryOperation", operation: OPERATORS[op], location: location(), right } })*
		{ return associate("left", left, right) }

AdditionExpression
	= left:MultiplicationExpression _ right:(_ op:("+"/"-") right:MultiplicationExpression { return { type:"BinaryOperation", operation: OPERATORS[op], location: location(), right } })*
		{ return associate("left", left, right) }

MultiplicationExpression
	= left:PowerExpression _ right:(_ op:("*" !"*"/"/"/"%") right:PowerExpression { return { type:"BinaryOperation", operation: OPERATORS[op], location: location(), right } })*
		{ return associate("left", left, right) }

PowerExpression
	= left:UnaryExpression _ "**" right:PowerExpression
		{ return { type: "BinaryOperation", operation: "Power", location: location(), left, right } }
	/ UnaryExpression

UnaryExpression
	= _ "-" value:UnaryExpression
		{ return { type: "UnaryOperation", operation: "Negation", location: location(), value } }
	/ _ "~" value:UnaryExpression
		{ return { type: "UnaryOperation", operation: "Complement", location: location(), value } }
	/ _ "&" value:UnaryExpression
		{ return { type: "UnaryOperation", operation: "Reference", location: location(), value } }
	/ _ "*" value:UnaryExpression
		{ return { type: "UnaryOperation", operation: "Dereference", location: location(), value } }
	/ TopExpression

TopExpression
	= _ "(" e:Expression _ ")"
		{ return e }
	/ String
	/ Number
	/ Variable

Variable
	= identifier:Identifier ops:VariableOperation*
		{ return associate("value", identifier, ops) }

VariableOperation
	= _ "[" index:Expression _ "]"
		{ return { type: "Index", location: location(), index } }
	/ _ "." name:Identifier
		{ return { type: "Property", location: location(), name } }
	/ _ "(" args:(value:Expression _ "," { return value })* last:Expression? _ ")"
		{ return { type: "FunctionCall", location: location(), args: last && args.concat(last) } }

String
	= _ '"' value:$(!'"' .)* '"'
		{ return { type: "String", location: location(), value }}

Number
	= _ "0x"i value:$([A-F0-9]i+)
		{ return { type: "Number", value: parseInt(value, 16), location: location() } }
	/ _ "0b"i value:$([01]+)
		{ return { type: "Number", value: parseInt(value, 2), location: location() } }
	/ _ value:$[0-9]+
		{ return { type: "Number", value: parseInt(value, 10), location: location() } }

Identifier
	= _ !ZeuxTerm !ReservedWord name:$("$"? [A-Z_]i [A-Z0-9_]i*)
		{ return { type: "Identifier", location: location(), name } }

ReservedWord
	= _ name:ReservedWords ![A-Z]i

ZeuxTerm
	= _ name:ZeuxTerms ![A-Z]i
		{ return { type: "ZeuxTerm", location: location(), name } }

ReservedWords
	= "using"i
	/ "global"i
	/ "local"i
	/ "define"i
	/ "end"i
	/ "default"i
	/ "switch"i

ZeuxTerms
	= "c"i [0-9?][0-9?]
	/ "p"i [0-9?][0-9?]
	/ "lockplayer"i

LineBreak
	= _ ("\r\n" / "\n" / "\r")
		{ return { type: "LineBreak", location: location() } }

_
	= [ \t]*
