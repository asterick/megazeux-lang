{
	const OPERATORS = {
		"..": "Concatinate",
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
	= Expression
	/ LineBreak

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
	/ TopExpression

TopExpression
	= _ "(" e:Expression _ ")"
		{ return e }
	/ identifier:Identifier ops:VariableOperation*
		{ return associate("value", identifier, ops) }
	/ Number
	/ String

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
	= _ !ReservedWord name:$("$"? [A-Z_]i [A-Z0-9_]i*)
		{ return { type: "Identifier", location: location(), name } }

ReservedWord
	= _ ReservedWords ![A-Z]i

ReservedWords
	= "IF"i

LineBreak
	= _ ("\r\n" / "\n" / "\r")
		{ return { type: "LineBreak", location: location() } }

_
	= [ \t]*
