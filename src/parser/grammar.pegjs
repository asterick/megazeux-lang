Body
	= body:Statement* _
		{ return { type: "ProgramBody", body } }

Statement
	= Expression
	/ LineBreak

Expression
	= ConditionalExpression

ConditionalExpression
	= test:BitwiseExpression _ "?" onTrue:ConditionalExpression _ ":" onFalse:ConditionalExpression
		{ return { type: "ConditionalOperation", location: location(), test, onTrue, onFalse } }
	/ BitwiseExpression

BitwiseExpression
	= left:ComparisonExpression _ "&" right:BitwiseExpression
		{ return { type: "BinaryOperation", operation: "And", location: location(), left, right } }
	/ left:ComparisonExpression _ "|" right:BitwiseExpression
		{ return { type: "BinaryOperation", operation: "Or", location: location(), left, right } }
	/ left:ComparisonExpression _ "^" right:BitwiseExpression
		{ return { type: "BinaryOperation", operation: "ExclusiveOr", location: location(), left, right } }
	/ ComparisonExpression

ComparisonExpression
	= left:ShiftExpression _ "=" "="? right:ComparisonExpression
		{ return { type: "BinaryOperation", operation: "Equal", location: location(), left, right } }
	/ left:ShiftExpression _ "!=" "<>"? right:ComparisonExpression
		{ return { type: "BinaryOperation", operation: "NotEqual", location: location(), left, right } }
	/ left:ShiftExpression _ "<=" right:ComparisonExpression
		{ return { type: "BinaryOperation", operation: "LessEqual", location: location(), left, right } }
	/ left:ShiftExpression _ ">=" right:ComparisonExpression
		{ return { type: "BinaryOperation", operation: "GreaterEqual", location: location(), left, right } }
	/ left:ShiftExpression _ "<" right:ComparisonExpression
		{ return { type: "BinaryOperation", operation: "Less", location: location(), left, right } }
	/ left:ShiftExpression _ ">" right:ComparisonExpression
		{ return { type: "BinaryOperation", operation: "Greater", location: location(), left, right } }
	/ ShiftExpression

ShiftExpression
	= left:AdditionExpression _ ">>>" right:ShiftExpression
		{ return { type: "BinaryOperation", operation: "ArithmaticShiftRight", location: location(), left, right } }
	/ left:AdditionExpression _ ">>" right:ShiftExpression
		{ return { type: "BinaryOperation", operation: "ShiftRight", location: location(), left, right } }
	/ left:AdditionExpression _ "<<" right:ShiftExpression
		{ return { type: "BinaryOperation", operation: "ShiftLeft", location: location(), left, right } }
	/ AdditionExpression

AdditionExpression
	= left:MultiplicationExpression _ "+" right:AdditionExpression
		{ return { type: "BinaryOperation", operation: "Add", location: location(), left, right } }
	/ left:MultiplicationExpression _ "-" right:AdditionExpression
		{ return { type: "BinaryOperation", operation: "Subtract", location: location(), left, right } }
	/ MultiplicationExpression

MultiplicationExpression
	= left:PowerExpression _ "*" right:MultiplicationExpression
		{ return { type: "BinaryOperation", operation: "Multiply", location: location(), left, right } }
	/ left:PowerExpression _ "/" right:MultiplicationExpression
		{ return { type: "BinaryOperation", operation: "Divide", location: location(), left, right } }
	/ left:PowerExpression _ "%" right:MultiplicationExpression
		{ return { type: "BinaryOperation", operation: "Modulo", location: location(), left, right } }
	/ PowerExpression

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
	/ Identifier
	/ Number

Number
	= _ "0x"i value:$([A-F0-9]i+)
		{ return { type: "Number", value: parseInt(value, 16), location: location() } }
	/ _ "0b"i value:$([01]+)
		{ return { type: "Number", value: parseInt(value, 2), location: location() } }
	/ _ value:$[0-9]+
		{ return { type: "Number", value: parseInt(value, 10), location: location() } }

Identifier
	= _ !ReservedWord name:$([A-Z_]i [A-Z0-9_]i+)
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
