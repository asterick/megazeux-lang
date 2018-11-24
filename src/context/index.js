const path = require('path');
const fs = require('fs');

const parser = require('../parser');
const logging = require('../logging');
const locate = require('../locate');

class Scope {
	constructor(globals) {
		this._globals = globals || {};
		this._locals = Object.create(this._globals);

		this._maxStrings = 0;
		this._maxNumber = 0;
		this._scopeStack = [];
	}

	global(name) {

	}

	local(name) {
		console.log(name)
	}

	constant(name, value) {

	}

	push() {

	}

	pop() {

	}
}

class Module {
	constructor (ctx, fn) {
		this._defaultName = path.basename(fn).split(".")[0].toUpperCase();

		this._context = ctx;
		this._scope = new Scope();

		const ast = parser.parse(fs.readFileSync(fn, 'utf-8'));
		this._root = ctx.unpack(ast, this._scope, path.dirname(fn));
	}
}

class Context {
	constructor() {
		this._modules = {};
		this.files = [];
	}

	import(fn, root) {
		const target = locate(fn, root);

		if (this._modules[target]) return this._modules[target];

		logging.info(`Importing: ${fn} (${target})`)

		this.files.push(target);
		const mod = new Module(this, target);
		this._modules[target] = mod;

		return mod;
	}

	export(fn) {
		// TODO: WRITE OUT TO FILE HERE
	}

	// Create global entries
	unpack(body, scope, root) {
		const localDepth = 0;
		const terms = [];

		scope.push();

		body.forEach(term => {
			switch (term.type) {
			case 'ImportStatement':
				{
					const i = this.import(term.file.value, root);			
					const name = term.name ? term.name.name : i._defaultName
		
					scope.constant(name, i);
				}
				break ;

			case 'GlobalStatement':
				{
					const name = term.name.name;
					
					terms.push({ 
						type: "Assignment", 
						location: term.location,
						value: this.unpackExpression(term.value, scope),
						target: scope.global(name)
					});
				}
				break ;

			case 'LocalStatement':
				{
					const name = term.name.name;

					terms.push({ 
						type: "Assignment", 
						location: term.location,
						value: this.unpackExpression(term.value, scope),
						target: scope.local(name)
					});
				}
				break ;

			case 'ConstStatement':
				{
					const name = term.name.name;
					scope.constant(this.unpackExpression(term.value, scope));
				}
				break ;

			case 'FunctionStatement':
			case 'AssignmentStatement':
			case 'ExpressionStatement':
				break ;
			case 'ZeuxStatement':
				term.body.forEach(term => {
					if (typeof term === 'string') {
						terms.push({
							type: "RawCode",
							value: term
						});

						return ;
					}

					terms.push(term);
				})
				break ;
			case 'Comment':
				term.body.split(/\r\n?|\n\r?/g).forEach(v => {
					terms.push({
						type: "RawCode",
						value: v
					});
				})
				break ;
			case 'LineBreak':
				terms.push({
					type: "RawCode",
					value: "\n"
				});
				break ;
			default:
				throw new Error(`Unhandled type: ${term.type}`);
			}
		});

		scope.pop();

		return terms;
	}

	unpackExpression(exp, scope) {
		return exp;
	}
}

module.exports = Context;
