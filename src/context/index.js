const path = require('path');
const fs = require('fs');

const parser = require('../parser');
const logging = require('../logging');
const locate = require('../locate');

function defer(cb) {
	setTimeout(cb, 0);
}

class Module {
	constructor (ctx, fn) {
		this._defaultName = path.basename(fn).split(".")[0];

		this._context = ctx;
		this._globals = {};

		const ast = parser.parse(fs.readFileSync(fn, 'utf-8'));
		this._root = ctx.unpack(ast, this._globals, this._globals, path.dirname(fn));
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

	unpack(body, parent, globals, root, depth = 0) {
		const defines = Object.create(parent);
		const localDepth = 0;
		const terms = [];

		body.forEach(term => {
			switch (term.type) {
			case 'ImportStatement':
				{
					if (term.file.type !== 'String') throw Error("Illegal import argument")
			
					const i = this.import(term.file.value, root);			
					const n = term.name ? term.name.name : i._defaultName
		
					defines[n.toUpperCase()] = i;
				}
				break ;
			case 'GlobalStatement':
				{
					const n = term.name.name;
					const target = this.global(n);
					globals[n] = defines[n] = target
					
					terms.push({ 
						type:"Assignment", 
						location: term.location,
						value: this.unpackTerm(term.value),
						target
					});
				}
				break ;
			case 'LocalStatement':
				{
					const n = term.name.name;
					const target = { type: "Local", depth: depth++ };
					defines[n] = target
					
					terms.push({ 
						type:"Assignment", 
						location: term.location,
						value: this.unpackTerm(term.value),
						target
					});
				}
				break ;
			case 'ConstStatement':
				{
					const n = term.name.name;
					defines[n] = this.constant(n);
				}
				break ;
			case 'FunctionStatement':
			case 'AssignmentStatement':
			case 'ExpressionStatement':
			case 'ZeuxStatement':
				//console.log(term);
				break ;

			case 'Comment':
			case 'LineBreak':
				terms.push(term);
				break ;
			default:
				throw new Error(`Unhandled type: ${term.type}`);
			}
		});

		return { statements: terms, localDepth: depth };
	}

	unpackTerm(tree, defines, globals) {
		console.log(tree);
		return tree;
	}

	global (name) {
		return { type: "Global", name: "name" }
	}

	local (name) {
		return { type: "Global", name: "name" }
	}

	constant (name) {
		return { type: "Global", name: "name" }
	}

	// Helper functions
	constants(tree) {

	}
}

module.exports = Context;
