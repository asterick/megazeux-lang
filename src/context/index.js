const path = require('path');
const fs = require('fs');

const parser = require('../parser');
const logging = require('../logging');
const locate = require('../locate');

class Module {
	constructor (ctx, fn) {
		this._path = fn;
		this._defaultName = path.basename(fn).split(".")[0];

		this._ast = parser.parse(fs.readFileSync(fn, 'utf-8'));
		this._context = ctx;
		this._globals = {};

		// TODO: UNROLL STATEMENTS INTO A FLAT FILE
		console.log(JSON.stringify(this._ast, null, 2));
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
		this._modules[target] = new Module(this, target);

		return this._modules[target];
	}

	export(fn) {
		// TODO: WRITE OUT TO FILE HERE
	}
}

module.exports = Context;
