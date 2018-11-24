const path = require('path');
const fs = require('fs');
const { Gaze } = require('gaze');

const parser = require('./parser');
const logging = require('./logging');
const locate = require('./locate');

class Module {
	constructor (fn) {
		this._path = fn;
		this._defaultName = path.basename(fn).split(".")[0];

		this._ast = parser.parse(fs.readFileSync(fn, 'utf-8'));
		this._imports = {};

		console.log(this._defaultName);
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
		this._modules[target] = new Module(target);
	}

	export(fn) {
		// TODO: WRITE OUT TO FILE HERE
	}
}

function compile(target, files) {
	const context = new Context();

	files.forEach((fileName) => context.import(fileName, process.cwd()));
	context.export(path.resolve(process.cwd(), target));

	return context.files;
}

module.exports = function (args) {
	logging.level = args.logging;

	if (args.watch) {
		function changed() {
			var files = compile(args.target, args.files);		
			var gaze = new Gaze(files);
			
			gaze.on('all', (event, filepath) => {
				console.log(filepath);
				gaze.close();
				changed();
			})
		}

		changed();
	} else {
		compile(args.target, args.files);
	}
}
