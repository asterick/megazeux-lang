const path = require('path');
const { Gaze } = require('gaze');

const logging = require('./logging');
const Context = require('./context');

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
