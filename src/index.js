const parser = require('./parser');
const logging = require('./logging');

//logging.level = args.logging;
console.log(JSON.stringify(parser.parse("~-~0xFF ** 5 ** 10"), null, 4))