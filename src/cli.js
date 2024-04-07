const { LispEvalBlock } = require("./lisp.js")
require("./foxlisp-node.js")

eval("net = require('node:net')"); 

let code = ""

for(i = 2; i < process.argv.length; i++){
    code += "(loadfile \"" + process.argv[i] + "\")";
}

promise = LispEvalBlock(code);
