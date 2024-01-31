const { LispEvalBlock } = require("./lispy")
require("./foxlisp-node.js")

eval("net = require('node:net')"); 

let code = ""

for(i = 2; i < process.argv.length; i++){
    code += "(load \"" + process.argv[i] + "\")";
}

LispEvalBlock(code);
