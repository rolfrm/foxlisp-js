const { LispEvalBlock } = require("./lispy")
require("./foxlisp-node.js")

let code = ""

for(i = 2; i < process.argv.length; i++){
    code += "(load \"" + process.argv[i] + "\")";
}

LispEvalBlock(code);
