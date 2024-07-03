const { LispEvalBlock } = require("./lisp.js")
require("./foxlisp-node.js")

eval("net = require('node:net')"); 

let code = ""

for(i = 2; i < process.argv.length; i++){
    code += "(loadfile \"" + process.argv[i] + "\")";
}

async function nop(){
	 console.log("nop")
}

async function EvalTopLevel(code){
	 try{
		  await LispEvalBlock(code);
		  if(error != null){
				throw error
		  }
		  
	 }catch(err){
		  console.log("error occured")
		  console.log(err)	  
	 }
	 console.log("Finished application")
}

EvalTopLevel(code)
