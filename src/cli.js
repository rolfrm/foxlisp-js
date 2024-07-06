const { LispEvalBlock } = require("./lisp.js")
require("./foxlisp-node.js")

eval("net = require('node:net')"); 

let code = ""

for(i = 2; i < process.argv.length; i++){
    code += "(loadfile \"" + process.argv[i] + "\")";
}

async function EvalTopLevel(code){
	 try{
		  await LispEvalBlock(code, "toplevel");
		  if(error != null){
				throw error
		  }
		  
	 }catch(err){
		  
		  if(err._at)
				console.log("Unhandled error at " + err._at + ".")
		  else
				console.log("Unhandled error. ")
		  
		  console.log(err)	  
	 }
	 console.log("Finished application")
}

EvalTopLevel(code)
