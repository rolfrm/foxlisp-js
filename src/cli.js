const { LispEvalBlock } = require("./lisp.js")
require("./foxlisp-node.js")

eval("net = require('node:net')"); 

let code = ""
___process_args = []
for(i = 2; i < process.argv.length; i++){
	 if(process.argv[i] == "--"){
		  i++;
		  for(; i < process.argv.length; i++){
				___process_args.push(process.argv[i])
		  }
		  break;
	 }
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
