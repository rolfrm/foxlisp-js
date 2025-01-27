const { LispEvalBlock } = require("./lisp.js")
require("./foxlisp-node.js")

eval("net = require('node:net')"); 
startup = "lisp/lisp.lisp"
console.log("argv???",  Bun.argv)
let code = ""
let noStartup = false
let logEval = false;
___process_args = []
for(i = 2; i < process.argv.length; i++){
	 if(process.argv[i] == "--"){
		  i++;
		  for(; i < process.argv.length; i++){
				___process_args.push(process.argv[i])
		  }
		  break;
	 }
	 if(process.argv[i] == "--no-startup"){
		  noStartup = true
		  continue;
	 }
	 if(process.argv[i] == "--script") {
		  code += process.argv[i + 1];
		  i += 1;
		  continue;
	 }
	 if(process.argv[i] == "--log-eval"){
		  logEval = true;
		  continue;
	 }
    code += "(loadfile \"" + process.argv[i] + "\")";
}

if(!noStartup){
    code = "(loadfile \"" + startup + "\")"+ code 
}
if(logEval){
	 doEval = function (code){
		  console.log(code)
		  eval?.(code)

	 }
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
