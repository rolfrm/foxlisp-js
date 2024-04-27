const parser = require("./lispy_parser")
const lisp = require("./symbols")


console.log("loading lispy")
function sym(x, jsname){
    return lisp.sym(x, jsname)
}

function issym(x){
	 return x && x.type == "symbol";
}


quotes = []
quotes_lookup = new Map();
function _getQuote(id) {
    return quotes[id]
}
getQuote = _getQuote
function setQuote(newQuote){
	 if(quotes_lookup.has(newQuote)) {
		  return quotes_lookup.get(newQuote);
	 }
    let id = quotes.length;
    quotes.length += 1
    quotes[id] = newQuote
	quotes_lookup.set(newQuote, id)
    return id
}

lookupsym = lisp.getsym
car = (x) => x && x[0]
cdr = (x) => x && x.slice(1)
op_add = (x, y) => x + y
op_sub = (x, y) => x - y
op_div = (x, y) => x / y
op_mul = (x, y) => x * y
op_leftshift = (x, y) => x << y;
op_rightshift = (x, y) => x >> y;
op_xor = (x, y) => x ^ y;
op_or = (x,y) => x|y;
op_and = (x,y) => x&y;
mod = (x, y) => x % y
not = (x) => !x
len = (x) => (x && x.length) || 0
list = (...x) => x
makehashmap = () => new Map();
__undefined = undefined

eq = (a, b) => a === b;
slice = (a, n) => a && (a.length <= n ? null : a.slice(n));
raise = (err) => {
	 throw err;
};

usplice = (x) => ({type: "unsplice", value: x})

loadfile = (x, loadcontext) => ({type: "load", value: x, loadcontext: loadcontext})

macroLookup = new Map();

ismacro = (x) => macroLookup.has(x)

ulist = (...x) => {
	 let out = []
	 for (let elem of x){
		  if(elem == undefined){
				throw new Error("content is undefined" + x);
		  }
		  if(typeof(elem) == "object" && elem.type == "unsplice"){
				for (let elem2 of elem.value){
					 out.push(elem2);
				}
				
		  }else{
				out.push(elem);
		  }
	 }
	 
	 return out
}

function escapeString(x){
	 return x.replace(/"/g, '""').replace(/\n/g, '\\n')
}

getsym = (s) => sym(s, null)

function println_impl(obj){
	 //console.log("print? ", Array.isArray(obj), obj)
	 if(Array.isArray(obj)){
		  let strOut = ""
		  let first = true
		  strOut += "("
		  for(let elem of obj){
				if(!first){
					 strOut = strOut + " ";
				}else{
					 first = false;
					 if(elem == quoteSym){
						  return "'" + println_impl(obj[1]);
					 }
				}
				strOut += println_impl(elem)

		  }
		  strOut += ")"
		  return strOut;
	 }else{
		  if(obj == undefined){
				return "undefined"
		  }
		  if(obj == null){
				return "null";
		  }
		  if(obj.type == "symbol"){
				return obj.value;
		  }
		  return obj.toString()
	 }
}

println = (...a) => {
	 let combined = ""
	 let first = true
	 for(let elem of a){
		  if(!first){
				combined = combined + ", "
		  }else{
				first = false
		  }
		  combined = combined + println_impl(elem)
	 }
	 // print without newline:
	 console.log(combined)
	 return a[0]
}

__valueToString = (a) => println_impl(a);

nth = (a, n) => a && (n >= a.length ? null : a[n])
setnth = (a, n, v) => a[n] = v
getnth = (a, n) => a[n]
makemap_ = () => ({type: "lisp-object"})
put = (obj, name, value) =>  obj[name.jsname ? name.jsname : name] = value
get = (obj, name) => obj[name.jsname ? name.jsname : name]
charcode = (a) => a.charCodeAt(0)
strfromchar = (...a) => String.fromCharCode(...a)
reverse = (a) => a.slice().reverse()
_op_gte = (a,b) => a >= b
_op_lte = (a,b) => b >= a
_op_lt = (a,b) => a < b
_op_gt = (a,b) => a > b
concat = (...lst)=> len(lst) == 0 ? [] : lst[0].concat(...lst.slice(1))
__makesym = (a) => sym(a)
_typeof = (a) => typeof a


const loopSym = sym("loop");
const notSym = sym("not");
const setSym = sym("set");
const letSym = sym("let");
const prognSym = sym("progn");
const ifSym = sym("if");
const lambdaSym = sym("_lambda");
const defMacroSym = sym("setmacro");
const orSym = sym("or");
const andSym = sym("and");
const blockSym = sym("_block");
const returnFromSym = sym("return-from")
const jsSym = sym("%js")
const quoteSym = lisp.quote_sym;
const quasiQuoteSym = lisp.quasiquote_sym;
const quasiUnQuoteSym = lisp.quasiunquote_sym;
const quasiUnQuoteSpliceSym = lisp.quasiunquotesplice_sym;
const restSym = sym("&rest")
const declareSym = sym("declare")
const defvarSym = sym("defvar");
const defConstSym = sym("defconstant");
const handleErrorsSym = sym("handle-errors")
const loop_sym = sym("loop")

function quotedJs(code){
	 if(Array.isArray(code)){
		  const innerCode = code.map(elem => quotedJs(elem)).join(',');
		  return `[${innerCode}]`
	 }
	 if(code.type == "symbol"){
		  return `getsym("${escapeString(code.value)}")`
	 }
	 if(typeof(code) == 'string'){
		  return `"${escapeString(code)}"`
	 }	 
	 return code.toString();

}

function unscope(code){
	 if(isScope(code)){
		  return `(()=>{var tmp_unscope; ${code.replaceAll(value_marker, "tmp_unscope =")}; return tmp_unscope})()`
	 }
	 return code
}

function unquoteToJs(code){
	 if(code == null){
		  return "null"
	 }
	 if(Array.isArray(code)){
		  if(code[0] == quasiQuoteSym){
				
				return quotedJs(code) 
		  }
		  if(code[0] == quasiUnQuoteSpliceSym){
				
				return `usplice(${unscope(lispCompile(code[1]))})`;
		  }
		  if(code[0] == quasiUnQuoteSym){
				return unscope(lispCompile(code[1]))
		  }
		  const innerCode = code.map(elem => unquoteToJs(elem)).join(',');
		  return `ulist(${innerCode})`;
	 }
	 if(code.type == "symbol"){
		  return `getsym("${escapeString(code.value)}")`
	 }
	 if(typeof(code) == 'string'){
		  return `"${escapeString(code)}"`
	 }
	 
	 
	 return code.toString();
}

function scope(items){
	 for(let i = 0; i < items.length; i++){
		  if(i == items.length -1){
				if(isScope(items[i])){
					 
					 
				}else{
					 items[i] = value_marker + " " + items[i]
				}
		  }else{
				if(isScope(items[i]))
					 items[i] = items[i].replaceAll(value_marker,"")
		  }
		  
      
	 }
	 let a =  items.map(i => i + ";").join("")
	 return a
}

const value_marker = "__>>VALUEMARKER<<__"
function isScope(code){
	 return typeof(code) == 'string' && code.includes(value_marker)
}

function lispCompileLet(variables, body, isConst){
	 
	 const  varCode = variables.map(updateExpr => {
		  if(updateExpr.length != 2){
				throw new Error("The expression (xyz) is malformed." + updateExpr.toString());
		  }
        const [left, right] = updateExpr;
		  let r = lispCompile2(right)
		  if(isScope(r)){
				return `{let ${left.jsname};${r.replaceAll(value_marker, left.jsname + "=")}`
		  }else{
				code = `{let ${left.jsname} = ${r}`
		  }
        return code;
    });
    if(body.length == 0){
		  body = [[]]
	 }
	 if(body.length == 1 && variables.length == 0){
		  return `${lispCompile(body[0])}`
	 }
	 return scope(varCode.concat(body.map(x => lispCompile(x)))) + variables.map(x => "}").join("")
}
lmbmark = (id, f) => {
	f.assoc_id = id;
	return f;
}

let associd = 0;
let assoc = {}
                ///\/\*lmb#(\d+)\*\//
const markRegex = /\/\*lmb#(\d+)\*\/\(/g;
let codeStack = []

function lispCompile(code, assignto){
	 const result = lispCompile2(code);

	 if(isScope(result) && assignto){
		  const fin = result.replace(value_marker, assignto);
		  return fin
	 }

	 if(assignto){
        const r = assignto + " " + result;
		  
		  return r
	 }
	 
	 return result;

}

function lispCompile2(code) {
	 
	 try{
		  codeStack.push(code);
	 if(typeof(code) == "number"){
        return code
	 }
	 if(typeof(code) == "string"){
		  code = code.replaceAll("\"", "\\\"");
		  code = code.replaceAll("\n", "\\n");
		  return `\"${code}\"`;
	 }
	 if(code == undefined){
		  return "undefined"
	 }
    if (code.type == "symbol"){
        return code.jsname
    }
    if(code.length == 0) {
        return "null"
    }

    const [operator, ...operands] = code;
    switch (operator) {
    case loopSym:
        const [condition, ...update] = operands;
        
		  let updateCode = lispCompileLet([], update);
		  let conditionCode = lispCompile(condition);
		  if(!isScope(updateCode)){
				updateCode = value_marker + updateCode
		  }
		  if(isScope(conditionCode)){
				conditionCode = conditionCode.replaceAll(value_marker, "condition =")
				return `while(true) { var condition; ${conditionCode}; if(!condition){break;} ${updateCode}}`;
		  }
        return `while (${conditionCode}) {  ${updateCode}; }`;
	 case jsSym:
		  {
			let outstr = "";
			for (let x of operands) {
				if (typeof(x) == "string") {
                   outstr += x;
				}else{
                   outstr += lispCompile(x)
				}
			}
			return outstr;
				
		  }
	 	  
    case blockSym:{
        const [sym, ...body] = operands;
        let bodyCode = lispCompile(body[0])
		  
		  if(!isScope(bodyCode)){
				
				bodyCode = value_marker + " " + bodyCode
				
		  }
        
        return `const ${sym.jsname} = {};try{${bodyCode}}catch(ex){if(ex.id === ${sym.jsname}){${value_marker} ex.value;}else{raise(ex);}}`
    }
    case returnFromSym:{
        const [sym, value] = operands;
		  let valuef = lispCompile(value);
		  let pre = ""
		  if(isScope(valuef)){
				pre = "var tmpValue;" + valuef.replaceAll(value_marker,"tmpValue =") + ";";
				valuef = "tmpValue";
		  }
        
        return `${pre} raise({id:${sym.jsname}, value:${valuef}, type: "return-from" })`;
    }
    case lambdaSym:
        {

            const [args, ...body] = operands;
			// find invalid args    
			for(let arg of args){
				if(arg.type != "symbol"){
					 throw new Error("Invalid argument in lambda: " + arg + " in " + println_impl(code));
				}
			}

				const restIndex = args.indexOf(restSym)
				let argstr = args.map(arg => arg.jsname).join(",")
				if(restIndex != -1){
					 argstr = args.slice(0, restIndex).map(arg => arg.jsname).concat(["..." + args[restIndex + 1].jsname]).join(",");
				}
			
				assoc[associd] = operands
            

            let bodyCode = lispCompileLet([], body)
				if(isScope(bodyCode)){
					 bodyCode = "{ var lambdaResult;"+ bodyCode.replaceAll(value_marker, "lambdaResult=") + "return lambdaResult}"
				}
            let lmb = `(${argstr}) => ${bodyCode}`;
				console.log(lmb)
				
				return lmb
		  }
	 case prognSym:
		  return lispCompileLet([], operands)
    case quoteSym:
        {
            const [quoted] = operands
				if(issym(quoted)){
					 return `getsym(\"${quoted.value}\")`
				}
				
            const id = setQuote(quoted)
            return `getQuote(${id})`
        }
	 case quasiQuoteSym:
		  {
				const [quoted] = operands
				const code = unquoteToJs(quoted)
				return code;

		  }
		  
    case defvarSym:
        {
			    
				const [sym, code] = operands;
				let valueCode = lispCompile(code);
				if (typeof(valueCode) == "string"){
					valueCode = valueCode.replace(markRegex, 'lmbmark($1,');
				}
				
				let code2 = `${sym.jsname} = ${valueCode}`;
				if(isScope(valueCode)){
					 code2 = `${sym.jsname} = null;${valueCode.replaceAll(value_marker, sym.jsname + "=")}`
				}
				//WriteCodeToLog(code2 + ";")
				let result = eval?.(code2);
				if(typeof(result) == "function" && result.assoc_id){
					result.lispname = sym
					//result.lispargs = assoc[result.assoc_id][0]
				}
				return `${sym.jsname}`
        }
	 case defConstSym:
        {
				const [sym, code] = operands;
				const valueCode = lispCompile(code);
				eval(`${sym.jsname} = ${valueCode}`)
				return `${sym.jsname}`
        }
    case defMacroSym:
        {
				const [sym, code] = operands;
				const macroCode = lispCompile(code)
				
				macroValue = eval(macroCode);
				
				macroLookup.set(sym, macroValue)
				
				return "1"
        }
    case setSym:
        const [variable, value] = operands;
		  let result = lispCompile(value)
		  let leftHand = variable.jsname
		  if(Array.isArray(variable)){
				leftHand = lispCompile(variable)
				if(isScope(leftHand)){
					 throw "left hand of set cannot be a scope"
				}
		  }
		  
		  if(isScope(result)){
				result = result.replaceAll(value_marker, value_marker + leftHand + "=")
				
		  }else{
            result = leftHand + "=" + result;
		  }
		  return result;
    case letSym: {
		  const [variables, ...body] = operands
		  return lispCompileLet(variables, body)
	 }
    case ifSym:
        {
				const [condition, thenClause, elseClause] = operands;

				let conditionCode = lispCompile(condition);
				let thenCode = lispCompile(thenClause);
				let elseCode = elseClause == null ? "null" : lispCompile(elseClause);
				
				if(thenCode === null || thenCode === ""){
					 thenCode = "null"
				}
				if(elseCode === null || elseCode === ""){
					 elseCode = "null"
				}
				if(!isScope(conditionCode) && !isScope(thenCode) && !isScope(elseCode)){
					 
					 return `(${conditionCode} ? ${thenCode} : ${elseCode})`
				}
			   let js = ""
				let isConditionScope = false;
				if(isScope(conditionCode)){
					 js = "{var check; " + conditionCode.replaceAll(value_marker, "check = ");
					 conditionCode = "check"
					 isConditionScope = true
				}
				if(!isScope(elseCode)){
					 elseCode = value_marker + " " + elseCode;
				}
				if(!isScope(thenCode)){
					 thenCode = value_marker + " " + thenCode;
				}
				js = js + `if(${conditionCode}){${thenCode}}else{${elseCode}${isConditionScope ? "}" : ""}} `

				
				return js;
        }
	 case handleErrorsSym:
		  {
				
				const [body, handler] = operands;

				const [varSym, handlerBody] = handler;
				const bodyCode = lispCompile(body);
				const handlerBodyCode = lispCompile(handlerBody);
				return `try{${isScope(bodyCode) ? bodyCode : value_marker + " " + bodyCode}}catch(${varSym.jsname}){ if(${varSym.jsname}.type === "return-from") raise(${varSym.jsname}); ${isScope(handlerBodyCode) ? handlerBodyCode : value_marker +" " + handlerBodyCode}}`
				
		  }
        // Add more cases for other operators as needed
		  
    default:
		  if(operator == undefined){
				throw new Error("undefined operator in ", code)
		  }
        if (macroLookup.has(operator)) {
				let macroFcn = macroLookup.get(operator)
				let newcode = null;
				newcode = macroFcn(...operands)
				
				return lispCompile(newcode)
        }
        args = operands.map(op => lispCompile(op))
        if(args.some(isScope)){
				let callargs = args.map((x,i) => "arg"+i)
				let argCall = args.map((x,i) => {
					 if(isScope(x)){
						  return "var arg" + i + ";" + x.replaceAll(value_marker, "arg" + i + "=") + ";";
						  
					 }else{
						  return "let arg" + i + "=" + x + ";";
					 }
				});
				return `{${argCall.join("")} ${value_marker} ${operator.jsname}(${callargs.join(",")})}`
		  }
        return `${operator.jsname}(${args})`;
    }
	 }finally{
		  codeStack.pop()
	 }
}

lisp_reader = function(code) {
	return code;
}

function lispCompileAst(ast){
    js = "'use strict'\n; return "+ lispCompile(lisp_reader(ast))
    //console.log("ast: ", ast)
    //console.log("js: ", js)
    return Function(js)
}

function lispCompileString(code) {
    const [ast, next] = parser.ParseLisp(code)
    if (next == null){
        throw "unable to parse code"
    }
    return lispCompileAst(ast)
}

function evalLisp(code){
	 let fn = lispCompileAst(code)
	 return fn();
}

eval2 = evalLisp
loadFileAsync = null
loadcontext = ""
async function LispEvalBlock(code, file) {
	
	 for(;;){
		  
		  const [ast, next] = parser.ParseLisp(code)
		  //console.log(">>>>", code.slice(0, code.length- next.length) )
		  if (ast == parser.UnexpectedEOF){
				alert("!")
				throw new Error("Unexpected EOF" + file == nil ? "" : " in file " + file)
		  }
		  if (next == null){
				return;
		  }
		  code = next;
		  var js;
		  
		  try{
				js = lispCompile(lisp_reader(ast));
				if(isScope(js)){
					 js = js.replaceAll(value_marker, "returnValue =");
					 js = "{'use strict'; var returnValue;" + js + "return returnValue;}";
				}else{
					 js = "{'use strict'; return " + js + "}";
				}
		  }catch(e){
				console.log(e);
				console.trace(e)
				return;//throw e
				}
		  finally{

		  }
		  
		  //WriteCodeToLog("()=> " + js +";")
		  println(["value code:", ast, "=>", js , "<< "])
		  
		  const result = eval?.("() => "+ js)()
		  console.log("result: ", result)
		  if(result != null && typeof(result) == "object" && result.type == "load"){

				const data = await loadFileAsync(result.value)
				const prevContext = loadcontext
				loadcontext = result.value
				LispEvalBlock(data + "\n" + next, result.value)
				loadcontext = prevContext
				return;
		  }
	 }
}

// do-eval can be used to eval as single line of code.
function do_eval(code){
	 code = "return " + code.trim()
	 const func = new Function(code)
	 return func()
}

js_eval = do_eval

lisp.lisp.eval = evalLisp
lisp.lisp.LispEvalBlock = LispEvalBlock

module.exports = {
	 EvalLisp: evalLisp,
	 LispEvalBlock: LispEvalBlock,
	 lispCompile: lispCompile
};
