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

getsym = lisp.getsym
car = (x) => x && x[0]
cdr = (x) => x && x.slice(1)
op_add = (x, y) => x + y
op_sub = (x, y) => x - y
op_div = (x, y) => x / y
op_mul = (x, y) => x * y
op_leftshift = (x, y) => x << y;
op_rightshift = (x, y) => x >> y;
op_xor = (x, y) => x ^ y;
mod = (x, y) => x % y
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
concat = (a,b)=> a.concat(b)
__makesym = (a) => sym(a)


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
const blockSym = sym("block");
const returnFromSym = sym("return-from")
const jsSym = sym("%js")
const quoteSym = lisp.quote_sym;
const quasiQuoteSym = lisp.quasiquote_sym;
const quasiUnQuoteSym = lisp.quasiunquote_sym;
const quasiUnQuoteSpliceSym = lisp.quasiunquotesplice_sym;
const restSym = sym("&rest")
const typeOfSym = sym("type-of")
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

function unquoteToJs(code){
	 if(code == null){
		  return "null"
	 }
	 if(Array.isArray(code)){
		  if(code[0] == quasiQuoteSym){
				
				return quotedJs(code) 
		  }
		  if(code[0] == quasiUnQuoteSpliceSym){
				
				return `usplice(${lispCompile(code[1])})`;
		  }
		  if(code[0] == quasiUnQuoteSym){
				return lispCompile(code[1])
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


function lispCompileLet(variables, body){
	 const  varCode = variables.map(updateExpr => {
		  if(updateExpr.length != 2){
				throw new Error("The expression (xyz) is malformed." + updateExpr.toString());
		  }
        const [left, right] = updateExpr;
        code = `let ${left.jsname} = (${lispCompile(right)})`
        return code;
    }).join(';');
    
	 if(body.length == 1){
		  if(variables.length == 0){
				return `${lispCompile(body[0])}`
		  }
		  return `(() => {${varCode};return ${lispCompile(body[0])};})()`
	 }

	 let bodyCode = ""
	 if(variables.length > 0){
	 	  bodyCode += "(() => {"+ varCode + ";return ";
	 }else{
		  bodyCode += "(";
	 }
	 
	 for(let i in body){
		  if(i > 0){
				bodyCode = bodyCode + ","
		  }
		  bodyCode = bodyCode + `${lispCompile(body[i])}`;
		  
	 }
	 if(body.length == 0){
		  bodyCode += "null";
	 }
	 if(variables.length > 0){
		  bodyCode = bodyCode + "})()";
	 }else{
		  bodyCode = bodyCode + ")";
	 }
	 
	 return bodyCode;
}
lmbmark = (id, f) => {
	f.assoc_id = id;
	return f;
}

let associd = 0;
let assoc = {}
                ///\/\*lmb#(\d+)\*\//
const markRegex = /\/\*lmb#(\d+)\*\/\(/g;
function lispCompile(code) {
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
        
		  const updateCode = lispCompileLet([], update);
		  const conditionCode = lispCompile(condition);
        return `(() => {let tmp = null; for (;${conditionCode};) { tmp = ${updateCode} };return tmp})()`;
    case orSym:
        {
				const combined = operands.map(op => lispCompile(op)).join("||")
				return `(${combined})`
        }
    case andSym:
        {
				const combined = operands.map(op => lispCompile(op)).join("&&")
				return `(${combined})`
        }
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
	 case typeOfSym:
		  {
				return `typeof ${lispCompile(operands[0])}`;
		  }
		  
    case blockSym:{
        const [sym, ...body] = operands;
        const bodyCode = body.map(updateExpr => 'tmp =(' + lispCompile(updateExpr) +')').join(';');
        
        return `(()=>{let tmp = null;const ${sym.jsname} = {};
         try{${bodyCode}}catch(ex){if(ex.id === ${sym.jsname}){return ex.value;}else{throw ex;}} return tmp;})()`
    }
    case returnFromSym:{
        const [sym, value] = operands;
        
        return `(()=> {throw {id:${sym.jsname}, value:${lispCompile(value)}, type: "return-from" }})()`;
    }
    case lambdaSym:
        {
            const [args, ...body] = operands;
				const restIndex = args.indexOf(restSym)
				let argstr = args.map(arg => arg.jsname).join(",")
				if(restIndex != -1){
					 argstr = args.slice(0, restIndex).map(arg => arg.jsname).concat(["..." + args[restIndex + 1].jsname]).join(",");
				}
			
			assoc[associd] = operands
            if(body.length == 1) {
                let lmb = `/*lmb#${associd++}*/((${argstr}) => ${lispCompile(body[0])})`;
				return lmb
            }

            const bodyCode = body.map(updateExpr => 'tmp =(' + lispCompile(updateExpr) +')').join(';');
            let lmb = `/*lmb#${associd++}*/((${argstr}) => {let tmp = null; ${bodyCode}; return tmp;})`;
				
				return lmb
		  }
	 case prognSym:
		  return lispCompileLet([], operands)
    case notSym:
        {
            const [left] = operands;
            return `(!${lispCompile(left)})`;
        }
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
				const code2 = `${sym.jsname} = ${valueCode}`;
				//WriteCodeToLog(code2 + ";")
				let result = eval(code2);
				console.log("result: ", valueCode)
				if(typeof(result) == "function" && result.assoc_id){
					result.lispname = sym
					result.lispargs = assoc[result.assoc_id][0]
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
        return `${lispCompile(variable)} = ${lispCompile(value)}`;
    case letSym: {
		  const [variables, ...body] = operands
		  return lispCompileLet(variables, body)
	 }
    case ifSym:
        {
				const [condition, thenClause, elseClause] = operands;
				const conditionCode = lispCompile(condition);
				const thenCode = lispCompile(thenClause);
				const elseCode = elseClause == null ? "null" : lispCompile(elseClause);
				return `(${conditionCode} ? ${thenCode} : ${elseCode})`
        }
	 case handleErrorsSym:
		  {
				
				const [body, handler] = operands;

				const [varSym, handlerBody] = handler;
				const bodyCode = lispCompile(body);
				const handlerBodyCode = lispCompile(handlerBody);
				return `(()=>{try{return ${bodyCode}}catch(${varSym.jsname}){ if(${varSym.jsname}.type === "return-from") throw ${varSym.jsname}; return ${handlerBodyCode}}})()`
				
		  }
        // Add more cases for other operators as needed
		  
    default:
		  if(operator == undefined){
				throw new Error("undefined operator in ", code)
		  }
        if (macroLookup.has(operator)) {
				newcode = (macroLookup.get(operator))(...operands)
				return lispCompile(newcode)
        }
        args = operands.map(op => lispCompile(op)).join(",")
        
        return `${operator.jsname}(${args})`;
    }
}

function lispCompileAst(ast){
    js = "'use strict'; return "+ lispCompile(ast)
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
function LispEvalBlock(code) {
	 for(;;){
		
		  const [ast, next] = parser.ParseLisp(code)
		  //console.log(">>>>", code.slice(0, code.length- next.length) )
		  if (next == null){
				return;
		  }
		  code = next;
		  js = "'use strict'; return "+ lispCompile(ast)
		  //WriteCodeToLog("()=> " + js +";")
		  println(["value code:", ast, "=>", js])
		  
		  let f = Function(js)
		  const result = f();
		  if(result != null && typeof(result) == "object" && result.type == "load"){

				loadFileAsync(result.value, (data) => {
					 const prevContext = loadcontext
					 loadcontext = result.value
					 LispEvalBlock(data + "\n" + next)
					 loadcontext = prevContext
				});
				
				return;
		  }
	 }
}


lisp.lisp.eval = evalLisp
lisp.lisp.LispEvalBlock = LispEvalBlock

module.exports = {
	 EvalLisp: evalLisp,
	 LispEvalBlock: LispEvalBlock,
	 lispCompile: lispCompile
};
