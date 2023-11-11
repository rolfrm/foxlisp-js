const parser = require("./lispy_parser")
const lisp = require("./lisp")
console.log("loading lispy")
function sym(x, jsname){
    return lisp.sym(x, jsname)
}

quotes = []
quotes_lookup = new Map();
function _getQuote(id) {
    return quotes[id]
}
getQuote = _getQuote
function setQuote(newQuote){
	 if(quotes_lookup.has(newQuote)) {
		  return quotes_lookup.get(newQuote);x
	 }
    let id = quotes.length;
    quotes.length += 1
    quotes[id] = newQuote
	 quotes_lookup.set(newQuote, id)
    return id
}

internedStrings = []
internStringLookup = {}

getInternedString = (id) => internedStrings[id]

function internString(str) {
    if (str in internStringLookup) {
        return internStringLookup[str];
    } else {
        const id = internedStrings.length;
        internedStrings.push(str);
        internStringLookup[str] =  id;
        return id;
    }
}

car = (x) => x[0]
cdr = (x) => x.slice(1)

//add = (...x) => x.reduce((sum, num) => sum + num, 0);;
add = (x, y) => x + y
sub = (x, y) => x - y
div = (x, y) => x / y
mul = (x, y) => x * y
cadr = (x) => x[1]
caddr => (x) => x[2]
cddr = (x) => x.slice(2);
len = (x) => x.length
list = (...x) => x
makehashmap = () => new Map();

get_type = (x) => typeof x;
eq = (a, b) => a === b;
slice = (a, n) => a.slice(n);
is_string = (a) => typeof(a) == "string";
is_null = (a) => a == null;
is_list = (a) => Array.isArray(a)
raise =(err) =>{
	 throw err;
};

usplice = (x) => ({type: "unsplice", value: x})

load = (x) => ({type: "load", value: x})



ulist = (...x) => {
	 let out = []
	 for (let elem of x){
		  if(elem == undefined){
				throw new Error("content is undefined");
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
  
  if(Array.isArray(obj)){
    let strOut = ""
    let first = true
    strOut += "["
   for(let elem of obj){
    if(!first){
      strOut = strOut + ", ";
    }else{
      first = false;
    }
      strOut += println_impl(elem)

    }
    strOut += "]"
    return strOut;
  }else{
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
nth = (a, n) => a[n]
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
makesym = (a) => sym(a)


const loopSym = sym("loop");
const notSym = sym("not");
const subSym = sym("sub");
const addSym = sym("add");
const mulSym = sym("mul");
const divSym = sym("div");
const setSym = sym("set");
const letSym = sym("let");
const prognSym = sym("progn");
const ifSym = sym("if");
const lambdaSym = sym("lambda");
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

const defvarSym = sym("defvar");

function mathMacro(sym){
  function macro(...operands){
    let [first, ...rest] = operands;
     if(first == null){
        throw "not enough arguments for add"
     }
     for (let x of rest) {
      first = [sym, first, x]
     }
     return first;
  }
  return macro;
  
}


function subMacro(...operands){
  let [first, ...rest] = operands;
   if(first == null){
      throw "not enough arguments for add"
   }
   if(rest.length == 0) {
    return [subSym, 0, first];
   }

   for (let x of rest) {
    first = [subSym, first, x]
   }
   return first;
}

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

add2 = sym("+")
add2.macro = mathMacro(addSym);
mul2 = sym("*")
mul2.macro = mathMacro(mulSym);
div2 = sym("/")
div2.macro = mathMacro(divSym);
sub2 = sym("-")
sub2.macro = subMacro;


loop_sym = sym("loop")

function lispCompile(code, n) {
  if(n == undefined){
    n = 1;
  }else {
    n += 1;
  }
  if(n > 100){
    throw new "errrrr";
  }
  if( typeof(code)  == "number" ){
      return code
  }
  if( typeof(code)  == "string" ){
    
    id = internString(code)
    return `getInternedString(${id})`;
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
        
        const updateCode = update.map(updateExpr => 'tmp =(' + lispCompile(updateExpr, n) +')').join(';');

        return `(() => {let tmp = null; for (;${lispCompile(condition)};) { ${updateCode} };return tmp})()`;
      case orSym:
        {
          const combined = operands.map(op => lispCompile(op, n)).join("||")
          return `(${combined})`
        }
      case andSym:
        {
          const combined = operands.map(op => lispCompile(op, n)).join("&&")
          return `(${combined})`
        }
	 case jsSym:
		  {
				return operands[0];
		  }
		
      case blockSym:{
        const [sym, ...body] = operands;
        const bodyCode = body.map(updateExpr => 'tmp =(' + lispCompile(updateExpr, n) +')').join(';');
            
        return `(()=>{let tmp = null;const ${sym.jsname} = {};
         try{${bodyCode}}catch(ex){if(ex.id === ${sym.jsname}){return ex.value;}else{throw ex;}} return tmp;})()`
      }
      case returnFromSym:{
        const [sym, value] = operands;
        
        return `(()=> {throw {id:${sym.jsname}, value:${lispCompile(value)} }})()`;
      }
      case lambdaSym:
        {
            const [args, ...body] = operands;
				const restIndex = args.indexOf(restSym)
				console.log("idx:", restIndex)
            let argstr = args.map(arg => arg.jsname).join(",")
				if(restIndex != -1){
					 argstr = args.slice(0, restIndex).map(arg => arg.jsname).concat(["..." + args[restIndex + 1].jsname]).join(",");
				}
            if(body.length == 1) {
                return `((${argstr}) => ${lispCompile(body[0], n)})`;
            }

            const bodyCode = body.map(updateExpr => 'tmp =(' + lispCompile(updateExpr, n) +')').join(';');
            return `((${argstr}) => {let tmp = null; ${bodyCode}; return tmp;})`;
        }
	 case prognSym:
		  {
				const body = operands;
				if(body.length == 0){
					 return "null"
				}
				if(body.length == 1){
					 return `${lispCompile(body[0], n)}`
				}
				const bodyCode = body.map(updateExpr => 'tmp =(' + lispCompile(updateExpr, n) +')').join(';');
				return `(() => {let tmp = null; ${bodyCode}; return tmp;})()`;
		  }
      case notSym:
        {
            const [left] = operands;
            return `(!${lispCompile(left, n)})`;
        }
      case quoteSym:
        {
            const [quoted] = operands
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
        const valueCode = lispCompile(code, n);
        //println(["value code:", valueCode])
        eval(`${sym.jsname} = ${valueCode}`)
        return `${sym.jsname}`
        }
      case defMacroSym:
        {
				const [sym, code] = operands;
				const macroCode = lispCompile(code, n)
				console.log("macro code: ", macroCode)
				// todo: macros can only take one arg.
				macroValue = eval(macroCode);
				sym.macro = macroValue;
				
				return "1"
        }
      case setSym:
        const [variable, value] = operands;
        return `${lispCompile(variable)} = ${lispCompile(value, n)}`;
    case letSym:{
        const [variables, ...body] = operands;
        const  varCode = variables.map(updateExpr => {
            const [left, right] = updateExpr;
            code = `let ${left.jsname} = (${lispCompile(right, n)})`
            return code;
        }).join(';');
        
		  if(body.length == 1){
				return `(() => {${varCode};return ${lispCompile(body[0])};})()`
		  }

		  const bodyCode = body.map(updateExpr => 'tmp =(' + lispCompile(updateExpr, n) +')').join(';');
        return `(() => {let tmp = null;${varCode};${bodyCode}; return tmp})()`
	 }
      case ifSym:
        {
          const [condition, thenClause, elseClause] = operands;
          const conditionCode = lispCompile(condition);
          const thenCode = lispCompile(thenClause);
          const elseCode = elseClause == null ? "null" : lispCompile(elseClause);
          return `(${conditionCode} ? ${thenCode} : ${elseCode})`
        }
      // Add more cases for other operators as needed
  
    default:
		  if(operator == undefined){
				throw new Error("undefined operator in ", code)
		  }
        if (operator.macro != null) {
				//console.log(operands)
			 newcode = operator.macro(...operands)
          
          return lispCompile(newcode, n)
        }
        args = operands.map(op => lispCompile(op, n)).join(",")
        
        return `${operator.jsname}(${args})`;
    }
  }

function lispCompileFunc(code) {
    const [ast, next] = parser.ParseLisp(code)
    if (next == null){
      throw "unable to parse code"
    }
    js = "return "+ lispCompile(ast)
    console.log("ast: ", ast)
    console.log("js: ", js)
    return Function(js)
}

function evalLisp(code){
  fn = lispCompileFunc(code)
  return fn();
}

loadFileAsync = null

console.log("??");

function LispEvalBlock(code) {
  for(;;){
    
		const [ast, next] = parser.ParseLisp(code)
		//console.log(">> ", next, ast);
		if (next == null){
			 return;
		}
		code = next;
		js = "return "+ lispCompile(ast)
		println(["value code:", ast, "=>", js])
		
		let f = Function(js)
		const result = f();
		if(result != null && typeof(result) == "object" && result.type == "load"){
			 console.log("load!");
			 loadFileAsync(result.value, (data) => {
				  LispEvalBlock(data + "\n" + next)
			 });
			 
			 return;
		}
  }
}


lisp.lisp.eval = evalLisp
lisp.lisp.LispEvalBlock = LispEvalBlock


if(evalLisp("(+ 1 2)") != 3){
	 throw new Error("ho no!")
}

x = 10000000
y = 0

code3 = `
//function x1() {
    let x = 1000000000;
    for (; x > 0; ) {
      x = x - 1;
    }
//}
`;

//x1 = Function(`${code2}`);
//x2 = Function(`
//
//   return (() => {x = 3; r = 0; for (i = 0; i < 1000000000; i++) { r = x = x * 2 } return r})()
//`)
//tmp = (() => {r = null; for (;(x > 0);) { tmp =(x = (x - 1));tmp =(y = (y + 1)) };return tmp})()
//console.log(">", x1())
//eval(code4)
//console.log("code2: ", code2)
//x1()
//console.log("x: ", x, y)
//console.log(parser)


fcn1 = lispCompileFunc("(let ((x 0)) (set x (add x 1)) (loop (not (_op_gt x 100)) (set x (add x +1)) ))")
console.log("l: ", fcn1())
if(fcn1() != 101){
	 throw "unexpected value" + fcn1()
}

eval("vartest = 10;")

fcn1 = lispCompileFunc("(defvar callx (lambda (x) (add vartest x)))")
console.log("l: ", fcn1())
console.log("x? ", callx(5))

fcn1 = lispCompileFunc("(quote (1 2 3 4 a b c (d e f g))))")
console.log("q: ", fcn1())

fcn1 = lispCompileFunc("(len (cddr (quote (1 2 3 4 a b c (d e f g))))))")
console.log("q2: ", fcn1())

fcn1 = lispCompileFunc("(get_type (add (list 1 2 3) (list 4 5 6)))")
console.log("q2: ", fcn1())

fcn1 = lispCompileFunc("(eq 1 2)")
console.log("eq: ", fcn1())


fcn1 = lispCompileFunc("(let ((x 0)) (set x (add x 1)) (loop (not (_op_gt x 10000000)) (set x (+ x +1)) ))")
console.log("l: ", fcn1())

fcn1 = lispCompileFunc("(let ((x 0)) (mul 4 (add x 2)))")
console.log("l: ", fcn1())

fcn1 = lispCompileFunc("(list (car \"asd123\"))")
console.log("str: ", fcn1())

fcn1 = lispCompileFunc("(+ 1 2 3 4 5 6 7)")
console.log("manyadd: ", fcn1())
console.log("/ 24 2 3: ", evalLisp("(/ 24 2 3)"))
console.log("- 24 2 3: ", evalLisp("(- 24 2 3)"))
x = 24
console.log("- x: ", evalLisp("(- x)"))
console.log("quoted: - x: ", evalLisp("'(- x)"))
console.log("quoted: - x: ", evalLisp("'x"))

let test = evalLisp("()")
if(test != null){
	 throw new Error("expected null")
}

console.log("loaded lispy.js ooko")

module.exports = {
  EvalLisp: evalLisp,
  LispEvalBlock: LispEvalBlock,
  lispCompile: lispCompile
};
