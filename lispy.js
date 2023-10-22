const parser = require("./lispy_parser")
const lisp = require("./lisp")

function sym(x, jsname){
    return lisp.sym(x, jsname)
}

quotes = []
function _getQuote(id) {
    return quotes[id]
}
getQuote = _getQuote
function setQuote(newQuote){
    id = quotes.length;
    quotes.length += 1
    quotes[id] = newQuote
    return id
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
get_type = (x) => typeof x;
eq = (a, b) => a === b;
slice = (a, n) => a.slice(n);
is_string = (a) => typeof(a) == "string";
is_null = (a) => a == null;



const loopSym = sym("loop");
const gtSym = sym("gt");
const notSym = sym("not");
const subSym = sym("sub");
const addSym = sym("add");
const mulSym = sym("mul");
const divSym = sym("div");
const xSym = sym("x");
const setSym = sym("set");
const letSym = sym("let");
const lambdaSym = sym("lambda");
const quoteSym = lisp.quote_sym;

const defvar = sym("defvar");
const ySym = sym("y")

function mathMacro(sym){
  function macro(operands){
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


function subMacro(operands){
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


add2 = sym("+")
add2.macro = mathMacro(addSym);
mul2 = sym("*")
mul2.macro = mathMacro(mulSym);
div2 = sym("/")
div2.macro = mathMacro(divSym);
sub2 = sym("-")
sub2.macro = subMacro;


loop_sym = sym("loop")
code = [loopSym, [gtSym, xSym, 0], [setSym, xSym, [subSym, xSym, 1]], [setSym, ySym, [addSym, ySym, 1]]]

function lispCompile(code) {
    //console.log("code:", code, typeof(code))
    if( typeof(code)  == "number" ){
      return code
  }
  if( typeof(code)  == "string" ){
    return `\"${code}\"`
  }
    if (code.type == "symbol"){
        return code.jsname
    }

    const [operator, ...operands] = code;
    //console.log("op: ", operator)
    switch (operator) {
      case loopSym:
        const [condition, ...update] = operands;
        
        const updateCode = update.map(updateExpr => 'tmp =(' + lispCompile(updateExpr) +')').join(';');

        return `(() => {let tmp = null; for (;${lispCompile(condition)};) { ${updateCode} };return tmp})()`;
      case gtSym:
        const [left, right] = operands;
        return `(${lispCompile(left)} > ${lispCompile(right)})`;
      case lambdaSym:
        {
            const [args, ...body] = operands;
            const argstr = args.map(arg => arg.value).join(",")
            if(body.length == 1) {
                return `((${argstr}) => ${lispCompile(body[0])})`;
            }
            const bodyCode = body.map(updateExpr => 'tmp =(' + lispCompile(updateExpr) +')').join(';');
            return `(${argstr}) => {let tmp = null; ${lispCompile(right)}; return tmp;})`;

        }
      case notSym:
        {
            const [left] = operands;
            return `(!${lispCompile(left)})`;
        }
      case quoteSym:
        {
            const [quoted] = operands;
            id = setQuote(quoted)
            return `(getQuote(${id}))`;
        }
      case defvar:
        {
        const [sym, code] = operands;
        eval(`${sym.value} = ${lispCompile(code)}`)
        return `${sym.value}`
        }
      case setSym:
        const [variable, value] = operands;
        return `${lispCompile(variable)} = ${lispCompile(value)}`;
      case letSym:
        const [variables, ...body] = operands;
        varCode = variables.map(updateExpr => {
            const [left, right] = updateExpr;
            code = `${left.value} = (${lispCompile(right)})`
            return code;
        }).join(';');
        
        const bodyCode = body.map(updateExpr => 'tmp =(' + lispCompile(updateExpr) +')').join(';');
        return `(() => {let tmp = null;${varCode};${bodyCode}; return tmp})()`
      
      // Add more cases for other operators as needed
  
      default:
        if (operator.macro != null) {
          console.log("operator: ", operator)
          newcode = operator.macro(operands)
          console.log(">>>>>>>>>>newcode: ", newcode)
          return lispCompile(newcode)
        }  
      args = operands.map(op => lispCompile(op)).join(",")
        
        return `${operator.jsname}(${args})`;
    }
  }

code2 = lispCompile(code)
function lispCompileFunc(code) {
    ast = parser.ParseLisp(code)
    js = "return "+ lispCompile(ast)
    console.log("ast: ", ast)
    console.log("js: ", js)
    return Function(js)
}

function evalLisp(code){
  fn = lispCompileFunc(code)
  return fn();

}
lisp.lisp.eval = evalLisp
module.exports = {
  EvalLisp: evalLisp
};

if(evalLisp("(+ 1 2)") != 3){
  throw "ho no!"
}

console.log(code2)
x = 10000000
y = 0
//eval(code2)

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
fcn1 = lispCompileFunc("(let ((x 0)) (set x (add x 1)) (loop (not (gt x 100)) (set x (add x +1)) ))")
console.log("l: ", fcn1())

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


fcn1 = lispCompileFunc("(let ((x 0)) (set x (add x 1)) (loop (not (gt x 10000000)) (set x (+ x +1)) ))")
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
//fcn1 = lispCompileFunc("(list '(1 2 3)) ")
//console.log("quoted: ", fcn1())


