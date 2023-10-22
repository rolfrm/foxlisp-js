
const util = require('util');

lisp = {symbolName: {}, symbols: []}

function sym(str, jsname) {
    if (!lisp.symbolName[str]) {
        if(jsname == null){
          jsname = str;
        }
        const symbol = { type: "symbol", value: str, 
          jsname: jsname,
          macro: null,
           util : {inspect: function () {
            return this.value; 
          }}, };
        symbol[util.inspect.custom] = function(depth, options) {
            return '\''+ symbol.value;
          };
          
        lisp.symbolName[str] = symbol;
        lisp.symbols.push(symbol);
      }
      return lisp.symbolName[str];
  }
  quote_sym = sym("quote");

  module.exports = {
    sym: sym,
    lisp: lisp,
    quote_sym : quote_sym
  };
