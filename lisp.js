
//const util = require('util');

lisp = {symbolName: {}, symbols: []}

function sym(str) {
    if (!lisp.symbolName[str]) {
        const symbol = { type: "symbol", value: str, 
           util : {inspect: function () {
            return this.value; 
          }}, };
        //symbol[util.inspect.custom] = function(depth, options) {
        //    return '\''+ symbol.value;
        //  };
          
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
