
const util = require('util');

lisp = {symbolName: {}, symbols: []}
const reserved = {"true" : true, "false":true, "null": true, "case": true}


nameid = 0;
function sanitizeSymbolName(name) {
  const sanitized = name.replace(/[^a-zA-Z0-9_]/g, '_');
  nameid += 1;
  isreserved = reserved[name];
  
  if(sanitized == name && !isreserved)
    return sanitized;
  
  
  return `_${sanitized}_${nameid}`;
}

function sym(str, jsname) {
    if (!lisp.symbolName[str]) {
        if(jsname == null){
          jsname = str;
          jsname = sanitizeSymbolName(jsname)
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
