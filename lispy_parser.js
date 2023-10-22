const lisp = require("./lisp")

class ParserCondition {
    constructor(err) {
      this.err = err;
    }
  
    toString() {
      return "Error parsing lisp code";
    }
  }
  
  class NothingParsed2 {}
  
  const NothingParsed = new ParserCondition();
  
  // Helper function to skip whitespace
  function skipWhitespace(input) {
    while (input.length > 0 && /\s|\n|\t/.test(input[0])) {
      input = input.slice(1);
    }
    return input
  }
  
  // Helper function to parse a number
  function parseNumber(input) {
    let value = "";
    let isFloat = false;
  
    while (input.length > 0 && (/\d/.test(input[0]) || input[0] === '-' || (!isFloat && input[0] === '.'))) {
      value += input[0];
      if (input[0] === '.') {
        isFloat = true;
      }
      input = input.slice(1);
    }
  
    if (value === "") {
      return [null, null];
    }
  
    if (input.length > 0) {
      const nextchr = input[0];
      if (/\s|\)|\(/.test(nextchr)) {
  
      } else {
        return [null, null];
      }
    }
  
    if (isFloat) {
      const num = parseFloat(value);
      if (isNaN(num)) {
        return [null, null];
      }
      return [num, input];
    }
  
    const num = parseInt(value, 10);
    if (isNaN(num)) {
      return [null, null];
    }
    return [num, input];
  }
  
  function parseString(input) {
    if (input.length === 0 || input[0] !== '"') {
      return [null, null];
    }
  
    input = input.slice(1); // Consume opening quote
    let value = "";
    let escaped = false;
  
    while (input.length > 0) {
      const char = input[0];
      input = input.slice(1);
  
      if (char === '\\' && !escaped) {
        escaped = true;
        continue;
      }
  
      if (char === '"' && !escaped) {
        // Closing quote found
        return [value, input];
      }
  
      value += char;
      escaped = false;
    }
  
    return [null, null]; // Unclosed string
  }
  
  // ParseLisp function
  function ParseLisp(input) {
    
    // Parsing loop
    while (true) {
      input = skipWhitespace(input);
      if (input.length === 0) {
        
        return [NothingParsed, null];
      }
  
      switch (input[0]) {
        case ';':
          while (input.length > 0 && input[0] !== '\n') {
            input = input.slice(1);
          }
          if (input.length > 0) {
            input = input.slice(1);
          }
          continue;
        case '(':
          input = skipWhitespace(input.slice(1));
          let out = []
          for(;;){
            const [result,next] = ParseLisp(input)
            if(next){
              input = next;
              out.push(result)
              input = skipWhitespace(input);
              if(input[0] == ')'){
                return [out, input.slice(1)];
              }
            }else{
              return [null, null]
            }
          }
          break;
        case '\'':
          {
            // Parse quote
            
            const [r, next] = ParseLisp(input.slice(1));
            return [[lisp.quote_sym, r], next]
          }
          case '"':
              return parseString(input);

  
        default:
          
          const [num, next] = parseNumber(input);
  
          if (next) {
            return [num, next];
          }
          
          // parse symbol
          value = ""
          while (input.length > 0 && !/\s|\n|\t|\(|\)/.test(input[0])) {
            value += input[0];
            input = input.slice(1);
          }
          if (value === ""){
            return [null, null]
          }
           
          s = lisp.sym(value);
          return [s, input]          
      }
    }
  }
  
  module.exports = {
    ParseLisp: ParseLisp
  };