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
    while (input.length > 0 && /\s/.test(input[0])) {
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
    let stack = [];
  
    // Parsing loop
    while (true) {
      input = skipWhitespace(input);
      if (input.length === 0) {
        if (stack !== null) {
          const cond = new ParserCondition({ err: "Incomplete lisp code parsed." });
          return cond;
        }
        return NothingParsed;
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
          // Opening parenthesis: push a new array to the stack
          stack.push([]);
          break;
        case ')':
          // Closing parenthesis: pop from stack and append to the parent array
          const popped = stack.pop();
          if (stack.length > 0) {
            // Append the popped array to the parent array
            stack[stack.length - 1].push(popped);
          } else {
            input = input.slice(1);
            // The stack is empty; this is the root array
            return popped;
          }
          break;
        default:
          
          const [num, next] = parseNumber(input);
  
          if (next) {
            input = next
            if (stack === null) {
              return num;
            }
            
            stack[stack.length - 1].push(num);
            continue;
          }
  

          const [str, next2] = parseString(input);
          if (next2) {
            input = next
            if (stack === null) {
              return str;
            }
            stack[stack.length - 1].push(str);
            continue;
          }
  
          // Parse symbol
          let value = "";
          const doquote = input[0] === '\'';
          if(doquote){
            input = input.slice(1);
          }
  
          while (input.length > 0 && !/\s|\n|\t|\(|\)/.test(input[0])) {
            value += input[0];
            input = input.slice(1);
          }
  
          if (value !== "") {
            let s = lisp.sym(value);
            if (doquote) {
              s = [lisp.quote_sym, s];
            }
            // Append the value to the current array on the stack
            if (stack === null) {
              return s;
            }
            stack[stack.length - 1].push(s);
          }
          continue;
      }
  
      input = input.slice(1);
    }
  }
  
  module.exports = {
    ParseLisp: ParseLisp
  };