const lisp = require("./symbols")

codeBase = null

function setCodeBase(code){
	 codeBase = code
}

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
  const UnexpectedEOF = new ParserCondition();
  
  // Helper function to skip whitespace
  function skipWhitespace(input) {
    while (input.length > 0 && /\s|\n|\t/.test(input[0])) {
      input = input.slice(1);
    }
    return input
  }

  function skipComment(input) {
    if(input.length == 0 || input[0] != ';'){
      return input;
    }
    while (input.length > 0 && input[0] !== '\n') {
            
      input = input.slice(1);
    }
    if (input.length > 0) {
      input = input.slice(1);
    }
    return input;
  }

  function skipCommentAndWhitespace(input){
    while(true){
      const input1 = input;
      input = skipComment(skipWhitespace(input))
      if(input == input1){
        return input;
      }
    }
  }

  
  // Helper function to parse a number
function parseNumber(input) {
	 let value = "";
	 let isFloat = false;
	 let isNegative = false;
	 let pattern = /\d/
	 let base = 10;
	 if(input[0] == '#' && input[1] == 'x'){
		  pattern = /[0-9a-fA-F]/;
		  base = 16
		  input = input.slice(2)
	 }else if(input[0] == '#' && input[1] == 'o'){
		  pattern = /[0-7]/
		  base = 8
		  input = input.slice(2)
	 }else if(input[0] == '#' && input[1] == 'b'){
		  pattern = /[0-1]/;
		  base = 2
		  input = input.slice(2)
	 }
		
    if(input[0] == '-'){
      isNegative = true;
      input = input.slice(1);
    }
    if(input[0] == '+'){
      input = input.slice(1)
    }
  
    while (input.length > 0 && (pattern.test(input[0]) || (!isFloat && input[0] === '.'))) {
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
        if (/\s|;|\)|\(/.test(nextchr)) {
  
      } else {
        return [null, null];
      }
    }
  
    if (isFloat) {
      const num = parseFloat(value);
      if (isNaN(num)) {
        return [null, null];
      }
      if(isNegative){
        return [-num, input];  
      }
      return [num, input];
    }
  
    const num = parseInt(value, base);
    if (isNaN(num)) {
      return [null, null];
    }
    if(isNegative){
      return [-num, input];  
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

  // ParseLisp0 function
  function ParseLisp0(input) {
    
    // Parsing loop
    while (true) {
      input = skipCommentAndWhitespace(input);
      if (input.length === 0) {
        
        return [NothingParsed, null];
      }
      
      switch (input[0]) {
        
        case '(':
          let offset = codeBase.length - input.lengt
			 let input0 = input
			 input = skipCommentAndWhitespace(input.slice(1));
          
          if(input[0] == ')'){  
            return [[], input.slice(1)];
          }
          let out = []
			 
          for(;;){
            const [result,next] = ParseLisp0(input)
            if(next){
              input = next;
              out.push(result)
              input = skipCommentAndWhitespace(input);
					 if(input[0] == ')'){
						  out.codeBase = codeBase
						  out.offset = offset
						  
						  Object.freeze(out)
						  return [out, input.slice(1)];
						  
					 }
            }else{
              return [UnexpectedEOF, null]
            }
          }
          break;
        case ':':
          {
            // Parse keyword
            const [r, next] = ParseLisp0(input.slice(1));
            return [[lisp.keyword_sym, r], next]
          }

        break;
        case '\'':
          {
            // Parse quote

            const [r, next] = ParseLisp0(input.slice(1));
            return [[lisp.quote_sym, r], next]
          }
		case '`':
			 {
				  const [r, next] = ParseLisp0(input.slice(1));
              return [[lisp.quasiquote_sym, r], next]

			 }
		case ',':
			 {
				  if(input[1] == '@'){
						const [r, next] = ParseLisp0(input.slice(2));
						return [[lisp.quasiunquotesplice_sym, r], next]
				  }
				  const [r, next] = ParseLisp0(input.slice(1));
              return [[lisp.quasiunquote_sym, r], next]

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



function ParseLisp (codestring){
    return ParseLisp0(codestring);
}
  
module.exports = {
    ParseLisp: ParseLisp,
    UnexpectedEOF: UnexpectedEOF,
	 setCodeBase: setCodeBase
};
