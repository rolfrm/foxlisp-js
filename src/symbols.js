
lisp = { symbolName: {}, symbols: [] }
const reserved = {
	"true": true, "false": true, "null": true, "case": true,
	 "tmp": true, "undefined": true, "new": true, "eval": true, "typeof" : true, "delete": true
}

const sanitizedLookup = {}
const sanitizedLookupRev = {}

nameid = 0;
function sanitizeSymbolName(name) {
	if (name in sanitizedLookup) {
		return sanitizedLookup[name]
	}

	const sanitized = name.replace(/[^\.a-zA-Z0-9_]/g, '_')

	isreserved = reserved[name];

	if (sanitized == name && !isreserved)
		return sanitized;

	if (sanitized in sanitizedLookupRev || isreserved) {
		for (i = 0; true; i = i + 1) {
			const test = `_${sanitized}_${i}`
			if (test in sanitizedLookupRev) {
				continue;
			}
			sanitizedLookup[name] = test
			sanitizedLookupRev[test] = name
			return test;
		}
	}

	sanitizedLookup[name] = sanitized
	sanitizedLookupRev[sanitized] = name
	return sanitized;

}

function getsym(str) {
	return lisp.symbolName[str]
}


function sym(str, jsname) {
	if (!lisp.symbolName[str]) {
		if (jsname == null) {
			jsname = str;
			jsname = sanitizeSymbolName(jsname)
		}

		const symbol = {
			type: "symbol",
			value: str,
			jsname: jsname
		};
		Object.freeze(symbol)

		lisp.symbolName[str] = symbol;
		lisp.symbols.push(symbol);
	}
	return lisp.symbolName[str];
}
quote_sym = sym("quote");
quasiquote_sym = sym("quasiquote")
quasiunquote_sym = sym("quasiunquote")
quasiunquotesplice_sym = sym("quasiunquote-splice")

module.exports = {
	sym: sym,
	getsym: getsym,
	lisp: lisp,
	quote_sym: quote_sym,
	quasiquote_sym: quasiquote_sym,
	quasiunquote_sym: quasiunquote_sym,
	quasiunquotesplice_sym: quasiunquotesplice_sym
};
