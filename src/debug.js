/**
 * FoxLisp Debug Module
 * Provides debugging support for Lisp code compiled to JavaScript
 */

// Debug configuration
const debugConfig = {
    enabled: false,
    traceEnabled: false,
    breakpointsEnabled: true,
    maxStackDepth: 100,
    logLevel: 'warn' // 'debug', 'info', 'warn', 'error'
};

// Source map storage: maps JS locations to Lisp source locations
const sourceMap = new Map();

// Breakpoints: Set of "file:line" strings
const breakpoints = new Set();

// Call stack for Lisp functions
const lispCallStack = [];

// Function name registry for better stack traces
const functionNames = new Map();

// Source location info attached to expressions
class SourceLocation {
    constructor(file, line, column, offset, endOffset) {
        this.file = file || '<unknown>';
        this.line = line || 0;
        this.column = column || 0;
        this.offset = offset || 0;
        this.endOffset = endOffset || 0;
    }

    toString() {
        return `${this.file}:${this.line}:${this.column}`;
    }
}

// Stack frame for debugging
class StackFrame {
    constructor(name, args, location, locals) {
        this.name = name;
        this.args = args || [];
        this.location = location;
        this.locals = locals || {};
        this.timestamp = Date.now();
    }

    toString() {
        const loc = this.location ? ` at ${this.location}` : '';
        const argsStr = this.args.map(a => formatValue(a)).join(', ');
        return `${this.name}(${argsStr})${loc}`;
    }
}

// Format a value for display
function formatValue(val, depth = 0) {
    if (depth > 3) return '...';

    if (val === null) return 'nil';
    if (val === undefined) return 'undefined';

    if (Array.isArray(val)) {
        if (val.length === 0) return '()';
        if (val.length > 10) {
            const first = val.slice(0, 10).map(v => formatValue(v, depth + 1)).join(' ');
            return `(${first} ...)`;
        }
        return `(${val.map(v => formatValue(v, depth + 1)).join(' ')})`;
    }

    if (typeof val === 'object') {
        if (val.type === 'symbol') return val.value;
        if (val.type === 'lisp-object') {
            const keys = Object.keys(val).filter(k => k !== 'type').slice(0, 5);
            const entries = keys.map(k => `${k}: ${formatValue(val[k], depth + 1)}`);
            return `{${entries.join(', ')}${keys.length < Object.keys(val).length - 1 ? ' ...' : ''}}`;
        }
        return JSON.stringify(val);
    }

    if (typeof val === 'string') return `"${val}"`;
    if (typeof val === 'function') {
        const name = val.lispname ? val.lispname.value : val.name || '<lambda>';
        return `#<function ${name}>`;
    }

    return String(val);
}

// Enable/disable debug mode
function setDebugMode(enabled) {
    debugConfig.enabled = enabled;
    if (enabled) {
        console.log('[FoxLisp Debug] Debug mode enabled');
    }
}

// Enable/disable function tracing
function setTraceMode(enabled) {
    debugConfig.traceEnabled = enabled;
    if (enabled) {
        console.log('[FoxLisp Debug] Trace mode enabled');
    }
}

// Add a breakpoint
function addBreakpoint(file, line) {
    const key = `${file}:${line}`;
    breakpoints.add(key);
    console.log(`[FoxLisp Debug] Breakpoint added: ${key}`);
    return key;
}

// Remove a breakpoint
function removeBreakpoint(file, line) {
    const key = `${file}:${line}`;
    breakpoints.delete(key);
    console.log(`[FoxLisp Debug] Breakpoint removed: ${key}`);
}

// Clear all breakpoints
function clearBreakpoints() {
    breakpoints.clear();
    console.log('[FoxLisp Debug] All breakpoints cleared');
}

// List all breakpoints
function listBreakpoints() {
    return Array.from(breakpoints);
}

// Check if we should break at this location
function shouldBreak(file, line) {
    if (!debugConfig.breakpointsEnabled) return false;
    return breakpoints.has(`${file}:${line}`);
}

// Debug hook called at breakpoint locations
function debugBreak(file, line, column, locals) {
    if (shouldBreak(file, line)) {
        console.log(`[FoxLisp Debug] Breakpoint hit: ${file}:${line}:${column}`);
        console.log('[FoxLisp Debug] Local variables:', locals);
        console.log('[FoxLisp Debug] Call stack:');
        printCallStack();
        // Trigger debugger
        debugger;
    }
}

// Function entry hook for tracing
function traceEnter(name, args, file, line) {
    const location = new SourceLocation(file, line, 0, 0, 0);
    const frame = new StackFrame(name, args, location);

    if (lispCallStack.length >= debugConfig.maxStackDepth) {
        throw new Error(`Stack overflow: max depth ${debugConfig.maxStackDepth} exceeded at ${name}`);
    }

    lispCallStack.push(frame);

    if (debugConfig.traceEnabled) {
        const indent = '  '.repeat(Math.min(lispCallStack.length - 1, 20));
        const argsStr = args.map(a => formatValue(a)).join(', ');
        console.log(`${indent}> ${name}(${argsStr})`);
    }

    return frame;
}

// Function exit hook for tracing
function traceExit(name, result) {
    const frame = lispCallStack.pop();

    if (debugConfig.traceEnabled) {
        const indent = '  '.repeat(Math.min(lispCallStack.length, 20));
        console.log(`${indent}< ${name} => ${formatValue(result)}`);
    }

    return result;
}

// Get the current call stack
function getCallStack() {
    return lispCallStack.slice();
}

// Print the current call stack
function printCallStack() {
    if (lispCallStack.length === 0) {
        console.log('  (empty stack)');
        return;
    }

    for (let i = lispCallStack.length - 1; i >= 0; i--) {
        const frame = lispCallStack[i];
        console.log(`  ${lispCallStack.length - i}. ${frame.toString()}`);
    }
}

// Get a formatted stack trace string
function getStackTraceString() {
    if (lispCallStack.length === 0) {
        return '  (empty stack)';
    }

    const lines = [];
    for (let i = lispCallStack.length - 1; i >= 0; i--) {
        const frame = lispCallStack[i];
        lines.push(`  ${lispCallStack.length - i}. ${frame.toString()}`);
    }
    return lines.join('\n');
}

// Clear the call stack (use with caution)
function clearCallStack() {
    lispCallStack.length = 0;
}

// Register a source location mapping
function registerSourceLocation(jsLine, jsColumn, lispFile, lispLine, lispColumn) {
    const key = `${jsLine}:${jsColumn}`;
    sourceMap.set(key, new SourceLocation(lispFile, lispLine, lispColumn));
}

// Look up Lisp source location from JS location
function lookupSourceLocation(jsLine, jsColumn) {
    const key = `${jsLine}:${jsColumn}`;
    return sourceMap.get(key);
}

// Register a function name for better stack traces
function registerFunction(func, name, file, line) {
    functionNames.set(func, { name, file, line });
}

// Get function debug info
function getFunctionInfo(func) {
    return functionNames.get(func);
}

// Wrap a function with debug tracing
function wrapWithTrace(func, name, file, line) {
    if (!debugConfig.enabled) return func;

    const wrappedName = name || func.name || '<anonymous>';
    registerFunction(func, wrappedName, file, line);

    return function(...args) {
        traceEnter(wrappedName, args, file, line);
        try {
            const result = func.apply(this, args);
            return traceExit(wrappedName, result);
        } catch (e) {
            traceExit(wrappedName, `<error: ${e.message}>`);
            throw e;
        }
    };
}

// Enhanced error with Lisp context
class LispError extends Error {
    constructor(message, lispLocation, lispCallStack) {
        super(message);
        this.name = 'LispError';
        this.lispLocation = lispLocation;
        this.lispCallStack = lispCallStack ? lispCallStack.slice() : [];
    }

    toString() {
        let msg = `${this.name}: ${this.message}`;
        if (this.lispLocation) {
            msg += `\n  at ${this.lispLocation}`;
        }
        if (this.lispCallStack.length > 0) {
            msg += '\n\nLisp Call Stack:\n';
            for (let i = this.lispCallStack.length - 1; i >= 0; i--) {
                msg += `  ${this.lispCallStack.length - i}. ${this.lispCallStack[i].toString()}\n`;
            }
        }
        return msg;
    }
}

// Create a LispError with current context
function createLispError(message, file, line, column) {
    const location = new SourceLocation(file, line, column);
    return new LispError(message, location, lispCallStack);
}

// Enhance an existing error with Lisp context
function enhanceError(error, file, line, column) {
    if (!error._lispEnhanced) {
        error._lispEnhanced = true;
        error._lispLocation = new SourceLocation(file, line, column);
        error._lispCallStack = lispCallStack.slice();

        const originalMessage = error.message;
        const location = error._lispLocation;

        // Enhance the error message
        let enhanced = `${originalMessage}\n  at ${location}`;
        if (error._lispCallStack.length > 0) {
            enhanced += '\n\nLisp Call Stack:';
            for (let i = error._lispCallStack.length - 1; i >= 0; i--) {
                enhanced += `\n  ${error._lispCallStack.length - i}. ${error._lispCallStack[i].toString()}`;
            }
        }

        error.message = enhanced;
    }
    return error;
}

// Variable inspector - get info about a variable in current scope
function inspect(name, value) {
    console.log(`[FoxLisp Debug] Inspecting '${name}':`);
    console.log(`  Type: ${typeof value}`);
    console.log(`  Value: ${formatValue(value)}`);

    if (Array.isArray(value)) {
        console.log(`  Length: ${value.length}`);
    }

    if (typeof value === 'object' && value !== null) {
        console.log(`  Properties: ${Object.keys(value).join(', ')}`);
    }

    if (typeof value === 'function') {
        const info = getFunctionInfo(value);
        if (info) {
            console.log(`  Defined at: ${info.file}:${info.line}`);
        }
    }

    return value;
}

// Step-through debugging state
const stepState = {
    mode: 'none', // 'none', 'step', 'next', 'continue'
    targetDepth: 0
};

function setStepMode(mode) {
    stepState.mode = mode;
    stepState.targetDepth = lispCallStack.length;
}

function shouldPause() {
    switch (stepState.mode) {
        case 'step':
            return true;
        case 'next':
            return lispCallStack.length <= stepState.targetDepth;
        case 'continue':
            return false;
        default:
            return false;
    }
}

// Debug assertion
function debugAssert(condition, message, file, line) {
    if (!condition) {
        const error = createLispError(
            `Assertion failed: ${message || 'no message'}`,
            file, line, 0
        );
        console.error(error.toString());
        throw error;
    }
}

// Log with source location
function debugLog(message, file, line) {
    if (debugConfig.logLevel === 'debug') {
        console.log(`[${file}:${line}] ${message}`);
    }
}

// Timing utility for profiling
const timers = new Map();

function timerStart(name) {
    timers.set(name, {
        start: performance.now(),
        calls: (timers.get(name)?.calls || 0) + 1
    });
}

function timerEnd(name) {
    const timer = timers.get(name);
    if (timer) {
        const elapsed = performance.now() - timer.start;
        timer.total = (timer.total || 0) + elapsed;
        timer.last = elapsed;
        return elapsed;
    }
    return 0;
}

function timerReport() {
    console.log('[FoxLisp Debug] Timer Report:');
    for (const [name, timer] of timers) {
        const avg = timer.total / timer.calls;
        console.log(`  ${name}: ${timer.calls} calls, ${timer.total.toFixed(2)}ms total, ${avg.toFixed(2)}ms avg`);
    }
}

function timerClear() {
    timers.clear();
}

// Export all debug utilities
module.exports = {
    // Configuration
    debugConfig,
    setDebugMode,
    setTraceMode,

    // Breakpoints
    addBreakpoint,
    removeBreakpoint,
    clearBreakpoints,
    listBreakpoints,
    shouldBreak,
    debugBreak,

    // Tracing
    traceEnter,
    traceExit,
    wrapWithTrace,

    // Call stack
    getCallStack,
    printCallStack,
    getStackTraceString,
    clearCallStack,

    // Source mapping
    SourceLocation,
    registerSourceLocation,
    lookupSourceLocation,

    // Function registry
    registerFunction,
    getFunctionInfo,

    // Errors
    LispError,
    createLispError,
    enhanceError,

    // Inspection
    inspect,
    formatValue,

    // Step debugging
    setStepMode,
    shouldPause,
    stepState,

    // Assertions
    debugAssert,
    debugLog,

    // Profiling
    timerStart,
    timerEnd,
    timerReport,
    timerClear
};
