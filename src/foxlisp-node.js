const fs = require('fs').promises;
const fs2 = require('fs');
const path = require('path');
const process = require('process');

const fallbackPath = process.env.LISP_PATH;

//worker_threads = require("node:worker_threads")
//Worker = worker_threads.Worker

function with_fallback(loader) {
    return async (filePath) => {
        try {
            return await loader(filePath);
        } catch (err) {
            if (err.code === 'ENOENT' && fallbackPath) {
                const fallbackFilePath = path.resolve(fallbackPath, filePath);
                return await loader(fallbackFilePath);
            } else {
                throw err; // rethrow if it's not a "file not found" error
            }
        }
    };
}

const load_file = with_fallback(async (filePath) => await fs.readFile(filePath, 'utf8'));
const load_file_bytes = with_fallback(async (filePath) => new Uint8Array(await fs.readFile(filePath)));

// Usage e
function WriteCodeToLog(code) {
	return;
	fs.appendFile("log.js", "\n" + code, (err) => {
		if (err) {
			console.error('Error appending to file:', err);
			return;
		}

	});
}
writeCodetoLog = WriteCodeToLog
loadFileAsync = load_file;
loadFileBytesAsync = load_file_bytes

loadWat = null;

node_fs = fs2
node_readFileSync = fs.readSync
node_process = process
