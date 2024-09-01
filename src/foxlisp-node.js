const fs = require('fs').promises;
async function load_file(filePath){
    return await fs.readFile(filePath, 'utf8');
}

async function load_file_bytes(filePath){
	 return new Uint8Array(await fs.readFile(filePath));
}

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

/*
const wabt = require('./extern/wabt.js');

(async () => {
    // Initialize wabt
    const wabtModule = await wabt();

    // Parse the WAT code to get a WABT module
	 loadWat = function (watCode){
		  
		  const parsedWat = wabtModule.parseWat('inline', watCode, {});
		  const {buffer} = parsedWat.toBinary({})
		  const mod = new WebAssembly.Module(buffer);
		  const instance = new WebAssembly.Instance(mod, {});
		  return instance
	 }

	
})();*/
