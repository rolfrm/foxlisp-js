const fs = require('fs').promises;
async function load_file(filePath){
    return await fs.readFile(filePath);
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
