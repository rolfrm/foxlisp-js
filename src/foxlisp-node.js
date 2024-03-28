const fs = require('fs');
let load_file = (filePath, contentAction) => {
	fs.readFile(filePath, (err, data) => {
		if (err) {
			console.error('Error reading the file:', err);
			return;
		}
		contentAction(data)
	});

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
