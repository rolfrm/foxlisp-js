require("./lisp")

let load_file = (filePath, contentAction) => {
	
	 fetch(filePath)
	 	
		  .then(response => {
			if (!response.ok) {
				return "(println 'file-not-found \"" + filePath + "\")"
			}
			return response.text()
			
		  })
		  .then(data => contentAction(data))
}

function WriteCodeToLog(code){
	return;
}
writeCodetoLog = WriteCodeToLog
loadFileAsync = load_file

console.log("foxlisp JS loaded!")
