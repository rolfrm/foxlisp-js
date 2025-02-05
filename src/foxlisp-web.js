___process_args = []

require("./lisp")

let load_file = (filePath) => {
	
	return fetch(filePath)
	 	
		  .then(response => {
			if (!response.ok) {
				return "(println 'file-not-found \"" + filePath + "\")"
			}
			return response.text()
			
		  })
		  
}

function WriteCodeToLog(code){
	return;
}
writeCodetoLog = WriteCodeToLog
loadFileAsync = load_file

console.log("foxlisp JS loaded!")
