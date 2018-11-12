var fs = require("fs");

var data = fs.readFileSync('input.txt');
// Blocking Code
/*
The first example shows that the program blocks until it reads the file and then only it proceeds to end the program.
*/

console.log(data.toString());
console.log("Blocking Code Ended");
//Non Blocking Code

/*
The second example shows that the program does not wait for file reading and proceeds to print "Program Ended" and at the same time, the program without blocking continues reading the file.
*/

fs.readFile('input.txt', function (err, data) {
   if (err) return console.error(err);
   console.log(data.toString());
});

console.log("Non Blocking Code");