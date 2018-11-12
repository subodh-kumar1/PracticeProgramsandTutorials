var express = require('express');
var app = express();

var cptable = require('codepage');

app.use(express.static('public'));
app.get('/index.htm', function (req, res) {
   res.sendFile( __dirname + "/" + "index.htm" );
})

app.get('/process_get', function (req, res) {
   // Prepare output in JSON format
   response = {
      first_name:req.query.first_name,
   };
   var res2 = req.query.first_name;
   var sbuf = cptable.utils.encode(1047,res2).toString('hex');
   console.log(response);
   console.log(sbuf);
   res.end(sbuf);
   
        //res.end("Ebcdic : "+sbuf);
})

var server = app.listen(8081, function () {
   var host = server.address().address
   var port = server.address().port
   
   console.log("Example app listening at http://%s:%s", host, port)
})