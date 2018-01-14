
var express = require('express');
var app = express();
var bodyParser = require('body-parser');

// Always make req.body available as a String
app.use(bodyParser.text(function() { return true; }));

app.use(express.static(__dirname));

app.get('/', function (req, res) {
  res.send('<html><script src="app.js"></script></html>');
});

app.get('/test-available-username', function(req, res) {
  res.header({'content-type': 'text/plain'});
  setTimeout(function () {
    var result = (req.query.name == "Tom" ? "no" : "yes");
    res.send(result);
  }, 3000);
});

var server = app.listen(8000, function () {
  var host = server.address().address;
  var port = server.address().port;

  console.log('Test server listening at http://%s:%s', host, port);
});

