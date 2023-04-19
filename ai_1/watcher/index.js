const WebSocket = require('ws');

const ws = new WebSocket('ws://localhost:12345');

ws.on('open', function open() {
  console.log('connected');
  
});

ws.on('message', function incoming(data) {
  console.log(`received: ${data}`);

  //console.debug(JSON.parse(data));

  ws.send('antwort: '+data);
});
