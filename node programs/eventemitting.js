var events = require('events');
var eventEmitter = new events.EventEmitter();

// listener #1
var listner1 = function listner1() {
   console.log('listner1 executed.');
}

// listener #2
var listner2 = function listner2() {
  console.log('listner2 executed.');
}

//Bind the listener 1 to connection event
eventEmitter.addListener('connection',listner1);

//Bind the listener 2 to connection event
eventEmitter.on('connection',listner2);

//Fire Connection Event
console.log("Fired connection");
eventEmitter.emit('connection');

eventEmitter.removeListener('connection',listner2);
console.log("Listener 2 deactivated");

//Fire connection event
console.log("Fired connection");
eventEmitter.emit('connection');

var listenerCount = require('events').EventEmitter.listenerCount(eventEmitter,'connection');
console.log(listenerCount+" Listener(s) are listening");
listenerCount = require('events').EventEmitter.listenerCount(eventEmitter,'connection');
console.log(listenerCount+" Listener(s) are listening");

//Remove all
console.log("Remove all");
eventEmitter.removeAllListeners('connection');

var listenerCount = require('events').EventEmitter.listenerCount(eventEmitter,'connection');
console.log(listenerCount+" Listener(s) are listening");

console.log("Program Ended")