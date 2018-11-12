//Import events module
var events = require('events');

//Create an Event Emitter Object
var eventEmitter = new events.EventEmitter();

//Create an Event Handler
var connectHandler = function connected(){
    console.log("Conenction SuccessFul");
    //Fire the DATARECIEVED
    eventEmitter.emit('data_recieved');
}

//Bind Connection
eventEmitter.on('connection',connectHandler);

//Bind DATARECIVED with anonymous function
eventEmitter.on('data_recieved',function(){
    console.log("Data Recieved Successfully");
});

//Fire connectHandler by connection event
eventEmitter.emit('connection');

console.log("Program Ended");