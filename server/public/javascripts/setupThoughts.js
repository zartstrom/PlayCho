

function setupWebSocket() {
    console.log("setupWebSocket")
    var socket = new WebSocket("ws:localhost:9000/socket");
    socket.onmessage = function(ev) {
        console.log("got message: ", ev.data)
        var box = document.getElementById("thoughts");
        box.innerHTML = ev.data;
    }
}
