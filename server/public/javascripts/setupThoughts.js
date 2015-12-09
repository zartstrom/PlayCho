

function setupWebSocket() {
    console.log("setupWebSocket")
    var socket = new WebSocket("ws:localhost:9000/socket");

    socket.onmessage = function(ev) {
        console.log("got message: ", ev.data);
        var json = JSON.parse(ev.data);
        var thoughts = "";
        for (idx in json.moves) {
            var move = json.moves[idx];
            console.log(move);
            thoughts += "<p>" + move.coord + ": <progress value=" + move.value + " max=1><progress><p>";
        }
        var t = document.getElementById("thoughts");
        t.innerHTML = thoughts;
    }
}
