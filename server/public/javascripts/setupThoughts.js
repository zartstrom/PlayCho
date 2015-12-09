

function setupWebSocket() {
    console.log("setupWebSocket")
    var socket = new WebSocket("ws:localhost:9000/socket");

    //var obj = { "moves": [
    //    {"coord": "B3", "value": 0.56},
    //    {"coord": "E5", "value": 0.51},
    //    {"coord": "G2", "value": 0.49}
    //]};
    //var thoughts = "";
    //for (idx in obj.moves) {
    //    var move = obj.moves[idx];
    //    //console.log(move)
    //    thoughts += "<p>" + move.coord + ": <progress value=" + move.value + " max=1><progress><p>";
    //}
    //var t = document.getElementById("thoughts");
    //t.innerHTML = thoughts;

    socket.onmessage = function(ev) {
        console.log("got message: ", ev.data)
        //var box = document.getElementById("bestMove");
        //box.innerHTML = ev.data;
        var thoughts = "";
        for (idx in ev.data.moves) {
            var move = obj.moves[idx];
            //console.log(move)
            thoughts += "<p>" + move.coord + ": <progress value=" + move.value + " max=1><progress><p>";
        }
        var t = document.getElementById("thoughts");
        t.innerHTML = thoughts;
    }
}
