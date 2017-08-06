function show_history_file() {
    var input = document.getElementById("hfile");
    var file = input.files[0];
    if (!file) {
        alert("Can't open file");
        return;
    }
    var reader = new FileReader();
    reader.onload = function () {
        var text = reader.result;
        var json = JSON.parse(text);
        var map = json.map;

        var bb = bounding_box(map.sites);
        var w = bb.max_x - bb.min_x;
        var h = bb.max_y - bb.min_y;
        var node_size = Math.sqrt((w * h / map.sites.length) * 0.07);
        var line_width = node_size * 0.5;
        var mine_size = node_size * 1.5;

        var players = [ "all" ];
        for (var move of json.moves) {
            var punter = null;
            if ("claim" in move) punter = move.claim.punter;
            if ("pass" in move) punter = move.pass.punter;
            if (punter != null && players.indexOf(punter) === -1)
                players.push(punter);
        }

        document.players_number = players.length - 1;

        var player_selector = document.getElementById("player_selector");

        for(var i = player_selector.options.length - 1 ; i >= 0 ; i--)
            player_selector.remove(i);

        for (var value of players) {
            var opt = document.createElement('option');
            opt.text = value;
            opt.value = value;
            player_selector.add(opt, null);
        }
        
        if (document.cy)
            document.cy.destroy();
        var cy = cytoscape({
            container: document.getElementById("cy"),
            style: 
            "node { background-color: DarkGray; width: " + node_size + "; height: " + node_size + ";" +
                "z-index: 10;" +
                " }" +
"edge { width: " + line_width + "; line-color: lightgray; }"
        });
        document.cy = cy;
        map.sites.forEach(function (site) {
            cy.add([
                { group: "nodes",
                  data: { id: "node_" + site.id },
                  position: { x: site["x"], y: site["y"] } },
            ]);
        });
        map.rivers.forEach(function (river) {
            cy.add([
                { group: "edges",
                  data: { id: make_edge_id(river.source, river.target),
                          source: "node_" + river.source,
                          target: "node_" + river.target,
                        }
                }
            ]);
        });
        map.mines.forEach(function (mine) {
            cy.nodes("#node_" + mine).forEach(function(node) {
                node.style("background-color", "red");
                node.style("width", mine_size);
                node.style("height", mine_size);
            });
        });
	json.futures.forEach(function (future) {
            cy.nodes("#node_" + future.target).forEach(function(node) {
                node.style("background-color", "orange");
                node.style("width", mine_size);
                node.style("height", mine_size);
            });
	    cy.add([
                { group: "edges",
                  data: { id: "future_" + future.source + "_" + future.target,
                          source: "node_" + future.source,
                          target: "node_" + future.target,
                        }
                }
            ]);
	    cy.edges("#future_" + future.source + "_" + future.target)
		.forEach(function(edge) {
		    edge.style("line-color", "blue");
		    edge.style("opacity", "0.3");
		    edge.style("z-index", 0);
		});
		    
        });
	// cy.on('mouseover', 'node', function(evt){
	//     var node = evt.target;
	//     console.log('mouseover ' + node.id());
	//     json.futures.forEach(function (future) {
	// 	if (future.source == node.id()) {
	// 	    cy.edges("#future_" + future.source + "_" + future.target)
	// 		.forEach(function(edge) {
	// 		    edge.style("line-color", "blue");
	// 		    edge.style("opacity", "0.2");
	// 		    edge.style("z-index", 0);
	// 		});
	// 	}
	//     });
	// });
	

        cy.fit();

        document.timestamp = 0;
        document.moves = json.moves;
        document.default_palette =
            [ "GoldenRod",
              "DarkTurquoise",
              "DarkMagenta",
              "Blue",
              
              "Coral",
              "DeepPink",
              "Indigo",
              "LightSeaGreen",
            ];
        document.palette = document.default_palette;
    };
    reader.readAsText(file);
}

function history_forward() {
    if (document.timestamp < document.moves.length) {
        var move = document.moves[document.timestamp];
        update_move_info(move);
        if ("claim" in move)
            mark_taken(move.claim)
        document.timestamp++;
    } else {
        update_move_info(null);
    }
    update_time_info();
}

function history_backward() {
    if (document.timestamp <= document.moves.length && document.timestamp > 0) {
        document.timestamp--;
        var move = document.moves[document.timestamp];
        update_move_info(move);
        if ("claim" in move)
            mark_free(move.claim)
    } else {
        update_move_info(null);
    }
    update_time_info();
}

function make_edge_id(from, to) {
    if (from > to)
        return "edge_" + from + "_" + to;
    else
        return "edge_" + to + "_" + from;
}

function bounding_box(sites) {
    var r = {  min_x: null,
               max_x: null,
               min_y: null,
               max_y: null
            }
    sites.forEach(function (site) {
        if (r.min_x != null) {
            if (r.min_x > site.x)
                r.min_x = site.x;
            if (r.max_x < site.x)
                r.max_x = site.x;
            
            if (r.min_y > site.y)
                r.min_y = site.y;
            if (r.max_y < site.y)
                r.max_y = site.y;
        } else {
            r.min_x = site.x;
            r.max_x = site.x;
            r.min_y = site.y;
            r.max_y = site.y;
        }
    });
    return r;
}

function update_time_info() {
    var div = document.getElementById("time_info");
    div.innerHTML = "time: " + document.timestamp + " / " + document.moves.length;
}

function update_move_info(move) {
    var div = document.getElementById("move_info");
    
    if (move == null) {
        div.innerHTML = "no move"
        return;
    }

    div.innerHTML = JSON.stringify(move);
}

function mark_edge(edge, style) {
    document.cy.edges("#" + make_edge_id(edge.source, edge.target))
        .forEach(function(edge) {
            for (var [key, val] of Object.entries(style))
                edge.style(key, val);
        });
}

function mark_free(claim) {
    mark_edge(claim, { "line-color": "lightgray",
                       "z-index": 10
                     })
}

function mark_taken(claim) {
    var palette = document.palette;
    var focus_player = parseInt(document.getElementById("player_selector").value);
    var z_index = 20;
    if (focus_player == claim.punter)
        z_index = 30;
    mark_edge(claim, { "line-color": palette[claim.punter],
                       "z-index": z_index
                     })
}

function history_reset() {
    document.timestamp = 0;
    update_move_info(null);
    update_time_info();
    for (var move of document.moves) {
        if ("claim" in move)
            mark_free(move.claim);
    }
}

function history_goto() {
    var div = document.getElementById("history_goto");
    history_goto_aux(parseInt(div.value));
}

function history_goto_aux(number) {
    if (number < 0)
        number = 0;
    if (number > document.moves.length)
        number = document.moves.length;

    history_reset();
    for (var i = 0; i < number; i++)
        history_forward();
}

function history_max() {
    history_goto_aux(document.moves.length);
}

function show_player() {
    var selector = document.getElementById("player_selector");
    var p = selector.value;
    if (p == "all") {
        document.palette = document.default_palette;
    } else {
        var palette = [];
        for (var i = 0; i < document.players_number; ++i)
            palette.push("PeachPuff");
        palette[p] = "SeaGreen";
        document.palette = palette;
    }
    history_goto_aux(document.timestamp);
}

function history_player_forward() {
    for (var i = 0; i < document.players_number; ++i)
        history_forward();
}

function history_player_backward() {
    for (var i = 0; i < document.players_number; ++i)
        history_backward();
}
