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
        var node_size = (w > h ? w : h) / 20;
        var line_width = node_size * 0.5;
        var mine_size = node_size * 1.5;
        
        if (document.cy)
            document.cy.destroy();
        var cy = cytoscape({
            container: document.getElementById("cy"),
            style: 
"node { background-color: black; width: " + node_size + "; height: " + node_size + "; }" +
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
        cy.fit();

        document.timestamp = 0;
        document.moves = json.moves;
        document.palette = [ "GoldenRod",
                             "DarkTurquoise",
                             "DarkMagenta",
                             "Blue",
                             
                             "Coral",
                             "DeepPink",
                             "Indigo",
                             "LightSeaGreen",
                           ]
    };
    reader.readAsText(file);
}

function history_forward() {
    if (document.timestamp < document.moves.length) {
        var move = document.moves[document.timestamp];
        update_move_info(move);
        if ("claim" in move) {
            var claim = move.claim;
            var cy = document.cy;
            cy.edges("#" + make_edge_id(claim.source, claim.target))
                .forEach(function(edge) {
                    edge.style("line-color", document.palette[claim.punter]);
                });
        }
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
        if ("claim" in move) {
            var claim = move.claim;
            var cy = document.cy;
            cy.edges("#" + make_edge_id(claim.source, claim.target))
                .forEach(function(edge) {
                    edge.style("line-color", "lightgray");
                });
        }
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
        if (r.min_x == null) {
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
