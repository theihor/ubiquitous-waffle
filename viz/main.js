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
        var claim = document.moves[document.timestamp]["claim"];
        if (claim) {
            var cy = document.cy;
            cy.edges("#" + make_edge_id(claim.source, claim.target))
                .forEach(function(edge) {
                    edge.style("line-color", document.palette[claim.punter]);
                });
        }
        document.timestamp++;
    }
}

function history_backward() {
    console.log("ts = " + document.timestamp);
    console.log("len = " + document.moves.length);
    if (document.timestamp <= document.moves.length && document.timestamp > 0) {
        document.timestamp--;
        var claim = document.moves[document.timestamp]["claim"];
        if (claim) {
            var cy = document.cy;
            cy.edges("#" + make_edge_id(claim.source, claim.target))
                .forEach(function(edge) {
                    edge.style("line-color", "lightgray");
                });
            }
    }
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
            console.log("min x: " + r.min_x + " max x: " + r.max_x);
        } else {
            r.min_x = site.x;
            r.max_x = site.x;
            r.min_y = site.y;
            r.max_y = site.y;
        }
    });

    return r;
}
