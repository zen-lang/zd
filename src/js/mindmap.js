
function drawTree(svg,root, pos) {

    var SWITCH_CONST = 1;

    if (pos === "left") {
        SWITCH_CONST = -1;
    }
    var width = +svg.attr("width");
    var height = +svg.attr("height");

    // Shift the entire tree by half it's width
    var g = svg.append("g").attr("transform", "translate(" + width / 2 + ",0)");

    // Create new default tree layout
    // Set the size
    // Remember the tree is rotated
    // so the height is used as the width
    // and the width as the height
    var tree = d3.tree().size([height, SWITCH_CONST * (width - 150) / 2]);

    tree(root);

    var nodes = root.descendants();
    var links = root.links();
    // Set both root nodes to be dead center vertically
    nodes[0].x = height / 2;

    // Create links
    var link = g.selectAll(".link").data(links).enter();

    link.append("path")
        .attr("class", "link")
        .attr("d", function(d) {
            return "M" + d.target.y + "," + d.target.x + "C" + (d.target.y + d.source.y) / 2.5 + "," + d.target.x + " " + (d.target.y + d.source.y) / 2 + "," + d.source.x + " " + d.source.y + "," + d.source.x;
        });

    // Create nodes
    var node = g.selectAll(".node")
        .data(nodes)
        .enter()
        .append("g")
        .attr("class", function(d) {
            return "node" + (d.children ? " node--internal" : " node--leaf");
        })
        .attr("transform", function(d) {
            return "translate(" + d.y + "," + d.x + ")";
        });

    node.append("circle")
        .attr("r", function(d, i) {
            return 2.5;
        });

    node.append("text")
        .attr("dy", -10)
        .style("text-anchor", "middle")
        .text(function(d) {
            return d.data.name;
        })
        .style("font-size", "10px")
        .style("font-size", function(d) { return (2 * d.r - 10) / this.getComputedTextLength() * 10 + "px"; })
    ;
}

function mindmap(id, data) {
    var split_index = Math.round(data.children.length / 2);

    // Left data
    var data1 = {
        "name": data.name,
        "children": JSON.parse(JSON.stringify(data.children.slice(0, split_index)))
    };

    // Right data
    var data2 = {
        "name": data.name,
        "children": JSON.parse(JSON.stringify(data.children.slice(split_index)))
    };

    // Create d3 hierarchies
    var right = d3.hierarchy(data1);
    var left = d3.hierarchy(data2);


    var svg = d3.select(id);
    // Render both trees
    drawTree(svg, right, "right");
    drawTree(svg, left, "left");
    // draw single tree
}
