import { drawTree } from './drawing.js';
import { addColors, flattenGoalConj } from './tree_setup.js';
import * as d3 from 'https://cdn.jsdelivr.net/npm/d3@7/+esm';

let treeData = { 
    "name" : "Empty",
    "children" : []
}

function redrawTree(treeData) {
    const svg = d3.select("svg").html("").append("g");

    const treeLayout = d3.tree()
        .nodeSize([150, 100]); // Adjust vertical/horizontal spacing

    const root = d3.hierarchy(flattenGoalConj(addColors(treeData)));
    treeLayout(root);

    const nodes = root.descendants();
    const links = root.links();

    // Calculate bounding box of the tree
    const minX = Math.min(...nodes.map(d => d.x));
    const maxX = Math.max(...nodes.map(d => d.x));
    const minY = Math.min(...nodes.map(d => d.y));
    const maxY = Math.max(...nodes.map(d => d.y));
    const padding = 100; // Increase padding if nodes are clipped

    // Calculate total width/height of the tree (including padding)
    const treeWidth = maxX - minX + padding * 2;
    const treeHeight = maxY - minY + padding * 2;

    // Set the SVG dimensions and viewBox to encapsulate the entire tree
    d3.select("svg")
        .attr("width", treeWidth)
        .attr("height", treeHeight)
        .attr("viewBox", `${minX - padding} ${minY - padding} ${treeWidth} ${treeHeight}`)
        .attr("preserveAspectRatio", "xMidYMid meet"); // Centers content

    // Draw the tree
    drawLinks(svg, links);
    drawNodes(svg, nodes);

    // Debugging: Log key metrics
    console.log("Bounding Box:", { minX, maxX, minY, maxY });
    console.log("SVG Dimensions:", { treeWidth, treeHeight });
}


function drawLinks(svg, links) {
    svg.selectAll(".link")
        .data(links)
        .join("path")
        .attr("class", "link")
        .attr("d", d3.linkVertical().x(d => d.x).y(d => d.y))
        .style("stroke", d => (d.target.data.color ? d.target.data.color : "#ccc"))
        .style("stroke-width", 4);
}

function drawNodes(svg, nodes) {
    const nodeGroups = svg.selectAll(".node")
        .data(nodes)
        .join("g")
        .attr("class", "node")
        .attr("transform", d => `translate(${d.x},${d.y})`)
        .on("click", (event, d) => alert(toString(d.data)));

    addTooltips(nodeGroups);
    drawTree(nodeGroups);
}

function adjustNodePositions(root) {
    const depthMap = new Map(); // Track x-positions at each depth

    root.eachBefore(node => {
        if (!depthMap.has(node.depth)) {
            depthMap.set(node.depth, 0); // Initialize x-tracking at depth
        }

        const previousX = depthMap.get(node.depth);
        const nodeWidth = node.data.width || 100; // Set a default width if undefined
        node.x = previousX + nodeWidth / 2; // Assign new x-position
        depthMap.set(node.depth, node.x + nodeWidth / 2 + 20); // Store updated x
    });

    return root;
}


function updatePositions(svg, nodes, links) {
    nodes.forEach(d => d.y = d.depth * 150); // Maintain vertical alignment

    svg.selectAll(".node")
        .attr("transform", d => `translate(${d.x},${d.y})`);

    svg.selectAll(".link")
        .attr("d", d3.linkVertical().x(d => d.x).y(d => d.y));
}

function updateScrollBar(nodes) {
    const minX = Math.min(...nodes.map(d => d.x));
    const maxX = Math.max(...nodes.map(d => d.x));
    const minY = Math.min(...nodes.map(d => d.y));
    const maxY = Math.max(...nodes.map(d => d.y));
    const padding = 100;

    const svgWidth = maxX - minX + padding;
    const svgHeight = maxY - minY + padding;

    // Compute the center of the tree
    const centerX = (minX + maxX) / 2;
    const centerY = (minY + maxY) / 2;

    // Update SVG dimensions
    d3.select("svg")
        .attr("width", svgWidth)
        .attr("height", svgHeight)
        .attr("transform", `translate(${-(minX - padding)}, 0`);
}



function toString(node) {
    if (node.name === "Conjunction" && node.sub) {
        return `Conjunction\n${subToString(node.sub)}`;
    }
    return node.name;
}

function subToString(sub) {
    return sub.map(({ key, value }) => `${key} ${value}`).join("\n");
}

function addTooltips(nodeGroups) {
    const tooltip = d3.select("body").append("div")
        .style("position", "absolute")
        .style("background-color", "white")
        .style("border", "1px solid #ccc")
        .style("border-radius", "5px")
        .style("padding", "5px")
        .style("visibility", "hidden");

    nodeGroups.filter(d => d.data.sub)
        .on("mouseover", (event, d) => {
            tooltip.html(subToString(d.data.sub).replace(/\n/g, "<br>"))
                .style("left", `${event.pageX + 10}px`)
                .style("top", `${event.pageY + 10}px`)
                .style("visibility", "visible");
        })
        .on("mouseout", () => tooltip.style("visibility", "hidden"));
}

function fetchAndUpdateTree() {
    fetch("http://localhost:5000/api/get", {  
        method: "GET",
        headers: { "Content-Type": "application/json" }
    })
    .then(response => {
        if (!response.ok) throw new Error(`HTTP error! Status: ${response.status}`);
        return response.text();  
    })
    .then(text => {
        try {
            console.log(text)
            const data = JSON.parse(JSON.parse(text));  
            console.log(data)
            redrawTree(data); 
        } catch (error) {
            console.error("Error parsing JSON:", error);
        }
    })
    .catch(error => console.error("Error:", error));
}

document.addEventListener("keyup", function(event) {
    if (event.key === "j") { fetchAndUpdateTree() }
});


// Initial render
redrawTree(treeData);
