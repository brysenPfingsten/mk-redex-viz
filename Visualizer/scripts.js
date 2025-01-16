import { drawTree } from  './drawing.js';
import { addColors, flattenGoalConj } from './tree_setup.js';
import * as d3 from 'https://cdn.jsdelivr.net/npm/d3@7/+esm';

const svg = d3.select("svg").append("g");

let treeData = {
    "name": "Answer",
    "sub": [
        {
            "key": 0,
            "value": "abc"
        },
        {
            "key": 1,
            "value": "def"
        },
        {
            "key": 2,
            "value": "ghi"
        }
    ],
    "children": [
        {
            "name": "+->",
            "children": [
                {
                    "name": "Fresh",
                    "vars": [
                        "a",
                        "ad",
                        "add"
                    ],
                    "children": [
                        {
                            "name": "Goal-Conj",
                            "children": [
                                {
                                    "name": "Unify",
                                    "left": "a",
                                    "right": "d"
                                },
                                {
                                    "name": "Unify",
                                    "left": "add",
                                    "right": "jkl"
                                }
                            ]
                        }
                    ],
                    "sub": [
                        {
                            "key": 0,
                            "value": "mno"
                        },
                        {
                            "key": 1,
                            "value": "pqr"
                        },
                        {
                            "key": 2,
                            "value": "stu"
                        }
                    ]
                },
                {
                    "name": "<-+",
                    "children": [
                        {
                            "name": "Conjunction",
                            "children": [
                                {
                                    "name": "Delay",
                                    "children": [
                                        {
                                            "name": "Empty"
                                        }
                                    ]
                                },
                                {
                                    "name": "Rel-Call",
                                    "rel": "testo",
                                    "args": [
                                        "abc"
                                    ]
                                }
                            ]
                        },
                        {
                            "name": "Goal-Disj",
                            "children": [
                                {
                                    "name": "Rel-Call",
                                    "rel": "testo",
                                    "args": [
                                        "vwx",
                                        "yz"
                                    ]
                                },
                                {
                                    "name": "Unify",
                                    "left": "abc",
                                    "right": "abc"
                                }
                            ],
                            "sub": [
                                {
                                    "key": 0,
                                    "value": "def"
                                },
                                {
                                    "key": 1,
                                    "value": 2
                                },
                                {
                                    "key": 2,
                                    "value": [
                                        "ghi",
                                        "jkl",
                                        "empty"
                                    ]
                                }
                            ]
                        }
                    ]
                }
            ]
        }
    ]
};

// Create a tree layout
const treeLayout = d3.tree().nodeSize([150, 100]);
const root = d3.hierarchy(addColors(treeData));
treeLayout(root);

const nodes = root.descendants();
const links = root.links();

function updateScrollBar() {
    // Calculate dynamic SVG dimensions
    const minX = Math.min(...nodes.map(d => d.x));
    const maxX = Math.max(...nodes.map(d => d.x));
    const minY = Math.min(...nodes.map(d => d.y));
    const maxY = Math.max(...nodes.map(d => d.y));
    const padding = 200;
    
    // Adjust the SVG size
    const svgWidth = maxX - minX + padding;
    const svgHeight = maxY - minY + padding;
    
    d3.select("svg")
    .attr("width", svgWidth)
    .attr("height", svgHeight);
    
    // Center the tree
    svg.attr(
        "transform",
        `translate(${(svgWidth / 2) - ((maxX + minX) / 2)},${padding / 2})`
    );
}


function toString(node, indent = "") {
    // Check if the node is a top-level conjunction with a "state"
    if (node.name === "Conjunction" && node.sub) {
        const stateLines = (node.sub || [])
        .map(({ key, value }) => `${indent}${key} => ${value}`)
        .join("\n");
        
        return `${indent}Conjunction\n${stateLines}`;
    }
    
    return `${indent}${node.name}`;
}

function subToString(sub) {
    return sub
    .map(({ key, value }) => `${key} => ${value}`)
    .join("\n");
}


// Draw links
svg.selectAll(".link")
.data(links)
.join("path")
.attr("class", "link")
.attr("d", d3.linkVertical()
.x(d => d.x)
.y(d => d.y))
.style("stroke", d => {
    if (d.target.data.color) return d.target.data.color;
    else return "#ccc";
})
.style("stroke-width", 4);

// Draw nodes
const nodeGroups = svg.selectAll(".node")
.data(nodes)
.join("g")
.attr("class", "node")
.attr("transform", d => `translate(${d.x},${d.y})`)
.on("click", (event, d) => {
    alert(toString(d.data));
});

const tooltip = d3.select("body")
.append("div")
.style("position", "absolute")
.style("background-color", "white")
.style("border", "1px solid #ccc")
.style("border-radius", "5px")
.style("padding", "5px")
.style("visibility", "hidden");

nodeGroups.on("mouseover", (event, d) => {
    const subText = d.data.sub ? `${subToString(d.data.sub).replace(/\n/g, "<br>")}` : "";
    
    tooltip.html(`${subText}<br>`)
    .style("left", (event.pageX + 10) + "px")
    .style("top", (event.pageY + 10) + "px")
    .style("visibility", "visible");
}).on("mouseout", () => {
    tooltip.style("visibility", "hidden");
});

drawTree(nodeGroups);

const simulation = d3.forceSimulation(root.descendants())
.force("link", d3.forceLink(root.links()).distance(100)) // Control link distance
.force("charge", d3.forceManyBody().strength(-200)) // Repel nodes horizontally
.force("center", d3.forceCenter(500, 300)) // TODO
.force("collision", d3.forceCollide().radius(100)) // Ensure no horizontal overlap
.on("tick", () => {
    // Constrain nodes to their vertical positions
    root.descendants().forEach(d => {
        d.y = d.depth * 150; // Keep y aligned based on depth in the tree
    });
    updateScrollBar();
    // Update node and link positions
    svg.selectAll(".node")
    .attr("transform", d => `translate(${d.x},${d.y})`);
    
    svg.selectAll(".link")
    .attr("d", d3.linkVertical()
    .x(d => d.x)
    .y(d => d.y));
});

fetch("http://localhost:8000", {
    method: "GET",
    headers: {
        "Content-Type": "application/json"
    }
})
.then(response => response.json())
.then(data => console.log("Response from Racket:", data))
.catch(error => console.error("Error:", error));
