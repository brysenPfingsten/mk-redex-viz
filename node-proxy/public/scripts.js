import { drawTree } from './drawing.js';
import { addColors, flattenGoalConj } from './tree_setup.js';
import * as d3 from 'https://cdn.jsdelivr.net/npm/d3@7/+esm';

const treeData = { 
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

function arrayToString(arr) {
    return "(cons " + arr.map(item => 
        Array.isArray(item) ? arrayToString(item) : item
    ).join(" ") + ")";
}


function termToString(term) {
    if (Array.isArray(term)) { return arrayToString(term); }
    return term
}

function subToString(sub) {
    return sub ? sub.map(({ key, value }) => `${key} => ${termToString(value)}`).join("\n") : "\n";
}

function trailToString(trail) {
    return trail ? trail.map(crumb => `(== ${termToString(crumb.left)} ${termToString(crumb.right)})`).join("\n") : "\n";
}

function reificationToString(reification) {
    if (!reification) { return ''; }
    if (reification.length === 1) { return reification[0] }
    return arrayToString(reification)
}

function toString(sub, trail, reification) {
    return `Substitutions:\n${subToString(sub)}\nTrail:\n${trailToString(trail)}\nCurrent Answer:\n${reificationToString(reification)}`
}


function highlightIDs(ids) {
    // First, clear any existing highlights.
    document.querySelectorAll('.hidden-tag.highlight').forEach(el => {
        el.classList.remove('highlight');
    });
    
    // For each ID in the array, add the highlight class to its corresponding elements.
    ids.forEach(id => {
        console.log(id)
        const elements = document.querySelectorAll(`.hidden-tag[data-id="${id}"]`);
        elements.forEach(el => {
            el.classList.add('highlight');
        });
    });
}


function addTooltips(nodeGroups) {
    const tooltip = d3.select("body").append("div")
    .style("position", "absolute")
    .style("background-color", "white")
    .style("border", "1px solid #ccc")
    .style("border-radius", "5px")
    .style("padding", "5px")
    .style("visibility", "hidden");
    
    nodeGroups.filter(d => d.data.sub || d.data.trail || d.data.reified)
    .on("click", (event, d) => {
        tooltip.html(toString(d.data.sub, d.data.trail, d.data.reified).replace(/\n/g, "<br>"))
        .style("left", `${event.pageX + 10}px`)
        .style("top", `${event.pageY + 10}px`)
        .style("visibility", "visible");
        let ids = d.data.trail.map(trail => trail.id)
        highlightIDs(ids)
    })
    .on("mouseout", () => tooltip.style("visibility", "hidden"));
}

function sendRequest(method, path) {
    if (method === 'GET') { document.getElementById('back-btn').disabled = false; }

    fetch(path, {
        method: method,
        headers: {'Content-Type': 'application/json'}
    })
    .then(response => {
        if (response.headers.get('X-Is-Last') === 'true') {
            setDisabled(['back'], true)
        }

        if (!response.ok) throw new Error(`HTTP error! Status: ${response.status}`);

        return response.text();  
    })
    .then(text => {
        try {
            const data = JSON.parse(JSON.parse(text));
            const redStep = data.stepName;
            const stepNum = data.step;
            const tree = data.program
            document.getElementById('step-info').innerHTML = `Step: ${stepNum}<br>Reduction Step: ${redStep}`;
            redrawTree(tree);
        } catch (error) {
            console.error('Error parsing JSON: ', error);
        }
    })
    .catch(error => console.error('Error: ', error));
}

function fetchAndUpdateTree() {
    sendRequest('GET', 'api/get/next');
}

function resetTree() {
    setDisabled(['back'], true)
    sendRequest('POST', 'api/post/reset');
}

function back() {
    sendRequest('POST', 'api/post/back');
}

function getInit() {
    sendRequest('GET', 'api/get/init');
}

function setDisabled(buttons, flag) {
    if (buttons.includes('debug')) { document.getElementById('debug-btn').disabled = flag };
    if (buttons.includes('reset')) { document.getElementById('reset-btn').disabled = flag };
    if (buttons.includes('back')) { document.getElementById('back-btn').disabled = flag };
    if (buttons.includes('step')) { document.getElementById('step-btn').disabled = flag };
}

// Function to update the overlay by processing marker syntax (e.g., [[tag]]...[[/tag]])
function updateOverlay() {
    const codeInput = document.getElementById("code-input");
    const overlay = document.getElementById("highlight-overlay");
    let code = codeInput.value;

    // Replace opening markers with a span tag
    code = code.replace(/\[\[([a-zA-Z0-9_-]+)\]\]/g, (match, p1) => {
        return `<span class="hidden-tag" data-id="${p1}">`;
    });
    // Replace closing markers with a span closing tag
    code = code.replace(/\[\[\/([a-zA-Z0-9_-]+)\]\]/g, "</span>");
    
    overlay.innerHTML = code;
}

// Function to lock the code: disable the textarea and show the overlay
function lockCode() {
    console.log('clicked')
    getInit();
    setDisabled(['reset', 'step'], false)
    const textarea = document.getElementById("code-input");
    const overlay = document.getElementById("highlight-overlay");
    
    // Disable editing
    textarea.disabled = true;
    
    // Process the content to update the overlay
    updateOverlay();
    
    // Change the textarea style so its text becomes transparent
    textarea.classList.add("locked");
    
    // Show the overlay
    overlay.style.display = "block";
}



document.addEventListener("DOMContentLoaded", () => {
    document.getElementById("debug-btn").addEventListener("click", lockCode);
    document.getElementById("back-btn").addEventListener("click", back)
    document.getElementById("reset-btn").addEventListener("click", resetTree);
    document.getElementById("step-btn").addEventListener("click", fetchAndUpdateTree);
});



// Initial render
setDisabled(['reset', 'back', 'step'], true);
redrawTree(treeData);