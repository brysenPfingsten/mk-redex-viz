import { drawTree } from './drawing.js';
import { addColors, flattenGoalConj } from './tree_setup.js';
import { toString } from './strings.js';
import { checkOverflow } from './drag_to_scroll.js';
import * as d3 from 'https://cdn.jsdelivr.net/npm/d3@7/+esm';

const treeData = { 
    "name" : "Empty",
    "children" : []
}

function redrawTree(treeData) {
    const svg = d3.select("svg").html("").append("g");
    
    // Create hierarchy and prepare data
    const root = d3.hierarchy(flattenGoalConj(addColors(treeData)));
    
    // First pass: measure node sizes
    const tempSvg = d3.select("body").append("svg")
        .style("position", "absolute")
        .style("left", "-9999px");
    const tempNodes = tempSvg.selectAll(".temp-node")
        .data(root.descendants())
        .join("g")
        .attr("class", "temp-node");
    
    // Draw the temp nodes
    drawTree(tempNodes); 
    
    // Measure each node and store dimensions
    tempNodes.each(function(d) {
        const bbox = this.getBBox();
        d.data.measuredWidth = bbox.width;
        d.data.measuredHeight = bbox.height;
    });
    tempSvg.remove();

    // Configure tree layout with dynamic spacing
    const treeLayout = d3.tree()
        .nodeSize([1, 100]) // Base horizontal unit, vertical spacing
        .separation((a, b) => {
            const padding = 20; // Adjust based on your needs
            if (a.parent === b.parent) return (a.data.measuredWidth + b.data.measuredWidth) / 2 + padding;
            else return (a.data.measuredWidth + b.data.measuredWidth) / 2 + padding + 100;s
        });

    // Compute the layout with adjusted spacing
    treeLayout(root);

    // Calculate dimensions and update SVG
    const nodes = root.descendants();
    const links = root.links();
    
    // Calculate bounding box with padding
    const minX = Math.min(...nodes.map(d => d.x - d.data.measuredWidth/2));
    const maxX = Math.max(...nodes.map(d => d.x + d.data.measuredWidth/2));
    const minY = Math.min(...nodes.map(d => d.y));
    const maxY = Math.max(...nodes.map(d => d.y));
    const padding = 50;
    
    const treeWidth = maxX - minX + padding * 2;
    const treeHeight = maxY - minY + padding * 2;
    
    d3.select("svg")
        .attr("width", treeWidth)
        .attr("height", treeHeight)
        .attr("viewBox", `${minX - padding} ${minY - padding} ${treeWidth} ${treeHeight}`)
        .attr("preserveAspectRatio", "xMidYMid meet");

    // Draw elements
    drawLinks(svg, links);
    drawNodes(svg, nodes);
}

function drawNodes(svg, nodes) {
    const nodeGroups = svg.selectAll(".node")
        .data(nodes)
        .join("g")
        .attr("class", "node")
        .attr("transform", d => `translate(${d.x},${d.y})`);
    
    addTooltips(nodeGroups);
    drawTree(nodeGroups); 
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

function clearHighlights() {
    document.querySelectorAll('.hidden-tag.highlight').forEach(el => {
        el.classList.remove('highlight');
    });
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
    
    nodeGroups.filter(d => d.data.sub || d.data.trail || d.data.reified || d.data.id)
    .on("click", (event, d) => {
        tooltip.html(toString(d.data.sub, d.data.trail, d.data.reified).replace(/\n/g, "<br>"))
        .style("left", `${event.pageX + 10}px`)
        .style("top", `${event.pageY + 10}px`)
        .style("visibility", "visible");
        let ids = []; // [d.data.trail.map(trail => trail.id)]
        if (d.data.id) { ids.push(d.data.id); }
        clearHighlights();
        highlightIDs(ids);
    })
    .on("mouseout", () => tooltip.style("visibility", "hidden"));
}

function sendRequest(method, path, msg="") {
    const options = {
        method: method,
        headers: { 'Content-Type': 'application/json' }
    };

    // Only attach body if the method supports it
    if (method !== "GET" && method !== "HEAD") {
        options.body = JSON.stringify({ text: msg });
    }

    return fetch(path, options)
    .then(response => {
        if (!response.ok) {
            console.error(`HTTP error! Status: ${response.status}`);
            return false;
        }

        if (response.headers.get('X-Is-Last') === 'true' ) { setDisabled(['back'], true); }

        return response.text();  
    })
    .then(text => {
        try {
            const data = JSON.parse(text);
            const redStep = data.stepName;
            const stepNum = data.step;
            const tree = JSON.parse(data.program);
            if (data.htmlGuids) {
                updateOverlay(data.htmlGuids);
                lockCode();
            }
            document.getElementById('step-info').innerHTML = `Step: ${stepNum}<br>Reduction Step: ${redStep}`;
            redrawTree(tree);
            return true;
        } catch (error) {
            console.error('Error parsing JSON: ', error);
            return false;
        }
    })
    .catch(error => {
        console.error('Error: ', error)
        return false;
    });
}

function fetchAndUpdateTree() {
    sendRequest('GET', 'api/get/next')
    .then(success => {
        if (success) {
            clearHighlights();
            setDisabled(['reset', 'back'], false);
            setDisabled(['debug'], true);
        }
    });
}

function resetTree() {
    sendRequest('POST', 'api/post/reset')
    .then(success => {
        if (success) {
            document.getElementById('code-input').disabled = false;
            setDisabled(['back'], true);
            setDisabled(['debug', 'step'], false);
        }
    });
}

function back() {
    sendRequest('POST', 'api/post/back')
}

function init() {
    const text = document.getElementById('code-input').value
    sendRequest('POST', 'api/post/init', text)
    .then(success => {
        if (success) {
            setDisabled(['step', 'reset'], false);
        }
    });
}


function setDisabled(buttons, flag) {
    if (buttons.includes('debug')) { document.getElementById('debug-btn').disabled = flag };
    if (buttons.includes('reset')) { document.getElementById('reset-btn').disabled = flag };
    if (buttons.includes('back')) { document.getElementById('back-btn').disabled = flag };
    if (buttons.includes('step')) { document.getElementById('step-btn').disabled = flag };
}

// Function to update the overlay by processing marker syntax (e.g., [[tag]]...[[/tag]])
function updateOverlay(code) {
    const overlay = document.getElementById("highlight-overlay");

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
    const textarea = document.getElementById("code-input");
    const overlay = document.getElementById("highlight-overlay");
    
    // Disable editing
    textarea.disabled = true;
    
    // Change the textarea style so its text becomes transparent
    textarea.classList.add("locked");
    
    // Show the overlay
    overlay.style.display = "block";
}


document.addEventListener("DOMContentLoaded", () => {
    document.getElementById("debug-btn").addEventListener("click", init);
    document.getElementById("back-btn").addEventListener("click", back)
    document.getElementById("reset-btn").addEventListener("click", resetTree);
    document.getElementById("step-btn").addEventListener("click", fetchAndUpdateTree);
    const container = document.querySelector(".scroll-container");

    let isDragging = false;
    let startX, startY, scrollLeft, scrollTop;

    function disableSelection() {
        document.body.style.userSelect = "none"; 
    }

    function enableSelection() {
        document.body.style.userSelect = "auto"; 
    }

    container.addEventListener("mousedown", (e) => {
        isDragging = true;
        disableSelection();
        container.style.cursor = "grabbing";
        startX = e.pageX - container.offsetLeft;
        startY = e.pageY - container.offsetTop;
        scrollLeft = container.scrollLeft;
        scrollTop = container.scrollTop;
    });

    container.addEventListener("mouseleave", () => {
        isDragging = false;
        enableSelection();
        container.style.cursor = "grab";
    });

    container.addEventListener("mouseup", () => {
        isDragging = false;
        enableSelection();
        container.style.cursor = "grab";
    });

    container.addEventListener("mousemove", (e) => {
        if (!isDragging) return;
        e.preventDefault();
        const x = e.pageX - container.offsetLeft;
        const y = e.pageY - container.offsetTop;
        const walkX = (x - startX) * 1.5; 
        const walkY = (y - startY) * 1.5;
        container.scrollLeft = scrollLeft - walkX;
        container.scrollTop = scrollTop - walkY;
    });
});

// Initial render
setDisabled(['reset', 'back', 'step'], true);
redrawTree(treeData);
