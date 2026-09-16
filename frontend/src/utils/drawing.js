import * as d3 from 'd3';
import { termToString } from './strings.js';

export const SEMANTIC_STYLES = Object.freeze({
    search: { label: "Search value", fill: "#fff7dc", stroke: "#986700" },
    operation: { label: "Pending operation", fill: "#edf4ff", stroke: "#285aa0" },
    frontier: { label: "Frontier", fill: "#eaf5ef", stroke: "#28734a" },
    answer: { label: "Committed answer", fill: "#def0e4", stroke: "#22613f" },
    goal: { label: "Goal syntax", fill: "#f5f5f8", stroke: "#737386" },
});

function semanticNodeTitle(data) {
    switch (data.name) {
        case "Unify": return `(== ${termToString(data.left)} ${termToString(data.right)})`;
        case "Disequality": return `(=/= ${termToString(data.left)} ${termToString(data.right)})`;
        case "Fresh": return `fresh (${(data.vars ?? []).map(termToString).join(' ')})`;
        case "Rel-Call": return `(${data.rel}${(data.args ?? []).map(t => ` ${termToString(t)}`).join('')})`;
        case "<-+": return "← mplus";
        case "+->": return "mplusR →";
        case "Delay": return "Delay · suspended body";
        case "More": return "More · paused Frontier";
        default: return data.name;
    }
}

// An Owner is an annotation on this card, never another tree vertex.
// The source IDs and group boundaries remain visible even for empty groups.
function drawSemanticNode(group, data) {
    const style = SEMANTIC_STYLES[data.semanticKind];
    group.attr("data-semantic-kind", data.semanticKind)
        .attr("data-node-name", data.name);
    const content = group.append("g").attr("class", "node-content");
    const category = data.name === "Candidate" ? "Search candidate" : style.label;
    content.append("text").attr("class", "node-category")
        .attr("text-anchor", "middle").attr("y", 0)
        .style("fill", style.stroke).style("font-size", "11px")
        .style("font-weight", 700).text(category);
    content.append("text").attr("class", "node-title")
        .attr("text-anchor", "middle").attr("y", 23)
        .style("fill", "#172334").style("font-size", "14px")
        .text(semanticNodeTitle(data));

    const ownerGroups = content.selectAll("g.owner-group")
        .data(data.owners ?? []).join("g")
        .attr("class", "owner-group")
        .attr("data-source-id", owner => owner.sourceId)
        .attr("transform", (_, index) => `translate(0,${47 + index * 44})`);
    ownerGroups.append("text").attr("text-anchor", "middle")
        .style("font-size", "12px").style("fill", "#47386b")
        .text(owner => `intro [${owner.vars.map(termToString).join(' ')}]`);
    ownerGroups.append("text").attr("text-anchor", "middle").attr("y", 15)
        .style("font-size", "10px").style("fill", "#57476f")
        .text(owner => `source ${owner.sourceId}`);
    ownerGroups.append("title").text(owner =>
        `Introduced together: [${owner.vars.map(termToString).join(' ')}]\nSource: ${owner.sourceId}`);
    ownerGroups.each(function () {
        const owner = d3.select(this);
        const bounds = this.getBBox();
        owner.insert("rect", ":first-child")
            .attr("x", bounds.x - 7).attr("y", bounds.y - 4)
            .attr("width", bounds.width + 14).attr("height", bounds.height + 8)
            .attr("rx", 4).style("fill", "#f0eaf8").style("stroke", "#c9bbdc");
    });

    const bounds = content.node().getBBox();
    const width = Math.max(164, bounds.width + 26);
    const height = bounds.height + 24;
    content.attr("transform", `translate(0,${-bounds.y - bounds.height / 2})`);
    const outline = group.insert("rect", ":first-child")
        .attr("class", "node-outline")
        .attr("x", -width / 2).attr("y", -height / 2)
        .attr("width", width).attr("height", height)
        .attr("rx", data.semanticKind === "search" ? 16 : 5)
        .style("fill", style.fill).style("stroke", style.stroke)
        .style("stroke-width", "2px")
        .style("stroke-dasharray", data.semanticKind === "operation" ? "6 3" : null);
    if (data.semanticKind === "frontier" || data.semanticKind === "answer") {
        group.insert("path", ".node-content")
            .attr("d", `M${-width / 2 + 6},${-height / 2 + 8}v${height - 16}`)
            .style("stroke", style.stroke).style("stroke-width", "3px");
    }
    outline.append("title").text(`${category}: ${semanticNodeTitle(data)}`);
}


function applyStroke(selection, hasSub, isPartial, hasAnswer) {
    if (hasSub && !isPartial) {
        selection
            .clone(true)
            .lower()
            .attr("stroke", "yellow")
            .attr("stroke-width", "8px")
            .attr("fill", "none");
    } else if (isPartial) {
        selection
            .clone(true)
            .lower()
            .attr("stroke", "black")
            .attr("stroke-dasharray", "4 4")
            .attr("stroke-width", "8px")
            .attr("fill", "none");

        selection
            .clone(true)
            .lower()
            .attr("stroke", hasAnswer ? "green" : "red")
            .attr("stroke-dasharray", "4 4")
            .attr("stroke-dashoffset", "4")
            .attr("stroke-width", "8px")
            .attr("fill", "none");
    } else {
        selection
            .attr("stroke", "black")
            .attr("stroke-width", "3px")
    }
}



function drawPolygonNode(group, fillColor, symbol, textColor = "black") {
    const size = 30;
    const points = [
        [0, -size], [size, 0], [0, size], [-size, 0]
    ].map(point => point.join(",")).join(" ");

    const polygon = group.append("polygon")
        .attr("points", points)
        .attr("fill", fillColor)

    group.append("text")
        .text(symbol)
        .attr("text-anchor", "middle")
        .attr("dy", ".35em")
        .style("font-size", "20px")
        .style("fill", textColor);
    return polygon;
}

function drawDirectedDisjunctionNode(group, _, isLeft, size = 30, strokeColor = "black") {
  // Diamond background
  const points = [
    [0, -size], [size, 0], [0, size], [-size, 0]
  ].map(p => p.join(",")).join(" ");

  group.append("polygon")
    .attr("points", points)
    .attr("fill", "#ff8000");

  const strokeWidth = 2;
  const arm = size * 0.28;      // half-length for plus arms (fits inside diamond)
  const arrowTipX = isLeft ? -arm : arm;

  // Vertical stroke of the plus (centered)
  group.append("line")
    .attr("x1", 0).attr("y1", -arm)
    .attr("x2", 0).attr("y2",  arm)
    .attr("stroke", strokeColor).attr("stroke-width", strokeWidth)
    .attr("stroke-linecap", "round");

  // Right horizontal arm (center to right)
  group.append("line")
    .attr("x1", 0).attr("y1", 0)
    .attr("x2", -arrowTipX).attr("y2", 0)
    .attr("stroke", strokeColor).attr("stroke-width", strokeWidth)
    .attr("stroke-linecap", "round");

  // group.append("line")
  //   .attr("x1", 0).attr("y1", 0)
  //   .attr("x2", -arm).attr("y2", 0)
  //   .attr("stroke", strokeColor).attr("stroke-width", strokeWidth)
  //   .attr("stroke-linecap", "round");

  // Top center -> left endpoint (diagonal)
  group.append("line")
    .attr("x1", 0).attr("y1", -arm)
    .attr("x2", arrowTipX).attr("y2", 0)
    .attr("stroke", strokeColor).attr("stroke-width", strokeWidth)
    .attr("stroke-linecap", "round")
    .attr("stroke-linejoin", "round");

  // Bottom center -> left endpoint (diagonal)
  group.append("line")
    .attr("x1", 0).attr("y1",  arm)
    .attr("x2", arrowTipX).attr("y2", 0)
    .attr("stroke", strokeColor).attr("stroke-width", strokeWidth)
    .attr("stroke-linecap", "round")
    .attr("stroke-linejoin", "round");

  return group;
}

function drawLeftDisjunctionNode(group) { return drawDirectedDisjunctionNode(group, null, true); }
function drawRightDisjunctionNode(group) { return drawDirectedDisjunctionNode(group, null, false); }
function drawConjunctionNode(group) { return drawPolygonNode(group, "blue", "×", "white"); }

function drawCircle(group, fill, text = "", textColor = "black", fontSize = "20px") {
    const radius = 25
    const circle = group.append("circle")
        .attr("r", radius)
        .attr("fill", fill)

    if (text) {
        group.append("text")
            .text(text)
            .attr("text-anchor", "middle")
            .attr("dy", ".35em")
            .style("font-size", fontSize)
            .style("fill", textColor);
    }
    return circle;
}

function drawGoalConjNode(group) { return drawCircle(group, "#57c4ff", "∧", "white"); }
function drawGoalDisjNode(group) { return drawCircle(group, "orange", "∨"); }
function drawGoalDelayNode(group) { return drawCircle(group, "#d9f2ff", "Zzz", "black", "12px"); }
function drawSucceedNode(group)  { return drawCircle(group, "green"); }
function drawFailNode(group)     { return drawCircle(group, "#ffdddd", "×"); }
function drawAnswerNode(group)   { return drawCircle(group, "green", "Answer", undefined, "10px") }
function drawEmptyNode(group)    { return drawCircle(group, "white") }
function drawCandidateNode(group) { return drawCircle(group, "#fff2cc", "Candidate", "black", "10px"); }
function drawOperation(group, data) { return drawTextNode(group, data.name, 12, "#edf2fa"); }
function drawDoneNode(group) { return drawCircle(group, "white", "Done", "black", "11px"); }
function drawSoloNode(group) { return drawCircle(group, "green", "Solo", undefined, "11px"); }
// Last belongs only to the earlier dormant-branch picture contract.
function drawLastNode(group) { return drawCircle(group, "#e3f2dd", "Last", "black", "11px"); }

function drawTextNode(group, textContent, padding = 10, fill = "lightgray") {
    const textElement = group.append("text")
        .text(textContent)
        .attr("text-anchor", "middle")
        .attr("dy", ".35em")
        .style("font-size", "14px");

    const textWidth = textElement.node().getBBox().width;

    const rect = group.append("rect")
        .attr("x", -textWidth / 2 - padding)
        .attr("y", -15)
        .attr("width", textWidth + 2 * padding)
        .attr("height", 30) 
        .style("fill", fill)

    textElement.raise();
    return rect;
}

function drawUnifyNode(group, data) {
    const textContent = `(== ${termToString(data.left)} ${termToString(data.right)})`;
    return drawTextNode(group, textContent);
}

function drawDisequalityNode(group, data) {
    const textContent = `(=/= ${termToString(data.left)} ${termToString(data.right)})`;
    return drawTextNode(group, textContent);
}

function drawFreshNode(group, data) {
    const varsText = data.vars ? data.vars.map(t => t.var).join(' ') : '';
    const textContent = `(fresh (${varsText}) ...)`;
    return drawTextNode(group, textContent, 10, "#f2f2f2");
}

function drawRelCallNode(group, data) {
  const rel = data?.rel || "call";
  const args = Array.isArray(data?.args) ? data.args : [];
  const argsText = args.map(t => (t && t.var) ? t.var : termToString(t)).join(' ');
  const textContent = `(${rel}${argsText ? ` ${argsText}` : ""})`;
  return drawTextNode(group, textContent);
}

function freshenedText(data) {
    const vars = Array.isArray(data?.vars) ? data.vars : [];
    return vars.map(v => termToString(v)).join(' ');
}

function drawFreshenedNode(group, data) {
    const varsText = freshenedText(data);
    const textContent = varsText ? `Freshened ${varsText}` : "Freshened";
    return drawTextNode(group, textContent, 12, "#d9e2f3");
}

function drawEmitNode(group) {
    return group.append("circle")
        .attr("r", 4)
        .attr("fill", "#666");
}

function drawForcedNode(group) {
    // "Deferred" is the friendly label shown in the tree UI.
    return drawCircle(group, "#fff2cc", "Deferred", "black", "10px");
}


function drawDelayNode(group) {
    const radius = 20; 
    const handLengthHour = radius * 0.5; 
    const handLengthMinute = radius * 0.8;

    // Draw the clock face (circle)
    group.append("circle")
        .attr("r", radius)
        .attr("fill", "white")
        .attr("stroke", "black")
        .attr("stroke-width", "2px");

    // Draw the hour hand
    group.append("line")
        .attr("x1", 0)
        .attr("y1", 0)
        .attr("x2", 0)
        .attr("y2", -handLengthHour)
        .attr("stroke", "black")
        .attr("stroke-width", "3px");

    // Draw the minute hand
    return group.append("line")
        .attr("x1", 0)
        .attr("y1", 0)
        .attr("x2", handLengthMinute * Math.cos(Math.PI / 4)) 
        .attr("y2", -handLengthMinute * Math.sin(Math.PI / 4))
        .attr("stroke", "black")
        .attr("stroke-width", "2px");
}

const nodeDrawFunctions = {
    "Answer": drawAnswerNode,
    "Candidate": drawCandidateNode,
    "Eval": drawOperation,
    "Work": drawOperation,
    "Returned": drawOperation,
    "Dead": drawOperation,
    "Mplus": drawOperation,
    "MplusR": drawOperation,
    "Bind": drawOperation,
    "One": drawOperation,
    "Yield": drawOperation,
    "YieldR": drawOperation,
    "Force": drawOperation,
    "Commit": drawOperation,
    "Advance": drawOperation,
    "Collect": drawOperation,
    "Render": drawOperation,
    "More": drawOperation,
    "Forced": drawOperation,
    "Done": drawDoneNode,
    "Solo": drawSoloNode,
    "Last": drawLastNode,
    "Succeed": drawSucceedNode,
    "Fail": drawFailNode,
    "Unify": drawUnifyNode,
    "Disequality": drawDisequalityNode,
    "<-+": drawLeftDisjunctionNode,
    "+->": drawRightDisjunctionNode,
    "Delay": drawDelayNode,
    "Conjunction": drawConjunctionNode,
    "Fresh": drawFreshNode,
    "Emit": drawEmitNode,
    "Freshened": drawFreshenedNode,
    "Deferred": drawForcedNode,
    "Rel-Call": drawRelCallNode,
    "Goal-Delay": drawGoalDelayNode,
    "Goal-Conj": drawGoalConjNode,
    "Goal-Disj": drawGoalDisjNode,
    "Empty": drawEmptyNode
};

export const DRAWABLE_NODE_NAMES = Object.freeze(Object.keys(nodeDrawFunctions));


export function drawTree(nodeGroups) {
    nodeGroups.each(function (d) {
        const group = d3.select(this);
        const data = d.data;
        if (SEMANTIC_STYLES[data.semanticKind]) {
            drawSemanticNode(group, data);
            return;
        }
        const drawFunction = nodeDrawFunctions[data.name];

        if (drawFunction) {
            const shape = drawFunction(group, data); 
            const hasSub = data.sub ? true : false;
            const isPartial = data.partial ? true : false;
            const hasAnswer = data.hasAnswer ? true : false;
            applyStroke(shape, hasSub, isPartial, hasAnswer);
        } else {
            console.error("Unknown tree node", data);
            drawTextNode(group, `Unknown: ${data?.name ?? "?"}`, 10);
        }
    });
}


export function drawNodes(container, nodes) {
    const nodeGroups = container.selectAll(".node")
    .data(nodes)
    .join("g")
    .attr("class", "node")
    .attr("transform", d => `translate(${d.x},${d.y})`);
    
    drawTree(nodeGroups); 
}


export function drawLinks(container, links) {
    container.selectAll(".link")
    .data(links)
    .join("path")
    .attr("class", "link")
    .attr("d", d => d3.linkVertical()({
        source: [d.source.x, d.source.y + d.source.data.measuredHeight / 2],
        target: [d.target.x, d.target.y - d.target.data.measuredHeight / 2],
    }))
    .attr("data-suspended", d => d.source.data.suspended ? "true" : null)
    .style("stroke-dasharray", d => d.source.data.suspended ? "4 5" : null)
    .style("stroke", d => (d.target.data.edgeColor ?? d.target.data.color ?? "#ccc"))
    .style("stroke-width", d => d.target.data.edgeColor ? 4 : 2);
}
