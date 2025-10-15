import * as d3 from 'd3';
import { termToString } from './strings.js';


function applyStroke(selection, hasSub, isProceed, isPartial, hasAnswer) {
    if (hasSub && !isPartial) {
        selection
            .clone(true)
            .lower()
            .attr("stroke", isProceed ? "green" : "yellow")
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

function drawGoalConjNode(group, _) { return drawCircle(group, "#57c4ff", "∧", "white"); }
function drawGoalDisjNode(group, _) { return drawCircle(group, "orange", "∨"); }
function drawSucceedNode(group, _)  { return drawCircle(group, "green"); }
function drawAnswerNode(group, _)   { return drawCircle(group, "green", "Answer", undefined, "10px") }
function drawEmptyNode(group, _)    { return drawCircle(group, "white") }

function drawTextNode(group, textContent, padding = 10, outline="") {
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
        .style("fill", "lightgray")

    textElement.raise();
    return rect;
}

function drawProceedNode(group, data) {
    const argsText = data.goal.args ? data.goal.args.map(t => t.var ? t.var : termToString(t)).join(' ') : '';
    const textContent = `(${data.goal.rel} ${argsText})`;
    return drawTextNode(group, textContent, 10, "green");
}

function drawUnifyNode(group, data) {
    const textContent = `(== ${termToString(data.left)} ${termToString(data.right)})`;
    return drawTextNode(group, textContent);
}

function drawFreshNode(group, data) {
    const varsText = data.vars ? data.vars.map(t => t.var).join(' ') : '';
    const textContent = `(fresh (${varsText}) ...)`;
    return drawTextNode(group, textContent);
}

function drawRelCallNode(group, data) {
    const argsText = data.args ? data.args.map(t => t.var ? t.var : termToString(t)).join(' ') : '';
    const textContent = `(${data.rel} ${argsText})`;
    return drawTextNode(group, textContent);
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
    "Succeed": drawSucceedNode,
    "Unify": drawUnifyNode,
    "<-+": drawLeftDisjunctionNode,
    "+->": drawRightDisjunctionNode,
    "Delay": drawDelayNode,
    "Conjunction": drawConjunctionNode,
    "Fresh": drawFreshNode,
    "Rel-Call": drawRelCallNode,
    "Proceed": drawProceedNode,
    "Goal-Conj": drawGoalConjNode,
    "Goal-Disj": drawGoalDisjNode,
    "Empty": drawEmptyNode
};


export function drawTree(nodeGroups) {
    nodeGroups.each(function (d) {
        const group = d3.select(this);
        const data = d.data;
        const drawFunction = nodeDrawFunctions[data.name];

        if (drawFunction) {
            const shape = drawFunction(group, data); 
            const hasSub = data.sub ? true : false;
            const isProceed = data.name === "Proceed";
            const isPartial = data.partial ? true : false;
            const hasAnswer = data.hasAnswer ? true : false;
            applyStroke(shape, hasSub, isProceed, isPartial, hasAnswer);
        }
    });
}


export function drawNodes(svg, nodes) {
    const nodeGroups = svg.selectAll(".node")
    .data(nodes)
    .join("g")
    .attr("class", "node")
    .attr("transform", d => `translate(${d.x},${d.y})`);
    
    drawTree(nodeGroups); 
}


export function drawLinks(svg, links) {
    svg.selectAll(".link")
    .data(links)
    .join("path")
    .attr("class", "link")
    .attr("d", d3.linkVertical().x(d => d.x).y(d => d.y))
    .style("stroke", d => (d.target.data.color ? d.target.data.color : "#ccc"))
    .style("stroke-width", 4);
}
