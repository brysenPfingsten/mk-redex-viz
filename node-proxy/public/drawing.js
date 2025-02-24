import * as d3 from 'https://cdn.jsdelivr.net/npm/d3@7/+esm';
import { termToString } from './strings.js';
export { drawTree }

function drawPolygonNode(group, fillColor, symbol, textColor = "black") {
    const size = 30;
    const points = [
        [0, -size], [size, 0], [0, size], [-size, 0]
    ].map(point => point.join(",")).join(" ");

    group.append("polygon")
        .attr("points", points)
        .attr("fill", fillColor)
        .attr("stroke", "black")
        .attr("stroke-width", "2px");

    group.append("text")
        .text(symbol)
        .attr("text-anchor", "middle")
        .attr("dy", ".35em")
        .style("font-size", "20px")
        .style("fill", textColor);
}

function drawDisjunctionNode(group) { drawPolygonNode(group, "orange", "+"); }
function drawConjunctionNode(group) { drawPolygonNode(group, "blue", "×", "white"); }

function drawCircle(group, fill, hasSub, text = "", textColor = "black", fontSize = "20px") {
    const radius = 25
    group.append("circle")
        .attr("r", radius)
        .attr("fill", fill)
        .attr("stroke", hasSub ? "yellow" : "black")
        .attr("stroke-width", hasSub ? "6px" : "2px");

    if (text) {
        group.append("text")
            .text(text)
            .attr("text-anchor", "middle")
            .attr("dy", ".35em")
            .style("font-size", fontSize)
            .style("fill", textColor);
    }
}

function drawGoalConjNode(group, d) { drawCircle(group, "purple", d.sub, "∧", "white"); }
function drawGoalDisjNode(group, d) { drawCircle(group, "#FF69B4", d.sub, "∨"); }
function drawSucceedNode(group, d)  { drawCircle(group, "green", d.sub); }
function drawAnswerNode(group, d)   { drawCircle(group, "green", d.sub, "Answer", undefined, "10px") }
function drawEmptyNode(group, d)    { drawCircle(group, "white", d.sub) }

function drawTextNode(group, textContent, hasSub, padding = 10) {
    const textElement = group.append("text")
        .text(textContent)
        .attr("text-anchor", "middle")
        .attr("dy", ".35em")
        .style("font-size", "14px");

    const textWidth = textElement.node().getBBox().width;

    group.append("rect")
        .attr("x", -textWidth / 2 - padding)
        .attr("y", -15)
        .attr("width", textWidth + 2 * padding)
        .attr("height", 30) 
        .style("fill", "white")
        .style("stroke", hasSub ? "yellow" : "black")
        .style("stroke-width", hasSub ? "6px" : "2px");

    textElement.raise();
}

function drawUnifyNode(group, data) {
    const textContent = `(== ${termToString(data.left)} ${termToString(data.right)})`;
    drawTextNode(group, textContent, data.sub);
}

function drawFreshNode(group, data) {
    const varsText = data.vars ? data.vars.map(t => t.var).join(' ') : '';
    const textContent = `(fresh (${varsText}) ...)`;
    drawTextNode(group, textContent, data.sub);
}

function drawRelCallNode(group, data) {
    const argsText = data.args ? data.args.map(t => t.var ? t.var : termToString(t)).join(' ') : '';
    const textContent = `(${data.rel} ${argsText})`;
    drawTextNode(group, textContent, data.sub);
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
    group.append("line")
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
    "Disjunction": drawDisjunctionNode,
    "<-+": drawDisjunctionNode,
    "+->": drawDisjunctionNode,
    "Delay": drawDelayNode,
    "Conjunction": drawConjunctionNode,
    "Fresh": drawFreshNode,
    "Rel-Call": drawRelCallNode,
    "Goal-Conj": drawGoalConjNode,
    "Goal-Disj": drawGoalDisjNode,
    "Empty": drawEmptyNode
};


function drawTree(nodeGroups) {
    nodeGroups.each(function (d) {
        const group = d3.select(this);
        const data = d.data
        const drawFunction = nodeDrawFunctions[data.name];

        if (drawFunction) {
            drawFunction(group, data);
        }
    });
}