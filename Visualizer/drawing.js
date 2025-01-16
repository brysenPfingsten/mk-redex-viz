import * as d3 from 'https://cdn.jsdelivr.net/npm/d3@7/+esm';
export { drawTree }

function drawAnswerNode(group) {
    group.append("circle")
        .attr("r", 30)
        .attr("fill", "green")
        .attr("stroke", "yellow")
        .attr("stroke-width", "5px");

    group.append("text")
        .text("Answer")
        .attr("text-anchor", "middle")
        .attr("dy", ".35em")
        .style("font-size", "12px")
        .style("fill", "#000000");
}

function drawUnifyNode(group, data) {
    const unificationText = `(== ${data.left} ${data.right})`;

    // Add the text to the group first to measure its size
    const textElement = group.append("text")
        .text(unificationText)
        .style("font-size", "14px")
        .attr("text-anchor", "middle")
        .attr("dy", ".35em");

    const textWidth = textElement.node().getBBox().width; // Measure the text width
    const boxPadding = 10; // Padding around the text

    // Draw the rectangle with dynamic width based on the text size
    group.append("rect")
        .attr("width", textWidth + 2 * boxPadding)
        .attr("height", 30) // Fixed height
        .style("fill", "white") // Light gray background
        .style("stroke", "#000000") // Black border
        .style("stroke-width", 2)
        .attr("x", -(textWidth / 2 + boxPadding)) // Center horizontally
        .attr("y", -15); // Center vertically

    // Bring the text to the front
    textElement.raise();
}


function drawDisjunctionNode(group) {
    const size = 30; 


    const points = [
        [0, -size],       // Top point
        [size, 0],        // Right point
        [0, size],        // Bottom point
        [-size, 0]        // Left point
    ]
    .map(point => point.join(","))
    .join(" "); 

    // Draw the diamond 
    group.append("polygon")
        .attr("points", points)
        .attr("fill", "orange")
        .attr("stroke", "black")
        .attr("stroke-width", "2px");

    group.append("text")
        .text("+") 
        .attr("text-anchor", "middle")
        .attr("dy", ".35em") 
        .style("font-size", "20px")
        .style("fill", "black");
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

function drawConjunctionNode(group) {
    const size = 30; 

    const points = [
        [0, -size],       // Top point
        [size, 0],        // Right point
        [0, size],        // Bottom point
        [-size, 0]        // Left point
    ]
    .map(point => point.join(","))
    .join(" "); 

    // Draw the diamond 
    group.append("polygon")
        .attr("points", points)
        .attr("fill", "blue")
        .attr("stroke", "black")
        .attr("stroke-width", "2px");

    // Add the conjunction symbol (∧) at the center
    group.append("text")
        .text("×") 
        .attr("text-anchor", "middle")
        .attr("dy", ".35em") 
        .style("font-size", "20px")
        .style("fill", "white");
}

function drawFreshNode(group, d) {
    const padding = 5; // Padding around the text

    // Extract variables and subs from the data
    const varsText = d.vars ? d.vars.join(" ") : "";
    const textContent = `(fresh (${varsText}) ...)`;
    const subExists = d.sub ? true : false;

    // Add text first to measure its size
    const textElement = group.append("text")
        .text(textContent)
        .attr("text-anchor", "middle")
        .attr("dy", ".35em")
        .style("font-size", "14px");

    // Measure the text width dynamically
    const textWidth = textElement.node().getBBox().width;

    // Draw the rectangle with dynamic width
    group.append("rect")
        .attr("x", -textWidth / 2 - padding)
        .attr("y", -15) // Set height dynamically or fixed
        .attr("width", textWidth + 2 * padding)
        .attr("height", 30) // Adjust height as needed
        .style("fill", "white")
        .style("stroke", subExists ? "yellow" : "black")
        .style("stroke-width", "4px");

    // Move the text to the front (since the rectangle is added after)
    textElement.raise();
}

function drawRelCallNode(group, d) {
    const padding = 10; // Padding around the text

    // Extract `rel` and `args` from the data
    const rel = d.rel || ""; // Default if `d.rel` is undefined
    const argsText = d.args ? d.args.join(" ") : "";
    const textContent = `(${rel} ${argsText})`;

    // Add the text first to measure its size
    const textElement = group.append("text")
        .text(textContent)
        .attr("text-anchor", "middle")
        .attr("dy", ".35em")
        .style("font-size", "14px");

    // Measure the text width dynamically
    const textWidth = textElement.node().getBBox().width;

    // Draw the rectangle with dynamic width
    group.append("rect")
        .attr("x", -textWidth / 2 - padding)
        .attr("y", -20) 
        .attr("width", textWidth + 2 * padding)
        .attr("height", 40)
        .style("fill", "white")
        .style("stroke", "black")
        .style("stroke-width", d.sub ? "12px" : "2px");

    // Move the text to the front (since the rectangle is added after)
    textElement.raise();
}

function drawGoalConjNode(group, d) {
    const radius = 25; 
    const hasSub = d.sub;

    // Draw the circle
    group.append("circle")
        .attr("r", radius)
        .attr("fill", "purple") 
        .attr("stroke", hasSub? "yellow" :"black")
        .attr("stroke-width", hasSub? "6px": "2px");

    // Add the AND symbol
    group.append("text")
        .text("∧") // Logical AND symbol
        .attr("text-anchor", "middle")
        .attr("dy", ".35em") 
        .style("font-size", "20px")
        .style("fill", "white"); 
}

function drawGoalDisjNode(group, d) {
    const radius = 25; 
    const hasSub = d.sub;

    // Draw the circle
    group.append("circle")
        .attr("r", radius)
        .attr("fill", "#FF69B4")
        .attr("stroke", hasSub? "yellow" : "black")
        .attr("stroke-width", hasSub? "6px" :  "2px");

    // Add the OR symbol
    group.append("text")
        .text("∨") // Logical OR symbol
        .attr("text-anchor", "middle")
        .attr("dy", ".35em") 
        .style("font-size", "20px")
        .style("fill", "black"); 
}

function drawEmptyNode(group) {
    const radius = 25;

    // Draw white circle
    group.append("circle")
    .attr("r", radius)
    .style("fill", "white")
    .style("stroke", "black")
    .style("stroke-width", "2px");
}

function drawTree(nodeGroups) {
    console.log("here");
    nodeGroups.each(function (d) {
        const group = d3.select(this);
        const { name } = d.data;

        if (name === "Answer") {
            drawAnswerNode(group);
        } else if (name === "Unify") {
            drawUnifyNode(group, d.data);
        } else if (["Disjunction", "<-+", "+->"].includes(name)) {
            drawDisjunctionNode(group);
        } else if (name === "Delay") {
            drawDelayNode(group);
        } else if (name === "Conjunction") {
            drawConjunctionNode(group);
        } else if (name === "Fresh") {
            drawFreshNode(group, d.data);
        } else if (name === "Rel-Call") {
            drawRelCallNode(group, d.data);
        } else if (name === "Goal-Conj") {
            drawGoalConjNode(group, d.data);
        } else if (name === "Goal-Disj") {
            drawGoalDisjNode(group, d.data)
        } else if (name === "Empty") {
            drawEmptyNode(group);
        }
    });
} 