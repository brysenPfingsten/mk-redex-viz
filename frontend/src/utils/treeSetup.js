

export function addColors(tree) {
    if (tree.sub && tree.name != 'Answer') return tree;

    let children = tree.children;

    switch (tree.name) {
        case "<-+":
            children[0].color = "orange";
            addColors(children[0]);
            break;
        case "+->":
            children[1].color = "orange";
            addColors(children[1]);
            break;
        case "Disjunction":
            children[0].color = "#FFA500";
            addColors(children[0]);
            break;
        case "Conjunction":
            children[0].color = "blue";
            addColors(children[0]);
            break;
        case "Fresh":
            children[0].color = "red";
            addColors(children[0]);
            break;
        case "Goal-Conj":
            children[0].color = "purple";
            addColors(children[0]);
            break;
        case "Goal-Disj":
            children[0].color = "#FF69B4";
            addColors(children[0]);
            break;
        case "Delay":
            children[0].color = "black";
            addColors(children[0]);
            break;
        case "Answer":
            children[0].color = "green";
            addColors(children[0]);
            break;
    }

    return tree;
}


export function flattenGoalConj(tree) {
    // Helper function to recursively flatten `Goal-Conj`
    function processNode(node) {
        // Check if the current node is a `Goal-Conj`
        if (node.name === "Goal-Conj" && Array.isArray(node.children)) {
            // Collect children that are also `Goal-Conj`
            let flattenedChildren = [];
            for (let child of node.children) {
                if (child.name === "Goal-Conj" && Array.isArray(child.children)) {
                    // Merge the child's children into `flattenedChildren`
                    flattenedChildren.push(...child.children);
                } else {
                    // Add non-Goal-Conj children directly
                    flattenedChildren.push(child);
                }
            }
            // Update this node's children with the flattened children
            node.children = flattenedChildren;
        }
        
        // Recursively process all children
        if (Array.isArray(node.children)) {
            for (let child of node.children) {
                processNode(child);
            }
        }
    }
    
    // Start processing the tree
    processNode(tree);
    return tree; // Return the modified tree
}