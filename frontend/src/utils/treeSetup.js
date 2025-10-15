export function addColors(tree) {

    const ACTIVE_INDEX = { Conjunction: 0, "<-+": 0, "+->": 1 };
    const TERMINALS1   = new Set(["Answer", "Succeed", "Empty", "Delay"]);
    const TERMINALS2   = new Set(["Answer", "Succeed"]);
    const DISJ         = new Set(["<-+", "+->"]);

    const activeChild = n => {
        if (!n) return null;
        const idx = ACTIVE_INDEX[n.name];
        return idx == null ? null : (n.children?.[idx] ?? null);
    };

    function stopColoringHere(node) {
        // Depth 0 check
        // Goal-states should not be colored
        if (node.sub && node.name !== "Answer") return true;

        // Depth 1 check
        // Disjunctions and conjunctions with answers, empty, or delays
        // Excepting disjunctions with conjunctions in their active position
        const d1 = activeChild(node);
        if (DISJ.has(node?.name) && d1?.name === "Conjunction") return false; 
        if (d1 && TERMINALS1.has(d1.name)) return true;

        // Depth 2 check
        // Combinations of disjunctions and conjunctions with answers in the active position
        const d2 = activeChild(d1);
        return !!(d2 && TERMINALS2.has(d2.name));
    }

    if (stopColoringHere(tree)) return tree;
    if (tree.partial) return tree;

    let children = tree.children;

    switch (tree.name) {
        case "<-+":
            children[0].color = "#ff8000";
            addColors(children[0]);
            break;
        case "+->":
            children[1].color = "#ff8000";
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
        case "Delay":
            return tree;
        case "Answer":
            if (children) {
                children[0].color = "green";
                addColors(children[0]);
            }
            break;
        default: return tree;
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
