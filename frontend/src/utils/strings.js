function arrayToString(arr) {
    if (!Array.isArray(arr) || arr.length === 0) return "'()";

    const terms = arr.map(t => termToString(t)).join(' ');
    return `(list ${terms})`

    /** 
    if (arr.length === 1) return `(${termToString(arr[0])})`;

    const allButLast = arr.slice(0, -1).map(item =>
        termToString(item)
    ).join(" ")
    const last = termToString(arr[arr.length - 1]);

    return `\`(${allButLast} . ${last})`;
    */
    
}

function nonListTermToString(t) {
    if (t.var) { return `${t.var}`; }
    if (t.pair) {
        return `(${termToString(t.pair[0])} . ${termToString(t.pair[1])})`;
    }
    if (t.sym) { return `'${t.sym}`; }
    if (typeof t === "object" && "num" in t) { return `${t.num}`; }
    if (Array.isArray(t)) { return arrayToString(t); }
    if (t === 'empty') { return "'()"; }
    if (typeof t === "string" && !t.includes('_.')) { return `"${t}"`; }
    if (typeof t === "number") { return `#(${t})`; }
    return t;
}

export function termToString(term) {
    if (term.var) { return `${term.var}`; }   
    if (term.pair) { return `(${termToString(term.pair[0])} . ${termToString(term.pair[1])})`; }
    if (term.sym) { return `'${term.sym}`; } 
    if (typeof term === "object" && "num" in term) { return `${term.num}`; }
    if (Array.isArray(term)) { return arrayToString(term); }
    if (term === 'empty') { return "'()"}
    if (typeof term === "string" && !term.includes('_.')) { return `"${term}"`}
    if (typeof term === "number") { return `#(${term})`}
    return term
}

function subToString(sub) {
    return sub ? sub.map(({ key, value }) => `${key} => ${termToString(value)}`).join("\n") : "\n";
}

function trailToString(trail) {
    return trail ? trail.map(crumb => `(== ${termToString(crumb.left)} ${termToString(crumb.right)})`).join("\n") : "\n";
}

function disequalitiesToString(disequalities) {
    return disequalities
        ? disequalities.map(clause => `(=/= ${termToString(clause.left)} ${termToString(clause.right)})`).join("\n")
        : "\n";
}

function reificationToString(reification) {
    if (!reification) { return ''; }
    return termToString(reification)
}

export function toString(sub, trail, reification, disequalities = []) {
    return `Substitutions:\n${subToString(sub)}\nTrail:\n${trailToString(trail)}\nDisequalities:\n${disequalitiesToString(disequalities)}\nCurrent Answer:\n${reificationToString(reification)}`
}
