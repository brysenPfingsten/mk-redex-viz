function arrayToString(arr) {
    if (!Array.isArray(arr) || arr.length === 0) return "'()";

    if (arr.length === 1) return `(${termToString(arr[0])})`;

    const allButLast = arr.slice(0, -1).map(item =>
        termToString(item)
    ).join(" ")
    const last = termToString(arr[arr.length - 1]);

    return `\`(${allButLast} . ${last})`;
}

export function termToString(term) {
    if (typeof term === "object" && term !== null && term.var) {
        return `,${term.var}`;
    }    
    if (Array.isArray(term)) { return arrayToString(term); }
    if (term === 'empty') { return "'()"}
    if (typeof term === "string" && !term.includes('_.')) { return `"${term}"`}
    if (typeof term === "number") { return `${term}`}
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

export function toString(sub, trail, reification) {
    return `Substitutions:\n${subToString(sub)}\nTrail:\n${trailToString(trail)}\nCurrent Answer:\n${reificationToString(reification)}`
}