import React, { forwardRef, useImperativeHandle } from 'react';
import * as d3 from 'd3';
import { drawTree, drawLinks, drawNodes } from '../utils/drawing.js';
import { flattenGoalConj, addColors } from '../utils/treeSetup.js'

const TreeCanvas = forwardRef((_, ref) => {
    const svgRef = React.useRef();
    useImperativeHandle(ref, () => ({
        redraw: (treeData) => {
            const svg = d3.select(svgRef.current).html('').append('g');
            
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
                const padding = 20; 
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
    }));
    return <svg ref={svgRef}></svg>;
});

export default TreeCanvas;
