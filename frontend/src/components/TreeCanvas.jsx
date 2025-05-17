import React, { useRef, useEffect, forwardRef, useImperativeHandle } from 'react';
import * as d3 from 'd3';
import { drawTree, drawLinks, drawNodes } from '../utils/drawing.js';
import { termToString } from '../utils/strings.js';
import { flattenGoalConj, addColors } from '../utils/treeSetup.js'

const TreeCanvas = forwardRef(({ onNodeClick, selectedId }, ref) => {
    const svgRef = useRef();

    useEffect(() => {
        const svg = d3.select(svgRef.current);

        // clear any old highlight
        svg.selectAll('g.node')
            .select('circle, rect, polygon')
            .classed('highlighted', false);

        if (selectedId) {
            svg.selectAll('g.node')
            .filter(d => d.data.id === selectedId)
            .select('circle, rect, polygon')
            .classed('highlighted', true);
        }
        }, [selectedId]);

    useImperativeHandle(ref, () => ({
        redraw: (treeData) => {
            const svg = d3.select(svgRef.current).html('');
            const g = svg.append('g');
            
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
            
            // Calculate dimensions after layout
            const nodes = root.descendants();
            const links = root.links();

            // 1. Calculate true bounding box including node sizes
            const trueMinX = Math.min(...nodes.map(d => d.x - d.data.measuredWidth/2));
            const trueMaxX = Math.max(...nodes.map(d => d.x + d.data.measuredWidth/2));
            const trueMinY = Math.min(...nodes.map(d => d.y));
            const trueMaxY = Math.max(...nodes.map(d => d.y + d.data.measuredHeight));

            // 2. Calculate required dimensions
            const padding = 50;
            const contentWidth = trueMaxX - trueMinX;
            const contentHeight = trueMaxY - trueMinY;
            const svgWidth = contentWidth + padding * 2;
            const svgHeight = contentHeight + padding * 2;

            // 3. Set SVG dimensions to contain entire tree
            d3.select(svgRef.current)
                .attr("width", svgWidth)
                .attr("height", svgHeight)
                .attr("viewBox", `${trueMinX - padding} ${trueMinY - padding} ${svgWidth} ${svgHeight}`)
                .style("overflow", "visible");

            // 4. Calculate centering translation
            const rootCenterX = root.x;
            const svgCenterX = (trueMinX - padding) + svgWidth/2;
            const translateX = svgCenterX - rootCenterX;

            // 5. Apply translation to the <g> element
            g.attr("transform", `translate(${translateX},0)`);

            // Draw elements
            drawLinks(svg, links);
            drawNodes(svg, nodes);

            // Add click event to show state data
            svg.selectAll('g.node')
            .filter(d => d.data.sub || d.data.trail || d.data.id)
            .on('click', (event, d) => {
                const subs = (d.data.sub || []).map(s => ({ 
                    left: termToString(s.key), 
                    right: termToString(s.value) 
                }));
                const trails = (d.data.trail || []).map(crumb => ({
                    left: termToString(crumb.left),
                    right: termToString(crumb.right),
                }));
                const id = d.data.id || null;
                onNodeClick({ substitutionData: subs, trailData: trails, id: id });
            });
        }
    }));
    return <svg ref={svgRef} ></svg>;
});

export default TreeCanvas;
