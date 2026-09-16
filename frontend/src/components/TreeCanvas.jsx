import React, { useRef, useEffect, useState, forwardRef, useImperativeHandle } from 'react';
import * as d3 from 'd3';
import { drawTree, drawLinks, drawNodes } from '../utils/drawing.js';
import { termToString } from '../utils/strings.js';
import { addColors } from '../utils/treeSetup.js'
import { goalIdFromTreeNodeData, stateKeyFromTreeNodeData } from '../utils/source_mapping.js';

const TreeCanvas = forwardRef(({ onNodeClick, selectedGoalId }, ref) => {
    const svgRef = useRef();
    const [tooltip, setTooltip] = useState({ visible: false, x: 0, y: 0, content: "" });

    const clearHighlights = (selection = d3.select(svgRef.current)) => {
        selection.selectAll('g.node')
            .select('circle, rect, polygon')
            .classed('highlighted', false);
        selection.selectAll('.owner-group rect').classed('highlighted', false);
    };

    const applyGoalHighlights = (goalId, selection = d3.select(svgRef.current)) => {
        clearHighlights(selection);
        if (goalId == null) return;
        selection.selectAll('g.node')
            .filter(d => d.data.id === goalId)
            .select('circle, rect, polygon')
            .classed('highlighted', true);
        selection.selectAll('.owner-group')
            .filter(owner => owner.sourceId === goalId)
            .select('rect').classed('highlighted', true);
    };

    const nodePayload = (d) => {
        const subs = (d.data.sub || []).map(s => ({
            left: termToString(s.key),
            right: termToString(s.value)
        }));
        const trails = (d.data.trail || []).map(crumb => ({
            left: termToString(crumb.left),
            right: termToString(crumb.right),
        }));

        return {
            substitutionData: subs,
            trailData: trails,
            gId: goalIdFromTreeNodeData(d.data),
            sId: stateKeyFromTreeNodeData(d.data),
        };
    };

    useEffect(() => {
        applyGoalHighlights(selectedGoalId);
    }, [selectedGoalId]);
    
    useImperativeHandle(ref, () => ({
        updateSidebar: (sId) => {
            if (sId == null) return;
            const nodeSel = d3.select(svgRef.current)
                .selectAll('g.node')
                .filter(d => stateKeyFromTreeNodeData(d?.data) === sId);

            if (nodeSel.empty()) return;

            // Prefer a node with concrete state payload when multiple nodes share stateId.
            const richNodeSel = nodeSel.filter(d =>
                (Array.isArray(d?.data?.sub) && d.data.sub.length > 0) ||
                (Array.isArray(d?.data?.trail) && d.data.trail.length > 0) ||
                d?.data?.reified !== undefined
            );

            const target = (richNodeSel.empty() ? nodeSel : richNodeSel).node();
            if (!target) return;

            const datum = d3.select(target).datum();
            onNodeClick(nodePayload(datum));
        },
        redraw: (treeData) => {
            const svg = d3.select(svgRef.current).html('');
            const g = svg.append('g');
            
            // Create hierarchy and prepare data
            const root = d3.hierarchy(addColors(treeData));
            
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
                d.data.measuredBounds = { x: bbox.x, y: bbox.y, width: bbox.width, height: bbox.height };
            });
            tempSvg.remove();
            
            // Configure tree layout with dynamic spacing
            const treeLayout = d3.tree()
            .nodeSize([1, 100]) // Base horizontal unit, vertical spacing
            .separation((a, b) => {
                const padding = 20; 
                if (a.parent === b.parent) return (a.data.measuredWidth + b.data.measuredWidth) / 2 + padding;
                else return (a.data.measuredWidth + b.data.measuredWidth) / 2 + padding + 100;
            });
            
            // Compute the layout with adjusted spacing
            treeLayout(root);

            // Owner groups increase a card's height without increasing tree
            // depth. Leave enough space at every level for all those groups.
            const levelHeights = [];
            root.each(d => {
                levelHeights[d.depth] = Math.max(levelHeights[d.depth] ?? 0, d.data.measuredHeight);
            });
            const levelY = [0];
            for (let depth = 1; depth < levelHeights.length; depth += 1) {
                levelY[depth] = levelY[depth - 1] + levelHeights[depth - 1] / 2
                    + levelHeights[depth] / 2 + 56;
            }
            root.each(d => { d.y = levelY[d.depth]; });
            
            // Calculate dimensions after layout
            const nodes = root.descendants();
            const links = root.links();

            // 1. Calculate true bounding box including node sizes
            const trueMinX = Math.min(...nodes.map(d => d.x + d.data.measuredBounds.x));
            const trueMaxX = Math.max(...nodes.map(d => d.x + d.data.measuredBounds.x + d.data.measuredWidth));
            const trueMinY = Math.min(...nodes.map(d => d.y + d.data.measuredBounds.y));
            const trueMaxY = Math.max(...nodes.map(d => d.y + d.data.measuredBounds.y + d.data.measuredHeight));

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

            // Draw elements
            drawLinks(g, links);
            drawNodes(g, nodes);
            applyGoalHighlights(selectedGoalId, g);

            // Add click event to show state data
            g.selectAll('g.node')
            .filter(d => d.data.id || d.data.sub || d.data.trail || d.data.reified)
            .on("click", (event, d) => {
                onNodeClick(nodePayload(d));
            })
            .on("mouseover", (event, d) => {
                if (d.data.reified) {
                setTooltip({
                    visible: true,
                    x: event.clientX,
                    y: event.clientY,
                    content: termToString(d.data.reified)
                });
                }
            })
            .on("mouseleave", () => {
                setTooltip(prev => ({ ...prev, visible: false }));
            });

            const selectOwner = (event, owner) => {
                event.stopPropagation();
                onNodeClick({ gId: owner.sourceId, sId: null });
            };
            g.selectAll('.owner-group')
                .on('click', event => event.stopPropagation())
                .filter(owner => typeof owner.sourceId === 'string' && !owner.sourceId.startsWith('hidden:'))
                .attr('role', 'button').attr('tabindex', 0)
                .attr('aria-label', owner => `Show introduction source ${owner.sourceId}`)
                .on('click', selectOwner)
                .on('keydown', (event, owner) => {
                    if (event.key === 'Enter' || event.key === ' ') {
                        event.preventDefault();
                        selectOwner(event, owner);
                    }
                });
        }
    }));
    return (
        <>
            <svg ref={svgRef} role="img" aria-label="Search configuration with introductions on their owning nodes" />
            {tooltip.visible && (
                <div className="reified-tooltip"
                    style={{
                        position: "fixed",
                        left: tooltip.x,
                        top: tooltip.y,
                        zIndex: 1000,
                        pointerEvents: "none"
                    }}
                >{tooltip.content}</div>
            )}
        </>
    );
});

export default TreeCanvas;
