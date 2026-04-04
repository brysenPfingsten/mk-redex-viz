import React, { useRef, useEffect, useState, forwardRef, useImperativeHandle } from 'react';
import * as d3 from 'd3';
import { drawTree, drawLinks, drawNodes } from '../utils/drawing.js';
import { termToString } from '../utils/strings.js';
import { flattenGoalConj, addColors } from '../utils/treeSetup.js'

const TreeCanvas = forwardRef(({ onNodeClick, selectedGoalId, selectedStateId }, ref) => {
    const svgRef = useRef();
    const containerRef = useRef();
    const [tooltip, setTooltip] = useState({ visible: false, x: 0, y: 0, content: "" });
    const [panOffset, setPanOffset] = useState({ x: 0, y: 0 });
    const [isDragging, setIsDragging] = useState(false);
    const dragStartRef = useRef({ x: 0, y: 0 });
    const baseTranslateXRef = useRef(0);

    const goalIdRef = useRef(selectedGoalId);
    const stateIdRef = useRef(selectedStateId);

    useEffect(() => {
        goalIdRef.current = selectedGoalId;
        stateIdRef.current = selectedStateId;
    }, [selectedGoalId, selectedStateId]);

    const handleMouseDown = (e) => {
        if (e.button !== 0) return; // Only left mouse button
        setIsDragging(true);
        dragStartRef.current = { x: e.clientX, y: e.clientY };
        if (containerRef.current) {
            containerRef.current.classList.add('dragging');
        }
    };

    const handleMouseMove = (e) => {
        if (!isDragging) return;
        const dx = e.clientX - dragStartRef.current.x;
        const dy = e.clientY - dragStartRef.current.y;
        setPanOffset(prev => ({
            x: prev.x + dx,
            y: prev.y + dy
        }));
        dragStartRef.current = { x: e.clientX, y: e.clientY };
    };

    const handleMouseUp = () => {
        setIsDragging(false);
        if (containerRef.current) {
            containerRef.current.classList.remove('dragging');
        }
    };

    useEffect(() => {
        if (isDragging) {
            document.addEventListener('mousemove', handleMouseMove);
            document.addEventListener('mouseup', handleMouseUp);
            return () => {
                document.removeEventListener('mousemove', handleMouseMove);
                document.removeEventListener('mouseup', handleMouseUp);
            };
        }
    }, [isDragging]);

    // Update transform when pan offset changes
    useEffect(() => {
        const svg = d3.select(svgRef.current);
        const g = svg.select('g');
        if (!g.empty()) {
            g.attr('transform', `translate(${baseTranslateXRef.current + panOffset.x},${panOffset.y})`);
        }
    }, [panOffset]);

    const clearHighlights = () => {
        const svg = d3.select(svgRef.current);
        svg.selectAll('g.node')
            .select('circle, rect, polygon')
            .classed('highlighted', false);
    }

    useEffect(() => {
        const svg = d3.select(svgRef.current);

        clearHighlights();

        if (selectedGoalId) {
            svg.selectAll('g.node')
            .filter(d => d.data.id === selectedGoalId)
            .select('circle, rect, polygon')
            .classed('highlighted', true);
        }
    }, [selectedGoalId]);
    
    useImperativeHandle(ref, () => ({
        updateSidebar(sId) {
        d3.select(svgRef.current)
            .selectAll('g.node')
            .filter(d => d.data.stateId === sId)
            .dispatch('click');
        }
    }));

    useImperativeHandle(ref, () => ({
        updateSidebar: (sId) => {
        d3.select(svgRef.current)
            .selectAll('g.node')
            .filter(d => d.data.stateId === sId)
            .dispatch('click');
        },
        redraw: (treeData) => {
            // Reset pan offset when redrawing tree
            setPanOffset({ x: 0, y: 0 });

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

            // Store base translation for pan calculations
            baseTranslateXRef.current = translateX;

            // 5. Apply translation to the <g> element
            g.attr("transform", `translate(${translateX},0)`);

            // Draw elements
            drawLinks(svg, links);
            drawNodes(svg, nodes);

            // Add click event to show state data
            svg.selectAll('g.node')
            .filter(d => d.data.id || d.data.sub || d.data.trail || d.data.reified)
            .on("click", (event, d) => {
                const subs = (d.data.sub || []).map(s => ({
                    left: termToString(s.key),
                    right: termToString(s.value)
                }));
                const trails = (d.data.trail || []).map(crumb => ({
                    left: termToString(crumb.left),
                    right: termToString(crumb.right),
                }));

                let sId = d.data.stateId;
                let gId = d.data.id;
                const prevGoalId = goalIdRef.current;
                const prevStateId = stateIdRef.current;

                if (event.isTrusted && gId === prevGoalId) {
                    sId = null;
                    gId = null;
                }
                onNodeClick({ substitutionData: subs, trailData: trails, gId: gId, sId: sId });
            })
            .on("mouseover", (event, d) => {
                if (d.data.reified) {
                setTooltip({
                    visible: true,
                    x: event.clientX,
                    y: event.clientY,
                    content: termToString(d.data.reified).replace(/\n/g, "<br>")
                });
                }
            })
            .on("mouseleave", () => {
                setTooltip(prev => ({ ...prev, visible: false }));
            });
        }
    }));
    return (
        <div
            ref={containerRef}
            style={{
                display: 'inline-block',
                cursor: isDragging ? 'grabbing' : 'grab'
            }}
            onMouseDown={handleMouseDown}
        >
            <svg ref={svgRef} style={{ userSelect: 'none', display: 'block' }} />
            {tooltip.visible && (
                <div className="reified-tooltip"
                    style={{
                        position: "fixed",
                        left: tooltip.x,
                        top: tooltip.y,
                        zIndex: 1000,
                        pointerEvents: "none"
                    }}
                    dangerouslySetInnerHTML={{ __html: tooltip.content }}
                />
            )}
        </div>
    );
});

export default TreeCanvas;
