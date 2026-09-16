import React, { useState } from 'react';
import { Drawer, Tabs, Tab, Box, IconButton } from '@mui/material';
import { Menu, Close } from '@mui/icons-material';
import '../styles.css';

const Sidebar = ({
  substitutionData = [],
  trailData = [],
  hasStateSelection = false,
  isOpen,
  onToggle,
}) => {
  const [activeTab, setActiveTab] = useState(0);
  const rows = activeTab === 0 ? substitutionData : trailData;
  const emptyMessage = hasStateSelection
    ? `No ${activeTab === 0 ? 'substitution' : 'trail'} entries for this state.`
    : 'Select a Solo, Answer, candidate, or active goal to inspect its state.';

  return (
    <>
      <IconButton
        onClick={onToggle}
        className="toggle-button"
      >
        {isOpen ? <Close /> : <Menu />}
      </IconButton>


      <Drawer
        variant="persistent"
        anchor="right"
        open={isOpen}
        sx={{
          width: isOpen ? 'var(--sidebar-width)' : 0,
          transition: 'width 225ms cubic-bezier(0.4, 0, 0.6, 1)',
          flexShrink: 0,
          '& .MuiDrawer-paper': {
            width: 'var(--sidebar-width)',
            boxSizing: 'border-box',
            position: 'relative',
          },
        }}
      >
        <Box className="sidebar-content">
          <Tabs
            value={activeTab}
            onChange={(e, v) => setActiveTab(v)}
            variant="fullWidth"
            className="sidebar-tabs"
          >
            <Tab label="Substitution" />
            <Tab label="Trail" />
          </Tabs>

          <Box className="sidebar-list">
            {rows.length === 0 ? (
              <Box className="sidebar-empty" role="status">
                {emptyMessage}
              </Box>
            ) : (
              rows.map((row, i) => (
                <Box key={i} className="sidebar-item">
                  <Box className="sidebar-left">{row.left}</Box>
                  <Box className="sidebar-right">{row.right}</Box>
                </Box>
              ))
            )}
          </Box>
        </Box>
      </Drawer>
    </>
  );
};

export default Sidebar;
