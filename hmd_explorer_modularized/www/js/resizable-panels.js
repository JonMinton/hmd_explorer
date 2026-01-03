// Resizable panels for HMD Explorer
// Allows users to drag to resize main plot vs subplot areas

(function() {
  'use strict';

  // Initialize resizable panels for row layout (horizontal splitter)
  window.initRowResizer = function(containerId, mainPanelId, subplotPanelId, splitterId) {
    const container = document.getElementById(containerId);
    const mainPanel = document.getElementById(mainPanelId);
    const subplotPanel = document.getElementById(subplotPanelId);
    const splitter = document.getElementById(splitterId);

    if (!container || !mainPanel || !subplotPanel || !splitter) {
      console.log('Row resizer: Elements not found, will retry...');
      return false;
    }

    let isResizing = false;
    let startY = 0;
    let startMainHeight = 0;
    let startSubplotHeight = 0;

    splitter.addEventListener('mousedown', function(e) {
      isResizing = true;
      startY = e.clientY;
      startMainHeight = mainPanel.offsetHeight;
      startSubplotHeight = subplotPanel.offsetHeight;

      document.body.style.cursor = 'row-resize';
      document.body.style.userSelect = 'none';
      e.preventDefault();
    });

    document.addEventListener('mousemove', function(e) {
      if (!isResizing) return;

      const deltaY = e.clientY - startY;
      const containerHeight = container.offsetHeight - splitter.offsetHeight;

      let newMainHeight = startMainHeight + deltaY;
      let newSubplotHeight = startSubplotHeight - deltaY;

      // Minimum heights
      const minHeight = 100;
      if (newMainHeight < minHeight) {
        newMainHeight = minHeight;
        newSubplotHeight = containerHeight - minHeight;
      }
      if (newSubplotHeight < minHeight) {
        newSubplotHeight = minHeight;
        newMainHeight = containerHeight - minHeight;
      }

      mainPanel.style.height = newMainHeight + 'px';
      subplotPanel.style.height = newSubplotHeight + 'px';

      // Trigger plotly resize
      window.dispatchEvent(new Event('resize'));
    });

    document.addEventListener('mouseup', function() {
      if (isResizing) {
        isResizing = false;
        document.body.style.cursor = '';
        document.body.style.userSelect = '';
      }
    });

    console.log('Row resizer initialized');
    return true;
  };

  // Initialize resizable panels for column layout (vertical splitter)
  window.initColResizer = function(containerId, mainPanelId, subplotPanelId, splitterId) {
    const container = document.getElementById(containerId);
    const mainPanel = document.getElementById(mainPanelId);
    const subplotPanel = document.getElementById(subplotPanelId);
    const splitter = document.getElementById(splitterId);

    if (!container || !mainPanel || !subplotPanel || !splitter) {
      console.log('Col resizer: Elements not found, will retry...');
      return false;
    }

    let isResizing = false;
    let startX = 0;
    let startMainWidth = 0;
    let startSubplotWidth = 0;

    splitter.addEventListener('mousedown', function(e) {
      isResizing = true;
      startX = e.clientX;
      startMainWidth = mainPanel.offsetWidth;
      startSubplotWidth = subplotPanel.offsetWidth;

      document.body.style.cursor = 'col-resize';
      document.body.style.userSelect = 'none';
      e.preventDefault();
    });

    document.addEventListener('mousemove', function(e) {
      if (!isResizing) return;

      const deltaX = e.clientX - startX;
      const containerWidth = container.offsetWidth - splitter.offsetWidth;

      let newMainWidth = startMainWidth + deltaX;
      let newSubplotWidth = startSubplotWidth - deltaX;

      // Minimum widths
      const minWidth = 150;
      if (newMainWidth < minWidth) {
        newMainWidth = minWidth;
        newSubplotWidth = containerWidth - minWidth;
      }
      if (newSubplotWidth < minWidth) {
        newSubplotWidth = minWidth;
        newMainWidth = containerWidth - minWidth;
      }

      // Convert to percentages for flex
      const totalWidth = newMainWidth + newSubplotWidth;
      mainPanel.style.flex = '0 0 ' + (newMainWidth / totalWidth * 100) + '%';
      subplotPanel.style.flex = '0 0 ' + (newSubplotWidth / totalWidth * 100) + '%';

      // Trigger plotly resize
      window.dispatchEvent(new Event('resize'));
    });

    document.addEventListener('mouseup', function() {
      if (isResizing) {
        isResizing = false;
        document.body.style.cursor = '';
        document.body.style.userSelect = '';
      }
    });

    console.log('Col resizer initialized');
    return true;
  };

  // Shiny message handler to initialize resizers
  if (window.Shiny) {
    Shiny.addCustomMessageHandler('initResizer', function(message) {
      const maxRetries = 10;
      let retries = 0;

      const tryInit = function() {
        let success;
        if (message.type === 'row') {
          success = window.initRowResizer(
            message.containerId,
            message.mainPanelId,
            message.subplotPanelId,
            message.splitterId
          );
        } else {
          success = window.initColResizer(
            message.containerId,
            message.mainPanelId,
            message.subplotPanelId,
            message.splitterId
          );
        }

        if (!success && retries < maxRetries) {
          retries++;
          setTimeout(tryInit, 200);
        }
      };

      // Small delay to ensure DOM is ready
      setTimeout(tryInit, 100);
    });
  }
})();
