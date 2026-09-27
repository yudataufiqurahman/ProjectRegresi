// Small, dependency-free enhancements shared by the five existing pages.
(function () {
  'use strict';

  function setDarkMode(enabled) {
    document.body.classList.toggle('pr-dark', enabled);
    try { localStorage.setItem('projectregresi-dark', enabled ? '1' : '0'); } catch (e) {}
  }

  document.addEventListener('DOMContentLoaded', function () {
    var saved = false;
    try { saved = localStorage.getItem('projectregresi-dark') === '1'; } catch (e) {}
    setDarkMode(saved);

    // Any element with data-pr-dark-toggle can control the appearance.
    document.addEventListener('change', function (event) {
      if (event.target && event.target.matches('[data-pr-dark-toggle]')) {
        setDarkMode(event.target.checked);
      }
    });

    // Add a subtle loading state while Plotly widgets are being drawn.
    if (window.Shiny) {
      $(document).on('shiny:outputinvalidated', function (event) {
        var node = document.getElementById(event.target.id);
        if (node && node.classList.contains('plotly')) node.classList.add('pr-loading');
      });
      $(document).on('shiny:value', function (event) {
        var node = document.getElementById(event.target.id);
        if (node) node.classList.remove('pr-loading');
      });
    }
  });
})();
