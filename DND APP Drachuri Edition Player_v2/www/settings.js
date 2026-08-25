(function () {
  "use strict";

  function fullscreenButton() {
    return document.querySelector('[id$="-browser_fullscreen"]');
  }

  function refreshFullscreenLabel() {
    var button = fullscreenButton();
    if (button) button.textContent = document.fullscreenElement ? "Leave Full Screen" : "Enter Full Screen";
  }

  document.addEventListener("click", function (event) {
    var button = event.target.closest && event.target.closest('[id$="-browser_fullscreen"]');
    if (!button) return;
    event.preventDefault();
    if (document.fullscreenElement) {
      document.exitFullscreen().catch(function () {});
    } else {
      document.documentElement.requestFullscreen().catch(function () {
        if (window.showToast) window.showToast("This browser did not allow full screen.", 4200);
      });
    }
  });

  document.addEventListener("fullscreenchange", refreshFullscreenLabel);
  document.addEventListener("DOMContentLoaded", refreshFullscreenLabel);
})();
