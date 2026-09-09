(function () {
  "use strict";

  function fullscreenButton() {
    return document.querySelector('[id$="-browser_fullscreen"]');
  }

  function refreshFullscreenLabel() {
    var button = fullscreenButton();
    if (button) button.textContent = document.fullscreenElement ? "Leave Full Screen" : "Enter Full Screen";
  }

  var cardSoundKey = "drachuri.cardHoverSound";
  var manualRollKey = "drachuri.manualRollMode";

  window.drachuriCardHoverSoundEnabled = function () {
    return window.localStorage.getItem(cardSoundKey) === "true";
  };

  function cardSoundCheckbox() {
    return document.querySelector('[id$="-card_hover_sound"]');
  }

  function refreshCardSoundSetting() {
    var checkbox = cardSoundCheckbox();
    if (checkbox) checkbox.checked = window.drachuriCardHoverSoundEnabled();
  }

  window.drachuriManualRollModeEnabled = function () {
    return window.localStorage.getItem(manualRollKey) === "true";
  };

  function manualRollCheckbox() {
    return document.querySelector('[id$="-manual_roll_mode"]');
  }

  function publishManualRollSetting() {
    var enabled = window.drachuriManualRollModeEnabled();
    var checkbox = manualRollCheckbox();
    if (checkbox) checkbox.checked = enabled;
    if (window.Shiny && Shiny.setInputValue) {
      Shiny.setInputValue("manual_roll_mode_enabled", enabled, { priority: "event" });
    }
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

  document.addEventListener("change", function (event) {
    var checkbox = event.target.closest && event.target.closest('[id$="-card_hover_sound"]');
    if (!checkbox) return;
    window.localStorage.setItem(cardSoundKey, checkbox.checked ? "true" : "false");
  });

  document.addEventListener("change", function (event) {
    var checkbox = event.target.closest && event.target.closest('[id$="-manual_roll_mode"]');
    if (!checkbox) return;
    window.localStorage.setItem(manualRollKey, checkbox.checked ? "true" : "false");
    publishManualRollSetting();
  });

  document.addEventListener("fullscreenchange", refreshFullscreenLabel);
  document.addEventListener("DOMContentLoaded", function () {
    refreshFullscreenLabel();
    refreshCardSoundSetting();
    publishManualRollSetting();
  });
  document.addEventListener("shiny:connected", publishManualRollSetting);
})();
