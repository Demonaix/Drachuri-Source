(function () {
  var totalMs = 10000;
  var tickMs = 120;

  var tips = [
    "Tip: Write one sensory detail in your diary (smell, sound, texture) to make it feel real.",
    "Lore: The veil thins where old blood was spilled — tread softly.",
    "Tip: If you are Wounded or worse, think tactically: cover, retreat, allies.",
    "Lore: Gwynn ap Nudd does not hurry. He waits for the moment that matters.",
    "Tip: Status effects are story hooks. Use them to roleplay, not just penalties."
  ];

  function byId(id) { return document.getElementById(id); }

  function startLoading() {
    var tipEl = byId("landing-tip");
    var pctEl = byId("landing-pct");
    var fill = byId("landing-fill");

    var idx = Math.floor(Math.random() * tips.length);
    if (tipEl) tipEl.textContent = tips[idx];

    var tipTimer = setInterval(function () {
      idx = (idx + 1) % tips.length;
      if (tipEl) tipEl.textContent = tips[idx];
    }, 2200);

    var elapsed = 0;
    var timer = setInterval(function () {
      elapsed += tickMs;
      var p = Math.min(1, elapsed / totalMs);
      var pct = Math.round(p * 100);

      if (fill) fill.style.width = pct + "%";
      if (pctEl) pctEl.textContent = pct + "%";

      if (elapsed >= totalMs) {
        clearInterval(timer);
        clearInterval(tipTimer);

        var loadPanel = byId("landing-load-panel");
        var modWrap = byId("landing-module-wrap");
        if (loadPanel) loadPanel.style.display = "none";
        if (modWrap) modWrap.style.display = "block";
      }
    }, tickMs);
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", startLoading);
  } else {
    startLoading();
  }
})();