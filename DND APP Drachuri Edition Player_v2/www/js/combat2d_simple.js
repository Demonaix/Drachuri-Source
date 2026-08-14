// combat2d_simple.js
// Lightweight 2D combat map renderer for Shiny.
// Purpose: replace the expensive Three.js combat map while keeping the same combat logic.
//
// Uses the same message/input pattern as combat3d.js where possible:
//   Shiny custom message: "combat3d-init"
//   Shiny custom message: "combat3d-update-tokens"
//   Click tile -> inputIds.move
//   Click token -> inputIds.target
//
// Recommended usage in UI:
//   tags$script(src = "js/combat2d_simple.js")
//
// Recommended container:
//   tags$div(id = session$ns("combat_3d_shell"), class = "combat-2d-shell",
//     actionButton(session$ns("map_3d_fullscreen"), "Fullscreen Map", class = "btn btn-default"),
//     tags$div(id = session$ns("combat_3d_canvas"), class = "combat-2d-canvas")
//   )
//
// You can keep the existing R server messages that send "combat3d-init" and
// "combat3d-update-tokens". This file intentionally listens to those names.

window.combat2dState = {
  initialized: false,
  containerId: null,
  inputIds: {},
  mapSignature: "",
  tileMap: {},
  tokenMap: {},
  currentTiles: [],
  fullscreenHandlerAttached: false,
  cssInjected: false,
  resizeHandlerAttached: false
};

function normaliseMapData2D(mapData) {
  if (typeof mapData === "string") {
    try {
      mapData = JSON.parse(mapData);
    } catch (e) {
      console.error("Could not parse 2D map data JSON", e, mapData);
      return [];
    }
  }

  if (Array.isArray(mapData)) return mapData;

  // Shiny can send data.frames as column objects:
  // { x:[...], y:[...], terrain:[...] }
  if (mapData && typeof mapData === "object") {
    const keys = Object.keys(mapData);
    const rowCount = Math.max(
      0,
      ...keys.map(k => Array.isArray(mapData[k]) ? mapData[k].length : 0)
    );

    return Array.from({ length: rowCount }, (_, i) => {
      const row = {};
      keys.forEach(k => {
        row[k] = Array.isArray(mapData[k]) ? mapData[k][i] : mapData[k];
      });
      return row;
    });
  }

  return [];
}

function isTruthy2D(x) {
  return x === true || x === "true" || x === 1 || x === "1";
}

function safeText2D(x, fallback = "") {
  if (x === null || x === undefined) return fallback;
  return String(x);
}

function seededRandom2D(x, y, salt = 1) {
  const n = Math.sin(
    Number(x) * 127.1 +
    Number(y) * 311.7 +
    salt * 74.7
  ) * 43758.5453;

  return n - Math.floor(n);
}

function getMapSignature2D(mapData) {
  return mapData
    .map(t => [
      t.x,
      t.y,
      t.terrain || "",
      t.tile_type || "",
      t.move_cost || "",
      t.blocks_movement || ""
    ].join(","))
    .sort()
    .join("|");
}

function terrainClass2D(terrain) {
  const t = safeText2D(terrain, "grass").toLowerCase().trim();

  if (t === "woodland") return "forest";
  if (t === "grass" || t === "forest" || t === "water" || t === "stone" ||
      t === "wall" || t === "road" || t === "swamp" || t === "pit" ||
      t === "ravine") {
    return t;
  }

  return "default";
}

function terrainEmoji2D(terrain) {
  const t = terrainClass2D(terrain);

  if (t === "grass") return "";
  if (t === "forest") return "♣";
  if (t === "water") return "≈";
  if (t === "stone") return "◆";
  if (t === "wall") return "█";
  if (t === "road") return "·";
  if (t === "swamp") return "∴";
  if (t === "pit") return "◉";
  if (t === "ravine") return "▾";

  return "";
}

function actorInitial2D(tile) {
  const name = safeText2D(
    tile.occupant_name ||
    tile.display_name ||
    tile.name ||
    tile.enemy_name ||
    tile.actor_name ||
    tile.occupant_id ||
    "?"
  ).trim();

  if (!name) return "?";

  const words = name.split(/\s+/).filter(Boolean);
  if (words.length >= 2) {
    return (words[0][0] + words[1][0]).toUpperCase();
  }

  return name.slice(0, 2).toUpperCase();
}

function injectCombat2DCSS() {
  const state = window.combat2dState;
  if (state.cssInjected) return;

  const style = document.createElement("style");
  style.id = "combat2d-simple-style";
  style.textContent = `
    .combat-2d-shell{
      width:100%;
      min-width:0;
    }

    .combat-2d-toolbar{
      display:flex;
      align-items:center;
      justify-content:space-between;
      gap:8px;
      margin-bottom:8px;
      flex-wrap:wrap;
    }

    .combat-2d-canvas{
      width:100%;
      height:620px;
      min-height:420px;
      overflow:auto;
      border-radius:14px;
      border:1px solid rgba(191,167,111,0.55);
      background:
        radial-gradient(circle at top, rgba(255,248,220,0.26), transparent 38%),
        linear-gradient(145deg, #24251e, #141714);
      box-shadow:
        inset 0 0 34px rgba(0,0,0,0.38),
        0 8px 22px rgba(0,0,0,0.12);
      padding:14px;
      box-sizing:border-box;
      touch-action:pan-x pan-y;
    }

    .combat-2d-grid{
      display:grid;
      gap:2px;
      width:max-content;
      min-width:max-content;
      margin:auto;
      padding:8px;
      border-radius:14px;
      background:rgba(18,16,12,0.38);
      box-shadow:0 10px 30px rgba(0,0,0,0.18);
    }

    .combat-2d-tile{
      position:relative;
      width:42px;
      height:42px;
      min-width:42px;
      min-height:42px;
      box-sizing:border-box;
      border-radius:7px;
      border:1px solid rgba(0,0,0,0.22);
      cursor:pointer;
      user-select:none;
      overflow:visible;
      transition:
        transform 0.08s ease,
        filter 0.12s ease,
        box-shadow 0.12s ease,
        outline-color 0.12s ease;
    }

    .combat-2d-tile:hover{
      filter:brightness(1.12);
      transform:translateY(-1px);
      z-index:20;
    }

    .combat-2d-tile::before{
      content:"";
      position:absolute;
      inset:0;
      border-radius:6px;
      pointer-events:none;
      background-image:
        linear-gradient(135deg, rgba(255,255,255,0.12), transparent 42%),
        radial-gradient(circle at 72% 20%, rgba(255,255,255,0.10), transparent 22%),
        linear-gradient(45deg, rgba(255,255,255,0.04) 25%, transparent 25%),
        linear-gradient(-45deg, rgba(0,0,0,0.05) 25%, transparent 25%);
      background-size:auto, auto, 8px 8px, 8px 8px;
      mix-blend-mode:soft-light;
    }

    .combat-2d-tile::after{
      content:attr(data-tip);
      position:absolute;
      left:50%;
      bottom:calc(100% + 8px);
      transform:translateX(-50%);
      min-width:180px;
      max-width:260px;
      white-space:pre-line;
      padding:8px 10px;
      border-radius:10px;
      background:rgba(20,18,16,0.96);
      color:#f5e6c8;
      border:1px solid rgba(191,167,111,0.55);
      box-shadow:0 8px 22px rgba(0,0,0,0.28);
      font-size:11px;
      line-height:1.3;
      pointer-events:none;
      opacity:0;
      z-index:99999;
    }

    .combat-2d-tile:hover::after{
      opacity:1;
    }

    .combat-2d-terrain-label{
      position:absolute;
      inset:0;
      display:flex;
      align-items:center;
      justify-content:center;
      font-size:18px;
      font-weight:900;
      color:rgba(255,255,255,0.58);
      text-shadow:0 1px 2px rgba(0,0,0,0.38);
      pointer-events:none;
      z-index:1;
    }

    .combat-2d-token{
      position:absolute;
      left:50%;
      top:50%;
      width:28px;
      height:28px;
      border-radius:999px;
      transform:translate(-50%, -50%);
      display:flex;
      align-items:center;
      justify-content:center;
      font-size:10px;
      font-weight:900;
      color:white;
      text-shadow:0 1px 2px rgba(0,0,0,0.65);
      border:2px solid rgba(255,255,255,0.55);
      box-shadow:
        0 3px 8px rgba(0,0,0,0.45),
        inset 0 1px 2px rgba(255,255,255,0.28);
      z-index:8;
      pointer-events:auto;
      transition:
        transform 0.12s ease,
        box-shadow 0.12s ease,
        left 0.16s ease,
        top 0.16s ease;
    }

    .combat-2d-token:hover{
      transform:translate(-50%, -50%) scale(1.16);
      box-shadow:
        0 0 14px rgba(255,220,130,0.95),
        0 3px 10px rgba(0,0,0,0.5);
      z-index:30;
    }

    .combat-2d-token.player{
      background:linear-gradient(135deg, #48c8ff, #176f9e);
    }

    .combat-2d-token.enemy{
      background:linear-gradient(135deg, #ff8a4a, #9c2f1d);
    }

    .combat-2d-token.actor{
      background:linear-gradient(135deg, #b48cff, #5d3c99);
    }

    .combat-2d-token.active{
      animation:combat2dActivePulse 1.25s ease-in-out infinite;
    }

    @keyframes combat2dActivePulse{
      0%, 100%{
        box-shadow:
          0 0 0 2px rgba(255,205,80,0.8),
          0 0 10px rgba(255,180,60,0.65),
          0 3px 8px rgba(0,0,0,0.45);
      }
      50%{
        box-shadow:
          0 0 0 4px rgba(255,232,130,0.98),
          0 0 18px rgba(255,185,65,0.95),
          0 3px 8px rgba(0,0,0,0.45);
      }
    }

    .combat-2d-tile.reachable{
      outline:2px solid rgba(90,210,105,0.9);
      outline-offset:-3px;
      box-shadow:inset 0 0 18px rgba(80,190,90,0.22);
    }

    .combat-2d-tile.pending-move{
      outline:2px solid rgba(255,210,70,0.98);
      outline-offset:-3px;
      box-shadow:
        inset 0 0 18px rgba(255,215,70,0.34),
        0 0 12px rgba(255,190,60,0.42);
    }

    .combat-2d-tile.attackable{
      outline:2px solid rgba(235,80,55,0.95);
      outline-offset:-3px;
      box-shadow:inset 0 0 18px rgba(235,80,55,0.25);
    }

    .combat-2d-tile.selected{
      outline:2px solid rgba(255,255,255,0.95);
      outline-offset:-3px;
    }

    .combat-2d-tile.terrain-grass{
      background:#5f9149;
    }

    .combat-2d-tile.terrain-forest{
      background:#315f38;
    }

    .combat-2d-tile.terrain-water{
      background:linear-gradient(135deg, #2c73a3, #17496f);
    }

    .combat-2d-tile.terrain-stone{
      background:#7c7c76;
    }

    .combat-2d-tile.terrain-wall{
      background:#484845;
      box-shadow:
        inset 0 0 0 3px rgba(0,0,0,0.16),
        inset 0 8px 12px rgba(255,255,255,0.06),
        0 2px 3px rgba(0,0,0,0.22);
    }

    .combat-2d-tile.terrain-road{
      background:#a5885b;
    }

    .combat-2d-tile.terrain-swamp{
      background:#4b6540;
    }

    .combat-2d-tile.terrain-pit,
    .combat-2d-tile.terrain-ravine{
      background:#111;
      box-shadow:inset 0 0 18px rgba(0,0,0,0.85);
    }

    .combat-2d-tile.terrain-default{
      background:#746e5e;
    }

    .combat-2d-shell:fullscreen{
      background:#111510;
      padding:14px;
      box-sizing:border-box;
      overflow:hidden;
    }

    .combat-2d-shell:fullscreen .combat-2d-canvas{
      height:calc(100vh - 74px) !important;
      min-height:0 !important;
      border-radius:14px;
    }

    .combat-2d-shell:fullscreen .combat-2d-toolbar{
      color:#f5e6c8;
    }

    @media (max-width: 900px){
      .combat-2d-tile{
        width:38px;
        height:38px;
        min-width:38px;
        min-height:38px;
      }

      .combat-2d-token{
        width:25px;
        height:25px;
        font-size:9px;
      }
    }
  `;

  document.head.appendChild(style);
  state.cssInjected = true;
}

function renderCombat2D(containerId, mapData, inputIds = {}) {
  injectCombat2DCSS();

  const state = window.combat2dState;
  const el = document.getElementById(containerId);

  mapData = normaliseMapData2D(mapData);

  if (!el) {
    console.error("Missing 2D combat container", containerId);
    return;
  }

  if (!Array.isArray(mapData) || mapData.length === 0) {
    el.innerHTML = `<div style="padding:12px;color:#f5e6c8;">No map data.</div>`;
    return;
  }

  state.containerId = containerId;
  state.inputIds = inputIds || {};
  state.currentTiles = mapData;

  const signature = getMapSignature2D(mapData);
  const mapChanged = state.mapSignature !== signature;

  if (!state.initialized || mapChanged || !el.querySelector(".combat-2d-grid")) {
    buildCombat2DMap(el, mapData);
    state.mapSignature = signature;
    state.initialized = true;
  } else {
    updateCombat2DMap(mapData);
  }

  setupFullscreen2DHandler();
  setupCombat2DResizeHandler();
}

window.renderCombat2D = renderCombat2D;

// Compatibility alias: this allows you to swap JS files without changing server messages.
window.renderCombat3D = renderCombat2D;

function buildCombat2DMap(el, tiles) {
  const state = window.combat2dState;

  state.tileMap = {};
  state.tokenMap = {};

  const xs = tiles.map(t => Number(t.x)).filter(Number.isFinite);
  const ys = tiles.map(t => Number(t.y)).filter(Number.isFinite);

  const minX = Math.min(...xs);
  const maxX = Math.max(...xs);
  const minY = Math.min(...ys);
  const maxY = Math.max(...ys);

  const cols = maxX - minX + 1;
  const rows = maxY - minY + 1;

  el.innerHTML = "";

  const grid = document.createElement("div");
  grid.className = "combat-2d-grid";
  grid.style.gridTemplateColumns = `repeat(${cols}, 42px)`;
  grid.style.gridTemplateRows = `repeat(${rows}, 42px)`;
  grid.dataset.minX = String(minX);
  grid.dataset.minY = String(minY);
  grid.dataset.maxX = String(maxX);
  grid.dataset.maxY = String(maxY);

  const byKey = {};
  tiles.forEach(tile => {
    byKey[`${Number(tile.x)},${Number(tile.y)}`] = tile;
  });

  for (let y = minY; y <= maxY; y++) {
    for (let x = minX; x <= maxX; x++) {
      const tile = byKey[`${x},${y}`] || { x, y, terrain: "void", missing: true };
      const tileEl = buildTileElement2D(tile);
      grid.appendChild(tileEl);
      state.tileMap[`${x},${y}`] = tileEl;
    }
  }

  el.appendChild(grid);

  tiles.forEach(tile => {
    if (tile.occupant_id) {
      placeOrUpdateToken2D(tile);
    }
  });

  // Keep the active actor visible when the map first appears.
  setTimeout(() => scrollActiveTokenIntoView2D(), 60);
}

function buildTileElement2D(tile) {
  const tileEl = document.createElement("div");
  const terrain = terrainClass2D(tile.terrain);

  tileEl.className = [
    "combat-2d-tile",
    `terrain-${terrain}`,
    isTruthy2D(tile.is_reachable) ? "reachable" : "",
    isTruthy2D(tile.is_pending_move) ? "pending-move" : "",
    isTruthy2D(tile.is_attackable) ? "attackable" : "",
    isTruthy2D(tile.is_selected) ? "selected" : ""
  ].filter(Boolean).join(" ");

  tileEl.dataset.x = String(Number(tile.x));
  tileEl.dataset.y = String(Number(tile.y));
  tileEl.dataset.terrain = safeText2D(tile.terrain, "grass");
  tileEl.dataset.tip = buildTileTip2D(tile);

  const label = document.createElement("div");
  label.className = "combat-2d-terrain-label";
  label.textContent = terrainEmoji2D(tile.terrain);
  tileEl.appendChild(label);

  tileEl.addEventListener("click", e => {
    // If token was clicked, token handler deals with it.
    if (e.target.closest(".combat-2d-token")) return;

    const state = window.combat2dState;
    if (!state.inputIds || !state.inputIds.move) return;

    Shiny.setInputValue(state.inputIds.move, {
      x: Number(tileEl.dataset.x),
      y: Number(tileEl.dataset.y),
      nonce: Math.random()
    }, { priority: "event" });
  });

  return tileEl;
}

function updateCombat2DMap(tiles) {
  const state = window.combat2dState;
  const seenTokens = {};

  tiles.forEach(tile => {
    const key = `${Number(tile.x)},${Number(tile.y)}`;
    const tileEl = state.tileMap[key];
    if (!tileEl) return;

    updateTileClasses2D(tileEl, tile);
    tileEl.dataset.tip = buildTileTip2D(tile);
    tileEl.dataset.terrain = safeText2D(tile.terrain, "grass");

    if (tile.occupant_id) {
      seenTokens[String(tile.occupant_id)] = true;
      placeOrUpdateToken2D(tile);
    }
  });

  Object.keys(state.tokenMap).forEach(id => {
    if (seenTokens[id]) return;

    const tokenEl = state.tokenMap[id];
    if (tokenEl && tokenEl.parentNode) tokenEl.parentNode.removeChild(tokenEl);
    delete state.tokenMap[id];
  });
}

function updateTileClasses2D(tileEl, tile) {
  const terrain = terrainClass2D(tile.terrain);

  tileEl.className = [
    "combat-2d-tile",
    `terrain-${terrain}`,
    isTruthy2D(tile.is_reachable) ? "reachable" : "",
    isTruthy2D(tile.is_pending_move) ? "pending-move" : "",
    isTruthy2D(tile.is_attackable) ? "attackable" : "",
    isTruthy2D(tile.is_selected) ? "selected" : ""
  ].filter(Boolean).join(" ");

  const label = tileEl.querySelector(".combat-2d-terrain-label");
  if (label) label.textContent = terrainEmoji2D(tile.terrain);
}

function placeOrUpdateToken2D(tile) {
  const state = window.combat2dState;
  const id = String(tile.occupant_id);
  const key = `${Number(tile.x)},${Number(tile.y)}`;
  const tileEl = state.tileMap[key];

  if (!tileEl) return;

  let tokenEl = state.tokenMap[id];

  if (!tokenEl) {
    tokenEl = document.createElement("div");
    tokenEl.className = "combat-2d-token";
    tokenEl.dataset.actorId = id;

    tokenEl.addEventListener("click", e => {
      e.stopPropagation();

      const stateNow = window.combat2dState;
      if (!stateNow.inputIds || !stateNow.inputIds.target) return;

      Shiny.setInputValue(stateNow.inputIds.target, {
        actor_id: String(tokenEl.dataset.actorId || ""),
        actor_type: String(tokenEl.dataset.actorType || "actor"),
        x: Number(tokenEl.dataset.x),
        y: Number(tokenEl.dataset.y),
        nonce: Math.random()
      }, { priority: "event" });
    });

    state.tokenMap[id] = tokenEl;
  }

  const actorType = safeText2D(tile.occupant_type, "actor").toLowerCase();
  const tokenClass = actorType === "player" ? "player" : actorType === "enemy" ? "enemy" : "actor";

  tokenEl.className = [
    "combat-2d-token",
    tokenClass,
    isTruthy2D(tile.is_active_actor) ? "active" : ""
  ].filter(Boolean).join(" ");

  tokenEl.textContent = actorInitial2D(tile);
  tokenEl.title = buildTokenTitle2D(tile);

  tokenEl.dataset.actorId = id;
  tokenEl.dataset.actorType = safeText2D(tile.occupant_type, "actor");
  tokenEl.dataset.x = String(Number(tile.x));
  tokenEl.dataset.y = String(Number(tile.y));

  if (tokenEl.parentNode !== tileEl) {
    if (tokenEl.parentNode) tokenEl.parentNode.removeChild(tokenEl);
    tileEl.appendChild(tokenEl);
  }
}

function buildTileTip2D(tile) {
  const parts = [];

  parts.push(`(${safeText2D(tile.x, "?")}, ${safeText2D(tile.y, "?")})`);
  parts.push(`Terrain: ${safeText2D(tile.terrain, "grass")}`);

  if (tile.move_cost !== undefined && tile.move_cost !== null && tile.move_cost !== "") {
    parts.push(`Move cost: ${tile.move_cost}`);
  }

  if (isTruthy2D(tile.blocks_movement)) {
    parts.push("Blocks movement");
  }

  if (isTruthy2D(tile.is_reachable)) {
    parts.push("Reachable");
  }

  if (isTruthy2D(tile.is_pending_move)) {
    parts.push("Pending move");
  }

  if (tile.occupant_id) {
    parts.push(`Actor: ${safeText2D(tile.occupant_name || tile.display_name || tile.name || tile.occupant_id)}`);
  }

  return parts.join("\n");
}

function buildTokenTitle2D(tile) {
  const name = safeText2D(
    tile.occupant_name ||
    tile.display_name ||
    tile.name ||
    tile.enemy_name ||
    tile.actor_name ||
    tile.occupant_id,
    "Actor"
  );

  return `${name} (${safeText2D(tile.occupant_type, "actor")})`;
}

function scrollActiveTokenIntoView2D() {
  const state = window.combat2dState;
  const active = Object.values(state.tokenMap).find(el => el.classList.contains("active"));
  if (!active) return;

  try {
    active.scrollIntoView({
      behavior: "smooth",
      block: "center",
      inline: "center"
    });
  } catch {
    active.scrollIntoView();
  }
}

function setupCombat2DResizeHandler() {
  const state = window.combat2dState;
  if (state.resizeHandlerAttached) return;

  window.addEventListener("resize", () => {
    // CSS grid handles most resizing. This is mostly for fullscreen changes.
    setTimeout(() => scrollActiveTokenIntoView2D(), 100);
  });

  document.addEventListener("fullscreenchange", () => {
    setTimeout(() => scrollActiveTokenIntoView2D(), 120);
  });

  state.resizeHandlerAttached = true;
}

function setupFullscreen2DHandler() {
  const state = window.combat2dState;
  if (state.fullscreenHandlerAttached) return;

  document.addEventListener("click", async function(e) {
    const btn = e.target.closest("[id$='map_3d_fullscreen'], [id$='map_2d_fullscreen']");
    if (!btn) return;

    e.preventDefault();
    e.stopPropagation();

    const nsPrefix = btn.id
      .replace("map_3d_fullscreen", "")
      .replace("map_2d_fullscreen", "");

    const shell =
      document.getElementById(nsPrefix + "combat_3d_shell") ||
      document.getElementById(nsPrefix + "combat_2d_shell") ||
      btn.closest(".combat-2d-shell") ||
      btn.closest(".combat-3d-shell");

    if (!shell) {
      alert("Fullscreen failed: map shell was not found.");
      return;
    }

    // Make sure CSS fullscreen rules apply even if the old 3D shell class is used.
    shell.classList.add("combat-2d-shell");

    try {
      if (!document.fullscreenElement) {
        if (shell.requestFullscreen) {
          await shell.requestFullscreen();
        } else if (shell.webkitRequestFullscreen) {
          shell.webkitRequestFullscreen();
        }
      } else {
        if (document.exitFullscreen) {
          await document.exitFullscreen();
        } else if (document.webkitExitFullscreen) {
          document.webkitExitFullscreen();
        }
      }

      setTimeout(() => scrollActiveTokenIntoView2D(), 120);
      setTimeout(() => scrollActiveTokenIntoView2D(), 400);
    } catch (err) {
      console.error("2D fullscreen failed:", err);
      alert("Fullscreen failed. Check browser console.");
    }
  });

  state.fullscreenHandlerAttached = true;
}

// Keep old Shiny message names for easy swap from combat3d.js.
Shiny.addCustomMessageHandler("combat3d-init", function(message) {
  function tryInit(attemptsLeft) {
    const el = document.getElementById(message.containerId);

    if (!el) {
      if (attemptsLeft <= 0) {
        console.error("Missing 2D container after retries", message.containerId);
        return;
      }

      setTimeout(() => tryInit(attemptsLeft - 1), 100);
      return;
    }

    renderCombat2D(
      message.containerId,
      message.mapData,
      message.inputIds || {}
    );
  }

  tryInit(80);
});

Shiny.addCustomMessageHandler("combat3d-update-tokens", function(message) {
  const state = window.combat2dState;
  if (!state.initialized) return;

  const tokens = message.tokens || [];

  tokens.forEach(t => {
    const id = String(t.actor_id);
    const tokenEl = state.tokenMap[id];
    if (!tokenEl) return;

    const oldTile = tokenEl.parentNode;
    const key = `${Number(t.x)},${Number(t.y)}`;
    const newTile = state.tileMap[key];

    if (newTile && oldTile !== newTile) {
      newTile.appendChild(tokenEl);
    }

    const actorType = safeText2D(t.actor_type, "actor").toLowerCase();
    const tokenClass = actorType === "player" ? "player" : actorType === "enemy" ? "enemy" : "actor";

    tokenEl.className = [
      "combat-2d-token",
      tokenClass,
      isTruthy2D(t.is_active_actor) ? "active" : ""
    ].filter(Boolean).join(" ");

    tokenEl.dataset.actorId = id;
    tokenEl.dataset.actorType = safeText2D(t.actor_type, "actor");
    tokenEl.dataset.x = String(Number(t.x));
    tokenEl.dataset.y = String(Number(t.y));
  });
});

Shiny.addCustomMessageHandler("combat3d-resize", function() {
  setTimeout(() => scrollActiveTokenIntoView2D(), 80);
  setTimeout(() => scrollActiveTokenIntoView2D(), 300);
});

Shiny.addCustomMessageHandler("combat3d-browser-fullscreen", async function(message) {
  const shell = document.getElementById(message.shellId);
  if (!shell) return;

  shell.classList.add("combat-2d-shell");

  if (!document.fullscreenElement) {
    if (shell.requestFullscreen) await shell.requestFullscreen();
    else if (shell.webkitRequestFullscreen) shell.webkitRequestFullscreen();
  } else {
    if (document.exitFullscreen) await document.exitFullscreen();
    else if (document.webkitExitFullscreen) document.webkitExitFullscreen();
  }

  setTimeout(() => scrollActiveTokenIntoView2D(), 120);
  setTimeout(() => scrollActiveTokenIntoView2D(), 400);
});
