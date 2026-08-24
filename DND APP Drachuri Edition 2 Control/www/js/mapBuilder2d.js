// mapBuilder2d.js
// Faster 2D map builder:
// - Reuses the existing grid where possible
// - Uses event delegation instead of listeners on every tile
// - Only updates changed cells
// - Keeps instant browser-side painting

window.mapBuilder2DState = window.mapBuilder2DState || {
  grids: {}
};

function terrainColor(terrain) {
  terrain = String(terrain || "grass").toLowerCase();

  return {
    grass: "#8fbf7a",
    sand: "#c9b277",
    stone: "#b8b8b8",
    forest: "#5d8a4f",
    swamp: "#6d8a57",
    water: "#6da7d9",
    wall: "#555555",
    ravine: "#111015",
    road: "#c8b58a"
  }[terrain] || "#d9d4c7";
}

function terrainTexture(terrain) {
  terrain = String(terrain || "grass").toLowerCase();
  const file = {
    grass: "grass.jpg", forest: "forest.jpg", stone: "stone.jpg",
    wall: "stone.jpg", water: "water.jpg", swamp: "swamp.jpg",
    ravine: "ravine.jpg", road: "dirt.jpg", sand: "dirt.jpg"
  }[terrain];
  return file ? `url("assets/textures/${file}")` : "none";
}

function overlayStyle(light, fog) {
  light = String(light || "full").toLowerCase();
  fog = Number(fog || 0);

  if (fog === 1) return "rgba(20,20,20,0.82)";
  if (light === "dark") return "rgba(20,20,20,0.52)";
  if (light === "dim") return "rgba(70,70,70,0.16)";
  return "transparent";
}

function normaliseTiles(tiles) {
  if (typeof tiles === "string") tiles = JSON.parse(tiles);
  if (Array.isArray(tiles)) return tiles;

  if (tiles && typeof tiles === "object") {
    const keys = Object.keys(tiles);
    const n = Math.max(
      ...keys.map(k => Array.isArray(tiles[k]) ? tiles[k].length : 0)
    );

    return Array.from({ length: n }, (_, i) => {
      const row = {};
      keys.forEach(k => {
        row[k] = Array.isArray(tiles[k]) ? tiles[k][i] : tiles[k];
      });
      return row;
    });
  }

  return [];
}

function tileKey(x, y) {
  return `${Number(x)},${Number(y)}`;
}

function isBlocked(value) {
  return (
    value === true ||
    value === "true" ||
    value === 1 ||
    value === "1"
  );
}

function applyTileStyle(cell, tile) {
  if (!cell || !tile) return;

  cell.style.backgroundColor = terrainColor(tile.terrain);
  cell.style.backgroundImage = terrainTexture(tile.terrain);
  cell.style.backgroundSize = "48px 48px";
  cell.style.backgroundPosition = "center";

  if (isBlocked(tile.blocks_movement)) {
    cell.style.boxShadow = "inset 0 0 0 2px rgba(60,20,20,0.65)";
  } else {
    cell.style.boxShadow = "";
  }

  cell.style.setProperty(
    "--map-builder-overlay",
    overlayStyle(tile.light, tile.fog)
  );
}

function createCell(tile) {
  const cell = document.createElement("div");

  cell.className = "map-builder-cell";
  cell.dataset.x = Number(tile.x);
  cell.dataset.y = Number(tile.y);
  cell.dataset.key = tileKey(tile.x, tile.y);

  applyTileStyle(cell, tile);

  return cell;
}

function getGridState(containerId) {
  if (!window.mapBuilder2DState.grids[containerId]) {
    window.mapBuilder2DState.grids[containerId] = {
      grid: null,
      cells: new Map(),
      tileData: new Map(),
      width: null,
      minX: null,
      maxX: null,
      minY: null,
      maxY: null,
      mouseDown: false,
      lastPaintKey: null,
      pendingPaint: [],
      paintFlushTimer: null,
      inputIds: {}
    };
  }

  return window.mapBuilder2DState.grids[containerId];
}

function flushPaint(state) {
  if (!state.pendingPaint.length) return;

  const inputIds = state.inputIds || {};
  if (!inputIds.tilePaint) {
    state.pendingPaint = [];
    return;
  }

  Shiny.setInputValue(inputIds.tilePaint, {
    tiles: state.pendingPaint,
    nonce: Math.random()
  }, { priority: "event" });

  state.pendingPaint = [];
}

function sendClick(state, tile) {
  const inputIds = state.inputIds || {};
  if (!inputIds.tileClick || !tile) return;

  Shiny.setInputValue(inputIds.tileClick, {
    x: Number(tile.x),
    y: Number(tile.y),
    nonce: Math.random()
  }, { priority: "event" });
}

function paintCell(state, cell) {
  if (!cell) return;

  const key = cell.dataset.key;
  if (!key || key === state.lastPaintKey) return;

  state.lastPaintKey = key;

  const tile = state.tileData.get(key);
  if (!tile) return;

 const brush = readBrushFromInputs();

  const paintedTile = {
    ...tile,
    terrain: brush.terrain || "grass",
    light: brush.light || "full",
    fog: brush.fog || 0,
    blocks_movement: !!brush.blocks_movement
  };

  state.tileData.set(key, paintedTile);
  applyTileStyle(cell, paintedTile);

  state.pendingPaint.push({
    x: Number(tile.x),
    y: Number(tile.y)
  });

  clearTimeout(state.paintFlushTimer);
  state.paintFlushTimer = setTimeout(() => flushPaint(state), 250);
}

function attachGridEvents(state, containerId) {
  if (!state.grid || state.grid.dataset.eventsAttached === "1") return;

  state.grid.dataset.eventsAttached = "1";

  state.grid.addEventListener("mousedown", e => {
    const cell = e.target.closest(".map-builder-cell");
    if (!cell || !state.grid.contains(cell)) return;

    e.preventDefault();

    state.mouseDown = true;
    state.lastPaintKey = null;

    const tile = state.tileData.get(cell.dataset.key);
    sendClick(state, tile);
    paintCell(state, cell);
  });

  state.grid.addEventListener("mouseover", e => {
    if (!state.mouseDown) return;

    const cell = e.target.closest(".map-builder-cell");
    if (!cell || !state.grid.contains(cell)) return;

    paintCell(state, cell);
  });

  document.addEventListener("mouseup", () => {
    state.mouseDown = false;
    state.lastPaintKey = null;
    flushPaint(state);
  });
}

Shiny.addCustomMessageHandler("mapbuilder2d-render", function(message) {
  console.log("mapbuilder2d-render received", message.containerId);

  const container = document.getElementById(message.containerId);

  if (!container) {
    console.warn("Missing map builder container:", message.containerId);
    return;
  }

  const tiles = normaliseTiles(message.tiles);
  const inputIds = message.inputIds || {};
  const selected = message.selected || { x: null, y: null };

  window.currentBrush = message.brush || {
    terrain: "grass",
    light: "full",
    fog: 0,
    blocks_movement: false
  };

  if (!tiles.length) {
    container.innerHTML = "<em>No map loaded.</em>";
    return;
  }

  const state = getGridState(message.containerId);
  state.inputIds = inputIds;

  const xs = tiles.map(t => Number(t.x));
  const ys = tiles.map(t => Number(t.y));

  const minX = Math.min(...xs);
  const maxX = Math.max(...xs);
  const minY = Math.min(...ys);
  const maxY = Math.max(...ys);
  const width = maxX - minX + 1;

const sameShape =
  state.grid &&
  container.contains(state.grid) &&
    state.width === width &&
    state.minX === minX &&
    state.maxX === maxX &&
    state.minY === minY &&
    state.maxY === maxY &&
    state.cells.size === tiles.length;

  if (!sameShape) {
    container.innerHTML = "";

    state.grid = document.createElement("div");
    state.grid.className = "map-builder-grid";
    state.grid.style.gridTemplateColumns = `repeat(${width}, 24px)`;

    state.cells.clear();
    state.tileData.clear();

    tiles
      .slice()
      .sort((a, b) => Number(a.y) - Number(b.y) || Number(a.x) - Number(b.x))
      .forEach(tile => {
        const key = tileKey(tile.x, tile.y);
        const cell = createCell(tile);

        state.cells.set(key, cell);
        state.tileData.set(key, tile);
        state.grid.appendChild(cell);
      });

    container.appendChild(state.grid);

    state.width = width;
    state.minX = minX;
    state.maxX = maxX;
    state.minY = minY;
    state.maxY = maxY;

    attachGridEvents(state, message.containerId);
  } else {
    tiles.forEach(tile => {
      const key = tileKey(tile.x, tile.y);
      const cell = state.cells.get(key);
      const oldTile = state.tileData.get(key);

      state.tileData.set(key, tile);

      if (
        !oldTile ||
        oldTile.terrain !== tile.terrain ||
        oldTile.light !== tile.light ||
        Number(oldTile.fog || 0) !== Number(tile.fog || 0) ||
        String(oldTile.blocks_movement) !== String(tile.blocks_movement)
      ) {
        applyTileStyle(cell, tile);
      }
    });
  }

  state.cells.forEach(cell => cell.classList.remove("selected"));

  const selectedCell = state.cells.get(tileKey(selected.x, selected.y));
  if (selectedCell) selectedCell.classList.add("selected");
});

Shiny.addCustomMessageHandler("mapbuilder2d-select", function(message) {
  const container = document.getElementById(message.containerId);
  if (!container) return;

  const state = getGridState(message.containerId);

  state.cells.forEach(cell => cell.classList.remove("selected"));

  const cell = state.cells.get(tileKey(message.x, message.y));
  if (cell) cell.classList.add("selected");
});

Shiny.addCustomMessageHandler("mapbuilder2d-paint-tile", function(message) {
  const container = document.getElementById(message.containerId);
  if (!container) return;

  const state = getGridState(message.containerId);
  const key = tileKey(message.x, message.y);
  const cell = state.cells.get(key);

  if (!cell) return;

  const oldTile = state.tileData.get(key) || {
    x: message.x,
    y: message.y
  };

  const newTile = {
    ...oldTile,
    terrain: message.terrain,
    light: message.light,
    fog: message.fog,
    blocks_movement: message.blocks_movement
  };

  state.tileData.set(key, newTile);
  applyTileStyle(cell, newTile);
});

Shiny.addCustomMessageHandler("mapbuilder2d-brush", function(message) {
  console.log("brush updated", message);

  window.currentBrush = message || {
    terrain: "grass",
    light: "full",
    fog: 0,
    blocks_movement: false
  };
});

function readBrushFromInputs() {
  const terrainEl = document.querySelector('[id$="paint_terrain"]');
  const lightEl = document.querySelector('[id$="paint_light"]');
  const fogEl = document.querySelector('[id$="paint_fog"]');
  const moveCostEl = document.querySelector('[id$="paint_move_cost"]');
  const blocksMovementEl = document.querySelector('[id$="paint_blocks_movement"]');
  const blocksVisionEl = document.querySelector('[id$="paint_blocks_vision"]');

  window.currentBrush = {
    terrain: terrainEl ? terrainEl.value : "grass",
    light: lightEl ? lightEl.value : "full",
    fog: fogEl ? Number(fogEl.value || 0) : 0,
    move_cost: moveCostEl ? Number(moveCostEl.value || 1) : 1,
    blocks_movement: blocksMovementEl ? blocksMovementEl.checked : false,
    blocks_vision: blocksVisionEl ? blocksVisionEl.checked : false
  };

  console.log("brush read from inputs", window.currentBrush);

  return window.currentBrush;
}

document.addEventListener("input", function(e) {
  if (
    e.target.id &&
    (
      e.target.id.endsWith("paint_terrain") ||
      e.target.id.endsWith("paint_light") ||
      e.target.id.endsWith("paint_fog") ||
      e.target.id.endsWith("paint_move_cost") ||
      e.target.id.endsWith("paint_blocks_movement") ||
      e.target.id.endsWith("paint_blocks_vision")
    )
  ) {
    readBrushFromInputs();
  }
});
