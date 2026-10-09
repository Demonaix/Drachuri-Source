// combat3d.js
// Full clean rewrite: OrbitControls-only camera, fixed click-to-move,
// smaller character models, flatter terrain, better camera modes,
// moody-but-readable lighting.

import * as THREE from "three";
import { GLTFLoader } from "three/addons/loaders/GLTFLoader.js";
import { OrbitControls } from "three/addons/controls/OrbitControls.js";
import { createHeartTreeBoss } from "./combat_custom_renders.js";


window.THREE = THREE;

const combatGLTFLoader = new GLTFLoader();

const MODEL_TARGET_HEIGHT = 0.22;
const TILE_SIZE = 1;
const TILE_CLICK_Y_OFFSET = 0.32;

window.combat3dState = {
  initialized: false,
  scene: null,
  camera: null,
  renderer: null,
  controls: null,

  worldGroup: null,
  terrainGroup: null,
  decorGroup: null,
  tokenGroup: null,
  groundMesh: null,

  tileMap: {},
  tokenMap: {},
  clickableMeshes: [],

  containerId: null,
  inputIds: {},

  animationFrame: null,
  resizeHandlerAttached: false,
  fullscreenHandlerAttached: false,
  clickHandlerAttached: false,

  mapSignature: "",
  centerX: 0,
  centerY: 0,
  pointerDown: null
};

function normaliseMapData(mapData) {
  if (typeof mapData === "string") mapData = JSON.parse(mapData);

  if (Array.isArray(mapData)) return mapData;

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

function getMapSignature(mapData) {
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

window.renderCombat3D = function(containerId, mapData, inputIds = {}) {
  mapData = normaliseMapData(mapData);

  if (!Array.isArray(mapData) || mapData.length === 0) {
    console.error("mapData is empty or invalid", mapData);
    return;
  }

  const el = document.getElementById(containerId);
  if (!el) {
    console.error("Missing container", containerId);
    return;
  }

  const state = window.combat3dState;
  const mapSignature = getMapSignature(mapData);

  const containerChanged =
    state.containerId !== containerId ||
    !state.renderer ||
    !state.renderer.domElement ||
    !el.contains(state.renderer.domElement);

  const mapChanged =
    state.mapSignature &&
    state.mapSignature !== mapSignature;

  if (!state.initialized || containerChanged) {
    disposeCombat3D();

    initCombat3D(containerId, inputIds);
    buildCombatTerrain(mapData);
    spawnCombatTokens(mapData);
    resetCombatCamera(mapData);

    state.mapSignature = mapSignature;
    state.initialized = true;
  } else if (mapChanged) {
    state.inputIds = inputIds || state.inputIds || {};

    buildCombatTerrain(mapData);
    spawnCombatTokens(mapData);
    resetCombatCamera(mapData);

    state.mapSignature = mapSignature;
  } else {
    state.inputIds = inputIds || state.inputIds || {};
    updateCombatTokens(mapData);
    updateReachableHighlights(mapData);
  }

  onCombat3DResize();
};

function disposeObject3D(obj) {
  if (!obj) return;

  obj.traverse(child => {
    if (child.geometry) child.geometry.dispose();

    if (child.material) {
      const mats = Array.isArray(child.material) ? child.material : [child.material];
      mats.forEach(mat => {
        Object.keys(mat).forEach(k => {
          const v = mat[k];
         if (v && v.isTexture) v.dispose();
        });
        mat.dispose();
      });
    }
  });
}

function disposeCombat3D() {
  const state = window.combat3dState;

  if (state.animationFrame) {
    cancelAnimationFrame(state.animationFrame);
    state.animationFrame = null;
  }

  if (state.controls) {
    try { state.controls.dispose(); } catch {}
    state.controls = null;
  }

  if (state.scene) disposeObject3D(state.scene);

  if (state.renderer) {
    try { state.renderer.dispose(); } catch {}
    if (state.renderer.domElement && state.renderer.domElement.parentNode) {
      state.renderer.domElement.parentNode.removeChild(state.renderer.domElement);
    }
  }

  state.initialized = false;
  state.scene = null;
  state.camera = null;
  state.renderer = null;
  state.worldGroup = null;
  state.terrainGroup = null;
  state.decorGroup = null;
  state.tokenGroup = null;
  state.groundMesh = null;
  state.tileMap = {};
  state.tokenMap = {};
  state.clickableMeshes = [];
  state.mapSignature = "";
  state.pointerDown = null;
}

function initCombat3D(containerId, inputIds) {
  const state = window.combat3dState;

  state.containerId = containerId;
  state.inputIds = inputIds || {};

  const el = document.getElementById(containerId);
  if (!el || !window.THREE) return;

  el.innerHTML = "";

  const width = Math.max(1, el.clientWidth || 1200);
  const height = Math.max(1, el.clientHeight || 700);

  const scene = new THREE.Scene();
  scene.background = new THREE.Color(0x1f2730);
  scene.fog = new THREE.FogExp2(0x252a33, 0.0042);
  addCombatBackdrop(scene);

  const camera = new THREE.PerspectiveCamera(50, width / height, 0.1, 2000);

  const renderer = new THREE.WebGLRenderer({
    antialias: true,
    alpha: false
  });

  renderer.setSize(width, height, false);
  renderer.setPixelRatio(Math.min(window.devicePixelRatio || 1, 2));

  renderer.shadowMap.enabled = true;
  renderer.shadowMap.type = THREE.PCFSoftShadowMap;

  if ("outputColorSpace" in renderer) {
    renderer.outputColorSpace = THREE.SRGBColorSpace;
  } else {
    renderer.outputEncoding = THREE.sRGBEncoding;
  }

  renderer.toneMapping = THREE.ACESFilmicToneMapping;
  renderer.toneMappingExposure = 1.12;

  if ("physicallyCorrectLights" in renderer) {
    renderer.physicallyCorrectLights = false;
  }

  el.appendChild(renderer.domElement);

  const worldGroup = new THREE.Group();
  const terrainGroup = new THREE.Group();
  const decorGroup = new THREE.Group();
  const tokenGroup = new THREE.Group();

  worldGroup.add(terrainGroup);
  worldGroup.add(decorGroup);
  worldGroup.add(tokenGroup);
  scene.add(worldGroup);

  addMoodLighting(scene);

  const controls = new OrbitControls(camera, renderer.domElement);
  controls.enableDamping = true;
  controls.dampingFactor = 0.08;
  controls.enablePan = true;
  controls.screenSpacePanning = true;
  controls.enableRotate = true;
  controls.rotateSpeed = 0.55;
  controls.enableZoom = true;
  controls.zoomSpeed = 0.85;
  controls.minDistance = 1.8;
  controls.maxDistance = 90;
  controls.minPolarAngle = 0.08;
  controls.maxPolarAngle = Math.PI / 2.06;
  controls.target.set(0, 0, 0);
  controls.update();

  state.scene = scene;
  state.camera = camera;
  state.renderer = renderer;
  state.controls = controls;
  state.worldGroup = worldGroup;
  state.terrainGroup = terrainGroup;
  state.decorGroup = decorGroup;
  state.tokenGroup = tokenGroup;

  el.__combat3dRenderer = renderer;
  el.__combat3dCamera = camera;
  el.__combat3dScene = scene;

  setupClickHandler();
  startAnimationLoop();

  if (!state.resizeHandlerAttached) {
    window.addEventListener("resize", onCombat3DResize);
    document.addEventListener("fullscreenchange", () => {
      setTimeout(onCombat3DResize, 80);
      setTimeout(onCombat3DResize, 300);
    });
    state.resizeHandlerAttached = true;
  }

  setupFullscreenButtonHandler();
  setupCameraButtonHandler();

  onCombat3DResize();
}

function addMoodLighting(scene) {
  scene.add(new THREE.AmbientLight(0xffffff, 0.38));

  const hemi = new THREE.HemisphereLight(0xddeaff, 0x6a4b32, 0.72);
  scene.add(hemi);

  const sun = new THREE.DirectionalLight(0xffdfad, 1.18);
  sun.position.set(18, 32, 12);
  sun.castShadow = true;
  sun.shadow.mapSize.width = 2048;
  sun.shadow.mapSize.height = 2048;
  sun.shadow.camera.near = 0.5;
  sun.shadow.camera.far = 140;
  sun.shadow.bias = -0.0002;
  scene.add(sun);

  const fill = new THREE.DirectionalLight(0xa9c4ff, 0.28);
  fill.position.set(-14, 10, -18);
  scene.add(fill);

  const warmBounce = new THREE.PointLight(0xffb36a, 0.28, 34, 2);
  warmBounce.position.set(0, 3.5, 0);
  scene.add(warmBounce);

  addLightShafts(scene);
}

function addLightShafts(scene) {
  const shaftGroup = new THREE.Group();
  shaftGroup.name = "combat_light_shafts";

  for (let i = 0; i < 5; i++) {
    const mat = new THREE.MeshBasicMaterial({
      color: 0xffe7b5,
      transparent: true,
      opacity: 0.035,
      depthWrite: false,
      blending: THREE.AdditiveBlending,
      side: THREE.DoubleSide
    });

    const geo = new THREE.PlaneGeometry(3.5, 18);
    const shaft = new THREE.Mesh(geo, mat);

    shaft.position.set(
      -8 + i * 4,
      7.5,
      -8 + seededRandom(i, 1, 22) * 16
    );

    shaft.rotation.x = -0.45;
    shaft.rotation.y = 0.35;
    shaft.rotation.z = -0.18;

    shaft.userData.isLightShaft = true;
    shaft.userData.phase = seededRandom(i, 2, 99) * Math.PI * 2;

    shaftGroup.add(shaft);
  }

  scene.add(shaftGroup);
}

function setupClickHandler() {
  const state = window.combat3dState;
  if (!state.renderer || !state.renderer.domElement) return;

  const dom = state.renderer.domElement;
  const raycaster = new THREE.Raycaster();
  const mouse = new THREE.Vector2();

  dom.addEventListener("pointerdown", e => {
    state.pointerDown = {
      x: e.clientX,
      y: e.clientY,
      t: performance.now()
    };
  });

  dom.addEventListener("click", e => {
    if (!state.pointerDown) return;

    const dx = e.clientX - state.pointerDown.x;
    const dy = e.clientY - state.pointerDown.y;
    const moved = Math.sqrt(dx * dx + dy * dy);
    const elapsed = performance.now() - state.pointerDown.t;

    if (moved > 6 || elapsed > 550) return;

    const rect = dom.getBoundingClientRect();

    mouse.x = ((e.clientX - rect.left) / rect.width) * 2 - 1;
    mouse.y = -((e.clientY - rect.top) / rect.height) * 2 + 1;

    raycaster.setFromCamera(mouse, state.camera);

    const hits = raycaster.intersectObjects(state.clickableMeshes, true);
    if (!hits.length) return;

    let obj = hits[0].object;
    let data = obj.userData || {};

    while ((!data || (!data.clickKind && !data.occupant_id && data.x === undefined)) && obj.parent) {
      obj = obj.parent;
      data = obj.userData || {};
    }

    if (data.occupant_id && state.inputIds.target) {
      Shiny.setInputValue(state.inputIds.target, {
        actor_id: String(data.occupant_id),
        actor_type: String(data.occupant_type || "actor"),
        x: Number(data.x),
        y: Number(data.y),
        nonce: Math.random()
      }, { priority: "event" });
      return;
    }

    if (data.x !== undefined && data.y !== undefined && state.inputIds.move) {
      Shiny.setInputValue(state.inputIds.move, {
        x: Number(data.x),
        y: Number(data.y),
        nonce: Math.random()
      }, { priority: "event" });
    }
  });
}

function buildCombatTerrain(tiles) {
  const state = window.combat3dState;

  state.terrainGroup.clear();
  state.decorGroup.clear();
  state.tileMap = {};
  state.clickableMeshes = [];

  const xs = tiles.map(t => Number(t.x));
  const ys = tiles.map(t => Number(t.y));

  const minX = Math.min(...xs);
  const maxX = Math.max(...xs);
  const minY = Math.min(...ys);
  const maxY = Math.max(...ys);

  state.centerX = (minX + maxX) / 2;
  state.centerY = (minY + maxY) / 2;

  tiles.forEach(tile => {
    const mesh = buildTerrainTile(tile);

    state.terrainGroup.add(mesh);
    state.tileMap[`${tile.x},${tile.y}`] = mesh;

    const clickMesh = buildTileClickPlane(tile);
    state.terrainGroup.add(clickMesh);
    state.clickableMeshes.push(clickMesh);

    generateDecor(tile, mesh.position.x, mesh.position.z);
  });

  if (state.groundMesh) {
    state.scene.remove(state.groundMesh);
  }

  const ground = new THREE.Mesh(
    new THREE.PlaneGeometry(maxX - minX + 14, maxY - minY + 14),
    new THREE.MeshStandardMaterial({
      color: 0x26301f,
      roughness: 1,
      metalness: 0
    })
  );

  ground.rotation.x = -Math.PI / 2;
  ground.position.y = -0.065;
  ground.receiveShadow = true;

  state.scene.add(ground);
  state.groundMesh = ground;
}

const textureLoader = new THREE.TextureLoader();

const grassTexture = textureLoader.load("assets/textures/grass.jpg");
grassTexture.wrapS = THREE.RepeatWrapping;
grassTexture.wrapT = THREE.RepeatWrapping;
grassTexture.repeat.set(1.4, 1.4);
grassTexture.colorSpace = THREE.SRGBColorSpace;

function loadTileTexture(path, repeatX = 1, repeatY = 1) {
  const tex = textureLoader.load(path);
  tex.wrapS = THREE.RepeatWrapping;
  tex.wrapT = THREE.RepeatWrapping;
  tex.repeat.set(repeatX, repeatY);
  tex.colorSpace = THREE.SRGBColorSpace;
  return tex;
}

const barkTexture = textureLoader.load("assets/textures/bark.jpg");
const leavesTexture = textureLoader.load("assets/textures/leaves.jpg");

[barkTexture, leavesTexture].forEach(tex => {
  tex.wrapS = THREE.RepeatWrapping;
  tex.wrapT = THREE.RepeatWrapping;
  tex.repeat.set(1, 1);
  tex.colorSpace = THREE.SRGBColorSpace;
});

const tileTextures = {
  grass: loadTileTexture("assets/textures/grass.jpg"),
  forest: loadTileTexture("assets/textures/forest.jpg"),
  woodland: loadTileTexture("assets/textures/forest.jpg"),
  road: loadTileTexture("assets/textures/dirt.jpg"),
  stone: loadTileTexture("assets/textures/stone.jpg"),
  wall: loadTileTexture("assets/textures/stone.jpg"),
  water: loadTileTexture("assets/textures/water.jpg", 2, 2),
  swamp: loadTileTexture("assets/textures/swamp.jpg"),
  pit: loadTileTexture("assets/textures/ravine.jpg"),
  ravine: loadTileTexture("assets/textures/ravine.jpg")
};

function buildTileClickPlane(tile) {
  const state = window.combat3dState;
  const terrain = String(tile.terrain || "grass").toLowerCase();
  const clickY = terrainHeight(terrain) + TILE_CLICK_Y_OFFSET;

  const clickMesh = new THREE.Mesh(
  new THREE.PlaneGeometry(1.12, 1.12),
    new THREE.MeshBasicMaterial({
      transparent: true,
      opacity: 0,
      depthWrite: false,
      side: THREE.DoubleSide
    })
  );

  clickMesh.rotation.x = -Math.PI / 2;
  clickMesh.position.set(
    Number(tile.x) - state.centerX,
    clickY,
    Number(tile.y) - state.centerY
  );

  clickMesh.userData = {
    ...tile,
    clickKind: "tile",
    x: Number(tile.x),
    y: Number(tile.y)
  };

  return clickMesh;
}

function addCombatBackdrop(scene) {
  const room = new THREE.Mesh(
    new THREE.BoxGeometry(90, 36, 90),
    new THREE.MeshBasicMaterial({
      color: 0x1f2730,
      side: THREE.BackSide
    })
  );

  room.position.y = 12;
  scene.add(room);

  const floor = new THREE.Mesh(
    new THREE.CircleGeometry(44, 64),
    new THREE.MeshStandardMaterial({
      color: 0x1f2b1d,
      roughness: 1,
      metalness: 0
    })
  );

  floor.rotation.x = -Math.PI / 2;
  floor.position.y = -0.08;
  floor.receiveShadow = true;
  scene.add(floor);
}

function buildTerrainTile(tile) {
  const state = window.combat3dState;

  const terrain = String(tile.terrain || "grass").toLowerCase();
  const h = terrainHeight(terrain);
  const blockHeight = Math.max(0.12, Math.abs(h) + 0.22);

  const geo =   terrain === "stone" || terrain === "wall"     ? new THREE.BoxGeometry(0.96, blockHeight, 0.96, 2, 2, 2)     : new THREE.BoxGeometry(0.98, blockHeight, 0.98, 1, 1, 1);

  const baseColor = new THREE.Color(terrainColor(terrain));
  const variance = (seededRandom(tile.x, tile.y, 999) - 0.5) * terrainColourVariance(terrain);

  baseColor.offsetHSL(
    variance * 0.12,
    variance * 0.08,
    variance
  );

  const reachable = isTruthy(tile.is_reachable);


const tex = getTerrainTexture(terrain, tile);

const mat = new THREE.MeshStandardMaterial({
  map: tex,
  color: tex ? 0xffffff : baseColor,
  roughness: terrain === "water" ? 0.42 : 0.94,
  metalness: terrain === "water" ? 0.04 : 0,
  emissive: reachable ? 0x143d18 : 0x000000,
  emissiveIntensity: reachable ? 0.16 : 0.018
});

  const mesh = new THREE.Mesh(geo, mat);
  
  if (terrain === "stone" || terrain === "wall") {
  mesh.scale.set(0.98, 1, 0.98);
}

  mesh.position.set(
    Number(tile.x) - state.centerX,
    h / 2,
    Number(tile.y) - state.centerY
  );

  mesh.castShadow = true;
  mesh.receiveShadow = true;
  mesh.userData = {
    ...tile,
    clickKind: "tile",
    x: Number(tile.x),
    y: Number(tile.y)
  };

  if (terrain === "pit" || terrain === "ravine") {
    mesh.material.color.setHex(terrain === "ravine" ? 0x101010 : 0x15110f);
    mesh.material.emissive.setHex(0x000000);
    mesh.material.emissiveIntensity = 0;
    createRavineWalls(mesh.position.x, mesh.position.z, h);
  }

  return mesh;
}

function updateReachableHighlights(tiles) {
  const state = window.combat3dState;

  tiles.forEach(tile => {
    const key = `${tile.x},${tile.y}`;
    const mesh = state.tileMap[key];
    if (!mesh || !mesh.material) return;

    const terrain = String(tile.terrain || "grass").toLowerCase();

    if (isTruthy(tile.is_pending_move)) {
      mesh.material.emissive.setHex(0xffcc33);
      mesh.material.emissiveIntensity = 0.42;
    } else if (isTruthy(tile.is_reachable)) {
      mesh.material.emissive.setHex(0x143d18);
      mesh.material.emissiveIntensity = 0.16;
    } else if (terrain === "water") {
      mesh.material.emissive.setHex(0x0e263d);
      mesh.material.emissiveIntensity = 0.11;
    } else {
      mesh.material.emissive.setHex(0x000000);
      mesh.material.emissiveIntensity = 0.018;
    }
  });
}


function generateDecor(tile, x, z) {
  const terrain = String(tile.terrain || "grass").toLowerCase();

  generateSurfacePatch(tile, x, z);

  if (terrain === "forest" || terrain === "woodland") {
const count =
  seededRandom(tile.x, tile.y, 1) > 0.7 ? 2 : 1;

    for (let i = 0; i < count; i++) {
      const ox = (seededRandom(tile.x, tile.y, i + 2) - 0.5) * 0.78;
      const oz = (seededRandom(tile.x, tile.y, i + 12) - 0.5) * 0.78;
      if (Math.abs(ox) < 0.18 && Math.abs(oz) < 0.18) continue;
      const scale = 1.8 + seededRandom(tile.x, tile.y, i + 20) * 1.4;

      window.combat3dState.decorGroup.add(createTree(x + ox, z + oz, scale));
    }
  }

  if (terrain === "stone" || terrain === "wall") {
    if (seededRandom(tile.x, tile.y, 10) > 0.55) {
      window.combat3dState.decorGroup.add(createRock(x, z, 0.65));
    }
  }

  if (terrain === "water" || terrain === "swamp") {
    if (seededRandom(tile.x, tile.y, 50) > 0.45) {
      window.combat3dState.decorGroup.add(createReeds(x, z));
    }
  }
}

function generateSurfacePatch(tile, x, z) {
  const state = window.combat3dState;
  const terrain = String(tile.terrain || "grass").toLowerCase();
  const topY = terrainHeight(terrain) + 0.035;

  if (terrain === "grass" || terrain === "forest" || terrain === "woodland") {
    const count = terrain === "grass" ? 2 : 3;

    for (let i = 0; i < count; i++) {
      const blade = new THREE.Mesh(
        new THREE.ConeGeometry(0.04, 0.22, 5),
        new THREE.MeshStandardMaterial({
          color: terrain === "grass" ? 0x75aa5a : 0x2e5f32,
          roughness: 1
        })
      );

      blade.position.set(
        x + (seededRandom(tile.x, tile.y, i + 300) - 0.5) * 0.65,
        topY + 0.075,
        z + (seededRandom(tile.x, tile.y, i + 400) - 0.5) * 0.65
      );

      blade.rotation.x = (seededRandom(tile.x, tile.y, i + 500) - 0.5) * 0.35;
      blade.rotation.z = (seededRandom(tile.x, tile.y, i + 600) - 0.5) * 0.35;

      state.decorGroup.add(blade);
    }
  }

  if (terrain === "road") {
    const streak = new THREE.Mesh(
      new THREE.PlaneGeometry(0.86, 0.28),
      new THREE.MeshBasicMaterial({
        color: 0x8b7046,
        transparent: true,
        opacity: 0.35,
        depthWrite: false,
        side: THREE.DoubleSide
      })
    );

    streak.rotation.x = -Math.PI / 2;
    streak.rotation.z = seededRandom(tile.x, tile.y, 700) * Math.PI;
    streak.position.set(x, topY + 0.012, z);

    state.decorGroup.add(streak);
  }

  if (terrain === "water") {
    const sheen = new THREE.Mesh(
      new THREE.PlaneGeometry(0.9, 0.9),
      new THREE.MeshBasicMaterial({
        color: 0x95d8ff,
        transparent: true,
        opacity: 0.12,
        blending: THREE.AdditiveBlending,
        depthWrite: false,
        side: THREE.DoubleSide
      })
    );

    sheen.rotation.x = -Math.PI / 2;
    sheen.position.set(x, topY + 0.02, z);
    sheen.userData.isWaterSheen = true;
    sheen.userData.phase = seededRandom(tile.x, tile.y, 900) * Math.PI * 2;

    state.decorGroup.add(sheen);
  }
}

function spawnCombatTokens(tiles) {
  const state = window.combat3dState;

  state.tokenGroup.clear();
  state.tokenMap = {};

  tiles.forEach(tile => {
    if (!tile.occupant_id) return;

    const token = createToken(tile);
    state.tokenGroup.add(token);
    state.tokenMap[String(tile.occupant_id)] = token;
    state.clickableMeshes.push(token);
  });
}

function applyTokenMarkerAppearance(token, tile) {
  if (!token || !token.userData) return;
  const isPlayer = String(tile.occupant_type || token.userData.occupant_type || "") === "player";
  if (!isPlayer) return;

  const rawColour = String(tile.marker_3d_color || token.userData.marker_3d_color || "#77ddff");
  const colour = /^#[0-9a-f]{6}$/i.test(rawColour) ? parseInt(rawColour.slice(1), 16) : 0x77ddff;
  const style = ["wisps", "beacon", "subtle"].includes(String(tile.marker_3d_style))
    ? String(tile.marker_3d_style)
    : String(token.userData.marker_3d_style || "wisps");

  [token.userData.beamGroup, token.userData.spiralGroup, token.userData.particleGroup, token.userData.glowOrb].forEach(part => {
    if (!part) return;
    part.traverse(obj => {
      const materials = Array.isArray(obj.material) ? obj.material : (obj.material ? [obj.material] : []);
      materials.forEach(material => { if (material.color) material.color.setHex(colour); });
    });
  });

  if (token.userData.beamGroup) token.userData.beamGroup.visible = style !== "subtle";
  if (token.userData.spiralGroup) token.userData.spiralGroup.visible = style === "wisps";
  if (token.userData.particleGroup) token.userData.particleGroup.visible = style === "wisps";
  if (token.userData.glowOrb) {
    token.userData.glowOrb.visible = true;
    token.userData.glowOrb.material.opacity = style === "subtle" ? 0.32 : 0.68;
  }
  if (token.userData.wispLight) {
    token.userData.wispLight.color.setHex(colour);
    token.userData.wispLight.intensity = style === "subtle" ? 0.16 : (isTruthy(tile.is_active_actor) ? 0.85 : 0.34);
  }
  token.userData.marker_3d_color = rawColour;
  token.userData.marker_3d_style = style;
}

function updateCombatTokens(tiles) {
  const state = window.combat3dState;
  const seen = {};

  tiles.forEach(tile => {
    if (!tile.occupant_id) return;

    const id = String(tile.occupant_id);
    seen[id] = true;

    const targetX = Number(tile.x) - state.centerX;
    const targetZ = Number(tile.y) - state.centerY;
    const terrain = String(tile.terrain || "grass").toLowerCase();
    const targetY = terrainHeight(terrain) + 0.05;

    let token = state.tokenMap[id];

    if (!token) {
      token = createToken(tile);
      state.tokenGroup.add(token);
      state.tokenMap[id] = token;
      state.clickableMeshes.push(token);
    }

    const oldTargetX = Number(token.userData.targetX ?? token.position.x);
    const oldTargetZ = Number(token.userData.targetZ ?? token.position.z);

    const moveDX = targetX - oldTargetX;
    const moveDZ = targetZ - oldTargetZ;

    if (Math.abs(moveDX) > 0.01 || Math.abs(moveDZ) > 0.01) {
      token.userData.targetRotationY = Math.atan2(moveDX, moveDZ) + Math.PI;
    }

    token.userData.targetX = targetX;
    token.userData.targetY = targetY;
    token.userData.targetZ = targetZ;

    token.userData.is_active_actor = isTruthy(tile.is_active_actor);
    token.userData.x = Number(tile.x);
    token.userData.y = Number(tile.y);
    applyTokenMarkerAppearance(token, tile);

    if (token.userData.hitbox) {
      token.userData.hitbox.userData = {
        ...token.userData.hitbox.userData,
        ...tile,
        clickKind: "token",
        occupant_id: tile.occupant_id,
        occupant_type: tile.occupant_type || "actor",
        x: Number(tile.x),
        y: Number(tile.y)
      };
    }

    if (token.userData.activeRing) {
      token.userData.activeRing.material.opacity =
        isTruthy(tile.is_active_actor) ? 0.85 : 0;
    }
  });

  Object.keys(state.tokenMap).forEach(id => {
    if (seen[id]) return;

    const token = state.tokenMap[id];
    state.tokenGroup.remove(token);
    delete state.tokenMap[id];
  });
}

function createToken(tile) {
  const state = window.combat3dState;

  const terrain = String(tile.terrain || "grass").toLowerCase();
  const y = terrainHeight(terrain) + 0.05;

  const isPlayer = String(tile.occupant_type || "") === "player";
  const mainColor = isPlayer ? 0x77ddff : 0xff7744;
  const glowColor = isPlayer ? 0x44ccff : 0xff5533;

  const token = new THREE.Group();

  const hitbox = new THREE.Mesh(
  new THREE.CylinderGeometry(0.58, 0.58, 1.35, 18),
    new THREE.MeshBasicMaterial({
      transparent: true,
      opacity: 0,
      depthWrite: false
    })
  );

hitbox.position.y = 0.68;
  hitbox.userData = {
    ...tile,
    clickKind: "token",
    occupant_id: tile.occupant_id,
    occupant_type: tile.occupant_type || "actor",
    x: Number(tile.x),
    y: Number(tile.y)
  };
  token.add(hitbox);

  const shadow = new THREE.Mesh(
    new THREE.CircleGeometry(0.32, 32),
    new THREE.MeshBasicMaterial({
      color: 0x000000,
      transparent: true,
      opacity: 0.18,
      depthWrite: false
    })
  );

  shadow.rotation.x = -Math.PI / 2;
  shadow.position.y = 0.01;
  token.add(shadow);

  const hpRing = new THREE.Mesh(
    new THREE.RingGeometry(0.32, 0.38, 40),
    new THREE.MeshBasicMaterial({
      color: 0x33ff66,
      side: THREE.DoubleSide,
      transparent: true,
      opacity: 0.82,
      depthWrite: false
    })
  );

  hpRing.rotation.x = -Math.PI / 2;
  hpRing.position.y = 0.025;
  token.add(hpRing);

  const activeRing = new THREE.Mesh(
    new THREE.RingGeometry(0.43, 0.52, 48),
    new THREE.MeshBasicMaterial({
      color: 0xffdd66,
      side: THREE.DoubleSide,
      transparent: true,
      opacity: isTruthy(tile.is_active_actor) ? 0.85 : 0,
      blending: THREE.AdditiveBlending,
      depthWrite: false
    })
  );

  activeRing.rotation.x = -Math.PI / 2;
  activeRing.position.y = 0.035;
  token.add(activeRing);

  const beamGroup = createFallbackBeam(mainColor);
  const spiralGroup = createFallbackSpiral(mainColor);
  const particleGroup = createFallbackParticles(tile);

  const glowOrb = new THREE.Mesh(
    new THREE.SphereGeometry(0.055, 16, 16),
    new THREE.MeshBasicMaterial({
      color: mainColor,
      transparent: true,
      opacity: 0.68,
      blending: THREE.AdditiveBlending,
      depthWrite: false
    })
  );

  glowOrb.position.y = 0.42;

  const light = new THREE.PointLight(
    glowColor,
    isTruthy(tile.is_active_actor) ? 0.85 : 0.34,
    isTruthy(tile.is_active_actor) ? 5 : 3,
    2
  );

  light.position.y = 0.46;

  token.add(beamGroup);
  token.add(spiralGroup);
  token.add(particleGroup);
  token.add(glowOrb);
  token.add(light);


  const tokenName = String(
    tile.occupant_name ||
    tile.name ||
    tile.enemy_name ||
    tile.actor_name ||
    ""
  ).toLowerCase();

  const isHeartTree =
    !isPlayer &&
    (
      tokenName.includes("heart tree") ||
      tokenName.includes("heart-tree") ||
      tokenName.includes("hearttree")
    );

  if (isHeartTree) {
    const heartTree = createHeartTreeBoss();

    token.add(heartTree);

    beamGroup.visible = false;
    spiralGroup.visible = false;
    particleGroup.visible = false;
    glowOrb.visible = false;

    hitbox.scale.set(2.8, 2.5, 2.8);
    hitbox.position.y = 1.2;

    hpRing.scale.set(2.4, 2.4, 2.4);
    activeRing.scale.set(2.7, 2.7, 2.7);

    light.position.y = 1.8;
    light.intensity = 1.4;
    light.distance = 7;
  }

  token.position.set(
    Number(tile.x) - state.centerX,
    y,
    Number(tile.y) - state.centerY
  );

  token.userData = {
    ...tile,
    clickKind: "token",
    occupant_id: tile.occupant_id,
    occupant_type: tile.occupant_type || "actor",
    x: Number(tile.x),
    y: Number(tile.y),

    targetX: token.position.x,
    targetY: token.position.y,
    targetZ: token.position.z,

    isPlayer,
    is_active_actor: isTruthy(tile.is_active_actor),
    phase: seededRandom(tile.x, tile.y, 999) * Math.PI * 2,

    hitbox,
    hpRing,
    activeRing,
    beamGroup,
    spiralGroup,
    particleGroup,
    glowOrb,
    wispLight: light
  };

  applyTokenMarkerAppearance(token, tile);

  if (isPlayer) {
    addCharacterModelToToken(token, tile);
  }

  return token;
}

function createFallbackBeam(mainColor) {
  const beamGroup = new THREE.Group();

  for (let i = 0; i < 4; i++) {
    const p = i / 3;

    const beam = new THREE.Mesh(
      new THREE.CylinderGeometry(
        0.025 + p * 0.025,
        0.025 + p * 0.025,
        0.68 - p * 0.04,
        18,
        1,
        true
      ),
      new THREE.MeshBasicMaterial({
        color: mainColor,
        transparent: true,
        opacity: 0.1 * (1 - p),
        blending: THREE.AdditiveBlending,
        depthWrite: false,
        side: THREE.DoubleSide
      })
    );

    beam.position.y = 0.38;
    beam.userData.baseOpacity = 0.1 * (1 - p);
    beamGroup.add(beam);
  }

  return beamGroup;
}

function createFallbackSpiral(mainColor) {
  const spiralGroup = new THREE.Group();

  for (let s = 0; s < 2; s++) {
    const points = [];

    for (let i = 0; i < 55; i++) {
      const p = i / 54;
      const angle = p * Math.PI * 3.8 + s * 2.1;
      const radius = 0.045 + p * 0.11;

      points.push(new THREE.Vector3(
        Math.cos(angle) * radius,
        0.12 + p * 0.52,
        Math.sin(angle) * radius
      ));
    }

    const geo = new THREE.BufferGeometry().setFromPoints(points);

    const line = new THREE.Line(
      geo,
      new THREE.LineBasicMaterial({
        color: mainColor,
        transparent: true,
        opacity: 0.28,
        blending: THREE.AdditiveBlending,
        depthWrite: false
      })
    );

    line.userData.spinSpeed = 0.003 + s * 0.002;
    spiralGroup.add(line);
  }

  return spiralGroup;
}

function createFallbackParticles(tile) {
  const particleGroup = new THREE.Group();

  for (let i = 0; i < 5; i++) {
    const particle = new THREE.Mesh(
      new THREE.SphereGeometry(0.014, 8, 8),
      new THREE.MeshBasicMaterial({
        color: 0xffffff,
        transparent: true,
        opacity: 0.5,
        blending: THREE.AdditiveBlending,
        depthWrite: false
      })
    );

    particle.position.set(
      (seededRandom(tile.x, tile.y, i + 100) - 0.5) * 0.32,
      0.14 + seededRandom(tile.x, tile.y, i + 200) * 0.52,
      (seededRandom(tile.x, tile.y, i + 300) - 0.5) * 0.32
    );

    particle.userData.baseY = particle.position.y;
    particle.userData.phase = seededRandom(tile.x, tile.y, i + 400) * Math.PI * 2;
    particleGroup.add(particle);
  }

  return particleGroup;
}

async function addCharacterModelToToken(token, tile) {
  const partsToLoad = [
    tile.model_base || tile.base_model || "",
    tile.model_hair || tile.hair_model || "",
    tile.model_body || tile.body_model || "",
    tile.model_arms || tile.arms_model || "",
    tile.model_legs || tile.legs_model || "",
    tile.model_feet || tile.feet_model || "",
    tile.model_headgear || tile.headgear_model || "",
    tile.model_accessory || tile.accessory_model || ""
  ].filter(path => path && path !== "");

  if (!partsToLoad.length) return;

  const characterGroup = new THREE.Group();

  const loadedParts = await Promise.allSettled(
    partsToLoad.map(path => loadCombatGLTFPromise(path))
  );

  loadedParts.forEach((result, index) => {
    const path = partsToLoad[index];

    if (result.status !== "fulfilled") {
      console.error("Failed combat character part:", path, result.reason);
      return;
    }

    const part = result.value;

    recolorCombatCharacterPart(part, {
      hairColor: tile.hair_color || tile.hairColor || "#3b2416"
    }, path);

    part.traverse(obj => {
      if (obj.isMesh) {
        obj.castShadow = true;
        obj.receiveShadow = true;
      }
    });

    characterGroup.add(part);
  });

  if (!characterGroup.children.length) return;

  centerAndScaleCombatModel(characterGroup);

  characterGroup.position.y = 0.02;
  characterGroup.rotation.y = Math.PI;

  token.add(characterGroup);
  token.userData.characterModel = characterGroup;

  applyTokenMarkerAppearance(token, tile);
}

function loadCombatGLTFPromise(path) {
  return new Promise((resolve, reject) => {
    combatGLTFLoader.load(
      encodeURI(path),
      gltf => resolve(gltf.scene),
      undefined,
      error => reject(error)
    );
  });
}

function centerAndScaleCombatModel(group) {
  const box = new THREE.Box3().setFromObject(group);
  const size = box.getSize(new THREE.Vector3());
  const center = box.getCenter(new THREE.Vector3());

  group.position.x -= center.x;
  group.position.z -= center.z;
  group.position.y -= box.min.y;

  if (size.y > 0) {
    const scale = MODEL_TARGET_HEIGHT / size.y;
    group.scale.setScalar(scale);
  }
}

function getCombatMaterials(obj) {
  if (!obj.material) return [];
  return Array.isArray(obj.material) ? obj.material : [obj.material];
}

function cloneCombatMaterial(obj) {
  if (Array.isArray(obj.material)) {
    obj.material = obj.material.map(mat => mat.clone());
  } else if (obj.material) {
    obj.material = obj.material.clone();
  }
}

function forceCombatMaterialColour(mat, colourHex) {
  if (!mat || !colourHex) return;

  if (mat.color) mat.color.set(colourHex);

  mat.map = null;
  mat.normalMap = null;
  mat.roughnessMap = null;
  mat.metalnessMap = null;
  mat.aoMap = null;
  mat.emissiveMap = null;
  mat.vertexColors = false;
  mat.needsUpdate = true;
}

function recolorCombatCharacterPart(part, message, path) {
  const hairColor = message.hairColor || "#3b2416";
  const pathText = String(path || "").toLowerCase();

  part.traverse(obj => {
    if (!obj.isMesh || !obj.material) return;

    const meshName = String(obj.name || "").toLowerCase();
    const materialNames = getCombatMaterials(obj)
      .map(mat => String(mat.name || "").toLowerCase())
      .join(" ");

    const combined = `${pathText} ${meshName} ${materialNames}`;

    const isHair =
      combined.includes("hair") ||
      combined.includes("beard");

    if (!isHair) return;

    cloneCombatMaterial(obj);

    getCombatMaterials(obj).forEach(mat => {
      forceCombatMaterialColour(mat, hairColor);
    });
  });
}

function animateCombat3D() {
  const state = window.combat3dState;

  if (!state.renderer || !state.scene || !state.camera) return;

  state.animationFrame = requestAnimationFrame(animateCombat3D);

  const t = performance.now() * 0.001;
  
  if (tileTextures.water) {
  tileTextures.water.offset.x += 0.00025;
  tileTextures.water.offset.y += 0.00012;
}

  Object.values(state.tokenMap).forEach(token => {
    if (!token.userData) return;

    token.position.x += (token.userData.targetX - token.position.x) * 0.14;
    token.position.y += (token.userData.targetY - token.position.y) * 0.14;
    token.position.z += (token.userData.targetZ - token.position.z) * 0.14;

if (typeof token.userData.targetRotationY === "number") {

  let diff = token.userData.targetRotationY - token.rotation.y;

  while (diff > Math.PI) diff -= Math.PI * 2;

  while (diff < -Math.PI) diff += Math.PI * 2;

  token.rotation.y += diff * 0.16;

}

    const active = !!token.userData.is_active_actor;
    const phase = token.userData.phase || 0;

    if (token.userData.beamGroup) {
      token.userData.beamGroup.rotation.y += 0.005;
      token.userData.beamGroup.children.forEach((child, i) => {
        if (!child.material) return;
        const base = child.userData.baseOpacity || 0.06;
        child.material.opacity = base + Math.sin(t * 2.3 + i + phase) * 0.012;
      });
    }

    if (token.userData.spiralGroup) {
      token.userData.spiralGroup.rotation.y += active ? 0.012 : 0.006;
    }

    if (token.userData.particleGroup) {
      token.userData.particleGroup.children.forEach(p => {
        const pPhase = p.userData.phase || 0;
        p.position.y = p.userData.baseY + Math.sin(t * 1.8 + pPhase) * 0.045;

        if (p.material) {
          p.material.opacity = 0.28 + Math.sin(t * 3 + pPhase) * 0.14;
        }
      });
    }

    if (token.userData.wispLight) {
      token.userData.wispLight.intensity =
        active
          ? 0.72 + Math.sin(t * 5 + phase) * 0.18
          : 0.3 + Math.sin(t * 2 + phase) * 0.06;
    }

    if (token.userData.activeRing) {
      token.userData.activeRing.material.opacity =
        active
          ? 0.48 + Math.sin(t * 5 + phase) * 0.28
          : 0;
    }
    
    

    if (token.userData.hpRing) {
      token.userData.hpRing.rotation.z += 0.008;
    }
  });

  animateMoodObjects(t);

  if (state.scene.fog && typeof state.scene.fog.density === "number") {
    state.scene.fog.density = 0.0042 + Math.sin(t * 0.08) * 0.00045;
  }

  if (state.controls) state.controls.update();

  state.renderer.render(state.scene, state.camera);
}

function animateMoodObjects(t) {
  const state = window.combat3dState;
  if (!state.scene) return;

  state.scene.traverse(obj => {
    if (obj.userData && obj.userData.isLightShaft && obj.material) {
      const phase = obj.userData.phase || 0;
      obj.material.opacity = 0.025 + Math.sin(t * 0.7 + phase) * 0.01;
    }

    if (obj.userData && obj.userData.isWaterSheen && obj.material) {
      const phase = obj.userData.phase || 0;
      obj.material.opacity = 0.09 + Math.sin(t * 1.4 + phase) * 0.03;
      obj.rotation.z += 0.001;
    }
  });
}

function resetCombatCamera(mapData = []) {
  const state = window.combat3dState;
  if (!state.camera || !state.controls) return;

  const xs = mapData.map(t => Number(t.x)).filter(Number.isFinite);
  const ys = mapData.map(t => Number(t.y)).filter(Number.isFinite);

  const width = xs.length ? Math.max(...xs) - Math.min(...xs) + 1 : 18;
  const depth = ys.length ? Math.max(...ys) - Math.min(...ys) + 1 : 18;
  const size = Math.max(width, depth);

  const dist = Math.max(16, Math.min(58, size * 1.35));

  state.controls.target.set(0, 0, 0);
  state.camera.position.set(dist, dist * 0.8, dist);

  state.controls.update();
}

function getMapCenterVector() {
  const state = window.combat3dState;
  const meshes = Object.values(state.tileMap || {});
  if (!meshes.length) return new THREE.Vector3(0, 0, 0);

  const box = new THREE.Box3();
  meshes.forEach(mesh => box.expandByObject(mesh));

  const center = new THREE.Vector3();
  box.getCenter(center);
  center.y = 0;
  return center;
}

function getActiveCombatToken() {
  const state = window.combat3dState;

  const tokens = Object.values(state.tokenMap || {});

  return tokens.find(token =>
    token &&
    token.userData &&
    isTruthy(token.userData.is_active_actor)
  ) || null;
}

function getMapWorldCenter() {
  return new THREE.Vector3(0, 0, 0);
}

function getTokenFacingVector(token) {
  const y = token?.userData?.targetRotationY ?? token?.rotation?.y ?? 0;

  return new THREE.Vector3(
    Math.sin(y),
    0,
    Math.cos(y)
  ).normalize();
}


function cloneTextureForTile(texture, tile) {
  if (!texture) return null;

  const t = texture.clone();
  t.wrapS = THREE.RepeatWrapping;
  t.wrapT = THREE.RepeatWrapping;
  t.repeat.set(1, 1);
  t.rotation = Math.floor(seededRandom(tile.x, tile.y, 888) * 4) * Math.PI / 2;
  t.center.set(0.5, 0.5);
  t.needsUpdate = true;

  return t;
}

function getTerrainTexture(terrain, tile) {
  const texture = tileTextures[terrain] || tileTextures.grass;
  return cloneTextureForTile(texture, tile);
}

function setCombatCameraMode(mode) {
  const state = window.combat3dState;
  if (!state.camera || !state.controls) return;

  const mapCenter = getMapCenterVector();
  const active = getActiveCombatToken();

  const actorPos = active
    ? active.position.clone()
    : mapCenter.clone();

  const facing = typeof active?.rotation?.y === "number"
    ? active.rotation.y
    : Math.PI / 4;

  // 1) Center = angled overview of the map center
  if (mode === "center") {
    const dist = 18;

    state.controls.target.copy(mapCenter);

    state.camera.position.set(
      mapCenter.x + dist,
      mapCenter.y + 15,
      mapCenter.z + dist
    );

    state.controls.update();
    onCombat3DResize();
    return;
  }

  // 2) Top = true top-down over map center
  if (mode === "top") {
    state.controls.target.copy(mapCenter);

    state.camera.position.set(
      mapCenter.x,
      mapCenter.y + 34,
      mapCenter.z + 0.01
    );

    state.controls.update();
    onCombat3DResize();
    return;
  }

  // 3) Character = top-down over active character
  if (mode === "character") {
    const target = actorPos.clone();
    target.y += 0.15;

    state.controls.target.copy(target);

    state.camera.position.set(
      target.x,
      target.y + 18,
      target.z + 0.01
    );

    state.controls.update();
    onCombat3DResize();
    return;
  }

  // 4) Shoulder = behind active character, looking where they face
  if (mode === "shoulder") {
    const target = actorPos.clone();
    target.y += 0.32;

    const forward = new THREE.Vector3(
      Math.sin(facing),
      0,
      Math.cos(facing)
    ).normalize();

    const back = forward.clone().multiplyScalar(-2.45);

    const side = new THREE.Vector3(
      Math.cos(facing),
      0,
      -Math.sin(facing)
    ).multiplyScalar(0.32);

    const cameraPos = target
      .clone()
      .add(back)
      .add(side)
      .add(new THREE.Vector3(0, 0.95, 0));

    const lookTarget = target
      .clone()
      .add(forward.clone().multiplyScalar(2.2))
      .add(new THREE.Vector3(0, 0.16, 0));

    state.camera.position.copy(cameraPos);
    state.controls.target.copy(lookTarget);

    state.controls.update();
    onCombat3DResize();
    return;
  }
}

function onCombat3DResize() {
  const state = window.combat3dState;

  const el = document.getElementById(state.containerId);
  if (!el || !state.camera || !state.renderer || !state.scene) return;

  const width = Math.max(1, el.clientWidth || 1);
  const height = Math.max(1, el.clientHeight || 1);

  state.camera.aspect = width / height;
  state.camera.updateProjectionMatrix();

  state.renderer.setSize(width, height, false);
  state.renderer.render(state.scene, state.camera);

  el.__combat3dRenderer = state.renderer;
  el.__combat3dCamera = state.camera;
  el.__combat3dScene = state.scene;
}

function startAnimationLoop() {
  const state = window.combat3dState;
  if (state.animationFrame) return;
  animateCombat3D();
}

function createTree(x, z, scale = 1) {
  const tree = new THREE.Group();

  const trunk = new THREE.Mesh(
    new THREE.CylinderGeometry(0.09 * scale, 0.14 * scale, 0.9 * scale, 8),
    new THREE.MeshStandardMaterial({
      map: barkTexture,
      color: 0xffffff,
      roughness: 1
    })
  );
  trunk.position.y = 0.275 * scale;
  trunk.castShadow = true;
  trunk.receiveShadow = true;

  const leafMat = new THREE.MeshStandardMaterial({
    map: leavesTexture,
    color: seededRandom(x, z, 44) > 0.5 ? 0xd8ffd0 : 0xc4f5bd,
    roughness: 1
  });

  const leaves = new THREE.Mesh(
    new THREE.ConeGeometry(0.38 * scale, 1.2 * scale, 10),
    leafMat
  );
  leaves.position.y = 1.05 * scale;
  leaves.castShadow = true;
  leaves.receiveShadow = true;

  const leaves2 = new THREE.Mesh(
    new THREE.ConeGeometry(0.22 * scale, 0.62 * scale, 10),
    leafMat.clone()
  );
 leaves2.position.y = 1.45 * scale;
  leaves2.castShadow = true;
  leaves2.receiveShadow = true;

  tree.add(trunk);
  tree.add(leaves);
  tree.add(leaves2);

  tree.rotation.y = seededRandom(x, z, 77) * Math.PI * 2;
  tree.position.set(x, 0.02, z);

  return tree;
}

function createRock(x, z, scale = 1) {

  const geo = new THREE.IcosahedronGeometry(0.22 * scale, 1);

  const pos = geo.attributes.position;

  for (let i = 0; i < pos.count; i++) {

    const vx = pos.getX(i);
    const vy = pos.getY(i);
    const vz = pos.getZ(i);

    const noise =
      0.82 +
      seededRandom(vx * 12, vz * 12, i) * 0.42;

    pos.setXYZ(
      i,
      vx * noise,
      vy * noise * 0.7,
      vz * noise
    );
  }

  pos.needsUpdate = true;
  geo.computeVertexNormals();

  const rock = new THREE.Mesh(
    geo,
    new THREE.MeshStandardMaterial({
      map: tileTextures.stone,
      color: 0xb8b1a3,
      roughness: 1,
      metalness: 0
    })
  );

  rock.rotation.set(
    seededRandom(x, z, 1) * 0.4,
    seededRandom(x, z, 2) * Math.PI * 2,
    seededRandom(x, z, 3) * 0.4
  );

  rock.scale.y *= 0.7;

  rock.position.set(x, 0.12 * scale, z);

  rock.castShadow = true;
  rock.receiveShadow = true;

  return rock;
}

function createReeds(x, z) {
  const reeds = new THREE.Group();

  for (let i = 0; i < 3; i++) {
    const reed = new THREE.Mesh(
      new THREE.CylinderGeometry(0.012, 0.018, 0.34, 5),
      new THREE.MeshStandardMaterial({ color: 0x8a8f3a, roughness: 1 })
    );

    reed.position.set(
      (seededRandom(x, z, i + 1) - 0.5) * 0.35,
      0.17,
      (seededRandom(x, z, i + 11) - 0.5) * 0.35
    );

    reeds.add(reed);
  }

  reeds.position.set(x, 0, z);
  return reeds;
}

function createRavineWalls(x, z, depth) {
  const state = window.combat3dState;
  if (!state.decorGroup) return;

  const wall = new THREE.Mesh(
    new THREE.BoxGeometry(1, Math.abs(depth), 0.18),
    new THREE.MeshStandardMaterial({
      color: 0x3a3128,
      roughness: 1
    })
  );

  wall.position.set(x, depth / 2, z - 0.5);
  state.decorGroup.add(wall);
}

function terrainColor(t) {
  t = String(t || "grass").toLowerCase();

  if (t === "grass") return 0x679750;
  if (t === "forest") return 0x315f38;
  if (t === "woodland") return 0x315f38;
  if (t === "water") return 0x2d6f99;
  if (t === "stone") return 0x85857f;
  if (t === "wall") return 0x55554f;
  if (t === "road") return 0xab8c5b;
  if (t === "swamp") return 0x4f6d46;
  if (t === "pit") return 0x111111;
  if (t === "ravine") return 0x0b0b0b;

  return 0x77705f;
}

function terrainColourVariance(t) {
  t = String(t || "grass").toLowerCase();

  if (t === "grass") return 0.05;
  if (t === "forest") return 0.04;
  if (t === "woodland") return 0.04;
  if (t === "road") return 0.038;
  if (t === "stone") return 0.032;
  if (t === "water") return 0.025;
  if (t === "swamp") return 0.038;

  return 0.025;
}

function terrainHeight(t) {
  t = String(t || "grass").toLowerCase();

  if (t === "wall") return 3.0;

  if (t === "grass") return 0;
  if (t === "forest") return 0;
  if (t === "woodland") return 0;

  if (t === "stone") return 1.0;
  if (t === "road") return 0.02;

  if (t === "water") return -0.16;
  if (t === "swamp") return -0.08;

  if (t === "pit") return -2.5;
  if (t === "ravine") return -4;

  return 0;
}

function seededRandom(x, y, salt = 1) {
  const n = Math.sin(
    Number(x) * 127.1 +
    Number(y) * 311.7 +
    salt * 74.7
  ) * 43758.5453;

  return n - Math.floor(n);
}



function isTruthy(x) {
  return x === true || x === "true" || x === 1 || x === "1";
}

function setupCameraButtonHandler() {
  document.addEventListener("click", function(e) {
    if (e.target.closest("[id$='camera_top']")) {
      setCombatCameraMode("top");
      return;
    }

    if (e.target.closest("[id$='camera_center']")) {
      setCombatCameraMode("center");
      return;
    }

    if (e.target.closest("[id$='camera_character']")) {
      setCombatCameraMode("character");
      return;
    }

    if (e.target.closest("[id$='camera_shoulder']")) {
      setCombatCameraMode("shoulder");
      return;
    }
  });
}

function setupFullscreenButtonHandler() {
  const state = window.combat3dState;
  if (state.fullscreenHandlerAttached) return;

  document.addEventListener("click", async function(e) {
    const btn = e.target.closest("[id$='map_3d_fullscreen']");
    if (!btn) return;

    e.preventDefault();
    e.stopPropagation();

    const nsPrefix = btn.id.replace("map_3d_fullscreen", "");
    const shell = document.getElementById(nsPrefix + "combat_3d_shell");

    if (!shell) {
      alert("Fullscreen failed: combat_3d_shell was not found.");
      return;
    }

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

      setTimeout(onCombat3DResize, 100);
      setTimeout(onCombat3DResize, 350);
      setTimeout(onCombat3DResize, 700);
    } catch (err) {
      console.error("Fullscreen failed:", err);
      alert("Fullscreen failed. Check browser console.");
    }
  });

  state.fullscreenHandlerAttached = true;
}

Shiny.addCustomMessageHandler("combat3d-init", function(message) {
  function tryInit(attemptsLeft) {
    const el = document.getElementById(message.containerId);

    if (!el) {
      if (attemptsLeft <= 0) {
        console.error("Missing container after retries", message.containerId);
        return;
      }

      setTimeout(() => tryInit(attemptsLeft - 1), 100);
      return;
    }

    window.renderCombat3D(
      message.containerId,
      message.mapData,
      message.inputIds || {}
    );
  }

  tryInit(80);
});

Shiny.addCustomMessageHandler("combat3d-update-tokens", function(message) {
  const state = window.combat3dState;
  if (!state.initialized) return;

  const tokens = message.tokens || [];

  tokens.forEach(t => {
    const id = String(t.actor_id);
    const token = state.tokenMap[id];
    if (!token) return;

    const oldX = token.position.x;
    const oldZ = token.position.z;

    const terrain = String(t.terrain || "grass").toLowerCase();
    const targetX = Number(t.x) - state.centerX;
    const targetZ = Number(t.y) - state.centerY;
    const targetY = terrainHeight(terrain) + 0.05;

    token.userData.targetX = targetX;
    token.userData.targetY = targetY;
    token.userData.targetZ = targetZ;

    const moveDX = targetX - oldX;
    const moveDZ = targetZ - oldZ;

    if (Math.abs(moveDX) > 0.01 || Math.abs(moveDZ) > 0.01) {
      token.userData.targetRotationY = Math.atan2(moveDX, moveDZ) + Math.PI;
    }

    token.userData.is_active_actor = isTruthy(t.is_active_actor);
    token.userData.x = Number(t.x);
    token.userData.y = Number(t.y);
    applyTokenMarkerAppearance(token, t);

    if (token.userData.hitbox) {
      token.userData.hitbox.userData = {
        ...token.userData.hitbox.userData,
        clickKind: "token",
        occupant_id: t.actor_id,
        occupant_type: t.actor_type,
        x: Number(t.x),
        y: Number(t.y)
      };
    }
  });
});

Shiny.addCustomMessageHandler("combat3d-resize", function(message) {
  const state = window.combat3dState;

  if (message && message.containerId) {
    state.containerId = message.containerId;
  }

  setTimeout(onCombat3DResize, 50);
  setTimeout(onCombat3DResize, 200);
  setTimeout(onCombat3DResize, 500);
});

Shiny.addCustomMessageHandler("combat3d-browser-fullscreen", function(message) {
  const shell = document.getElementById(message.shellId);
  if (!shell) return;

  if (!document.fullscreenElement) {
    shell.requestFullscreen?.();
  } else {
    document.exitFullscreen?.();
  }

  setTimeout(onCombat3DResize, 120);
  setTimeout(onCombat3DResize, 400);
});
