import * as THREE from "three";
import { GLTFLoader } from "three/addons/loaders/GLTFLoader.js";

window.THREE = THREE;

const combatGLTFLoader = new GLTFLoader();

window.combat3dState = {
  initialized: false,

  scene: null,
  camera: null,
  renderer: null,

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

  angle: 0.7,
  radius: 24,
  cameraHeight: 14
};

function normaliseMapData(mapData) {
  if (typeof mapData === "string") {
    mapData = JSON.parse(mapData);
  }

  if (Array.isArray(mapData)) {
    return mapData;
  }

  // Shiny sometimes sends data.frames as column objects:
  // { x:[...], y:[...], terrain:[...] }
  if (mapData && typeof mapData === "object") {
    const keys = Object.keys(mapData);
    const rowCount = Math.max(...keys.map(k => Array.isArray(mapData[k]) ? mapData[k].length : 0));

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

window.renderCombat3D = function(containerId, mapData, inputIds = {}) {

  console.log("renderCombat3D called", { containerId, mapData, inputIds });

  mapData = normaliseMapData(mapData);

  if (!Array.isArray(mapData) || mapData.length === 0) {
    console.error("mapData is empty or invalid", mapData);
    return;
  }

  if (!window.THREE) {
    console.error("THREE not loaded");
    return;
  }

  const state = window.combat3dState;

  const el = document.getElementById(containerId);

  if (!el) {
    console.error("Missing container", containerId);
    return;
  }

  if (!state.initialized) {

    initCombat3D(containerId, inputIds);

    buildCombatTerrain(mapData);

    spawnCombatTokens(mapData);

    state.initialized = true;

  } else {

    updateCombatTokens(mapData);
  }

  updateCamera();
};

function setupControls() {
  const state = window.combat3dState;
  const renderer = state.renderer;

  if (!renderer || !renderer.domElement) return;

  renderer.domElement.addEventListener("wheel", function(e) {
    e.preventDefault();

    state.radius += e.deltaY * 0.015;
    state.radius = Math.max(8, Math.min(55, state.radius));

    updateCamera();
  });

  renderer.domElement.addEventListener("mousemove", function(e) {
    if (e.buttons !== 1) return;

    state.angle += e.movementX * 0.008;
    state.cameraHeight -= e.movementY * 0.03;
    state.cameraHeight = Math.max(4, Math.min(30, state.cameraHeight));

    updateCamera();
  });

  setupClickHandler();
}

function setupClickHandler() {
  const state = window.combat3dState;

  const raycaster = new THREE.Raycaster();
  const mouse = new THREE.Vector2();

  state.renderer.domElement.addEventListener("click", function(e) {
    const rect = state.renderer.domElement.getBoundingClientRect();

    mouse.x = ((e.clientX - rect.left) / rect.width) * 2 - 1;
    mouse.y = -((e.clientY - rect.top) / rect.height) * 2 + 1;

    raycaster.setFromCamera(mouse, state.camera);

    const hits = raycaster.intersectObjects(state.clickableMeshes, true);
    if (!hits.length) return;

    let obj = hits[0].object;
    let data = obj.userData || {};

    if (!data.occupant_id && obj.parent && obj.parent.userData) {
      data = obj.parent.userData;
    }

    if (data.occupant_id && state.inputIds.target) {
      Shiny.setInputValue(state.inputIds.target, {
        actor_id: String(data.occupant_id),
        actor_type: String(data.occupant_type || "actor"),
        nonce: Math.random()
      }, { priority: "event" });
    } else if (state.inputIds.move) {
      Shiny.setInputValue(state.inputIds.move, {
        x: Number(data.x),
        y: Number(data.y),
        nonce: Math.random()
      }, { priority: "event" });
    }
  });
}

function initCombat3D(containerId, inputIds) {

  const state = window.combat3dState;

  state.containerId = containerId;
  state.inputIds = inputIds;

  const el = document.getElementById(containerId);

  if (!el || !window.THREE) return;

  el.innerHTML = "";

  const width = el.clientWidth || 1200;
  const height = el.clientHeight || 700;

  const scene = new THREE.Scene();
  scene.background = new THREE.Color(0x101216);
  scene.fog = new THREE.Fog(0x101216, 20, 70);

  const camera = new THREE.PerspectiveCamera(
    55,
    width / height,
    0.1,
    2000
  );

  const renderer = new THREE.WebGLRenderer({
    antialias: true
  });

  renderer.setSize(width, height);
  renderer.setPixelRatio(window.devicePixelRatio || 1);

  renderer.shadowMap.enabled = true;
  renderer.shadowMap.type = THREE.PCFSoftShadowMap;
  
  renderer.outputEncoding = THREE.sRGBEncoding;

renderer.toneMapping = THREE.ACESFilmicToneMapping;
renderer.toneMappingExposure = 1.15;

renderer.physicallyCorrectLights = true;

  el.appendChild(renderer.domElement);

  const worldGroup = new THREE.Group();
  const terrainGroup = new THREE.Group();
  const decorGroup = new THREE.Group();
  const tokenGroup = new THREE.Group();

  worldGroup.add(terrainGroup);
  worldGroup.add(decorGroup);
  worldGroup.add(tokenGroup);

  scene.add(worldGroup);

  // =====================================
// AMBIENT WORLD LIGHT
// =====================================

const hemi = new THREE.HemisphereLight(
  0x7fa4c9,   // cool sky
  0x1a120d,   // warm ground bounce
  0.42
);

scene.add(hemi);

// =====================================
// MOON / SUN DIRECTIONAL
// =====================================

const sun = new THREE.DirectionalLight(
  0xffd7a8,
  1.35
);

sun.position.set(18, 32, 14);

sun.castShadow = true;

sun.shadow.mapSize.width = 4096;
sun.shadow.mapSize.height = 4096;

sun.shadow.camera.near = 0.5;
sun.shadow.camera.far = 120;

sun.shadow.bias = -0.0004;

scene.add(sun);

// =====================================
// LOW FILL LIGHT
// =====================================

const fill = new THREE.PointLight(
  0x6ea8ff,
  0.35,
  80
);

fill.position.set(-12, 10, -8);

scene.add(fill);

// =====================================
// ATMOSPHERIC FOG
// =====================================

scene.fog = new THREE.FogExp2(
  0x0f1318,
  0.028
);

  state.scene = scene;
  state.camera = camera;
  state.renderer = renderer;

  state.worldGroup = worldGroup;
  state.terrainGroup = terrainGroup;
  state.decorGroup = decorGroup;
  state.tokenGroup = tokenGroup;

  setupControls();

  startAnimationLoop();

  window.addEventListener("resize", onCombat3DResize);
  

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

    const clickMesh = new THREE.Mesh(
      new THREE.BoxGeometry(1, 3, 1),
      new THREE.MeshBasicMaterial({
        transparent: true,
        opacity: 0,
        depthWrite: false
      })
    );

    clickMesh.position.set(
      Number(tile.x) - state.centerX,
      1,
      Number(tile.y) - state.centerY
    );

    clickMesh.userData = tile;
    state.terrainGroup.add(clickMesh);
    state.clickableMeshes.push(clickMesh);

    generateDecor(tile, mesh.position.x, mesh.position.z);
  });

  if (state.groundMesh) {
    state.scene.remove(state.groundMesh);
  }

  const ground = new THREE.Mesh(
    new THREE.PlaneGeometry(maxX - minX + 10, maxY - minY + 10),
    new THREE.MeshStandardMaterial({
      color: 0x11140f,
      roughness: 1
    })
  );

  ground.rotation.x = -Math.PI / 2;
  ground.position.y = -0.06;
  ground.receiveShadow = true;

  state.scene.add(ground);
  state.groundMesh = ground;
}

function buildTerrainTile(tile) {
  const state = window.combat3dState;

  const terrain = String(tile.terrain || "grass").toLowerCase();
  const height = terrainHeight(terrain);

  const tileHeight = Math.max(0.08, Math.abs(height) + 0.22);

  const surfaceNoise =
    terrain === "water" ? 0.012 :
    terrain === "road" ? 0.028 :
    terrain === "stone" ? 0.075 :
    terrain === "forest" ? 0.11 :
    terrain === "swamp" ? 0.085 :
    terrain === "pit" || terrain === "ravine" ? 0.035 :
    0.055;

  const geo = new THREE.BoxGeometry(
    0.96,
    tileHeight,
    0.96,
    6,
    1,
    6
  );

  const pos = geo.attributes.position;

  for (let i = 0; i < pos.count; i++) {
    const vx = pos.getX(i);
    const vy = pos.getY(i);
    const vz = pos.getZ(i);

const edgeFalloff = 0;

    if (vy > 0) {
const worldX = Number(tile.x) + vx;
const worldZ = Number(tile.y) + vz;

const n1 =
  (seededRandom(Math.floor(worldX * 2), Math.floor(worldZ * 2), 77) - 0.5) *
  surfaceNoise;

const n2 =
  (seededRandom(Math.floor(worldX * 5), Math.floor(worldZ * 5), 123) - 0.5) *
  surfaceNoise *
  0.35;

const n = n1 + n2;

      pos.setY(i, vy + n + edgeFalloff);
    }
  }

  geo.computeVertexNormals();

  const baseColor = new THREE.Color(terrainColor(terrain));

  const colorVariance =
    terrain === "forest" ? 0.12 :
    terrain === "grass" ? 0.10 :
    terrain === "road" ? 0.08 :
    terrain === "stone" ? 0.06 :
    terrain === "swamp" ? 0.07 :
    0.04;

  const variance =
    (seededRandom(tile.x, tile.y, 999) - 0.5) *
    colorVariance;

  baseColor.offsetHSL(
    variance * 0.25,
    variance * 0.15,
    variance
  );

  const mat = new THREE.MeshStandardMaterial({
    color: baseColor,
    roughness:
      terrain === "water" ? 0.35 :
      terrain === "stone" ? 0.95 :
      terrain === "road" ? 1 :
      0.9,
    metalness: terrain === "water" ? 0.08 : 0.02,
    emissive:
      terrain === "water" ? 0x102444 :
      terrain === "forest" ? 0x071707 :
      tile.is_reachable ? 0x102810 :
      0x000000,
    emissiveIntensity:
      terrain === "water" ? 0.22 :
      terrain === "forest" ? 0.08 :
      tile.is_reachable ? 0.12 :
      0.015
  });

  const mesh = new THREE.Mesh(geo, mat);

  mesh.position.set(
    Number(tile.x) - state.centerX,
    height / 2,
    Number(tile.y) - state.centerY
  );

  mesh.castShadow = true;
  mesh.receiveShadow = true;
  mesh.userData = tile;

  if (terrain === "pit" || terrain === "ravine") {
    mesh.material.color.setHex(terrain === "ravine" ? 0x101010 : 0x15110f);
    mesh.material.emissive.setHex(0x000000);
    mesh.material.emissiveIntensity = 0;
    createRavineWalls(mesh.position.x, mesh.position.z, height);
  }

  return mesh;
}

function generateDecor(tile, x, z) {
  const state = window.combat3dState;
  const terrain = String(tile.terrain || "grass").toLowerCase();

  generateSurfacePatch(tile, x, z);

  if (terrain === "forest") {
    const count = 1 + Math.floor(seededRandom(tile.x, tile.y, 1) * 4);

    for (let i = 0; i < count; i++) {
      const ox = (seededRandom(tile.x, tile.y, i + 2) - 0.5) * 0.7;
      const oz = (seededRandom(tile.x, tile.y, i + 12) - 0.5) * 0.7;
      const scale = 0.7 + seededRandom(tile.x, tile.y, i + 20);

      state.decorGroup.add(createTree(x + ox, z + oz, scale));
    }
  }

  if (terrain === "stone" || terrain === "wall") {
    if (seededRandom(tile.x, tile.y, 10) > 0.45) {
      state.decorGroup.add(createRock(x, z, 0.8));
    }
  }

  if (terrain === "water" || terrain === "swamp") {
    if (seededRandom(tile.x, tile.y, 50) > 0.3) {
      state.decorGroup.add(createReeds(x, z));
    }
  }
}

function generateSurfacePatch(tile, x, z) {
  const state = window.combat3dState;
  const terrain = String(tile.terrain || "grass").toLowerCase();
  const h = terrainHeight(terrain);

  const topY = h + 0.035;

  if (terrain === "grass" || terrain === "forest") {
    const count = terrain === "forest" ? 5 : 3;

    for (let i = 0; i < count; i++) {
      const blade = new THREE.Mesh(
        new THREE.ConeGeometry(0.055, 0.34, 5),
        new THREE.MeshStandardMaterial({
          color: terrain === "forest" ? 0x224d24 : 0x6fa85b,
          roughness: 1
        })
      );

      blade.position.set(
        x + (seededRandom(tile.x, tile.y, i + 300) - 0.5) * 0.85,
        topY + 0.08,
        z + (seededRandom(tile.x, tile.y, i + 400) - 0.5) * 0.85
      );

      blade.rotation.x = (seededRandom(tile.x, tile.y, i + 500) - 0.5) * 0.45;
      blade.rotation.z = (seededRandom(tile.x, tile.y, i + 600) - 0.5) * 0.45;

      state.decorGroup.add(blade);
    }
  }

  if (terrain === "road") {
    const streak = new THREE.Mesh(
      new THREE.PlaneGeometry(0.95, 0.32),
      new THREE.MeshBasicMaterial({
        color: 0x7d6a45,
        transparent: true,
        opacity: 0.55,
        depthWrite: false,
        side: THREE.DoubleSide
      })
    );

    streak.rotation.x = -Math.PI / 2;
    streak.rotation.z = seededRandom(tile.x, tile.y, 700) * Math.PI;
    streak.position.set(x, topY + 0.01, z);

    state.decorGroup.add(streak);
  }

  if (terrain === "stone" || terrain === "wall") {
    const pebbleCount = 2 + Math.floor(seededRandom(tile.x, tile.y, 800) * 3);

    for (let i = 0; i < pebbleCount; i++) {
      const pebble = new THREE.Mesh(
        new THREE.DodecahedronGeometry(0.04 + seededRandom(tile.x, tile.y, i + 810) * 0.04),
        new THREE.MeshStandardMaterial({
          color: 0x6f6f6f,
          roughness: 1
        })
      );

      pebble.position.set(
        x + (seededRandom(tile.x, tile.y, i + 820) - 0.5) * 0.75,
        topY + 0.03,
        z + (seededRandom(tile.x, tile.y, i + 830) - 0.5) * 0.75
      );

      state.decorGroup.add(pebble);
    }
  }

  if (terrain === "water") {
    const sheen = new THREE.Mesh(
      new THREE.PlaneGeometry(0.92, 0.92),
      new THREE.MeshBasicMaterial({
        color: 0x8fd8ff,
        transparent: true,
        opacity: 0.16,
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

  if (terrain === "swamp") {
    const muck = new THREE.Mesh(
      new THREE.CircleGeometry(0.28 + seededRandom(tile.x, tile.y, 950) * 0.18, 18),
      new THREE.MeshBasicMaterial({
        color: 0x28351f,
        transparent: true,
        opacity: 0.32,
        depthWrite: false,
        side: THREE.DoubleSide
      })
    );

    muck.rotation.x = -Math.PI / 2;
    muck.position.set(
      x + (seededRandom(tile.x, tile.y, 951) - 0.5) * 0.35,
      topY + 0.015,
      z + (seededRandom(tile.x, tile.y, 952) - 0.5) * 0.35
    );

    state.decorGroup.add(muck);
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

function updateCombatTokens(tiles) {
  const state = window.combat3dState;
  const seen = {};

  tiles.forEach(tile => {
    if (!tile.occupant_id) return;

    const id = String(tile.occupant_id);
    seen[id] = true;

    const targetX = Number(tile.x) - state.centerX;
    const targetZ = Number(tile.y) - state.centerY;
    const terrain = String(tile.terrain || "grass");
    const targetY = terrainHeight(terrain) + 0.05;

    let token = state.tokenMap[id];

    if (!token) {
      token = createToken(tile);
      state.tokenGroup.add(token);
      state.tokenMap[id] = token;
      state.clickableMeshes.push(token);
    }

    token.userData.targetX = targetX;
    token.userData.targetY = targetY;
    token.userData.targetZ = targetZ;
    token.userData.is_active_actor = !!tile.is_active_actor;

    if (token.userData.activeRing) {
      token.userData.activeRing.material.opacity = tile.is_active_actor ? 0.85 : 0;
    }
  });

  Object.keys(state.tokenMap).forEach(id => {
    if (seen[id]) return;

    const token = state.tokenMap[id];
    state.tokenGroup.remove(token);
    delete state.tokenMap[id];
  });
}

function loadCombatGLTF(path, onLoad) {
  if (!path || path === "") return;

  combatGLTFLoader.load(
    path,
    function (gltf) {
      onLoad(gltf.scene);
    },
    undefined,
    function (error) {
      console.error("Failed to load combat GLTF:", path, error);
    }
  );
}

function centerAndScaleCombatModel(group) {
  const box = new THREE.Box3().setFromObject(group);
  const size = box.getSize(new THREE.Vector3());
  const center = box.getCenter(new THREE.Vector3());

  group.position.x -= center.x;
  group.position.z -= center.z;
  group.position.y -= box.min.y;

  if (size.y > 0) {
    const targetHeight = 0.55; // smaller = smaller character
    const scale = targetHeight / size.y;
    group.scale.setScalar(scale);
  }
}

function addCharacterModelToToken(token, tile) {
  const baseModel = tile.model_base || tile.base_model || "";
  const hairModel = tile.model_hair || tile.hair_model || "";

  if (!baseModel) return;

  const characterGroup = new THREE.Group();

  loadCombatGLTF(baseModel, function (baseScene) {
    characterGroup.add(baseScene);

    if (hairModel && hairModel !== "") {
      loadCombatGLTF(hairModel, function (hairScene) {
        characterGroup.add(hairScene);
        finishCharacterModel();
      });
    } else {
      finishCharacterModel();
    }
  });

  function finishCharacterModel() {
    centerAndScaleCombatModel(characterGroup);

    characterGroup.position.y = 0.02;
    characterGroup.rotation.y = Math.PI;

    characterGroup.traverse(function (obj) {
      if (obj.isMesh) {
        obj.castShadow = true;
        obj.receiveShadow = true;
      }
    });

    token.add(characterGroup);
    token.userData.characterModel = characterGroup;

    // Make the old glowing beam less dominant for real 3D characters
    if (token.userData.beamGroup) token.userData.beamGroup.visible = false;
    if (token.userData.spiralGroup) token.userData.spiralGroup.visible = false;
    if (token.userData.glowOrb) token.userData.glowOrb.visible = false;
  }
}

function createToken(tile) {
  const state = window.combat3dState;

  const terrain = String(tile.terrain || "grass");
  const y = terrainHeight(terrain) + 0.05;

  const isPlayer = String(tile.occupant_type || "") === "player";
  const mainColor = isPlayer ? 0x77ddff : 0xff7744;
  const glowColor = isPlayer ? 0x44ccff : 0xff5533;

  const token = new THREE.Group();

  // clickable invisible body
  const hitbox = new THREE.Mesh(
    new THREE.CylinderGeometry(0.42, 0.42, 1.65, 16),
    new THREE.MeshBasicMaterial({
      transparent: true,
      opacity: 0,
      depthWrite: false
    })
  );

  hitbox.position.y = 0.85;
  hitbox.userData = tile;
  token.add(hitbox);

  // shadow disc
  const shadow = new THREE.Mesh(
    new THREE.CircleGeometry(0.46, 40),
    new THREE.MeshBasicMaterial({
      color: 0x000000,
      transparent: true,
      opacity: 0.24,
      depthWrite: false
    })
  );

  shadow.rotation.x = -Math.PI / 2;
  shadow.position.y = 0.01;
  token.add(shadow);

  // HP base ring
  const hpRing = new THREE.Mesh(
    new THREE.RingGeometry(0.47, 0.56, 48),
    new THREE.MeshBasicMaterial({
      color: 0x33ff66,
      side: THREE.DoubleSide,
      transparent: true,
      opacity: 0.9,
      depthWrite: false
    })
  );

  hpRing.rotation.x = -Math.PI / 2;
  hpRing.position.y = 0.025;
  token.add(hpRing);

  // active turn ring
  const activeRing = new THREE.Mesh(
    new THREE.RingGeometry(0.62, 0.75, 56),
    new THREE.MeshBasicMaterial({
      color: 0xffdd66,
      side: THREE.DoubleSide,
      transparent: true,
      opacity: tile.is_active_actor ? 0.85 : 0,
      blending: THREE.AdditiveBlending,
      depthWrite: false
    })
  );

  activeRing.rotation.x = -Math.PI / 2;
  activeRing.position.y = 0.035;
  token.add(activeRing);

  // ethereal beam
  const beamGroup = new THREE.Group();

  for (let i = 0; i < 8; i++) {
    const p = i / 7;

    const beam = new THREE.Mesh(
      new THREE.CylinderGeometry(
        0.045 + p * 0.045,
        0.045 + p * 0.045,
        1.45 - p * 0.08,
        28,
        1,
        true
      ),
      new THREE.MeshBasicMaterial({
        color: mainColor,
        transparent: true,
        opacity: 0.17 * (1 - p),
        blending: THREE.AdditiveBlending,
        depthWrite: false,
        side: THREE.DoubleSide
      })
    );

    beam.position.y = 0.82;
    beam.userData.baseOpacity = 0.17 * (1 - p);
    beamGroup.add(beam);
  }

  // bright vertical core line
  const lineGeo = new THREE.BufferGeometry().setFromPoints([
    new THREE.Vector3(0, 0.15, 0),
    new THREE.Vector3(0, 1.55, 0)
  ]);

  const coreLine = new THREE.Line(
    lineGeo,
    new THREE.LineBasicMaterial({
      color: 0xffffff,
      transparent: true,
      opacity: 0.65,
      blending: THREE.AdditiveBlending,
      depthWrite: false
    })
  );

  beamGroup.add(coreLine);

  // spiral wisps
  const spiralGroup = new THREE.Group();

  for (let s = 0; s < 3; s++) {
    const points = [];

    for (let i = 0; i < 90; i++) {
      const p = i / 89;
      const angle = p * Math.PI * 4.5 + s * 2.1;
      const radius = 0.08 + p * 0.22;

      points.push(new THREE.Vector3(
        Math.cos(angle) * radius,
        0.22 + p * 1.18,
        Math.sin(angle) * radius
      ));
    }

    const geo = new THREE.BufferGeometry().setFromPoints(points);

    const line = new THREE.Line(
      geo,
      new THREE.LineBasicMaterial({
        color: mainColor,
        transparent: true,
        opacity: 0.45,
        blending: THREE.AdditiveBlending,
        depthWrite: false
      })
    );

    line.userData.spinSpeed = 0.004 + s * 0.002;
    spiralGroup.add(line);
  }

  // drifting particles
  const particleGroup = new THREE.Group();

  for (let i = 0; i < 9; i++) {
    const particle = new THREE.Mesh(
      new THREE.SphereGeometry(0.025, 8, 8),
      new THREE.MeshBasicMaterial({
        color: 0xffffff,
        transparent: true,
        opacity: 0.75,
        blending: THREE.AdditiveBlending,
        depthWrite: false
      })
    );

    particle.position.set(
      (seededRandom(tile.x, tile.y, i + 100) - 0.5) * 0.55,
      0.25 + seededRandom(tile.x, tile.y, i + 200) * 1.25,
      (seededRandom(tile.x, tile.y, i + 300) - 0.5) * 0.55
    );

    particle.userData.baseY = particle.position.y;
    particle.userData.phase = seededRandom(tile.x, tile.y, i + 400) * Math.PI * 2;

    particleGroup.add(particle);
  }

  // glow orb at centre
  const glowOrb = new THREE.Mesh(
    new THREE.SphereGeometry(0.1, 20, 20),
    new THREE.MeshBasicMaterial({
      color: mainColor,
      transparent: true,
      opacity: 0.85,
      blending: THREE.AdditiveBlending,
      depthWrite: false
    })
  );

  glowOrb.position.y = 0.85;
const light = new THREE.PointLight(
  glowColor,
  tile.is_active_actor ? 2.4 : 1.15,
  tile.is_active_actor ? 9 : 6,
  2
);
  light.position.y = 0.9;

  token.add(beamGroup);
  token.add(spiralGroup);
  token.add(particleGroup);
  token.add(glowOrb);
  token.add(light);

  token.position.set(
    Number(tile.x) - state.centerX,
    y,
    Number(tile.y) - state.centerY
  );

  token.userData = {
    ...tile,

    targetX: token.position.x,
    targetY: token.position.y,
    targetZ: token.position.z,

    isPlayer: isPlayer,
    is_active_actor: !!tile.is_active_actor,

    phase: seededRandom(tile.x, tile.y, 999) * Math.PI * 2,

    hitbox: hitbox,
    hpRing: hpRing,
    activeRing: activeRing,
    beamGroup: beamGroup,
    spiralGroup: spiralGroup,
    particleGroup: particleGroup,
    glowOrb: glowOrb,
    wispLight: light
  };
if (isPlayer) {
  addCharacterModelToToken(token, tile);
}
  return token;
}

function animateCombat3D() {
  const state = window.combat3dState;

  if (!state.renderer || !state.scene || !state.camera) return;

  state.animationFrame = requestAnimationFrame(animateCombat3D);

  const now = performance.now();
  const t = now * 0.001;

  Object.values(state.tokenMap).forEach(token => {
    if (!token.userData) return;

    token.position.x += (token.userData.targetX - token.position.x) * 0.12;
    token.position.y += (token.userData.targetY - token.position.y) * 0.12;
    token.position.z += (token.userData.targetZ - token.position.z) * 0.12;

    const active = !!token.userData.is_active_actor;
    const phase = token.userData.phase || 0;

    if (token.userData.beamGroup) {
      token.userData.beamGroup.rotation.y += 0.008;
      token.userData.beamGroup.scale.y = 1 + Math.sin(t * 2.2 + phase) * 0.035;

      token.userData.beamGroup.children.forEach((child, i) => {
        if (!child.material) return;

        if (child.type === "Line") {
          child.material.opacity = active
            ? 0.72 + Math.sin(t * 4 + phase) * 0.2
            : 0.42 + Math.sin(t * 3 + phase) * 0.1;
        } else {
          const base = child.userData.baseOpacity || 0.1;
          child.material.opacity = base + Math.sin(t * 2.5 + i + phase) * 0.025;
        }
      });
    }

    if (token.userData.spiralGroup) {
      token.userData.spiralGroup.rotation.y += active ? 0.018 : 0.01;

      token.userData.spiralGroup.children.forEach(line => {
        line.rotation.y += line.userData.spinSpeed || 0.004;
        if (line.material) {
          line.material.opacity = active
            ? 0.58 + Math.sin(t * 3 + phase) * 0.18
            : 0.34 + Math.sin(t * 2 + phase) * 0.08;
        }
      });
    }

    if (token.userData.particleGroup) {
      token.userData.particleGroup.children.forEach((p, i) => {
        const pPhase = p.userData.phase || 0;
        p.position.y = p.userData.baseY + Math.sin(t * 1.8 + pPhase) * 0.08;
        p.rotation.y += 0.01;

        if (p.material) {
          p.material.opacity = 0.45 + Math.sin(t * 3 + pPhase) * 0.28;
        }
      });
    }

    if (token.userData.glowOrb) {
      token.userData.glowOrb.scale.setScalar(
        active
          ? 1.25 + Math.sin(t * 5 + phase) * 0.18
          : 1 + Math.sin(t * 3 + phase) * 0.08
      );

      token.userData.glowOrb.material.opacity =
        active
          ? 0.9 + Math.sin(t * 5 + phase) * 0.08
          : 0.55 + Math.sin(t * 3 + phase) * 0.12;
    }

    if (token.userData.wispLight) {
      token.userData.wispLight.intensity =
        active
          ? 1.45 + Math.sin(t * 5 + phase) * 0.45
          : 0.65 + Math.sin(t * 2 + phase) * 0.12;
    }

    if (token.userData.activeRing) {
      token.userData.activeRing.material.opacity =
        active
          ? 0.5 + Math.sin(t * 5 + phase) * 0.35
          : 0;
    }

    if (token.userData.hpRing) {
      token.userData.hpRing.rotation.z += 0.01;
    }
    
    state.decorGroup.children.forEach(obj => {
  if (obj.userData && obj.userData.isWaterSheen) {
    const phase = obj.userData.phase || 0;
    obj.material.opacity = 0.11 + Math.sin(performance.now() * 0.002 + phase) * 0.05;
    obj.rotation.z += 0.0015;
  }
});
    
    state.scene.fog.density =
  0.026 + Math.sin(t * 0.08) * 0.002;
  });

  state.renderer.render(state.scene, state.camera);
  
  
}

function createTree(x, z, scale = 1) {
  const tree = new THREE.Group();

  const trunk = new THREE.Mesh(
    new THREE.CylinderGeometry(0.08 * scale, 0.12 * scale, 0.8 * scale, 6),
    new THREE.MeshStandardMaterial({ color: 0x5a351f })
  );
  trunk.position.y = 0.4 * scale;

const leaves = new THREE.Mesh(
  new THREE.ConeGeometry(
    0.38 * scale,
    1.25 * scale,
    9
  ),
    new THREE.MeshStandardMaterial({ color:
  Math.random() > 0.5
    ? 0x285f32
    : 0x1d4d29 })
  );
  leaves.position.y = 1.1 * scale;

  tree.add(trunk);
  tree.add(leaves);
  const leaves2 = leaves.clone();

leaves2.scale.setScalar(0.72);

leaves2.position.y += 0.38 * scale;

tree.add(leaves2);
  tree.position.set(x, 0.2, z);

  return tree;
}

function createRock(x, z, scale = 1) {
  const rock = new THREE.Mesh(
    new THREE.DodecahedronGeometry(0.22 * scale),
    new THREE.MeshStandardMaterial({
      color: 0x777777,
      roughness: 0.95
    })
  );

  rock.position.set(x, 0.2, z);
  return rock;
}

function createReeds(x, z) {
  const reeds = new THREE.Group();

  for (let i = 0; i < 4; i++) {
    const reed = new THREE.Mesh(
      new THREE.CylinderGeometry(0.015, 0.02, 0.45, 5),
      new THREE.MeshStandardMaterial({ color: 0x8a8f3a })
    );

    reed.position.set(
      (Math.random() - 0.5) * 0.4,
      0.22,
      (Math.random() - 0.5) * 0.4
    );

    reeds.add(reed);
  }

  reeds.position.set(x, 0, z);
  return reeds;
}

function createRavineWalls(x, z, depth) {
  const state = window.combat3dState;

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

  if (t === "grass") return 0x5b8f47;
  if (t === "forest") return 0x244f2d;
  if (t === "water") return 0x245f91;
  if (t === "stone") return 0x7c7c78;
  if (t === "wall") return 0x4a4a46;
  if (t === "road") return 0xa98c5c;
  if (t === "swamp") return 0x3f5b3d;
  if (t === "pit") return 0x080808;
  if (t === "ravine") return 0x070707;

  return 0x77705f;
}

function terrainHeight(t) {
  t = String(t || "grass").toLowerCase();

  if (t === "wall") return 2.2;
  if (t === "forest") return 0.3;
  if (t === "stone") return 0.18;
  if (t === "water") return -0.2;
  if (t === "swamp") return -0.1;
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

function updateCamera() {
  const state = window.combat3dState;

  if (!state.camera) return;

  state.camera.position.set(
    state.radius * Math.sin(state.angle),
    state.cameraHeight,
    state.radius * Math.cos(state.angle)
  );

  state.camera.lookAt(0, 0, 0);
}

function startAnimationLoop() {
  const state = window.combat3dState;

  if (state.animationFrame) return;

  animateCombat3D();
}

function onCombat3DResize() {
  const state = window.combat3dState;

  const el = document.getElementById(state.containerId);
  if (!el || !state.camera || !state.renderer) return;

  const width = el.clientWidth || 1200;
  const height = el.clientHeight || 700;

  state.camera.aspect = width / height;
  state.camera.updateProjectionMatrix();

  state.renderer.setSize(width, height);
}

Shiny.addCustomMessageHandler("combat3d-init", function(message) {
  console.log("3D INIT MESSAGE", message);

  function tryInit(attemptsLeft) {
    const el = document.getElementById(message.containerId);

    if (!el) {
      if (attemptsLeft <= 0) {
        console.error("Missing container after retries", message.containerId);
        return;
      }

      setTimeout(function() {
        tryInit(attemptsLeft - 1);
      }, 100);

      return;
    }

    window.renderCombat3D(
      message.containerId,
      message.mapData,
      message.inputIds || {}
    );
  }

  tryInit(20);
});

Shiny.addCustomMessageHandler("combat3d-update-tokens", function(message) {
  const state = window.combat3dState;

  if (!state.initialized) return;

  const tokens = message.tokens || [];

  tokens.forEach(function(t) {
    const id = String(t.actor_id);
    const token = state.tokenMap[id];

    if (!token) return;

    const terrain = String(t.terrain || "grass");
    const targetX = Number(t.x) - state.centerX;
    const targetZ = Number(t.y) - state.centerY;
    const targetY = terrainHeight(terrain) + 0.05;

    token.userData.targetX = targetX;
    token.userData.targetY = targetY;
    token.userData.targetZ = targetZ;
    token.userData.is_active_actor = !!t.is_active_actor;

    if (token.userData.hitbox) {
      token.userData.hitbox.userData = {
        ...token.userData.hitbox.userData,
        occupant_id: t.actor_id,
        occupant_type: t.actor_type,
        x: t.x,
        y: t.y
      };
    }
  });
});

 Shiny.addCustomMessageHandler("combat3d-resize", function(message) {
  const el = document.getElementById(message.containerId);
  if (!el) return;

  window.dispatchEvent(new Event("resize"));

  if (el.__combat3dRenderer && el.__combat3dCamera) {
    const w = el.clientWidth || 1;
    const h = el.clientHeight || 1;

    el.__combat3dCamera.aspect = w / h;
    el.__combat3dCamera.updateProjectionMatrix();
    el.__combat3dRenderer.setSize(w, h, false);
  }
});