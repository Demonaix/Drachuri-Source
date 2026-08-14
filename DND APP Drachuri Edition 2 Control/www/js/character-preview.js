import * as THREE from "three";
import { GLTFLoader } from "three/addons/loaders/GLTFLoader.js";
import { OrbitControls } from "three/addons/controls/OrbitControls.js";

console.log("CHARACTER PREVIEW JS VERSION: HAIR ONLY V3");

let characterScene = null;
let characterCamera = null;
let characterRenderer = null;
let characterControls = null;
let characterCurrentModel = null;
let animationFrameId = null;

function initCharacterPreview(containerId) {
  const container = document.getElementById(containerId);

  if (!container) {
    console.error("Missing character preview container:", containerId);
    return;
  }

  if (animationFrameId) {
    cancelAnimationFrame(animationFrameId);
    animationFrameId = null;
  }

  container.innerHTML = "";

  characterScene = new THREE.Scene();
  characterScene.background = new THREE.Color(0xf5f5f5);

  characterCamera = new THREE.PerspectiveCamera(
    45,
    container.clientWidth / container.clientHeight,
    0.1,
    1000
  );

  characterCamera.position.set(0, 1.2, 3.2);

  characterRenderer = new THREE.WebGLRenderer({ antialias: true });
  characterRenderer.setSize(container.clientWidth, container.clientHeight);
  characterRenderer.setPixelRatio(window.devicePixelRatio || 1);

  container.appendChild(characterRenderer.domElement);

  characterControls = new OrbitControls(
    characterCamera,
    characterRenderer.domElement
  );

  characterControls.enableDamping = true;
  characterControls.dampingFactor = 0.05;
  characterControls.enablePan = true;
  characterControls.screenSpacePanning = true;
  characterControls.target.set(0, 0.75, 0);
  characterControls.update();

  characterScene.add(new THREE.HemisphereLight(0xffffff, 0x444444, 2));

  const directionalLight = new THREE.DirectionalLight(0xffffff, 2);
  directionalLight.position.set(3, 5, 4);
  characterScene.add(directionalLight);

  characterScene.add(new THREE.GridHelper(10, 10));

  animateCharacterPreview();
}

function animateCharacterPreview() {
  animationFrameId = requestAnimationFrame(animateCharacterPreview);

  if (characterControls) characterControls.update();

  if (characterRenderer && characterScene && characterCamera) {
    characterRenderer.render(characterScene, characterCamera);
  }
}

function loadGLTFModel(path) {
  const loader = new GLTFLoader();

  return new Promise((resolve, reject) => {
    loader.load(
      encodeURI(path),
      function (gltf) {
        resolve(gltf.scene);
      },
      undefined,
      function (error) {
        console.error("Error loading model:", path, error);
        reject(error);
      }
    );
  });
}

function centerAndScaleWholeCharacter(group) {
  const box = new THREE.Box3().setFromObject(group);
  const center = box.getCenter(new THREE.Vector3());
  const size = box.getSize(new THREE.Vector3());

  group.position.x -= center.x;
  group.position.z -= center.z;
  group.position.y -= box.min.y;

  if (size.y > 0) {
    const targetHeight = 0.65;
    const scale = targetHeight / size.y;
    group.scale.setScalar(scale);
  }

  characterControls.target.set(0, 0.75, 0);
  characterCamera.position.set(0, 1.2, 3.2);
  characterControls.update();
}

function enableShadowsAndMaterials(group) {
  group.traverse(function (obj) {
    if (obj.isMesh) {
      obj.castShadow = true;
      obj.receiveShadow = true;
    }
  });
}

function cloneMaterial(obj) {
  if (Array.isArray(obj.material)) {
    obj.material = obj.material.map(mat => mat.clone());
  } else if (obj.material) {
    obj.material = obj.material.clone();
  }
}

function getMaterials(obj) {
  if (!obj.material) return [];
  return Array.isArray(obj.material) ? obj.material : [obj.material];
}

function forceMaterialColour(mat, colourHex) {
  if (!mat || !colourHex) return;

  if (mat.color) mat.color.set(colourHex);

  // Hair textures are usually baked, so disable maps to make colour obvious.
  mat.map = null;
  mat.normalMap = null;
  mat.roughnessMap = null;
  mat.metalnessMap = null;
  mat.aoMap = null;
  mat.emissiveMap = null;

  mat.vertexColors = false;
  mat.needsUpdate = true;
}

function recolorCharacterPart(part, message, path) {
  const hairColor = message.hairColor || "#3b2416";
  const pathText = (path || "").toLowerCase();

  part.traverse(function (obj) {
    if (!obj.isMesh || !obj.material) return;

    const meshName = (obj.name || "").toLowerCase();

    const materialNames = getMaterials(obj)
      .map(mat => (mat.name || "").toLowerCase())
      .join(" ");

    const combined = `${pathText} ${meshName} ${materialNames}`;

    const isHair =
      combined.includes("hair") ||
      combined.includes("beard");

    if (!isHair) return;

    cloneMaterial(obj);

    getMaterials(obj).forEach(function (mat) {
      forceMaterialColour(mat, hairColor);
    });

    console.log("Recoloured hair:", {
      path,
      meshName,
      materialNames,
      hairColor
    });
  });
}

function logMaterialNames(part, path) {
  part.traverse(function (obj) {
    if (!obj.isMesh || !obj.material) return;

    console.log("Mesh/material:", {
      path,
      mesh: obj.name || "(unnamed mesh)",
      materials: getMaterials(obj).map(mat => mat.name || "(unnamed)")
    });
  });
}

Shiny.addCustomMessageHandler("loadCharacterPreview", async function (message) {
  initCharacterPreview(message.containerId);

  characterCurrentModel = new THREE.Group();

  const partsToLoad = [
    message.baseModel,
    message.hairModel,
    message.bodyModel,
    message.armsModel,
    message.legsModel,
    message.feetModel,
    message.headgearModel,
    message.accessoryModel
  ].filter(path => path && path !== "");

  console.log("Character preview message:", message);
  console.log("Character parts to load:", partsToLoad);

  if (partsToLoad.length === 0) {
    console.warn("No character parts selected.");
    return;
  }

  const loadedParts = await Promise.allSettled(
    partsToLoad.map(path => loadGLTFModel(path))
  );

  loadedParts.forEach(function (result, index) {
    const path = partsToLoad[index];

    if (result.status === "fulfilled") {
      const part = result.value;

      logMaterialNames(part, path);
      recolorCharacterPart(part, message, path);
      enableShadowsAndMaterials(part);

      characterCurrentModel.add(part);

      console.log("Loaded character part:", path);
    } else {
      console.error("Failed character part:", path, result.reason);
    }
  });

  if (characterCurrentModel.children.length === 0) {
    console.error("No character parts loaded successfully.");
    return;
  }

  characterScene.add(characterCurrentModel);
  centerAndScaleWholeCharacter(characterCurrentModel);
});