import * as THREE from "three";

export function createHeartTreeBoss() {
  const group = new THREE.Group();

  const trunkMat = new THREE.MeshStandardMaterial({
    color: 0x3a1f13,
    roughness: 0.9
  });

  // trunk
  const trunkGeo = new THREE.CylinderGeometry(0.35, 0.6, 2.4, 10);
  const trunk = new THREE.Mesh(trunkGeo, trunkMat);
  trunk.position.y = 1.2;
  trunk.castShadow = true;
  trunk.receiveShadow = true;
  group.add(trunk);

  // corrupted heart
  const heartGeo = new THREE.SphereGeometry(0.45, 24, 24);
  const heartMat = new THREE.MeshStandardMaterial({
    color: 0x7a0018,
    emissive: 0x330008,
    emissiveIntensity: 1.2,
    roughness: 0.45
  });

  const heart = new THREE.Mesh(heartGeo, heartMat);
  heart.scale.set(1.0, 1.15, 0.75);
  heart.position.set(0, 1.55, 0.42);
  heart.castShadow = true;
  heart.receiveShadow = true;
  group.add(heart);

  // roots
  for (let i = 0; i < 10; i++) {
    const angle = (Math.PI * 2 * i) / 10;

    const rootGeo = new THREE.CylinderGeometry(0.06, 0.11, 1.8, 8);
    const root = new THREE.Mesh(rootGeo, trunkMat);

    root.rotation.z = Math.PI / 2;
    root.rotation.y = -angle;

    root.position.set(
      Math.cos(angle) * 0.75,
      0.12,
      Math.sin(angle) * 0.75
    );

    root.castShadow = true;
    root.receiveShadow = true;

    group.add(root);
  }

  group.userData.customRenderType = "heart_tree_boss";

  return group;
}