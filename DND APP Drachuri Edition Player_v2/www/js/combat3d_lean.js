import * as THREE from "three";
import { OrbitControls } from "three/addons/controls/OrbitControls.js";
import { GLTFLoader } from "three/addons/loaders/GLTFLoader.js";
import { clone as cloneSkeleton } from "three/addons/utils/SkeletonUtils.js";

const states = new Map();
const modelLoader=new GLTFLoader(),modelAssetCache=new Map(),characterTemplateCache=new Map();
const TERRAIN = {
  grass:{color:0x719a58,tex:"grass.jpg",h:.10}, sand:{color:0xc7ad70,tex:"dirt.jpg",h:.02},
  forest:{color:0x315f38,tex:"forest.jpg",h:.14}, woodland:{color:0x315f38,tex:"forest.jpg",h:.14},
  water:{color:0x367eaa,tex:"water.jpg",h:-.18}, stone:{color:0x85837b,tex:"stone.jpg",h:.10},
  wall:{color:0x76706a,tex:"battlefield_fieldstone.jpg",h:.10}, road:{color:0xa5885b,tex:"dirt.jpg",h:-.07},
  swamp:{color:0x526944,tex:"swamp.jpg",h:-.03}, ravine:{color:0x17151a,tex:"ravine.jpg",h:-2.65},
  pit:{color:0x17151a,tex:"ravine.jpg",h:-2.10}, mandred_convergence:{color:0x714ca1,tex:"stone.jpg",h:.12}
};

function normalise(value) {
  if (typeof value === "string") value = JSON.parse(value);
  if (Array.isArray(value)) return value;
  if (!value || typeof value !== "object") return [];
  const keys=Object.keys(value),n=Math.max(0,...keys.map(k=>Array.isArray(value[k])?value[k].length:0));
  return Array.from({length:n},(_,i)=>Object.fromEntries(keys.map(k=>[k,Array.isArray(value[k])?value[k][i]:value[k]])));
}
function truthy(v){return v===true||v===1||v==="1"||v==="true";}
function terrainName(v){const t=String(v||"grass").toLowerCase();return TERRAIN[t]?t:"grass";}
function seeded(x,y,salt=0){const n=Math.sin(Number(x)*12.9898+Number(y)*78.233+salt*37.719)*43758.5453;return n-Math.floor(n);}
function modelUrl(path){if(/^https?:/i.test(path))return path;return new URL(`../${String(path).replace(/^\/+/,"")}`,import.meta.url).href;}
function loadModelAsset(path){const url=modelUrl(path);if(!modelAssetCache.has(url))modelAssetCache.set(url,new Promise((resolve,reject)=>modelLoader.load(url,g=>resolve(g.scene),undefined,reject)));return modelAssetCache.get(url);}
function elevation(row){const name=terrainName(row?.terrain),base=TERRAIN[name].h;if(["road","water","ravine","pit","wall"].includes(name))return base;return base+(seeded(row.x,row.y,9)-.5)*.13;}
function signature(rows){return rows.map(t=>[t.x,t.y,t.terrain,t.blocks_movement,t.light,t.fog].join(",")).sort().join("|");}
function disposeObject(root){
  if(!root)return;root.traverse(o=>{o.geometry?.dispose();const ms=Array.isArray(o.material)?o.material:[o.material];ms.filter(Boolean).forEach(m=>m.dispose());});
}
function disposeState(state){
  if(!state)return;state.resizeObserver?.disconnect();state.controls?.dispose();disposeObject(state.scene);
  Object.values(state.textures||{}).forEach(t=>t.dispose());
  state.renderer?.dispose();state.renderer?.domElement?.remove();states.delete(state.containerId);
}
function requestRender(state){
  if(!state||state.renderPending)return;state.renderPending=true;
  requestAnimationFrame(()=>{state.renderPending=false;if(state.renderer&&state.scene&&state.camera)state.renderer.render(state.scene,state.camera);});
}
function texture(state,file,repeatX=1.5,repeatY=repeatX){
  if(!file)return null;const key=`${file}:${repeatX}:${repeatY}`;if(state.textures[key])return state.textures[key];
  const url=new URL(`../assets/textures/${file}`,import.meta.url).href,t=new THREE.TextureLoader().load(url,()=>requestRender(state));
  t.colorSpace=THREE.SRGBColorSpace;t.wrapS=t.wrapT=THREE.RepeatWrapping;t.repeat.set(repeatX,repeatY);state.textures[key]=t;return t;
}
function makeState(containerId,inputIds,quality){
  const el=document.getElementById(containerId);if(!el)return null;
  const previous=states.get(containerId);if(previous)disposeState(previous);
  const renderer=new THREE.WebGLRenderer({antialias:quality!=="low",powerPreference:"high-performance"});
  renderer.setPixelRatio(quality==="low"?1:Math.min(devicePixelRatio||1,1.5));renderer.setSize(Math.max(1,el.clientWidth),Math.max(1,el.clientHeight),false);
  renderer.outputColorSpace=THREE.SRGBColorSpace;renderer.shadowMap.enabled=quality==="decorative";el.replaceChildren(renderer.domElement);
  const scene=new THREE.Scene();scene.background=new THREE.Color(0x241b17);scene.add(new THREE.HemisphereLight(0xffe8c7,0x4b3325,1.55));scene.add(new THREE.AmbientLight(0xffe8cc,.48));
  const sun=new THREE.DirectionalLight(0xffefd2,quality==="low"?.95:1.35);sun.position.set(14,24,10);sun.castShadow=quality==="decorative";
  if(sun.castShadow){sun.shadow.mapSize.set(1024,1024);sun.shadow.camera.left=-30;sun.shadow.camera.right=30;sun.shadow.camera.top=30;sun.shadow.camera.bottom=-30;}scene.add(sun);
  const camera=new THREE.PerspectiveCamera(48,1,.1,500),controls=new OrbitControls(camera,renderer.domElement);
  controls.enableDamping=false;controls.maxPolarAngle=Math.PI*.47;controls.minPolarAngle=.12;controls.minDistance=4;controls.maxDistance=100;
  const state={containerId,el,renderer,scene,camera,controls,inputIds:inputIds||{},quality,textures:{},terrainRoot:new THREE.Group(),tokenRoot:new THREE.Group(),decorRoot:new THREE.Group(),overlayRoot:new THREE.Group(),tiles:[],tileByKey:new Map(),signature:"",boundsSignature:"",centerX:0,centerY:0,renderPending:false};
  scene.add(state.terrainRoot,state.decorRoot,state.overlayRoot,state.tokenRoot);controls.addEventListener("change",()=>requestRender(state));
  const resize=()=>{const w=Math.max(1,el.clientWidth),h=Math.max(1,el.clientHeight);camera.aspect=w/h;camera.updateProjectionMatrix();renderer.setSize(w,h,false);requestRender(state);};
  state.resizeObserver=new ResizeObserver(resize);state.resizeObserver.observe(el);states.set(containerId,state);setupPicking(state);resize();return state;
}
function setupPicking(state){
  const ray=new THREE.Raycaster(),pointer=new THREE.Vector2();let down=null;
  state.renderer.domElement.addEventListener("pointerdown",e=>{down={x:e.clientX,y:e.clientY};});
  state.renderer.domElement.addEventListener("pointerup",e=>{
    if(!down||Math.hypot(e.clientX-down.x,e.clientY-down.y)>6)return;down=null;
    const rect=state.renderer.domElement.getBoundingClientRect();pointer.set(((e.clientX-rect.left)/rect.width)*2-1,-((e.clientY-rect.top)/rect.height)*2+1);ray.setFromCamera(pointer,state.camera);
    const tokenHits=ray.intersectObjects(state.tokenRoot.children,true);if(tokenHits.length){let o=tokenHits[0].object;while(o.parent&&o.userData.actorId==null)o=o.parent;const d=o.userData;if(d.actorId&&state.inputIds.target)Shiny.setInputValue(state.inputIds.target,{actor_id:d.actorId,actor_type:d.actorType||"enemy",x:d.x,y:d.y,nonce:Math.random()},{priority:"event"});return;}
    const plane=new THREE.Plane(new THREE.Vector3(0,1,0),0),point=new THREE.Vector3();if(!ray.ray.intersectPlane(plane,point))return;
    const x=Math.round(point.x+state.centerX),y=Math.round(point.z+state.centerY),tile=state.tileByKey.get(`${x},${y}`);if(!tile)return;
    if(tile.occupant_id&&state.inputIds.target)Shiny.setInputValue(state.inputIds.target,{actor_id:tile.occupant_id,actor_type:tile.occupant_type||"enemy",x,y,nonce:Math.random()},{priority:"event"});
    else if(state.inputIds.move)Shiny.setInputValue(state.inputIds.move,{x,y,nonce:Math.random()},{priority:"event"});
  });
}
function clearGroup(group){for(const child of [...group.children]){group.remove(child);disposeObject(child);}}
function buildTabletop(state,rows,xs,ys){
  const bottom=-3.05,tileMap=state.tileByKey,edges=[];
  for(const row of rows){const x=Number(row.x),y=Number(row.y),top=Math.max(elevation(row),-.05);for(const [dx,dy,rot] of [[-1,0,Math.PI/2],[1,0,Math.PI/2],[0,-1,0],[0,1,0]])if(!tileMap.has(`${x+dx},${y+dy}`))edges.push({x:x-state.centerX+dx*.5,z:y-state.centerY+dy*.5,top,rot});}
  if(edges.length){const geo=new THREE.BoxGeometry(1,1,.075),mat=new THREE.MeshLambertMaterial({color:0x3d2d20}),fascia=new THREE.InstancedMesh(geo,mat,edges.length),matrix=new THREE.Matrix4(),position=new THREE.Vector3(),rotation=new THREE.Quaternion(),scale=new THREE.Vector3();edges.forEach((e,i)=>{const height=e.top-bottom;position.set(e.x,bottom+height/2,e.z);rotation.setFromAxisAngle(new THREE.Vector3(0,1,0),e.rot);scale.set(1.01,height,1);matrix.compose(position,rotation,scale);fascia.setMatrixAt(i,matrix);});fascia.instanceMatrix.needsUpdate=true;fascia.receiveShadow=true;state.terrainRoot.add(fascia);}
  const mapWidth=Math.max(...xs)-Math.min(...xs)+1,mapDepth=Math.max(...ys)-Math.min(...ys)+1,tableWidth=mapWidth+4.5,tableDepth=mapDepth+4.5;
  const wood=new THREE.MeshLambertMaterial({color:0xffffff,map:texture(state,"tavern_table_oak.jpg",Math.max(1,tableWidth/7),Math.max(1,tableDepth/7))}),darkWood=new THREE.MeshLambertMaterial({color:0x3b2518}),plaster=new THREE.MeshLambertMaterial({color:0xffffff,map:texture(state,"tavern_plaster_timbers.jpg",5,3)}),floorMat=new THREE.MeshLambertMaterial({color:0xffffff});
  const top=new THREE.Mesh(new THREE.BoxGeometry(tableWidth,.34,tableDepth),wood);top.position.y=bottom-.17;top.receiveShadow=true;top.castShadow=state.quality==="decorative";state.terrainRoot.add(top);
  const apronY=bottom-.58,apronH=.72;for(const apron of [
    [tableWidth-.35,apronH,.22,0,apronY,-tableDepth/2+.22],[tableWidth-.35,apronH,.22,0,apronY,tableDepth/2-.22],
    [.22,apronH,tableDepth-.35,-tableWidth/2+.22,apronY,0],[.22,apronH,tableDepth-.35,tableWidth/2-.22,apronY,0]
  ]){const mesh=new THREE.Mesh(new THREE.BoxGeometry(apron[0],apron[1],apron[2]),darkWood);mesh.position.set(apron[3],apron[4],apron[5]);mesh.castShadow=true;state.terrainRoot.add(mesh);}
  const floorY=bottom-3.65,legH=3.15,legGeo=new THREE.BoxGeometry(.48,legH,.48);for(const x of [-tableWidth/2+.65,tableWidth/2-.65])for(const z of [-tableDepth/2+.65,tableDepth/2-.65]){const leg=new THREE.Mesh(legGeo,darkWood);leg.position.set(x,bottom-.34-legH/2,z);leg.castShadow=true;state.terrainRoot.add(leg);}
  const roomSpan=Math.max(40,Math.max(mapWidth,mapDepth)*3),roomWidth=roomSpan,roomDepth=roomSpan;
  floorMat.map=texture(state,"tavern_floorboards.jpg",Math.max(2,roomWidth/8),Math.max(2,roomDepth/8));floorMat.needsUpdate=true;
  const floor=new THREE.Mesh(new THREE.BoxGeometry(roomWidth,.28,roomDepth),floorMat);floor.position.y=floorY;floor.receiveShadow=true;state.terrainRoot.add(floor);
  const wallH=18,wallY=floorY+wallH/2;for(const wall of [[roomWidth,wallH,.35,0,wallY,-roomDepth/2],[.35,wallH,roomDepth,-roomWidth/2,wallY,0],[.35,wallH,roomDepth,roomWidth/2,wallY,0]]){const mesh=new THREE.Mesh(new THREE.BoxGeometry(wall[0],wall[1],wall[2]),plaster);mesh.position.set(wall[3],wall[4],wall[5]);mesh.receiveShadow=true;state.terrainRoot.add(mesh);}
}
function buildSurfaceGeometry(items,tileMap,centerX,centerY){
  const positions=[],uvs=[],indices=[];
  const get=(x,y,fallback)=>tileMap.get(`${x},${y}`)||fallback;
  const average=rows=>rows.reduce((sum,row)=>sum+elevation(row),0)/rows.length;
  items.forEach(row=>{
    const x=Number(row.x),y=Number(row.y),base=positions.length/3;
    const edge=(dx,dy)=>average([row,get(x+dx,y+dy,row)]);
    const corner=(dx,dy)=>average([row,get(x+dx,y,row),get(x,y+dy,row),get(x+dx,y+dy,row)]);
    const points=[
      [0,elevation(row),0,.5,.5],[-.5,corner(-1,-1),-.5,0,0],[0,edge(0,-1),-.5,.5,0],[.5,corner(1,-1),-.5,1,0],
      [.5,edge(1,0),0,1,.5],[.5,corner(1,1),.5,1,1],[0,edge(0,1),.5,.5,1],[-.5,corner(-1,1),.5,0,1],[-.5,edge(-1,0),0,0,.5]
    ];
    for(const p of points){positions.push(x-centerX+p[0],p[1],y-centerY+p[2]);uvs.push(p[3],p[4]);}
    for(let i=1;i<=8;i++)indices.push(base,base+(i===8?1:i+1),base+i);
  });
  const geometry=new THREE.BufferGeometry();geometry.setAttribute("position",new THREE.Float32BufferAttribute(positions,3));geometry.setAttribute("uv",new THREE.Float32BufferAttribute(uvs,2));geometry.setIndex(indices);geometry.computeVertexNormals();return geometry;
}
function buildTerrain(state,rows){
  clearGroup(state.terrainRoot);clearGroup(state.decorRoot);state.tileByKey.clear();state.tiles=rows;
  const xs=rows.map(r=>Number(r.x)),ys=rows.map(r=>Number(r.y));state.centerX=(Math.min(...xs)+Math.max(...xs))/2;state.centerY=(Math.min(...ys)+Math.max(...ys))/2;
  const grouped={};for(const row of rows){const t=terrainName(row.terrain);(grouped[t]??=[]).push(row);state.tileByKey.set(`${row.x},${row.y}`,row);}
  const matrix=new THREE.Matrix4();
  for(const [name,items] of Object.entries(grouped)){const def=TERRAIN[name],mat=new THREE.MeshLambertMaterial({color:def.color,map:texture(state,def.tex)});
    if(state.quality==="low"){
      const height=name==="wall"?2.35:Math.max(.08,Math.abs(def.h)+.12),geo=new THREE.BoxGeometry(.97,height,.97),mesh=new THREE.InstancedMesh(geo,mat,items.length);
      items.forEach((r,i)=>{const top=name==="wall"?elevation(r)+2.35:elevation(r);matrix.makeTranslation(Number(r.x)-state.centerX,top-height/2,Number(r.y)-state.centerY);mesh.setMatrixAt(i,matrix);});mesh.instanceMatrix.needsUpdate=true;state.terrainRoot.add(mesh);
    }else{
      const surface=new THREE.Mesh(buildSurfaceGeometry(items,state.tileByKey,state.centerX,state.centerY),mat);surface.receiveShadow=state.quality==="decorative";state.terrainRoot.add(surface);
      if(name==="wall"){const geo=new THREE.BoxGeometry(1.015,2.35,1.015),walls=new THREE.InstancedMesh(geo,mat,items.length);items.forEach((r,i)=>{matrix.makeTranslation(Number(r.x)-state.centerX,elevation(r)+1.175,Number(r.y)-state.centerY);walls.setMatrixAt(i,matrix);});walls.instanceMatrix.needsUpdate=true;walls.castShadow=walls.receiveShadow=state.quality==="decorative";state.terrainRoot.add(walls);}
    }
  }
  buildTabletop(state,rows,xs,ys);
  if(state.quality!=="low")buildDecor(state,rows);
  const boundsSignature=[Math.min(...xs),Math.max(...xs),Math.min(...ys),Math.max(...ys)].join(",");if(state.boundsSignature!==boundsSignature){const size=Math.max(Math.max(...xs)-Math.min(...xs)+1,Math.max(...ys)-Math.min(...ys)+1),dist=Math.max(12,size*1.25);state.controls.target.set(0,0,0);state.camera.position.set(dist,dist*.78,dist);state.controls.update();state.boundsSignature=boundsSignature;}
}
function buildDecor(state,rows){
  const forests=rows.filter(r=>["forest","woodland"].includes(terrainName(r.terrain)));if(!forests.length)return;
  const trunkGeo=new THREE.CylinderGeometry(.065,.115,.72,6),lowerGeo=new THREE.ConeGeometry(.31,.72,7),upperGeo=new THREE.ConeGeometry(.23,.62,7);
  const trunkMat=new THREE.MeshLambertMaterial({color:0x604027}),lowerMat=new THREE.MeshLambertMaterial({color:0x315f38}),upperMat=new THREE.MeshLambertMaterial({color:0x447848});
  const trunks=new THREE.InstancedMesh(trunkGeo,trunkMat,forests.length),lower=new THREE.InstancedMesh(lowerGeo,lowerMat,forests.length),upper=new THREE.InstancedMesh(upperGeo,upperMat,forests.length);
  const matrix=new THREE.Matrix4(),position=new THREE.Vector3(),rotation=new THREE.Quaternion(),scale=new THREE.Vector3();
  forests.forEach((r,i)=>{
    const angle=seeded(r.x,r.y,1)*Math.PI*2,size=.82+seeded(r.x,r.y,2)*.34,offset=.18+seeded(r.x,r.y,3)*.12;
    const x=Number(r.x)-state.centerX+Math.cos(angle)*offset,z=Number(r.y)-state.centerY+Math.sin(angle)*offset;
    rotation.setFromAxisAngle(new THREE.Vector3(0,1,0),angle);scale.set(size,size,size);
    const ground=elevation(r);position.set(x,ground+.48*size,z);matrix.compose(position,rotation,scale);trunks.setMatrixAt(i,matrix);
    position.set(x,ground+.90*size,z);matrix.compose(position,rotation,scale);lower.setMatrixAt(i,matrix);
    position.set(x,ground+1.25*size,z);matrix.compose(position,rotation,scale);upper.setMatrixAt(i,matrix);
  });
  for(const mesh of [trunks,lower,upper]){mesh.instanceMatrix.needsUpdate=true;mesh.castShadow=true;mesh.receiveShadow=true;}
  state.decorRoot.add(trunks,lower,upper);
}
function modelParts(row){return [row.model_base,row.model_hair,row.model_body,row.model_arms,row.model_legs,row.model_feet,row.model_headgear,row.model_accessory].map(x=>String(x||"")).filter(Boolean);}
function characterTemplate(row){
  const parts=modelParts(row),hair=String(row.hair_color||"#3b2416"),key=`${parts.join("|")}|${hair}`;if(!parts.length)return null;
  if(!characterTemplateCache.has(key))characterTemplateCache.set(key,Promise.all(parts.map(loadModelAsset)).then(assets=>{const group=new THREE.Group();assets.forEach((asset,i)=>{const part=cloneSkeleton(asset);part.traverse(obj=>{if(!obj.isMesh)return;obj.castShadow=false;obj.receiveShadow=true;if(/hair|beard/i.test(parts[i])){obj.material=Array.isArray(obj.material)?obj.material.map(m=>m.clone()):obj.material?.clone();const mats=Array.isArray(obj.material)?obj.material:[obj.material];mats.filter(Boolean).forEach(m=>{if(m.color)m.color.set(hair);});}});group.add(part);});const box=new THREE.Box3().setFromObject(group),size=box.getSize(new THREE.Vector3()),center=box.getCenter(new THREE.Vector3());group.position.set(-center.x,-box.min.y,-center.z);if(size.y>0)group.scale.setScalar(.78/size.y);group.rotation.y=Math.PI;return group;}));
  return characterTemplateCache.get(key);
}
function attachCharacterModel(state,token,row){const pending=characterTemplate(row);if(!pending)return;pending.then(template=>{if(token.parent!==state.tokenRoot)return;const model=cloneSkeleton(template);token.add(model);token.userData.characterModel=model;if(token.userData.fallback)token.userData.fallback.visible=false;requestRender(state);}).catch(error=>console.warn("3D miniature failed; using fallback token.",error));}
function updateTokens(state,rows){
  clearGroup(state.tokenRoot);const geo=new THREE.CylinderGeometry(.25,.30,.62,8);
  for(const row of rows){if(!row.occupant_id)continue;const player=String(row.occupant_type)==="player",active=truthy(row.is_active_actor),mat=new THREE.MeshLambertMaterial({color:active?0xffd34d:player?0x48b9df:0xd65b42});
    const token=new THREE.Group(),fallback=new THREE.Mesh(geo,mat);fallback.position.y=.34;token.add(fallback);token.position.set(Number(row.x)-state.centerX,elevation(row),Number(row.y)-state.centerY);token.userData={actorId:String(row.occupant_id),actorType:row.occupant_type,x:Number(row.x),y:Number(row.y),fallback};state.tokenRoot.add(token);if(state.quality==="decorative"&&player)attachCharacterModel(state,token,row);
  }
}
function addCylinderBetween(group,a,b,radius,material){const delta=b.clone().sub(a),length=delta.length();if(!length)return;const mesh=new THREE.Mesh(new THREE.CylinderGeometry(radius,radius,length,8),material);mesh.position.copy(a).add(b).multiplyScalar(.5);mesh.quaternion.setFromUnitVectors(new THREE.Vector3(0,1,0),delta.normalize());group.add(mesh);}
function updateOverlays(state,rows,zones){
  clearGroup(state.overlayRoot);
  const path=rows.filter(r=>truthy(r.is_pending_move)).sort((a,b)=>Number(a.move_path_step||0)-Number(b.move_path_step||0));
  if(path.length){const mat=new THREE.MeshBasicMaterial({color:0xffd34d,transparent:true,opacity:.9,depthWrite:false}),points=path.map(r=>new THREE.Vector3(Number(r.x)-state.centerX,elevation(r)+.16,Number(r.y)-state.centerY));for(let i=1;i<points.length;i++)addCylinderBetween(state.overlayRoot,points[i-1],points[i],.055,mat);for(const p of points){const marker=new THREE.Mesh(new THREE.CylinderGeometry(.16,.16,.035,18),mat);marker.position.copy(p);state.overlayRoot.add(marker);}}
  for(const zone of zones){const radius=Math.max(.5,Number(zone.area_ft||5)/5),x=Number(zone.center_x),y=Number(zone.center_y);if(!Number.isFinite(x)||!Number.isFinite(y))continue;const tile=state.tileByKey.get(`${x},${y}`),height=(tile?elevation(tile):.1)+.18,color=new THREE.Color(zone.colour||((String(zone.glyph_type)==="ward")?"#3a78c2":"#d63b2f"));const fill=new THREE.Mesh(new THREE.CircleGeometry(radius,48),new THREE.MeshBasicMaterial({color,transparent:true,opacity:.22,side:THREE.DoubleSide,depthWrite:false}));fill.rotation.x=-Math.PI/2;fill.position.set(x-state.centerX,height,y-state.centerY);fill.userData={tooltip:zone.tooltip||zone.name||"Glyph area"};state.overlayRoot.add(fill);const ring=new THREE.Mesh(new THREE.RingGeometry(Math.max(.01,radius-.06),radius,48),new THREE.MeshBasicMaterial({color,transparent:true,opacity:.8,side:THREE.DoubleSide,depthWrite:false}));ring.rotation.x=-Math.PI/2;ring.position.copy(fill.position);state.overlayRoot.add(ring);}
}
function render(message){
  const rows=normalise(message.mapData),zones=normalise(message.zones);if(!rows.length)return;const quality=["low","balanced","decorative"].includes(message.quality)?message.quality:"balanced";
  let state=states.get(message.containerId),el=document.getElementById(message.containerId);if(!el)return;
  if(!state||state.el!==el||state.quality!==quality)state=makeState(message.containerId,message.inputIds,quality);else state.inputIds=message.inputIds||state.inputIds;
  if(!state)return;const sig=signature(rows);if(sig!==state.signature){buildTerrain(state,rows);state.signature=sig;}else{state.tiles=rows;state.tileByKey=new Map(rows.map(r=>[`${r.x},${r.y}`,r]));}
  updateTokens(state,rows);updateOverlays(state,rows,zones);requestRender(state);
}
Shiny.addCustomMessageHandler("combat3d-lean-init",render);
Shiny.addCustomMessageHandler("combat3d-lean-resize",m=>{const s=states.get(m.containerId);if(s){s.renderer.setSize(Math.max(1,s.el.clientWidth),Math.max(1,s.el.clientHeight),false);requestRender(s);}});
function signalReady(attempt=0){
  if(window.Shiny?.setInputValue){Shiny.setInputValue("combat3d_lean_ready",{nonce:Date.now()+Math.random()},{priority:"event"});return;}
  if(attempt<40)setTimeout(()=>signalReady(attempt+1),100);
}
signalReady();
