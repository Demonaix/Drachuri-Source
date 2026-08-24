import * as THREE from "three";
import { OrbitControls } from "three/addons/controls/OrbitControls.js";

const states = new Map();
const TERRAIN = {
  grass:{color:0x719a58,tex:"grass.jpg",h:.10}, sand:{color:0xc7ad70,tex:"dirt.jpg",h:.02},
  forest:{color:0x315f38,tex:"forest.jpg",h:.14}, woodland:{color:0x315f38,tex:"forest.jpg",h:.14},
  water:{color:0x367eaa,tex:"water.jpg",h:-.18}, stone:{color:0x85837b,tex:"stone.jpg",h:.10},
  wall:{color:0x4e4d49,tex:"stone.jpg",h:.10}, road:{color:0xa5885b,tex:"dirt.jpg",h:-.07},
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
function texture(state,file){
  if(!file)return null;if(state.textures[file])return state.textures[file];
  const t=new THREE.TextureLoader().load(`assets/textures/${file}`,()=>requestRender(state));
  t.colorSpace=THREE.SRGBColorSpace;t.wrapS=t.wrapT=THREE.RepeatWrapping;t.repeat.set(1.5,1.5);state.textures[file]=t;return t;
}
function makeState(containerId,inputIds,quality){
  const el=document.getElementById(containerId);if(!el)return null;
  const previous=states.get(containerId);if(previous)disposeState(previous);
  const renderer=new THREE.WebGLRenderer({antialias:quality!=="low",powerPreference:"high-performance"});
  renderer.setPixelRatio(quality==="low"?1:Math.min(devicePixelRatio||1,1.5));renderer.setSize(Math.max(1,el.clientWidth),Math.max(1,el.clientHeight),false);
  renderer.outputColorSpace=THREE.SRGBColorSpace;renderer.shadowMap.enabled=quality==="decorative";el.replaceChildren(renderer.domElement);
  const scene=new THREE.Scene();scene.background=new THREE.Color(0x202730);scene.add(new THREE.HemisphereLight(0xe4edff,0x493a2b,1.25));
  const sun=new THREE.DirectionalLight(0xffe1b2,quality==="low"?.65:1.05);sun.position.set(14,24,10);sun.castShadow=quality==="decorative";
  if(sun.castShadow){sun.shadow.mapSize.set(1024,1024);sun.shadow.camera.left=-30;sun.shadow.camera.right=30;sun.shadow.camera.top=30;sun.shadow.camera.bottom=-30;}scene.add(sun);
  const camera=new THREE.PerspectiveCamera(48,1,.1,500),controls=new OrbitControls(camera,renderer.domElement);
  controls.enableDamping=false;controls.maxPolarAngle=Math.PI/2.04;controls.minDistance=4;controls.maxDistance=100;
  const state={containerId,el,renderer,scene,camera,controls,inputIds:inputIds||{},quality,textures:{},terrainRoot:new THREE.Group(),tokenRoot:new THREE.Group(),decorRoot:new THREE.Group(),tiles:[],tileByKey:new Map(),signature:"",centerX:0,centerY:0,renderPending:false};
  scene.add(state.terrainRoot,state.decorRoot,state.tokenRoot);controls.addEventListener("change",()=>requestRender(state));
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
      if(name==="wall"){const geo=new THREE.BoxGeometry(.92,2.35,.92),walls=new THREE.InstancedMesh(geo,mat,items.length);items.forEach((r,i)=>{matrix.makeTranslation(Number(r.x)-state.centerX,elevation(r)+1.175,Number(r.y)-state.centerY);walls.setMatrixAt(i,matrix);});walls.instanceMatrix.needsUpdate=true;walls.castShadow=walls.receiveShadow=state.quality==="decorative";state.terrainRoot.add(walls);}
    }
  }
  if(state.quality==="decorative")buildDecor(state,rows);
  const size=Math.max(Math.max(...xs)-Math.min(...xs)+1,Math.max(...ys)-Math.min(...ys)+1),dist=Math.max(12,size*1.25);state.controls.target.set(0,0,0);state.camera.position.set(dist,dist*.78,dist);state.controls.update();
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
function updateTokens(state,rows){
  clearGroup(state.tokenRoot);const geo=new THREE.CylinderGeometry(.25,.30,.62,8);
  for(const row of rows){if(!row.occupant_id)continue;const player=String(row.occupant_type)==="player",active=truthy(row.is_active_actor),mat=new THREE.MeshLambertMaterial({color:active?0xffd34d:player?0x48b9df:0xd65b42});
    const token=new THREE.Mesh(geo,mat);token.position.set(Number(row.x)-state.centerX,elevation(row)+.34,Number(row.y)-state.centerY);token.userData={actorId:String(row.occupant_id),actorType:row.occupant_type,x:Number(row.x),y:Number(row.y)};state.tokenRoot.add(token);
  }
}
function render(message){
  const rows=normalise(message.mapData);if(!rows.length)return;const quality=["low","balanced","decorative"].includes(message.quality)?message.quality:"balanced";
  let state=states.get(message.containerId),el=document.getElementById(message.containerId);if(!el)return;
  if(!state||state.el!==el||state.quality!==quality)state=makeState(message.containerId,message.inputIds,quality);else state.inputIds=message.inputIds||state.inputIds;
  if(!state)return;const sig=signature(rows);if(sig!==state.signature){buildTerrain(state,rows);state.signature=sig;}else{state.tiles=rows;state.tileByKey=new Map(rows.map(r=>[`${r.x},${r.y}`,r]));}
  updateTokens(state,rows);requestRender(state);
}
Shiny.addCustomMessageHandler("combat3d-lean-init",render);
Shiny.addCustomMessageHandler("combat3d-lean-resize",m=>{const s=states.get(m.containerId);if(s){s.renderer.setSize(Math.max(1,s.el.clientWidth),Math.max(1,s.el.clientHeight),false);requestRender(s);}});
