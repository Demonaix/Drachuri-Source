import * as THREE from "three";
import { OrbitControls } from "three/addons/controls/OrbitControls.js";

const states = new Map();
const TERRAIN = {
  grass:{color:0x719a58,tex:"grass.jpg",h:.10}, sand:{color:0xc7ad70,tex:"dirt.jpg",h:.02},
  forest:{color:0x315f38,tex:"forest.jpg",h:.14}, woodland:{color:0x315f38,tex:"forest.jpg",h:.14},
  water:{color:0x367eaa,tex:"water.jpg",h:-.18}, stone:{color:0x85837b,tex:"stone.jpg",h:.10},
  wall:{color:0x76706a,tex:"battlefield_fieldstone.jpg",h:.10}, road:{color:0xa5885b,tex:"dirt.jpg",h:-.07},
  swamp:{color:0x526944,tex:"swamp.jpg",h:-.03}, ravine:{color:0x17151a,tex:"ravine.jpg",h:-2.65},
  pit:{color:0x17151a,tex:"pit.jpg",h:-2.10}, mandred_convergence:{color:0x714ca1,tex:"stone.jpg",h:.12}
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
function constrainCamera(state){
  if(!state?.camera)return;const margin=.7,halfW=Math.max(2,state.roomWidth/2-margin),halfD=Math.max(2,state.roomDepth/2-margin),floor=state.roomFloorY+.45,ceiling=state.roomFloorY+17.3;
  state.camera.position.x=THREE.MathUtils.clamp(state.camera.position.x,-halfW,halfW);state.camera.position.z=THREE.MathUtils.clamp(state.camera.position.z,-halfD,halfD);state.camera.position.y=THREE.MathUtils.clamp(state.camera.position.y,floor,ceiling);
  state.controls.target.x=THREE.MathUtils.clamp(state.controls.target.x,-halfW,halfW);state.controls.target.z=THREE.MathUtils.clamp(state.controls.target.z,-halfD,halfD);state.controls.target.y=THREE.MathUtils.clamp(state.controls.target.y,floor,ceiling-1);
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
  const state={containerId,el,renderer,scene,camera,controls,inputIds:inputIds||{},quality,textures:{},terrainRoot:new THREE.Group(),tokenRoot:new THREE.Group(),decorRoot:new THREE.Group(),overlayRoot:new THREE.Group(),posterRoot:new THREE.Group(),tiles:[],tileByKey:new Map(),signature:"",posterSignature:"",boundsSignature:"",centerX:0,centerY:0,roomWidth:40,roomDepth:40,roomFloorY:-6.7,renderPending:false};
  scene.add(state.terrainRoot,state.decorRoot,state.overlayRoot,state.tokenRoot,state.posterRoot);controls.addEventListener("change",()=>{constrainCamera(state);requestRender(state);});
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
function makeTextTexture(state,text){const key=`canvas-label:${text}`;if(state.textures[key])return state.textures[key];const canvas=document.createElement("canvas");canvas.width=768;canvas.height=150;const c=canvas.getContext("2d");c.clearRect(0,0,canvas.width,canvas.height);c.fillStyle="#291c12";let size=104;c.font=`bold ${size}px Georgia`;while(c.measureText(text).width>710&&size>48){size-=4;c.font=`bold ${size}px Georgia`;}c.textAlign="center";c.textBaseline="middle";c.fillText(text,canvas.width/2,canvas.height/2+5);const tex=new THREE.CanvasTexture(canvas);tex.colorSpace=THREE.SRGBColorSpace;state.textures[key]=tex;return tex;}
function buildTabletop(state,rows,xs,ys){
  const bottom=-3.05,tileMap=state.tileByKey,edges=[];
  for(const row of rows){const x=Number(row.x),y=Number(row.y),top=Math.max(elevation(row),-.05);for(const [dx,dy,rot] of [[-1,0,Math.PI/2],[1,0,Math.PI/2],[0,-1,0],[0,1,0]])if(!tileMap.has(`${x+dx},${y+dy}`))edges.push({x:x-state.centerX+dx*.5,z:y-state.centerY+dy*.5,top,rot});}
  if(edges.length){const geo=new THREE.BoxGeometry(1,1,.075),mat=new THREE.MeshLambertMaterial({color:0x3d2d20}),fascia=new THREE.InstancedMesh(geo,mat,edges.length),matrix=new THREE.Matrix4(),position=new THREE.Vector3(),rotation=new THREE.Quaternion(),scale=new THREE.Vector3();edges.forEach((e,i)=>{const height=e.top-bottom;position.set(e.x,bottom+height/2,e.z);rotation.setFromAxisAngle(new THREE.Vector3(0,1,0),e.rot);scale.set(1.01,height,1);matrix.compose(position,rotation,scale);fascia.setMatrixAt(i,matrix);});fascia.instanceMatrix.needsUpdate=true;fascia.receiveShadow=true;state.terrainRoot.add(fascia);}
  const mapWidth=Math.max(...xs)-Math.min(...xs)+1,mapDepth=Math.max(...ys)-Math.min(...ys)+1,tableWidth=mapWidth+4.5,tableDepth=mapDepth+4.5;
  const wood=new THREE.MeshLambertMaterial({color:0xffffff,map:texture(state,"tavern_table_oak.jpg",Math.max(1,tableWidth/7),Math.max(1,tableDepth/7))}),darkWood=new THREE.MeshLambertMaterial({color:0x3b2518}),plaster=new THREE.MeshLambertMaterial({color:0xffffff,map:texture(state,"tavern_plaster_timbers.jpg",2,1.5)}),floorMat=new THREE.MeshLambertMaterial({color:0xffffff});
  const top=new THREE.Mesh(new THREE.BoxGeometry(tableWidth,.34,tableDepth),wood);top.position.y=bottom-.17;top.receiveShadow=true;top.castShadow=state.quality==="decorative";state.terrainRoot.add(top);
  const apronY=bottom-.58,apronH=.72;for(const apron of [
    [tableWidth-.35,apronH,.22,0,apronY,-tableDepth/2+.22],[tableWidth-.35,apronH,.22,0,apronY,tableDepth/2-.22],
    [.22,apronH,tableDepth-.35,-tableWidth/2+.22,apronY,0],[.22,apronH,tableDepth-.35,tableWidth/2-.22,apronY,0]
  ]){const mesh=new THREE.Mesh(new THREE.BoxGeometry(apron[0],apron[1],apron[2]),darkWood);mesh.position.set(apron[3],apron[4],apron[5]);mesh.castShadow=true;state.terrainRoot.add(mesh);}
  const floorY=bottom-3.65,legH=3.15,legGeo=new THREE.BoxGeometry(.48,legH,.48);for(const x of [-tableWidth/2+.65,tableWidth/2-.65])for(const z of [-tableDepth/2+.65,tableDepth/2-.65]){const leg=new THREE.Mesh(legGeo,darkWood);leg.position.set(x,bottom-.34-legH/2,z);leg.castShadow=true;state.terrainRoot.add(leg);}
  const roomSpan=Math.max(40,Math.max(mapWidth,mapDepth)*3),roomWidth=roomSpan,roomDepth=roomSpan;
  const ceilingMat=new THREE.MeshLambertMaterial({color:0xb69b72,map:texture(state,"tavern_floorboards.jpg",Math.max(2,roomSpan/8),Math.max(2,roomSpan/8))});
  state.roomWidth=roomWidth;state.roomDepth=roomDepth;state.roomFloorY=floorY;state.posterSignature="";state.controls.maxDistance=Math.min(roomWidth,roomDepth)*.48;
  floorMat.map=texture(state,"tavern_floorboards.jpg",Math.max(2,roomWidth/8),Math.max(2,roomDepth/8));floorMat.needsUpdate=true;
  const floor=new THREE.Mesh(new THREE.BoxGeometry(roomWidth,.28,roomDepth),floorMat);floor.position.y=floorY;floor.receiveShadow=true;state.terrainRoot.add(floor);
  const wallH=18,wallY=floorY+wallH/2,frontZ=roomDepth/2;for(const wall of [[roomWidth,wallH,.35,0,wallY,-roomDepth/2],[.35,wallH,roomDepth,-roomWidth/2,wallY,0],[.35,wallH,roomDepth,roomWidth/2,wallY,0]]){const mesh=new THREE.Mesh(new THREE.BoxGeometry(wall[0],wall[1],wall[2]),plaster);mesh.position.set(wall[3],wall[4],wall[5]);mesh.receiveShadow=true;state.terrainRoot.add(mesh);}
  const doorW=3.5,doorH=6.7,doorX=roomWidth*.27,leftW=doorX-doorW/2+roomWidth/2,rightW=roomWidth/2-(doorX+doorW/2);for(const part of [[leftW,wallH,-roomWidth/2+leftW/2,wallY],[rightW,wallH,doorX+doorW/2+rightW/2,wallY],[doorW,wallH-doorH,doorX,floorY+doorH+(wallH-doorH)/2]]){const mesh=new THREE.Mesh(new THREE.BoxGeometry(part[0],part[1],.35),plaster);mesh.position.set(part[2],part[3],frontZ);mesh.receiveShadow=true;state.terrainRoot.add(mesh);}
  const door=new THREE.Mesh(new THREE.PlaneGeometry(doorW,doorH),new THREE.MeshLambertMaterial({color:0xffffff,map:texture(state,"tavern_door_oak.jpg",1,1),side:THREE.DoubleSide}));door.position.set(doorX,floorY+doorH/2,frontZ-.19);state.terrainRoot.add(door);
  const ceiling=new THREE.Mesh(new THREE.BoxGeometry(roomWidth,.28,roomDepth),ceilingMat);ceiling.position.y=floorY+wallH+.14;ceiling.receiveShadow=true;state.terrainRoot.add(ceiling);
  const rafterY=floorY+wallH-.42,rafterCount=Math.max(5,Math.min(11,Math.round(roomDepth/6)));for(let i=0;i<rafterCount;i++){const z=-roomDepth/2+.9+i*(roomDepth-1.8)/Math.max(1,rafterCount-1),beam=new THREE.Mesh(new THREE.BoxGeometry(roomWidth-.5,.32,.38),darkWood);beam.position.set(0,rafterY,z);beam.castShadow=true;state.terrainRoot.add(beam);}for(const x of [-roomWidth/2+.32,roomWidth/2-.32]){const beam=new THREE.Mesh(new THREE.BoxGeometry(.4,.44,roomDepth-.5),darkWood);beam.position.set(x,rafterY-.08,0);beam.castShadow=true;state.terrainRoot.add(beam);}
  const dracnosW=3.6,dracnosH=5.4,dracnosZ=-roomDepth*.18,dracnosY=floorY+8.5,dracnosX=-roomWidth/2+.19,dracnos=new THREE.Mesh(new THREE.PlaneGeometry(dracnosW,dracnosH),new THREE.MeshBasicMaterial({map:texture(state,"wanted_dracnos.png",1,1),side:THREE.DoubleSide}));dracnos.rotation.y=Math.PI/2;dracnos.position.set(dracnosX,dracnosY,dracnosZ);state.terrainRoot.add(dracnos);
  const nameLabel=new THREE.Mesh(new THREE.PlaneGeometry(3,.58),new THREE.MeshBasicMaterial({map:makeTextTexture(state,"DRACNOS"),transparent:true,side:THREE.DoubleSide,depthWrite:false}));nameLabel.rotation.y=Math.PI/2;nameLabel.position.set(dracnosX+.025,dracnosY-dracnosH*.405,dracnosZ);state.terrainRoot.add(nameLabel);
  for(const [dy,dz] of [[dracnosH*.43,-dracnosW*.42],[dracnosH*.43,dracnosW*.42],[-dracnosH*.43,-dracnosW*.42],[-dracnosH*.43,dracnosW*.42]]){const pin=new THREE.Mesh(new THREE.SphereGeometry(.065,8,6),new THREE.MeshBasicMaterial({color:0x3d291c}));pin.position.set(dracnosX+.045,dracnosY+dy,dracnosZ+dz);state.terrainRoot.add(pin);}
  const blacklynW=3.6,blacklynH=5.4,blacklynZ=roomDepth*.12,blacklynY=floorY+8.5,blacklynX=-roomWidth/2+.19,blacklyn=new THREE.Mesh(new THREE.PlaneGeometry(blacklynW,blacklynH),new THREE.MeshBasicMaterial({map:texture(state,"wanted_lord_blacklyn.png",1,1),side:THREE.DoubleSide}));blacklyn.rotation.y=Math.PI/2;blacklyn.position.set(blacklynX,blacklynY,blacklynZ);state.terrainRoot.add(blacklyn);
  for(const [text,width,height,dy] of [["DANGEROUS",3,.46,2.12],["LORD BLACKLYN",3.05,.42,-1.69],["FAE ALLY",2.7,.36,-2.22]]){const label=new THREE.Mesh(new THREE.PlaneGeometry(width,height),new THREE.MeshBasicMaterial({map:makeTextTexture(state,text),transparent:true,side:THREE.DoubleSide,depthWrite:false}));label.rotation.y=Math.PI/2;label.position.set(blacklynX+.025,blacklynY+dy,blacklynZ);state.terrainRoot.add(label);}
  for(const [dy,dz] of [[blacklynH*.43,-blacklynW*.42],[blacklynH*.43,blacklynW*.42],[-blacklynH*.43,-blacklynW*.42],[-blacklynH*.43,blacklynW*.42]]){const pin=new THREE.Mesh(new THREE.SphereGeometry(.065,8,6),new THREE.MeshBasicMaterial({color:0x3d291c}));pin.position.set(blacklynX+.045,blacklynY+dy,blacklynZ+dz);state.terrainRoot.add(pin);}
  const vowW=3.6,vowH=5.4,vowZ=roomDepth*.36,vowY=floorY+8.5,vowX=-roomWidth/2+.19,vow=new THREE.Mesh(new THREE.PlaneGeometry(vowW,vowH),new THREE.MeshBasicMaterial({map:texture(state,"propaganda_iron_vow_disbanded.png",1,1),side:THREE.DoubleSide}));vow.rotation.y=Math.PI/2;vow.position.set(vowX,vowY,vowZ);state.terrainRoot.add(vow);
  for(const [dy,dz] of [[vowH*.43,-vowW*.42],[vowH*.43,vowW*.42],[-vowH*.43,-vowW*.42],[-vowH*.43,vowW*.42]]){const pin=new THREE.Mesh(new THREE.SphereGeometry(.065,8,6),new THREE.MeshBasicMaterial({color:0x3d291c}));pin.position.set(vowX+.045,vowY+dy,vowZ+dz);state.terrainRoot.add(pin);}
  const successionW=3.6,successionH=5.4,successionZ=-roomDepth*.39,successionY=floorY+8.5,successionX=-roomWidth/2+.19,succession=new THREE.Mesh(new THREE.PlaneGeometry(successionW,successionH),new THREE.MeshBasicMaterial({map:texture(state,"propaganda_order_succession_fae.png",1,1),side:THREE.DoubleSide}));succession.rotation.y=Math.PI/2;succession.position.set(successionX,successionY,successionZ);state.terrainRoot.add(succession);
  for(const [dy,dz] of [[successionH*.43,-successionW*.42],[successionH*.43,successionW*.42],[-successionH*.43,-successionW*.42],[-successionH*.43,successionW*.42]]){const pin=new THREE.Mesh(new THREE.SphereGeometry(.065,8,6),new THREE.MeshBasicMaterial({color:0x3d291c}));pin.position.set(successionX+.045,successionY+dy,successionZ+dz);state.terrainRoot.add(pin);}
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
function ruinedWallGeometry(height=1.65){const geometry=new THREE.BoxGeometry(1.015,height,1.015,3,3,3),position=geometry.getAttribute("position");for(let i=0;i<position.count;i++){const y=position.getY(i);if(y>height/2-.01){const x=position.getX(i),z=position.getZ(i),chip=(seeded(Math.round((x+.51)*12),Math.round((z+.51)*12),4))* .18;position.setY(i,y-chip);}}position.needsUpdate=true;geometry.computeVertexNormals();return geometry;}
function buildTerrain(state,rows){
  clearGroup(state.terrainRoot);clearGroup(state.decorRoot);state.tileByKey.clear();state.tiles=rows;
  const xs=rows.map(r=>Number(r.x)),ys=rows.map(r=>Number(r.y));state.centerX=(Math.min(...xs)+Math.max(...xs))/2;state.centerY=(Math.min(...ys)+Math.max(...ys))/2;
  const grouped={};for(const row of rows){const t=terrainName(row.terrain);(grouped[t]??=[]).push(row);state.tileByKey.set(`${row.x},${row.y}`,row);}
  const matrix=new THREE.Matrix4();
  for(const [name,items] of Object.entries(grouped)){const def=TERRAIN[name],mat=new THREE.MeshLambertMaterial({color:def.color,map:texture(state,def.tex)});
    if(state.quality==="low"){
      const height=name==="wall"?1.65:Math.max(.08,Math.abs(def.h)+.12),geo=new THREE.BoxGeometry(.97,height,.97),mesh=new THREE.InstancedMesh(geo,mat,items.length);
      items.forEach((r,i)=>{const top=name==="wall"?elevation(r)+1.65:elevation(r);matrix.makeTranslation(Number(r.x)-state.centerX,top-height/2,Number(r.y)-state.centerY);mesh.setMatrixAt(i,matrix);});mesh.instanceMatrix.needsUpdate=true;state.terrainRoot.add(mesh);
    }else{
      const surface=new THREE.Mesh(buildSurfaceGeometry(items,state.tileByKey,state.centerX,state.centerY),mat);surface.receiveShadow=state.quality==="decorative";state.terrainRoot.add(surface);
      if(name==="wall"){const geo=ruinedWallGeometry(),walls=new THREE.InstancedMesh(geo,mat,items.length),position=new THREE.Vector3(),rotation=new THREE.Quaternion(),scale=new THREE.Vector3(1,1,1);items.forEach((r,i)=>{position.set(Number(r.x)-state.centerX,elevation(r)+.825,Number(r.y)-state.centerY);rotation.setFromAxisAngle(new THREE.Vector3(0,1,0),Math.floor(seeded(r.x,r.y,12)*4)*Math.PI/2);matrix.compose(position,rotation,scale);walls.setMatrixAt(i,matrix);});walls.instanceMatrix.needsUpdate=true;walls.castShadow=walls.receiveShadow=state.quality==="decorative";state.terrainRoot.add(walls);}
    }
  }
  buildTabletop(state,rows,xs,ys);
  if(state.quality!=="low")buildDecor(state,rows);
  const boundsSignature=[Math.min(...xs),Math.max(...xs),Math.min(...ys),Math.max(...ys)].join(",");if(state.boundsSignature!==boundsSignature){const size=Math.max(Math.max(...xs)-Math.min(...xs)+1,Math.max(...ys)-Math.min(...ys)+1),dist=Math.max(12,size*1.25);state.controls.target.set(0,0,0);state.camera.position.set(dist,dist*.78,dist);state.controls.update();state.boundsSignature=boundsSignature;}
}
function buildDecor(state,rows){
  const forests=rows.filter(r=>["forest","woodland"].includes(terrainName(r.terrain)));if(!forests.length)return;
  const trunkGeo=new THREE.CylinderGeometry(.065,.115,.72,6),lowerGeo=new THREE.ConeGeometry(.31,.72,7),upperGeo=new THREE.ConeGeometry(.23,.62,7);
  const trunkMat=new THREE.MeshLambertMaterial({color:0xffffff,map:texture(state,"bark.jpg",1,2)}),lowerMat=new THREE.MeshLambertMaterial({color:0x8eb58a,map:texture(state,"leaves.jpg",1.5,1.5)}),upperMat=new THREE.MeshLambertMaterial({color:0xa4c69b,map:texture(state,"leaves.jpg",1.5,1.5)});
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
function actorColour(id,player,self){if(self)return new THREE.Color(0x39c4e5);if(!player)return new THREE.Color(0xb54538);let hash=0;for(const c of String(id))hash=(hash*31+c.charCodeAt(0))|0;return new THREE.Color().setHSL(((Math.abs(hash)%300)+25)/360,.58,.48);}
function firstValue(row,names,fallback=""){for(const name of names)if(row?.[name]!=null&&String(row[name])!=="")return row[name];return fallback;}
function posterActors(rows){const seen=new Set(),actors=[];for(const row of rows){const id=String(row.occupant_id||"");if(!id||seen.has(id))continue;seen.add(id);actors.push(row);}return actors;}
function posterConditions(row){const raw=firstValue(row,["occupant_conditions","conditions","status_effects","effects"],"");if(Array.isArray(raw))return raw.map(String).filter(Boolean);if(raw&&typeof raw==="object")return Object.keys(raw).filter(k=>truthy(raw[k]));return String(raw||"").split(/[,;|]/).map(x=>x.trim()).filter(Boolean);}
function posterCanvas(row){
  const canvas=document.createElement("canvas");canvas.width=640;canvas.height=800;const c=canvas.getContext("2d");c.scale(640/384,800/480);const player=String(row.occupant_type)==="player",active=truthy(row.is_active_actor),self=truthy(row.is_self_actor),name=String(firstValue(row,["occupant_name","display_name","name"],player?"Unknown Adventurer":"Unknown Foe")),cur=Number(firstValue(row,["occupant_current_hp","current_hp","hp_current"],NaN)),max=Number(firstValue(row,["occupant_max_hp","max_hp","hp_max"],NaN)),temp=Number(firstValue(row,["occupant_temp_hp","temp_hp"],0)),si=Number(firstValue(row,["occupant_sindre_cur","sindre_cur"],NaN)),siMax=Number(firstValue(row,["occupant_sindre_max","sindre_max"],NaN)),siTemp=Number(firstValue(row,["occupant_sindre_temp","sindre_temp"],0)),conditions=posterConditions(row);
  c.fillStyle="#d8bd82";c.fillRect(0,0,384,480);const stain=c.createRadialGradient(190,210,30,190,210,270);stain.addColorStop(0,"rgba(255,246,193,.46)");stain.addColorStop(1,"rgba(83,47,20,.24)");c.fillStyle=stain;c.fillRect(0,0,384,480);c.strokeStyle=active?"#c8392d":self?"#247b91":"#4b2f1b";c.lineWidth=active?18:10;c.strokeRect(12,12,360,456);c.strokeStyle="#6a4525";c.lineWidth=3;c.strokeRect(28,28,328,424);
  c.fillStyle=player?"#315f76":"#8c3027";c.font="bold 25px Georgia";c.textAlign="center";c.fillText(player?(self?"YOUR COMPANY":"ADVENTURER"):"WANTED",192,68);
  c.fillStyle="#2d2015";c.font="bold 34px Georgia";const words=name.split(/\s+/);let lines=[""];for(const word of words){const test=(lines.at(-1)+" "+word).trim();if(c.measureText(test).width>320&&lines.at(-1))lines.push(word);else lines[lines.length-1]=test;}lines.slice(0,2).forEach((line,i)=>c.fillText(line,192,128+i*40));
  c.fillStyle=player?"#477d8d":"#86382e";c.beginPath();c.arc(192,246,62,0,Math.PI*2);c.fill();c.fillStyle="#eadcae";c.font="bold 58px Georgia";c.fillText(name.trim().charAt(0).toUpperCase()||"?",192,266);
  if(Number.isFinite(cur)&&Number.isFinite(max)&&max>0){const pct=Math.max(0,Math.min(1,cur/max));c.fillStyle="#4d3725";c.fillRect(48,326,288,27);c.fillStyle=pct>.5?"#4b8a45":pct>.25?"#c28b32":"#a43a30";c.fillRect(52,330,280*pct,19);c.fillStyle="#f5e8c3";c.font="bold 18px Georgia";c.fillText(`HP ${cur}/${max}${temp>0?` +${temp}`:""}`,192,347);}
  c.fillStyle="#4d3725";c.fillRect(48,364,288,27);if(Number.isFinite(si)&&Number.isFinite(siMax)&&siMax>0){const pct=Math.max(0,Math.min(1,(si+Math.max(0,siTemp))/siMax));c.fillStyle="#496a9c";c.fillRect(52,368,280*pct,19);}c.fillStyle="#f5e8c3";c.font="bold 18px Georgia";c.fillText(Number.isFinite(si)&&Number.isFinite(siMax)&&siMax>0?`SI ${si}/${siMax}${siTemp>0?` +${siTemp}`:""}`:"SI —",192,385);
  c.fillStyle="#4a2f1d";c.font="bold 18px Georgia";const status=conditions.length?conditions.slice(0,3).map(x=>String(x).replaceAll("_"," ").toUpperCase()).join(" • "):(active?"ACTING NOW":"READY");c.fillText(status.length>42?status.slice(0,39)+"…":status,192,438);return canvas;
}
function updateWallPosters(state,rows){
  const actors=posterActors(rows),sig=actors.map(r=>[r.occupant_id,r.occupant_name,r.occupant_type,r.is_active_actor,r.is_self_actor,firstValue(r,["occupant_current_hp","current_hp","hp_current"]),firstValue(r,["occupant_max_hp","max_hp","hp_max"]),firstValue(r,["occupant_sindre_cur","sindre_cur"]),firstValue(r,["occupant_sindre_max","sindre_max"]),posterConditions(r).join(",")].join("|")).join(";");if(sig===state.posterSignature)return;
  for(const child of [...state.posterRoot.children]){child.material?.map?.dispose();state.posterRoot.remove(child);disposeObject(child);}state.posterSignature=sig;if(!actors.length)return;
  const cardW=2.5,cardH=cardW*1.25,maxColumns=Math.max(1,Math.floor((state.roomWidth-7)/(cardW+1))),columns=Math.min(actors.length,maxColumns),lineCount=Math.ceil(actors.length/columns),desiredGap=lineCount===1?2:1,gap=Math.max(.25,Math.min(desiredGap,(state.roomWidth-7-columns*cardW)/Math.max(1,columns-1))),totalW=columns*cardW+(columns-1)*gap,startX=-totalW/2+cardW/2,baseY=state.roomFloorY+(lineCount===1?8.7:6.2);
  actors.forEach((row,i)=>{const tex=new THREE.CanvasTexture(posterCanvas(row));tex.colorSpace=THREE.SRGBColorSpace;const mat=new THREE.MeshBasicMaterial({map:tex,side:THREE.DoubleSide}),card=new THREE.Mesh(new THREE.PlaneGeometry(cardW,cardH),mat),col=i%columns,line=Math.floor(i/columns);card.position.set(startX+col*(cardW+gap),baseY+line*(cardH+.35),-state.roomDepth/2+.19);card.userData={actorId:String(row.occupant_id),actorType:row.occupant_type};state.posterRoot.add(card);const pin=new THREE.Mesh(new THREE.SphereGeometry(.055,7,5),new THREE.MeshBasicMaterial({color:0x4a3021}));pin.position.set(card.position.x,card.position.y+cardH*.43,card.position.z+.035);state.posterRoot.add(pin);});
}
function makeMiniature(row,active){const player=String(row.occupant_type)==="player",self=truthy(row.is_self_actor),colour=actorColour(row.occupant_id,player,self),group=new THREE.Group(),baseMat=new THREE.MeshLambertMaterial({color:active?0xe7b83f:0x342d2a}),bodyMat=new THREE.MeshLambertMaterial({color:colour}),headMat=new THREE.MeshLambertMaterial({color:player?0xc99472:colour.clone().multiplyScalar(.72)}),base=new THREE.Mesh(new THREE.CylinderGeometry(.27,.3,.085,12),baseMat),body=new THREE.Mesh(player?new THREE.ConeGeometry(.21,.42,8):new THREE.DodecahedronGeometry(.23,0),bodyMat),head=new THREE.Mesh(new THREE.SphereGeometry(.13,8,6),headMat);base.position.y=.045;body.position.y=player?.31:.34;head.position.y=player?.64:.65;group.add(base,body,head);if(active){const ring=new THREE.Mesh(new THREE.TorusGeometry(.33,.035,6,24),new THREE.MeshBasicMaterial({color:0xffdf62}));ring.rotation.x=Math.PI/2;ring.position.y=.04;group.add(ring);}return group;}
function updateTokens(state,rows){
  clearGroup(state.tokenRoot);
  for(const row of rows){if(!row.occupant_id)continue;const token=makeMiniature(row,truthy(row.is_active_actor));token.position.set(Number(row.x)-state.centerX,elevation(row),Number(row.y)-state.centerY);token.userData={actorId:String(row.occupant_id),actorType:row.occupant_type,x:Number(row.x),y:Number(row.y)};state.tokenRoot.add(token);
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
  updateTokens(state,rows);updateOverlays(state,rows,zones);updateWallPosters(state,rows);requestRender(state);
}
Shiny.addCustomMessageHandler("combat3d-lean-init",render);
Shiny.addCustomMessageHandler("combat3d-lean-resize",m=>{const s=states.get(m.containerId);if(s){s.renderer.setSize(Math.max(1,s.el.clientWidth),Math.max(1,s.el.clientHeight),false);requestRender(s);}});
function signalReady(attempt=0){
  if(window.Shiny?.setInputValue){Shiny.setInputValue("combat3d_lean_ready",{nonce:Date.now()+Math.random()},{priority:"event"});return;}
  if(attempt<40)setTimeout(()=>signalReady(attempt+1),100);
}
signalReady();
