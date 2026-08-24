BEGIN;

-- A weapon is one inventory object. Its attack_modes describe the valid ways
-- that object can be used; "finesse" is resolved to the wielder's better STR
-- or DEX modifier by the shared combat code.
UPDATE weapons
SET attack_modes = jsonb_build_array(
  jsonb_build_object('id','one_handed','name','One-handed','damage',damage_1,'damage_type',damage_type_1,'stat','str','hands',1,'range_ft',5,'long_range_ft',5),
  jsonb_build_object('id','two_handed','name','Two-handed','damage',
    CASE WHEN damage_1 LIKE '%d4%' THEN replace(damage_1,'d4','d6')
         WHEN damage_1 LIKE '%d6%' THEN replace(damage_1,'d6','d8')
         WHEN damage_1 LIKE '%d8%' THEN replace(damage_1,'d8','d10')
         WHEN damage_1 LIKE '%d10%' THEN replace(damage_1,'d10','d12') ELSE damage_1 END,
    'damage_type',damage_type_1,'stat','str','hands',2,'range_ft',5,'long_range_ft',5),
  jsonb_build_object('id','thrown','name','Thrown (20/60 ft)','damage',damage_1,'damage_type',damage_type_1,'stat','str','hands',1,'range_ft',20,'long_range_ft',60)
), updated_at = now()
WHERE lower(name) ~ '(spear|trident)';

UPDATE weapons
SET attack_modes = jsonb_build_array(
  jsonb_build_object('id','one_handed','name','One-handed','damage',damage_1,'damage_type',damage_type_1,'stat','str','hands',1,'range_ft',5,'long_range_ft',5),
  jsonb_build_object('id','two_handed','name','Two-handed','damage',
    CASE WHEN damage_1 LIKE '%d4%' THEN replace(damage_1,'d4','d6')
         WHEN damage_1 LIKE '%d6%' THEN replace(damage_1,'d6','d8')
         WHEN damage_1 LIKE '%d8%' THEN replace(damage_1,'d8','d10')
         WHEN damage_1 LIKE '%d10%' THEN replace(damage_1,'d10','d12') ELSE damage_1 END,
    'damage_type',damage_type_1,'stat','str','hands',2,'range_ft',5,'long_range_ft',5)
), updated_at = now()
WHERE lower(name) ~ '(battleaxe|longsword|quarterstaff|warhammer|staff)';

UPDATE weapons
SET attack_modes = jsonb_build_array(
  jsonb_build_object('id','melee','name','Melee (finesse)','damage',damage_1,'damage_type',damage_type_1,'stat','finesse','hands',1,'range_ft',5,'long_range_ft',5),
  jsonb_build_object('id','thrown','name','Thrown (20/60 ft, finesse)','damage',damage_1,'damage_type',damage_type_1,'stat','finesse','hands',1,'range_ft',20,'long_range_ft',60)
), updated_at = now()
WHERE lower(name) LIKE '%dagger%';

UPDATE weapons
SET attack_modes = jsonb_build_array(
  jsonb_build_object('id','melee','name','Melee','damage',damage_1,'damage_type',damage_type_1,'stat','str','hands',1,'range_ft',5,'long_range_ft',5),
  jsonb_build_object('id','thrown','name','Thrown (20/60 ft)','damage',damage_1,'damage_type',damage_type_1,'stat','str','hands',1,'range_ft',20,'long_range_ft',60)
), updated_at = now()
WHERE lower(name) ~ '(handaxe|hand axe|light hammer)';

UPDATE weapons
SET attack_modes = jsonb_build_array(
  jsonb_build_object('id','melee','name','Melee','damage',damage_1,'damage_type',damage_type_1,'stat','str','hands',1,'range_ft',5,'long_range_ft',5),
  jsonb_build_object('id','thrown','name','Thrown (30/120 ft)','damage',damage_1,'damage_type',damage_type_1,'stat','str','hands',1,'range_ft',30,'long_range_ft',120)
), updated_at = now()
WHERE lower(name) LIKE '%javelin%';

UPDATE weapons
SET attack_modes = jsonb_build_array(
  jsonb_build_object('id','finesse','name','Finesse (STR or DEX)','damage',damage_1,'damage_type',damage_type_1,'stat','finesse','hands',1,'range_ft',5,'long_range_ft',5)
), updated_at = now()
WHERE lower(name) ~ '(rapier|shortsword|scimitar|whip|sabre)';

UPDATE weapons SET stat='dex', attack_modes=jsonb_build_array(
  jsonb_build_object('id','ranged','name','Ranged (150/600 ft)','damage',damage_1,'damage_type',damage_type_1,'stat','dex','hands',2,'range_ft',150,'long_range_ft',600)
), updated_at=now() WHERE lower(name) LIKE '%longbow%';

UPDATE weapons SET stat='dex', attack_modes=jsonb_build_array(
  jsonb_build_object('id','ranged','name','Ranged (80/320 ft)','damage',damage_1,'damage_type',damage_type_1,'stat','dex','hands',2,'range_ft',80,'long_range_ft',320)
), updated_at=now() WHERE lower(name) LIKE '%shortbow%';

-- Existing relational ownership receives the same modes immediately. Blob
-- compatibility also infers these modes by weapon family when loaded.
UPDATE character_inventory_items ci
SET properties = COALESCE(ci.properties, '{}'::jsonb) ||
  jsonb_build_object('attack_modes', w.attack_modes) ||
  CASE WHEN lower(w.name) ~ '(dagger|rapier|shortsword|scimitar|whip|sabre)'
       THEN '{"finesse":true}'::jsonb ELSE '{}'::jsonb END,
  updated_at = now()
FROM weapons w
WHERE ci.weapon_id=w.id AND jsonb_array_length(w.attack_modes)>0;

COMMIT;
