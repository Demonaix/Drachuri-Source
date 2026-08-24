BEGIN;

ALTER TABLE weapons ADD COLUMN IF NOT EXISTS attack_modes JSONB NOT NULL DEFAULT '[]'::jsonb;

UPDATE weapons
SET attack_modes='[{"id":"one_handed","name":"One-handed","damage":"1d6","damage_type":"Piercing","stat":"str","hands":1,"range_ft":5},{"id":"two_handed","name":"Two-handed","damage":"1d8","damage_type":"Piercing","stat":"str","hands":2,"range_ft":5},{"id":"thrown","name":"Thrown (20/60 ft)","damage":"1d6","damage_type":"Piercing","stat":"str","hands":1,"range_ft":20,"long_range_ft":60}]'::jsonb,
    stat='str',damage_1='1d6',damage_type_1='piercing',updated_at=now()
WHERE id='spear' OR lower(name) ~ '^spear[[:space:]]*(\(.*\))?$';

-- Early-campaign player discovery cannot produce end-game craftsmanship.
WITH steel AS (SELECT id FROM item_materials WHERE lower(name)='steel'),
     titanium AS (SELECT id FROM item_materials WHERE lower(name)='titanium copper')
UPDATE character_inventory_items ci
SET material_id=steel.id,material_assignment='migration',updated_at=now()
FROM steel,titanium
WHERE ci.material_id=titanium.id
  AND EXISTS(SELECT 1 FROM equipment_assignment_log l WHERE l.character_id=ci.character_id AND l.instance_id=ci.instance_id AND l.assignment_type='material' AND l.assignment_source='player_roll');

WITH well AS (SELECT id FROM item_conditions WHERE lower(name)='well-crafted'),
     master AS (SELECT id FROM item_conditions WHERE lower(name)='master-crafted')
UPDATE character_inventory_items ci
SET condition_id=well.id,condition_assignment='migration',updated_at=now()
FROM well,master
WHERE ci.condition_id=master.id
  AND EXISTS(SELECT 1 FROM equipment_assignment_log l WHERE l.character_id=ci.character_id AND l.instance_id=ci.instance_id AND l.assignment_type='build_quality' AND l.assignment_source='player_roll');

-- Collapse legacy records where one physical spear was modelled as separate
-- thrown/two-handed inventory items. Multiple ordinary Spears are untouched.
WITH candidates AS (
  SELECT ci.character_id,ci.instance_id,
    row_number() OVER(PARTITION BY ci.character_id ORDER BY
      CASE WHEN lower(trim(ci.custom_name))='spear' THEN 0 WHEN lower(ci.custom_name) LIKE '%thrown%' THEN 1 ELSE 2 END,
      ci.created_at,ci.instance_id) AS rn,
    count(*) OVER(PARTITION BY ci.character_id) AS n,
    bool_or(ci.custom_name LIKE '%(%') OVER(PARTITION BY ci.character_id) AS has_variant
  FROM character_inventory_items ci
  WHERE ci.weapon_id IS NOT NULL AND lower(trim(ci.custom_name)) ~ '^spear[[:space:]]*(\((thrown|two hands|two handed|two-handed)\))?$'
), canonical AS (SELECT * FROM candidates WHERE rn=1 AND n>1 AND has_variant)
UPDATE character_inventory_items ci
SET custom_name='Spear',weapon_id=COALESCE((SELECT id FROM weapons WHERE id='spear'),ci.weapon_id),quantity=1,
    properties=ci.properties||jsonb_build_object('attack_modes',(SELECT attack_modes FROM weapons WHERE id='spear'),'stat','str','damage1','1d6','dmg_type1','Piercing'),updated_at=now()
FROM canonical c WHERE ci.character_id=c.character_id AND ci.instance_id=c.instance_id;

WITH candidates AS (
  SELECT ci.character_id,ci.instance_id,
    row_number() OVER(PARTITION BY ci.character_id ORDER BY
      CASE WHEN lower(trim(ci.custom_name))='spear' THEN 0 WHEN lower(ci.custom_name) LIKE '%thrown%' THEN 1 ELSE 2 END,
      ci.created_at,ci.instance_id) AS rn,
    count(*) OVER(PARTITION BY ci.character_id) AS n,
    bool_or(ci.custom_name LIKE '%(%') OVER(PARTITION BY ci.character_id) AS has_variant
  FROM character_inventory_items ci
  WHERE ci.weapon_id IS NOT NULL AND lower(trim(ci.custom_name)) ~ '^spear[[:space:]]*(\((thrown|two hands|two handed|two-handed)\))?$'
)
DELETE FROM character_inventory_items ci USING candidates c
WHERE ci.character_id=c.character_id AND ci.instance_id=c.instance_id AND c.n>1 AND c.has_variant AND c.rn>1;

COMMIT;
