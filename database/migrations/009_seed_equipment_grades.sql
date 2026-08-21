-- Fresh installs do not have the original quoted prototype tables. Seed the
-- canonical campaign definitions here; live rows are updated by name.

ALTER TABLE character_inventory_items
  ADD COLUMN IF NOT EXISTS needs_provenance_roll BOOLEAN NOT NULL DEFAULT FALSE;

UPDATE character_inventory_items
SET needs_provenance_roll = TRUE
WHERE weapon_id IS NOT NULL
  AND source = 'character_backfill'
  AND (material_id IS NULL OR condition_id IS NULL);

INSERT INTO item_materials (
  name,cost_modifier,weight_modifier,attack_bonus,damage_modifier,
  light_armour_modifier,medium_armour_modifier,heavy_armour_modifier,
  medium_armour_stealth,heavy_armour_stealth,heavy_armour_strength_req,
  is_iron,is_wood,drop_rate,excluded_enemy_types,required_characteristics,pool_restrict
) VALUES
  ('Copper',0.5,1.2,-1,0,0,-1,-1,'Disadvantage','Disadvantage',1,FALSE,FALSE,0.55,'{}','{}',NULL),
  ('Iron',0.8,1,0,-1,0,0,-1,NULL,NULL,0,TRUE,FALSE,0.15,ARRAY['Fae'],'{}',NULL),
  ('Steel',1,1,0,0,0,0,0,NULL,NULL,0,FALSE,FALSE,0.25,'{}','{}',NULL),
  ('Titanium Copper',5,0.5,1,6,1,1,1,'Removes disadvantage','',-2,FALSE,FALSE,0.05,'{}',ARRAY['Boss'],'Boss'),
  ('Wood',1,1,0,0,0,0,0,NULL,NULL,0,FALSE,TRUE,0,'{}','{}',NULL)
ON CONFLICT(name) DO UPDATE SET
  cost_modifier=EXCLUDED.cost_modifier,weight_modifier=EXCLUDED.weight_modifier,
  attack_bonus=EXCLUDED.attack_bonus,damage_modifier=EXCLUDED.damage_modifier,
  light_armour_modifier=EXCLUDED.light_armour_modifier,
  medium_armour_modifier=EXCLUDED.medium_armour_modifier,
  heavy_armour_modifier=EXCLUDED.heavy_armour_modifier,
  medium_armour_stealth=EXCLUDED.medium_armour_stealth,
  heavy_armour_stealth=EXCLUDED.heavy_armour_stealth,
  heavy_armour_strength_req=EXCLUDED.heavy_armour_strength_req,
  is_iron=EXCLUDED.is_iron,is_wood=EXCLUDED.is_wood,drop_rate=EXCLUDED.drop_rate,
  excluded_enemy_types=EXCLUDED.excluded_enemy_types,
  required_characteristics=EXCLUDED.required_characteristics,pool_restrict=EXCLUDED.pool_restrict,
  updated_at=now();

INSERT INTO item_conditions (
  name,cost_modifier,attack_bonus,damage_modifier,armour_modifier,drop_rate
) VALUES
  ('Very-Poorly-Crafted',0.1,-3,-3,-3,0.10),
  ('Poorly-Crafted',0.2,-2,-2,-2,0.25),
  ('Passably-Crafted',0.8,-1,-1,-1,0.20),
  ('Bog-Standard',1,0,0,0,0.30),
  ('Well-Crafted',1.2,1,1,1,0.10),
  ('Master-Crafted',1.4,2,2,2,0.05)
ON CONFLICT(name) DO UPDATE SET
  cost_modifier=EXCLUDED.cost_modifier,attack_bonus=EXCLUDED.attack_bonus,
  damage_modifier=EXCLUDED.damage_modifier,armour_modifier=EXCLUDED.armour_modifier,
  drop_rate=EXCLUDED.drop_rate,updated_at=now();
