-- DATA-002 / DATA-003 / LOOT-001: equipment provenance and eligibility.

ALTER TABLE item_materials
  ADD COLUMN IF NOT EXISTS is_wood BOOLEAN NOT NULL DEFAULT FALSE,
  ADD COLUMN IF NOT EXISTS excluded_enemy_types TEXT[] NOT NULL DEFAULT '{}',
  ADD COLUMN IF NOT EXISTS required_characteristics TEXT[] NOT NULL DEFAULT '{}';

INSERT INTO item_materials (
  name, cost_modifier, weight_modifier, attack_bonus, damage_modifier,
  light_armour_modifier, medium_armour_modifier, heavy_armour_modifier,
  is_iron, is_wood, drop_rate
)
VALUES ('Wood', 1, 1, 0, 0, 0, 0, 0, FALSE, TRUE, 0)
ON CONFLICT (name) DO UPDATE SET
  cost_modifier = 1,
  weight_modifier = 1,
  attack_bonus = 0,
  damage_modifier = 0,
  light_armour_modifier = 0,
  medium_armour_modifier = 0,
  heavy_armour_modifier = 0,
  is_iron = FALSE,
  is_wood = TRUE;

UPDATE item_materials
SET excluded_enemy_types = ARRAY['Fae']::TEXT[],
    required_characteristics = '{}'::TEXT[],
    pool_restrict = NULL,
    updated_at = now()
WHERE lower(name) = 'iron';

UPDATE item_materials
SET excluded_enemy_types = '{}'::TEXT[],
    required_characteristics = ARRAY['Boss']::TEXT[],
    pool_restrict = 'Boss',
    updated_at = now()
WHERE lower(name) = 'titanium copper';

UPDATE weapons
SET default_material_id = (
  SELECT id FROM item_materials WHERE lower(name) = 'wood' LIMIT 1
), updated_at = now()
WHERE lower(name) ~ '(shortbow|longbow|crossbow|quarterstaff|wooden club)';

ALTER TABLE character_inventory_items
  ADD COLUMN IF NOT EXISTS material_assignment TEXT,
  ADD COLUMN IF NOT EXISTS condition_assignment TEXT;

DO $migration$
DECLARE character_id_type TEXT;
BEGIN
  SELECT format_type(a.atttypid, a.atttypmod) INTO character_id_type
  FROM pg_attribute a WHERE a.attrelid='character_blobs'::regclass AND a.attname='id';
  EXECUTE format($sql$
    CREATE TABLE IF NOT EXISTS equipment_assignment_log (
  id BIGSERIAL PRIMARY KEY,
  character_id %s NOT NULL REFERENCES character_blobs(id) ON DELETE CASCADE,
  instance_id TEXT NOT NULL,
  assignment_type TEXT NOT NULL CHECK (assignment_type IN ('material', 'build_quality')),
  definition_id BIGINT NOT NULL,
  roll_value DOUBLE PRECISION,
  eligible_weights JSONB NOT NULL DEFAULT '{}'::jsonb,
  assignment_source TEXT NOT NULL CHECK (
    assignment_source IN ('player_roll', 'forced_wood', 'loot_roll', 'control', 'migration')
  ),
  created_at TIMESTAMPTZ NOT NULL DEFAULT now()
    )
  $sql$, character_id_type);
END $migration$;

CREATE INDEX IF NOT EXISTS equipment_assignment_character_idx
  ON equipment_assignment_log(character_id, created_at DESC);

-- Definitions now support two validated damage components. Existing columns
-- remain available to the app during compatibility rollout.
ALTER TABLE weapons
  ADD CONSTRAINT weapons_damage_type_1_check CHECK (
    lower(damage_type_1) IN (
      'slashing','piercing','bludgeoning','fire','cold','lightning','acid',
      'poison','necrotic','radiant','psychic','force','thunder','iron','silver','other'
    )
  ) NOT VALID;

ALTER TABLE weapons
  ADD CONSTRAINT weapons_damage_type_2_check CHECK (
    damage_type_2 IS NULL OR lower(damage_type_2) IN (
      'slashing','piercing','bludgeoning','fire','cold','lightning','acid',
      'poison','necrotic','radiant','psychic','force','thunder','iron','silver','other'
    )
  ) NOT VALID;
