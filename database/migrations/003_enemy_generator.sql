ALTER TABLE npc_templates
  ADD COLUMN IF NOT EXISTS enemy_type TEXT NOT NULL DEFAULT 'Custom',
  ADD COLUMN IF NOT EXISTS characteristics JSONB NOT NULL DEFAULT '[]'::jsonb,
  ADD COLUMN IF NOT EXISTS abilities JSONB NOT NULL DEFAULT '{"str":10,"dex":10,"con":10,"int":10,"cha":10,"bld_str":10}'::jsonb,
  ADD COLUMN IF NOT EXISTS attacks JSONB NOT NULL DEFAULT '[]'::jsonb,
  ADD COLUMN IF NOT EXISTS loot JSONB NOT NULL DEFAULT '[]'::jsonb,
  ADD COLUMN IF NOT EXISTS resistances TEXT[] NOT NULL DEFAULT '{}',
  ADD COLUMN IF NOT EXISTS immunities TEXT[] NOT NULL DEFAULT '{}',
  ADD COLUMN IF NOT EXISTS vulnerabilities TEXT[] NOT NULL DEFAULT '{}',
  ADD COLUMN IF NOT EXISTS condition_immunities TEXT[] NOT NULL DEFAULT '{}';

UPDATE npc_templates
SET enemy_type = CASE
      WHEN lower(name || ' ' || tags) LIKE '%bandit%' THEN 'Bandit'
      WHEN lower(name || ' ' || tags) LIKE '%animal%' OR lower(name || ' ' || tags) LIKE '%beast%' THEN 'Animal'
      WHEN lower(name || ' ' || tags) LIKE '%fae%' THEN 'Fae'
      ELSE COALESCE(NULLIF(enemy_type, ''), 'Custom')
    END,
    attacks = CASE WHEN attacks = '[]'::jsonb THEN jsonb_build_array(jsonb_build_object(
      'name', attack_name, 'hit', attack_bonus, 'dmg', damage_expr, 'type', damage_type, 'material', '')) ELSE attacks END,
    updated_at = now();

ALTER TABLE encounter_enemies
  ADD COLUMN IF NOT EXISTS attack_name TEXT NOT NULL DEFAULT 'Attack',
  ADD COLUMN IF NOT EXISTS attacks_json JSONB,
  ADD COLUMN IF NOT EXISTS enemy_type TEXT NOT NULL DEFAULT 'Custom',
  ADD COLUMN IF NOT EXISTS characteristics JSONB NOT NULL DEFAULT '[]'::jsonb,
  ADD COLUMN IF NOT EXISTS abilities JSONB NOT NULL DEFAULT '{"str":10,"dex":10,"con":10,"int":10,"cha":10,"bld_str":10}'::jsonb,
  ADD COLUMN IF NOT EXISTS attacks JSONB NOT NULL DEFAULT '[]'::jsonb,
  ADD COLUMN IF NOT EXISTS loot JSONB NOT NULL DEFAULT '[]'::jsonb,
  ADD COLUMN IF NOT EXISTS resistances TEXT[] NOT NULL DEFAULT '{}',
  ADD COLUMN IF NOT EXISTS immunities TEXT[] NOT NULL DEFAULT '{}',
  ADD COLUMN IF NOT EXISTS vulnerabilities TEXT[] NOT NULL DEFAULT '{}',
  ADD COLUMN IF NOT EXISTS condition_immunities TEXT[] NOT NULL DEFAULT '{}';

UPDATE encounter_enemies
SET attacks = CASE WHEN attacks = '[]'::jsonb THEN jsonb_build_array(jsonb_build_object(
  'name', 'Attack', 'hit', attack_bonus, 'dmg', damage_expr, 'type', damage_type, 'material', '')) ELSE attacks END;

CREATE INDEX IF NOT EXISTS npc_templates_enemy_type_idx ON npc_templates (lower(enemy_type));
CREATE INDEX IF NOT EXISTS npc_templates_characteristics_idx ON npc_templates USING gin (characteristics);
