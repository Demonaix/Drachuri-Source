ALTER TABLE encounter_enemies
  ADD COLUMN IF NOT EXISTS creature_type TEXT NOT NULL DEFAULT '',
  ADD COLUMN IF NOT EXISTS str_save INTEGER NOT NULL DEFAULT 0,
  ADD COLUMN IF NOT EXISTS dex_save INTEGER NOT NULL DEFAULT 0,
  ADD COLUMN IF NOT EXISTS con_save INTEGER NOT NULL DEFAULT 0,
  ADD COLUMN IF NOT EXISTS int_save INTEGER NOT NULL DEFAULT 0,
  ADD COLUMN IF NOT EXISTS bld_str_save INTEGER NOT NULL DEFAULT 0,
  ADD COLUMN IF NOT EXISTS cha_save INTEGER NOT NULL DEFAULT 0;

CREATE TABLE encounter_effects (
  id BIGSERIAL PRIMARY KEY,
  encounter_id BIGINT NOT NULL REFERENCES encounters(id) ON DELETE CASCADE,
  source_actor_type TEXT NOT NULL,
  source_actor_id TEXT NOT NULL,
  spell_id TEXT NOT NULL,
  effect_type TEXT NOT NULL,
  target_actor_type TEXT,
  target_actor_id TEXT,
  center_x INTEGER,
  center_y INTEGER,
  radius_ft INTEGER,
  starts_round INTEGER NOT NULL DEFAULT 1,
  ends_round INTEGER,
  concentration BOOLEAN NOT NULL DEFAULT FALSE,
  payload JSONB NOT NULL DEFAULT '{}'::jsonb,
  is_active BOOLEAN NOT NULL DEFAULT TRUE,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE TABLE encounter_summons (
  id BIGSERIAL PRIMARY KEY,
  summon_uuid UUID NOT NULL DEFAULT gen_random_uuid() UNIQUE,
  encounter_id BIGINT NOT NULL REFERENCES encounters(id) ON DELETE CASCADE,
  owner_actor_id TEXT NOT NULL,
  name TEXT NOT NULL,
  creature_type TEXT NOT NULL DEFAULT 'beast',
  max_cr TEXT NOT NULL DEFAULT '1/2',
  hp_max INTEGER NOT NULL DEFAULT 10,
  hp_current INTEGER NOT NULL DEFAULT 10,
  temp_hp INTEGER NOT NULL DEFAULT 0,
  ac INTEGER NOT NULL DEFAULT 12,
  movement_speed INTEGER NOT NULL DEFAULT 30,
  initiative INTEGER,
  turn_order INTEGER,
  is_active BOOLEAN NOT NULL DEFAULT TRUE,
  expires_round INTEGER,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX encounter_effects_active_idx
  ON encounter_effects(encounter_id, is_active, ends_round);
CREATE INDEX encounter_effects_source_idx
  ON encounter_effects(encounter_id, source_actor_id, concentration);
CREATE INDEX encounter_summons_active_idx
  ON encounter_summons(encounter_id, is_active);
