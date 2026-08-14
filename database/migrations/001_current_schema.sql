BEGIN;

-- Canonical baseline schema for a brand-new Drachuri database.
-- Existing databases must use the migration runner's --baseline mode rather
-- than executing this migration over live tables.

CREATE TABLE character_blobs (
  id BIGSERIAL PRIMARY KEY,
  player_id TEXT NOT NULL DEFAULT 'shared',
  char_name TEXT NOT NULL,
  state_blob BYTEA NOT NULL,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE TABLE maps (
  id BIGSERIAL PRIMARY KEY,
  name TEXT NOT NULL,
  width INTEGER NOT NULL DEFAULT 10,
  height INTEGER NOT NULL DEFAULT 10,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE TABLE map_tiles (
  map_id BIGINT NOT NULL REFERENCES maps(id) ON DELETE CASCADE,
  x INTEGER NOT NULL,
  y INTEGER NOT NULL,
  terrain TEXT NOT NULL DEFAULT 'grass',
  fog INTEGER NOT NULL DEFAULT 0,
  light TEXT NOT NULL DEFAULT 'full',
  move_cost NUMERIC NOT NULL DEFAULT 1,
  blocks_movement BOOLEAN NOT NULL DEFAULT FALSE,
  blocks_vision BOOLEAN NOT NULL DEFAULT FALSE,
  PRIMARY KEY (map_id, x, y)
);

CREATE TABLE game_sessions (
  id BIGSERIAL PRIMARY KEY,
  name TEXT NOT NULL,
  mode TEXT NOT NULL DEFAULT 'exploration',
  status TEXT NOT NULL DEFAULT 'active',
  round_number INTEGER NOT NULL DEFAULT 1,
  current_turn_order INTEGER,
  active_actor_type TEXT,
  active_actor_id TEXT,
  active_map_id BIGINT REFERENCES maps(id),
  active_encounter_id BIGINT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE TABLE session_players (
  id BIGSERIAL PRIMARY KEY,
  session_id BIGINT NOT NULL REFERENCES game_sessions(id) ON DELETE CASCADE,
  character_id BIGINT NOT NULL REFERENCES character_blobs(id) ON DELETE CASCADE,
  display_name TEXT,
  initiative INTEGER,
  turn_order INTEGER,
  is_active BOOLEAN NOT NULL DEFAULT TRUE,
  current_hp INTEGER NOT NULL DEFAULT 12,
  max_hp INTEGER NOT NULL DEFAULT 12,
  temp_hp INTEGER NOT NULL DEFAULT 0,
  joined_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  UNIQUE (session_id, character_id)
);

CREATE TABLE encounters (
  id BIGSERIAL PRIMARY KEY,
  session_id BIGINT NOT NULL REFERENCES game_sessions(id) ON DELETE CASCADE,
  name TEXT NOT NULL,
  map_id BIGINT REFERENCES maps(id),
  status TEXT NOT NULL DEFAULT 'setup',
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

ALTER TABLE game_sessions
  ADD CONSTRAINT game_sessions_active_encounter_fk
  FOREIGN KEY (active_encounter_id) REFERENCES encounters(id) ON DELETE SET NULL;

CREATE TABLE npc_templates (
  npc_id TEXT PRIMARY KEY,
  name TEXT NOT NULL,
  hp_max INTEGER NOT NULL DEFAULT 10,
  ac INTEGER NOT NULL DEFAULT 12,
  movement_speed INTEGER NOT NULL DEFAULT 30,
  attack_name TEXT NOT NULL DEFAULT 'Attack',
  attack_bonus INTEGER NOT NULL DEFAULT 2,
  damage_expr TEXT NOT NULL DEFAULT '1d6',
  damage_type TEXT NOT NULL DEFAULT 'slashing',
  attacks_json TEXT NOT NULL DEFAULT '',
  tags TEXT NOT NULL DEFAULT '',
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE TABLE encounter_enemies (
  id BIGSERIAL PRIMARY KEY,
  encounter_id BIGINT REFERENCES encounters(id) ON DELETE CASCADE,
  session_id BIGINT REFERENCES game_sessions(id) ON DELETE CASCADE,
  enemy_uuid UUID NOT NULL DEFAULT gen_random_uuid() UNIQUE,
  name TEXT NOT NULL,
  template_key TEXT,
  hp_max INTEGER NOT NULL DEFAULT 10,
  hp_current INTEGER NOT NULL DEFAULT 10,
  temp_hp INTEGER NOT NULL DEFAULT 0,
  ac INTEGER NOT NULL DEFAULT 12,
  initiative INTEGER,
  turn_order INTEGER,
  is_active BOOLEAN NOT NULL DEFAULT TRUE,
  movement_speed INTEGER NOT NULL DEFAULT 30,
  attack_bonus INTEGER NOT NULL DEFAULT 2,
  damage_expr TEXT NOT NULL DEFAULT '1d6',
  damage_type TEXT NOT NULL DEFAULT 'slashing',
  notes TEXT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE TABLE encounter_positions (
  id BIGSERIAL PRIMARY KEY,
  encounter_id BIGINT NOT NULL REFERENCES encounters(id) ON DELETE CASCADE,
  actor_type TEXT NOT NULL,
  actor_id TEXT NOT NULL,
  x INTEGER NOT NULL,
  y INTEGER NOT NULL,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  UNIQUE (encounter_id, actor_type, actor_id)
);

CREATE TABLE combat_state (
  id BIGSERIAL PRIMARY KEY,
  session_id BIGINT REFERENCES game_sessions(id) ON DELETE CASCADE,
  encounter_id BIGINT NOT NULL REFERENCES encounters(id) ON DELETE CASCADE,
  round_number INTEGER NOT NULL DEFAULT 1,
  current_turn_order INTEGER,
  active_actor_type TEXT,
  active_actor_id TEXT,
  phase TEXT NOT NULL DEFAULT 'setup',
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE TABLE game_events (
  id BIGSERIAL PRIMARY KEY,
  session_id BIGINT REFERENCES game_sessions(id) ON DELETE CASCADE,
  encounter_id BIGINT REFERENCES encounters(id) ON DELETE CASCADE,
  event_type TEXT NOT NULL,
  actor_type TEXT,
  actor_id TEXT,
  target_id TEXT,
  payload JSONB NOT NULL DEFAULT '{}'::jsonb,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE TABLE player_positions (
  id BIGSERIAL PRIMARY KEY,
  session_id BIGINT NOT NULL REFERENCES game_sessions(id) ON DELETE CASCADE,
  character_id TEXT NOT NULL,
  x INTEGER NOT NULL,
  y INTEGER NOT NULL,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  UNIQUE (session_id, character_id)
);

CREATE TABLE enemy_positions (
  id BIGSERIAL PRIMARY KEY,
  session_id BIGINT NOT NULL REFERENCES game_sessions(id) ON DELETE CASCADE,
  enemy_uuid UUID NOT NULL,
  x INTEGER NOT NULL,
  y INTEGER NOT NULL,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  UNIQUE (session_id, enemy_uuid)
);

CREATE INDEX session_players_session_idx ON session_players(session_id);
CREATE INDEX encounter_positions_encounter_idx ON encounter_positions(encounter_id);
CREATE INDEX encounter_enemies_encounter_idx ON encounter_enemies(encounter_id);
CREATE INDEX game_events_encounter_idx ON game_events(encounter_id, created_at DESC);

COMMIT;
