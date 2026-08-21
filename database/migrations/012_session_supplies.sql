BEGIN;

CREATE TABLE IF NOT EXISTS session_supplies (
  session_id BIGINT PRIMARY KEY REFERENCES game_sessions(id) ON DELETE CASCADE,
  wood INTEGER NOT NULL DEFAULT 3 CHECK(wood >= 0),
  wood_max INTEGER NOT NULL DEFAULT 10 CHECK(wood_max >= 0),
  water INTEGER NOT NULL DEFAULT 3 CHECK(water >= 0),
  water_max INTEGER NOT NULL DEFAULT 5 CHECK(water_max >= 0),
  rations INTEGER NOT NULL DEFAULT 3 CHECK(rations >= 0),
  rations_max INTEGER NOT NULL DEFAULT 5 CHECK(rations_max >= 0),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

COMMIT;
