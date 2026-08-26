BEGIN;

CREATE TABLE IF NOT EXISTS session_environment_state (
  session_id BIGINT PRIMARY KEY REFERENCES game_sessions(id) ON DELETE CASCADE,
  day_number INTEGER NOT NULL DEFAULT 1 CHECK (day_number >= 1),
  time_of_day TEXT NOT NULL DEFAULT 'dawn'
    CHECK (time_of_day IN ('dawn', 'day', 'dusk', 'night')),
  geography TEXT NOT NULL DEFAULT 'Temperate wilderness',
  climate TEXT NOT NULL DEFAULT 'Temperate',
  weather TEXT NOT NULL DEFAULT 'Clear',
  revision BIGINT NOT NULL DEFAULT 1,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

COMMIT;
