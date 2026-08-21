BEGIN;

CREATE TABLE IF NOT EXISTS session_rest_cycles (
  id BIGSERIAL PRIMARY KEY,
  session_id BIGINT NOT NULL REFERENCES game_sessions(id) ON DELETE CASCADE,
  day_number INTEGER NOT NULL CHECK(day_number > 0),
  rest_type TEXT NOT NULL DEFAULT 'long_rest' CHECK(rest_type IN ('long_rest')),
  initiated_by TEXT,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  UNIQUE(session_id,day_number,rest_type)
);

CREATE TABLE IF NOT EXISTS session_rest_completions (
  rest_cycle_id BIGINT NOT NULL REFERENCES session_rest_cycles(id) ON DELETE CASCADE,
  character_id TEXT NOT NULL,
  completed_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  PRIMARY KEY(rest_cycle_id,character_id)
);

CREATE INDEX IF NOT EXISTS session_rest_cycles_session_idx
  ON session_rest_cycles(session_id,day_number DESC);

COMMIT;
