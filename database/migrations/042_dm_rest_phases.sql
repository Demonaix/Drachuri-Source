BEGIN;

CREATE TABLE IF NOT EXISTS session_time_phases (
  id BIGSERIAL PRIMARY KEY,
  session_id BIGINT NOT NULL REFERENCES game_sessions(id) ON DELETE CASCADE,
  phase_kind TEXT NOT NULL CHECK (phase_kind IN ('standard','rest')),
  duration_hours NUMERIC(6,2) NOT NULL DEFAULT 6 CHECK (duration_hours > 0),
  status TEXT NOT NULL DEFAULT 'open' CHECK (status IN ('open','resolving','resolved','cancelled')),
  watches_open BOOLEAN NOT NULL DEFAULT FALSE,
  opened_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  resolved_at TIMESTAMPTZ
);
CREATE UNIQUE INDEX IF NOT EXISTS one_open_session_time_phase
  ON session_time_phases(session_id) WHERE status IN ('open','resolving');

CREATE TABLE IF NOT EXISTS session_phase_actions (
  id BIGSERIAL PRIMARY KEY,
  phase_id BIGINT NOT NULL REFERENCES session_time_phases(id) ON DELETE CASCADE,
  character_id TEXT NOT NULL,
  action_type TEXT NOT NULL,
  label TEXT NOT NULL,
  hours NUMERIC(6,2) NOT NULL DEFAULT 0 CHECK (hours >= 0),
  start_offset NUMERIC(6,2),
  end_offset NUMERIC(6,2),
  metadata JSONB NOT NULL DEFAULT '{}'::jsonb,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);
CREATE INDEX IF NOT EXISTS session_phase_actions_lookup
  ON session_phase_actions(phase_id,character_id,created_at);
CREATE UNIQUE INDEX IF NOT EXISTS one_watch_per_character_phase
  ON session_phase_actions(phase_id,character_id) WHERE action_type='watch';

COMMIT;
