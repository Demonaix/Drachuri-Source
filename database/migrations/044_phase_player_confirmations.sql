BEGIN;

CREATE TABLE IF NOT EXISTS session_phase_confirmations (
  phase_id BIGINT NOT NULL REFERENCES session_time_phases(id) ON DELETE CASCADE,
  character_id TEXT NOT NULL,
  confirmed_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  PRIMARY KEY (phase_id, character_id)
);

COMMIT;
