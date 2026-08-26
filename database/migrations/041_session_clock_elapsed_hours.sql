BEGIN;

ALTER TABLE session_environment_state
  ADD COLUMN IF NOT EXISTS phase_elapsed_hours NUMERIC(6,2) NOT NULL DEFAULT 0
    CHECK (phase_elapsed_hours >= 0 AND phase_elapsed_hours < 6);

COMMIT;
