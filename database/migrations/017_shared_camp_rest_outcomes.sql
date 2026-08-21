BEGIN;

ALTER TABLE session_supplies
  ADD COLUMN IF NOT EXISTS has_fire BOOLEAN NOT NULL DEFAULT FALSE;

ALTER TABLE session_rest_completions
  ADD COLUMN IF NOT EXISTS rest_outcome TEXT NOT NULL DEFAULT 'full'
    CHECK (rest_outcome IN ('full', 'half', 'skip'));

COMMIT;
