BEGIN;

ALTER TABLE camp_gather_actions
  ADD COLUMN IF NOT EXISTS id BIGSERIAL;

ALTER TABLE camp_gather_actions
  DROP CONSTRAINT IF EXISTS camp_gather_actions_pkey;

DO $$
BEGIN
  IF NOT EXISTS (
    SELECT 1
    FROM pg_constraint
    WHERE conrelid = 'camp_gather_actions'::regclass
      AND contype = 'p'
  ) THEN
    ALTER TABLE camp_gather_actions
      ADD CONSTRAINT camp_gather_actions_pkey PRIMARY KEY (id);
  END IF;
END $$;

DROP INDEX IF EXISTS camp_gather_one_pending_requester_idx;

CREATE INDEX IF NOT EXISTS camp_gather_requester_status_idx
  ON camp_gather_requests(session_id, requester_character_id, status, created_at DESC);

COMMIT;
