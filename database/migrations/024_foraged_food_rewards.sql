BEGIN;
ALTER TABLE camp_gather_requests ADD COLUMN IF NOT EXISTS reward_json JSONB NOT NULL DEFAULT '{}'::jsonb;
COMMIT;
