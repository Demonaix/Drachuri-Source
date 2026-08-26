BEGIN;

ALTER TABLE party_skill_checks
  ADD COLUMN IF NOT EXISTS expected_responses INTEGER NOT NULL DEFAULT 0,
  ADD COLUMN IF NOT EXISTS support_cap INTEGER NOT NULL DEFAULT 2;

CREATE TABLE IF NOT EXISTS party_skill_check_contributions (
  id BIGSERIAL PRIMARY KEY,
  check_id BIGINT NOT NULL REFERENCES party_skill_checks(id) ON DELETE CASCADE,
  character_id TEXT NOT NULL,
  character_name TEXT NOT NULL DEFAULT 'Party member',
  response TEXT NOT NULL CHECK (response IN ('contributed', 'declined')),
  natural_roll INTEGER,
  modifier INTEGER NOT NULL DEFAULT 0,
  card_bonus INTEGER NOT NULL DEFAULT 0,
  total INTEGER,
  wild_roll INTEGER,
  cards_json JSONB NOT NULL DEFAULT '[]'::jsonb,
  effects_json JSONB NOT NULL DEFAULT '[]'::jsonb,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  UNIQUE(check_id, character_id)
);

CREATE INDEX IF NOT EXISTS party_skill_contributions_check_idx
  ON party_skill_check_contributions(check_id, created_at);

COMMIT;
