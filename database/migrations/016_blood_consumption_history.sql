-- BLOOD-002: append-only daily blood and heart consumption history.
BEGIN;

CREATE TABLE IF NOT EXISTS blood_consumption_events (
  id BIGSERIAL PRIMARY KEY,
  character_id TEXT NOT NULL,
  session_id BIGINT REFERENCES game_sessions(id) ON DELETE SET NULL,
  campaign_day INTEGER NOT NULL CHECK (campaign_day >= 1),
  consumption_type TEXT NOT NULL CHECK (consumption_type IN ('blood','heart')),
  source TEXT NOT NULL DEFAULT 'Unknown source',
  quantity NUMERIC NOT NULL CHECK (quantity > 0),
  sindre_value NUMERIC NOT NULL DEFAULT 0 CHECK (sindre_value >= 0),
  created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS blood_consumption_character_day_idx
  ON blood_consumption_events(character_id,campaign_day,created_at DESC);

COMMIT;
