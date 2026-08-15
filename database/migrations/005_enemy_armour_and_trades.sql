ALTER TABLE npc_templates ADD COLUMN IF NOT EXISTS armor_id TEXT NOT NULL DEFAULT 'unarmoured';
ALTER TABLE encounter_enemies ADD COLUMN IF NOT EXISTS armor_id TEXT NOT NULL DEFAULT 'unarmoured';

CREATE TABLE IF NOT EXISTS trade_offers (
  id BIGSERIAL PRIMARY KEY,
  session_id BIGINT NOT NULL REFERENCES game_sessions(id) ON DELETE CASCADE,
  sender_character_id TEXT NOT NULL,
  recipient_character_id TEXT NOT NULL,
  offer_kind TEXT NOT NULL CHECK (offer_kind IN ('item','gold')),
  item_blob BYTEA,
  gold_amount INTEGER NOT NULL DEFAULT 0 CHECK (gold_amount >= 0),
  summary JSONB NOT NULL DEFAULT '{}'::jsonb,
  status TEXT NOT NULL DEFAULT 'pending' CHECK (status IN ('pending','accepted','declined','cancelled')),
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  resolved_at TIMESTAMPTZ
);
ALTER TABLE trade_offers ADD COLUMN IF NOT EXISTS sender_seen BOOLEAN NOT NULL DEFAULT FALSE;
CREATE INDEX IF NOT EXISTS trade_offers_recipient_pending_idx ON trade_offers(recipient_character_id,status,created_at);
CREATE INDEX IF NOT EXISTS trade_offers_sender_pending_idx ON trade_offers(sender_character_id,status,created_at);
