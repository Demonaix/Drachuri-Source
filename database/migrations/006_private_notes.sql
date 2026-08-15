CREATE TABLE IF NOT EXISTS private_notes (
  id BIGSERIAL PRIMARY KEY,
  session_id BIGINT NOT NULL REFERENCES game_sessions(id) ON DELETE CASCADE,
  sender_character_id TEXT NOT NULL,
  recipient_character_id TEXT NOT NULL,
  body TEXT NOT NULL CHECK (length(body) BETWEEN 1 AND 4000),
  reply_to_id BIGINT REFERENCES private_notes(id) ON DELETE SET NULL,
  status TEXT NOT NULL DEFAULT 'sent' CHECK (status IN ('sent','read','acknowledged')),
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  read_at TIMESTAMPTZ,
  acknowledged_at TIMESTAMPTZ
);
CREATE INDEX IF NOT EXISTS private_notes_recipient_idx ON private_notes(recipient_character_id,status,created_at DESC);
CREATE INDEX IF NOT EXISTS private_notes_sender_idx ON private_notes(sender_character_id,created_at DESC);

