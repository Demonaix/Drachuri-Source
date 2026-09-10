ALTER TABLE private_notes
  ADD COLUMN IF NOT EXISTS sender_notified_at TIMESTAMPTZ;

CREATE INDEX IF NOT EXISTS private_notes_sender_ack_idx
  ON private_notes(sender_character_id, status, sender_notified_at, acknowledged_at DESC);
