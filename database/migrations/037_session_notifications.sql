BEGIN;

CREATE TABLE IF NOT EXISTS session_notifications (
  id BIGSERIAL PRIMARY KEY,
  session_id BIGINT NOT NULL REFERENCES game_sessions(id) ON DELETE CASCADE,
  source_character_id TEXT,
  source_name TEXT NOT NULL DEFAULT 'Player',
  message TEXT NOT NULL,
  notification_type TEXT NOT NULL DEFAULT 'message',
  dedupe_key TEXT NOT NULL UNIQUE,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS session_notifications_session_idx
  ON session_notifications(session_id,id);

COMMIT;
