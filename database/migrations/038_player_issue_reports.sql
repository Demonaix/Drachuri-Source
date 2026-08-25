BEGIN;

CREATE TABLE IF NOT EXISTS player_issue_reports (
  id BIGSERIAL PRIMARY KEY,
  session_id BIGINT REFERENCES game_sessions(id) ON DELETE SET NULL,
  character_id TEXT,
  character_name TEXT NOT NULL DEFAULT 'Unknown Player',
  category TEXT NOT NULL CHECK (category IN ('error', 'feature_upgrade')),
  description TEXT NOT NULL CHECK (length(trim(description)) > 0),
  log_text TEXT NOT NULL DEFAULT '',
  log_bytes INTEGER NOT NULL DEFAULT 0,
  app_version TEXT,
  platform TEXT,
  status TEXT NOT NULL DEFAULT 'open'
    CHECK (status IN ('open', 'acknowledged', 'resolved')),
  dm_notes TEXT NOT NULL DEFAULT '',
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  acknowledged_at TIMESTAMPTZ,
  resolved_at TIMESTAMPTZ,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS player_issue_reports_status_idx
  ON player_issue_reports(status, created_at DESC);
CREATE INDEX IF NOT EXISTS player_issue_reports_session_idx
  ON player_issue_reports(session_id, created_at DESC);

COMMIT;
