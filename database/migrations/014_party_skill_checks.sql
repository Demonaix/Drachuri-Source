BEGIN;

CREATE TABLE IF NOT EXISTS party_skill_checks (
  id BIGSERIAL PRIMARY KEY,
  session_id BIGINT NOT NULL REFERENCES game_sessions(id) ON DELETE CASCADE,
  skill TEXT NOT NULL,
  ability TEXT NOT NULL,
  context TEXT NOT NULL DEFAULT '',
  requester_character_id TEXT NOT NULL,
  requester_modifier INTEGER NOT NULL,
  helper_character_id TEXT,
  helper_modifier INTEGER,
  requester_roll INTEGER,
  helper_roll INTEGER,
  final_total INTEGER,
  scope TEXT NOT NULL CHECK(scope IN ('solo','party')),
  status TEXT NOT NULL DEFAULT 'pending' CHECK(status IN ('pending','resolved','cancelled')),
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  resolved_at TIMESTAMPTZ
);

CREATE INDEX IF NOT EXISTS party_skill_checks_pending_idx
  ON party_skill_checks(session_id,status,created_at);

COMMIT;
