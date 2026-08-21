BEGIN;

CREATE TABLE IF NOT EXISTS camp_gather_requests (
  id BIGSERIAL PRIMARY KEY,
  session_id BIGINT NOT NULL REFERENCES game_sessions(id) ON DELETE CASCADE,
  day_number INTEGER NOT NULL CHECK(day_number > 0),
  resource TEXT NOT NULL CHECK(resource IN ('rations','water','wood')),
  requester_character_id TEXT NOT NULL,
  helper_character_id TEXT,
  requester_bonus INTEGER NOT NULL DEFAULT 0,
  helper_bonus INTEGER,
  requester_roll INTEGER,
  helper_roll INTEGER,
  result_total INTEGER,
  yield_amount INTEGER,
  status TEXT NOT NULL DEFAULT 'pending' CHECK(status IN ('pending','declined','resolved','cancelled')),
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  resolved_at TIMESTAMPTZ
);

CREATE TABLE IF NOT EXISTS camp_gather_actions (
  session_id BIGINT NOT NULL REFERENCES game_sessions(id) ON DELETE CASCADE,
  day_number INTEGER NOT NULL,
  character_id TEXT NOT NULL,
  resource TEXT NOT NULL CHECK(resource IN ('rations','water','wood')),
  request_id BIGINT NOT NULL REFERENCES camp_gather_requests(id) ON DELETE CASCADE,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  PRIMARY KEY(session_id,day_number,character_id)
);

CREATE UNIQUE INDEX IF NOT EXISTS camp_gather_one_pending_requester_idx
  ON camp_gather_requests(session_id,day_number,requester_character_id)
  WHERE status='pending';
CREATE INDEX IF NOT EXISTS camp_gather_pending_helper_idx
  ON camp_gather_requests(helper_character_id,status,created_at);

COMMIT;
