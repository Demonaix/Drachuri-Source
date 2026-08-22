BEGIN;

CREATE TABLE IF NOT EXISTS session_story_state (
  session_id BIGINT PRIMARY KEY REFERENCES game_sessions(id) ON DELETE CASCADE,
  storyboard_id TEXT NOT NULL,
  title TEXT NOT NULL DEFAULT 'Story',
  current_slide INTEGER NOT NULL DEFAULT 1 CHECK (current_slide >= 1),
  revision BIGINT NOT NULL DEFAULT 1,
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

COMMIT;
