CREATE TABLE IF NOT EXISTS party_quests (
  id BIGSERIAL PRIMARY KEY,
  session_id BIGINT NOT NULL REFERENCES game_sessions(id) ON DELETE CASCADE,
  title TEXT NOT NULL CHECK (length(trim(title)) BETWEEN 1 AND 160),
  description TEXT NOT NULL DEFAULT '' CHECK (length(description) <= 8000),
  status TEXT NOT NULL DEFAULT 'active' CHECK (status IN ('active','completed','failed','hidden')),
  sort_order INTEGER NOT NULL DEFAULT 0,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE TABLE IF NOT EXISTS party_quest_objectives (
  id BIGSERIAL PRIMARY KEY,
  quest_id BIGINT NOT NULL REFERENCES party_quests(id) ON DELETE CASCADE,
  objective_text TEXT NOT NULL CHECK (length(trim(objective_text)) BETWEEN 1 AND 500),
  is_complete BOOLEAN NOT NULL DEFAULT FALSE,
  sort_order INTEGER NOT NULL DEFAULT 0,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX IF NOT EXISTS party_quests_session_idx ON party_quests(session_id,status,sort_order,id);
CREATE INDEX IF NOT EXISTS party_quest_objectives_quest_idx ON party_quest_objectives(quest_id,sort_order,id);
