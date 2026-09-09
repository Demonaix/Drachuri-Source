BEGIN;

CREATE TABLE chests (
  id BIGSERIAL PRIMARY KEY,
  session_id BIGINT NOT NULL REFERENCES game_sessions(id) ON DELETE CASCADE,
  name TEXT NOT NULL,
  difficulty TEXT NOT NULL DEFAULT 'standard' CHECK (difficulty IN ('easy','standard','hard','master')),
  sweet_spot NUMERIC NOT NULL CHECK (sweet_spot >= -90 AND sweet_spot <= 90),
  locked BOOLEAN NOT NULL DEFAULT TRUE,
  jammed BOOLEAN NOT NULL DEFAULT FALSE,
  status TEXT NOT NULL DEFAULT 'available' CHECK (status IN ('available','closed')),
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(), updated_at TIMESTAMPTZ NOT NULL DEFAULT now()
);
CREATE TABLE chest_items (
  id BIGSERIAL PRIMARY KEY,
  chest_id BIGINT NOT NULL REFERENCES chests(id) ON DELETE CASCADE,
  catalogue_id TEXT, item_json JSONB NOT NULL,
  quantity INTEGER NOT NULL DEFAULT 1 CHECK (quantity >= 0)
);
CREATE TABLE chest_invitations (
  chest_id BIGINT NOT NULL REFERENCES chests(id) ON DELETE CASCADE,
  character_id TEXT NOT NULL,
  status TEXT NOT NULL DEFAULT 'pending' CHECK (status IN ('pending','opened','dismissed')),
  pick_damage INTEGER NOT NULL DEFAULT 0,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(), updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  PRIMARY KEY(chest_id,character_id)
);
CREATE INDEX chests_session_idx ON chests(session_id,status,created_at DESC);
CREATE INDEX chest_invitation_character_idx ON chest_invitations(character_id,status,created_at DESC);
CREATE INDEX chest_items_chest_idx ON chest_items(chest_id,quantity);
COMMIT;
