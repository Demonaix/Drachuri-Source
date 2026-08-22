BEGIN;

CREATE TABLE IF NOT EXISTS glyph_zone_triggers (
  glyph_id BIGINT NOT NULL REFERENCES character_glyphs(id) ON DELETE CASCADE,
  encounter_id BIGINT NOT NULL REFERENCES encounters(id) ON DELETE CASCADE,
  actor_type TEXT NOT NULL,
  actor_id TEXT NOT NULL,
  round_number INTEGER NOT NULL,
  trigger_type TEXT NOT NULL DEFAULT 'entry',
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  PRIMARY KEY (glyph_id, encounter_id, actor_type, actor_id, round_number, trigger_type)
);

CREATE INDEX IF NOT EXISTS glyph_zone_triggers_encounter_idx
  ON glyph_zone_triggers(encounter_id, round_number);

COMMIT;
