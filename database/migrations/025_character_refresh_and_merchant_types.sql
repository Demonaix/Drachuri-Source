BEGIN;

ALTER TABLE merchants DROP CONSTRAINT IF EXISTS merchants_specialty_check;
ALTER TABLE merchants ADD CONSTRAINT merchants_specialty_check CHECK(specialty IN (
  'general','provisions','food','weapons','armour','hunter','apothecary','arcane','outfitter','luxury','smith','materials'
));

DO $migration$
DECLARE character_id_type TEXT;
BEGIN
  SELECT format_type(a.atttypid,a.atttypmod) INTO character_id_type
  FROM pg_attribute a WHERE a.attrelid='character_blobs'::regclass AND a.attname='id';
  EXECUTE format($sql$
    CREATE TABLE IF NOT EXISTS character_refresh_notifications(
      id BIGSERIAL PRIMARY KEY,
      character_id %s NOT NULL REFERENCES character_blobs(id) ON DELETE CASCADE,
      update_kind TEXT NOT NULL DEFAULT 'inventory',
      message TEXT NOT NULL DEFAULT 'Your character was updated.',
      seen BOOLEAN NOT NULL DEFAULT FALSE,
      created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
      seen_at TIMESTAMPTZ
    )
  $sql$,character_id_type);
END $migration$;

CREATE INDEX IF NOT EXISTS character_refresh_unseen_idx
  ON character_refresh_notifications(character_id,created_at) WHERE seen=FALSE;

COMMIT;
