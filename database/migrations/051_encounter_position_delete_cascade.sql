DO $$
DECLARE
  constraint_name text;
BEGIN
  SELECT c.conname
    INTO constraint_name
  FROM pg_constraint c
  WHERE c.contype = 'f'
    AND c.conrelid = 'encounter_positions'::regclass
    AND c.confrelid = 'encounters'::regclass
  LIMIT 1;

  IF constraint_name IS NOT NULL THEN
    EXECUTE format('ALTER TABLE encounter_positions DROP CONSTRAINT %I', constraint_name);
  END IF;

  ALTER TABLE encounter_positions
    ADD CONSTRAINT encounter_positions_encounter_id_fkey
    FOREIGN KEY (encounter_id) REFERENCES encounters(id) ON DELETE CASCADE;
END $$;
