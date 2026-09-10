ALTER TABLE chests
  ADD COLUMN IF NOT EXISTS lock_kind TEXT NOT NULL DEFAULT 'chest';

UPDATE chests c
SET lock_kind = mo.object_type
FROM map_objects mo
WHERE mo.chest_id = c.id
  AND mo.object_type IN ('door', 'gate');

UPDATE chests
SET lock_kind = CASE
  WHEN lower(name) LIKE 'gate (%' THEN 'gate'
  WHEN lower(name) LIKE 'door (%' THEN 'door'
  ELSE lock_kind
END;

ALTER TABLE chests DROP CONSTRAINT IF EXISTS chests_lock_kind_check;
ALTER TABLE chests
  ADD CONSTRAINT chests_lock_kind_check CHECK (lock_kind IN ('chest', 'door', 'gate'));
