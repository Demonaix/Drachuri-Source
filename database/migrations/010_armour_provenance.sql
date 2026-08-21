-- DATA-003: imported armour uses the same one-time provenance workflow as
-- imported weapons. Newly created equipment remains manually configurable.

UPDATE character_inventory_items
SET needs_provenance_roll = TRUE
WHERE armour_id IS NOT NULL
  AND (
    source = 'character_backfill' OR created_at <= COALESCE(
      (SELECT applied_at FROM schema_migrations WHERE version='009'), now()
    )
  )
  AND (material_id IS NULL OR condition_id IS NULL);
