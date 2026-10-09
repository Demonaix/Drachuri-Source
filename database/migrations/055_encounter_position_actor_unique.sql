-- Movement upserts require one position row per encounter actor.  Some older
-- live databases were baselined without the original table-level unique key.
DELETE FROM encounter_positions older
USING encounter_positions newer
WHERE older.encounter_id = newer.encounter_id
  AND older.actor_type = newer.actor_type
  AND older.actor_id = newer.actor_id
  AND older.id < newer.id;

CREATE UNIQUE INDEX IF NOT EXISTS encounter_positions_actor_unique_idx
  ON encounter_positions(encounter_id, actor_type, actor_id);
