BEGIN;
CREATE TABLE map_objects (
  id BIGSERIAL PRIMARY KEY,
  map_id BIGINT NOT NULL REFERENCES maps(id) ON DELETE CASCADE,
  x INTEGER NOT NULL,
  y INTEGER NOT NULL,
  object_type TEXT NOT NULL CHECK (object_type IN ('door','gate','chest')),
  chest_id BIGINT REFERENCES chests(id) ON DELETE CASCADE,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  UNIQUE(map_id,x,y)
);
CREATE INDEX map_objects_map_idx ON map_objects(map_id,x,y);
COMMIT;
