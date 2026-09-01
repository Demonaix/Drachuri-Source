BEGIN;

WITH knots(id,name,description) AS (VALUES
  ('glyph_knot_warding','Knot of Warding','A worked mnemonic knot that unlocks the knowledge needed to create wards while it is carried.'),
  ('glyph_knot_runes','Knot of Runes','A worked mnemonic knot that unlocks the knowledge needed to carve and release runes while it is carried.'),
  ('glyph_knot_enhancement','Knot of Enhancement','A worked mnemonic knot that unlocks the knowledge needed to enhance weapons while it is carried.')
)
INSERT INTO items(id,name,item_type,description,value,weight,category,updated_at)
SELECT id,name,'item',description,0,.1,'crafting',now() FROM knots
ON CONFLICT(id) DO UPDATE SET name=EXCLUDED.name,description=EXCLUDED.description,value=EXCLUDED.value,
  weight=EXCLUDED.weight,category=EXCLUDED.category,updated_at=now();

COMMIT;
