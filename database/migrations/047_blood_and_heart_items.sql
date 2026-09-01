BEGIN;

ALTER TABLE items DROP CONSTRAINT IF EXISTS items_category_check;
ALTER TABLE items ADD CONSTRAINT items_category_check CHECK (
  category IN ('mundane_loot','food','magical_item','consumable','crafting','tool','treasure','quest','blood','heart')
);

WITH biological_items(id,name,description,value,weight,category) AS (VALUES
  ('stored_blood_pint','Stored Blood Pint','A sealed one-pint flask of blood. Its source and transferable Sindre value are recorded on the individual flask.',0,1.0,'blood'),
  ('preserved_heart','Preserved Heart','A preserved heart. Its source and Sindre value are recorded on the individual item.',0,1.0,'heart')
)
INSERT INTO items(id,name,item_type,description,value,weight,category,updated_at)
SELECT id,name,'item',description,value,weight,category,now() FROM biological_items
ON CONFLICT(id) DO UPDATE SET name=EXCLUDED.name,item_type=EXCLUDED.item_type,
 description=EXCLUDED.description,value=EXCLUDED.value,weight=EXCLUDED.weight,
 category=EXCLUDED.category,updated_at=now();

COMMIT;
