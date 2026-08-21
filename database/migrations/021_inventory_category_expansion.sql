BEGIN;

ALTER TABLE items DROP CONSTRAINT IF EXISTS items_category_check;
ALTER TABLE items ADD CONSTRAINT items_category_check CHECK (
  category IN ('mundane_loot','food','magical_item','consumable','crafting','tool','treasure','quest')
);

COMMIT;
