-- LOOT-002: authoritative categories for non-weapon/non-armour inventory definitions.
ALTER TABLE items
  ADD COLUMN IF NOT EXISTS category TEXT NOT NULL DEFAULT 'mundane_loot';

UPDATE items SET category = CASE
  WHEN lower(item_type) IN ('consumable', 'potion', 'food', 'drink') THEN 'consumable'
  WHEN lower(item_type) IN ('tool', 'tools') THEN 'tool'
  WHEN lower(item_type) IN ('quest', 'quest_item') THEN 'quest'
  WHEN lower(item_type) IN ('treasure', 'valuable') THEN 'treasure'
  WHEN lower(item_type) IN ('crafting', 'material', 'ingredient') THEN 'crafting'
  ELSE 'mundane_loot'
END
WHERE category IS NULL OR category = 'mundane_loot';

ALTER TABLE items DROP CONSTRAINT IF EXISTS items_category_check;
ALTER TABLE items ADD CONSTRAINT items_category_check CHECK (
  category IN ('mundane_loot','consumable','crafting','tool','treasure','quest')
);

CREATE INDEX IF NOT EXISTS items_category_idx ON items(category);
