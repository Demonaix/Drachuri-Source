BEGIN;

ALTER TABLE merchants ADD COLUMN IF NOT EXISTS pricing_style TEXT NOT NULL DEFAULT 'standard';
DO $$ BEGIN
  ALTER TABLE merchants ADD CONSTRAINT merchants_pricing_style_check
    CHECK (pricing_style IN ('cheap','standard','expensive','very_expensive'));
EXCEPTION WHEN duplicate_object THEN NULL;
END $$;

WITH scaled AS (
  SELECT id,
    CASE
      WHEN substring(id from '[0-9]+$')::integer <= 12 THEN '1d4'
      WHEN substring(id from '[0-9]+$')::integer <= 20 THEN '1d6'
      WHEN substring(id from '[0-9]+$')::integer <= 25 THEN '1d8'
      WHEN substring(id from '[0-9]+$')::integer <= 28 THEN '1d10'
      WHEN substring(id from '[0-9]+$')::integer = 29 THEN '1d12'
      ELSE '1d20'
    END AS bonus_die,
    CASE
      WHEN substring(id from '[0-9]+$')::integer <= 12 THEN 'common'
      WHEN substring(id from '[0-9]+$')::integer <= 20 THEN 'uncommon'
      WHEN substring(id from '[0-9]+$')::integer <= 25 THEN 'rare'
      WHEN substring(id from '[0-9]+$')::integer <= 28 THEN 'very_rare'
      ELSE 'legendary'
    END AS rarity,
    CASE
      WHEN substring(id from '[0-9]+$')::integer <= 12 THEN 100
      WHEN substring(id from '[0-9]+$')::integer <= 20 THEN 180
      WHEN substring(id from '[0-9]+$')::integer <= 25 THEN 325
      WHEN substring(id from '[0-9]+$')::integer <= 28 THEN 600
      WHEN substring(id from '[0-9]+$')::integer = 29 THEN 950
      ELSE 2000
    END AS new_value
  FROM weapons WHERE id LIKE 'magic_weapon\_%' ESCAPE '\'
)
UPDATE weapons w SET
  damage_2=s.bonus_die,
  value=s.new_value,
  description='An enchanted weapon dealing an additional '||s.bonus_die||' '||w.damage_type_2||' damage.',
  magical_properties=COALESCE(w.magical_properties,'{}'::jsonb)||jsonb_build_object(
    'is_magical',true,'category','magical_item','effect','extra_damage',
    'extra_damage_type',w.damage_type_2,'extra_damage_die',s.bonus_die,'rarity',s.rarity
  ),
  updated_at=now()
FROM scaled s WHERE w.id=s.id;

COMMIT;
