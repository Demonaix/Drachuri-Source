BEGIN;

ALTER TABLE merchants DROP CONSTRAINT IF EXISTS merchants_specialty_check;
ALTER TABLE merchants ADD CONSTRAINT merchants_specialty_check CHECK(specialty IN (
  'general','provisions','food','weapons','armour','hunter','apothecary','arcane',
  'outfitter','luxury','smith','materials','livestock'
));

COMMIT;
