BEGIN;

-- Homebrew bows without "shortbow" or "longbow" in their name use the
-- shortbow range unless their catalogue entry is later given explicit modes.
UPDATE weapons SET stat='dex', attack_modes=jsonb_build_array(
  jsonb_build_object('id','ranged','name','Ranged (80/320 ft)','damage',damage_1,'damage_type',damage_type_1,'stat','dex','hands',2,'range_ft',80,'long_range_ft',320)
), updated_at=now()
WHERE lower(name) ~ '(^|[^a-z])bow([^a-z]|$)'
  AND lower(name) NOT LIKE '%shortbow%'
  AND lower(name) NOT LIKE '%longbow%'
  AND jsonb_array_length(attack_modes)=0;

UPDATE character_inventory_items ci
SET properties=COALESCE(ci.properties,'{}'::jsonb)||jsonb_build_object('attack_modes',w.attack_modes),updated_at=now()
FROM weapons w
WHERE ci.weapon_id=w.id AND lower(w.name) ~ '(^|[^a-z])bow([^a-z]|$)'
  AND jsonb_array_length(w.attack_modes)>0;

COMMIT;
