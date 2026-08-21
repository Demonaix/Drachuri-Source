BEGIN;

ALTER TABLE armour ADD COLUMN IF NOT EXISTS equipment_slot TEXT NOT NULL DEFAULT 'body';
ALTER TABLE armour ADD COLUMN IF NOT EXISTS ac_bonus INTEGER NOT NULL DEFAULT 0;

UPDATE armour
SET equipment_slot = CASE WHEN armour_type = 'Shield' THEN 'shield' ELSE 'body' END,
    ac_bonus = CASE WHEN armour_type = 'Shield' THEN 2 ELSE ac_bonus END
WHERE equipment_slot = 'body' OR armour_type = 'Shield';

DO $$ BEGIN
  ALTER TABLE armour ADD CONSTRAINT armour_equipment_slot_check
    CHECK (equipment_slot IN ('body','shield','head','accessory'));
EXCEPTION WHEN duplicate_object THEN NULL;
END $$;

WITH variants(id,name,description,value,weight,base_ac,armour_type,max_dex_bonus,equipment_slot,ac_bonus) AS (
  VALUES
  ('armour_variant_001','Fae-Style Leather Armour','Supple leather cut in layered leaf patterns and finished with bright fae stitching.',18,10,11,'Light',99,'body',0),
  ('armour_variant_002','Ranger''s Leafweave','Quiet leather panels dyed in broken woodland colours.',22,9,11,'Light',99,'body',0),
  ('armour_variant_003','Marshwalker Hide','Waxed hide armour that sheds bog water and marsh muck.',15,12,12,'Light',99,'body',0),
  ('armour_variant_004','Nightwatch Leather','Darkened leather with soft buckles made for sentries and scouts.',25,10,12,'Light',99,'body',0),
  ('armour_variant_005','Highland Studded Leather','Thick hill-cured leather reinforced with close-set metal studs.',30,13,12,'Light',99,'body',0),
  ('armour_variant_006','Foxrunner Jerkin','A light, flexible jerkin favoured by messengers and poachers.',14,8,11,'Light',99,'body',0),
  ('armour_variant_007','Drachuri Scout Leathers','Overlapping leather scales designed for long marches.',28,11,12,'Light',99,'body',0),
  ('armour_variant_008','Thornbound Leather','Leather reinforced with lacquered thornwood strips.',24,12,12,'Light',99,'body',0),
  ('armour_variant_009','River Scale Coat','Small metal scales sewn to an oiled coat for river patrols.',45,22,14,'Medium',2,'body',0),
  ('armour_variant_010','Boarhide Cuirass','A broad hide cuirass reinforced across the shoulders and ribs.',32,18,13,'Medium',2,'body',0),
  ('armour_variant_011','Warden''s Chain Shirt','A compact shirt of close-linked rings worn beneath a surcoat.',50,20,13,'Medium',2,'body',0),
  ('armour_variant_012','Bronze Scale Mail','Overlapping bronze plates that shine warmly when polished.',55,35,14,'Medium',2,'body',0),
  ('armour_variant_013','Hunter''s Half-Plate','Partial plate protecting the chest and leading limbs without full encumbrance.',80,32,15,'Medium',2,'body',0),
  ('armour_variant_014','Fae Court Breastplate','A fitted breastplate engraved with curling branches and moth wings.',90,20,14,'Medium',2,'body',0),
  ('armour_variant_015','Bone-Laced Hide','Heavy hide strengthened with cleaned ribs and horn splints.',26,16,12,'Medium',2,'body',0),
  ('armour_variant_016','Coastal Brigandine','Small plates riveted inside a salt-resistant canvas coat.',65,28,14,'Medium',2,'body',0),
  ('armour_variant_017','Blackened Ring Mail','Soot-dark rings over padded cloth, common among night guards.',45,40,14,'Heavy',0,'body',0),
  ('armour_variant_018','Drachuri Chain Mail','A full suit of sturdy local chain with reinforced shoulders.',75,55,16,'Heavy',0,'body',0),
  ('armour_variant_019','Antler Guard Splint','Vertical splints patterned after branching antlers.',110,60,17,'Heavy',0,'body',0),
  ('armour_variant_020','Hearthguard Plate','Broad plate armour with a warm copper-red ceremonial finish.',220,65,18,'Heavy',0,'body',0),
  ('armour_variant_021','Gravewatch Mail','Severe grey chain and plate used by tomb wardens.',95,55,16,'Heavy',0,'body',0),
  ('armour_variant_022','Old Kingdom Plate','An imposing, old-fashioned suit rebuilt from inherited pieces.',180,68,18,'Heavy',0,'body',0),
  ('armour_variant_023','Round Wooden Shield','A hide-faced round shield with a sturdy wooden boss.',10,6,2,'Shield',0,'shield',2),
  ('armour_variant_024','Iron-Rimmed Kite Shield','A long shield suited to guards and mounted warriors.',18,8,2,'Shield',0,'shield',2),
  ('armour_variant_025','Woven Wicker Shield','A light layered shield reinforced with hide.',8,4,2,'Shield',0,'shield',2),
  ('armour_variant_026','Bone-Faced Shield','A wooden shield faced with fitted plates of carved bone.',16,7,2,'Shield',0,'shield',2),
  ('armour_variant_027','Fae Buckler','A graceful leaf-shaped buckler that favours mobility over coverage.',20,3,1,'Shield',0,'shield',1),
  ('armour_variant_028','Drachuri Tower Shield','A tall, weighty shield painted with a household device.',35,15,2,'Shield',0,'shield',2),
  ('armour_variant_029','Antlered Helm','A leather-and-iron helm crowned with shortened antler tines.',20,5,0,'Light',0,'head',1),
  ('armour_variant_030','Iron Nasal Helm','A practical open helm with a strong nose guard.',12,4,0,'Light',0,'head',1),
  ('armour_variant_031','Boar-Tusk Helm','A broad cheeked helm decorated with polished boar tusks.',18,5,0,'Light',0,'head',1),
  ('armour_variant_032','Raven Guard Helm','A dark helm with a swept beak-like brow.',22,5,0,'Light',0,'head',1),
  ('armour_variant_033','Fae Leaf Circlet','A protective band of layered bronze leaves; light enough for court wear.',25,2,0,'Light',0,'head',1),
  ('armour_variant_034','Horned Raider Helm','A battered raider''s helm with compact horn plates.',15,5,0,'Light',0,'head',1),
  ('armour_variant_035','Hooded Chain Coif','A close-linked coif covering the head, neck and shoulders.',16,6,0,'Light',0,'head',1),
  ('armour_variant_036','Stag Warden Helm','An ornate iron helm with a restrained branching crest.',30,6,0,'Light',0,'head',1)
)
INSERT INTO armour(id,name,description,value,weight,base_ac,armour_type,max_dex_bonus,proficient,equipment_slot,ac_bonus,updated_at)
SELECT id,name,description,value,weight,base_ac,armour_type,max_dex_bonus,true,equipment_slot,ac_bonus,now()
FROM variants
ON CONFLICT(id) DO UPDATE SET name=EXCLUDED.name,description=EXCLUDED.description,value=EXCLUDED.value,
weight=EXCLUDED.weight,base_ac=EXCLUDED.base_ac,armour_type=EXCLUDED.armour_type,
max_dex_bonus=EXCLUDED.max_dex_bonus,equipment_slot=EXCLUDED.equipment_slot,ac_bonus=EXCLUDED.ac_bonus,updated_at=now();

COMMIT;
