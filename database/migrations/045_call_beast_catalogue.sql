BEGIN;

ALTER TABLE animals
  ADD COLUMN IF NOT EXISTS summonable BOOLEAN NOT NULL DEFAULT FALSE,
  ADD COLUMN IF NOT EXISTS challenge_rating NUMERIC(5,2),
  ADD COLUMN IF NOT EXISTS attack_name TEXT,
  ADD COLUMN IF NOT EXISTS attack_bonus INTEGER,
  ADD COLUMN IF NOT EXISTS damage_expr TEXT,
  ADD COLUMN IF NOT EXISTS damage_type TEXT,
  ADD COLUMN IF NOT EXISTS portrait_asset TEXT;

ALTER TABLE encounter_summons
  ADD COLUMN IF NOT EXISTS animal_id TEXT REFERENCES animals(id) ON UPDATE CASCADE,
  ADD COLUMN IF NOT EXISTS attack_name TEXT NOT NULL DEFAULT 'Natural Attack',
  ADD COLUMN IF NOT EXISTS attack_bonus INTEGER NOT NULL DEFAULT 2,
  ADD COLUMN IF NOT EXISTS damage_expr TEXT NOT NULL DEFAULT '1d6',
  ADD COLUMN IF NOT EXISTS damage_type TEXT NOT NULL DEFAULT 'slashing',
  ADD COLUMN IF NOT EXISTS portrait_asset TEXT NOT NULL DEFAULT 'summoned-beast.png';

INSERT INTO animals(id,name,species,description,speed,armour_class,max_hp,gold_value,mountable,
  summonable,challenge_rating,attack_name,attack_bonus,damage_expr,damage_type,portrait_asset,updated_at)
VALUES
 ('summon_wolf','Wolf','Wolf','A swift pack hunter that harasses exposed enemies.',40,13,11,0,false,true,0.25,'Bite',4,'2d4+2','piercing','summoned-beast.png',now()),
 ('summon_boar','Boar','Boar','A stubborn woodland charger with dangerous tusks.',40,11,11,0,false,true,0.25,'Tusk',3,'1d6+1','slashing','summoned-beast.png',now()),
 ('summon_giant_badger','Giant Badger','Badger','A broad-clawed burrower with a furious temperament.',30,10,13,0,false,true,0.25,'Claws',3,'2d4+1','slashing','summoned-beast.png',now()),
 ('summon_black_bear','Black Bear','Bear','A powerful forest beast that fights with heavy claws.',40,11,19,0,false,true,0.50,'Claws',4,'2d4+2','slashing','summoned-beast.png',now()),
 ('summon_giant_goat','Giant Goat','Goat','A sure-footed upland beast that drives foes back with its horns.',40,11,19,0,false,true,0.50,'Ram',4,'2d4+2','bludgeoning','summoned-beast.png',now()),
 ('summon_dire_wolf','Dire Wolf','Wolf','A massive pack hunter able to bring down armed prey.',50,14,37,0,false,true,1.00,'Bite',5,'2d6+3','piercing','summoned-beast.png',now()),
 ('summon_great_stag','Great Stag','Stag','A proud forest stag with a crown of dangerous antlers.',50,13,30,0,false,true,1.00,'Antlers',5,'2d6+3','piercing','summoned-beast.png',now()),
 ('summon_giant_boar','Giant Boar','Boar','A huge tusked beast that crashes through the battlefield.',40,12,42,0,false,true,2.00,'Tusk',5,'2d6+3','slashing','summoned-beast.png',now()),
 ('summon_cave_bear','Cave Bear','Bear','An ancient, scarred bear from the deep woods and mountain caves.',40,13,52,0,false,true,2.00,'Claws',7,'2d8+5','slashing','summoned-beast.png',now()),
 ('summon_elder_hart','Elder Hart','Fae Stag','A vast fae-touched hart whose antlers carry the force of the old forest.',50,15,68,0,false,true,4.00,'Crowned Charge',8,'3d8+5','piercing','summoned-beast.png',now())
ON CONFLICT(id) DO UPDATE SET
 name=EXCLUDED.name,species=EXCLUDED.species,description=EXCLUDED.description,speed=EXCLUDED.speed,
 armour_class=EXCLUDED.armour_class,max_hp=EXCLUDED.max_hp,summonable=EXCLUDED.summonable,
 challenge_rating=EXCLUDED.challenge_rating,attack_name=EXCLUDED.attack_name,attack_bonus=EXCLUDED.attack_bonus,
 damage_expr=EXCLUDED.damage_expr,damage_type=EXCLUDED.damage_type,portrait_asset=EXCLUDED.portrait_asset,updated_at=now();

CREATE INDEX IF NOT EXISTS animals_summonable_cr_idx ON animals(summonable,challenge_rating);

COMMIT;
