BEGIN;

ALTER TABLE items ADD COLUMN IF NOT EXISTS ration_value INTEGER NOT NULL DEFAULT 0 CHECK(ration_value>=0);
ALTER TABLE items ADD COLUMN IF NOT EXISTS shelf_life_days INTEGER NOT NULL DEFAULT 0 CHECK(shelf_life_days>=0);

WITH food(id,name,description,value,weight,rations,shelf) AS (VALUES
('food_001','Red Apple','A crisp red orchard apple.',0.1,0.3,1,7),('food_002','Green Apple','A tart green apple.',0.1,0.3,1,7),
('food_003','Pear','A ripe sweet pear.',0.1,0.3,1,5),('food_004','Plum','A soft purple plum.',0.1,0.2,1,4),
('food_005','Peach','A fragrant summer peach.',0.2,0.3,1,3),('food_006','Apricot','A small golden apricot.',0.1,0.1,1,3),
('food_007','Cherries','A paper twist of cherries.',0.2,0.5,1,3),('food_008','Blackberries','A small basket of blackberries.',0.2,0.5,1,2),
('food_009','Bilberries','Dark berries gathered from the hills.',0.2,0.4,1,2),('food_010','Gooseberries','Sharp green berries in a leaf wrap.',0.2,0.5,1,3),
('food_011','Wild Strawberries','Tiny intensely sweet woodland berries.',0.3,0.4,1,2),('food_012','Dried Apple Rings','Chewy rings of dried apple.',0.5,0.5,2,30),
('food_013','Dried Pear Slices','Sun-dried slices of pear.',0.5,0.5,2,30),('food_014','Raisins','A pouch of dried grapes.',0.4,0.5,2,45),
('food_015','Hazelnuts','A pouch of shelled hazelnuts.',0.5,0.5,2,60),('food_016','Walnuts','A pouch of cracked walnuts.',0.6,0.5,2,60),
('food_017','Chestnuts','Roast-ready sweet chestnuts.',0.4,1,2,21),('food_018','Honeycomb','Wax comb heavy with wild honey.',1,1,2,90),
('food_019','Jar of Honey','A sealed clay jar of honey.',2,2,4,365),('food_020','Berry Preserve','A jar of thick berry preserve.',1,1,3,120),
('food_021','Oat Bread','A dense round oat loaf.',0.3,1,2,5),('food_022','Barley Bread','A rough but filling barley loaf.',0.2,1,2,5),
('food_023','Rye Bread','A dark sour rye loaf.',0.4,1,2,7),('food_024','White Trencher Loaf','A broad loaf suitable for trenchers.',0.5,1.5,3,4),
('food_025','Seeded Bannock','A flat oatcake filled with seeds.',0.4,0.8,2,10),('food_026','Hardtack Biscuits','Rock-hard travel biscuits.',0.8,1,4,180),
('food_027','Honey Cake','A small spiced cake sweetened with honey.',0.8,0.8,2,8),('food_028','Fruit Scone','A crumbly scone with dried fruit.',0.3,0.4,1,4),
('food_029','Oatcakes','A tied stack of dry oatcakes.',0.4,0.8,3,30),('food_030','Barley Crackers','Thin salted barley crackers.',0.4,0.6,2,45),
('food_031','Fresh Cow Cheese','A soft wheel of mild cheese.',1,2,4,5),('food_032','Aged Cheddar','A firm clothbound wedge.',2,2,5,60),
('food_033','Goat Cheese','A tangy little cheese round.',1,1,3,10),('food_034','Smoked Cheese','A firm smoke-cured cheese.',2,1.5,4,45),
('food_035','Buttermilk Flask','A stoppered flask of tart buttermilk.',0.3,2,2,2),('food_036','Fresh Milk Jug','A small sealed jug of milk.',0.3,2,2,1),
('food_037','Salted Butter','A wrapped crock of butter.',0.8,1,3,14),('food_038','Curds and Herbs','Fresh curds mixed with garden herbs.',0.5,1,2,2),
('food_039','Boiled Eggs','Six hard-boiled hen eggs.',0.5,1,3,5),('food_040','Pickled Eggs','Six eggs in a sealed vinegar jar.',1,2,3,60),
('food_041','Smoked Bacon','A thick smoke-cured bacon slab.',2,2,5,30),('food_042','Salt Pork','A heavily salted pork ration.',1.5,2,5,90),
('food_043','Beef Jerky','Tough strips of dried beef.',1.5,1,4,120),('food_044','Venison Jerky','Lean dried strips of venison.',2,1,4,120),
('food_045','Smoked Sausages','A string of smoked pork sausages.',2,2,5,45),('food_046','Blood Sausage','A dark seasoned sausage.',1,1,2,7),
('food_047','Roast Chicken','A whole herb-roasted chicken.',2,4,6,2),('food_048','Cold Mutton','Slices from a cooked mutton joint.',1.5,2,4,3),
('food_049','Rabbit Pie','A covered pie filled with rabbit and gravy.',1,2,4,4),('food_050','Game Pasty','A sealed pastry of chopped game meat.',0.6,1,2,5),
('food_051','Salted Herring','Two heavily salted herrings.',0.8,1,3,45),('food_052','Smoked Trout','A whole smoke-cured trout.',1,1,3,21),
('food_053','Dried Cod','A hard slab of wind-dried cod.',1,1,4,120),('food_054','Pickled Eels','A sealed jar of vinegar-pickled eel.',1.5,2,4,90),
('food_055','Fresh Salmon','A cleaned side of salmon.',2,3,5,1),('food_056','Crab Cakes','Four seasoned oat and crab cakes.',1,1,2,2),
('food_057','Mussel Pot','Cooked mussels sealed beneath butter.',1,2,3,4),('food_058','Seaweed Cakes','Pressed savoury cakes of dried seaweed.',0.7,1,3,90),
('food_059','Carrots','A tied bunch of carrots.',0.2,1,2,14),('food_060','Turnips','Three earthy turnips.',0.2,2,3,21),
('food_061','Onions','A net of pungent onions.',0.2,2,3,30),('food_062','Leeks','A tied bundle of fresh leeks.',0.3,2,3,7),
('food_063','Cabbage','A firm green cabbage.',0.2,3,4,14),('food_064','Parsnips','A bundle of sweet parsnips.',0.3,2,3,21),
('food_065','Mushrooms','A basket of edible woodland mushrooms.',0.5,1,2,3),('food_066','Dried Mushrooms','A pouch of dried woodland mushrooms.',0.8,0.5,3,120),
('food_067','Peas','A sack of dried peas.',0.5,2,5,180),('food_068','Lentils','A sack of red lentils.',0.5,2,5,240),
('food_069','Barley Grain','A small sack of pearl barley.',0.5,3,6,240),('food_070','Oats','A small sack of rolled oats.',0.5,3,6,180),
('food_071','Pot of Porridge','Warm oat porridge in a lidded pot.',0.3,2,3,1),('food_072','Barley Stew','A thick vegetable and barley stew.',0.5,3,5,2),
('food_073','Mutton Stew','A hearty pot of mutton and roots.',1,3,6,2),('food_074','Fish Chowder','Creamy fish and leek chowder.',1,3,5,1),
('food_075','Pease Pudding','Dense cooked peas with onion and herbs.',0.4,2,4,3),('food_076','Mushroom Broth','A flask of rich mushroom broth.',0.4,2,2,2),
('food_077','Bone Broth','A sealed flask of nourishing bone broth.',0.5,2,2,3),('food_078','Vegetable Soup','A lidded pot of root-vegetable soup.',0.4,3,4,2),
('food_079','Stuffed Cabbage','Cabbage leaves filled with barley and meat.',0.8,2,4,3),('food_080','Baked Turnips','Honey-glazed baked turnips.',0.4,2,3,3),
('food_081','Travel Ration Pack','Bread, cheese, dried meat and fruit.',1,2,5,30),('food_082','Light Scout Rations','Compact nuts, jerky and oatcakes.',2,1,4,45),
('food_083','Hearty Soldier Rations','A generous bundle of preserved staples.',2,3,7,45),('food_084','Hunter Ration Pack','Jerky, dried berries and hard cheese.',2,2,6,60),
('food_085','Sailor Ration Pack','Hardtack, salt fish and pickled onion.',1.5,3,6,90),('food_086','Pilgrim Ration Pack','Oatcakes, dried fruit and a little cheese.',1,2,4,30),
('food_087','Luxury Picnic Hamper','Fine bread, cheese, fruit and cold meats.',8,8,12,3),('food_088','Emergency Ration Brick','Compressed grain, fat and dried fruit.',2,1,5,180),
('food_089','Spiced Nuts','Roasted nuts coated in salt and spice.',0.8,0.5,2,60),('food_090','Candied Ginger','Sweet-hot preserved ginger pieces.',1,0.3,1,180),
('food_091','Marzipan Fruits','Almond sweets shaped like tiny fruit.',2,0.5,2,90),('food_092','Toffee Slab','A hard slab of butter toffee.',0.8,0.5,2,120),
('food_093','Dried Fig Bundle','A tied bundle of dried figs.',0.8,0.8,3,90),('food_094','Date and Nut Roll','Pressed dates and nuts wrapped in leaves.',1,0.8,3,60),
('food_095','Pickled Onions','A sealed jar of sharp pickled onions.',0.8,2,3,120),('food_096','Sauerkraut Crock','A small sealed crock of fermented cabbage.',1,3,5,180),
('food_097','Chutney Jar','A spiced fruit and onion preserve.',1,1,2,180),('food_098','Rendered Lard','A sealed crock of cooking fat.',0.7,2,5,120),
('food_099','Dried Herb Dumplings','Dry dumplings ready to simmer in broth.',0.8,1,4,90),('food_100','Feast Basket','Bread, roast meat, cheese, fruit and cakes.',10,12,18,2)
)
INSERT INTO items(id,name,item_type,description,value,weight,category,ration_value,shelf_life_days,updated_at)
SELECT id,name,'item',description,value,weight,'food',rations,shelf,now() FROM food
ON CONFLICT(id) DO UPDATE SET name=EXCLUDED.name,item_type=EXCLUDED.item_type,description=EXCLUDED.description,value=EXCLUDED.value,weight=EXCLUDED.weight,category=EXCLUDED.category,ration_value=EXCLUDED.ration_value,shelf_life_days=EXCLUDED.shelf_life_days,updated_at=now();

COMMIT;
