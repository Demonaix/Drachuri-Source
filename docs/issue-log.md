# Drachuri issue and testing log

This is the authoritative issue register for the player and control apps. The original `Issue:error log.rtf` is retained as the raw testing notebook.

## How to use this log

- Give every issue a permanent ID; never reuse an ID.
- Use one status: `Open`, `Investigating`, `In progress`, `Fixed — awaiting retest`, `Verified`, `Deferred`, or `Won't fix`.
- A code change is not complete until its checkpoint is recorded and the issue has been retested.
- Add new observations beneath **Test notes** rather than replacing the original report.
- Move an issue to **Verified** only after it works in the real app.

## Active issues

### CTRL-001 — Control dropdown selections flash or revert

- **Status:** Fixed — awaiting real control-app retest
- **Priority:** High
- **Area:** Control UI / reactive refresh
- **Reported:** 2026-08-17
- **Original report:** Difficulty selecting weapons and other values because dropdowns flash, revert, or lose their options.
- **Work already attempted:** Reactive selection-preservation changes in checkpoints `1e536cc` and `5ab776e`.
- **Test notes:** User reports the issue is now fixed as of 2026-08-21. Retain the exact-tab/value reproduction note if it returns.
- **Fix/checkpoint:** —
- **Retest:** Required in the real control app.

### CHAR-001 — Starting above level 1 leaves HP at 10

- **Status:** Fixed — awaiting retest
- **Priority:** High
- **Area:** Character creation / levelling / HP
- **Reported:** 2026-08-17
- **Original report:** A character created at a level other than 1 remains at 10 HP and cannot be changed correctly.
- **Cause:** Landing creation changed the saved class and level but retained `new_character()`'s generic 10/10 HP. The load-time repair path was also an unfinished placeholder that assigned every eligible character 12 HP regardless of class, level or Constitution.
- **Fix (2026-08-21):** Added one authoritative starting-HP calculation: maximum class hit die plus Constitution modifier at level 1, then the fixed average hit-die gain plus Constitution modifier for every later level, with a minimum gain of one per level. Landing creation writes both current and maximum HP before the first save. The legacy repair path uses the same rule and only replaces the old 10/10 default for characters above level 1, so a legitimate level-one 10 HP character is not silently changed.
- **Automated test:** Covers level-one Rogue, level-five Rogue, level-five Barbarian with Constitution 14, and a low-Constitution minimum-gain case.
- **Fix/checkpoint:** `13896e1`
- **Retest:** Create a level-five Rogue and confirm 28/28 HP at the creation default Constitution 10. Create a level-five Barbarian and confirm 40/40 HP. Reload each character and confirm HP remains unchanged.

### BLOOD-001 — Hearts do not grant expected temporary Sindre

- **Status:** Fixed — awaiting retest
- **Priority:** Medium
- **Area:** Blood / hearts / resources
- **Reported:** 2026-08-17
- **Original report:** Eating hearts appears not to add temporary Sindre. It adds only +2 temporary HP, which may also be incorrect.
- **Cause:** Player and Control had diverged heart rules. Player treated the Heart Eater's “overheal” as 25% of maximum HP (only +2 on a 10-HP character) and granted no temporary Sindre. Control instead applied ordinary Sindre overflow to everyone. The class description did not say which resource was intended to overheal.
- **Rule fixed (2026-08-21):** A normal consumer adds the heart's listed Sindre to their current reserve and retains only the amount above their normal maximum as temporary Sindre. A Heart Eater first restores their normal Sindre completely, then gains the heart's entire listed value as temporary Sindre. Hearts do not grant temporary HP. Existing temporary Sindre is preserved and the new gain is added to it.
- **Automated test:** Covers a normal character consuming a 25-Sindre heart at 90/100 with three existing temporary Sindre (ends 100 normal and 18 temporary), and a Heart Eater consuming the same heart at 10/100 (ends 100 normal and 28 temporary). Both blood modules and their descriptions parse; all 56 rule tests pass.
- **Fix/checkpoint:** `d0b565e`
- **Retest:** With a Heart Eater below maximum Sindre, consume one heart and confirm normal Sindre fills completely, temporary Sindre increases by the heart's displayed value, temporary HP does not change, and the heart count falls by one. Repeat with a non-Heart-Eater near maximum and confirm only the actual overflow becomes temporary Sindre.

### CHAR-002 — Magical nature and skill identity lack variety

- **Status:** Fixed — awaiting retest
- **Priority:** Medium
- **Area:** Character generation
- **Reported:** 2026-08-17
- **Original report:** Magical nature and skill identity repeatedly produce the same archetypes, especially Starved Abyss Channeler and Cunning Stalker Operative.
- **Cause:** These titles are derived identities rather than random generation, but two fallbacks erased much of the character variation. Magical Nature read the obsolete single `build$subclass` field instead of the current class entries, making most characters generic Channelers; its capacity label then overwrote its flow label. Skill Identity recognised only seven of the 29 available skills, so characters led by History, Medicine, Insight, Endurance and most other skills became Wanderer Operatives.
- **Fix (2026-08-21):** Added one shared identity engine used by Player and Control. Magical Nature reads all current and legacy subclass locations and retains capacity, flow, subclass and magical-state dimensions in the title. Skill Identity now has distinct core and supporting titles for every skill in the homebrew skill list. Titles remain grounded in saved character mechanics rather than being randomly varied, so identical builds can still legitimately share an identity.
- **Automated test:** Confirms a multiclass Heart Eater is detected from `build$classes`, distinguishes an `Abyss Storm Devourer`, produces `Chronicler Physician` for History/Medicine, and verifies all 29 skills avoid the generic Wanderer fallback. Both module copies parse and all 57 rule tests pass.
- **Fix/checkpoint:** `9868484`
- **Retest:** Compare Magical Nature on characters with different Sindre capacity/flow and subclasses; confirm the title changes on each relevant dimension. Compare Skill Identity on characters led by less-common skills such as History, Medicine, Endurance, Precision and Mandred Connection; confirm their titles are distinct and their displayed top skills remain accurate.

### REST-001 — Unlit rest-fire image is missing

- **Status:** Fixed — awaiting retest
- **Priority:** Low
- **Area:** Rest UI / assets
- **Reported:** 2026-08-17
- **Original report:** The rest fire has a missing image when the fire is not lit.
- **Cause:** Both Rest modules referenced `embers.png`, but that asset was absent from both app packages.
- **Fix (2026-08-21):** The unlit state now reuses the shipped `fire.png` with a dark grayscale/low-opacity treatment, while the lit state retains the full image. Both states include accessible alternative text. This avoids adding another duplicated asset that could drift between Player and Control.
- **Automated test:** Confirms both app packages contain the referenced fire image, neither Rest module requests the missing `embers.png`, and the unlit state is labelled. Both modules parse and all 58 rule tests pass.
- **Fix/checkpoint:** `3cd3a0f`
- **Retest:** Open Rest with no fire and confirm a dark, unlit campfire appears without a broken-image icon. Spend one wood to light it and confirm it changes to the full-colour fire image.

### PARTY-001 — Wood, water, and rations are not party-synchronised

- **Status:** Fixed — awaiting real multi-player retest
- **Priority:** High
- **Area:** Party resources / database synchronisation
- **Reported:** 2026-08-17
- **Original report:** Wood, water, and rations need to be shared consistently across the player party.
- **Decision:** While connected to a session, wood, water and rations belong to that session's shared camp. Personal survival flags such as ate/drank/foraged today remain character-owned. Offline play retains local supplies.
- **Fix (2026-08-21):** Migration 012 adds one bounded supply ledger per session. Every rest-screen supply action now uses a row lock and atomic update, preventing two clients from spending the same final ration, water or wood. Gathering, forage, additions, removals, consumption, fire-lighting and water refill all update the shared ledger. Player clients refresh the same totals every 2.5 seconds and mirror them into their character state for compatibility and offline continuity. The first connected character seeds a new session ledger from existing saved values; subsequent character blobs cannot overwrite it.
- **Automated test:** Fresh migration 001–012 and the full integration suite pass. Integration coverage confirms shared visibility, atomic decrement, zero-floor protection and refill-to-session-maximum. Migration 012 is applied to live Supabase.
- **Fix/checkpoint:** `35d1852`
- **Retest:** Open Rest on two player clients in the same session. Add or consume each resource on one client and confirm the other updates within 2.5 seconds. With one unit remaining, click consume nearly simultaneously and confirm only one succeeds. Confirm eating/drinking changes only the acting character's daily survival status. Launch offline and confirm local supplies still work.

### PARTY-002 — Camp gathering ignores character skill and has no assistance workflow

- **Status:** Implemented — awaiting real two-player retest
- **Priority:** High
- **Area:** Rest / party supplies / skills
- **Reported:** 2026-08-21
- **Original report:** Foraging should use the acting player's values, allow another player to assist through an accept/decline prompt, benefit from two capable participants, and restrict every character to one daily choice among food, water or firewood gathering.
- **Rules implemented:** Food foraging, water gathering and firewood gathering use Survival with Blood Strength, including saved proficiency or Expertise. A solo attempt rolls once. On an accepted assistance request both characters roll and the better total determines a bounded yield: below 10 yields 0, then 1/2/3/4 units at DC 10/15/20/25. This models assistance as advantage while allowing the more capable helper's modifier to matter.
- **Fix (2026-08-21):** Migration 013 adds durable assistance requests and a unique session/day/character gathering-action ledger. The acting player chooses solo or an active party helper in a modal. The helper receives an accept/decline prompt; accepting atomically reserves both daily actions, resolves both checks and updates shared supplies. Declining consumes neither action, allowing a solo retry or another helper. Requests and results survive client refresh, and the requester receives the final total/yield notification. Offline gathering uses the same skill/yield rules with a local daily marker.
- **Automated test:** 55 rule tests pass, including all yield thresholds. The 13-migration integration suite covers pending request delivery, assisted resolution, supply yield bounds, duplicate-action rejection, decline-without-consumption and retry. Migration 013 is applied to live Supabase.
- **Fix/checkpoint:** `8ba3561`
- **Retest:** On player one, choose each gathering type and inspect the solo/assistant modal. Ask player two to help; decline once and confirm both can still act, then retry and accept. Confirm the better of both displayed Survival totals determines 0–4 shared supplies. After resolution, verify neither participant can forage, gather water or gather wood again until the next shared day, while an uninvolved third player still can.

### SKILL-001 — Independent player rolls duplicate one party skill check

- **Status:** Implemented — awaiting real multi-player retest
- **Priority:** High
- **Area:** Skills / party collaboration
- **Reported:** 2026-08-21
- **Original report:** A question such as knowledge of a city should not prompt three unrelated History rolls. One player should lead the check alone or ask the party for assistance, combining relevant character skill.
- **Rules implemented:** The normal skill-roll button creates one named check with optional context. Solo uses the leader's d20 plus the skill's mapped ability and saved proficiency/Expertise. “Ask party” broadcasts one request; the first other player to accept rolls with their own modifier, and the better of the two modified totals becomes the single party result. This follows the bounded advantage model used for assisted gathering and prevents accepted helpers from generating extra outcomes. Explicit advantage/disadvantage buttons remain personal rolls for circumstances imposed by the DM.
- **Fix (2026-08-21):** Migration 014 adds durable pending and resolved party checks. Assistance prompts survive refresh, resolution uses a row lock so only one helper can complete a request, and every connected player receives the same leader/helper breakdown, context and final total in their log. The modifier helper uses the canonical skill-to-ability table plus character proficiency or Expertise, so History uses Intelligence, Medicine uses Blood Strength, and so on.
- **Automated test:** The 14-migration integration suite verifies request visibility, assisted resolution, better-total selection, second-helper rejection, shared result retrieval, and solo resolution. All 55 rule tests pass. Migration 014 is applied to live Supabase.
- **Fix/checkpoint:** `bba9e12`
- **Retest:** From player one, click the normal History roll, enter a city question and ask the party. Accept on player two and confirm every client logs exactly one result with both modified rolls and the better total. Try accepting on player three afterward and confirm no second result appears. Repeat alone with Medicine and confirm the correct Blood Strength/proficiency modifier is used.

### MAGIC-001 — Natural magic spell rules need an intent audit

- **Status:** Implemented — awaiting combat retest
- **Priority:** Medium
- **Area:** Magic / rules audit
- **Reported:** 2026-08-17
- **Original report:** Natural magic spells need checking against their originally intended behaviour.
- **Intent audit (2026-08-21):** Compared the original `deep_magic` source descriptions with the canonical spell definitions, unlock choices, scaling and executable Player combat workflow. The four branches remain distinct and consistent: Plants creates difficult terrain and can restrain on a Strength save; Rain weakens fire and strengthens cold/lightning; Animals summons a friendly scaling beast; Disease poisons enemies on a Constitution save. All cost 20 Sindre, use an action and concentration, use Blood Strength for saves, and apply the previously established level 11/15 improvements. Mandred convergence correctly halves cost and worsens affected enemy saves.
- **Protection added:** Added a complete four-specialty contract test covering specialty selection, cost/action/concentration, Vine terrain and restraint, Rain modifiers, Beast CR scaling, and Disease save/condition. This preserves the accepted homebrew rules during future combat refactors.
- **Automated test:** All 59 rule tests pass.
- **Fix/checkpoint:** `86bd80c`
- **Retest:** With a Hanianol character for each saved specialty, cast its unlocked spell in combat and confirm the displayed description, 20-Sindre cost, action spend, area/summon creation and combat effect. At Hanianol levels 11 and 15, confirm the displayed/created radius and specialty upgrade match the canonical definition. Repeat one cast on Mandred convergence and confirm the cost is 10.

### BALANCE-001 — Early homebrew combat outliers

- **Status:** Fixed — awaiting combat retest
- **Priority:** High
- **Area:** Class balance / combat / magic
- **Reported:** 2026-08-21
- **Original report:** The class/archetype review identified boss-scaling Water Channeler damage, a consequence-free enemy-only Wasting Sickness aura, and excessive bonus-action damage from Flesh Witherer's Hand. Heart economy, Ancestor power, Na'Haran durability and later-level abilities are intentionally unchanged for now.
- **Fix (2026-08-21):** Water Channeler now deals `1d8` necrotic damage for 10 Sindre and its level-11 Improved Water Channeler deals `2d8`, replacing 10%/20% maximum-HP damage. Wasting Sickness remains a 60-foot emanation centred on its caster but now forces saves from every other creature in range, including allied players and friendly summons; the caster is immune to their own emanation. Player saving throws include Constitution and save proficiency, while enemies use their Constitution score. Flesh Witherer's Hand now deals `2d8`, scaling to `3d8` at level 11 and `4d8` at level 17. The ordinary action-based Flesh Witherer remains `3d8`, scaling to `4d8`/`5d8`.
- **Automated test:** Confirms dice-based Improved Water Channeler, indiscriminate self-origin Wasting Sickness, and the complete Flesh Witherer's Hand damage progression. All affected files parse and all 59 rule tests pass.
- **Fix/checkpoint:** `74d5a8b`
- **Retest:** Use Water Channeler below and above Na'Haran level 11 and confirm `1d8` then `2d8`, with no dependency on target maximum HP. Cast Wasting Sickness beside an enemy, another player and a friendly summon; confirm all three save while the caster does not. Use Flesh Witherer's Hand at levels 6, 11 and 17 and confirm `2d8`, `3d8` and `4d8` respectively.

### MAP-001 — Ravine tile missing from Control map builder

- **Status:** Fixed — awaiting map-builder retest
- **Priority:** Low
- **Area:** Control map builder
- **Reported:** 2026-08-17
- **Original report:** Ravine is unavailable as a map tile option.
- **Rule:** Ravine is an impassable ground gap: it blocks movement but not vision. Falling/jumping into a ravine is not automatic; a future jump/fall action can explicitly override the movement block rather than allowing accidental clicks to drop a creature.
- **Fix (2026-08-21):** Added Ravine to Control's terrain brush. Selecting it automatically enables movement blocking, disables vision blocking and uses the existing dark ravine treatment in Player 2D/3D maps plus a matching Control-builder colour.
- **Automated test:** Confirms the brush, blocking defaults and colour are present. All 63 rule tests pass and the three-app startup smoke test passes.
- **Fix/checkpoint:** `a288ceb`
- **Retest:** Paint and save a Ravine strip. Confirm it appears dark in Control and Player maps, cannot be entered by click movement, and does not prevent viewing creatures on the opposite side.

### COMBAT-001 — Bloodlust Risk may not be implemented

- **Status:** Fixed — awaiting combat retest
- **Priority:** Medium
- **Area:** Class tree / combat
- **Reported:** 2026-08-17
- **Original report:** Bloodlust Risk exists in the tree but may not be enacted in combat.
- **Rule implemented:** Bloodlust is the starvation state already set by the daily addiction system at stages 3–4. At stage 3, a natural 1 on an applied attack forces an immediate Bloodthirsty Bite against the nearest adjacent living creature. At stage 4, the first action of every turn is overridden by that bite. Distance resolves first; ties prefer an enemy and then an ally, avoiding arbitrary selection while retaining risk. If nobody is adjacent, the surge is logged but cannot bite. A successful forced bite counts as half a pint.
- **Fix (2026-08-21):** Added automatic target selection, attack/critical/damage resolution, HP application and a structured combat-log event. The stage-4 trigger is guarded by turn identity so refreshes cannot repeat it.
- **Automated test:** Covers stage-3 natural-1, non-triggering ordinary rolls, stage-4 turn override and inactive Bloodlust. All 63 rule tests pass.
- **Fix/checkpoint:** `a288ceb`
- **Retest:** Use a blood-starved stage-3 character and force/observe a natural 1 with adjacent enemy and ally; confirm one nearest creature is bitten and the event is logged. At stage 4, start two separate turns and confirm exactly one forced bite/action spend per turn. Confirm refresh does not repeat it and no-adjacent-creature turns log without damage.

### BLOOD-002 — Blood consumption needs a daily history

- **Status:** Implemented — awaiting Blood-screen retest
- **Priority:** Low
- **Area:** Blood inventory / history
- **Reported:** 2026-08-17
- **Original report:** Add a record of what blood was consumed each day.
- **Fix (2026-08-21):** Migration 016 adds append-only `blood_consumption_events`, recording character, optional session, campaign day, blood/heart type, source, quantity, Sindre value and timestamp. Successful blood drinks and heart consumption append an event; the Blood screen shows the 30 most recent entries. Historical rows are not stored in or rewritten with the character blob.
- **Automated test:** Fresh migration 001–016 and integration coverage confirm blood and heart events persist and can be retrieved together by day. Migration 016 is live on Supabase.
- **Fix/checkpoint:** `a288ceb`
- **Retest:** Drink part of two differently named blood stores and consume a heart. Confirm three history entries show the correct day, source, quantities and Sindre values after navigating away, reconnecting, and restarting the app.

### REST-002 — Long rests can become unsynchronised across the party

- **Status:** Fixed — awaiting real multi-player retest
- **Priority:** High
- **Area:** Rest / party synchronisation
- **Reported:** 2026-08-17
- **Original report:** A party long rest should be coordinated so different players do not advance independently.
- **Cause:** Long Rest advanced only the local character blob. There was no session record identifying the campaign day being rested into, nor any idempotency guard against the same character completing it twice.
- **Fix (2026-08-21):** Migration 011 adds a session-level long-rest cycle and per-character completion ledger. The first active character opens the next campaign day; all other active party members join that same cycle. Repeat clicks by a completed character do nothing, and a later day cannot open until every active session character has completed the current rest. Character state is saved before its completion marker. Offline play retains the existing local rest behaviour. Character IDs are stored format-neutrally so both the UUID-based live database and numeric isolated database are supported.
- **Automated test:** A ten-character integration test confirms shared cycle/day identity, repeat-click idempotency, incomplete-party locking, and next-cycle release after all active characters complete. Fresh migration 001–011 and the full integration suite pass. Migration 011 is applied to live Supabase.
- **Fix/checkpoint:** `0af71a1`
- **Retest:** With two active player clients in one session, click Long Rest on the first and note `1/2`; click it again and confirm no second day is created. Complete Long Rest on the second and confirm `2/2` with both characters on the same day. Start the following rest and confirm both join the next shared day. Characters intentionally absent for a rest must be marked inactive in the session or they will correctly keep the next day locked.

### BLOOD-003 — Tylwyth Teg blood reminder is not appearing

- **Status:** Fixed — awaiting rest retest
- **Priority:** Medium
- **Area:** Rest / blood-drinker rules
- **Reported:** 2026-08-17
- **Original report:** Tylwyth Teg blood drinkers should receive a reminder when starting a long rest, but the prompt does not appear.
- **Cause:** The reminder guarded Skip Day but the Long Rest observer bypassed it and advanced the blood day directly.
- **Fix (2026-08-21):** Both Player and Control now run the same Tylwyth Teg intake check before Long Rest. A deficient character receives the Blood Required modal and may cancel or explicitly choose Rest Anyway; the existing Skip Day confirmation remains separate.
- **Automated test:** Both Rest modules parse, all 63 rule tests pass, and one Control/two Player apps start successfully.
- **Fix/checkpoint:** `8d379c4`
- **Retest:** With a Tylwyth Teg character below required daily intake, click Long Rest and confirm it does not proceed until Rest Anyway is chosen. Cancel once, then consume enough blood and confirm Long Rest proceeds without the warning.

### REST-003 — Starvation/rest calculation can reduce HP to 0/0 incorrectly

- **Status:** Fixed — awaiting retest
- **Priority:** Critical
- **Area:** Rest / survival / HP
- **Reported:** 2026-08-17
- **Original report:** While starving at exhaustion level 4, drinking the required blood, eating, drinking, and lighting the fire still resulted in HP becoming 0/0.
- **Cause:** At exhaustion level 4, `validate_character()` permanently halved the stored maximum HP on every read. Repeated Shiny validation could therefore cascade `10 → 5 → 2 → 1 → 0`. Long-rest day processing could also read stale character state while applying survival damage.
- **Fix:** Exhaustion now derives an effective maximum without mutating canonical HP. Long rest publishes prepared state before day processing, advances the day, then heals to the resulting effective maximum. Invalid saved maximum HP now stops healing rather than writing zero.
- **Fix/checkpoint:** `21eb8cc`
- **Automated test:** Added repeated-read regression coverage; all 50 tests pass.
- **Retest:** In the real player app, use a character with a known non-zero maximum at exhaustion 4. Open several modules, long rest after meeting food/water/blood needs, and confirm the stored maximum never shrinks across refresh/relaunch. A character already corrupted to maximum 0 requires separate repair because the original value is no longer recoverable from that field.

### MAGIC-002 — Sorcerer magic branches need corrected availability

- **Status:** Fixed — awaiting Magic-screen retest
- **Priority:** Medium
- **Area:** Magic / class progression
- **Reported:** 2026-08-17
- **Original report:** Exothermic should link to unlocked fire magic; endothermic to cold; mechanical should be available to all sorcerers; natural should be restricted to Hanianol.
- **Cause:** Magic-school pills were arbitrary saved toggles, and a fallback granted Thermal and Mechanical labels when no data existed. They were not derived from class progression.
- **Fix (2026-08-21):** Magic schools are now read-only derived state in both apps. Every Hanianol or Na'Haran Sorcerer receives Mechanical; Hanianol receives Natural from level 2; an Exothermic level choice grants Fire and an Endothermic choice grants Cold. Non-sorcerers receive none. Legacy toggled labels are ignored, so no destructive backfill is needed; actual executable spells continue to use authoritative progression.
- **Automated test:** Covers Hanianol Exothermic, Na'Haran Endothermic and non-sorcerer characters. All 60 then-current rule tests passed; the completed suite now passes 63.
- **Fix/checkpoint:** `8d379c4`
- **Retest:** Open Magic with Hanianol Exothermic, Hanianol Endothermic, Na'Haran Exothermic, Na'Haran Endothermic and a non-sorcerer. Confirm the highlighted schools match progression and cannot be manually toggled.

### DATA-001 — Inventory definitions are still stored primarily inside blobs

- **Status:** In progress — live dual-write compatibility active
- **Priority:** Critical
- **Area:** Database / inventory foundation
- **Reported:** 2026-08-20
- **Original report:** Weapons and items need authoritative relational tables with explicit variables rather than relying on blobs.
- **Schema audit (2026-08-21):** Live Supabase contains lowercase `armour`, `weapons`, and `items`, plus quoted mixed-case `"Materials"` and `"Condition"`. The three item tables have useful typed columns and pool arrays. However, weapons and armour currently have no material/condition foreign keys, so the modifier tables cannot yet be applied authoritatively. `"Condition"` also risks confusion with combat conditions, and both mixed-case names require permanent SQL quoting.
- **Recommended foundation:** Keep the existing tables untouched until a reviewed migration exists. Standardise the modifier tables to unquoted lowercase names (prefer `item_materials` and `item_conditions`), add explicit nullable material/condition references with foreign keys, validation constraints, and timestamps, then backfill catalogue data before switching application reads away from blobs. Character inventory should ultimately store owned-item instances referencing definitions, while retaining per-instance quantity, equipped state, condition and approved overrides.
- **Migration safety:** Do not make the new tables authoritative or remove blob fields until definition backfill, dual-read compatibility, and rollback tests pass.
- **Fix/checkpoint (2026-08-21):** Applied migration `007_relational_inventory`. Added lowercase `item_materials` and `item_conditions`, copied all legacy modifier rows, added default material/condition foreign keys to all three definition tables, and added relational `character_wallets`, `character_inventory_items`, and `inventory_pool_rules`. Owned items use real foreign keys through separate weapon/armour/item columns and retain quantity, equipped state, bag state, per-instance modifiers and approved JSON properties. Live backfill created six wallet rows and 44 owned-item rows (17 weapons, six armour pieces and 21 other items), including party-specific/homebrew definitions. Existing character blobs were not altered. Re-ran the backfill idempotently and confirmed the same totals.
- **Compatibility checkpoint (2026-08-21):** Character loads now hydrate wallet and equipment provenance from relational rows, with the blob as fallback. Character saves update the blob and relational wallet/ownership rows in one database transaction. Control-to-player gifts use that common save path. Player trade creation, acceptance and decline hydrate and mirror both sides so material/build quality travels with the specific item rather than being rerolled. Fresh-database migration support was corrected and verified locally; live migration checksums were reconciled only after the expected tables were checked.
- **Retest:** Live ledger confirms 001–009 applied and clean. Fresh isolated database builds all nine migrations. Full integration test passes character load/save/sync, equipment assignment, HP, movement, turns, enemy generation, reinforcement/loot, provenance-preserving trade and reconnection. 52 automated tests pass. Remaining before authority switch: compare blob and relational representations during normal multi-client play, migrate the remaining control pool editors away from local RDS, then test rollback before retiring inventory fields inside character blobs.

### DATA-002 — Control item editor lacks complete damage-type support

- **Status:** In progress — validated editor and combat modifiers wired
- **Priority:** High
- **Area:** Control inventory / NPC attacks / damage traits
- **Reported:** 2026-08-20
- **Original report:** Control does not provide a reliable place to specify item damage type(s), so NPC weapon attacks may not trigger vulnerabilities, resistances, or immunities correctly.
- **Test notes:** Support multiple damage components without accepting arbitrary invalid values.
- **Fix/checkpoint (2026-08-21):** Control item editing now selects primary and optional secondary damage types from the shared validated damage-type list rather than accepting arbitrary text. Migration 008 adds database checks for both weapon damage components. Equipped weapons expose relational material/build-quality attack and damage modifiers to combat, and their material is included in resistance/immunity/vulnerability matching (including Iron).
- **Retest:** Parsing passes and automated coverage confirms equipment attack bonuses are included. Full UI attack confirmation still needs a manual player/control combat pass.

### DATA-003 — Materials and conditions need rule integration

- **Status:** In progress — weapon provenance live
- **Priority:** High
- **Area:** Database / equipment rules / loot
- **Reported:** 2026-08-20
- **Original report:** Armour and weapons should have an appropriate material or condition. These definitions provide modifiers, and conditions include a drop rate, but the new tables are not integrated.
- **Test notes:** Clarify whether “condition” means item quality/durability, combat condition, or a separate equipment-condition concept. Avoid naming collision with combat conditions.
- **Fix/checkpoint (2026-08-21):** Added neutral `Wood` (all modifiers zero). Migrated weapons without provenance prompt their owning player once per session to roll permanent material and build quality; bows have forced Wood and only roll build quality. Results are written to the owned-item row and an immutable assignment log, hydrated into inventory metadata, retained through gifts/trades, and applied to attack, damage and armour calculations. Newly saved NPC equipment rolls eligible provenance when the template is saved, so later loot already carries its make and quality.
- **Regression fix (2026-08-21):** The first fresh test environment contained Wood but no build-quality rows, causing “material/build quality could not be saved”. Migration 009 now seeds the complete canonical material and build-quality definitions everywhere. A dedicated flag limits prompts to imported legacy weapons. Fresh player-created weapons instead expose manual Material and Build Quality fields. Control/NPC equipment rolls automatically unless its catalogue entry has “Lock this weapon to one material and build quality” enabled. Removed material adjectives from unlocked standard catalogue names/descriptions.
- **Display/NPC checkpoint (2026-08-21):** Removed the Armoury's older local stat calculation. Weapon cards now include material and quality attack modifiers and show their flat damage modifier; armour cards show material, quality and adjusted AC. Imported armour now receives the same one-time provenance assignment through migration 010. NPC carried weapons are generated directly from inventory definitions, automatically become attacks and loot, and are no longer duplicated in the manual NPC attack picker. Changing NPC armour immediately replaces its guaranteed armour loot. The creator includes a rolled equipment preview showing material, quality and weapon/armour statistics.
- **Retest:** The integration test now recreates a migrated weapon, successfully rolls and persists both values, verifies a fresh manually specified weapon does not prompt, generates random NPC equipment, loots it, and trades it without losing provenance. Isolated schema/integration tests and 52 automated tests pass.

### LOOT-001 — Loot restrictions need data-driven rules

- **Status:** Implemented — awaiting manual generator/loot retest
- **Priority:** Medium
- **Area:** NPC generation / loot
- **Reported:** 2026-08-20
- **Original report:** Iron weapons should not appear on Fae NPCs. Titanium-copper should only be lootable from Boss enemies.
- **Test notes:** Implement as validated eligibility rules rather than scattered UI checks.
- **Fix/checkpoint (2026-08-21):** Migration 008 stores material eligibility as data: Iron excludes enemy type Fae; Titanium Copper requires characteristic Boss. NPC equipment provenance filters through those rules before material selection, and the chosen weapon material is copied into the NPC attack as well as its loot record.
- **Retest:** Automated eligibility tests cover Fae/Iron and Boss/Titanium-Copper allow/deny paths. Manually save and loot one Fae weapon-user and one Boss weapon-user before closing.

### NPC-UI-001 — Feature inventory rules and equipment/attack duplication

- **Status:** Closed — user signed off 2026-08-21
- **Priority:** High
- **Area:** Control / NPC features / enemy generator
- **Reported:** 2026-08-21
- **Original report:** Current feature rules could not be reliably selected from their dropdown; users wanted every rule printed with an example roll. Inventory weapons also required a separate duplicate NPC attack definition.
- **Fix/checkpoint (2026-08-21):** Replaced the current-rules dropdown with a complete non-paginated table. Removal uses the selected table row, and Roll Example prints the actual item names produced. NPC weapon attacks are derived from the shared weapon inventory definition, including custom control weapons; the attack editor/picker remains for natural and special attacks only.
- **Retest:** Both apps parse, the full integration suite passes, and one control plus two players start together. Manually add/update/remove grouped and independent feature rules and verify the printed sample roll.

### NPC-UI-002 — NPC rule editors flash and generated attacks lose carried weapons

- **Status:** Closed — user signed off 2026-08-21
- **Priority:** High
- **Area:** Control / NPC creator / pools / features / attacks
- **Reported:** 2026-08-21
- **Original report:** Automatically adding armour caused the loot and AC panels to flash; the feature inventory table also flashed; generated examples showed only Unarmed Strike; pool statistics and damage traits could not be edited; and catalogue weapons such as Shortbow still appeared in the special-attack editor.
- **Cause:** Polling recreated equivalent inventory data frames and repeatedly invalidated dependent controls. Generated attacks were assembled separately from rolled inventory, while pool and feature records did not expose all of the fields needed for layered generation.
- **Fix/checkpoint (2026-08-21):** Inventory-backed controls now refresh only when their data signature changes, and the armour selector only updates loot after a real selection change. Pool records can edit exact ability scores and damage traits. Feature records can add damage and condition traits, natural attacks, movement/HP/AC changes and inventory rules. Generated weapon attacks now come from the NPC's actual rolled inventory; the natural/special attack designer filters out weapon-backed definitions. Layered trait resolution unions distinct traits, promotes a repeated resistance from separate pool/feature layers to immunity, lets explicit immunity win, and cancels a resistance/vulnerability conflict to normal damage.
- **Automated test:** 53 tests pass, including duplicate-resistance escalation and resistance/vulnerability conflict resolution. The fresh-database integration suite applies all ten migrations and passes character, combat, enemy generation, reinforcement, loot, provenance, trade and reconnection checks.
- **Retest:** In Control, leave the creator and feature editor open for several polling cycles and confirm no panels flash. Generate a pool with guaranteed Padded Armour and Shortbow and confirm both are carried/lootable, the armour affects AC, and Shortbow appears as an attack without appearing in the natural/special attack designer. Add the same resistance once at pool level and once through a feature and confirm the generated NPC has immunity.

### NPC-UI-003 — Custom features missing and NPC weapon provenance is opaque

- **Status:** Closed — user signed off 2026-08-21
- **Priority:** High
- **Area:** Control / NPC creator / feature equipment
- **Reported:** 2026-08-21
- **Original report:** Newly saved features did not appear in the creator. NPC weapons worked as attacks but were not directly visible/editable, repeated spear examples appeared to be Iron, and weapon build quality could not be seen.
- **Cause:** The creator's characteristic control still used the built-in static labels instead of the saved feature catalogue. Weapons were mixed into generic loot, while a legacy unlocked catalogue entry was still named “Iron Spear” even though its material roll was random.
- **Fix/checkpoint (2026-08-21):** The creator now builds its feature choices and descriptions from saved feature records, including custom additions. Carried weapons have a dedicated selector and per-template material/build-quality override; they remain automatic attacks and loot. Unlocked legacy material prefixes are removed for display, random rolls use the database drop weights, and generated template tables show material and build quality. Feature inventory rules can specify random or fixed material and quality and show a fully rolled example. Identical feature constraints combine; conflicting constraints safely fall back to a random roll, while a direct creator override wins. Material and quality attack/damage modifiers are copied into the generated NPC weapon attack.
- **Retest:** Select both newly created Barbarian features in the creator. Generate the spear at least ten times with both overrides set to Random and confirm the result column shows mixed eligible materials and build qualities. Then force one material and quality, save, and confirm its equipment and attack columns show them and combat uses the adjusted hit/damage values.

### COMBAT-CTRL-001 — Control combat lacks player movement and grapple controls

- **Status:** Fixed — awaiting manual control UI retest
- **Priority:** High
- **Area:** Control / live combat
- **Reported:** 2026-08-21
- **Original report:** The Control combat screen appeared to lack player-side features including Escape Grapple and click-to-move.
- **Cause:** Control had a tile-click listener, but it wrote to a removed movement-step input. Every destination was therefore reduced to a single square and distant clicks appeared not to work. Control had condition overrides but no rules-based grapple escape action.
- **Fix/checkpoint (2026-08-21):** Empty-tile clicks now pass the requested distance directly to the existing validated movement routine, allowing the full remaining straight/diagonal movement allowance while respecting terrain, occupancy, speed and grapple/restrained movement zero. The battlefield explains the gesture. Escape Grapple is enabled only when the active combatant is grappled, uses that actor's better Strength/Dexterity modifier and proficiency, removes the condition on success, and records the contested totals as a combat event.
- **Retest:** Start combat, click a clear tile more than one square away and confirm the active actor moves the full legal distance. Confirm blocked, occupied and over-speed destinations stop correctly. Apply Grappled to an actor, confirm Escape Grapple enables only on that actor's turn, and verify both successful and failed attempts appear in the combat log.

### LOOT-002 — Mundane loot needs an authoritative category

- **Status:** Implemented — awaiting inventory retest
- **Priority:** Medium
- **Area:** Inventory / loot data
- **Reported:** 2026-08-20
- **Original report:** Mundane loot needs a table or an `items.type = mundane_loot` category.
- **Fix (2026-08-21):** Migration 015 adds a validated category to the common `items` table rather than creating another loot table. Allowed categories are mundane loot, consumable, crafting material, tool, treasure and quest item. Existing generic rows safely backfill to mundane loot, while recognisable item types retain their semantic category. Control's inventory editor exposes the category and relational saves preserve it.
- **Automated test:** Unit coverage verifies default mundane, explicit crafting and inferred consumable categories. Fresh migration/integration inserts and retrieves a mundane item under the database constraint. Migration 015 is live on Supabase.
- **Fix/checkpoint:** `a288ceb`
- **Retest:** Create one ordinary object and one item in every other category in Control. Save/reload each, give or loot copies to a player, and confirm names/details remain intact and Control retains the selected category.

### TRADE-002 — Gold transfer appeared to change neither purse

- **Status:** Fixed — awaiting two-player retest
- **Priority:** High
- **Area:** Player trading / HUD
- **Reported:** 2026-08-21
- **Fix:** Prevented the stale Inventory gold input from overwriting a transactional trade result, immediately refreshes both participants after resolution, and implemented the previously missing Player HUD gold renderer.
- **Automated test:** An accepted five-gold offer now leaves the sender five lower and the recipient five higher in the relational wallet and character state.
- **Retest:** Send and accept gold with two open Player clients. Confirm both Inventory and top-HUD totals change without reopening the app.

### MAGIC-003 — Blood-dependent power text appeared for non-Fae sorcerers

- **Status:** Fixed — awaiting Magic-screen retest
- **Priority:** Medium
- **Area:** Magic description
- **Reported:** 2026-08-21
- **Fix:** The “must be taken” description is now restricted to Tylwyth/Fae characters with zero natural regeneration in both applications.
- **Retest:** Compare a non-Fae zero-regeneration sorcerer with a Tylwyth/Fae equivalent.

### CAMP-003 — Fire was local to the player who lit it

- **Status:** Fixed — awaiting multi-player retest
- **Priority:** High
- **Area:** Rest / shared camp
- **Reported:** 2026-08-21
- **Fix:** Migration 017 stores fire state with shared session supplies. Camp screens poll that common state; lighting or extinguishing it updates every client.
- **Retest:** Open two Player clients, light the fire in one, and confirm the second changes within three seconds and does not spend extra wood.

### REST-004 — Mixed party rest outcomes and runaway Skip Day

- **Status:** Implemented — awaiting multi-player retest
- **Priority:** High
- **Area:** Rest / day synchronisation
- **Reported:** 2026-08-21
- **Rule:** Each active player chooses Full, Half, or No Rest for the same party night. All advance to the shared day; full receives full recovery, half receives six-hour/short-rest recovery and half maximum HP, and no-rest receives no recovery. Online Skip Day records No Rest in the current shared cycle and cannot jump ahead alone.
- **Fix:** Migration 017 records the individual outcome on each rest completion. Existing one-completion-per-character protection remains authoritative.
- **Retest:** Use three clients in one session and choose full, half, and skip. Confirm the same day on all three, distinct recovery, and that repeated Skip Day cannot advance again.

### CTRL-002 — Control gift recipient and session removal selection

- **Status:** Fixed — awaiting Control retest
- **Priority:** High
- **Area:** Control inventory / players
- **Reported:** 2026-08-21
- **Fix:** Gift recipients now load directly from the active session and retain a valid selection. Player removal has an explicit named selector rather than depending on a fragile table-row selection, and reports success only when a row was actually removed.
- **Retest:** Switch Control between two sessions, give a catalogue copy to a selected player, then remove that player and confirm both lists refresh.

### CTRL-003 — Control needs a persistent party HUD

- **Status:** Implemented — awaiting visual retest
- **Priority:** Medium
- **Area:** Control shell
- **Reported:** 2026-08-21
- **Fix:** Added a scrollable left-side session HUD showing each player, current/temp HP, and inactive status from the shared Control snapshot.
- **Retest:** Check the HUD at common laptop resolution with several players and confirm it updates after HP changes and session switches.

### NPC-006 — Generated default names omit feature identity

- **Status:** Fixed — awaiting NPC Creator retest
- **Priority:** Low
- **Area:** NPC Creator
- **Reported:** 2026-08-21
- **Fix:** Generated names now combine the selected pool/type with readable configured feature names, for example “Undead Animal Armoured”, rather than falling back to “Undead Enemy”.
- **Retest:** Apply defaults to pools with one and several custom features and confirm the editable name includes each feature display name.

### FEATURE-004 — Persistent merchant generation and player haggling

- **Status:** Implemented — awaiting live playtest
- **Priority:** Medium
- **Area:** Control merchants / Player trading
- **Requested:** 2026-08-21
- **Scope delivered:** Control can generate Poor, Moderate, or Rich merchants specialising in general goods, food, weapons, armour, or hunting stock. Stock comes from the authoritative Control catalogue, including quantities and rolled equipment material/build quality. Merchants have a real purse and a Hard, Fair, or Generous haggling temperament.
- **Player workflow:** Invited players receive a merchant popup and retain a merchant button while the shop is open. Buying and selling makes a Persuasion check against the merchant temperament, shows the roll/DC/result and calculates a bounded price. Inventory, stock, character gold and merchant gold update atomically; equipment provenance travels with the item.
- **Persistence:** Migration 018 stores merchants, stock, invitations and an immutable transaction history. Control can inspect current stock/purse, reinvite players, or close the merchant.
- **Automated test:** Haggling price rules pass unit coverage. Fresh migrations 001–018 and integration coverage pass invitation, purchase, resale, stock, inventory and transaction history. One Control and two Player apps start together.
- **Retest:** Generate one merchant of each wealth/specialty combination. Invite two players; buy and sell ordinary items and equipment; confirm the shown roll affects price, material/quality survives, both purses and quantities update, an unaffordable sale is refused, and closing the merchant removes Player access.

### CTRL-004 — Party HUD empty and merchant invitation controls obscured

- **Status:** Fixed — awaiting Control retest
- **Priority:** High
- **Area:** Control shell / sessions / merchants
- **Reported:** 2026-08-21
- **Cause:** The legacy header Session ID input defaulted to `1` and could overwrite the session selected by the Sessions module. The HUD and merchant recipient list therefore queried the wrong session. The fixed HUD also occupied space over the centred dashboard at laptop widths.
- **Fix:** Removed the competing Session ID input; the Sessions tab is now authoritative and the header only displays its active choice. Player membership/HP uses one shared three-second Control reactive. The desktop dashboard has a HUD gutter, responsive narrow-screen layout, and a separated merchant invitation panel/actions row.
- **Retest:** Set a populated session active and confirm the HUD and merchant player choices populate within three seconds. Switch sessions and confirm both change together. Check that the HUD covers no controls on desktop and becomes an in-flow panel below 900px.

## Planned features

### FEATURE-001 — Rune module and combat integration

- **Status:** Deferred
- **Priority:** Medium
- **Reported:** 2026-08-17
- **Scope:** Build rune ownership/use rules and expose valid rune actions in combat.

### FEATURE-002 — Player-submitted issue reports

- **Status:** Deferred
- **Priority:** Medium
- **Reported:** 2026-08-17
- **Scope:** Let players submit reports with app/module context, character/session identifiers, reproduction steps, severity, timestamp, and resolution status. Store structured records in SQL rather than free-form logs alone.

### FEATURE-003 — Horse and animal companion module

- **Status:** Deferred
- **Priority:** Low
- **Reported:** 2026-08-17
- **Scope:** Companion ownership, statistics, travel, combat participation, inventory, injury, and persistence.

## Verified issues

No issues from this testing log have been verified yet.
