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

- **Status:** In progress — live dual-write compatibility active
- **Priority:** High
- **Area:** Control UI / reactive refresh
- **Reported:** 2026-08-17
- **Original report:** Difficulty selecting weapons and other values because dropdowns flash, revert, or lose their options.
- **Work already attempted:** Reactive selection-preservation changes in checkpoints `1e536cc` and `5ab776e`.
- **Test notes:** Issue was still reproducible after earlier fixes. Record the exact tab, dropdown, selected value, and value it changes to on the next reproduction.
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

- **Status:** Open
- **Priority:** Medium
- **Area:** Blood / hearts / resources
- **Reported:** 2026-08-17
- **Original report:** Eating hearts appears not to add temporary Sindre. It adds only +2 temporary HP, which may also be incorrect.
- **Test notes:** Confirm the intended rule by heart type and compare temporary HP and temporary Sindre before/after consumption.
- **Fix/checkpoint:** —
- **Retest:** Not started.

### CHAR-002 — Magical nature and skill identity lack variety

- **Status:** Open
- **Priority:** Medium
- **Area:** Character generation
- **Reported:** 2026-08-17
- **Original report:** Magical nature and skill identity repeatedly produce the same archetypes, especially Starved Abyss Channeler and Cunning Stalker Operative.
- **Test notes:** Audit weighting, deterministic seeds, fallback choices, and whether existing values are being reused.
- **Fix/checkpoint:** —
- **Retest:** Not started.

### REST-001 — Unlit rest-fire image is missing

- **Status:** Open
- **Priority:** Low
- **Area:** Rest UI / assets
- **Reported:** 2026-08-17
- **Original report:** The rest fire has a missing image when the fire is not lit.
- **Test notes:** Record the missing asset URL from the browser console if available.
- **Fix/checkpoint:** —
- **Retest:** Not started.

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

### MAGIC-001 — Natural magic spell rules need an intent audit

- **Status:** Open
- **Priority:** Medium
- **Area:** Magic / rules audit
- **Reported:** 2026-08-17
- **Original report:** Natural magic spells need checking against their originally intended behaviour.
- **Test notes:** Requires the original homebrew descriptions or a confirmed replacement specification.
- **Fix/checkpoint:** —
- **Retest:** Not started.

### MAP-001 — Ravine tile missing from Control map builder

- **Status:** Open
- **Priority:** Low
- **Area:** Control map builder
- **Reported:** 2026-08-17
- **Original report:** Ravine is unavailable as a map tile option.
- **Test notes:** Define movement blocking, line-of-sight, appearance, and fall behaviour.
- **Fix/checkpoint:** —
- **Retest:** Not started.

### COMBAT-001 — Bloodlust Risk may not be implemented

- **Status:** Open
- **Priority:** Medium
- **Area:** Class tree / combat
- **Reported:** 2026-08-17
- **Original report:** Bloodlust Risk exists in the tree but may not be enacted in combat.
- **Test notes:** Locate the unlock definition, intended trigger, save/check, effect, duration, and recovery rule.
- **Fix/checkpoint:** —
- **Retest:** Not started.

### BLOOD-002 — Blood consumption needs a daily history

- **Status:** Open
- **Priority:** Low
- **Area:** Blood inventory / history
- **Reported:** 2026-08-17
- **Original report:** Add a record of what blood was consumed each day.
- **Test notes:** Prefer append-only events over another mutable character blob field.
- **Fix/checkpoint:** —
- **Retest:** Not started.

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

- **Status:** Open
- **Priority:** Medium
- **Area:** Rest / blood-drinker rules
- **Reported:** 2026-08-17
- **Original report:** Tylwyth Teg blood drinkers should receive a reminder when starting a long rest, but the prompt does not appear.
- **Test notes:** Record race/subrace, blood requirement state, current starvation state, and rest type.
- **Fix/checkpoint:** —
- **Retest:** Not started.

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

- **Status:** Open
- **Priority:** Medium
- **Area:** Magic / class progression
- **Reported:** 2026-08-17
- **Original report:** Exothermic should link to unlocked fire magic; endothermic to cold; mechanical should be available to all sorcerers; natural should be restricted to Hanianol.
- **Test notes:** Audit unlock prerequisites, existing characters, and whether correction requires a backfill.
- **Fix/checkpoint:** —
- **Retest:** Not started.

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

- **Status:** Open
- **Priority:** Medium
- **Area:** Inventory / loot data
- **Reported:** 2026-08-20
- **Original report:** Mundane loot needs a table or an `items.type = mundane_loot` category.
- **Test notes:** Prefer the common items table with a validated category unless mundane loot has genuinely different fields or lifecycle.
- **Fix/checkpoint:** —
- **Retest:** Not started.

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
