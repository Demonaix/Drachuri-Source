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

- **Status:** Open
- **Priority:** High
- **Area:** Character creation / levelling / HP
- **Reported:** 2026-08-17
- **Original report:** A character created at a level other than 1 remains at 10 HP and cannot be changed correctly.
- **Test notes:** Capture class, starting level, CON, expected HP, displayed maximum HP, and saved maximum HP.
- **Fix/checkpoint:** —
- **Retest:** Not started.

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

- **Status:** Open
- **Priority:** High
- **Area:** Party resources / database synchronisation
- **Reported:** 2026-08-17
- **Original report:** Wood, water, and rations need to be shared consistently across the player party.
- **Test notes:** Decide whether resources belong to the session, camp, or individual characters before migration.
- **Fix/checkpoint:** —
- **Retest:** Not started.

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

- **Status:** Open
- **Priority:** High
- **Area:** Rest / party synchronisation
- **Reported:** 2026-08-17
- **Original report:** A party long rest should be coordinated so different players do not advance independently.
- **Test notes:** Likely needs one session-level rest event initiated or approved by Control.
- **Fix/checkpoint:** —
- **Retest:** Not started.

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

- **Status:** Implemented — awaiting manual control UI retest
- **Priority:** High
- **Area:** Control / NPC features / enemy generator
- **Reported:** 2026-08-21
- **Original report:** Current feature rules could not be reliably selected from their dropdown; users wanted every rule printed with an example roll. Inventory weapons also required a separate duplicate NPC attack definition.
- **Fix/checkpoint (2026-08-21):** Replaced the current-rules dropdown with a complete non-paginated table. Removal uses the selected table row, and Roll Example prints the actual item names produced. NPC weapon attacks are derived from the shared weapon inventory definition, including custom control weapons; the attack editor/picker remains for natural and special attacks only.
- **Retest:** Both apps parse, the full integration suite passes, and one control plus two players start together. Manually add/update/remove grouped and independent feature rules and verify the printed sample roll.

### NPC-UI-002 — NPC rule editors flash and generated attacks lose carried weapons

- **Status:** Fixed — awaiting manual control UI retest
- **Priority:** High
- **Area:** Control / NPC creator / pools / features / attacks
- **Reported:** 2026-08-21
- **Original report:** Automatically adding armour caused the loot and AC panels to flash; the feature inventory table also flashed; generated examples showed only Unarmed Strike; pool statistics and damage traits could not be edited; and catalogue weapons such as Shortbow still appeared in the special-attack editor.
- **Cause:** Polling recreated equivalent inventory data frames and repeatedly invalidated dependent controls. Generated attacks were assembled separately from rolled inventory, while pool and feature records did not expose all of the fields needed for layered generation.
- **Fix/checkpoint (2026-08-21):** Inventory-backed controls now refresh only when their data signature changes, and the armour selector only updates loot after a real selection change. Pool records can edit exact ability scores and damage traits. Feature records can add damage and condition traits, natural attacks, movement/HP/AC changes and inventory rules. Generated weapon attacks now come from the NPC's actual rolled inventory; the natural/special attack designer filters out weapon-backed definitions. Layered trait resolution unions distinct traits, promotes a repeated resistance from separate pool/feature layers to immunity, lets explicit immunity win, and cancels a resistance/vulnerability conflict to normal damage.
- **Automated test:** 53 tests pass, including duplicate-resistance escalation and resistance/vulnerability conflict resolution. The fresh-database integration suite applies all ten migrations and passes character, combat, enemy generation, reinforcement, loot, provenance, trade and reconnection checks.
- **Retest:** In Control, leave the creator and feature editor open for several polling cycles and confirm no panels flash. Generate a pool with guaranteed Padded Armour and Shortbow and confirm both are carried/lootable, the armour affects AC, and Shortbow appears as an attack without appearing in the natural/special attack designer. Add the same resistance once at pool level and once through a feature and confirm the generated NPC has immunity.

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
