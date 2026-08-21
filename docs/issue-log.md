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

- **Status:** In progress — relational schema and live backfill complete
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

- **Status:** Investigating
- **Priority:** Critical
- **Area:** Database / inventory foundation
- **Reported:** 2026-08-20
- **Original report:** Weapons and items need authoritative relational tables with explicit variables rather than relying on blobs.
- **Schema audit (2026-08-21):** Live Supabase contains lowercase `armour`, `weapons`, and `items`, plus quoted mixed-case `"Materials"` and `"Condition"`. The three item tables have useful typed columns and pool arrays. However, weapons and armour currently have no material/condition foreign keys, so the modifier tables cannot yet be applied authoritatively. `"Condition"` also risks confusion with combat conditions, and both mixed-case names require permanent SQL quoting.
- **Recommended foundation:** Keep the existing tables untouched until a reviewed migration exists. Standardise the modifier tables to unquoted lowercase names (prefer `item_materials` and `item_conditions`), add explicit nullable material/condition references with foreign keys, validation constraints, and timestamps, then backfill catalogue data before switching application reads away from blobs. Character inventory should ultimately store owned-item instances referencing definitions, while retaining per-instance quantity, equipped state, condition and approved overrides.
- **Migration safety:** Do not make the new tables authoritative or remove blob fields until definition backfill, dual-read compatibility, and rollback tests pass.
- **Fix/checkpoint (2026-08-21):** Applied migration `007_relational_inventory`. Added lowercase `item_materials` and `item_conditions`, copied all legacy modifier rows, added default material/condition foreign keys to all three definition tables, and added relational `character_wallets`, `character_inventory_items`, and `inventory_pool_rules`. Owned items use real foreign keys through separate weapon/armour/item columns and retain quantity, equipped state, bag state, per-instance modifiers and approved JSON properties. Live backfill created six wallet rows and 44 owned-item rows (17 weapons, six armour pieces and 21 other items), including party-specific/homebrew definitions. Existing character blobs were not altered. Re-ran the backfill idempotently and confirmed the same totals.
- **Retest:** Migration ledger confirms 001–007 applied. Backfill dry run and applied verification agree on six characters, 44 owned items and 111 total gold. Existing 50 automated game tests still pass. Remaining before authority switch: add application dual-write/dual-read compatibility, compare both representations during normal play, migrate the control catalogue/pool editors away from local RDS, then test rollback before retiring inventory fields inside character blobs.

### DATA-002 — Control item editor lacks complete damage-type support

- **Status:** Open
- **Priority:** High
- **Area:** Control inventory / NPC attacks / damage traits
- **Reported:** 2026-08-20
- **Original report:** Control does not provide a reliable place to specify item damage type(s), so NPC weapon attacks may not trigger vulnerabilities, resistances, or immunities correctly.
- **Test notes:** Support multiple damage components without accepting arbitrary invalid values.
- **Fix/checkpoint:** —
- **Retest:** Not started.

### DATA-003 — Materials and conditions need rule integration

- **Status:** Open
- **Priority:** High
- **Area:** Database / equipment rules / loot
- **Reported:** 2026-08-20
- **Original report:** Armour and weapons should have an appropriate material or condition. These definitions provide modifiers, and conditions include a drop rate, but the new tables are not integrated.
- **Test notes:** Clarify whether “condition” means item quality/durability, combat condition, or a separate equipment-condition concept. Avoid naming collision with combat conditions.
- **Fix/checkpoint:** —
- **Retest:** Not started.

### LOOT-001 — Loot restrictions need data-driven rules

- **Status:** Open
- **Priority:** Medium
- **Area:** NPC generation / loot
- **Reported:** 2026-08-20
- **Original report:** Iron weapons should not appear on Fae NPCs. Titanium-copper should only be lootable from Boss enemies.
- **Test notes:** Implement as validated eligibility rules rather than scattered UI checks.
- **Fix/checkpoint:** —
- **Retest:** Not started.

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
