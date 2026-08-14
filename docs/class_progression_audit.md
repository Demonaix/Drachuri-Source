# Class progression audit

Reviewed against the class and subclass definitions used by the player app.

| Class | Defined levels | ASI levels | Subclass choice | Current status |
|---|---:|---|---:|---|
| Rogue | 1–10 | 4, 8, 10 | 3 | Complete through level 10 |
| Fighter | 1–10 | 4, 6, 8, 10 | 3 | Complete through level 10 |
| Barbarian | 1–10 | 4, 8 | 3 | Complete through level 10 |
| Hanianol Sorcerer | 1–20 (some scaling-only levels) | 4, 8, 12, 16, 19 | 3 | Full 20-level progression |
| Na'Haran Sorcerer | 1–20 (some scaling-only levels) | 4, 8, 12, 16, 19 | 3 | Full 20-level progression |

## Corrections made

- Ability Score Improvements now require two `+1` choices. Choosing the same stat twice gives `+2`; scores are capped at 20.
- Both Sorcerer classes now receive the standard ASI schedule at levels 4, 8, 12, 16 and 19.
- Level-up cannot advance past the last level actually defined for a class. Rogue, Fighter and Barbarian therefore stop safely at level 10 until levels 11–20 are authored.
- Existing subclass choices remain mandatory at level 3.
- Natural Magic specialty, Thermal Wild Magic and Electromagnetic Magic choices are now explicit and saved.

## Feature metadata

Every feature is assigned one or more normalized tags:

- `stat_increase`
- `ability`
- `combat`
- `spell`
- `passive`
- `reaction`
- `movement`
- `healing`
- `utility`
- `subclass`

Tags are shown in the level-up preview and provide the routing layer for future combat, spellbook and character-sheet views.

## Combat automation currently defined

Only features with explicit mechanics become combat buttons. Descriptive text is never guessed into damage rules.

| Feature | Combat result |
|---|---|
| Bloodthirsty Bite | `1d8 + Strength modifier` piercing damage |
| Water Channeler | Costs 10 Sindre; deals 10% target maximum HP as necrotic damage |
| Improved Water Channeler | Replaces Water Channeler; costs 10 Sindre; deals 20% target maximum HP |

Damage goes through the existing resistance, immunity and vulnerability rules, updates encounter HP, spends the character resource and writes to the combat log.

## Character-data synchronization

Unlocked features now grant deterministic character effects through one shared derivation pass:

- Fae Blooded: Survival Expertise.
- Natural Magic: Medicine proficiency (without downgrading existing Expertise).
- Survival Mastery: Survival Expertise.
- Assassin Bonus Proficiencies: disguise kit and poisoner's kit.

The canonical damage fields are `combat_profile$resistances`, `combat_profile$immunities` and `combat_profile$vulnerabilities`. Existing values are preserved and normalized. Combat already consumes these fields.

Conditional defences are stored separately with their source and activation condition. Rage therefore records physical resistance while raging, and Mindless Rage records charm/fear immunity while raging, without incorrectly making either benefit permanent.

## Content still requiring design decisions

- Rogue, Fighter and Barbarian levels 11–20 need feature definitions before those levels can be enabled.
- Feats are mentioned as an alternative to ASIs but there is no feat catalogue yet; the safe implementation currently applies stat increases only.
- Fighting Style is selected during character creation, but the creation flow does not yet offer the detailed style choices.
- Battle Master manoeuvres, Totem choices and several Sorcerer branches have narrative descriptions but need explicit option lists and mechanical payloads.
- Rest-limited uses, reactions, saving throws, areas of effect, conditions and duration tracking need a shared action/effect schema before those abilities should become executable combat buttons.
