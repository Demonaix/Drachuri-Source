# Player performance profile

The static reactive audit identifies these primary hotspots:

1. `debug_combat_module.R` — the largest reactive graph, with combat map
   rendering, actor state, movement, attacks, and database-driven updates.
2. `magic_module.R` — several interdependent derived-resource reactives.
3. `level_module.R`, `rest_module.R`, and `landing_module.R` — many observers,
   though most are user-event driven rather than continuous polling.
4. `inventory_module.R` and `armoury_module.R` — repeated character validation
   and derived equipment calculations after character changes.

The former continuous database hotspots were the standalone HP poll, party HUD
poll, and combat poll. They now consume one shared snapshot that uses a single
checked-out connection every three seconds and only invalidates consumers when
the payload changes.

Run the reproducible static audit with:

```sh
Rscript scripts/profile_player_hotspots.R
```

For real database timing and payload sizes, launch the player app with the
environment variable `DND_PROFILE=true`. Each snapshot logs elapsed time and
serialized payload size. This makes it possible to profile a real game session
without logging character contents.
