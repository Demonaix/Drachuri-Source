# DND Drachuri Edition

This workspace contains two R Shiny applications:

- `DND APP Drachuri Edition Player_v2`: the app distributed to players.
- `DND APP Drachuri Edition 2 Control`: the GM control dashboard.

Both applications share a Supabase PostgreSQL database and currently carry
one authoritative copy of the common game and database logic. It lives in the
player app's `shared/` directory so a distributed player folder is complete on
its own. The control app's `global.R` and `session_db.R` are compatibility
entry points that source those same files.

## Safe version workflow

Before a working session, commit the current state. Make changes on a named
branch and commit small, tested steps. Useful rollback commands are:

```sh
git status
git diff
git log --oneline --decorate
git restore --source <commit> -- "path/to/file"
```

Avoid using `git reset --hard` for routine rollback because it discards all
uncommitted work. The large third-party 3D model directory is deliberately
excluded from Git; it remains present on disk.

Until Git is enabled for this folder, create a dated source rollback archive
from Terminal with:

```sh
sh scripts/create_source_backup.sh
```

## Building the player release

Update `VERSION`, then build a dated archive with:

```sh
sh scripts/build_player_release.sh
```

The normal release omits the disabled large 3D model library and the local
`renv` package cache. To include the models, use:

```sh
INCLUDE_3D_MODELS=1 sh scripts/build_player_release.sh
```

The player's `renv.lock` is included. After unpacking, players can restore the
same dependencies with `renv::restore()` when package versions change.

Player releases now include one-click launchers for Windows, macOS, and Linux.
They install or update locked dependencies automatically, write diagnostic
logs under `launcher/logs/`, and launch the local app without RStudio.

## Database connections

Both apps use the `pool` package with a maximum of three connections per R
process. If `pool` has not been restored yet, the code temporarily falls back
to one reusable PostgreSQL connection rather than reconnecting for every poll.
The connection resources are closed when the Shiny process stops.

## Database migrations

Ordered schema migrations live under `database/migrations/`. Check migration
status without making changes using:

```sh
sh scripts/migrate_database.sh --status
```

See `docs/database-migrations.md` before baselining Supabase or adding a schema
change.

## Tests and profiling

Run the dependency-light stability tests with:

```sh
Rscript tests/run_tests.R
```

Run the static player hotspot report with:

```sh
Rscript scripts/profile_player_hotspots.R
```

## Isolated local multiplayer test

PostgreSQL 18 is configured under `test-env/` with a separate database, two
fictional characters, and one encounter. It never uses Supabase because the
test launchers disable machine-level R startup files and supply local database
settings explicitly.

Run the database integration test with:

```sh
sh test-env/run_integration_test.sh
```

Launch one control app and two independent player processes with:

```sh
sh test-env/run_local_apps.sh
```

Run the bounded three-server startup smoke test with:

```sh
sh test-env/smoke_test_apps.sh
```

Then open ports 3838, 3839, and 3840 as printed by the launcher. Stop the apps
and database with:

```sh
sh test-env/stop_local_apps.sh
sh test-env/stop_db.sh
```

## Running locally

Run each app with its own directory as the working directory:

```r
shiny::runApp("DND APP Drachuri Edition Player_v2")
shiny::runApp("DND APP Drachuri Edition 2 Control")
```
