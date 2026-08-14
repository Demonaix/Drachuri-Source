# Database migrations

The authoritative ordered schema history is in `database/migrations/`.
Migration files are immutable after they have been applied. Schema changes are
made by adding the next numbered SQL file, never by editing an applied file.

## Existing Supabase database

The live database predates the migration ledger. Initialise its ledger once,
after taking a Supabase backup:

```sh
sh scripts/migrate_database.sh --status
sh scripts/migrate_database.sh --baseline
sh scripts/migrate_database.sh --status
```

`--baseline` does not execute migration SQL. It validates the required current
tables, then records the existing schema as migration `001`.

## Normal workflow

Inspect pending work without changing the database:

```sh
sh scripts/migrate_database.sh --dry-run
```

Apply pending migrations atomically:

```sh
sh scripts/migrate_database.sh
```

The runner takes a PostgreSQL advisory lock, verifies checksums of previously
applied files, and records successful migrations in `schema_migrations`.

## Fresh local database

Migration `001_current_schema.sql` is the canonical fresh schema. The legacy
`test-env/schema.sql` path remains as a small psql compatibility wrapper for
the existing reset scripts.
