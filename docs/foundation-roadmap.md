# Foundation roadmap

This is the agreed order of work before substantial module development.

## 1. Local Git version history

- Create a source archive before initialising the repository.
- Make the current tested application the baseline commit.
- Use small commits and a named branch for substantial module upgrades.

## 2. Database schema migrations

- Add ordered SQL migrations shared by Supabase and the local PostgreSQL test environment.
- Record which migrations have been applied.
- Provide a safe migration command and document database backup expectations.

## 3. Complete multiplayer regression

- Run one control process and two independent player processes locally.
- Verify character load/save, HP changes, combat turns, encounter positions,
  reconnect behaviour, and clean shutdown.

## Later improvements

- Consolidate the remaining control combat polling into compact snapshots.
- Load module source files once per process rather than once per session.
- Replace noisy console output with consistent diagnostic logging.
- Add regression tests alongside every module bug fix.
- Archive inactive `_old`, `_backup`, `_new`, and `_sad` files after confirming
  that they are not sourced by either application.
