-- Compatibility entry point for existing local reset scripts.
-- The authoritative schema now lives in the ordered migrations directory.
\ir ../database/migrations/001_current_schema.sql
