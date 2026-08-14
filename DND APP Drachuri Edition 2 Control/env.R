set_env_default <- function(name, value) {
  if (!nzchar(Sys.getenv(name, unset = ""))) {
    do.call(Sys.setenv, stats::setNames(list(value), name))
  }
}

# Local tests and deployments can override these before the app starts.
set_env_default("SUPABASE_HOST", "aws-1-eu-west-2.pooler.supabase.com")
set_env_default("SUPABASE_PORT", "5432")
set_env_default("SUPABASE_DBNAME", "postgres")
set_env_default("SUPABASE_USER", "postgres.kymncomirjlvjhcsnfnl")
set_env_default("SUPABASE_DB_PASSWORD", "defvEf-sufru5-suwnac")
