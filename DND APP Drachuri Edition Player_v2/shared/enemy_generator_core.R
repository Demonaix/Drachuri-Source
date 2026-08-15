enemy_generator_types <- function() {
  list(
    Custom = list(hp_max = 10L, ac = 12L, movement_speed = 30L,
      abilities = c(str=10L,dex=10L,con=10L,int=10L,cha=10L,bld_str=10L), attacks = list(), loot = character()),
    Bandit = list(hp_max = 12L, ac = 12L, movement_speed = 30L,
      abilities = c(str=12L,dex=14L,con=12L,int=10L,cha=10L,bld_str=10L),
      attacks = list(list(name="Shortsword",hit=3L,dmg="1d6+1",type="slashing",material="steel")), loot = c("Coin purse")),
    Animal = list(hp_max = 11L, ac = 12L, movement_speed = 40L,
      abilities = c(str=12L,dex=14L,con=12L,int=3L,cha=6L,bld_str=8L),
      attacks = list(list(name="Claw",hit=4L,dmg="1d6+2",type="slashing",material="natural")), loot = c("Animal pelt")),
    Fae = list(hp_max = 10L, ac = 13L, movement_speed = 30L,
      abilities = c(str=8L,dex=14L,con=10L,int=12L,cha=14L,bld_str=12L),
      attacks = list(list(name="Fae Bolt",hit=4L,dmg="1d6+2",type="force",material="magic")), loot = c("Fae dust"), vulnerabilities = c("iron"))
  )
}

enemy_generator_characteristics <- function() {
  list(
    Barbarian = list(ability_bonus=c(str=2L,con=2L),
      attacks=list(list(name="Battleaxe",hit=4L,dmg="1d8+2",type="slashing",material="steel")), loot=c("Bear pelt")),
    Speedy = list(ability_bonus=c(dex=2L), movement_bonus=10L),
    Boss = list(ability_bonus=c(str=2L,dex=2L,con=2L,int=2L,cha=2L,bld_str=2L), hp_multiplier=2, ac_bonus=1L),
    Fae = list(vulnerabilities=c("iron")),
    Animal = list(attacks=list(list(name="Claw",hit=4L,dmg="1d6+2",type="slashing",material="natural")), loot=c("Animal pelt"))
  )
}

resolve_enemy_blueprint <- function(enemy_type="Custom", characteristics=character()) {
  types <- enemy_generator_types(); mods <- enemy_generator_characteristics()
  type <- if (enemy_type %in% names(types)) enemy_type else "Custom"
  out <- types[[type]]; out$enemy_type <- type; out$characteristics <- unique(intersect(as.character(characteristics), names(mods)))
  for (nm in out$characteristics) {
    mod <- mods[[nm]]
    if (!is.null(mod$ability_bonus)) for (stat in names(mod$ability_bonus)) out$abilities[[stat]] <- out$abilities[[stat]] + mod$ability_bonus[[stat]]
    out$hp_max <- as.integer(round(out$hp_max * (mod$hp_multiplier %||% 1)))
    out$ac <- as.integer(out$ac + (mod$ac_bonus %||% 0L))
    out$movement_speed <- as.integer(out$movement_speed + (mod$movement_bonus %||% 0L))
    for (field in c("attacks","loot","resistances","immunities","vulnerabilities","condition_immunities"))
      out[[field]] <- unique(c(out[[field]] %||% list(), mod[[field]] %||% list()))
  }
  for (field in c("resistances","immunities","vulnerabilities","condition_immunities")) out[[field]] <- as.character(out[[field]] %||% character())
  out
}

enemy_json <- function(x) jsonlite::toJSON(x %||% list(), auto_unbox=TRUE, null="null")
enemy_text_values <- function(x) unique(trimws(Filter(nzchar, unlist(strsplit(as.character(x %||% ""), ",", fixed=TRUE)))))
enemy_pg_array <- function(x) {
  x <- as.character(x %||% character()); if (!length(x)) return("{}")
  paste0("{", paste(sprintf('"%s"', gsub('(["\\\\])', '\\\\\\1', x)), collapse=","), "}")
}

enemy_db_json <- function(x, default=list()) {
  if (is.list(x) && !is.data.frame(x)) return(x)
  tryCatch(jsonlite::fromJSON(as.character(x %||% ""), simplifyVector=FALSE), error=function(e) default)
}

enemy_db_values <- function(x) {
  if (is.null(x) || !length(x)) return(character())
  if (is.list(x)) x <- unlist(x, use.names=FALSE)
  text <- as.character(x); text <- gsub('^\\{|\\}$', '', text)
  text <- gsub('^"|"$', '', unlist(strsplit(text, ',', fixed=TRUE)))
  unique(trimws(text[nzchar(trimws(text))]))
}
