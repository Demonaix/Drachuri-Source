story_safe_id <- function(x) {
  x<-gsub("[^a-zA-Z0-9_-]+","-",tolower(trimws(as.character(x%||%"story"))));x<-gsub("^-+|-+$","",x);if(!nzchar(x))"story"else substr(x,1L,80L)
}

storyboard_root <- function(app_root=NULL) {
  if(is.null(app_root)||!length(app_root)||is.na(app_root[[1L]])||!nzchar(as.character(app_root[[1L]])))app_root<-Sys.getenv("DRACHURI_DATA_DIR",unset="")
  if(!nzchar(as.character(app_root[[1L]]))){wd<-getwd();app_root<-if(is.null(wd)||!length(wd))tempdir()else wd}
  path<-file.path(as.character(app_root[[1L]]),"storyboards")
  if(!dir.exists(path))dir.create(path,recursive=TRUE,showWarnings=FALSE)
  if(!dir.exists(path)){path<-file.path(tempdir(),"Drachuri","storyboards");dir.create(path,recursive=TRUE,showWarnings=FALSE)}
  normalizePath(path,mustWork=FALSE)
}

storyboard_path <- function(storyboard_id,app_root=NULL) file.path(storyboard_root(app_root),story_safe_id(storyboard_id))

storyboard_manifest_path <- function(storyboard_id,app_root=NULL) file.path(storyboard_path(storyboard_id,app_root),"manifest.json")

storyboard_read <- function(storyboard_id,app_root=NULL) {
  path<-storyboard_manifest_path(storyboard_id,app_root);if(!file.exists(path))return(NULL)
  tryCatch(jsonlite::fromJSON(path,simplifyVector=FALSE),error=function(e)NULL)
}

storyboard_write <- function(board,app_root=NULL) {
  board$storyboard_id<-story_safe_id(board$storyboard_id%||%board$title);board$title<-trimws(as.character(board$title%||%"Story"));board$slides<-board$slides%||%list();path<-storyboard_path(board$storyboard_id,app_root);dir.create(file.path(path,"assets"),recursive=TRUE,showWarnings=FALSE);jsonlite::write_json(board,file.path(path,"manifest.json"),auto_unbox=TRUE,pretty=TRUE,null="null");board
}

storyboard_list_local <- function(app_root=NULL) {
  root<-storyboard_root(app_root);dirs<-list.dirs(root,recursive=FALSE,full.names=FALSE);boards<-lapply(dirs,function(id)storyboard_read(id,app_root));boards<-Filter(Negate(is.null),boards);if(!length(boards))return(character());stats::setNames(vapply(boards,function(x)as.character(x$storyboard_id),character(1)),vapply(boards,function(x)as.character(x$title%||%x$storyboard_id),character(1)))
}

storyboard_copy_image <- function(storyboard_id,datapath,filename,app_root=NULL) {
  if(!file.exists(datapath))return("");ext<-tolower(tools::file_ext(filename));if(!ext%in%c("png","jpg","jpeg","gif","webp"))stop("Storyboard pictures must be PNG, JPG, GIF, or WebP files.");assets<-file.path(storyboard_path(storyboard_id,app_root),"assets");dir.create(assets,recursive=TRUE,showWarnings=FALSE);name<-paste0(format(Sys.time(),"%Y%m%d%H%M%S"),"-",sample.int(999999L,1L),".",ext);if(!file.copy(datapath,file.path(assets,name),overwrite=TRUE))stop("Picture could not be copied.");file.path("assets",name)
}

storyboard_import_zip <- function(zip_path,app_root=NULL) {
  listing<-utils::unzip(zip_path,list=TRUE)$Name;if(!length(listing)||!"manifest.json"%in%listing)stop("This bundle has no manifest.json file.");if(any(grepl("(^/|^\\\\|(^|/)\\.\\.(/|$))",listing)))stop("Unsafe paths were found in this storyboard bundle.");tmp<-tempfile("storyboard-import-");dir.create(tmp);on.exit(unlink(tmp,recursive=TRUE,force=TRUE),add=TRUE);utils::unzip(zip_path,exdir=tmp);manifest<-tryCatch(jsonlite::fromJSON(file.path(tmp,"manifest.json"),simplifyVector=FALSE),error=function(e)NULL);if(is.null(manifest)||!length(manifest$slides%||%list()))stop("The storyboard manifest is invalid or has no slides.");manifest$storyboard_id<-story_safe_id(manifest$storyboard_id%||%manifest$title);dest<-storyboard_path(manifest$storyboard_id,app_root);if(dir.exists(dest))unlink(dest,recursive=TRUE,force=TRUE);dir.create(dest,recursive=TRUE);files<-list.files(tmp,recursive=TRUE,all.files=TRUE,no..=TRUE);for(rel in files){src<-file.path(tmp,rel);if(dir.exists(src))next;target<-file.path(dest,rel);dir.create(dirname(target),recursive=TRUE,showWarnings=FALSE);file.copy(src,target,overwrite=TRUE)};storyboard_write(manifest,app_root)
}

get_session_story_state <- function(session_id) {
  sid<-suppressWarnings(as.integer(session_id));if(is.na(sid))return(data.frame());con<-get_db_connection();if(is.null(con))return(data.frame());on.exit(release_db_connection(con),add=TRUE);tryCatch(DBI::dbGetQuery(con,"SELECT * FROM session_story_state WHERE session_id=$1",params=list(sid)),error=function(e)data.frame())
}

set_session_story_state <- function(session_id,storyboard_id,title,current_slide=1L) {
  sid<-suppressWarnings(as.integer(session_id));slide<-max(1L,as.integer(current_slide));if(is.na(sid))return(NULL);con<-get_db_connection();if(is.null(con))return(NULL);on.exit(release_db_connection(con),add=TRUE);tryCatch(DBI::dbGetQuery(con,"INSERT INTO session_story_state(session_id,storyboard_id,title,current_slide) VALUES($1,$2,$3,$4) ON CONFLICT(session_id) DO UPDATE SET storyboard_id=EXCLUDED.storyboard_id,title=EXCLUDED.title,current_slide=EXCLUDED.current_slide,revision=session_story_state.revision+1,updated_at=now() RETURNING *",params=list(sid,story_safe_id(storyboard_id),as.character(title),slide))[1,,drop=FALSE],error=function(e){message("set_session_story_state failed: ",e$message);NULL})
}
