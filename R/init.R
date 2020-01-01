library(magrittr)
library(rlang)

master_db_file <- "planning/ees_3310/alt_semester.sqlite3"
base_db_file <- "planning/ees_3310/new_semester.sqlite3"
dest_db_file <- "planning/ees_3310/dest_semester.sqlite3"

db_file <- "planning/ees_3310/dest_semester.sqlite3"

r_dir <- rprojroot::find_root("DESCRIPTION") %>% file.path("R")

semestr.debug <- TRUE

source(file.path(r_dir, "assignment_utils.R"), chdir = TRUE)
source(file.path(r_dir, "name_conversions.R"), chdir = TRUE)
source(file.path(r_dir, "key_prefixes.R"), chdir = TRUE)
source(file.path(r_dir, "date_manipulation.R"), chdir = TRUE)
source(file.path(r_dir, "text_formatting.R"), chdir = TRUE)
source(file.path(r_dir, "contexts.R"), chdir = TRUE)
source(file.path(r_dir, "code_expansion.R"), chdir = TRUE)
source(file.path(r_dir, "load_semester_db.R"), chdir = TRUE)
source(file.path(r_dir, "format_notice.R"), chdir = TRUE)
source(file.path(r_dir, "format_reading.R"), chdir = TRUE)
source(file.path(r_dir, "format_homework.R"), chdir = TRUE)
source(file.path(r_dir, "format_lab.R"), chdir = TRUE)
source(file.path(r_dir, "generate_assignments.R"), chdir = TRUE)

