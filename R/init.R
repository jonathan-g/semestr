library(magrittr)
library(rlang)

master_db_file <- "planning/ees_3310/alt_semester.sqlite3"
base_db_file <- "planning/ees_3310/new_semester.sqlite3"
dest_db_file <- "planning/ees_3310/dest_semester.sqlite3"

db_file <- "planning/ees_3310/dest_semester.sqlite3"

r_dir <- rprojroot::find_root("DESCRIPTION") %>% file.path("R")

source(file.path(r_dir, "assignment_utils.R"), chdir = TRUE)
source(file.path(r_dir, "load_semester_db.R"), chdir = TRUE)
source(file.path(r_dir, "format_reading.R"), chdir = TRUE)

