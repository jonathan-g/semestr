semestr.env <- new.env()

with( semestr.env, {
  prefixes <- c(class = "CLS", lab = "LAB", homework = "HW", due_date = "DUE",
                exam = "EXAM", holiday = "VAc", event = "EVT")
  bases <- c(class = 1000, lab = 2000, homework = 3000, due_date = 4000,
             exam = 5000,holiday = 6000, event = 7000)
  base_mods <- c(cancelled = 100, make_up = 200), envir = semestr.env)
})

build_base_calendar <- function(master_cal) {

}

transition_database <- function(dest_db = "semester.sqlite3",
                                master_db = "alt_semester.sqlite3",
                                base_db = "new_semester.sqlite3") {
  base_dir <- rprojroot::find_root(rprojroot::has_file("DESCRIPTION"))
  planning_dir <- file.path(base_dir, "planning")






}


