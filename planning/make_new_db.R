library(pacman)
p_load(knitr)
p_load(RSQLite)
p_load(tidyverse, magrittr, lubridate, rlang)
p_load(xtable)
p_load(assertthat)
p_load(yaml)
p_load(here)
p_load(blogdown)
p_load_current_gh("jonathan-g/blogdownDigest")

root_dir <- here::here()
planning_dir <- here::here("planning")

database <- "new_EES_3310_5310.sqlite3"

csv_dir <- file.path(planning_dir, "db_save", "new")

calendar <- read_csv(file.path(csv_dir, "calendar.csv"), col_types = "iiic",
                     na = c("", "NA", "N/A", "na", "n/a")) %>%
  mutate(date = mdy(date))
events <- read_csv(file.path(csv_dir, "events.csv"), col_types = "ic",
                   na = c("", "NA", "N/A", "na", "n/a"))
exams <- read_csv(file.path(csv_dir, "exams.csv"), col_types = "icc",
                  na = c("", "NA", "N/A", "na", "n/a"))
holidays <- read_csv(file.path(csv_dir, "holidays.csv"), col_types = "ic",
                     na = c("", "NA", "N/A", "na", "n/a"))
notices <- read_csv(file.path(csv_dir, "notices.csv"), col_types = "iccc",
                    na = c("", "NA", "N/A", "na", "n/a"))
text_codes <- read_csv(file.path(csv_dir, "text_codes.csv"), col_types = "cccl",
                       na = c("", "NA", "N/A", "na", "n/a"))

homework_assignments <- read_csv(file.path(csv_dir, "homework_assignments.csv"),
                                 col_types = "icccclccc",
                                 na = c("", "NA", "N/A", "na", "n/a")) %>%
  mutate(hw_due_date = mdy(hw_due_date))
homework_groups <- read_csv(file.path(csv_dir, "homework_groups.csv"),
                            col_types = "iic",
                            na = c("", "NA", "N/A", "na", "n/a"))
homework_items <- read_csv(file.path(csv_dir, "homework_items.csv"),
                           col_types = "icccclllll",
                           na = c("", "NA", "N/A", "na", "n/a"))
homework_solutions <- read_csv(file.path(csv_dir, "homework_solutions.csv"),
                               col_types = "icccccc",
                               na = c("", "NA", "N/A", "na", "n/a")) %>%
  mutate(hw_sol_pub_date = ymd(hw_sol_pub_date))
homework_topics <- read_csv(file.path(csv_dir, "homework_topics.csv"),
                            col_types = "cc",
                            na = c("", "NA", "N/A", "na", "n/a"))

lab_assignments <- read_csv(file.path(csv_dir, "lab_assignments.csv"),
                            col_types = "icccccc",
                            na = c("", "NA", "N/A", "na", "n/a")) %>%
  mutate(report_due_date = ymd(report_due_date),
         presentation_date = ymd(presentation_date))
lab_groups <- read_csv(file.path(csv_dir, "lab_groups.csv"),
                       col_types = "iic",
                       na = c("", "NA", "N/A", "na", "n/a"))
lab_items <- read_csv(file.path(csv_dir, "lab_items.csv"),
                      col_types = "iccccccc",
                      na = c("", "NA", "N/A", "na", "n/a"))
lab_solutions <- read_csv(file.path(csv_dir, "lab_solutions.csv"),
                          col_types = "iccccccc",
                          na = c("", "NA", "N/A", "na", "n/a")) %>%
  mutate(lab_sol_pub_date = ymd(lab_sol_pub_date))

reading_items <- read_csv(file.path(csv_dir, "reading_items.csv"),
                          col_types = "iccccccllllll",
                          na = c("", "NA", "N/A", "na", "n/a"))
reading_sources <- read_csv(file.path(csv_dir, "reading_sources.csv"),
                            col_types = "ccccccccllc",
                            na = c("", "NA", "N/A", "na", "n/a"))

link_reading_group <- read_csv(file.path(csv_dir, "link_reading_group.csv"),
                         col_types = "ic",
                         na = c("", "NA", "N/A", "na", "n/a"))

link_cal_class <- read_csv(file.path(csv_dir, "link_cal_class.csv"),
                           col_types = "ii",
                           na = c("", "NA", "N/A", "na", "n/a"))
link_cal_event <- read_csv(file.path(csv_dir, "link_cal_event.csv"),
                           col_types = "ii",
                           na = c("", "NA", "N/A", "na", "n/a"))
link_cal_exam <- read_csv(file.path(csv_dir, "link_cal_exam.csv"),
                          col_types = "ii",
                          na = c("", "NA", "N/A", "na", "n/a"))
link_cal_holiday <- read_csv(file.path(csv_dir, "link_cal_holiday.csv"),
                             col_types = "ii",
                             na = c("", "NA", "N/A", "na", "n/a"))
link_cal_hw <- read_csv(file.path(csv_dir, "link_cal_hw.csv"),
                        col_types = "ii",
                        na = c("", "NA", "N/A", "na", "n/a"))
link_cal_lab <- read_csv(file.path(csv_dir, "link_cal_lab.csv"),
                         col_types = "ii",
                         na = c("", "NA", "N/A", "na", "n/a"))

link_class_reading <- read_csv(file.path(csv_dir, "link_class_reading.csv"),
                               col_types = "ii",
                               na = c("", "NA", "N/A", "na", "n/a"))

class_map <- c(integer = "INTEGER",
               numeric = "REAL",
               character = "TEXT",
               Date = "TEXT",
               Time = "TEXT",
               POSIXct = "TEXT",
               POSIXt = "TEXT",
               logical = "INTEGER")

db <- dbConnect(RSQLite::SQLite(), file.path(planning_dir, database), create = TRUE)

for(t in c("calendar", "events", "exams", "holidays",
           "homework_assignments", "homework_groups", "homework_items",
           "homework_solutions", "homework_topics", "lab_assignments",
           "lab_groups", "lab_items", "lab_solutions", "link_cal_class",
           "link_cal_event", "link_cal_exam", "link_cal_holiday",
           "link_cal_hw", "link_cal_lab", "link_class_reading",
           "link_reading_group", "notices", "reading_items",
           "reading_sources","text_codes")) {
  df <- get(t)
  classes <- map_chr(df, ~class(.x)[1])
  type = class_map[classes] %>% unname()
  idx <- list(names(df)[1])
  df <- df %>% mutate_if(is.Date, as.character)
  copy_to(db, df, name = t, overwrite = TRUE, types = type,
          temporary = FALSE, indexes = idx)
}

dbDisconnect(db)
gc()
