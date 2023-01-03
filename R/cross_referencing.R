#' Look Up Past and Future Classes
#'
#' Look up previous or future classes, based on the current class's id.
#'
#' @param calendar The calendar to use for looking up the classes.
#' @param id The `cal_id` index of the current class.
#' @param delta The number of classes forward or backward
#'
#' @return A row from the calendar table
#'
#' @name lookup_past_future_classes
NULL

#' @describeIn lookup_past_future_classes
#'
#' @export
lookup_future_class <- function(calendar, id, delta = 1) {
  this_class <- dplyr::filter(calendar, .data$class_id == id)
  if (nrow(this_class) == 1) {
    target <- this_class$class_num + delta
    that_class <- dplyr::filter(calendar, .data$class_num == target)
    if (nrow(that_class) == 1) {
      return(that_class)
    } else if (nrow(that_class == 0)) {
      warning("Class ", id, " (#", this_class$class_num, ") ",
              "tried to look up a nonexistant class ",
              delta, " sessions in the future")
      return(NULL)
    } else if (nrow(that_class > 1)) {
      warning("Class ", id, " (#", this_class$class_num, ") ",
              "looked up a class ",
              delta, " sessions in the future and got ",
              nrow(that_class), " results.")
      return(NULL)
    }
  } else if (nrow(this_class) == 0) {
    warning("Class ", id,  "(#", this_class$class_num, ") ",
            "couldn't find itself.")
    return(NULL)
  } else if (nrow(this_class) > 1) {
    warning("Class ", id,  "(#", this_class$class_num, ") ",
            " looked itself up and got ",
            nrow(this_class), " results.")
    return(NULL)
  }
}

#' @describeIn lookup_past_future_classes
#'
#' @export
lookup_past_class <- function(calendar, id, delta = 1) {
  this_class <- dplyr::filter(calendar, .data$class_id == id)
  if (nrow(this_class) == 1) {
    target <- this_class$class_num - delta
    that_class <- dplyr::filter(calendar, .data$class_num == target)
    if (nrow(that_class) == 1) {
      return(that_class)
    } else if (nrow(that_class == 0)) {
      warning("Class ", id, " (#", this_class$class_num, ") ",
              "tried to look up a nonexistant class ",
              delta, " sessions in the past")
      return(NULL)
    } else if (nrow(that_class > 1)) {
      warning("Class ", id, " (#", this_class$class_num, ") ",
              "looked up a class ",
              delta, " sessions in the past and got ",
              nrow(that_class), " results.")
      return(NULL)
    }
  } else if (nrow(this_class) == 0) {
    warning("Class ", id,  "(#", this_class$class_num, ") ",
            "couldn't find itself.")
    return(NULL)
  } else if (nrow(this_class) > 1) {
    warning("Class ", id,  "(#", this_class$class_num, ") ",
            " looked itself up and got ",
            nrow(this_class), " results.")
    return(NULL)
  }
}

#' Look Up Past and Future Reading Assignments
#'
#' Look up previous or future assignments, based on the current class's id
#'
#' @param schedule The schedule to use for looking up the classes.
#' @param id The `cal_id` index of the current class.
#' @param grp The `rd_grp_key` for the current class's reading group.
#' @param delta The number of classes forward or backward
#'
#' @return A row from the calendar table
#'
#' @name lookup_past_future_reading
NULL

#' @describeIn lookup_past_future_reading
#'
#' @export
lookup_future_reading <- function(schedule, id = NULL, grp = NULL, delta = 1) {
  sched <- schedule %>% dplyr::filter(!is.na(.data$key_rd)) %>%
    dplyr::arrange("class_num", "id_class")
  if (is.null(id)) {
    if (is.null(grp)) {
      stop("lookup_future_reading() must specify id or grp")
    }
    this_idx <- which(sched$key_rd == grp)
    id <- sched$id_class[this_idx]
  } else {
    this_idx <- which(sched$id_class == id)
    if (is.null(grp)) {
      grp <- sched$key_rd[this_idx]
    }
  }
  if (length(this_idx) == 1) {
    target <- this_idx + delta
    if (dplyr::between(target, 1, nrow(sched))) {
      return(sched[target,])
    } else {
      warning("Class ", id, " (reading ", grp, ") ",
              "tried to look up a nonexistant reading assignment ",
              delta, " sessions in the future")
      return(NULL)
    }
  } else if (length(this_idx) == 0) {
    warning("Class ", id,  "(reading ", grp, ") ",
            "couldn't find its reading assignment.")
    return(NULL)
  } else if (length(this_idx) > 1) {
    warning("Class ", id,  "(reading ", grp, ") ",
            " looked its reading assignments up and got ",
            length(this_idx), " results.")
    return(NULL)
  }
}

#' @describeIn lookup_past_future_reading
#'
#' @export
lookup_past_reading <- function(schedule, id = NULL, grp = NULL, delta = 1) {
  sched <- schedule %>% dplyr::filter(!is.na(.data$key_rd)) %>%
    dplyr::arrange("class_num", "id_class")
  if (is.null(id)) {
    if (is.null(grp)) {
      stop("lookup_past_reading() must specify id or grp")
    }
    this_idx <- which(sched$key_rd == grp)
    id <- sched$id_class[this_idx]
  } else {
    this_idx <- which(sched$id_class == id)
    if (is.null(grp)) {
      grp <- sched$key_rd[this_idx]
    }
  }
  if (length(this_idx) == 1) {
    target <- this_idx + delta
    if (dplyr::between(target, 1, nrow(sched))) {
      return(sched[target,])
    } else {
      warning("Class ", id, " (reading ", grp, ") ",
              "tried to look up a nonexistant reading assignment ",
              delta, " sessions in the past")
      return(NULL)
    }
  } else if (length(this_idx) == 0) {
    warning("Class ", id,  "(reading ", grp, ") ",
            "couldn't find its reading assignment.")
    return(NULL)
  } else if (length(this_idx) > 1) {
    warning("Class ", id,  "(reading ", grp, ") ",
            " looked its reading assignments up and got ",
            length(this_idx), " results.")
    return(NULL)
  }
}
