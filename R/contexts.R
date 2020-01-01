make_reading_context <- function(asgt, semester) {
  context <- list(
    type = "class",
    key = asgt$rd_key,
    cal_key = asgt$cal_key,
    cal_id = asgt$cal_id,
    date = asgt$date,
    class_num = asgt$class_num,
    title = asgt$topic
  )
  context
}

make_hw_context <- function(asgt, semester) {
  context <- list(
    type = "homework",
    key = asgt$hw_key,
    cal_key = asgt$cal_key,
    cal_id = asgt$cal_id,
    due_cal_id = asgt$due_cal_id,
    date = asgt$date,
    due_date = asgt$due_date,
    title = asgt$title
  )
  context
}

make_hw_sol_context <- function(asgt, semester) {
  sol <- asgt
  asgt <- get_hw_assignment(sol$hw_key, semester)
  context <- list(
    type = "homework",
    key = asgt$hw_key,
    cal_key = asgt$cal_key,
    cal_id = asgt$cal_id,
    due_cal_id = asgt$due_cal_id,
    date = asgt$date,
    due_date = asgt$due_date,
    sol_title = sol$sol_title,
    title = asgt$title,
    sol_pub_cal_id = sol$sol_pub_cal_id,
    sol_pub_date = sol$sol_pub_date
  )
  context

}

make_lab_context <- function(asgt, semester) {
  context <- list(
    type = "lab",
    key = asgt$lab_key,
    cal_id = asgt$cal_id,
    cal_key = asgt$cal_key,
    date = asgt$date,
    lab_num = asgt$lab_num,
    title = asgt$title,
    report_date = asgt$report_date,
    report_cal_id = asgt$report_cal_id,
    presentation_date = asgt$pres_date,
    pres_cal_id = asgt$pres_cal_id
  )
  context
}

make_lab_sol_context <- function(asgt, semester) {
  context <- list(
    type = "lab",
    key = asgt$lab_key,
    cal_id = asgt$cal_id,
    cal_key = asgt$cal_key,
    date = asgt$date,
    lab_num = asgt$lab_num,
    title = asgt$sol_title,
    sol_pub_date = asgt$sol_pub_date,
    sol_pub_cal_id = asgt$sol_pub_cal_id
  )
  context
}

make_lab_doc_context <- function(asgt, semester) {
  context <- list(
    type = "lab",
    key = asgt$lab_key,
    cal_id = asgt$cal_id,
    cal_key = asgt$cal_key,
    date = asgt$date,
    lab_num = asgt$lab_num,
    lab_item_id = asgt$lab_item_id,
    title = asgt$document_title
  )
  context
}

make_exam_context <- function(asgt, semester) {
  context <- list(
    type = "exam",
    key = asgt$exam_key,
    cal_id = asgt$cal_id,
    cal_key = asgt$cal_key,
    date = asgt$date,
    exam_num = asgt$exam_num,
    title = asgt$exam
  )
  context
}


make_context <- function(asgt, type, semester) {
  dbg_checkpoint("g_context_asgt", asgt)
  dbg_checkpoint("g_context_type", type)
  if (type %in% c("class", "reading")) {
    return(make_reading_context(asgt, semester))
  } else if (type == "lab") {
    return(make_lab_context(asgt, semester))
  } else if (type == "lab doc") {
    return(make_lab_doc_context(asgt, semester))
  } else if (type == "lab solution") {
    return(make_lab_sol_context(asgt, semester))
  } else if (type == "homework") {
    return(make_hw_context(asgt, semester))
  } else if (type == "homework solution") {
    return(make_hw_sol_context(asgt, semester))
  } else if (type == "exam") {
    return(make_exam_context(asgt, semester))
  } else {
    stop("Unknown context type ", type)
  }

}
