### DROPT Function Script ###

# Standalone script for processing DROPT data from one or more rank-order questions.
# Users can source this file directly and call process_dropt_sequence().
#
# Find more detailed documentation and the main DROPT manuscript on the OSF:
# https://osf.io/vaycw/overview?view_only=f7fe74ff872546cf8fc6841c94ed93c4
#
# This script processes DROPT data recorded by the JavaScript. The 
# JavaScript writes all DROPT rank-order tasks into one shared
# embedded-data field, with chronological task ids: DROPT1, DROPT2, DROPT3, etc.
#
# Required Qualtrics Embedded Data fields:
#   DROPT_RankProcess
#   DROPT_TaskMetadata
#   DROPT_TaskCount
#   DROPT_Submitted
#
# Main log entry format:
#   {DROPT1|0|NA|1,2,3,4,5}
#   {DROPT1|423,890|3|1,2,4,3,5}
#
# Entry fields are:
#   {task_id|timing|item_moved|current_order}
#
# Metadata entry format:
#   {DROPT1|QID12|1,2,3,4,5}
#
# The script does not assume five options. The item ids and number of options
# are inferred separately for each respondent and DROPT task from the initial
# recorded order. This also allows a survey to contain multiple DROPT rank-order
# questions, including questions with different numbers of options.
#
# Important for randomized answer options:
#   item_id is the actual Qualtrics choice id captured by JavaScript.
#   item.f is a stable label derived from item_id, e.g., item_id == 4 -> Item_4.
#   item_initial_position records where that item appeared in the respondent's
#   randomized initial order. Thus, if item_id 5 appeared first, item_initial_position
#   is 1 and item.f is Item_5.
#
# Output:
#   DROPT.df: one row per respondent x DROPT task x item
#   Columns include task_id, question_id, item_id, item.f, item_initial_position,
#   n_options, drag_count, order, drag_time_ms, distance, and distance.r.

library(tidyverse)

### Optional: read data exported from Qualtrics ###

read_dropt_csv <- function(path, remove_qualtrics_header_rows = TRUE) {
  dat <- read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)

  # Qualtrics .csv exports often include two non-data rows after the column names.
  # Set remove_qualtrics_header_rows = FALSE if your file has already been cleaned.
  if (remove_qualtrics_header_rows && nrow(dat) >= 3) {
    dat <- dat %>% dplyr::slice(3:n())
  }

  dat
}

### Helper functions ###

split_order_vec <- function(x) {
  x <- stringr::str_trim(as.character(x))
  x[x == ""] <- NA_character_
  stringr::str_split(x, "\\s*,\\s*")
}

valid_order_vector <- function(x) {
  if (is.null(x)) return(FALSE)
  x <- as.character(x)
  if (length(x) < 2) return(FALSE)
  if (any(is.na(x)) || any(x == "")) return(FALSE)
  length(unique(x)) == length(x)
}

same_item_set <- function(x, y) {
  x <- as.character(x)
  y <- as.character(y)
  length(x) == length(y) && setequal(x, y)
}

parse_timing_start <- function(timing) {
  timing <- stringr::str_trim(as.character(timing))
  parts <- stringr::str_split_fixed(timing, "\\s*,\\s*", 2)
  out <- parts[, 1]
  out[timing == "0" | out == ""] <- NA_character_
  suppressWarnings(as.numeric(out))
}

parse_timing_end <- function(timing) {
  timing <- stringr::str_trim(as.character(timing))
  parts <- stringr::str_split_fixed(timing, "\\s*,\\s*", 2)
  out <- parts[, 2]
  out[timing == "0" | out == ""] <- NA_character_
  suppressWarnings(as.numeric(out))
}

parse_task_number <- function(task_id) {
  suppressWarnings(as.integer(stringr::str_extract(as.character(task_id), "\\d+")))
}

sort_item_id_numeric_first <- function(item_id) {
  suppressWarnings(as.numeric(as.character(item_id)))
}

### Parse the cumulative DROPT process log ###

parse_dropt_log <- function(data,
                            log_col = "DROPT_RankProcess",
                            response_id_col = "ResponseId",
                            malformed_action = c("drop_task", "error"),
                            verbose = TRUE) {
  malformed_action <- match.arg(malformed_action)

  if (!response_id_col %in% names(data)) {
    stop("Could not find response id column: ", response_id_col)
  }
  if (!log_col %in% names(data)) {
    stop("Could not find DROPT log column: ", log_col)
  }

  parsed <- data %>%
    dplyr::transmute(
      ResponseId = .data[[response_id_col]],
      raw_log_full = as.character(.data[[log_col]])
    ) %>%
    dplyr::filter(!is.na(raw_log_full), stringr::str_trim(raw_log_full) != "") %>%
    # This keeps the original script's simple braced-entry format but avoids
    # assuming a fixed number of options or a fixed number of DROPT questions.
    tidyr::separate_rows(raw_log_full, sep = "\\}") %>%
    dplyr::mutate(
      raw_log = stringr::str_remove_all(raw_log_full, "[{}]"),
      raw_log = stringr::str_trim(raw_log)
    ) %>%
    dplyr::filter(raw_log != "") %>%
    dplyr::group_by(ResponseId) %>%
    dplyr::mutate(entry_index = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(parts = stringr::str_split(raw_log, "\\|", simplify = FALSE)) %>%
    dplyr::mutate(
      n_parts = purrr::map_int(parts, length),
      task_id = purrr::map_chr(parts, ~ if (length(.x) >= 1) stringr::str_trim(.x[[1]]) else NA_character_),
      timing = purrr::map_chr(parts, ~ if (length(.x) >= 2) stringr::str_trim(.x[[2]]) else NA_character_),
      item_moved = purrr::map_chr(parts, ~ if (length(.x) >= 3) stringr::str_trim(.x[[3]]) else NA_character_),
      order = purrr::map_chr(parts, ~ if (length(.x) >= 4) stringr::str_trim(paste(.x[4:length(.x)], collapse = "|")) else NA_character_)
    ) %>%
    dplyr::select(-parts, -raw_log_full) %>%
    dplyr::mutate(
      task_id = stringr::str_trim(task_id),
      task_num = parse_task_number(task_id),
      timing = stringr::str_trim(timing),
      item_moved = stringr::str_trim(item_moved),
      item_moved = dplyr::na_if(item_moved, "NA"),
      item_moved = dplyr::na_if(item_moved, ""),
      order = stringr::str_trim(order),
      order_clean = stringr::str_replace_all(order, "\\s+", ""),
      order_list = split_order_vec(order),
      valid_entry_format = !is.na(task_id) & task_id != "" & n_parts >= 4,
      valid_order = purrr::map_lgl(order_list, valid_order_vector),
      drag_start = parse_timing_start(timing),
      drag_end = parse_timing_end(timing),
      valid_timing = timing == "0" | (!is.na(drag_start) & !is.na(drag_end) & drag_end >= drag_start)
    ) %>%
    dplyr::arrange(ResponseId, task_num, task_id, entry_index)

  if (nrow(parsed) == 0) {
    empty_diag <- tibble::tibble(
      ResponseId = character(),
      task_id = character(),
      problem = character(),
      n_bad_entries = integer()
    )
    attr(parsed, "malformed_entries") <- empty_diag
    attr(parsed, "malformed_tasks") <- empty_diag
    return(parsed)
  }

  parsed <- parsed %>%
    dplyr::group_by(ResponseId, task_id) %>%
    dplyr::mutate(
      # Store the initial item set as a list-column so it is repeated once per
      # row in the respondent-task group. This is deliberately separate from
      # item order: randomized display order is preserved in step 0, but later
      # process entries should contain the same set of items.
      initial_order_list = list(order_list[[1]]),
      order_matches_initial_item_set = purrr::map2_lgl(order_list, initial_order_list, same_item_set),
      moved_item_valid = purrr::map2_lgl(
        item_moved,
        initial_order_list,
        ~ if (is.na(.x)) FALSE else .x %in% .y
      ),
      moved_item_valid = dplyr::if_else(timing == "0" & is.na(item_moved), TRUE, moved_item_valid),
      valid_entry = valid_entry_format & valid_order & valid_timing &
        order_matches_initial_item_set & moved_item_valid
    ) %>%
    dplyr::ungroup()

  malformed_entries <- parsed %>%
    dplyr::filter(!valid_entry) %>%
    dplyr::mutate(problem = dplyr::case_when(
      !valid_entry_format ~ "entry does not have four fields: task_id|timing|item_moved|order",
      !valid_order ~ "order is missing, duplicated, or has fewer than two items",
      !valid_timing ~ "timing is malformed or drag_end is earlier than drag_start",
      !order_matches_initial_item_set ~ "item set changed within the same respondent-task",
      !moved_item_valid ~ "moved item is missing or not present in the task's initial item set",
      TRUE ~ "unknown parsing problem"
    )) %>%
    dplyr::select(ResponseId, task_id, task_num, entry_index, raw_log, problem)

  malformed_tasks <- malformed_entries %>%
    dplyr::count(ResponseId, task_id, problem, name = "n_bad_entries")

  if (nrow(malformed_tasks) > 0) {
    msg <- paste0(
      "Found ", nrow(malformed_tasks),
      " respondent-task/problem combination(s) with malformed DROPT logs."
    )

    if (malformed_action == "error") {
      print(utils::head(malformed_entries, 20))
      stop(msg, " Set malformed_action = 'drop_task' to drop affected respondent-task logs.")
    }

    if (verbose) {
      warning(msg, " Affected respondent-task logs were dropped. Use return_intermediate = TRUE to inspect DroppedTasks.df and MalformedEntries.df.", call. = FALSE)
    }
  }

  bad_response_tasks <- malformed_entries %>%
    dplyr::distinct(ResponseId, task_id)

  parsed_clean <- parsed %>%
    dplyr::anti_join(bad_response_tasks, by = c("ResponseId", "task_id")) %>%
    dplyr::arrange(ResponseId, task_num, task_id, entry_index) %>%
    dplyr::group_by(ResponseId, task_id) %>%
    dplyr::mutate(
      previous_order_clean = dplyr::lag(order_clean),
      order_changed_from_previous = is.na(previous_order_clean) | order_clean != previous_order_clean
    ) %>%
    # Keep the first state for each task, then keep only real order changes.
    # This protects against accidental duplicate initial states and no-change
    # touches if they appear in test logs.
    dplyr::filter(dplyr::row_number() == 1L | order_changed_from_previous) %>%
    dplyr::mutate(step = dplyr::row_number() - 1L) %>%
    dplyr::ungroup() %>%
    dplyr::select(ResponseId, task_id, task_num, step, entry_index, timing,
                  drag_start, drag_end, item_moved, order, order_clean,
                  order_list, valid_entry, dplyr::everything(),
                  -previous_order_clean, -order_changed_from_previous,
                  -valid_entry_format, -valid_order, -valid_timing,
                  -order_matches_initial_item_set, -moved_item_valid,
                  -initial_order_list)

  attr(parsed_clean, "malformed_entries") <- malformed_entries
  attr(parsed_clean, "malformed_tasks") <- malformed_tasks

  parsed_clean
}

### Parse task metadata ###

parse_dropt_metadata <- function(data,
                                 metadata_col = "DROPT_TaskMetadata",
                                 response_id_col = "ResponseId") {
  if (!response_id_col %in% names(data) || !metadata_col %in% names(data)) {
    return(tibble::tibble(
      ResponseId = character(),
      task_id = character(),
      task_num = integer(),
      question_id = character(),
      metadata_item_ids = character()
    ))
  }

  data %>%
    dplyr::transmute(
      ResponseId = .data[[response_id_col]],
      raw_metadata = as.character(.data[[metadata_col]])
    ) %>%
    dplyr::filter(!is.na(raw_metadata), stringr::str_trim(raw_metadata) != "") %>%
    tidyr::separate_rows(raw_metadata, sep = "\\}") %>%
    dplyr::mutate(
      raw_metadata = stringr::str_remove_all(raw_metadata, "[{}]"),
      raw_metadata = stringr::str_trim(raw_metadata)
    ) %>%
    dplyr::filter(raw_metadata != "") %>%
    tidyr::separate(raw_metadata,
                    into = c("task_id", "question_id", "metadata_item_ids"),
                    sep = "\\|",
                    fill = "right",
                    extra = "merge") %>%
    dplyr::mutate(
      dplyr::across(c(task_id, question_id, metadata_item_ids), stringr::str_trim),
      task_num = parse_task_number(task_id)
    ) %>%
    dplyr::arrange(ResponseId, task_num, task_id) %>%
    dplyr::distinct(ResponseId, task_id, .keep_all = TRUE)
}

### Build a respondent x task x item grid ###

make_item_grid <- function(process_log) {
  process_log %>%
    dplyr::filter(step == 0) %>%
    dplyr::group_by(ResponseId, task_id) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(ResponseId, task_id, task_num, order_list) %>%
    tidyr::unnest_longer(order_list, values_to = "item_id") %>%
    dplyr::mutate(
      item_id = as.character(item_id),
      item_sort_numeric = sort_item_id_numeric_first(item_id)
    ) %>%
    dplyr::group_by(ResponseId, task_id) %>%
    dplyr::mutate(
      # Displayed starting position for this respondent and task. This is the
      # randomized initial position if Qualtrics randomizes answer options.
      # Example: if the initial order is 5,1,2, then item_id 5 gets
      # item_initial_position = 1.
      item_initial_position = dplyr::row_number(),
      n_options = dplyr::n(),

      # Stable item label: this tracks the actual Qualtrics item id, not the
      # randomized display position. Example: item_id 5 is Item_5 even if it
      # was displayed first.
      item.f = paste0("Item_", item_id)
    ) %>%
    dplyr::ungroup()
}

### Compute DROPT measures ###

compute_drag_count <- function(process_log, item_grid) {
  counts <- process_log %>%
    dplyr::filter(step != 0, !is.na(item_moved)) %>%
    dplyr::count(ResponseId, task_id, item_id = item_moved, name = "drag_count")

  item_grid %>%
    dplyr::left_join(counts, by = c("ResponseId", "task_id", "item_id")) %>%
    dplyr::mutate(drag_count = tidyr::replace_na(drag_count, 0L)) %>%
    dplyr::select(ResponseId, task_id, item_id, item.f, item_initial_position,
                  n_options, drag_count, item_sort_numeric)
}

compute_drag_order <- function(process_log,
                               item_grid,
                               untouched_order = c("max_observed_plus_one", "max_possible", "NA")) {
  untouched_order <- match.arg(untouched_order)

  first_touches <- process_log %>%
    dplyr::filter(step != 0, !is.na(item_moved)) %>%
    dplyr::arrange(ResponseId, task_num, task_id, step) %>%
    dplyr::group_by(ResponseId, task_id, item_id = item_moved) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(ResponseId, task_id) %>%
    dplyr::arrange(step, .by_group = TRUE) %>%
    dplyr::mutate(order = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::select(ResponseId, task_id, item_id, order)

  max_touches <- first_touches %>%
    dplyr::group_by(ResponseId, task_id) %>%
    dplyr::summarise(max_order = max(order, na.rm = TRUE), .groups = "drop")

  out <- item_grid %>%
    dplyr::left_join(first_touches, by = c("ResponseId", "task_id", "item_id")) %>%
    dplyr::left_join(max_touches, by = c("ResponseId", "task_id")) %>%
    dplyr::mutate(max_order = tidyr::replace_na(max_order, 0L))

  if (untouched_order == "max_observed_plus_one") {
    out <- out %>%
      dplyr::mutate(order = dplyr::if_else(is.na(order), max_order + 1L, order))
  } else if (untouched_order == "max_possible") {
    out <- out %>%
      dplyr::mutate(order = dplyr::if_else(is.na(order), as.integer(n_options), order))
  } else {
    out <- out %>%
      dplyr::mutate(order = as.integer(order))
  }

  out %>%
    dplyr::select(ResponseId, task_id, item_id, order)
}

compute_drag_distance <- function(process_log, item_grid) {
  positions <- process_log %>%
    dplyr::select(ResponseId, task_id, task_num, step, drag_start, drag_end,
                  item_moved, order_list) %>%
    tidyr::unnest_longer(order_list, values_to = "item_id") %>%
    dplyr::mutate(item_id = as.character(item_id)) %>%
    dplyr::group_by(ResponseId, task_id, step) %>%
    dplyr::mutate(current_position = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(ResponseId, task_num, task_id, item_id, step) %>%
    dplyr::group_by(ResponseId, task_id, item_id) %>%
    dplyr::mutate(last_position = dplyr::lag(current_position)) %>%
    dplyr::ungroup()

  first_distances <- positions %>%
    dplyr::filter(step != 0, !is.na(item_moved), item_id == item_moved) %>%
    dplyr::mutate(
      distance = current_position - last_position,
      distance.r = -1 * distance,
      drag_time_ms = drag_end - drag_start
    ) %>%
    dplyr::arrange(ResponseId, task_num, task_id, step) %>%
    dplyr::group_by(ResponseId, task_id, item_id) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(ResponseId, task_id, item_id, drag_time_ms, distance, distance.r)

  item_grid %>%
    dplyr::left_join(first_distances, by = c("ResponseId", "task_id", "item_id")) %>%
    dplyr::mutate(
      distance = tidyr::replace_na(distance, 0L),
      distance.r = tidyr::replace_na(distance.r, 0L)
    ) %>%
    dplyr::select(ResponseId, task_id, item_id, drag_time_ms, distance, distance.r)
}

### Main function ###

process_dropt_sequence <- function(data,
                                   response_id_col = "ResponseId",
                                   process_col = "DROPT_RankProcess",
                                   metadata_col = "DROPT_TaskMetadata",
                                   untouched_order = c("max_observed_plus_one", "max_possible", "NA"),
                                   malformed_action = c("drop_task", "error"),
                                   return_intermediate = FALSE,
                                   verbose = TRUE) {
  untouched_order <- match.arg(untouched_order)
  malformed_action <- match.arg(malformed_action)

  process_log <- parse_dropt_log(
    data = data,
    log_col = process_col,
    response_id_col = response_id_col,
    malformed_action = malformed_action,
    verbose = verbose
  )

  malformed_entries <- attr(process_log, "malformed_entries")
  malformed_tasks <- attr(process_log, "malformed_tasks")

  metadata <- parse_dropt_metadata(
    data = data,
    metadata_col = metadata_col,
    response_id_col = response_id_col
  )

  item_grid <- make_item_grid(process_log)

  Count.df <- compute_drag_count(process_log, item_grid)
  Order.df <- compute_drag_order(process_log, item_grid, untouched_order = untouched_order)
  Distance.df <- compute_drag_distance(process_log, item_grid)

  DROPT.df <- Count.df %>%
    dplyr::left_join(Order.df, by = c("ResponseId", "task_id", "item_id")) %>%
    dplyr::left_join(Distance.df, by = c("ResponseId", "task_id", "item_id")) %>%
    dplyr::left_join(
      metadata %>% dplyr::select(ResponseId, task_id, question_id),
      by = c("ResponseId", "task_id")
    ) %>%
    dplyr::mutate(
      task_num = parse_task_number(task_id),
      item_sort_numeric = sort_item_id_numeric_first(item_id)
    ) %>%
    dplyr::select(ResponseId, task_id, question_id, item_id, item.f,
                  item_initial_position, n_options, drag_count, order,
                  drag_time_ms, distance, distance.r,
                  task_num, item_sort_numeric) %>%
    # Sort by actual item id within each respondent-task, not by randomized
    # initial display position. This gives Item_1, Item_2, Item_3, etc.
    dplyr::arrange(ResponseId, task_num, task_id,
                   is.na(item_sort_numeric), item_sort_numeric, item_id) %>%
    dplyr::select(-task_num, -item_sort_numeric)

  if (return_intermediate) {
    return(list(
      DROPT.df = DROPT.df,
      RankProcess.df = process_log,
      TaskMetadata.df = metadata,
      ItemGrid.df = item_grid,
      Count.df = Count.df,
      Order.df = Order.df,
      Distance.df = Distance.df,
      MalformedEntries.df = malformed_entries,
      DroppedTasks.df = malformed_tasks
    ))
  }

  DROPT.df
}

### Example use ###

# data <- read_dropt_csv("mock_data.csv")
# DROPT.df <- process_dropt_sequence(data)
#
# If your Qualtrics export has already had the first two Qualtrics metadata rows
# removed, use:
# data <- read_dropt_csv("mock_data.csv", remove_qualtrics_header_rows = FALSE)
#
# If you prefer untouched items to receive the maximum possible order instead of
# the maximum observed order plus one, use:
# DROPT.df <- process_dropt_sequence(data, untouched_order = "max_possible")
#
# If you prefer untouched items to be coded as missing on the order measure, use:
# DROPT.df <- process_dropt_sequence(data, untouched_order = "NA")
#
# If you want malformed respondent-task logs to stop the script rather than be
# dropped with a warning, use:
# DROPT.df <- process_dropt_sequence(data, malformed_action = "error")
#
# To inspect intermediate parsed objects and malformed logs:
# dropt_objects <- process_dropt_sequence(data, return_intermediate = TRUE)
# DROPT.df <- dropt_objects$DROPT.df
# RankProcess.df <- dropt_objects$RankProcess.df
# DroppedTasks.df <- dropt_objects$DroppedTasks.df
# MalformedEntries.df <- dropt_objects$MalformedEntries.df