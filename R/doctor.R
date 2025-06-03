
#' Documents R Code
#'
#' @param file Path to the input code file
#' @param level Documentation level of detail. Defaults to light. Light: Provides a description of functions and pipes. Heavy: All lines are documented (not very useful).
#'
#' @return A new R file is created adding a documentation to the code. The file ends with "_doctord" and is saved in the same directory as the input file.
#' @export
#'

doctor <- function(file, level = "light") {

#  llm_use("ollama", "llama3.2:3b", seed = 100, temperature = 0)

  doc_test <- data.frame(code = readLines(file, warn = FALSE))

  tot_char <- sum(nchar(doc_test$code))
  tot_row <- sum(nrow(doc_test))

  if (tot_char > 5000) {
    response <- readline(prompt = paste0(
      "This script includes ", tot_char,
      " characters over ", tot_row,
      " lines of code. It might take a few minutes to process it,
      depending on the model chosen. Do you want to proceed? (yes/no): "))
    if (tolower(response) != "yes") {
      message("Operation cancelled.")
      return(NULL)
    }
  }

# SUMMARY function at the top of the doc
# OUTPUT: doc_test_sum

if (tot_char < 5000) {

  # < 5,000 characters of script is small enough to be read by the model entirely
    doc_test_sum <- doc_test |>
      summarise(code = paste(code, collapse = " \n "))

    prompt50 <- paste(
      "This text contains R code. Describe to a technical audience what the code does in 50 words."
    )

   doc_test_sum <- llm_custom(doc_test_sum, code, prompt50, pred_name = "Comment") |>
      mutate(code = "") |>
      mutate(Comment = paste0("# ", Comment)) |>
      #  add_row(code = "", Comment = "") |>
      mutate(Comment1 = word(Comment, 1, 10)) |>
      mutate(Comment2 = paste0("# ", word(Comment, 11, 20))) |>
      mutate(Comment3 = if_else(str_count(Comment, '\\w+') <= 30,
                                paste0("# ", word(Comment, 21, str_count(Comment, '\\w+'))),
                                paste0("# ", word(Comment, 21, 30)))) |>
      mutate(Comment4 = if_else(str_count(Comment, '\\w+') <= 40,
                                paste0("# ", word(Comment, 31, str_count(Comment, '\\w+'))),
                                paste0("# ", word(Comment, 31, 40)))) |>
      mutate(Comment5 = if_else(str_count(Comment, '\\w+') <= 50,
                                paste0("# ", word(Comment, 41, str_count(Comment, '\\w+'))),
                                paste0("# ", word(Comment, 41, 50)))) |>
      select(code, Comment1:Comment5) |>
      pivot_longer(Comment1:Comment5, names_to = "Comment") |>
      select(code, value) |>
      rename(Comment = value) |>
      add_row(code = "", Comment = "")

  } else {

      # For bigger scripts
    # We want to create groupings of code for the LLM to review and comment

    doc_test_sum <- doc_test |>
      mutate(len = str_length(code)) |>
      mutate(cnt = cumsum(len)) |>
      mutate(brk = trunc(cnt / 4000))

    for (i in 1:30) { # This adjusts the breaks among groups in a more logical way
      doc_test_sum <- doc_test_sum  |>
        mutate(brk = if_else(lag(brk) != brk & len != 0, lag(brk), brk))
    }

    doc_test_sum <- doc_test_sum  |> # Creates the groups
      mutate(brk = replace_na(brk, 0)) |>
      select(code, brk) |>
      group_by(brk) |>
      summarise(code = paste(code, collapse = " \n ")) |>
      ungroup() |>
      select(code)

    prompt50 <- paste(
      "This text contains R code. Describe to a technical audience what the code does in 50 words."
    )

    # Here we feed the groups of code to the LLM to review
    doc_test_sum <- llm_custom(doc_test_sum, code, prompt50, pred_name = "Comment") |>
      mutate(group = "group")

    doc_test_sum <- doc_test_sum  |> # We collapse again the groups into a single blob of text
      select(Comment, group) |>
      group_by(group) |>
      summarise(Comment = paste(Comment, collapse = " \n ")) |>
      ungroup() |>
      select(Comment)

    prompt40 <- paste(
      "This text represents summaries of comments on a single R script.",
      "Based on this text, provide a summary in 40 words or less of what the original code does.",
      "The summary is for an external audience who is not familiar with the code but will have
  at least an intermediate level of R. Use key words, packages and other elements in this context.",
      "Only write the summary, no introductions to it. Be efficient in the use of text."
    )

    # Aaand we now ask the LLM to guess what the script did based on the descriptions
    doc_test_sum <- llm_custom(doc_test_sum, Comment, prompt40, pred_name = "Comment_2")|>
      mutate(code = "") |>
      select(code, Comment_2) |>
      rename(Comment = Comment_2) |>
      mutate(Comment = paste0("# ", Comment)) |>
      #  add_row(code = "", Comment = "") |>
      mutate(Comment1 = word(Comment, 1, 10)) |>
      mutate(Comment2 = paste0("# ", word(Comment, 11, 20))) |>
      mutate(Comment3 = if_else(str_count(Comment, '\\w+') <= 30,
                                paste0("# ", word(Comment, 21, str_count(Comment, '\\w+'))),
                                paste0("# ", word(Comment, 21, 30)))) |>
      mutate(Comment4 = if_else(str_count(Comment, '\\w+') <= 40,
                                paste0("# ", word(Comment, 31, str_count(Comment, '\\w+'))),
                                paste0("# ", word(Comment, 31, 40)))) |>
      mutate(Comment5 = if_else(str_count(Comment, '\\w+') <= 50,
                                paste0("# ", word(Comment, 41, str_count(Comment, '\\w+'))),
                                paste0("# ", word(Comment, 41, 50)))) |>
      #   add_row(code = "", Comment = "") |>
      select(code, Comment1:Comment5) |>
      pivot_longer(Comment1:Comment5, names_to = "Comment") |>
      filter(value != "# NA") |>
      select(code, value) |>
      rename(Comment = value)


  }

# CODE DOC within the document
# OUTPUT: test_doc_function

  prompt10 <- paste(
    "This text contains R code. Describe to a technical audience what the code does in 10 words.",
    "Return only the answer.",
    "If the content is empty, provide no answer."
  )

  prompt20 <- paste(
    "This text contains R code. Describe to a technical audience what the code does in 20 words."
  )

  empty <- data.frame(code = "")

  doc_origin <- doc_test |>
    mutate(row = row_number())

  doc_test <- doc_origin |>
    mutate(code = str_replace(code, "#.*", "")) |>
    mutate(code = str_trim(code, "both")) |>
    filter(code != "")

  # separate_wider_delim(code, delim = "#", names = c("code", "comments")) |>
  # select(code, row)

  # Documentation for functions
  doc_fun <- bind_rows(empty, doc_test) |>
    mutate(brackets_open = if_else(grepl("{", code, fixed = TRUE),
                                   1, 0)) |>
    mutate(brackets_close = if_else(grepl("}", code, fixed = TRUE), 1, 0)) |>
    mutate(brackets_count = cumsum(brackets_open)-cumsum(brackets_close)) |>
    mutate(brackets_check = if_else(lag(brackets_count) == 0 &
                                      brackets_open == 1 & brackets_count == 1, "start",
                                    if_else(brackets_close == 1 & brackets_count == 0, "end",
                                            ""))) |>
    select(code, row, brackets_check)

  # Create a flag for starts and ends
  df_grouped <- doc_fun %>%
    mutate(
      is_start = brackets_check == "start",
      is_end = brackets_check == "end",
      group_id = NA_integer_
    )

  # Loop to assign group numbers from each "start" to the next "end"
  group_num <- 0
  inside_group <- FALSE

  for (i in seq_len(nrow(df_grouped))) {
    if (df_grouped$is_start[i]) {
      group_num <- group_num + 1
      inside_group <- TRUE
    }
    if (inside_group) {
      df_grouped$group_id[i] <- group_num
    }
    if (df_grouped$is_end[i]) {
      inside_group <- FALSE
    }
  }

  doc_fun <- df_grouped %>%
    mutate(group_id = ifelse(!is.na(group_id), as.character(group_id), "")) |>
    mutate(code = if_else(brackets_check == "start", paste0(row, "big_bird", code), code)) |>
    group_by(group_id) %>%
    filter(group_id != "") %>%
    summarise(code = paste(code, collapse = "\n")) %>%
    ungroup() %>%
    select(-group_id) |>
    filter(code != "") |>
    separate_wider_delim(code, delim = "big_bird", names = c("row", "code"))

  doc_fun <- llm_custom(doc_fun, code, prompt20, pred_name = "Comment") |>
    mutate(Comment = paste0("# DL: ", Comment)) |>
    select(row, Comment)

  # Documentation for pipes
  doc_pipe <- bind_rows(empty, doc_test) |>
    mutate(char = nchar(code)) |>
    mutate(pipes_cont = if_else(str_ends(code, "\\|>") | str_ends(code, "%>%"),
                                "continues", "")) |>
    mutate(comma_cont = if_else(str_ends(code, "\\,") | str_ends(code, "\\=") |
                                  str_ends(code, "\\&") | str_ends(code, "\\|") |
                                  str_ends(code, "\\+") | str_ends(code, "\\("),
                                "continues", "")) |>
    mutate(any_cont = if_else(pipes_cont == "continues" | comma_cont == "continues",
                              "continues", "")) |>
    #  mutate(char = if_else(char == 0, 1, 0)) |>
    mutate(pipes_ends = if_else(grepl("|>", code, fixed = TRUE) & pipes_cont != "continues",
                                "ends", "")) |>
    mutate(new_var = if_else(grepl("<-", code, fixed = TRUE),
                             "new_var", "")) |>
    mutate(pipes_ends = if_else(pipes_ends == "ends", pipes_ends,
                                if_else(lag(any_cont == "continues")
                                        & any_cont == "", "ends", ""))) |>
    # Creating the rules for start and end of pipe, with all the possible combinations and exceptions
    mutate(pipe = if_else(new_var == "new_var" & lead(any_cont == "continues"), "start",
                          if_else(new_var == "new_var" & lead(pipes_ends == "ends"), "start",
                                  #                               if_else(lead(any_cont == "continues"), "start",
                                  if_else(pipes_ends == "ends", "end", "")))) |>
    mutate(pipe = if_else(pipe == "" & new_var == "" & pipes_ends == "" &
                            any_cont == "" & lead(any_cont != "") &
                            lead(new_var != "new_var"), "start", pipe)) |>
    mutate(pipe = if_else(lag(pipe == "end") & pipes_cont == "continues", "start", pipe)) |>
    mutate(pipe = if_else(is.na(pipe), "", pipe))

  # Create a flag for starts and ends
  df_grouped <- doc_pipe %>%
    mutate(
      is_start = pipe == "start",
      is_end = pipe == "end",
      group_id = NA_integer_
    )

  # Loop to assign group numbers from each "start" to the next "end"
  group_num <- 0
  inside_group <- FALSE

  for (i in seq_len(nrow(df_grouped))) {
    if (df_grouped$is_start[i]) {
      group_num <- group_num + 1
      inside_group <- TRUE
    }
    if (inside_group) {
      df_grouped$group_id[i] <- group_num
    }
    if (df_grouped$is_end[i]) {
      inside_group <- FALSE
    }
  }

  doc_pipe <- df_grouped %>%
    mutate(pipe_group = ifelse(!is.na(group_id), as.character(group_id), "")) |>
    # mutate(row_id = row_number(),
    #        group = cumsum(pipe == "start")) %>%
    # group_by(group) %>%
    # mutate(
    #   last_end_row = ifelse(any(pipe == "end"), max(row_id[pipe == "end"]), NA_integer_),
    #   pipe = ifelse(pipe == "end" & row_id != last_end_row, "", pipe)
    # ) %>%
    # ungroup() %>%
    select(code, row, pipe, pipe_group) |>
    filter(pipe_group != "") |>
    mutate(code = if_else(pipe == "start", paste0(row, "big_bird", code), code)) |>
    group_by(pipe_group) %>%
    # filter(pipe != "end") %>%
    summarise(code = paste(code, collapse = "\n")) %>%
    ungroup() %>%
    select(-pipe_group) |>
    separate_wider_delim(code, delim = "big_bird", names = c("row", "code"))


  doc_pipe <- llm_custom(doc_pipe, code, prompt20, pred_name = "Comment") |>
    mutate(Comment = paste0("# DL: ", Comment)) |>
    select(row, Comment)


  # Now joining together doc_fun and doc_pipe
  doc_all <- bind_rows(doc_fun, doc_pipe) |>
    mutate(row = as.numeric(row)) |>
    arrange(row) |>
    distinct(row, .keep_all = TRUE)


  # Now putting them back with the original script
  # doc_final is the original script documented with comments
  doc_final <- left_join(doc_origin, doc_all, by = "row") |>
    select(code, Comment) |>
    mutate(Comment = if_else(is.na(Comment), "", Comment))


  if (str_sub(file,-2, -1) == "md") { # Different way for markdown files, to ignore actual text outside of chunks
    doc_final <- doc_final |>
      mutate(
        Comment = ifelse(
          row_number() >= which(code == "---")[1] &
            row_number() <= which(code == "---")[2],
          "",
          Comment
        )
      ) |>
      mutate(
        count = cumsum(grepl("```", code, fixed = TRUE)),                    # Count occurrences of "abc"
        flag = (count %% 2 == 1 )) |>
      mutate(Comment = if_else(flag == FALSE,
                               "",
                               Comment)) |>
      select(-count, -flag)

  } else {
    doc_final
  }

  if (level == "light") {
    doc_final
  } else if (level == "heavy") {
    doc_test_sum <- llm_custom(doc_test, code, prompt10, pred_name = "Comment") |>
      mutate(Comment = if_else(code != "",
                               paste0("  # DL: ", Comment),
                               "")) |>
      mutate(Comment = if_else(grepl("# DL: ", code, fixed = TRUE),
                               "",
                               Comment)) |>
      mutate(Comment = if_else(grepl("```", code, fixed = TRUE),
                               "",
                               Comment))


  }  else {
    print("Please define if light or heavy commentary.")
  }

# FINALIZE

  final_doc <- bind_rows(doc_test_sum, doc_final)
  #final_doc <- doc_test_sum
  rm(doc_test_sum)
  rm(doc_final)

  filename <- paste0(str_sub(file, 1, -3), "_doctord.R")

  write.table(final_doc, filename, sep = "", row.names = FALSE,
              col.names = FALSE, quote = FALSE)
  message("File written to: ", filename)

}
