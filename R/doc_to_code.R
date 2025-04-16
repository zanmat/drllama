
#' Write documentation for code
#'
#' @param file Path to the code file
#' @param level Documentation level of detail. Defaults to light. Heavy: All lines are documented. Light: Only lines within pipes are documented.
#'
#' @return A new R file is created adding a 10 word documentation comment to the code
#' @export
#'
#' @examples code_to_doc("my_code.R", "light")
#' @examples code_to_doc("my_code.R", "heavy")

doc_to_code <- function(file, level = "light") {

#  llm_use("ollama", "llama3.2:3b", seed = 100, temperature = 0)

  doc_test <- data.frame(code = readLines(file, warn = FALSE))

# SUMMARY function
# OUTPUT: doc_test_sum

if (sum(nchar(doc_test$code)) < 5000) {

    doc_test_sum <- doc_test |>
      summarise(code = paste(code, collapse = " \n "))

    prompt <- paste(
      "What does this R code do? Please answer in 50 words."
    )

    doc_test_sum <- llm_custom(doc_test_sum, code, prompt, pred_name = "Comment") |>
      mutate(code = "") |>
      mutate(Comment = paste0("# ", Comment)) |>
      mutate(Comment1 = word(Comment, 1, 10)) |>
      mutate(Comment2 = paste0("# ", word(Comment, 11, 20))) |>
      mutate(Comment3 = paste0("# ", word(Comment, 21, 30))) |>
      mutate(Comment4 = paste0("# ", word(Comment, 31, 40))) |>
      mutate(Comment5 = paste0("# ", word(Comment, 41, 50))) |>

      select(code, Comment1:Comment5) |>
      pivot_longer(Comment1:Comment5, names_to = "Comment") |>
      select(code, value) |>
      rename(Comment = value) |>
      add_row(code = "", Comment = "")

  } else {

    prompt <- paste(
      "Provide a 15 word comment explaining what this R code does.",
      "Return only the answer.",
      "Answer this in a simple way for someone at an intermediate level of R."
    )

    doc_test_sum <- llm_custom(doc_test, code, prompt, pred_name = "Comment") |>
      mutate(Comment = if_else(code != "",
                               paste0("  # ", Comment),
                               "")) |>
      mutate(Comment = if_else(grepl("#", code, fixed = TRUE),
                               "",
                               Comment)) |>
      mutate(Comment = if_else(grepl("```", code, fixed = TRUE),
                               "",
                               Comment))

    doc_test_sum <- doc_test_sum |>
      mutate(len = str_length(code)) |>
      mutate(cnt = cumsum(len)) |>
      mutate(brk = trunc(cnt / 4000))

    for (i in 1:30) {
      doc_test_sum <- doc_test_sum  |>
        mutate(brk = if_else(lag(brk) != brk & len != 0, lag(brk), brk))
    }

    doc_test_sum <- doc_test_sum  |>
      mutate(brk = replace_na(brk, 0)) |>
      select(Comment, brk) |>
      group_by(brk) |>
      summarise(Comment = paste(Comment, collapse = " \n ")) |>
      ungroup() |>
      select(Comment)


    prompt2 <- paste(
      "This text represents comments on R code. Try to provide a summary in 20 words of what this code does keeping key words.",
      "Return only the answer.",
      "Answer this in a simple way for someone at an intermediate level of R."
    )

    doc_test_sum <- llm_custom(doc_test_sum, Comment, prompt2, pred_name = "Comment_2")

    doc_test_sum <- doc_test_sum  |>
      summarise(Comment_2 = paste(Comment_2, collapse = " \n "))


    prompt3 <- paste(
      "This text represents summaries of comments on R code. Try to provide a summary in 40 words of what this code does keeping key words.",
      "Return only the answer.",
      "Answer this in a simple way for someone at an intermediate level of R."
    )


    doc_test_sum <- llm_custom(doc_test_sum, Comment_2, prompt3, pred_name = "Comment") |>
      mutate(code = "") |>
      mutate(Comment = paste0("# ", Comment)) |>
      add_row(code = "", Comment = "") |>
      mutate(Comment1 = word(Comment, 1, 10)) |>
      mutate(Comment2 = paste0("# ", word(Comment, 11, 20))) |>
      mutate(Comment3 = paste0("# ", word(Comment, 21, 30))) |>
      mutate(Comment4 = paste0("# ", word(Comment, 31, 40))) |>
      mutate(Comment5 = paste0("# ", word(Comment, 41, 50))) |>

      #   add_row(code = "", Comment = "") |>
      select(code, Comment1:Comment5) |>
      pivot_longer(Comment1:Comment5, names_to = "Comment") |>
      select(code, value) |>
      rename(Comment = value)

  }

# CODE DOC
# OUTPUT: test_doc_function

  prompt <- paste(
    "Provide a 10 word comment explaining what this R code does.",
    "Return only the answer.",
    "If the content is empty, provide no answer.",
    "Answer this in a simple way for someone at beginner to intermediate level of R."
  )


  test_doc_function <- llm_custom(doc_test, code, prompt, pred_name = "Comment") |>
    mutate(Comment = if_else(code != "",
                             paste0("  # ", Comment),
                             "")) |>
    mutate(Comment = if_else(grepl("#", code, fixed = TRUE),
                             "",
                             Comment)) |>
    mutate(Comment = if_else(grepl("```", code, fixed = TRUE),
                             "",
                             Comment))

  if (str_sub(file,-2, -1) == "md") { # Different way for markdown files, to ignore actual text outside of chunks
    test_doc_function <- test_doc_function |>
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
    test_doc_function <- test_doc_function |>
      mutate(Comment = if_else(code != "",
                               Comment,
                               "")) |>
      mutate(Comment = if_else(grepl("#", code, fixed = TRUE),
                               "",
                               Comment)) |>
      mutate(Comment = if_else(grepl("```", code, fixed = TRUE),
                               "",
                               Comment))
  }


  if (level == "light") {
    test_doc_function <- test_doc_function |>
      mutate(Comment = if_else(grepl("|>", code, fixed = TRUE) | grepl("%>%", code, fixed = TRUE),
                               Comment,
                               ""))
  } else if (level == "heavy") {
    test_doc_function
  }  else {
    print("Please define if light or heavy commentary.")
  }

# FINALIZE

  final_doc <- bind_rows(doc_test_sum, test_doc_function)
  rm(doc_test_sum)
  rm(test_doc_function)

  filename <- paste0("with_doc_", file)

  write.table(final_doc, filename, sep = "", row.names = FALSE,
              col.names = FALSE, quote = FALSE)
  message("File written to: ", filename)

}
