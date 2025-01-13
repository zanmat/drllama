
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

  if (str_sub(file,-2, -1) == "md") {
    test_doc_function <- test_doc_function |>
      mutate(
        Comment = ifelse(
          row_number() >= which(code == "---")[1] &
            row_number() <= which(code == "---")[2],
          "",
          Comment
        )
      )
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

  filename <- paste0("with_doc_", file)

  write.table(test_doc_function, filename, sep = "", row.names = FALSE,
              col.names = FALSE, quote = FALSE)
  message("File written to: ", filename)

}
