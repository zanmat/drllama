
#' Write documentation for code
#'
#' @param file Path to the code file with the function
#'
#' @return A new R file is created adding a 10 word documentation comment to the code
#' @export
#'
#' @examples droxygen("my_new_package_function.R")

droxygen <- function(file) {

  #  llm_use("ollama", "llama3.2:3b", seed = 100, temperature = 0)

  doc_test <- data.frame(code = readLines(file, warn = FALSE))

# Function title
# OUTPUT: A short title for the function

  doc_test_title <- doc_test |>
    summarise(code = paste(code, collapse = " \n "))

  prompt_title <- paste(
    "Give a title to this R function in 5 words or less that describes it. Don't use quotation marks."
  )

  doc_test_title <- llm_custom(doc_test_title, code, prompt_title, pred_name = "Comment") |>
    mutate(Comment = paste0("#' #' ", Comment)) |>
    select(Comment) |>
    add_row(Comment = "#'")

# Overall function description
# OUTPUT: A description of the function

  prompt20 <- paste(
    "Describe in 20 words what this R function does."
  )

  doc_test_sum <- doc_test |>
    summarise(code = paste(code, collapse = " \n ")) |>
    llm_custom(code, prompt20, pred_name = "Comment") |>
    mutate(code = "") |>
    mutate(Comment = paste0("#' ", Comment)) |>
    #  add_row(code = "", Comment = "") |>
    mutate(Comment1 = word(Comment, 1, 10)) |>
    mutate(Comment2 = if_else(str_count(Comment, '\\w+') <= 20,
                              paste0("#' ", word(Comment, 11, str_count(Comment, '\\w+') + 1)),
                              paste0("#' ", word(Comment, 11, 20)))) |>
    select(code, Comment1:Comment2) |>
    pivot_longer(Comment1:Comment2, names_to = "Comment") |>
    select(value, value) |>
    rename(Comment = value) |>
    add_row(Comment = "#' @description", .before = 1) |>
    add_row(Comment = "#' ")



# Identifying the parameters
# OUTPUT: A list of parameters under a vector "code" in a df

  param_count <- doc_test |>
    filter(grepl("function", code, fixed = TRUE)) |>
    separate_wider_delim(code, delim = "function", names = c("ignore", "code")) |>
      mutate(code = str_replace_all(code, "c\\(([^\\)]*?)\\)", function(x) {
      str_replace_all(x, ",", "")
    })) |>
    filter(row_number() == 1) |>
    select(code) |>
    mutate(code = str_remove(code, "\\(")) |>
    mutate(code = str_remove(code, "\\{")) |>
    separate_rows(code, sep = ",") |>
    mutate(code = str_trim(code, "both")) |>
    separate_wider_delim(code, delim = "=", names = c("code", "ignore"), too_few = "align_start") |>
    select(code) |>
    mutate(code = str_remove(code, "\\)"))


  doc_test_body <- doc_test |>
    summarise(code = paste(code, collapse = " \n "))

  temp_dfs <- list()

  params <- param_count$code

  for (i in params) {

  prompt_param <- paste0("In less than 10 words: what does the parameter ", i, " do in this function? Don't repeat its name in the explanation")

  param_description <- llm_custom(doc_test_body, code, prompt_param, pred_name = "Comment") |>
    mutate(param = i) |>
    select(param, Comment)

    temp_dfs[[i]] <- param_description

  }

  params <- bind_rows(temp_dfs) |>
    mutate(Comment = paste0("#' @param ", param, " ", Comment)) |>
    select(Comment) |>
    add_row(Comment = "#' ")



# Function returns
# OUTPUT: What the function returns

    doc_test_return <- doc_test |>
      summarise(code = paste(code, collapse = " \n "))

    prompt20 <- paste(
      "Describe in 20 words what this R function returns and what arguments it accepts."
    )


    doc_test_return <- llm_custom(doc_test_return, code, prompt20, pred_name = "Comment") |>
      mutate(code = "") |>
      mutate(Comment = paste0("#' ", Comment)) |>
      #  add_row(code = "", Comment = "") |>
      mutate(Comment1 = word(Comment, 1, 10)) |>
      mutate(Comment2 = if_else(str_count(Comment, '\\w+') <= 20,
                                paste0("#' ", word(Comment, 11, str_count(Comment, '\\w+') +1 )),
                                paste0("#' ", word(Comment, 11, 20)))) |>
      select(code, Comment1:Comment2) |>
      pivot_longer(Comment1:Comment2, names_to = "Comment") |>
      select(value, value) |>
      rename(Comment = value) |>
      add_row(Comment = "#'") |>
      add_row(Comment = "#' @export") |>
      add_row(Comment = "#' @return", .before = 1)


# Adding it all together

droxygend <- bind_rows(doc_test_title, doc_test_sum, params, doc_test_return) |>
  add_row(Comment = "") |>
  rename(code = Comment) |>
  mutate(code = str_remove_all(code, "\\\n")) |>
  bind_rows(doc_test_body)

rm(doc_test_title)
rm(doc_test_sum)
rm(params)
rm(doc_test_return)

filename <- paste0("droxygend_", file)

write.table(droxygend, filename, sep = "", row.names = FALSE,
            col.names = FALSE, quote = FALSE)
message("File written to: ", filename)

}
