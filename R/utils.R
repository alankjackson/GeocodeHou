
#' Test or Apply string replacements
#'
#' Apply a regular expression and return the original and the changed
#' value, only the unique values
#'
#'
#' @param  Input          Input tibble
#' @param  Output         Variable to out result into
#' @param  Reg_expression Regular expression to apply to input
#' @param  Replacement    String to use for replacement
#' @param  Mode           Test/Apply = test will return just unique changes
#' @return A tibble will be returned with the:
#' - Input$Var (where a change occurred)
#' - Output
#' @examples
#' string_replace(df, "Address", ^(\\d+ )\\d/\\d ", "\\1", "Test")
#' @export
string_replace <- function( Input, Var, Reg_expression,
                            Replacement, Mode="Test"){
  Output <- Input %>%
    dplyr::mutate(Output=stringr::str_replace(!!rlang::sym(Var),
                         Reg_expression, Replacement))
  if (Mode == "Test") {
    Output <-  Output %>%
      dplyr::filter(Output!=!!rlang::sym(Var)) %>%
      dplyr::select(!!rlang::sym(Var), Output) %>%
      dplyr::distinct()
  }

  return(Output)

}
