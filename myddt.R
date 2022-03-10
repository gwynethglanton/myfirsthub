#' @title MyDDT Function
#'
#' @description project 1: a ddt function
#' @param df the data frame
#' @param SPECIES the species being called from the ddt data set
#'
#' @return reads the dtt file
#' @export
#' @importFrom dplyr '%>%' filter
#' @importFrom ggplot2 ggplot geom_point geom_smooth ggtitle aes_string
#' @importFrom stats addmargins
#' @importFrom utils head write.csv
#'
#'
#' @examples
#' \dontrun{myddt(df = ddt, SPECIES = "CCATFISH")}
#'



myddt <- function(df, SPECIES){


  print(df)
  tab= table(df$RIVER) / length(df$RIVER)
  addmargins(tab)
  print(tab)
  ## Create new data frame using {{SPECIES}} from the function call
  df1 <- df %>% filter(SPECIES == {{SPECIES}})

  if ({{SPECIES}} == "CCATFISH"){
    write.csv(x = df1, "LvsWforCCATFISH.csv", row.names = FALSE)
  }
  if ({{SPECIES}} == "SMBUFFALO"){
    write.csv(x = df1, "LvsWforSMBUFFALO.csv", row.names = FALSE)
  }
  if ({{SPECIES}} == "LMBASS"){
    write.csv(x = df1, "LvsWforLMBASS.csv", row.names = FALSE)
  }
  print(df1)
  g <- ggplot(df1, aes_string(x="WEIGHT",y="LENGTH")) + # Note the use of aes_string
    geom_point(aes_string(color = "RIVER" )) +
    geom_smooth(formula = y~x +I(x^2), method = "lm") + ggtitle("Gwyneth Glanton")
  print(g)
  head(df1)

}
