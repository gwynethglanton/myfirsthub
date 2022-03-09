
#'
#' @title Makes a ddt function
#'
#' @param df the data frame
#' @param species the species being called from the ddt data set
#'
#' @return reads the dtt file
#' @export
#'
#'
#' @examples
#' \dontrun{myddt(df, species)}
#'
# functions that call dplyr and ggplot2.
# this is not obvious because of non standard evaluation
library(dplyr)
library(ggplot2)



myddt <- function(df, species){


  print(df)
  tab= table(df$RIVER) / length(df$RIVER)
  addmargins(tab)
  print(tab)
  df1 <- filter(df, SPECIES == species)
  if (species == "CCATFISH"){
    write.csv(x = df1, "LvsWforCCATFISH.csv", row.names = FALSE)
  }
  if (species == "SMBUFFALO"){
    write.csv(x = df1, "LvsWforSMBUFFALO.csv", row.names = FALSE)
  }
  if (species == "LMBASS"){
    write.csv(x = df1, "LvsWforLMBASS.csv", row.names = FALSE)
  }
  print(df1)
  g <- ggplot(df1, aes_string(x="WEIGHT",y="LENGTH")) + # Note the use of aes_string

    geom_point(aes_string(color = "RIVER" )) +
    geom_smooth(formula = y~x +I(x^2), method = "lm") + ggtitle("Gwyneth Glanton")
  print(g)
  head(df1)

}
