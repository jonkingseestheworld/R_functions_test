library(palmerpenguins)
library(table1)
library(dplyr)
library(stringr)

## Useful post from D Navarro
#https://blog.djnavarro.net/posts/2024-06-21_table1/



df <- penguins

table1(~ island + bill_length_mm | species, df)

# Applying labels
# label() function -- setting the “label” metadata attribute for the relevant object
# similar to base R, attr() function
label(df$island) <- "Island"
label(df$bill_length_mm) <- "Bill Length (mm)"

table1(~ island + bill_length_mm | species, df)



## setLabel() that can be used with {dplyr}
df <- df |> 
  mutate(
    flipper_length_mm = setLabel(flipper_length_mm, "Flipper Length (mm)"),
    body_mass_g = setLabel(body_mass_g, "Body Mass (g)"),
    sex = setLabel(sex, "Sex"),
    year = setLabel(year, "Year")
  )

table1(~ flipper_length_mm + body_mass_g + sex + year | species, df)

table1(~ flipper_length_mm + body_mass_g + sex + year , df)

table1(
  x = ~ flipper_length_mm + body_mass_g | island * species,
  data = df,
  overall = FALSE
)



## Customing cell content
table1(
  x = ~ flipper_length_mm + body_mass_g + sex | species, 
  data = penguins, 
  render.continuous = c(
    "10th percentile" = "q10", 
    "50th percentile" = "q50",
    "90th percentile" = "q90"
  )
)

## Supported aliases
continuous <- 1:10
names(stats.default(continuous)) 


# Writing render functions
render_winsorized <- function(x, cutoff = .05, ...) {
  lo <- quantile(x, cutoff, na.rm = TRUE)
  hi <- quantile(x, 1 - cutoff, na.rm = TRUE)
  x[x < lo] <- lo
  x[x > hi] <- hi
  strs <- c(
    "",
    "Winsorized mean" = sprintf("%1.2f", mean(x, na.rm = TRUE)),
    "Winsorized SD" = sprintf("%1.2f", sd(x, na.rm = TRUE))
  )
  return(strs)
}


render_counts <- function(x, ...) c("", table(stringr::str_to_title(x)))


table1(
  x = ~ flipper_length_mm + body_mass_g + sex | species, 
  data = df, 
  render.continuous = render_winsorized,
  render.categorical = render_counts
)
