# convert October 2023 Council word docs to rmd as starting point
# for 2024 April Council review
# word docs from Brandon

rmarkdown::pandoc_convert(input = here::here("4_Risk Element Overview for Council_09_23.docx"),
                          to = "markdown",
                          output = here::here("MAB_RiskAssess_2024.md"))

# then save the resulting markdown file as .Rmd and edit from there