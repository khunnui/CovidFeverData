library(gtsummary)
library(gt)
library(rstatix)

# detach("package:gtExtras", unload = TRUE)
# devtools::install_github("Nartladac/gtExtras")

library(gtExtras)

create_sumtable <- function(df, head) {
  
  if (length(unique(df$hospital)) == 1) {
    tt <- paste0(unique(df$hospital), " Hospital")
  }
  else if (length(unique(df$province)) == 1) {
    tt <- paste0(unique(df$province), " Province")
  }
  else {
    tt <- ""
  }
  
  vars <- df %>% filter(finalresult %in% c('Positive','Negative')) %>% 
    select(-c(province, hospital, finalresult)) %>%
    map_dbl(sum, na.rm = TRUE) %>%
    sort(decreasing = TRUE)
  
  gtsum <- df %>% filter(finalresult %in% c('Positive','Negative')) %>% 
    select(finalresult, names(which(vars > 0))) %>%
    tbl_summary(by = finalresult,
                statistic = all_categorical() ~ "{n} ({p})",
                digits = all_categorical() ~ c(0, 1),
                missing = "no") %>%
    add_overall() %>%
    add_p()
  
  N0 <- inline_text(gtsum, variable = names(vars[1]), column = "stat_0", pattern = "N = {N_obs}")
  N1 <- inline_text(gtsum, variable = names(vars[1]), column = "stat_1", pattern = "N = {N_obs}")
  N2 <- inline_text(gtsum, variable = names(vars[1]), column = "stat_2", pattern = "N = {N_obs}")

  gtsum_data <- gtsum$table_body %>%
    select(variable, stat_0:stat_2, p.value) %>%
    mutate(
      `p value` = p_mark_significant(format.pval(
        p.value, eps = .001, digits = 1
      )),
      pos = as.integer(sub(
        ",", "", str_extract(stat_1, boundary("word")), fixed = TRUE
      )),
      neg = as.integer(sub(
        ",", "", str_extract(stat_2, boundary("word")), fixed = TRUE
      ))
    )
  gtsum_data$posneg <-
    lapply(1:nrow(gtsum_data), 
           function(i) c(gtsum_data$pos[i], gtsum_data$neg[i]))
  
  gt <- gtsum_data %>%
    select(-c(p.value, pos, neg)) %>%
    gt() %>%
    gt_plt_bar_stack(
      posneg,
      palette = c('#b78f62','#a1caf1'),
      labels = c("PCR Positive", "PCR Negative"),
      position = "stack",
      width = 80
    ) %>%
    tab_options(
      heading.title.font.size = px(14L),
      table.font.size = px(11L),
      data_row.padding = 0,
      table.border.top.style = "hidden"
    ) %>%
    tab_header(
      title = html(paste0('<p style="font-family: Verdana">',tt,'</p>'))
    ) %>%
    tab_spanner(label = "PCR Result",
                columns = stat_1:stat_2) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = list(cells_column_spanners(), cells_column_labels())
    ) %>%
    tab_style(
      style = cell_borders(
        sides = c("bottom"),
        color = "LightGray",
        weight = px(0.5),
        style = "solid"
      ),
      locations = cells_body(columns = everything(),
                             rows = everything())
    ) %>%
    cols_label(
      variable = head,
      stat_0 = html(paste0("Overall (",N0,")<br>n (%)")),
      stat_1 = html(paste0("Positive (",N1,")<br>n (%)")),
      stat_2 = html(paste0("Negative (",N2,")<br>n (%)"))
    ) %>%
    cols_align(align = "center",
               columns = c(starts_with("stat_"),`p value`)) %>%
    cols_width(starts_with("stat_") ~ px(120)) %>%
    cols_width(`p value` ~ px(100))
  
  return(gt)
  
}
