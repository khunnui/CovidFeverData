library(gtsummary)
library(gt)
library(rstatix)

# detach("package:gtExtras", unload = TRUE)
# devtools::install_github("Nartladac/gtExtras")
library(gtExtras)

get_sum_data <- function(df) {

  df <- filter(df, finalresult %in% c('Positive','Negative'))
  
  vars <- df %>%
    select(-c(province, hospital, finalresult)) %>%
    map_dbl(sum, na.rm = TRUE) %>%
    sort(decreasing = TRUE)
  
  gtsum <- df %>%
    select(finalresult, names(which(vars > 0))) %>%
    tbl_summary(by = finalresult,
                statistic = all_categorical() ~ "{n} ({p})",
                digits = all_categorical() ~ c(0, 1),
                missing = "no") %>%
    add_overall() %>%
    add_p()

  N0 <- inline_text(gtsum, variable = names(vars[1]), column = "stat_0", pattern = "{N_obs}")
  N1 <- inline_text(gtsum, variable = names(vars[1]), column = "stat_1", pattern = "{N_obs}")
  N2 <- inline_text(gtsum, variable = names(vars[1]), column = "stat_2", pattern = "{N_obs}")
  
  df_sum <- gtsum$table_body %>%
    select(variable, stat_0:stat_2, p.value) %>%
    mutate(
      p.value = p_mark_significant(format.pval(
        p.value, eps = .001, digits = 1
      )),
      pos = as.integer(sub(
        ",", "", str_extract(stat_1, boundary("word")), fixed = TRUE
      )),
      neg = as.integer(sub(
        ",", "", str_extract(stat_2, boundary("word")), fixed = TRUE
      ))
    )
  df_sum$posneg <- lapply(1:nrow(df_sum), function(i) c(df_sum$pos[i], df_sum$neg[i]))
  df_sum <- select(df_sum, -c(pos, neg))

  return(list(df_sum = df_sum, N0 = N0, N1 = N1, N2 = N2))

}


create_sum_table <- function(df_sum, tt, head, N0, N1, N2) {
  
  df_sum %>%
    gt() %>%
    gt_plt_bar_stack(
      posneg,
      palette = c('#b78f62','#a1caf1'),
      labels = c("PCR Positive", "PCR Negative"),
      position = "stack",
      width = 80
    ) %>%
    tab_options(
      table.font.size = px(11L),
      table.border.top.style = "hidden",
      data_row.padding = 2
    ) %>%
    tab_header(
      title = tt
    ) %>%
    tab_spanner(label = "PCR Result",
                columns = stat_1:stat_2) %>%
    tab_style(
      style = list(cell_text(font = "Verdana", size = 14)),
      locations = list(cells_title(groups = "title"))
    ) %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = list(cells_column_spanners())
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
      variable = gt::html(paste0("<b>", head, "</b>")),
      stat_0   = gt::html(paste0("<b>Overall</b>  (N = ", N0, ")<br>n (%)")),
      stat_1   = gt::html(paste0("<b>Positive</b> (N = ", N1, ")<br>n (%)")),
      stat_2   = gt::html(paste0("<b>Negative</b> (N = ", N2, ")<br>n (%)")),
      p.value  = gt::html("<b>p value</b>")
    ) %>%
    cols_align(align = "center",
               columns = c(starts_with("stat_"), p.value)) %>%
    cols_width(starts_with("stat_") ~ px(120)) %>%
    cols_width(p.value ~ px(100))

}
