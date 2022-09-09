library(plotly)

df_enr <- CFMast %>%
  mutate(enrdate = floor_date(s1enrolldate, "month")) %>%
  group_by(province, enrdate) %>%
  tally()

df_pos <- CFMast %>%
  mutate(enrdate = floor_date(s1enrolldate, "month")) %>%
  group_by(province, enrdate, finalresult) %>%
  tally() %>% 
  summarise(inc = round(sum(n[finalresult == 'Positive'], na.rm = TRUE) / sum(n), 2))

plot_ly(x = ~ enrdate,
        hoverinfo = 'y') %>%
  add_trace(
    data = df_enr %>% filter(province == 'Nakorn Phanom'),
    y = ~ n,
    name = "NP Enrollment",
    type = 'bar',
    marker = list(color = '#BDE0EB')
  ) %>%
  add_trace(
    data = df_enr %>% filter(province == 'Tak'),
    y = ~ n,
    name = "Tak Enrollment",
    type = 'bar',
    marker = list(color = '#E1CE82')
  ) %>%
  add_trace(
    data = df_pos %>% filter(province == 'Nakorn Phanom'),
    x = ~ enrdate,
    y = ~ inc,
    yaxis = "y2",
    name = "NP Covid-19 rate",
    type = 'scatter',
    mode = 'lines',
    line = list(color = '#82c4d9')
  ) %>%
  add_trace(
    data = df_pos %>% filter(province == 'Tak'),
    x = ~ enrdate,
    y = ~ inc,
    yaxis = "y2",
    name = "Tak Covid-19 rate",
    type = 'scatter',
    mode = 'lines',
    line = list(color = '#d2b644')
  ) %>%
  layout(
    title = list(text = 'Enrollment by month',
                 font = list(family = "Verdana", size = 14),
                 yref = 'container',
                 y = 0.95),
    yaxis2 = list(
      overlaying = "y",
      side = "right",
      rangemode = 'tozero',
      title = 'Covid-19 Positive (%)',
      range = list(0, 1),
      tickformat = '.0%',
      showgrid = FALSE
    ),
    xaxis = list(
      title = '',
      dtick = "M1",
      tickformat = "%b %y"
    ),
    yaxis = list(title = 'Number of Enrolments'),
    barmode = 'group',
    margin = list(t = 80, b = 80, l = 80, r = 80),
    legend = list(
      orientation = "h",
      # show entries horizontally
      xanchor = "center",
      # use center of legend as anchor
      x = 0.5
    ),
    paper_bgcolor = '#EAF2F3'
  )
