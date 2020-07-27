# this allows you to take a quick look at crime trends over time
quick_line <- function(df) {
  df %>%
    group_by(month) %>%
    # June data isn't complete yet
    filter(month < as.Date("2020-06-01")) %>%
    summarise(n = n_distinct(id)) %>%
    ggplot() +
    geom_line(aes(x = month, y = n))
}



