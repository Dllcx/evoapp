plot_vector <- as_tibble(read_rds('~/R/evoapp/DB/plot_vectors.rds'))
plot_vector %>% add_column(.,dt_vct2,.before= 1) -> plot_vector
plot_vector %>% toJSON(dataframe = 'columns',pretty=TRUE) %>% write(.,'~/PycharmProjects/tsy_flasktv/db/plot.json')