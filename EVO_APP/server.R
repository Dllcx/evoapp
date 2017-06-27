# server.R
library(shiny)
library(shinydashboard)
library(tidyverse)
library(highcharter)

# ANTES DE INICIAR O SERVER RODAR O SOURCE plot_base_vectors_OC.r

setwd('~/R/tsy_DailyReport/DB/')
# DB SOCKET
db <- src_sqlite("cetip_db.sqlite3", create = FALSE)

# DB TABLES
tbl_oc_mercado <- tbl(db,"b3_oc_mercado")
tbl_oc_santander <- tbl(db,"b3_oc_santander")
tbl_CDB_mercado <- tbl(db,"b3_CDB_mercado")
tbl_CDB_santander <- tbl(db,"b3_CDB_santander")

# Dates Vector
dt_vct <- tbl_oc_mercado %>% distinct(DAT_REGISTRO) %>% filter(DAT_REGISTRO >= '2016-12-02') %>% collect()
dt_vct2 <- lubridate::ymd(dt_vct[[1]])

# TS Vectors
plot_vector <- as_tibble(read_rds('plot_vectors.rds'))
ts_San_Share <- xts::xts(plot_vector$vect_share_oc,order.by = dt_vct2)
ts_delta <- xts::xts(plot_vector$vect_delta_oc,order.by = dt_vct2)
ts_San_Share_cdb <- xts::xts(plot_vector$vect_share_cdb,order.by = dt_vct2)
ts_delta_cdb <- xts::xts(plot_vector$vect_delta_cdb,order.by = dt_vct2)
ts_San_Share_cdb2 <- xts::xts(plot_vector$vect_share_cdb2,order.by = dt_vct2)
ts_delta_cdb2 <- xts::xts(plot_vector$vect_delta_cdb2,order.by = dt_vct2)

# Define server logic
shinyServer(function(input, output) {
  
# GRAFICOS
  output$graph <- renderHighchart({
    highchart(type="stock") %>% 
      hc_add_series(ts_San_Share, name = "Share", color = "red") %>% 
      hc_add_series(ts_delta, type = "column", name = "Delta", color = "gray") %>% 
      hc_rangeSelector(selected = 0) %>% 
      hc_tooltip(useHTML=TRUE,pointFormat = paste0("<span style=\"color:{point.color}\">\u25CF</span>", "{series.name}: <b>{point.y:.2f}%</b><br/>"))
  })
  
  output$graph_cdb <- renderHighchart({
    switch(input$var2,
           Cliente = {
             highchart(type="stock") %>% 
               hc_add_series(ts_San_Share_cdb, name = "Share", color = "red") %>% 
               hc_add_series(ts_delta_cdb, type = "column", name = "Delta", color = "gray") %>% 
               hc_rangeSelector(selected = 0) %>% 
               hc_tooltip(useHTML=TRUE,pointFormat = paste0("<span style=\"color:{point.color}\">\u25CF</span>", "{series.name}: <b>{point.y:.2f}%</b><br/>"))
           },
           Mercado = {
             highchart(type="stock") %>% 
               hc_add_series(ts_San_Share_cdb2, name = "Share", color = "red") %>% 
               hc_add_series(ts_delta_cdb2, type = "column", name = "Delta", color = "gray") %>% 
               hc_rangeSelector(selected = 0) %>% 
               hc_tooltip(useHTML=TRUE,pointFormat = paste0("<span style=\"color:{point.color}\">\u25CF</span>", "{series.name}: <b>{point.y:.2f}%</b><br/>"))
           })
  })
  
# TABELAS RENDERIZADAS
  output$tbl_vol <- DT::renderDataTable({
    df_press() %>% 
      select(VolSantander) %>% 
      sum() -> total_sum_volumeSantander
    
    df_press() %>% 
      select(volXsantander) %>% 
      sum() -> total_sum_volumexsantander
    
    total_share <- total_sum_volumeSantander / (total_sum_volumeSantander + total_sum_volumexsantander)
    
    sketch = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th("Faixa Prazo"),
          th("Santander"),
          th("Peer Groups"),
          th("Share")
        )
      ),
      tfoot(
        tr(
          th('Total'),
          th(sprintf(fmt="$%.2f",total_sum_volumeSantander)),
          th(sprintf(fmt="$%.2f",total_sum_volumexsantander)),
          th(sprintf(fmt="%.2f%%",total_share))
        )
      )
    ))
    df_press() %>% 
      select(FAIXA_PRAZO,VolSantander,volXsantander, ShareSantander) %>% 
      DT::datatable(options = list(dom = 't'), container = sketch, rownames=FALSE, extensions = 'Responsive') %>% 
      DT::formatCurrency(c('VolSantander','volXsantander')) %>% 
      DT::formatPercentage('ShareSantander',2)
  })
  
  output$tbl_tax <- DT::renderDataTable({
    df_press() %>% 
      select(soma.y) %>% 
      sum() -> total_volsan
    
    df_press() %>% 
      select(tx.y) %>% 
      sum() -> total_txvolsan
    
    df_press() %>% 
      select(tx.x) %>% 
      sum() -> total_txvolmkt
    
    df_press() %>% 
      select(soma.x) %>% 
      sum() -> total_volmkt
    
    total_txmd_santander <- total_txvolsan/total_volsan
    total_txmd_xsantander <- (total_txvolmkt - total_txvolsan)/(total_volmkt - total_volsan)
    total_delta_santander <- total_txmd_santander - total_txmd_xsantander
    
    sketch = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th("Faixa Prazo"),
          th("Santander"),
          th("Peer Groups"),
          th("Delta")
        )
      ),
      tfoot(
        tr(
          th("Total"),
          th(sprintf(fmt="%.2f%%",total_txmd_santander)),
          th(sprintf(fmt="%.2f%%",total_txmd_xsantander)),
          th(sprintf(fmt="%.2f%%",total_delta_santander))
        )
      )
    ))
    df_press() %>% 
      select(FAIXA_PRAZO,txMd.y,txmdxsan,DeltaSan) %>% 
      DT::datatable(options = list(dom = 't'), container = sketch, rownames= FALSE, extensions = 'Responsive') %>% 
      DT::formatPercentage(c('txMd.y','txmdxsan','DeltaSan'),2)
  })
  
  output$tbl_vol_cdb <- DT::renderDataTable({
    df_press_cdb() %>% 
      select(VolSantander) %>% 
      sum() -> total_sum_volumeSantander_cdb
    
    df_press_cdb() %>% 
      select(volXsantander) %>% 
      sum() -> total_sum_volumexsantander_cdb
    
    total_share_cdb <- total_sum_volumeSantander_cdb / (total_sum_volumeSantander_cdb + total_sum_volumexsantander_cdb) * 100
    
    sketch = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th("Faixa Prazo"),
          th("Santander"),
          th("Peer Groups"),
          th("Share")
        )
      ),
      tfoot(
        tr(
          th('Total'),
          th(sprintf(fmt="$%.2f",total_sum_volumeSantander_cdb)),
          th(sprintf(fmt="$%.2f",total_sum_volumexsantander_cdb)),
          th(sprintf(fmt="%.2f%%",total_share_cdb))
        )
      )
    ))
    df_press_cdb() %>% 
      select(FAIXA_PRAZO_AJUSTADO, VolSantander, volXsantander, ShareSantander) %>% 
      DT::datatable(options = list(dom = 't'), container = sketch, rownames=FALSE, extensions = 'Responsive') %>% 
      DT::formatCurrency(c('VolSantander','volXsantander')) %>% 
      DT::formatPercentage('ShareSantander',2)
  })
  
  output$tbl_tax_cdb <- DT::renderDataTable({
    df_press_cdb() %>% 
      select(soma.y) %>% 
      sum() -> total_volsan_cdb
    
    df_press_cdb() %>% 
      select(tx.y) %>% 
      sum() -> total_txvolsan_cdb
    
    df_press_cdb() %>% 
      select(tx.x) %>% 
      sum() -> total_txvolmkt_cdb
    
    df_press_cdb() %>% 
      select(soma.x) %>% 
      sum() -> total_volmkt_cdb
    
    total_txmd_santander_cdb <- total_txvolsan_cdb/total_volsan_cdb
    total_txmd_xsantander_cdb <- (total_txvolmkt_cdb - total_txvolsan_cdb)/(total_volmkt_cdb - total_volsan_cdb)
    total_delta_santander_cdb <- total_txmd_santander_cdb - total_txmd_xsantander_cdb
    
    sketch = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th("Faixa Prazo"),
          th("Santander"),
          th("Peer Groups"),
          th("Delta")
        )
      ),
      tfoot(
        tr(
          th("Total"),
          th(sprintf(fmt="%.2f%%",total_txmd_santander_cdb)),
          th(sprintf(fmt="%.2f%%",total_txmd_xsantander_cdb)),
          th(sprintf(fmt="%.2f%%",total_delta_santander_cdb))
        )
      )
    ))
    df_press_cdb() %>% 
      select(FAIXA_PRAZO_AJUSTADO,txmd.y,txmdxsan,DeltaSan) %>% 
      DT::datatable(options = list(dom = 't'), container = sketch, rownames= FALSE, extensions = 'Responsive') %>% 
      DT::formatPercentage(c('txmd.y','txmdxsan','DeltaSan'),2)
  })
  
# TABELAS INTERMEDIARIAS

  # TABELAS OC
  oc_mercado <- reactive({
     tbl_oc_mercado %>% 
      filter(TICKET %in% c("(H) De 10 a 20 milhões","(I) De 20 a 50 milhões", "(J) De 50 a 100 milhões","(K) Mais de 100 milhões"),
             ATIVO == 'DEB',
             INDEXADOR == 'DI',
             TIPO_ATIVO == 'Não Leasing',
             PEER == 'Grandes bancos de Varejo',
             DAT_REGISTRO == input$date) %>% 
      mutate(taxa = TAXA_MEDIA * VAL_FINANCEIRO) %>%
      group_by(FAIXA_PRAZO) %>% 
      summarise(soma = sum(VAL_FINANCEIRO), tx = sum(taxa), txMd = sum(taxa)/sum(VAL_FINANCEIRO)/100) %>% collect()
  })
  
  oc_santander <- reactive({
    tbl_oc_santander %>% 
      filter(TICKET %in% c("(H) De 10 a 20 milhões","(I) De 20 a 50 milhões", "(J) De 50 a 100 milhões","(K) Mais de 100 milhões"),
             ATIVO == 'DEB',
             INDEXADOR == 'DI',
             TIPO_ATIVO == 'Não Leasing',
             PEER == 'Grandes bancos de Varejo',
             DAT_REGISTRO == input$date) %>%
      mutate(taxa = TAXA_MEDIA * VAL_FINANCEIRO) %>%
      group_by(FAIXA_PRAZO) %>% 
      summarise(soma = sum(VAL_FINANCEIRO), tx = sum(taxa), txMd = sum(taxa)/sum(VAL_FINANCEIRO)/100) %>% collect()
  })
  
  df_press <- reactive({
    oc_mercado() %>% 
      left_join(oc_santander(), by='FAIXA_PRAZO') %>% 
      mutate_each(funs(replace(.,is.na(.),0))) %>% 
      mutate(volXsantander = (soma.x  - soma.y)/1000000, VolSantander = soma.y/1000000) %>% 
      mutate(ShareSantander = VolSantander / (volXsantander+VolSantander)) %>% 
      mutate(txmdxsan = ((tx.x - tx.y)/(soma.x-soma.y))/100) %>% 
      mutate(DeltaSan = txMd.y - txmdxsan) %>% 
      collect()
  })
  
  # TABELAS CDB
  cdb_mercado <- reactive({
    tbl_CDB_mercado %>% 
      filter(Peer == "Grandes bancos de Varejo",
             INDICE == "DI",
             TICKET_AJUSTADO %in% c("(H) De 10 a 20 milhões","(I) De 20 a 50 milhões", "(J) De 50 a 100 milhões","(K) Mais de 100 milhões"),
             EMISSOR == input$var1,
             DETENTOR == input$var2) %>% collect()
  })
  
  cdb_mercado2 <- reactive({
    cdb_mercado() %>% 
      unite(COD,CONDICAO_RESGATE,CONDICAO_ESPECIFICA,sep="") %>% 
      filter(COD %in% c("CONDICAO A MERCADONAO","TEM CONDICAOSIM"),
             DAT_REGISTRO == input$data) %>% 
      mutate(taxa = TAXA_MEDIA * VAL_FINANCEIRO_TOTAL) %>% 
      group_by(FAIXA_PRAZO_AJUSTADO) %>% 
      summarise(soma = sum(VAL_FINANCEIRO_TOTAL), tx = sum(taxa), txmd = sum(taxa)/sum(VAL_FINANCEIRO_TOTAL)/100)
  })
  
  cdb_santander <- reactive({
    tbl_CDB_santander %>% 
      filter(Peer == "Grandes bancos de Varejo",
             INDICE == "DI",
             TICKET_AJUSTADO %in% c("(H) De 10 a 20 milhões","(I) De 20 a 50 milhões", "(J) De 50 a 100 milhões","(K) Mais de 100 milhões"),
             EMISSOR == input$var1,
             DETENTOR == input$var2) %>% collect()
  })
  
  cdb_santander2 <- reactive({
    cdb_santander() %>% 
      unite(COD,CONDICAO_RESGATE,CONDICAO_ESPECIFICA,sep="") %>% 
      filter(COD %in% c("CONDICAO A MERCADONAO","TEM CONDICAOSIM"),
             DAT_REGISTRO == input$data) %>% 
      mutate(taxa = TAXA_MEDIA * VAL_FINANCEIRO_TOTAL) %>% 
      group_by(FAIXA_PRAZO_AJUSTADO) %>% 
      summarise(soma = sum(VAL_FINANCEIRO_TOTAL), tx = sum(taxa), txmd = sum(taxa)/sum(VAL_FINANCEIRO_TOTAL)/100)
  })
  
  df_press_cdb <- reactive({
    cdb_mercado2() %>% 
      left_join(cdb_santander2(), by='FAIXA_PRAZO_AJUSTADO') %>% 
      mutate_each(funs(replace(.,is.na(.),0))) %>% 
      mutate(volXsantander = (soma.x  - soma.y)/1000000, VolSantander = soma.y/1000000) %>% 
      mutate(ShareSantander = VolSantander / (volXsantander+VolSantander)) %>% 
      mutate(txmdxsan = ((tx.x - tx.y)/(soma.x-soma.y))/100) %>% 
      mutate(DeltaSan = txmd.y - txmdxsan)
  })
  
})
