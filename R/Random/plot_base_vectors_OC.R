require(tidyverse)

# Vector for ploting Compromissadas share

setwd("~/R/tsy_DailyReport/DB/")
db <- src_sqlite("cetip_db.sqlite3", create = FALSE)
tbl_oc_mercado <- tbl(db,"b3_oc_mercado")
tbl_oc_santander <- tbl(db,"b3_oc_santander")

dt_vct <- tbl_oc_mercado %>% distinct(DAT_REGISTRO) %>% filter(DAT_REGISTRO >= '2016-12-02') %>% collect()

for(i in 1:nrow(dt_vct)){
  
  v1 <- paste(dt_vct[i,])
  
  tbl_oc_mercado %>% 
    filter(TICKET %in% c("(H) De 10 a 20 milhões","(I) De 20 a 50 milhões", "(J) De 50 a 100 milhões","(K) Mais de 100 milhões"),
           ATIVO == 'DEB',
           INDEXADOR == 'DI',
           TIPO_ATIVO == 'Não Leasing',
           PEER == 'Grandes bancos de Varejo',
           DAT_REGISTRO == v1) %>%
    mutate(taxa = TAXA_MEDIA * VAL_FINANCEIRO) %>%
    group_by(FAIXA_PRAZO) %>% 
    summarise(soma = sum(VAL_FINANCEIRO), tx = sum(taxa), txMd = sum(taxa)/sum(VAL_FINANCEIRO)/100) %>% collect() -> df_mercado
  
  tbl_oc_santander %>% 
    filter(TICKET %in% c("(H) De 10 a 20 milhões","(I) De 20 a 50 milhões", "(J) De 50 a 100 milhões","(K) Mais de 100 milhões"),
           ATIVO == 'DEB',
           INDEXADOR == 'DI',
           TIPO_ATIVO == 'Não Leasing',
           PEER == 'Grandes bancos de Varejo',
           DAT_REGISTRO == v1) %>%
    mutate(taxa = TAXA_MEDIA * VAL_FINANCEIRO) %>%
    group_by(FAIXA_PRAZO) %>% 
    summarise(soma = sum(VAL_FINANCEIRO), tx = sum(taxa), txMd = sum(taxa)/sum(VAL_FINANCEIRO)/100) %>% collect() -> df_santander
  
  df_mercado %>% 
    left_join(df_santander, by='FAIXA_PRAZO') %>% 
    mutate_each(funs(replace(.,is.na(.),0))) %>% 
    mutate(volXsantander = (soma.x  - soma.y)/1000000, VolSantander = soma.y/1000000) %>% 
    mutate(ShareSantander = VolSantander / (volXsantander+VolSantander)) %>% 
    mutate(txmdxsan = ((tx.x - tx.y)/(soma.x-soma.y))/100) %>% 
    mutate(DeltaSan = txMd.y - txmdxsan) %>% 
    collect() -> df_press
  
  if(i==1){
    # Totais
    df_press %>% 
      select(VolSantander) %>% 
      sum() -> total_sum_volumeSantander
    
    df_press %>% 
      select(volXsantander) %>% 
      sum() -> total_sum_volumexsantander
    
    total_share <- (total_sum_volumeSantander / (total_sum_volumeSantander + total_sum_volumexsantander))*100
    
    df_press %>% 
      select(soma.y) %>% 
      sum() -> total_volsan
    
    df_press %>% 
      select(tx.y) %>% 
      sum() -> total_txvolsan
    
    df_press %>% 
      select(tx.x) %>% 
      sum() -> total_txvolmkt
    
    df_press %>% 
      select(soma.x) %>% 
      sum() -> total_volmkt
    
    total_txmd_santander <- total_txvolsan/total_volsan
    total_txmd_xsantander <- (total_txvolmkt - total_txvolsan)/(total_volmkt - total_volsan)
    total_delta_santander <- (total_txmd_santander - total_txmd_xsantander)
    
    vect_share_oc <- total_share
    vect_delta_oc <- total_delta_santander
    
  } 
  
  else {
    # Totais
    df_press %>% 
      select(VolSantander) %>% 
      sum() -> total_sum_volumeSantander
    
    df_press %>% 
      select(volXsantander) %>% 
      sum() -> total_sum_volumexsantander
    
    total_share <- (total_sum_volumeSantander / (total_sum_volumeSantander + total_sum_volumexsantander))*100
    
    df_press %>% 
      select(soma.y) %>% 
      sum() -> total_volsan
    
    df_press %>% 
      select(tx.y) %>% 
      sum() -> total_txvolsan
    
    df_press %>% 
      select(tx.x) %>% 
      sum() -> total_txvolmkt
    
    df_press %>% 
      select(soma.x) %>% 
      sum() -> total_volmkt
    
    total_txmd_santander <- total_txvolsan/total_volsan
    total_txmd_xsantander <- (total_txvolmkt - total_txvolsan)/(total_volmkt - total_volsan)
    total_delta_santander <- (total_txmd_santander - total_txmd_xsantander)
    
    vect_share_oc <- append(vect_share_oc,total_share)
    vect_delta_oc <- append(vect_delta_oc,total_delta_santander)
  }

}

vect <- cbind(vect_share_oc,vect_delta_oc)
saveRDS(vect,'plot_vectors.rds')

rm(list=ls())

gc()

