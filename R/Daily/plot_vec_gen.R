require(tidyverse)

rm(list = ls())

setwd('~/R/tsy_DailyReport/DB/')

fl <- 'plot_vectors.rds'
if(file.exists(fl)) file.remove(fl)

db <- src_sqlite("cetip_db.sqlite3", create = FALSE)

# OC

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

# CDB MC

db <- src_sqlite("cetip_db.sqlite3", create = FALSE)
tbl_CDB_mercado <- tbl(db,"b3_CDB_mercado")
tbl_CDB_santander <- tbl(db,"b3_CDB_santander")

dt_vct <- tbl_CDB_mercado %>% distinct(DAT_REGISTRO) %>% filter(DAT_REGISTRO >= '2016-12-02') %>% collect()

for(i in 1:nrow(dt_vct)){
  
  v1 <- paste(dt_vct[i,])
  
  tbl_CDB_mercado %>% 
    filter(PEER == "Grandes bancos de Varejo",
           INDICE == "DI",
           TICKET_AJUSTADO %in% c("(H) De 10 a 20 milhões","(I) De 20 a 50 milhões", "(J) De 50 a 100 milhões","(K) Mais de 100 milhões"),
           EMISSOR == "Mercado",
           DETENTOR == "Cliente") %>% collect() -> df_cdb_mercado
  df_cdb_mercado %>% 
    unite(COD,CONDICAO_RESGATE,CONDICAO_ESPECIFICA,sep="") %>% 
    filter(COD %in% c("CONDICAO A MERCADONAO","TEM CONDICAOSIM"),
           DAT_REGISTRO == v1) %>% 
    mutate(taxa = TAXA_MEDIA * VAL_FINANCEIRO_TOTAL) %>% 
    group_by(FAIXA_PRAZO_AJUSTADO) %>% 
    summarise(soma = sum(VAL_FINANCEIRO_TOTAL), tx = sum(taxa), txmd = sum(taxa)/sum(VAL_FINANCEIRO_TOTAL)/100) -> df_cdb_mercado2
  
  tbl_CDB_santander %>% 
    filter(PEER == "Grandes bancos de Varejo",
           INDICE == "DI",
           TICKET_AJUSTADO %in% c("(H) De 10 a 20 milhões","(I) De 20 a 50 milhões", "(J) De 50 a 100 milhões","(K) Mais de 100 milhões"),
           EMISSOR == "Mercado",
           DETENTOR == "Cliente") %>% collect() -> df_cdb_santander
  df_cdb_santander %>% 
    unite(COD,CONDICAO_RESGATE,CONDICAO_ESPECIFICA,sep="") %>% 
    filter(COD %in% c("CONDICAO A MERCADONAO","TEM CONDICAOSIM"),
           DAT_REGISTRO == v1) %>% 
    mutate(taxa = TAXA_MEDIA * VAL_FINANCEIRO_TOTAL) %>% 
    group_by(FAIXA_PRAZO_AJUSTADO) %>% 
    summarise(soma = sum(VAL_FINANCEIRO_TOTAL), tx = sum(taxa), txmd = sum(taxa)/sum(VAL_FINANCEIRO_TOTAL)/100) -> df_cdb_santander2
  
  df_cdb_mercado2 %>% 
    left_join(df_cdb_santander2, by='FAIXA_PRAZO_AJUSTADO') %>% 
    mutate_each(funs(replace(.,is.na(.),0))) %>% 
    mutate(volXsantander = (soma.x  - soma.y), VolSantander = soma.y) %>% 
    mutate(ShareSantander = VolSantander / (volXsantander+VolSantander)) %>% 
    mutate(txmdxsan = ((tx.x - tx.y)/(soma.x-soma.y))/100) %>% 
    mutate(DeltaSan = txmd.y - txmdxsan) -> df_press_cdb
  
  if(i==1){
    # Totais
    df_press_cdb %>% 
      select(VolSantander) %>% 
      sum() -> total_sum_volumeSantander_cdb
    
    df_press_cdb %>% 
      select(volXsantander) %>% 
      sum() -> total_sum_volumexsantander_cdb
    
    total_share_cdb <- (total_sum_volumeSantander_cdb / (total_sum_volumeSantander_cdb + total_sum_volumexsantander_cdb))*100
    
    df_press_cdb %>% 
      select(soma.y) %>% 
      sum() -> total_volsan_cdb
    
    df_press_cdb %>% 
      select(tx.y) %>% 
      sum() -> total_txvolsan_cdb
    
    df_press_cdb %>% 
      select(tx.x) %>% 
      sum() -> total_txvolmkt_cdb
    
    df_press_cdb %>% 
      select(soma.x) %>% 
      sum() -> total_volmkt_cdb
    
    total_txmd_santander_cdb <- total_txvolsan_cdb/total_volsan_cdb
    total_txmd_xsantander_cdb <- (total_txvolmkt_cdb - total_txvolsan_cdb)/(total_volmkt_cdb - total_volsan_cdb)
    
    total_delta_santander_cdb <- (total_txmd_santander_cdb - total_txmd_xsantander_cdb)
    
    vect_share_cdb <- total_share_cdb
    vect_delta_cdb <- total_delta_santander_cdb
    
  }
  
  else{
    df_press_cdb %>% 
      select(VolSantander) %>% 
      sum() -> total_sum_volumeSantander_cdb
    
    df_press_cdb %>% 
      select(volXsantander) %>% 
      sum() -> total_sum_volumexsantander_cdb
    
    total_share_cdb <- (total_sum_volumeSantander_cdb / (total_sum_volumeSantander_cdb + total_sum_volumexsantander_cdb))*100
    
    df_press_cdb %>% 
      select(soma.y) %>% 
      sum() -> total_volsan_cdb
    
    df_press_cdb %>% 
      select(tx.y) %>% 
      sum() -> total_txvolsan_cdb
    
    df_press_cdb %>% 
      select(tx.x) %>% 
      sum() -> total_txvolmkt_cdb
    
    df_press_cdb %>% 
      select(soma.x) %>% 
      sum() -> total_volmkt_cdb
    
    total_txmd_santander_cdb <- total_txvolsan_cdb/total_volsan_cdb
    total_txmd_xsantander_cdb <- (total_txvolmkt_cdb - total_txvolsan_cdb)/(total_volmkt_cdb - total_volsan_cdb)
    total_delta_santander_cdb <- (total_txmd_santander_cdb - total_txmd_xsantander_cdb)
    
    vect_share_cdb <- append(vect_share_cdb,total_share_cdb)
    vect_delta_cdb <- append(vect_delta_cdb,total_delta_santander_cdb)
  }
}

base <- read_rds('plot_vectors.rds')
base <- cbind(base,vect_share_cdb,vect_delta_cdb)
saveRDS(base,'plot_vectors.rds')

rm(list = ls())

gc()

# CDB MM

db <- src_sqlite("cetip_db.sqlite3", create = FALSE)
tbl_CDB_mercado <- tbl(db,"b3_CDB_mercado")
tbl_CDB_santander <- tbl(db,"b3_CDB_santander")

dt_vct <- tbl_CDB_mercado %>% distinct(DAT_REGISTRO) %>% filter(DAT_REGISTRO >= '2016-12-02') %>% collect()

for(i in 1:nrow(dt_vct)){
  
  v1 <- paste(dt_vct[i,])
  
  tbl_CDB_mercado %>% 
    filter(Peer == "Grandes bancos de Varejo",
           INDICE == "DI",
           EMISSOR == "Mercado",
           DETENTOR == "Mercado") %>% collect() -> df_cdb_mercado
  df_cdb_mercado %>% 
    unite(COD,CONDICAO_RESGATE,CONDICAO_ESPECIFICA,sep="") %>% 
    filter(DAT_REGISTRO == v1) %>% 
    mutate(taxa = TAXA_MEDIA * VAL_FINANCEIRO_TOTAL) %>% 
    group_by(FAIXA_PRAZO_AJUSTADO) %>% 
    summarise(soma = sum(VAL_FINANCEIRO_TOTAL), tx = sum(taxa), txmd = sum(taxa)/sum(VAL_FINANCEIRO_TOTAL)/100) -> df_cdb_mercado2
  
  tbl_CDB_santander %>% 
    filter(Peer == "Grandes bancos de Varejo",
           INDICE == "DI",
           EMISSOR == "Mercado",
           DETENTOR == "Mercado") %>% collect() -> df_cdb_santander
  df_cdb_santander %>% 
    unite(COD,CONDICAO_RESGATE,CONDICAO_ESPECIFICA,sep="") %>% 
    filter(DAT_REGISTRO == v1) %>% 
    mutate(taxa = TAXA_MEDIA * VAL_FINANCEIRO_TOTAL) %>% 
    group_by(FAIXA_PRAZO_AJUSTADO) %>% 
    summarise(soma = sum(VAL_FINANCEIRO_TOTAL), tx = sum(taxa), txmd = sum(taxa)/sum(VAL_FINANCEIRO_TOTAL)/100) -> df_cdb_santander2
  
  df_cdb_mercado2 %>% 
    left_join(df_cdb_santander2, by='FAIXA_PRAZO_AJUSTADO') %>% 
    mutate_each(funs(replace(.,is.na(.),0))) %>% 
    mutate(volXsantander = (soma.x  - soma.y), VolSantander = soma.y) %>% 
    mutate(ShareSantander = VolSantander / (volXsantander+VolSantander)) %>% 
    mutate(txmdxsan = ((tx.x - tx.y)/(soma.x-soma.y))/100) %>% 
    mutate(DeltaSan = txmd.y - txmdxsan) -> df_press_cdb
  
  if(i==1){
    # Totais
    
    if(nrow(df_cdb_mercado2)==0){
      
      vect_share_cdb2 <- 0
      vect_delta_cdb2 <- 0
      
    } else{
      
      df_press_cdb %>% 
        select(VolSantander) %>% 
        sum() -> total_sum_volumeSantander_cdb
      
      df_press_cdb %>% 
        select(volXsantander) %>% 
        sum() -> total_sum_volumexsantander_cdb
      
      total_share_cdb <- (total_sum_volumeSantander_cdb / (total_sum_volumeSantander_cdb + total_sum_volumexsantander_cdb))*100
      
      df_press_cdb %>% 
        select(soma.y) %>% 
        sum() -> total_volsan_cdb
      
      df_press_cdb %>% 
        select(tx.y) %>% 
        sum() -> total_txvolsan_cdb
      
      df_press_cdb %>% 
        select(tx.x) %>% 
        sum() -> total_txvolmkt_cdb
      
      df_press_cdb %>% 
        select(soma.x) %>% 
        sum() -> total_volmkt_cdb
      
      total_txmd_santander_cdb <- total_txvolsan_cdb/total_volsan_cdb
      total_txmd_xsantander_cdb <- (total_txvolmkt_cdb - total_txvolsan_cdb)/(total_volmkt_cdb - total_volsan_cdb)
      
      total_delta_santander_cdb <- (total_txmd_santander_cdb - total_txmd_xsantander_cdb)
      
      vect_share_cdb2 <- total_share_cdb
      vect_delta_cdb2 <- total_delta_santander_cdb
    }
  }
  
  else{
    
    if(nrow(df_cdb_mercado2)==0){
      
      vect_share_cdb2 <- append(vect_share_cdb2,0)
      vect_delta_cdb2 <- append(vect_delta_cdb2,0)
      
    } else{
      
      df_press_cdb %>% 
        select(VolSantander) %>% 
        sum() -> total_sum_volumeSantander_cdb
      
      df_press_cdb %>% 
        select(volXsantander) %>% 
        sum() -> total_sum_volumexsantander_cdb
      
      total_share_cdb <- (total_sum_volumeSantander_cdb / (total_sum_volumeSantander_cdb + total_sum_volumexsantander_cdb))*100
      
      df_press_cdb %>% 
        select(soma.y) %>% 
        sum() -> total_volsan_cdb
      
      df_press_cdb %>% 
        select(tx.y) %>% 
        sum() -> total_txvolsan_cdb
      
      df_press_cdb %>% 
        select(tx.x) %>% 
        sum() -> total_txvolmkt_cdb
      
      df_press_cdb %>% 
        select(soma.x) %>% 
        sum() -> total_volmkt_cdb
      
      total_txmd_santander_cdb <- total_txvolsan_cdb/total_volsan_cdb
      total_txmd_xsantander_cdb <- (total_txvolmkt_cdb - total_txvolsan_cdb)/(total_volmkt_cdb - total_volsan_cdb)
      total_delta_santander_cdb <- (total_txmd_santander_cdb - total_txmd_xsantander_cdb)
      
      vect_share_cdb2 <- append(vect_share_cdb2,total_share_cdb)
      vect_delta_cdb2 <- append(vect_delta_cdb2,total_delta_santander_cdb)
    }
  }
}

base <- read_rds('plot_vectors.rds')
base <- cbind(base,vect_share_cdb2,vect_delta_cdb2)
saveRDS(base,'plot_vectors.rds')
rm(list=ls())

gc()

print("base de vetores atualizada.")