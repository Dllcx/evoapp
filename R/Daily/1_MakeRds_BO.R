require(tidyverse)

#set database and tables
db <- src_sqlite("~/R/evoapp/DB/cetip_db.sqlite3", create = FALSE)
tbl_CDB_mercado   <- tbl(db,"b3_CDB_mercado")
tbl_CDB_santander <- tbl(db,"b3_CDB_santander")
tbl_oc_mercado    <- tbl(db,"b3_oc_mercado")
tbl_oc_santander  <- tbl(db,"b3_oc_santander")
tbl_bo_santander  <- tbl(db,"bo_daily")

# ---- Define Functions ----
oc_press_terc <- function(day,tbl_mercado, tbl_santander){
  tbl_mercado %>% 
    filter(TICKET %in% c("(H) De 10 a 20 milhões","(I) De 20 a 50 milhões", "(J) De 50 a 100 milhões","(K) Mais de 100 milhões"),
           ATIVO == 'DEB',
           INDEXADOR == 'DI',
           TIPO_ATIVO == 'Não Leasing',
           PEER == 'Grandes bancos de Varejo',
           DAT_REGISTRO == day) %>%
    mutate(VOl_Tx = VAL_FINANCEIRO * TAXA_MEDIA) %>% 
    group_by(FAIXA_PRAZO) %>% 
    summarise(VAL_FINANCEIRO, VOl_Tx) %>% collect() -> tbl_oc_1 
  
  tbl_santander %>% 
    filter(TICKET %in% c("(H) De 10 a 20 milhões","(I) De 20 a 50 milhões", "(J) De 50 a 100 milhões","(K) Mais de 100 milhões"),
           ATIVO == 'DEB',
           INDEXADOR == 'DI',
           TIPO_ATIVO == 'Não Leasing',
           PEER == 'Grandes bancos de Varejo',
           DAT_REGISTRO == day) %>%
    mutate(VOl_Tx = VAL_FINANCEIRO * TAXA_MEDIA) %>% 
    group_by(FAIXA_PRAZO) %>% 
    summarise(VAL_FINANCEIRO, VOl_Tx) %>% collect() -> tbl_oc_2
  
  tbl_oc_1 %>% 
    full_join(tbl_oc_2, by="FAIXA_PRAZO") %>% 
    mutate_all(funs(replace(.,is.na(.),0))) %>% 
    mutate(VOL = VAL_FINANCEIRO.x - VAL_FINANCEIRO.y, Tx = VOl_Tx.x - VOl_Tx.y) %>% 
    select(FAIXA_PRAZO,VOL, Tx) -> oc_press
  return(oc_press)
}
oc_press_prop <- function(day,tbl_mercado){
  tbl_mercado %>% 
    filter(TICKET %in% c("(H) De 10 a 20 milhões","(I) De 20 a 50 milhões", "(J) De 50 a 100 milhões","(K) Mais de 100 milhões"),
           ATIVO == 'DEB',
           INDEXADOR == 'DI',
           TIPO_ATIVO == 'Leasing',
           PEER == 'Grandes bancos de Varejo',
           DAT_REGISTRO == day) %>%
    mutate(VOL = VAL_FINANCEIRO, Tx = VAL_FINANCEIRO * TAXA_MEDIA) %>% 
    group_by(FAIXA_PRAZO) %>% 
    summarise(VOL, Tx) %>% 
    collect() -> tbl_oc_1 
  
  return(tbl_oc_1)
}
cdb_press_mc <- function(day,tbl_mercado, tbl_santander){
  tbl_mercado %>% 
    filter(Peer == "Grandes bancos de Varejo",
           INDICE == "DI",
           TICKET_AJUSTADO %in% c("(H) De 10 a 20 milhões","(I) De 20 a 50 milhões", "(J) De 50 a 100 milhões","(K) Mais de 100 milhões"),
           EMISSOR == "Mercado",
           DETENTOR == "Cliente") %>% 
    collect() %>% 
    unite(COD,CONDICAO_RESGATE,CONDICAO_ESPECIFICA,sep="") %>% 
    filter(COD %in% c("CONDICAO A MERCADONAO","TEM CONDICAOSIM"),
           DAT_REGISTRO == day) %>% 
    mutate(taxa = TAXA_MEDIA * VAL_FINANCEIRO_TOTAL) %>% 
    group_by(FAIXA_PRAZO_AJUSTADO) %>% 
    summarise(VOL_FIN = sum(VAL_FINANCEIRO_TOTAL), Tx_Mkt = sum(taxa)) -> tbl_cdb_1
  
  tbl_santander %>% 
    filter(Peer == "Grandes bancos de Varejo",
           INDICE == "DI",
           TICKET_AJUSTADO %in% c("(H) De 10 a 20 milhões","(I) De 20 a 50 milhões", "(J) De 50 a 100 milhões","(K) Mais de 100 milhões"),
           EMISSOR == "Mercado",
           DETENTOR == "Cliente") %>% 
    collect() %>%
    unite(COD,CONDICAO_RESGATE,CONDICAO_ESPECIFICA,sep="") %>% 
    filter(COD %in% c("CONDICAO A MERCADONAO","TEM CONDICAOSIM"),
           DAT_REGISTRO == day) %>% 
    mutate(taxa = TAXA_MEDIA * VAL_FINANCEIRO_TOTAL) %>% 
    group_by(FAIXA_PRAZO_AJUSTADO) %>% 
    summarise(VOL_FIN = sum(VAL_FINANCEIRO_TOTAL), Tx_San = sum(taxa)) -> tbl_cdb_2
  
  tbl_cdb_1 %>% 
    full_join(tbl_cdb_2, by='FAIXA_PRAZO_AJUSTADO') %>% 
    mutate_all(funs(replace(.,is.na(.),0))) %>% 
    mutate(VOL = VOL_FIN.x - VOL_FIN.y, Tx = Tx_Mkt - Tx_San) %>% 
    select(FAIXA_PRAZO_AJUSTADO, VOL, Tx) -> cdb_press
  
  return(cdb_press)
}
cdb_press_mm <- function(day, tbl_mercado, tbl_santander){
  tbl_mercado %>% 
    filter(Peer == "Grandes bancos de Varejo",
           INDICE == "DI",
           EMISSOR == "Mercado",
           DETENTOR == "Mercado") %>% 
    collect() %>%
    unite(COD,CONDICAO_RESGATE,CONDICAO_ESPECIFICA,sep="") %>% 
    filter(COD %in% c("CONDICAO A MERCADONAO","TEM CONDICAOSIM"),
           DAT_REGISTRO == day) %>% 
    mutate(taxa = TAXA_MEDIA * VAL_FINANCEIRO_TOTAL) %>% 
    group_by(FAIXA_PRAZO_AJUSTADO) %>% 
    summarise(Vol_mkt = sum(VAL_FINANCEIRO_TOTAL), Tx_mkt = sum(taxa)) -> cdb_mercado_m2
  
  tbl_santander %>% 
    filter(Peer == "Grandes bancos de Varejo",
           INDICE == "DI",
           EMISSOR == "Mercado",
           DETENTOR == "Mercado") %>% 
    collect() %>% 
    unite(COD,CONDICAO_RESGATE,CONDICAO_ESPECIFICA,sep="") %>% 
    filter(COD %in% c("CONDICAO A MERCADONAO","TEM CONDICAOSIM"),
           DAT_REGISTRO == day) %>% 
    mutate(taxa = TAXA_MEDIA * VAL_FINANCEIRO_TOTAL) %>% 
    group_by(FAIXA_PRAZO_AJUSTADO) %>% 
    summarise(Vol_san = sum(VAL_FINANCEIRO_TOTAL), Tx_san = sum(taxa)) -> cdb_santander_m2
  
  cdb_mercado_m2 %>% 
    full_join(cdb_santander_m2, by='FAIXA_PRAZO_AJUSTADO') %>% 
    mutate_all(funs(replace(.,is.na(.),0))) %>% 
    mutate(VOL = Vol_mkt - Vol_san, Tx = Tx_mkt - Tx_san) %>% 
    select(FAIXA_PRAZO_AJUSTADO, VOL, Tx) -> cdb_press
  
  return(cdb_press)
}
oc_bo <- function(day,bo_santander){
  bo_santander %>% 
    filter(DT_CONT == day,
           DESC_SUB_PROD == "COMPROMISSADA DI") %>% 
    mutate(Tx = VOL * Per_index) %>% 
    group_by(FAIXA_PRAZO) %>% 
    summarise(VOL = sum(VOL), Tx = sum(Tx)) %>% 
    collect() -> bo_san
  return(bo_san)
}
cdb_mktCli_bo <- function(day, bo_santander){
  bo_santander %>% 
    filter(DT_CONT == day,
           DESC_SUB_PROD %in% c("CDB DI", "CDB DI CORPORATE"),
           INST == "Cliente") %>% 
    mutate(FAIXA_PRAZO_AJUSTADO = FAIXA_PRAZO, Tx = VOL * Per_index) %>% 
    group_by(FAIXA_PRAZO_AJUSTADO) %>% 
    summarise(VOL = sum(VOL), Tx = sum(Tx)) %>% 
    collect() -> bo_san
  return(bo_san)
}
cdb_mktMkt_bo <- function(day, bo_santander){
  bo_santander %>% 
    filter(DT_CONT == day,
           DESC_SUB_PROD %in% c("CDB DI", "CDB DI CORPORATE"),
           INST == "Institucionais") %>% 
    mutate(FAIXA_PRAZO_AJUSTADO = FAIXA_PRAZO, Tx = VOL * Per_index) %>% 
    group_by(FAIXA_PRAZO_AJUSTADO) %>% 
    summarise(VOL = sum(VOL), Tx = sum(Tx)) %>% 
    collect() -> bo_san
  return(bo_san)
}

# ---- Grep base date vector ----
dt_vct <- tbl_oc_mercado %>% distinct(DAT_REGISTRO) %>% filter(DAT_REGISTRO >= '2017-05-02') %>% collect()

# ---- Loop Creation ----

for (i in 1:nrow(dt_vct)){
  day <- paste(dt_vct[i,])
  if(i==1){
    # Colect values in DB
    mkt <- oc_press_terc(day,tbl_oc_mercado, tbl_oc_santander)
    mkt_cdb <- cdb_press_mc(day,tbl_CDB_mercado,tbl_CDB_santander)
    mkt_cdb2 <- cdb_press_mm(day,tbl_CDB_mercado,tbl_CDB_santander)
    san <- oc_bo(day,tbl_bo_santander)
    san_cdb <- cdb_mktCli_bo(day,tbl_bo_santander)
    san_cdb2 <- cdb_mktMkt_bo(day,tbl_bo_santander)
    
    # Share Calculation
    total_share      <- (san %>% select(VOL) %>% sum())/((mkt %>% select(VOL) %>% sum()) + (san %>% select(VOL) %>% sum()))
    total_share_cdb  <- (san_cdb %>% select(VOL) %>% sum())/((mkt_cdb %>% select(VOL) %>% sum()) + (san_cdb %>% select(VOL) %>% sum()))
    
    # Delta Calculation
    total_delta_santander      <- ((san %>% select(Tx) %>% sum()) / (san %>% select(VOL) %>% sum())) - ((mkt %>% select(Tx) %>% sum())/(mkt %>% select(VOL) %>% sum()))
    total_delta_santander_cdb  <- ((san_cdb %>% select(Tx) %>% sum()) / (san_cdb %>% select(VOL) %>% sum())) - ((mkt_cdb %>% select(Tx) %>% sum())/(mkt_cdb %>% select(VOL) %>% sum()))
    
    # Vetor Composer
    vect_share_oc   <- total_share * 100
    vect_delta_oc   <- total_delta_santander
    vect_share_cdb  <- total_share_cdb * 100
    vect_delta_cdb  <- total_delta_santander_cdb
    
    # nrow Logic for mktmkt
    if(nrow(san_cdb2)==0){
      vect_share_cdb2 <- 0
      vect_delta_cdb2 <- 0
    }
    else{
      # Calculation for MktMkt
      total_share_cdb2 <- (san_cdb2 %>% select(VOL) %>% sum())/((mkt_cdb2 %>% select(VOL) %>% sum()) + (san_cdb2 %>% select(VOL) %>% sum()))
      total_delta_santander_cdb2 <- ((san_cdb2 %>% select(Tx) %>% sum()) / (san_cdb2 %>% select(VOL) %>% sum())) - ((mkt_cdb2 %>% select(Tx) %>% sum())/(mkt_cdb2 %>% select(VOL) %>% sum()))
      
      vect_share_cdb2 <- total_share_cdb2 * 100
      vect_delta_cdb2 <- total_delta_santander_cdb2
    }
  }
  else{
    # Colect values in DB
    mkt <- oc_press_terc(day,tbl_oc_mercado, tbl_oc_santander)
    mkt_cdb <- cdb_press_mc(day,tbl_CDB_mercado,tbl_CDB_santander)
    mkt_cdb2 <- cdb_press_mm(day,tbl_CDB_mercado,tbl_CDB_santander)
    san <- oc_bo(day,tbl_bo_santander)
    san_cdb <- cdb_mktCli_bo(day,tbl_bo_santander)
    san_cdb2 <- cdb_mktMkt_bo(day,tbl_bo_santander)
    
    # Share Calculation
    total_share      <- (san %>% select(VOL) %>% sum())/((mkt %>% select(VOL) %>% sum()) + (san %>% select(VOL) %>% sum()))
    total_share_cdb  <- (san_cdb %>% select(VOL) %>% sum())/((mkt_cdb %>% select(VOL) %>% sum()) + (san_cdb %>% select(VOL) %>% sum()))
    
    
    # Delta Calculation
    total_delta_santander      <- ((san %>% select(Tx) %>% sum()) / (san %>% select(VOL) %>% sum())) - ((mkt %>% select(Tx) %>% sum())/(mkt %>% select(VOL) %>% sum()))
    total_delta_santander_cdb  <- ((san_cdb %>% select(Tx) %>% sum()) / (san_cdb %>% select(VOL) %>% sum())) - ((mkt_cdb %>% select(Tx) %>% sum())/(mkt_cdb %>% select(VOL) %>% sum()))
    
    # Vector Append
    vect_share_oc <- append(vect_share_oc,total_share * 100)
    vect_delta_oc <- append(vect_delta_oc,total_delta_santander)
    vect_share_cdb <- append(vect_share_cdb,total_share_cdb * 100)
    vect_delta_cdb <- append(vect_delta_cdb,total_delta_santander_cdb)
    
    # nrow Logic for mktmkt
    if(nrow(san_cdb2)==0){
      vect_share_cdb2 <- append(vect_share_cdb2,0)
      vect_delta_cdb2 <- append(vect_delta_cdb2,0)
    }
    else{
      # Calculation for MktMkt
      total_share_cdb2 <- (san_cdb2 %>% select(VOL) %>% sum())/((mkt_cdb2 %>% select(VOL) %>% sum()) + (san_cdb2 %>% select(VOL) %>% sum()))
      total_delta_santander_cdb2 <- ((san_cdb2 %>% select(Tx) %>% sum()) / (san_cdb2 %>% select(VOL) %>% sum())) - ((mkt_cdb2 %>% select(Tx) %>% sum())/(mkt_cdb2 %>% select(VOL) %>% sum()))
      
      vect_share_cdb2 <- append(vect_share_cdb2,total_share_cdb2 * 100)
      vect_delta_cdb2 <- append(vect_delta_cdb2,total_delta_santander_cdb2)
    }
  }
}
vect <- cbind(vect_share_oc,vect_delta_oc,vect_share_cdb,vect_delta_cdb,vect_share_cdb2,vect_delta_cdb2)

if(file.exists('~/R/evoapp/DB/plot_vectors_.rds')) file.remove('~/R/evoapp/DB/plot_vectors_.rds')
saveRDS(vect,'~/R/evoapp/DB/plot_vectors_.rds')
rm(list=ls())
gc()
print("base de vetores atualizada.")
