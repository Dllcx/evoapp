get_day <- function(){
  dt <- readline(prompt="Inserir data (deixar em branco para ultima data disponivel): ")
  if(dt == ""){
    dt <- Sys.Date()
    if(weekdays(dt)=="segunda-feira"){
      cdata <- dt - 3
    }
    else{
      cdata <- dt - 1
    }
  }
  else{
    cdata <- dt
  }
  return(as.character(cdata))
}

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

share_san <- function(tbl_mercado_cdb,tbl_santander_cdb,tbl_mercado_oc,tbl_santander_oc,bo_santander){
  tbl_mercado_cdb %>% 
    filter(Peer == "Grandes bancos de Varejo",
           INDICE == "DI",
           TICKET_AJUSTADO %in% c("(H) De 10 a 20 milhões","(I) De 20 a 50 milhões", "(J) De 50 a 100 milhões","(K) Mais de 100 milhões"),
           EMISSOR == "Mercado",
           DETENTOR == "Cliente",
           DAT_REGISTRO >= '2016-12-02') %>% 
    collect() %>% 
    unite(COD,CONDICAO_RESGATE,CONDICAO_ESPECIFICA,sep="") %>% 
    filter(COD %in% c("CONDICAO A MERCADONAO","TEM CONDICAOSIM")) %>%
    group_by(DAT_REGISTRO) %>% 
    summarise(VOL_FIN = sum(VAL_FINANCEIRO_TOTAL)) -> full_mkt
  
  tbl_santander_cdb %>% 
    filter(Peer == "Grandes bancos de Varejo",
           INDICE == "DI",
           TICKET_AJUSTADO %in% c("(H) De 10 a 20 milhões","(I) De 20 a 50 milhões", "(J) De 50 a 100 milhões","(K) Mais de 100 milhões"),
           EMISSOR == "Mercado",
           DETENTOR == "Cliente",
           DAT_REGISTRO >= '2016-12-02') %>% 
    collect() %>%
    unite(COD,CONDICAO_RESGATE,CONDICAO_ESPECIFICA,sep="") %>% 
    filter(COD %in% c("CONDICAO A MERCADONAO","TEM CONDICAOSIM")) %>% 
    group_by(DAT_REGISTRO) %>% 
    summarise(VOL_FIN = sum(VAL_FINANCEIRO_TOTAL)) -> cdb_san
  
  full_mkt %>% 
    full_join(cdb_san, by='DAT_REGISTRO') %>% 
    mutate(VOL_CDB = VOL_FIN.x - VOL_FIN.y) %>% 
    select(DAT_REGISTRO,VOL_CDB) %>% 
    collect() -> cdb_mkt
  
  tbl_mercado_oc %>% 
    filter(TICKET %in% c("(H) De 10 a 20 milhões","(I) De 20 a 50 milhões", "(J) De 50 a 100 milhões","(K) Mais de 100 milhões"),
           ATIVO == 'DEB',
           INDEXADOR == 'DI',
           TIPO_ATIVO == 'Leasing',
           PEER == 'Grandes bancos de Varejo') %>%
    mutate(VOL = VAL_FINANCEIRO, Tx = VAL_FINANCEIRO * TAXA_MEDIA) %>% 
    group_by(DAT_REGISTRO) %>% 
    summarise(VOL_OC = sum(VOL)) %>% 
    collect() -> full_oc_prop_mkt
  
  tbl_santander_oc %>% 
    filter(TICKET %in% c("(H) De 10 a 20 milhões","(I) De 20 a 50 milhões", "(J) De 50 a 100 milhões","(K) Mais de 100 milhões"),
           ATIVO == 'DEB',
           INDEXADOR == 'DI',
           TIPO_ATIVO == 'Leasing',
           PEER == 'Grandes bancos de Varejo') %>%
    mutate(VOL = VAL_FINANCEIRO, Tx = VAL_FINANCEIRO * TAXA_MEDIA) %>% 
    group_by(DAT_REGISTRO) %>% 
    summarise(VOL_OC = sum(VOL)) %>% 
    collect() -> oc_prop_san
  
  full_oc_prop_mkt %>% 
    full_join(oc_prop_san, by='DAT_REGISTRO') %>% 
    mutate_all(funs(replace(.,is.na(.),0))) %>% 
    mutate(VOL_OC = VOL_OC.x - VOL_OC.y) %>% 
    select(DAT_REGISTRO,VOL_OC) %>% 
    collect() -> oc_prop_mkt
  
  cdb_mkt %>% 
    full_join(oc_prop_mkt, by='DAT_REGISTRO') %>% 
    mutate_all(funs(replace(.,is.na(.),0))) %>% 
    mutate(VOL = VOL_CDB + VOL_OC) %>% 
    select(DAT_REGISTRO,VOL, VOL_CDB, VOL_OC) %>% 
    collect() -> tbl_mkt
  
  bo_santander %>% 
    filter(DESC_SUB_PROD %in% c("CDB DI", "CDB DI CORPORATE"), 
           INST == "Cliente") %>% 
    group_by(DT_CONT) %>% 
    summarise(DAT_REGISTRO = DT_CONT,VOL = sum(VOL)) %>% 
    select(DAT_REGISTRO, VOL) %>% 
    collect() -> san_bo
  
  san_bo %>% 
    full_join(tbl_mkt, by='DAT_REGISTRO') %>% 
    mutate(Share = VOL.x / (VOL.y + VOL.x)) %>% 
    collect() -> hist_share
  
  return(hist_share)
}

make_mktmkt_plot <- function(tbl_mercado, tbl_santander, bo_santander){
  tbl_mercado %>% 
    filter(Peer == "Grandes bancos de Varejo",
           INDICE == "DI",
           EMISSOR == "Mercado",
           DETENTOR == "Mercado",
           DAT_REGISTRO >= '2017-05-02') %>% 
    collect() %>%
    unite(COD,CONDICAO_RESGATE,CONDICAO_ESPECIFICA,sep="") %>% 
    filter(COD %in% c("CONDICAO A MERCADONAO","TEM CONDICAOSIM")) %>% 
    mutate(taxa = TAXA_MEDIA * VAL_FINANCEIRO_TOTAL) %>% 
    group_by(DAT_REGISTRO) %>% 
    summarise(Vol_mkt = sum(VAL_FINANCEIRO_TOTAL), Tx_mkt = sum(taxa)) -> cdb_mercado_m2
  
  tbl_santander %>% 
    filter(Peer == "Grandes bancos de Varejo",
           INDICE == "DI",
           EMISSOR == "Mercado",
           DETENTOR == "Mercado",
           DAT_REGISTRO >= '2017-05-02') %>% 
    collect() %>% 
    unite(COD,CONDICAO_RESGATE,CONDICAO_ESPECIFICA,sep="") %>% 
    filter(COD %in% c("CONDICAO A MERCADONAO","TEM CONDICAOSIM")) %>% 
    mutate(taxa = TAXA_MEDIA * VAL_FINANCEIRO_TOTAL) %>% 
    group_by(DAT_REGISTRO) %>% 
    summarise(Vol_san = sum(VAL_FINANCEIRO_TOTAL), Tx_san = sum(taxa)) -> cdb_santander_m2
  
  cdb_mercado_m2 %>% 
    full_join(cdb_santander_m2, by = 'DAT_REGISTRO') %>% 
    mutate(VOL = Vol_mkt - Vol_san, Tx = (Tx_mkt - Tx_san)/(Vol_mkt - Vol_san)) %>% 
    select(DAT_REGISTRO,VOL,Tx) -> cdb_mkt
  
  bo_santander %>% 
    filter(DESC_SUB_PROD %in% c("CDB DI", "CDB DI CORPORATE"), 
           INST == "Institucionais",
           DT_CONT >= '2017-05-02') %>%
    mutate(Tx = VOL * Per_index) %>% 
    group_by(DT_CONT) %>%
    summarise(DAT_REGISTRO = DT_CONT,VOL = sum(VOL), Tx = sum(Tx)) %>% 
    mutate(Taxa = Tx/VOl) %>% 
    select(DAT_REGISTRO, VOL, Taxa) %>% 
    collect() -> san_cdb_mktmkt
  
  cdb_mkt %>% 
    full_join(san_cdb_mktmkt, by = 'DAT_REGISTRO') %>% 
    mutate(share = (VOL.y / (VOL.x + VOL.y)), delta = Taxa - Tx) %>% 
    select(DAT_REGISTRO,VOL.y,Taxa,share,delta) %>% 
    collect() %>% 
    mutate_all(funs(replace(.,is.na(.),0))) -> mktmkt_plot
  
  return(mktmkt_plot)
}