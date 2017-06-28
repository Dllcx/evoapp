require(tidyverse)
require(stringr)
require(readxl)

rm(list = ls())

# Link Data base
setwd("~/R/evoapp/DB/")
db <- src_sqlite("cetip_db.sqlite3", create = FALSE)

# DB TABLES
tbl_oc_mercado <- tbl(db,"b3_oc_mercado")
tbl_oc_santander <- tbl(db,"b3_oc_santander")
tbl_CDB_mercado <- tbl(db,"b3_CDB_mercado")
tbl_CDB_santander <- tbl(db,"b3_CDB_santander")

# headers names
tbl_oc_mercado %>% colnames() -> oc_mkt_names
tbl_oc_santander %>% colnames() -> oc_san_names
tbl_CDB_mercado %>% colnames() -> cdb_mkt_names
tbl_CDB_santander %>% colnames() -> cdb_san_names

# Read OC Mercado/Santander XLSX
df_mercado_oc <- read_xlsx("~/R/evoapp/XLSX/Cetip Market Report Diario (Compromissada) - Consolidado.XLSX")
df_mercado_oc$DAT_REGISTRO <- as.character(df_mercado_oc$DAT_REGISTRO)
colnames(df_mercado_oc) <- oc_mkt_names

df_santander_oc <- read_xlsx("~/R/evoapp/XLSX/Cetip Market Report Diario (Compromissada) - Santander.XLSX")
df_santander_oc$DAT_REGISTRO <- as.character(df_santander_oc$DAT_REGISTRO)
colnames(df_santander_oc) <- oc_san_names

# Read CDB Mercado/Santander XLSX
df_mercado_cdb <- read_xlsx("~/R/evoapp/XLSX/Cetip Market Report Diario (CDB) - Consolidado.xlsx")
df_mercado_cdb$DAT_REGISTRO <- as.character(df_mercado_cdb$DAT_REGISTRO)
df_mercado_cdb$DESV_PAD <- NULL
colnames(df_mercado_cdb) <- cdb_mkt_names

df_santander_cdb <- read_xlsx("~/R/evoapp/XLSX/Cetip Market Report Diario (CDB) - Santander.xlsx")
df_santander_cdb$DAT_REGISTRO <- as.character(df_santander_cdb$DAT_REGISTRO)
df_santander_cdb$DESV_PAD <- NULL
colnames(df_santander_cdb) <- cdb_san_names

# insert on data base
db_insert_into(con = db$con, table = "b3_oc_mercado", values = df_mercado_oc)
db_insert_into(con = db$con, table = "b3_oc_santander", values = df_santander_oc)
db_insert_into(con = db$con, table = "b3_CDB_mercado", values = df_mercado_cdb)
db_insert_into(con = db$con, table = "b3_CDB_santander", values = df_santander_cdb)

# remove memory df
rm(list = ls())
gc()
