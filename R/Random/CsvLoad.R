require(lubridate)
require(stringr)
require(tidyverse)

rm(list = ls())
gc()

# Import full csv OC Mercado
b3_oc_mercado <- read_csv2("./CSV/oc_mercado.txt")
b3_oc_mercado$DAT_REGISTRO <- str_replace_all(b3_oc_mercado$DAT_REGISTRO,"/","")
b3_oc_mercado$DAT_REGISTRO <- dmy(b3_oc_mercado$DAT_REGISTRO)
b3_oc_mercado$DAT_REGISTRO <- as.character(b3_oc_mercado$DAT_REGISTRO)

# Import full csv OC Santander
b3_oc_santander <- read_csv2("./CSV/oc_santander.txt")
b3_oc_santander$DAT_REGISTRO <- str_replace_all(b3_oc_santander$DAT_REGISTRO, "/","")
b3_oc_santander$DAT_REGISTRO <- dmy(b3_oc_santander$DAT_REGISTRO)
b3_oc_santander$DAT_REGISTRO <- as.character(b3_oc_santander$DAT_REGISTRO)

# Import full csv CDB Mercado
b3_CDB_mercado <- read_csv2("./CSV/cdb_mercado.txt")
b3_CDB_mercado$DAT_REGISTRO <- str_replace_all(b3_CDB_mercado$DAT_REGISTRO,"/","")
b3_CDB_mercado$DAT_REGISTRO <- dmy(b3_CDB_mercado$DAT_REGISTRO)
b3_CDB_mercado$DAT_REGISTRO <- as.character(b3_CDB_mercado$DAT_REGISTRO)

# Import full csv CDB Santander
b3_CDB_santander <- read_csv2("./CSV/cdb_santander.txt")
b3_CDB_santander$DAT_REGISTRO <- str_replace_all(b3_CDB_santander$DAT_REGISTRO, "/","")
b3_CDB_santander$DAT_REGISTRO <- dmy(b3_CDB_santander$DAT_REGISTRO)
b3_CDB_santander$DAT_REGISTRO <- as.character(b3_CDB_santander$DAT_REGISTRO)

# connect into cetip_db
db <- src_sqlite("./DB/cetip_db.sqlite3", create = TRUE)
copy_to(db,b3_oc_mercado,temporary = FALSE)
copy_to(db,b3_oc_santander,temporary = FALSE)
copy_to(db,b3_CDB_mercado, temporary = FALSE)
copy_to(db,b3_CDB_santander, temporary = FALSE)

# remove memory df
rm(b3_oc_mercado)
rm(b3_oc_santander)
rm(b3_CDB_santander)
rm(b3_CDB_mercado)
gc()
