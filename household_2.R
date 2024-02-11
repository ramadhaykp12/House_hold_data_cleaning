library(haven)
library(dplyr)

but193 <- read_dta('buk2ut1_93.dta')
but197 <- read_dta('b2_ut1_97.dta')
but100 <- read_dta('b2_ut1_00.dta')
but107 <- read_dta('b2_ut1_07.dta')
but114 <- read_dta('b2_ut1_14.dta')

bnt193 <- read_dta('buk2nt1_93.dta')
bnt197 <- read_dta('b2_nt1_97.dta')
bnt100 <- read_dta('b2_nt2_00.dta')
bnt107 <- read_dta('b2_nt2_07.dta')
bnt114 <- read_dta('b2_nt2_14.dta')

df2 <- read_dta("kec_9899000714_v12.dta")

df_1993 <- sorted_df %>% filter(Tahun == 1993)
df_1997 <- sorted_df %>% filter(Tahun == 1997)
df_2000 <- sorted_df %>% filter(Tahun == 2000)
df_2007 <- sorted_df %>% filter(Tahun == 2007)
df_2014 <- sorted_df %>% filter(Tahun == 2014)

daerah_1993 <- df2 %>% select(provid98, nmprov1998, 
                              nmkab1998, nmkec1998)
colnames(daerah_1993) <- c("Provinsi", "Nama Provinsi",
                           "Nama Kabupaten","Nama Kecamatan")

daerah_1997 <- df2 %>% select(provid99, nmprov1999, 
                              nmkab1999, nmkec1999)
colnames(daerah_1997) <- c("Provinsi", "Nama Provinsi",
                           "Nama Kabupaten", "Nama Kecamatan")

daerah_2000 <- df2 %>% select(provid00, nmprov2000, 
                              nmkab2000, nmkec2000)
colnames(daerah_2000) <- c("Provinsi", "Nama Provinsi",
                           "Nama Kabupaten","Nama Kecamatan")

daerah_2007 <- df2 %>% select(provid07,nmprov2007, 
                              nmkab2007, nmkec2007)
colnames(daerah_2007) <- c("Provinsi", "Nama Provinsi",
                           "Nama Kabupaten","Nama Kecamatan")

daerah_2014 <- df2 %>% select(provid14,nmprov2014, 
                              nmkab2014, nmkec2014)
colnames(daerah_2014) <- c("Provinsi", "Nama Provinsi",
                           "Nama Kabupaten","Nama Kecamatan")

combined_daerah <- bind_rows(
  mutate(daerah_1993, Tahun = 1993),
  mutate(daerah_1997, Tahun = 1997),
  mutate(daerah_2000, Tahun = 2000),
  mutate(daerah_2007, Tahun = 2007),
  mutate(daerah_2014, Tahun = 2014)
)
colnames(combined_daerah) <- c("Provinsi", "Nama_Provinsi",
                           "Nama Kabupaten","Nama Kecamatan", "Tahun")

combined_daerah <- combined_daerah %>% select(Provinsi, Nama_Provinsi, Tahun)
combined_daerah <- unique(combined_daerah)

# Badan Usaha Tani
but193_new <- but193 %>% select(hhid93, ut07b1, ut08b1)
colnames(but193_new) <- c("ID", "revfarm", "expfarm")

but197_new <- but197 %>% select(hhid97, ut07, ut08)
colnames(but197_new) <- c("ID", "revfarm", "expfarm")

but100_new <- but100 %>% select(hhid00, ut07, ut08)
colnames(but100_new) <- c("ID", "revfarm", "expfarm")

but107_new <- but107 %>% select(hhid07, ut07, ut08)
colnames(but107_new) <- c("ID", "revfarm", "expfarm")

but114_new <- but114 %>% select(hhid14, ut07, ut08)
colnames(but114_new) <- c("ID", "revfarm", "expfarm")

# Badan Usaha Non Tani
bnt193_new <- bnt193 %>% select(hhid93, nt07b1, nt08b1)
colnames(bnt193_new) <- c("ID", "revnonfarm", "expnonfarm")

bnt197_new <- bnt197 %>% select(hhid97, nt07, nt08)
colnames(bnt197_new) <- c("ID", "revnonfarm", "expnonfarm")

bnt100_new <- bnt100 %>% select(hhid00, nt07, nt08)
colnames(bnt100_new) <- c("ID", "revnonfarm", "expnonfarm")

bnt107_new <- bnt107 %>% select(hhid07, nt07, nt08)
colnames(bnt107_new) <- c("ID", "revnonfarm", "expnonfarm")

bnt114_new <- bnt114 %>% select(hhid14, nt07, nt08)
colnames(bnt114_new) <- c("ID", "revnonfarm", "expnonfarm")

merged_utnt_93 <- full_join(but193_new, bnt193_new, by = "ID")

merged_utnt_97 <- full_join(but197_new, bnt197_new, by = "ID")

merged_utnt_00<- full_join(but100_new, bnt100_new, by = "ID")

merged_utnt_07 <- full_join(but107_new, bnt107_new, by = "ID")

merged_utnt_14 <- full_join(but114_new, bnt114_new, by = "ID")

merged_df93 <- left_join(df_1993, merged_utnt_93, by="ID")
merged_df97 <- left_join(df_1997, merged_utnt_97, by="ID")
merged_df00 <- left_join(df_2000, merged_utnt_00, by="ID")
merged_df07 <- left_join(df_2007, merged_utnt_07, by="ID")
merged_df14 <- left_join(df_2014, merged_utnt_14, by="ID")

combined_data_df <- bind_rows(
  mutate(merged_df93, Tahun = 1993),
  mutate(merged_df97, Tahun = 1997),
  mutate(merged_df00, Tahun = 2000),
  mutate(merged_df07, Tahun = 2007),
  mutate(merged_df14, Tahun = 2014)
)

sum(combined_data_df$Tahun == 1993)
sum(combined_data_df$Tahun == 1997)
sum(combined_data_df$Tahun == 2000)
sum(combined_data_df$Tahun == 2007)
sum(combined_data_df$Tahun == 2014)

df_unique <- distinct(combined_data_df)

fixed_data_df <- left_join(combined_data_df, combined_daerah, 
                           join_by(Provinsi, Tahun), 
                           relationship = "many-to-many")

sum(fixed_data_df$Tahun == 1993)
sum(fixed_data_df$Tahun == 1997)
sum(fixed_data_df$Tahun == 2000)
sum(fixed_data_df$Tahun == 2007)
sum(fixed_data_df$Tahun == 2014)

fixed_data_df <- distinct(fixed_data_df)

fixed_df <- inner_join(sorted_df, fixed_data_df, 
                      join_by(ID, Tahun))

# Menghapus baris data yang memiliki ID yang tidak memiliki 5 pasangan ID yang sama 
column_of_interest <- "ID"
value_counts <- table(fixed_data_df [[column_of_interest]])
values_to_keep <- names(value_counts[value_counts == 5])
filtered_fix_df <- fixed_data_df [fixed_data_df [[column_of_interest]] %in% values_to_keep, ]

sum(filtered_fix_df$Tahun == 1993)
sum(filtered_fix_df$Tahun == 1997)
sum(filtered_fix_df$Tahun == 2000)
sum(filtered_fix_df$Tahun == 2007)
sum(filtered_fix_df$Tahun == 2014)

filtered_fix_df <- filtered_fix_df[order(filtered_fix_df$ID), ]

filtered_fix_df <- filtered_fix_df[, c("ID", "Tahun", "Nama_Provinsi", "Provinsi",
             "Kabupaten", "Kecamatan", "UrbanOrRural",
             "revfarm", "expfarm", "revnonfarm",
             "expnonfarm")]

filtered_fix_df <- subset(filtered_fix_df, select = -c(Provinsi))
