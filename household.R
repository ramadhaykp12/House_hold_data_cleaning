library(haven)
library(dplyr)

# Specify the path to your DTA file
ht00 <- read_dta("htrack00.dta")
ht07 <- read_dta("htrack07.dta")
ht14 <- read_dta("htrack14.dta")
ht97 <- read_dta("htrack97.dta")

# Filter data ht14 untuk kolom yang akan dipakai saja
ht14_new <- ht14 %>% select(hhid93,sc01_93,sc02_93,sc03_93,sc05_93,
                            hhid97,sc01_97,sc02_97,sc03_97,sc05_97, 
                            hhid00,sc010000,sc020000,sc030000,
                            hhid07,sc01_07_07,sc02_07_07,sc03_07_07,
                            hhid14,sc01_14_14,sc02_14_14,sc03_14_14)

# Menghapus nilai kosong
filtered_data <- ht14_new %>% filter(hhid93 != "")
filtered_data <- filtered_data %>% filter(hhid97 != "")
filtered_data <- filtered_data %>% filter(hhid00 != "")
filtered_data <- filtered_data %>% filter(hhid00 != "")
filtered_data <- filtered_data %>% filter(hhid07 != "")
filtered_data <- filtered_data %>% filter(hhid14 != "")

# Memisahkan kolom menjadi data tahun 2014 yang baru
t14 <- filtered_data %>% select(hhid14,sc01_14_14,sc02_14_14,sc03_14_14)
# Mengganti nama kolom
colnames(t14) <- c("ID", "Provinsi", "Kabupaten", "Kecamatan")

# Memisahkan kolom menjadi data tahun 2007 yang baru
t7 <- filtered_data %>% select(hhid07,sc01_07_07,sc02_07_07,sc03_07_07)
colnames(t7) <- c("ID", "Provinsi", "Kabupaten", "Kecamatan")

# Memisahkan kolom menjadi data tahun 2000 yang baru
t00 <- filtered_data %>% select(hhid00,sc010000,sc020000,sc030000)
colnames(t00) <- c("ID", "Provinsi", "Kabupaten", "Kecamatan")

# Memisahkan kolom menjadi data tahun 1997 yang baru
t97 <- filtered_data %>% select(hhid97,sc01_97,sc02_97,sc03_97,sc05_97)
colnames(t97) <- c("ID", "Provinsi", "Kabupaten", "Kecamatan", "Urban/Rural")

# Memisahkan kolom menjadi data tahun 1993 yang baru
t93 <- filtered_data %>% select(hhid93,sc01_93,sc02_93,sc03_93,sc05_93)
colnames(t93) <- c("ID", "Provinsi", "Kabupaten", "Kecamatan", "Urban/Rural")

# Menggabungkan data yang sudah diganti nama kolomnya dan melabelkan data berdasarkan tahun dari data tersebut
combined_data <- bind_rows(
  mutate(t93, Tahun = 1993),
  mutate(t97, Tahun = 1997),
  mutate(t00, Tahun = 2000),
  mutate(t7, Tahun = 2007),
  mutate(t14, Tahun = 2014)
)

# Menghapus duplicated row
unique_df_dplyr <- distinct(combined_data)

# Menghapus baris data yang memiliki ID yang tidak memiliki 5 pasangan ID yang sama 
column_of_interest <- "ID"
value_counts <- table(unique_df_dplyr[[column_of_interest]])
values_to_keep <- names(value_counts[value_counts >= 5])
filtered_df <- unique_df_dplyr[unique_df_dplyr[[column_of_interest]] %in% values_to_keep, ]
colnames(filtered_df) <- c("ID", "Provinsi", "Kabupaten", "Kecamatan", "UrbanOrRural","Tahun")

# Mengisi missing values pada kolom Urban ural dengan nilai dari ID yang sama
df <- filtered_df %>%
  group_by(ID) %>%
  mutate(UrbanOrRural = ifelse(all(!is.na(UrbanOrRural)), first(UrbanOrRural), UrbanOrRural))

# Mengurutkan kolom
df <- df[, c("ID", "Tahun", "Provinsi", "Kabupaten", "Kecamatan", "UrbanOrRural")]

#Sortir data berdasarkan ID
sorted_df <- df[order(df$ID), ]
print(sorted_df)

sum(sorted_df$Tahun == 1993)
sum(sorted_df$Tahun == 1997)
sum(sorted_df$Tahun == 2000)
sum(sorted_df$Tahun == 2007)
sum(sorted_df$Tahun == 2014)

df2 <- read_dta("kec_9899000714_v12.dta")

df3 <- read_dta("b2_ut1_00.dta")
df4 <- read_dta("b2_ut1_07.dta")
df5 <- read_dta("b2_ut1_97.dta")
df6 <- read_dta("b2_ut1_14.dta")
df7 <- read_dta("buk2ut1_93.dta")

