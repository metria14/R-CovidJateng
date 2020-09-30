# covid Indonesia
resp <- GET("https://data.covid19.go.id/public/api/update.json")
cov_id_raw <- content(resp, as ="parsed", simplifyVector = TRUE)
cov_id_update <- cov_id_raw$update
str(cov_id_update)
str(cov_id_raw)

# Mengakses Data Master Perkembangan Covid di Jawa Tengah Bulan September
library(httr)
library (xml2)
resp_jateng <- GET("https://data.covid19.go.id/public/api/prov_detail_JAWA_TENGAH.json")
status_code (resp_jateng)
cov_jateng_raw <- content(resp_jateng, as = "parsed", simplifyVector = TRUE)
names(cov_jateng_raw)
str(cov_jateng_raw)
cov_jateng_raw$kasus_total
cov_jateng_raw$meninggal_persen
cov_jateng_raw$sembuh_persen


# Memperoleh informasi lebih lengkap 
cov_jateng <- cov_jateng_raw$list_perkembangan
str(cov_jateng)
cov_jateng_raw$last_date 
cov_jateng$AKUMULASI_DIRAWAT_OR_ISOLASI
cov_jateng$AKUMULASI_KASUS
cov_jateng$AKUMULASI_SEMBUH
cov_jateng$AKUMULASI_MENINGGAL

# menjinakkan data
library(dplyr)
new_cov_jateng <- 
  cov_jateng %>% 
  select(-contains("DIRAWAT_OR_ISOLASI")) %>% 
  select(-starts_with("AKUMULASI")) %>% 
  rename(
    kasus_baru = KASUS,
    meninggal = MENINGGAL,
    sembuh = SEMBUH
  ) %>% 
  mutate(
    tanggal = as.POSIXct(tanggal / 1000, origin = "1970-01-01"),
    tanggal = as.Date(tanggal)
  )

# Gambar
str(new_cov_jateng)  
new_cov_jateng$kasus_baru
new_cov_jateng$meninggal
new_cov_jateng$sembuh
new_cov_jateng$tanggal
library(ggplot2)
library(hrbrthemes)
ggplot(new_cov_jateng, aes(x = tanggal, y = kasus_baru)) +
  geom_col()

# Gambar 2
library(hrbrthemes)
ggplot(new_cov_jateng, aes(tanggal, kasus_baru)) +
  geom_col(fill="salmon")+ 
  labs(
    x=NULL,
    y="Jumlah kasus",
    title = "Kasus Harian Positif COVID-19 di Jawa Tengah", caption = "Sumber data : covid.19.go.id"
  ) +
  theme_ipsum(
    base_size = 13,
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE 
  ) + 
  theme(plot.title.position = "plot")

#Grafik untuk Kasus Sembuh
library(ggplot2)
library(hrbrthemes)
ggplot(new_cov_jateng, aes(tanggal, sembuh)) +
  geom_col(fill = "olivedrab2") +
  labs(
    x = NULL,
    y = "Jumlah kasus",
    title = "Kasus Harian Sembuh Dari COVID-19 di Jawa Tengah",
    caption = "Sumber data: covid.19.go.id"
  ) +
  theme_ipsum(
    base_size = 13, 
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(plot.title.position = "plot")
library(ggplot2)
library(hrbrthemes)
ggplot(new_cov_jateng, aes(tanggal, meninggal)) +
  geom_col(fill ="darkslategray4") +
  labs(
    x = NULL,
    y = "Jumlah kasus",
    title = "Kasus Harian Meninggal Akibat COVID-19 di Jawa Tengah",
    caption = "Sumber data: covid.19.go.id"
  ) +
  theme_ipsum(
    base_size = 13, 
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(plot.title.position = "plot")

# Grafik untuk Kasus Meninggal
ggplot(new_cov_jateng, aes(tanggal, meninggal)) +
  geom_col(fill ="darkslategray4") +
  labs(
    x = NULL,
    y = "Jumlah kasus",
    title = "Kasus Harian Meninggal Akibat COVID-19 di Jawa Tengah",
    caption = "Sumber data: covid.19.go.id"
  ) +
  theme_ipsum(
    base_size = 13, 
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(plot.title.position = "plot")

#	Apakah Pekan ini Lebih Baik
library(dplyr)
library(lubridate)

cov_jateng_pekanan <- new_cov_jateng %>%
  count(
    tahun = year(tanggal),
    pekan_ke = week(tanggal),
    wt = kasus_baru,
    name = "jumlah"
  )

glimpse(cov_jateng_pekanan)

#	Menjawab Pertanyaan
cov_jateng_pekanan <-
  cov_jateng_pekanan %>%
  mutate(
    jumlah_pekanlalu = dplyr::lag(jumlah, 1),
    jumlah_pekanlalu = ifelse(is.na(jumlah_pekanlalu), 0, jumlah_pekanlalu),
    lebih_baik = jumlah < jumlah_pekanlalu
  )
glimpse(cov_jateng_pekanan)

#	Membuat Bar Chart
library(ggplot2)
library(hrbrthemes)
ggplot(cov_jateng_pekanan, aes(pekan_ke, jumlah, fill = lebih_baik)) +
  geom_col(show.legend = FALSE) +
  scale_x_continuous(breaks = 9:29, expand = c(0, 0)) +
  scale_fill_manual(values = c("TRUE" = "blue", "FALSE" = "maroon")) +
  labs(
    x = NULL,
    y = "Jumlah kasus",
    title = "Kasus Pekanan Positif COVID-19 di Jawa Tengah",
    subtitle = "Kolom biru menunjukan penambahan kasus baru lebih sedikit dibandingkan satu pekan sebelumnya",
    caption = "Sumber data: covid.19.go.id"
  ) +
  theme_ipsum(
    base_size = 13,
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(plot.title.position = "plot")

# Pola dan Dinamika
library(dplyr)
cov_jateng_akumulasi <- 
  new_cov_jateng %>% 
  transmute(
    tanggal,
    akumulasi_aktif = cumsum(kasus_baru) - cumsum(sembuh) - cumsum(meninggal),
    akumulasi_sembuh = cumsum(sembuh),
    akumulasi_meninggal = cumsum(meninggal)
  )

tail(cov_jateng_akumulasi)
# Membuat Line Chart
ggplot(data =  cov_jateng_akumulasi, aes(x = tanggal, y = akumulasi_aktif)) +
  geom_line()
# Transformasi Data
library(dplyr)
library(tidyr)

dim(cov_jateng_akumulasi)

cov_jateng_akumulasi_pivot <- 
  cov_jateng_akumulasi %>% 
  gather(
    key = "kategori",
    value = "jumlah",
    -tanggal
  ) %>% 
  mutate(
    kategori = sub(pattern = "akumulasi_", replacement = "", kategori)
  )

dim(cov_jateng_akumulasi_pivot)

glimpse(cov_jateng_akumulasi_pivot)



# Dinamika Kasus Covid19 di Jawa Tengah
library(ggplot2)
library(hrbrthemes)
ggplot(cov_jateng_akumulasi_pivot, aes(tanggal, jumlah, colour = (kategori))) +
  geom_line(size = 0.9) +
  scale_y_continuous(sec.axis = dup_axis(name = NULL)) +
  scale_colour_manual(
    values = c(
      "aktif" = "salmon",
      "meninggal" = "darkslategray4",
      "sembuh" = "olivedrab2"
    ),
    labels = c("Aktif", "Meninggal", "Sembuh")
  ) +
  labs(
    x = NULL,
    y = "Jumlah kasus akumulasi",
    colour = NULL,
    title = "Dinamika Kasus COVID-19 di Jateng",
    caption = "Sumber data: covid.19.go.id"
  ) +
  theme_ipsum(
    base_size = 13,
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "top"
  )

