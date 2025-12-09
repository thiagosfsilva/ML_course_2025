# Generate Turbidity data

set.seed(1)

sites <- c("A01", "A02", "B01", "B02")
n <- 120

dates <- sample(seq(as.Date("2024-01-10"), as.Date("2024-03-20"), by = "day"), n, replace = TRUE)
dates_messy <- ifelse(runif(n) > 0.5,
                      format(dates, "%d/%m/%Y"),
                      format(dates, "%Y-%m-%d"))

df_wq <- data.frame(
    sample_id = sample(1000:1100, n, replace = TRUE),  # duplicates
    site = sample(sites, n, replace = TRUE),
    date = dates_messy,
    temp_c = round(rnorm(n, mean = 22, sd = 4), 2),
    ph = round(rnorm(n, mean = 7.2, sd = 0.4), 2),
    turbidity_ntu = round(abs(rnorm(n, 10, 5)), 1)
)

# Introduce messiness
df_wq$temp_c[sample(1:n, 5)] <- c("-999", "NA", ".", "27,4", "15,2")
df_wq$ph[sample(1:n, 4)] <- c("-999", "8,1", ".", "NA")
df_wq$turbidity_ntu[sample(1:n, 4)] <- c("NA", "-999", ".", "20,3")

names(df_wq) <- c("id_amostra", "local", "data", "temp_c","ph","turbidez_ntu")

write.csv(df_wq, "./data/turbidez.csv", row.names = FALSE)
