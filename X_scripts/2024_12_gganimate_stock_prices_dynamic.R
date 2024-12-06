#####set basics #### 
username <- "WITTEJ09"
setwd(paste0("C:/Users/",username,"/OneDrive - Pfizer/Document/")) ####essential modify


#### Load necessary libraries####
library(tidyquant)
library(lubridate)
library(ggplot2)
library(gifski)  # Ensure gifski is installed
library(gganimate)
library(av)
library(ggrepel)
library(dplyr)

#### simple chart of one stock####
ticker_symbol <- "META"
company <- "Meta Platforms Inc"

####create data#
# Fetch stock data from Yahoo Finance
getSymbols(ticker_symbol, src = 'yahoo')

# Convert the fetched data into a data frame
df1 <- data.frame(Date = index(get(ticker_symbol)), coredata(get(ticker_symbol)))

# Create a line chart using ggplot2
p <- ggplot(df1, aes(x = Date, y = get(paste(ticker_symbol, ".Close", sep = "")))) +
  geom_line(color = "blue") +
  labs(title = paste("Stock Price of", company),
       x = "Date",
       y = "Stock Price (USD)") +
  theme_minimal() +
  transition_reveal(Date)  # Add transition for animation

# Animate the plot
animated_plot <- animate(p, nframes = 1500, fps = 30, renderer = gifski_renderer(), height = 1920, width =1080)

# Save the animation as a gif
anim_save("animated_line_chart_META.gif", animation = animated_plot)


#######################################################batch 1: December 2024####
#### relative appreciation KO vs PEP  ####
# Define ticker symbols and company names
ticker_symbols <- c("KO", "PEP")
companies <- c("Coca-Cola", "PepsiCo")

# Fetch stock data from Yahoo Finance
getSymbols(ticker_symbols, src = 'yahoo')

# Convert the fetched data into a data frame for each company
df_list <- lapply(ticker_symbols, function(ticker) {
  df <- data.frame(Date = index(get(ticker)), coredata(get(ticker)))
  colnames(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
  df$Company <- companies[which(ticker_symbols == ticker)]
  return(df)
})

# Combine the data frames
df_combined <- do.call(rbind, df_list)

# Calculate relative share price appreciation (both starting at 100%)
df_combined <- df_combined %>%
  group_by(Company) %>%
  mutate(Relative_Close = 100 * Close / first(Close))

# Create a line chart using ggplot2
p <- ggplot(df_combined, aes(x = Date, y = Relative_Close, color = Company)) +
  geom_line(size = 2.5) +  # Increase line thickness
  labs(title = "Relative Share Price Appreciation of Coca-Cola and PepsiCo",
       y = "Relative Share Price (Starting at 100%)") +
  theme_minimal() +
  theme(legend.position = c(0.05, 0.95),  # Position legend within the plot
        legend.justification = c("left", "top"),
        legend.text = element_text(size = 30),  # Increase size of legend text
        legend.title = element_blank(),  # Remove legend title
        axis.title.x = element_blank(),  # Remove x-axis title
        axis.text.x = element_text(size = 22),
        axis.text.y = element_text(size = 20)) +  # Increase size of y-axis labels
  scale_color_manual(values = c("Coca-Cola" = "red", "PepsiCo" = "blue")) +  # Set colors
  transition_reveal(Date)  # Add transition for animation

# Add labels directly to the lines within every frame using ggrepel
# Add labels directly to the lines within every frame using ggrepel
#p <- p + geom_text_repel(data = df_combined %>% filter(Company == "Coca-Cola"), aes(label = "KO"), color = "black", hjust = 0.7, vjust = 0.3, size = 8, check_overlap = TRUE) +
#  geom_text_repel(data = df_combined %>% filter(Company == "PepsiCo"), aes(label = "PEP"), color = "black", hjust = 0.7, vjust = 0.3, size = 8, check_overlap = TRUE)

# Animate the plot and save as video with resolution 1920x1080
animated_plot <- animate(p, nframes = 1500, fps = 30, renderer = av_renderer(), height = 1920, width = 1080)

# Save the animation as a video file
anim_save("animated_relative_share_price_ko_vs_pep.mp4", animation = animated_plot)

#### car manufacturers comparison ####
# Define ticker symbols and company names
ticker_symbols <- c("VOW3.DE", "STLA", "TM", "BMW.DE", "MBG.DE", "TSLA")
companies <- c("Volkswagen", "Stellantis", "Toyota", "BMW", "Mercedes", "Tesla")

# Fetch stock data from Yahoo Finance
getSymbols(ticker_symbols, src = 'yahoo')

# Convert the fetched data into a data frame for each company
df_list <- lapply(ticker_symbols, function(ticker) {
  df <- data.frame(Date = index(get(ticker)), coredata(get(ticker)))
  colnames(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
  df$Company <- companies[which(ticker_symbols == ticker)]
  return(df)
})

# Combine the data frames
df_combined <- do.call(rbind, df_list)

# Calculate relative share price appreciation (both starting at 100%)
df_combined <- df_combined %>%
  group_by(Company) %>%
  mutate(Relative_Close = 100 * Close / first(Close))

# Create a line chart using ggplot2
p <- ggplot(df_combined, aes(x = Date, y = Relative_Close, color = Company)) +
  geom_line(size = 2.5) +  # Increase line thickness
  labs(title = "Relative Share Price Appreciation of Major Automakers",
       y = "Relative Share Price (Starting at 100%)") +
  theme_minimal() +
  theme(legend.position = c(0.05, 0.95),  # Position legend within the plot
        legend.justification = c("left", "top"),
        legend.text = element_text(size = 36),  # Increase size of legend text
        legend.title = element_blank(),  # Remove legend title
        axis.title.x = element_blank(),  # Remove x-axis title
        axis.text.x = element_text(size = 22),
        axis.text.y = element_text(size = 20)) +  # Increase size of y-axis labels
  scale_color_manual(values = c("Volkswagen" = "blue", "Stellantis" = "green", "Toyota" = "red", 
                                "BMW" = "purple", "Mercedes" = "orange", "Tesla" = "black")) +  # Set colors
  transition_reveal(Date)  # Add transition for animation

# Add labels directly to the lines within every frame using ggrepel
#p <- p + geom_text_repel(data = df_combined %>% filter(Company == "Volkswagen"), aes(label = "VOW3.DE"), color = "black", hjust = 0.7, vjust = 0.3, size = 12, check_overlap = TRUE) +
  #geom_text_repel(data = df_combined %>% filter(Company == "Stellantis"), aes(label = "STLA"), color = "black", hjust = 0.7, vjust = 0.3, size = 12, check_overlap = TRUE) +
  #geom_text_repel(data = df_combined %>% filter(Company == "Toyota"), aes(label = "TM"), color = "black", hjust = 0.7, vjust = 0.3, size = 12, check_overlap = TRUE) +
  #geom_text_repel(data = df_combined %>% filter(Company == "BMW"), aes(label = "BMW.DE"), color = "black", hjust = 0.7, vjust = 0.3, size = 12, check_overlap = TRUE) +
  #geom_text_repel(data = df_combined %>% filter(Company == "Mercedes"), aes(label = "MBG.DE"), color = "black", hjust = 0.7, vjust = 0.3, size = 12, check_overlap = TRUE) +
  #geom_text_repel(data = df_combined %>% filter(Company == "Tesla"), aes(label = "TSLA"), color = "black", hjust = 0.7, vjust = 0.3, size = 12, check_overlap = TRUE)

# Animate the plot and save as video with resolution 1920x1080
animated_plot <- animate(p, nframes = 1000, fps = 30, renderer = av_renderer(), height = 1920, width = 1080)

# Save the animation as a video file
anim_save("animated_relative_share_price_auto.mp4", animation = animated_plot)



####Amazon vs Shopify ####
# Define ticker symbols and company names
ticker_symbols <- c("SHOP", "AMZN")
companies <- c("Shopify", "Amazon")

# Fetch stock data from Yahoo Finance
getSymbols(ticker_symbols, src = 'yahoo')

# Convert the fetched data into a data frame for each company
df_list <- lapply(ticker_symbols, function(ticker) {
  df <- data.frame(Date = index(get(ticker)), coredata(get(ticker)))
  colnames(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
  df$Company <- companies[which(ticker_symbols == ticker)]
  return(df)
})

# Combine the data frames
df_combined <- do.call(rbind, df_list)

# Calculate relative share price appreciation (both starting at 100%)
df_combined <- df_combined %>%
  group_by(Company) %>%
  mutate(Relative_Close = 100 * Close / first(Close))

# Create a line chart using ggplot2
p <- ggplot(df_combined, aes(x = Date, y = Relative_Close, color = Company)) +
  geom_line(size = 2.5) +  # Increase line thickness
  labs(title = "Relative Share Price Appreciation of Shopify and Amazon",
       y = "Relative Share Price (Starting at 100%)") +
  theme_minimal() +
  theme(legend.position = c(0.05, 0.95),  # Position legend within the plot
        legend.justification = c("left", "top"),
        legend.text = element_text(size = 32),  # Increase size of legend text
        legend.title = element_blank(),  # Remove legend title
        axis.title.x = element_blank(),  # Remove x-axis title
        axis.text.x = element_text(size = 22),
        axis.text.y = element_text(size = 20)) +  # Increase size of y-axis labels
  scale_color_manual(values = c("Shopify" = "green", "Amazon" = "orange")) +  # Set colors
  transition_reveal(Date)  # Add transition for animation

# Animate the plot and save as video with resolution 1920x1080
animated_plot <- animate(p, nframes = 1500, fps = 30, renderer = av_renderer(), height = 1920, width = 1080)

# Save the animation as a video file
anim_save("animated_relative_share_price_shop_vs_amzn.mp4", animation = animated_plot)

#### Microstrategy vs bitcoin ####
# Define ticker symbols and company names
ticker_symbols <- c("BTC-USD", "MSTR")
companies <- c("Bitcoin", "Microstrategy")

# Fetch stock data from Yahoo Finance
getSymbols(ticker_symbols, src = 'yahoo')

# Convert the fetched data into a data frame for each company
df_list <- lapply(ticker_symbols, function(ticker) {
  df <- data.frame(Date = index(get(ticker)), coredata(get(ticker)))
  colnames(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
  df$Company <- companies[which(ticker_symbols == ticker)]
  return(df)
})

# Combine the data frames
df_combined <- do.call(rbind, df_list)

# Calculate relative share price appreciation (both starting at 100%)
df_combined <- df_combined %>%
  group_by(Company) %>%
  mutate(Relative_Close = 100 * Close / first(Close))

# Create a line chart using ggplot2
p <- ggplot(df_combined, aes(x = Date, y = Relative_Close, color = Company)) +
  geom_line(size = 2.5) +  # Increase line thickness
  labs(title = "Relative Share Price Appreciation of Bitcoin and Microstrategy",
       y = "Relative Share Price (Starting at 100%)") +
  theme_minimal() +
  theme(legend.position = c(0.05, 0.95),  # Position legend within the plot
        legend.justification = c("left", "top"),
        legend.text = element_text(size = 30),  # Increase size of legend text
        legend.title = element_blank(),  # Remove legend title
        axis.title.x = element_blank(),  # Remove x-axis title
        axis.text.x = element_text(size = 22),
        axis.text.y = element_text(size = 20)) +  # Increase size of y-axis labels
  scale_color_manual(values = c("Bitcoin" = "gold", "Microstrategy" = "red")) +  # Set colors
  transition_reveal(Date)  # Add transition for animation

# Animate the plot and save as video with resolution 1920x1080
animated_plot <- animate(p, nframes = 1500, fps = 30, renderer = av_renderer(), height = 1920, width = 1080)

# Save the animation as a video file
anim_save("animated_relative_share_price_btc_vs_mstr.mp4", animation = animated_plot)

#### Adidas vs Nike ####

# Define ticker symbols and company names
ticker_symbols <- c("ADS.DE", "NKE")
companies <- c("Adidas", "Nike")

# Fetch stock data from Yahoo Finance
getSymbols(ticker_symbols, src = 'yahoo')

# Convert the fetched data into a data frame for each company
df_list <- lapply(ticker_symbols, function(ticker) {
  df <- data.frame(Date = index(get(ticker)), coredata(get(ticker)))
  colnames(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
  df$Company <- companies[which(ticker_symbols == ticker)]
  return(df)
})

# Combine the data frames
df_combined <- do.call(rbind, df_list)

# Calculate relative share price appreciation (both starting at 100%)
df_combined <- df_combined %>%
  group_by(Company) %>%
  mutate(Relative_Close = 100 * Close / first(Close))

# Create a line chart using ggplot2
p <- ggplot(df_combined, aes(x = Date, y = Relative_Close, color = Company)) +
  geom_line(size = 2.5) +  # Increase line thickness
  labs(title = "Relative Share Price Appreciation of Adidas and Nike",
       y = "Relative Share Price (Starting at 100%)") +
  theme_minimal() +
  theme(legend.position = c(0.05, 0.95),  # Position legend within the plot
        legend.justification = c("left", "top"),
        legend.text = element_text(size = 32),  # Increase size of legend text
        legend.title = element_blank(),  # Remove legend title
        axis.title.x = element_blank(),  # Remove x-axis title
        axis.text.x = element_text(size = 22),
        axis.text.y = element_text(size = 20)) +  # Increase size of y-axis labels
  scale_color_manual(values = c("Adidas" = "green", "Nike" = "blue")) +  # Set colors
  transition_reveal(Date)  # Add transition for animation

# Animate the plot and save as video with resolution 1920x1080
animated_plot <- animate(p, nframes = 1500, fps = 30, renderer = av_renderer(), height = 1920, width = 1080)

# Save the animation as a video file
anim_save("animated_relative_share_price_ads_vs_nke.mp4", animation = animated_plot)

#### JNJ vs PFE ####
# Define ticker symbols and company names
ticker_symbols <- c("JNJ", "PFE")
companies <- c("Johnson & Johnson", "Pfizer")

# Fetch stock data from Yahoo Finance
getSymbols(ticker_symbols, src = 'yahoo')

# Convert the fetched data into a data frame for each company
df_list <- lapply(ticker_symbols, function(ticker) {
  df <- data.frame(Date = index(get(ticker)), coredata(get(ticker)))
  colnames(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
  df$Company <- companies[which(ticker_symbols == ticker)]
  return(df)
})

# Combine the data frames
df_combined <- do.call(rbind, df_list)

# Calculate relative share price appreciation (both starting at 100%)
df_combined <- df_combined %>%
  group_by(Company) %>%
  mutate(Relative_Close = 100 * Close / first(Close))

# Create a line chart using ggplot2
p <- ggplot(df_combined, aes(x = Date, y = Relative_Close, color = Company)) +
  geom_line(size = 2.5) +  # Increase line thickness
  labs(title = "Relative Share Price Appreciation of Johnson & Johnson and Pfizer",
       y = "Relative Share Price (Starting at 100%)") +
  theme_minimal() +
  theme(legend.position = c(0.05, 0.95),  # Position legend within the plot
        legend.justification = c("left", "top"),
        legend.text = element_text(size = 30),  # Increase size of legend text
        legend.title = element_blank(),  # Remove legend title
        axis.title.x = element_blank(),  # Remove x-axis title
        axis.text.x = element_text(size = 22),
        axis.text.y = element_text(size = 20)) +  # Increase size of y-axis labels
  scale_color_manual(values = c("Johnson & Johnson" = "red", "Pfizer" = "blue")) +  # Set colors
  transition_reveal(Date)  # Add transition for animation

# Animate the plot and save as video with resolution 1920x1080
animated_plot <- animate(p, nframes = 1500, fps = 30, renderer = av_renderer(), height = 1920, width = 1080)

# Save the animation as a video file
anim_save("animated_relative_share_price_jnj_vs_pfe.mp4", animation = animated_plot)

#### Novo vs Lilly #### 
# Define ticker symbols and company names
ticker_symbols <- c("NVO", "LLY")
companies <- c("Novo Nordisk", "Eli Lilly")

# Fetch stock data from Yahoo Finance
getSymbols(ticker_symbols, src = 'yahoo')

# Convert the fetched data into a data frame for each company
df_list <- lapply(ticker_symbols, function(ticker) {
  df <- data.frame(Date = index(get(ticker)), coredata(get(ticker)))
  colnames(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
  df$Company <- companies[which(ticker_symbols == ticker)]
  return(df)
})

# Combine the data frames
df_combined <- do.call(rbind, df_list)

# Calculate relative share price appreciation (both starting at 100%)
df_combined <- df_combined %>%
  group_by(Company) %>%
  mutate(Relative_Close = 100 * Close / first(Close))

# Create a line chart using ggplot2
p <- ggplot(df_combined, aes(x = Date, y = Relative_Close, color = Company)) +
  geom_line(size = 2.5) +  # Increase line thickness
  labs(title = "Relative Share Price Appreciation of Novo Nordisk and Eli Lilly",
       y = "Relative Share Price (Starting at 100%)") +
  theme_minimal() +
  theme(legend.position = c(0.05, 0.95),  # Position legend within the plot
        legend.justification = c("left", "top"),
        legend.text = element_text(size = 30),  # Increase size of legend text
        legend.title = element_blank(),  # Remove legend title
        axis.title.x = element_blank(),  # Remove x-axis title
        axis.text.x = element_text(size = 22),
        axis.text.y = element_text(size = 20)) +  # Increase size of y-axis labels
  scale_color_manual(values = c("Novo Nordisk" = "green", "Eli Lilly" = "blue")) +  # Set colors
  transition_reveal(Date)  # Add transition for animation

# Animate the plot and save as video with resolution 1920x1080
animated_plot <- animate(p, nframes = 1500, fps = 30, renderer = av_renderer(), height = 1920, width = 1080)

# Save the animation as a video file
anim_save("animated_relative_share_price_nvo_vs_lly.mp4", animation = animated_plot)

#### Boeing vs Airbus ####
# Define ticker symbols and company names
ticker_symbols <- c("AIR.PA", "BA")
companies <- c("Airbus", "Boeing")

# Fetch stock data from Yahoo Finance
getSymbols(ticker_symbols, src = 'yahoo')

# Convert the fetched data into a data frame for each company
df_list <- lapply(ticker_symbols, function(ticker) {
  df <- data.frame(Date = index(get(ticker)), coredata(get(ticker)))
  colnames(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
  df$Company <- companies[which(ticker_symbols == ticker)]
  return(df)
})

# Combine the data frames
df_combined <- do.call(rbind, df_list)

# Calculate relative share price appreciation (both starting at 100%)
df_combined <- df_combined %>%
  group_by(Company) %>%
  mutate(Relative_Close = 100 * Close / first(Close))

# Create a line chart using ggplot2
p <- ggplot(df_combined, aes(x = Date, y = Relative_Close, color = Company)) +
  geom_line(size = 2.5) +  # Increase line thickness
  labs(title = "Relative Share Price Appreciation of Airbus and Boeing",
       y = "Relative Share Price (Starting at 100%)") +
  theme_minimal() +
  theme(legend.position = c(0.05, 0.95),  # Position legend within the plot
        legend.justification = c("left", "top"),
        legend.text = element_text(size = 30),  # Increase size of legend text
        legend.title = element_blank(),  # Remove legend title
        axis.title.x = element_blank(),  # Remove x-axis title
        axis.text.x = element_text(size = 22),
        axis.text.y = element_text(size = 20)) +  # Increase size of y-axis labels
  scale_color_manual(values = c("Airbus" = "green", "Boeing" = "blue")) +  # Set colors
  transition_reveal(Date)  # Add transition for animation

# Animate the plot and save as video with resolution 1920x1080
animated_plot <- animate(p, nframes = 1500, fps = 30, renderer = av_renderer(), height = 1920, width = 1080)

# Save the animation as a video file
anim_save("animated_relative_share_price_air_vs_ba.mp4", animation = animated_plot)

####
#### IBM vs Cisco ####
# Define ticker symbols and company names
ticker_symbols <- c("CSCO", "IBM")
companies <- c("Cisco", "IBM")

# Fetch stock data from Yahoo Finance
getSymbols(ticker_symbols, src = 'yahoo')

# Convert the fetched data into a data frame for each company
df_list <- lapply(ticker_symbols, function(ticker) {
  df <- data.frame(Date = index(get(ticker)), coredata(get(ticker)))
  colnames(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
  df$Company <- companies[which(ticker_symbols == ticker)]
  return(df)
})

# Combine the data frames
df_combined <- do.call(rbind, df_list)

# Calculate relative share price appreciation (both starting at 100%)
df_combined <- df_combined %>%
  group_by(Company) %>%
  mutate(Relative_Close = 100 * Close / first(Close))

# Create a line chart using ggplot2
p <- ggplot(df_combined, aes(x = Date, y = Relative_Close, color = Company)) +
  geom_line(size = 2.5) +  # Increase line thickness
  labs(title = "Relative Share Price Appreciation of Cisco and IBM",
       y = "Relative Share Price (Starting at 100%)") +
  theme_minimal() +
  theme(legend.position = c(0.05, 0.95),  # Position legend within the plot
        legend.justification = c("left", "top"),
        legend.text = element_text(size = 30),  # Increase size of legend text
        legend.title = element_blank(),  # Remove legend title
        axis.title.x = element_blank(),  # Remove x-axis title
        axis.text.x = element_text(size = 22),
        axis.text.y = element_text(size = 20)) +  # Increase size of y-axis labels
  scale_color_manual(values = c("Cisco" = "blue", "IBM" = "black")) +  # Set colors
  transition_reveal(Date)  # Add transition for animation

# Animate the plot and save as video with resolution 1920x1080
animated_plot <- animate(p, nframes = 1500, fps = 30, renderer = av_renderer(), height = 1920, width = 1080)

# Save the animation as a video file
anim_save("animated_relative_share_price_csco_vs_ibm.mp4", animation = animated_plot)
#### Caterpillar vs Deere ##### 
#Define ticker symbols and company names
ticker_symbols <- c("CAT", "DE")
companies <- c("Caterpillar", "Deere")

# Fetch stock data from Yahoo Finance
getSymbols(ticker_symbols, src = 'yahoo')

# Convert the fetched data into a data frame for each company
df_list <- lapply(ticker_symbols, function(ticker) {
  df <- data.frame(Date = index(get(ticker)), coredata(get(ticker)))
  colnames(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
  df$Company <- companies[which(ticker_symbols == ticker)]
  return(df)
})

# Combine the data frames
df_combined <- do.call(rbind, df_list)

# Calculate relative share price appreciation (both starting at 100%)
df_combined <- df_combined %>%
  group_by(Company) %>%
  mutate(Relative_Close = 100 * Close / first(Close))

# Create a line chart using ggplot2
p <- ggplot(df_combined, aes(x = Date, y = Relative_Close, color = Company)) +
  geom_line(size = 2.5) +  # Increase line thickness
  labs(title = "Relative Share Price Appreciation of Caterpillar and Deere",
       y = "Relative Share Price (Starting at 100%)") +
  theme_minimal() +
  theme(legend.position = c(0.05, 0.95),  # Position legend within the plot
        legend.justification = c("left", "top"),
        legend.text = element_text(size = 30),  # Increase size of legend text
        legend.title = element_blank(),  # Remove legend title
        axis.title.x = element_blank(),  # Remove x-axis title
        axis.text.x = element_text(size = 22),
        axis.text.y = element_text(size = 20)) +  # Increase size of y-axis labels
  scale_color_manual(values = c("Caterpillar" = "yellow", "Deere" = "green")) +  # Set colors
  transition_reveal(Date)  # Add transition for animation

# Animate the plot and save as video with resolution 1920x1080
animated_plot <- animate(p, nframes = 1500, fps = 30, renderer = av_renderer(), height = 1920, width = 1080)

# Save the animation as a video file
anim_save("animated_relative_share_price_cat_vs_de.mp4", animation = animated_plot)

##################################################next batch in January 2025 ####
#### NR 1: cosmetic PG, Kimberly Clark Colgate and Loreal #####
# Define ticker symbols and company names
ticker_symbols <- c("PG", "KMB", "CL", "OR.PA")
companies <- c("Procter & Gamble", "Kimberly-Clark Corp.", "Colgate-Palmolive Company", "L'Oreal")

# Fetch stock data from Yahoo Finance
getSymbols(ticker_symbols, src = 'yahoo')

# Convert the fetched data into a data frame for each company
df_list <- lapply(ticker_symbols, function(ticker) {
  df <- data.frame(Date = index(get(ticker)), coredata(get(ticker)))
  colnames(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
  df$Company <- companies[which(ticker_symbols == ticker)]
  return(df)
})

# Combine the data frames
df_combined <- do.call(rbind, df_list)

# Calculate relative share price appreciation (both starting at 100%)
df_combined <- df_combined %>%
  group_by(Company) %>%
  mutate(Relative_Close = 100 * Close / first(Close))

# Create a line chart using ggplot2
p <- ggplot(df_combined, aes(x = Date, y = Relative_Close, color = Company)) +
  geom_line(size = 2.5) +  # Increase line thickness
  labs(title = "Relative Share Price Appreciation of Procter & Gamble, Kimberly-Clark Corp., Colgate-Palmolive Company, and L'Oreal",
       y = "Relative Share Price (Starting at 100%)") +
  theme_minimal() +
  theme(legend.position = c(0.05, 0.95),  # Position legend within the plot
        legend.justification = c("left", "top"),
        legend.text = element_text(size = 30),  # Increase size of legend text
        legend.title = element_blank(),  # Remove legend title
        axis.title.x = element_blank(),  # Remove x-axis title
        axis.text.x = element_text(size = 22),
        axis.text.y = element_text(size = 20)) +  # Increase size of y-axis labels
  scale_color_manual(values = c("Procter & Gamble" = "green", "Kimberly-Clark Corp." = "blue", "Colgate-Palmolive Company" = "red", "L'Oreal" = "purple")) +  # Set colors
  transition_reveal(Date)  # Add transition for animation

# Animate the plot and save as video with resolution 1920x1080
animated_plot <- animate(p, nframes = 1500, fps = 30, renderer = av_renderer(), height = 1920, width = 1080)

# Save the animation as a video file
anim_save("animated_relative_share_price_pg_kmb_cl_or.mp4", animation = animated_plot)

#### NR 2: Alibaba vs Tencent vs Xiaomi ####
# Define ticker symbols and company names
ticker_symbols <- c("BABA", "0700.HK", "1810.HK")
companies <- c("Alibaba", "Tencent", "Xiaomi")

# Fetch stock data from Yahoo Finance
getSymbols(ticker_symbols, src = 'yahoo')

# Convert the fetched data into a data frame for each company
df_list <- lapply(ticker_symbols, function(ticker) {
  df <- data.frame(Date = index(get(ticker)), coredata(get(ticker)))
  colnames(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
  df$Company <- companies[which(ticker_symbols == ticker)]
  return(df)
})

# Combine the data frames
df_combined <- do.call(rbind, df_list)

# Calculate relative share price appreciation (both starting at 100%)
df_combined <- df_combined %>%
  group_by(Company) %>%
  mutate(Relative_Close = 100 * Close / first(Close))

# Create a line chart using ggplot2
p <- ggplot(df_combined, aes(x = Date, y = Relative_Close, color = Company)) +
  geom_line(size = 2.5) +  # Increase line thickness
  labs(title = "Relative Share Price Appreciation of Alibaba, Tencent, and Xiaomi",
       y = "Relative Share Price (Starting at 100%)") +
  theme_minimal() +
  theme(legend.position = c(0.05, 0.95),  # Position legend within the plot
        legend.justification = c("left", "top"),
        legend.text = element_text(size = 30),  # Increase size of legend text
        legend.title = element_blank(),  # Remove legend title
        axis.title.x = element_blank(),  # Remove x-axis title
        axis.text.x = element_text(size = 22),
        axis.text.y = element_text(size = 20)) +  # Increase size of y-axis labels
  scale_color_manual(values = c("Alibaba" = "green", "Tencent" = "blue", "Xiaomi" = "red")) +  # Set colors
  transition_reveal(Date)  # Add transition for animation

# Animate the plot and save as video with resolution 1920x1080
animated_plot <- animate(p, nframes = 1500, fps = 30, renderer = av_renderer(), height = 1920, width = 1080)

# Save the animation as a video file
anim_save("animated_relative_share_price_baba_0700_1810.mp4", animation = animated_plot)
#### NR 3: Walmart vs Target vs Ahold####
# Define ticker symbols and company names
ticker_symbols <- c("WMT", "TGT", "AD.AS")
companies <- c("Walmart", "Target", "Ahold")

# Fetch stock data from Yahoo Finance
getSymbols(ticker_symbols, src = 'yahoo')

# Convert the fetched data into a data frame for each company
df_list <- lapply(ticker_symbols, function(ticker) {
  df <- data.frame(Date = index(get(ticker)), coredata(get(ticker)))
  colnames(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
  df$Company <- companies[which(ticker_symbols == ticker)]
  return(df)
})

# Combine the data frames
df_combined <- do.call(rbind, df_list)

# Calculate relative share price appreciation (both starting at 100%)
df_combined <- df_combined %>%
  group_by(Company) %>%
  mutate(Relative_Close = 100 * Close / first(Close))

# Create a line chart using ggplot2
p <- ggplot(df_combined, aes(x = Date, y = Relative_Close, color = Company)) +
  geom_line(size = 2.5) +  # Increase line thickness
  labs(title = "Relative Share Price Appreciation of Walmart, Target, and Ahold",
       y = "Relative Share Price (Starting at 100%)") +
  theme_minimal() +
  theme(legend.position = c(0.05, 0.95),  # Position legend within the plot
        legend.justification = c("left", "top"),
        legend.text = element_text(size = 30),  # Increase size of legend text
        legend.title = element_blank(),  # Remove legend title
        axis.title.x = element_blank(),  # Remove x-axis title
        axis.text.x = element_text(size = 22),
        axis.text.y = element_text(size = 20)) +  # Increase size of y-axis labels
  scale_color_manual(values = c("Walmart" = "blue", "Target" = "red", "Ahold" = "black")) +  # Set colors
  transition_reveal(Date)  # Add transition for animation

# Animate the plot and save as video with resolution 1920x1080
animated_plot <- animate(p, nframes = 1500, fps = 30, renderer = av_renderer(), height = 1920, width = 1080)

# Save the animation as a video file
anim_save("animated_relative_share_price_wmt_tgt_ad.mp4", animation = animated_plot)

#### NR 4: Thermofisher vs Danaher ####
# Define ticker symbols and company names
ticker_symbols <- c("DHR", "TMO")
companies <- c("Danaher", "ThermoFisher")

# Fetch stock data from Yahoo Finance
getSymbols(ticker_symbols, src = 'yahoo')

# Convert the fetched data into a data frame for each company
df_list <- lapply(ticker_symbols, function(ticker) {
  df <- data.frame(Date = index(get(ticker)), coredata(get(ticker)))
  colnames(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
  df$Company <- companies[which(ticker_symbols == ticker)]
  return(df)
})

# Combine the data frames
df_combined <- do.call(rbind, df_list)

# Calculate relative share price appreciation (both starting at 100%)
df_combined <- df_combined %>%
  group_by(Company) %>%
  mutate(Relative_Close = 100 * Close / first(Close))

# Create a line chart using ggplot2
p <- ggplot(df_combined, aes(x = Date, y = Relative_Close, color = Company)) +
  geom_line(size = 2.5) +  # Increase line thickness
  labs(title = "Relative Share Price Appreciation of Danaher and ThermoFisher",
       y = "Relative Share Price (Starting at 100%)") +
  theme_minimal() +
  theme(legend.position = c(0.05, 0.95),  # Position legend within the plot
        legend.justification = c("left", "top"),
        legend.text = element_text(size = 30),  # Increase size of legend text
        legend.title = element_blank(),  # Remove legend title
        axis.title.x = element_blank(),  # Remove x-axis title
        axis.text.x = element_text(size = 22),
        axis.text.y = element_text(size = 20)) +  # Increase size of y-axis labels
  scale_color_manual(values = c("Danaher" = "green", "ThermoFisher" = "blue")) +  # Set colors
  transition_reveal(Date)  # Add transition for animation

# Animate the plot and save as video with resolution 1920x1080
animated_plot <- animate(p, nframes = 1500, fps = 30, renderer = av_renderer(), height = 1920, width = 1080)

# Save the animation as a video file
anim_save("animated_relative_share_price_dhr_vs_tmo.mp4", animation = animated_plot)


#### NR 5: LVHM vs Hermes vs Lululemon####
# Define ticker symbols and company names
ticker_symbols <- c("RMS.PA", "MC.PA", "LULU")
companies <- c("Hermes", "LVHM", "Lululemon")

# Fetch stock data from Yahoo Finance
getSymbols(ticker_symbols, src = 'yahoo')

# Convert the fetched data into a data frame for each company
df_list <- lapply(ticker_symbols, function(ticker) {
  df <- data.frame(Date = index(get(ticker)), coredata(get(ticker)))
  colnames(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
  df$Company <- companies[which(ticker_symbols == ticker)]
  return(df)
})

# Combine the data frames
df_combined <- do.call(rbind, df_list)

# Calculate relative share price appreciation (both starting at 100%)
df_combined <- df_combined %>%
  group_by(Company) %>%
  mutate(Relative_Close = 100 * Close / first(Close))

# Create a line chart using ggplot2
p <- ggplot(df_combined, aes(x = Date, y = Relative_Close, color = Company)) +
  geom_line(size = 2.5) +  # Increase line thickness
  labs(title = "Relative Share Price Appreciation of Hermes, LVHM, and Lululemon",
       y = "Relative Share Price (Starting at 100%)") +
  theme_minimal() +
  theme(legend.position = c(0.05, 0.95),  # Position legend within the plot
        legend.justification = c("left", "top"),
        legend.text = element_text(size = 30),  # Increase size of legend text
        legend.title = element_blank(),  # Remove legend title
        axis.title.x = element_blank(),  # Remove x-axis title
        axis.text.x = element_text(size = 22),
        axis.text.y = element_text(size = 20)) +  # Increase size of y-axis labels
  scale_color_manual(values = c("Hermes" = "green", "LVHM" = "blue", "Lululemon" = "red")) +  # Set colors
  transition_reveal(Date)  # Add transition for animation

# Animate the plot and save as video with resolution 1920x1080
animated_plot <- animate(p, nframes = 1500, fps = 30, renderer = av_renderer(), height = 1920, width = 1080)

# Save the animation as a video file
anim_save("animated_relative_share_price_rms_mc_lulu.mp4", animation = animated_plot)
#### NR 6: Realty Income vs Essex Property Trust####
# Define ticker symbols and company names
ticker_symbols <- c("O", "ESS")
companies <- c("Realty Income", "Essex Property Trust")

# Fetch stock data from Yahoo Finance
getSymbols(ticker_symbols, src = 'yahoo')

# Convert the fetched data into a data frame for each company
df_list <- lapply(ticker_symbols, function(ticker) {
  df <- data.frame(Date = index(get(ticker)), coredata(get(ticker)))
  colnames(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
  df$Company <- companies[which(ticker_symbols == ticker)]
  return(df)
})

# Combine the data frames
df_combined <- do.call(rbind, df_list)

# Calculate relative share price appreciation (both starting at 100%)
df_combined <- df_combined %>%
  group_by(Company) %>%
  mutate(Relative_Close = 100 * Close / first(Close))

# Create a line chart using ggplot2
p <- ggplot(df_combined, aes(x = Date, y = Relative_Close, color = Company)) +
  geom_line(size = 2.5) +  # Increase line thickness
  labs(title = "Relative Share Price Appreciation of Realty Income and Essex Property Trust",
       y = "Relative Share Price (Starting at 100%)") +
  theme_minimal() +
  theme(legend.position = c(0.05, 0.95),  # Position legend within the plot
        legend.justification = c("left", "top"),
        legend.text = element_text(size = 30),  # Increase size of legend text
        legend.title = element_blank(),  # Remove legend title
        axis.title.x = element_blank(),  # Remove x-axis title
        axis.text.x = element_text(size = 22),
        axis.text.y = element_text(size = 20)) +  # Increase size of y-axis labels
  scale_color_manual(values = c("Realty Income" = "green", "Essex Property Trust" = "blue")) +  # Set colors
  transition_reveal(Date)  # Add transition for animation

# Animate the plot and save as video with resolution 1920x1080
animated_plot <- animate(p, nframes = 1500, fps = 30, renderer = av_renderer(), height = 1920, width = 1080)

# Save the animation as a video file
anim_save("animated_relative_share_price_o_vs_ess.mp4", animation = animated_plot)
#### NR 7: Waters vs Agilent ####
# Define ticker symbols and company names
ticker_symbols <- c("A", "WAT")
companies <- c("Agilent", "Waters")

# Fetch stock data from Yahoo Finance
getSymbols(ticker_symbols, src = 'yahoo')

# Convert the fetched data into a data frame for each company
df_list <- lapply(ticker_symbols, function(ticker) {
  df <- data.frame(Date = index(get(ticker)), coredata(get(ticker)))
  colnames(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
  df$Company <- companies[which(ticker_symbols == ticker)]
  return(df)
})

# Combine the data frames
df_combined <- do.call(rbind, df_list)

# Calculate relative share price appreciation (both starting at 100%)
df_combined <- df_combined %>%
  group_by(Company) %>%
  mutate(Relative_Close = 100 * Close / first(Close))

# Create a line chart using ggplot2
p <- ggplot(df_combined, aes(x = Date, y = Relative_Close, color = Company)) +
  geom_line(size = 2.5) +  # Increase line thickness
  labs(title = "Relative Share Price Appreciation of Agilent and Waters",
       y = "Relative Share Price (Starting at 100%)") +
  theme_minimal() +
  theme(legend.position = c(0.05, 0.95),  # Position legend within the plot
        legend.justification = c("left", "top"),
        legend.text = element_text(size = 30),  # Increase size of legend text
        legend.title = element_blank(),  # Remove legend title
        axis.title.x = element_blank(),  # Remove x-axis title
        axis.text.x = element_text(size = 22),
        axis.text.y = element_text(size = 20)) +  # Increase size of y-axis labels
  scale_color_manual(values = c("Agilent" = "green", "Waters" = "blue")) +  # Set colors
  transition_reveal(Date)  # Add transition for animation

# Animate the plot and save as video with resolution 1920x1080
animated_plot <- animate(p, nframes = 1500, fps = 30, renderer = av_renderer(), height = 1920, width = 1080)

# Save the animation as a video file
anim_save("animated_relative_share_price_a_vs_wat.mp4", animation = animated_plot)
#### NR 8: Starbucks vs McDonalds####
# Define ticker symbols and company names
ticker_symbols <- c("SBUX", "MCD")
companies <- c("Starbucks", "McDonald's")

# Fetch stock data from Yahoo Finance
getSymbols(ticker_symbols, src = 'yahoo')

# Convert the fetched data into a data frame for each company
df_list <- lapply(ticker_symbols, function(ticker) {
  df <- data.frame(Date = index(get(ticker)), coredata(get(ticker)))
  colnames(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
  df$Company <- companies[which(ticker_symbols == ticker)]
  return(df)
})

# Combine the data frames
df_combined <- do.call(rbind, df_list)

# Calculate relative share price appreciation (both starting at 100%)
df_combined <- df_combined %>%
  group_by(Company) %>%
  mutate(Relative_Close = 100 * Close / first(Close))

# Create a line chart using ggplot2
p <- ggplot(df_combined, aes(x = Date, y = Relative_Close, color = Company)) +
  geom_line(size = 2.5) +  # Increase line thickness
  labs(title = "Relative Share Price Appreciation of Starbucks and McDonald's",
       y = "Relative Share Price (Starting at 100%)") +
  theme_minimal() +
  theme(legend.position = c(0.05, 0.95),  # Position legend within the plot
        legend.justification = c("left", "top"),
        legend.text = element_text(size = 30),  # Increase size of legend text
        legend.title = element_blank(),  # Remove legend title
        axis.title.x = element_blank(),  # Remove x-axis title
        axis.text.x = element_text(size = 22),
        axis.text.y = element_text(size = 20)) +  # Increase size of y-axis labels
  scale_color_manual(values = c("Starbucks" = "green", "McDonald's" = "blue")) +  # Set colors
  transition_reveal(Date)  # Add transition for animation

# Animate the plot and save as video with resolution 1920x1080
animated_plot <- animate(p, nframes = 1500, fps = 30, renderer = av_renderer(), height = 1920, width = 1080)

# Save the animation as a video file
anim_save("animated_relative_share_price_sbux_vs_mcd.mp4", animation = animated_plot)
#### NR 9: Netflix vs WaltDisney####
# Define ticker symbols and company names
ticker_symbols <- c("NFLX", "DIS")
companies <- c("Netflix", "Walt Disney")

# Fetch stock data from Yahoo Finance
getSymbols(ticker_symbols, src = 'yahoo')

# Convert the fetched data into a data frame for each company
df_list <- lapply(ticker_symbols, function(ticker) {
  df <- data.frame(Date = index(get(ticker)), coredata(get(ticker)))
  colnames(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
  df$Company <- companies[which(ticker_symbols == ticker)]
  return(df)
})

# Combine the data frames
df_combined <- do.call(rbind, df_list)

# Calculate relative share price appreciation (both starting at 100%)
df_combined <- df_combined %>%
  group_by(Company) %>%
  mutate(Relative_Close = 100 * Close / first(Close))

# Create a line chart using ggplot2
p <- ggplot(df_combined, aes(x = Date, y = Relative_Close, color = Company)) +
  geom_line(size = 2.5) +  # Increase line thickness
  labs(title = "Relative Share Price Appreciation of Netflix and Walt Disney",
       y = "Relative Share Price (Starting at 100%)") +
  theme_minimal() +
  theme(legend.position = c(0.05, 0.95),  # Position legend within the plot
        legend.justification = c("left", "top"),
        legend.text = element_text(size = 30),  # Increase size of legend text
        legend.title = element_blank(),  # Remove legend title
        axis.title.x = element_blank(),  # Remove x-axis title
        axis.text.x = element_text(size = 22),
        axis.text.y = element_text(size = 20)) +  # Increase size of y-axis labels
  scale_color_manual(values = c("Netflix" = "green", "Walt Disney" = "blue")) +  # Set colors
  transition_reveal(Date)  # Add transition for animation

# Animate the plot and save as video with resolution 1920x1080
animated_plot <- animate(p, nframes = 1500, fps = 30, renderer = av_renderer(), height = 1920, width = 1080)

# Save the animation as a video file
anim_save("animated_relative_share_price_nflx_vs_dis.mp4", animation = animated_plot)
#### NR 10: Autodesk vs Cadence####
# Define ticker symbols and company names
ticker_symbols <- c("ADSK", "CDNS")
companies <- c("AutoDesk", "Cadence")

# Fetch stock data from Yahoo Finance
getSymbols(ticker_symbols, src = 'yahoo')

# Convert the fetched data into a data frame for each company
df_list <- lapply(ticker_symbols, function(ticker) {
  df <- data.frame(Date = index(get(ticker)), coredata(get(ticker)))
  colnames(df) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")
  df$Company <- companies[which(ticker_symbols == ticker)]
  return(df)
})

# Combine the data frames
df_combined <- do.call(rbind, df_list)

# Calculate relative share price appreciation (both starting at 100%)
df_combined <- df_combined %>%
  group_by(Company) %>%
  mutate(Relative_Close = 100 * Close / first(Close))

# Create a line chart using ggplot2
p <- ggplot(df_combined, aes(x = Date, y = Relative_Close, color = Company)) +
  geom_line(size = 2.5) +  # Increase line thickness
  labs(title = "Relative Share Price Appreciation of AutoDesk and Cadence",
       y = "Relative Share Price (Starting at 100%)") +
  theme_minimal() +
  theme(legend.position = c(0.05, 0.95),  # Position legend within the plot
        legend.justification = c("left", "top"),
        legend.text = element_text(size = 30),  # Increase size of legend text
        legend.title = element_blank(),  # Remove legend title
        axis.title.x = element_blank(),  # Remove x-axis title
        axis.text.x = element_text(size = 22),
        axis.text.y = element_text(size = 20)) +  # Increase size of y-axis labels
  scale_color_manual(values = c("AutoDesk" = "green", "Cadence" = "blue")) +  # Set colors
  transition_reveal(Date)  # Add transition for animation

# Animate the plot and save as video with resolution 1920x1080
animated_plot <- animate(p, nframes = 1500, fps = 30, renderer = av_renderer(), height = 1920, width = 1080)

# Save the animation as a video file
anim_save("animated_relative_share_price_adsk_vs_cdns.mp4", animation = animated_plot)

####################################################batch NR 3: Feb 2025 ####


