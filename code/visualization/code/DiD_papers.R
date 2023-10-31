# Graph DiD papers by year 
library(data.table)

out_dir <- '../output/'
dir.create(out_dir, showWarnings = F)

# Define preferred ggplot theme
my_theme <- function() {
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "#f0f0f0", linetype = 1), # Adds major grid lines
    panel.grid.minor = element_blank(), # Adds minor grid lines
    axis.line = element_line(color = "gray"),
    text = element_text(size = 20)
  )
}

dt <- fread("../external/DiD_allmarketing.txt", sep = "\t")
dt <- dt[PY >= 2018]
head(dt)

cols <- c('TI', 'PY', 'SO', 'AU')
dt <- dt[, ..cols]
head(dt)

# Add missing papers as you discover
add1 <- list('Changing Their Tune', '2018', 'Mktg Sci', 'Datta, H; Knox, G; Bronnenberg, BJ')
dt <- rbind(dt, add1)


dt$PY
dt$SO
unique(dt$SO)
# Abbreviate the journal names
dt[SO == "MARKETING SCIENCE", SO := "Mktg Sci"]
dt[SO == "JOURNAL OF MARKETING RESEARCH", SO := "JMR"]
dt[SO == "JOURNAL OF MARKETING", SO := "JM"]
dt[SO == "INTERNATIONAL JOURNAL OF RESEARCH IN MARKETING", SO := "IJRM"]


# Create a summary table
summary_table <- dt[, .(Number_of_Papers = .N), by = PY]

# Create the ggplot
ggplot(summary_table, aes(x = PY, y = Number_of_Papers)) +
  geom_bar(stat = "identity") +
  # labs(title = "Number of Papers by Year") +
  xlab("Publication Year")+
  ylab("Number of Papers")+
  my_theme()


# Create a summary table by year and publication source
summary_table <- dt[, .(Number_of_Papers = .N), by = .(PY, SO)]
setnames(summary_table, 'SO', 'Journal')

# Create the ggplot
gg <- ggplot(summary_table[PY<2023], aes(x = PY, y = Number_of_Papers, fill = Journal)) +
  geom_bar(stat = "identity", position = "stack") +
  # labs(title = "Number of Papers by Year and Publication Source") +
  xlab("Year")+
  ylab("Number of Papers")+
  my_theme() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Display the plot
print(gg)
ggsave(paste0(out_dir, 'pubs_overyears_by_so.png'))


# GREYSCALE
# Create the ggplot
gg <- ggplot(summary_table[PY<2023], aes(x = PY, y = Number_of_Papers, fill = Journal)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("grey20", "grey40", "grey60", "grey80")) +  # Add this line to specify grayscale colors
  # labs(title = "Number of Papers by Year and Publication Source") +
  xlab("Year") +
  ylab("Number of Papers") +
  my_theme() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plot
print(gg)
ggsave(paste0(out_dir, 'pubs_overyears_by_so_gray.png'))

# Fractional
# The total number of papers = 6,776 (query link: https://www.webofscience.com/wos/woscc/summary/2b2df4e8-a4ca-480e-ba39-318582e4b7fe-a1b4e5a6/relevance/1)
tot = 6776
summary_table[,fraction := Number_of_Papers/6776]

gg2 <- ggplot(summary_table, aes(x = PY, y = fraction, fill = Journal)) +
  geom_bar(stat = "identity", position = "stack") +
  # labs(title = "Number of Papers by Year and Publication Source") +
  xlab("Year")+
  ylab("Share")+
  my_theme() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Display the plot
print(gg2)
ggsave(paste0(out_dir, 'pubs_overyears_by_so_fraction.png'))



