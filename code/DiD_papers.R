# Graph DiD papers by year 
library(data.table)


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

dt <- fread("DiD_allmarketing.txt", sep = "\t")
dt <- dt[PY >= 2015]
head(dt)

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
gg <- ggplot(summary_table, aes(x = PY, y = Number_of_Papers, fill = Journal)) +
  geom_bar(stat = "identity", position = "stack") +
  # labs(title = "Number of Papers by Year and Publication Source") +
  xlab("Year")+
  ylab("Number of Papers")+
  my_theme() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Display the plot
print(gg)
