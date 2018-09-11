# ===================================================================================
# -- Comparing Job Zones with probability of automation bt Frey and Osborne 2013 -- #
# ===================================================================================
library(tidyverse)
library(pdftools)
# "#3366FF"
pdf <- pdf_text("https://www.oxfordmartin.ox.ac.uk/downloads/academic/future-of-employment.pdf")
pdf <- paste0(pdf, collapse = " ")
pattern <- "Rank\\s+Probability(.)*Telemarketers"
extract <- regmatches(pdf, regexpr(pattern, pdf))
extract <- unlist(strsplit(extract, "\r\n"))
FO <- read.fwf(con <- textConnection(extract), widths = c(7, 13, 6, 9, 80))
close(con)

FO <- FO %>% filter(grepl(".\\d", V2)) %>%
  mutate(rank = as.numeric(gsub(" ", "", V1)),
         prob = as.numeric(gsub(" ", "", V2)),
         soc  = as.character(gsub(" ", "",V4)),
         ocup = as.character(V5)) %>% select(-(V1:V5))

Job_Zones <- readxl::read_excel("Data/All_Job_Zones.xlsx", 
                                skip = 3, col_types = "text") %>% 
  mutate(Code = substr(Code,1,7)) %>% 
  rename(Job_Zone = `Job Zone`) %>% distinct(Job_Zone, Code) 

che <- Job_Zones %>% group_by(Code) %>% summarise(freq = n())

Job_Zones <- Job_Zones %>% filter(Code %in% filter(che, freq==1)$Code )

Comp <- full_join(FO, Job_Zones, by = c("soc" = "Code"))

Comp %>% filter(is.na(Comp$prob)) %>% distinct(soc) %>% nrow()
Comp %>% filter(is.na(Comp$Job_Zone)) %>% distinct(soc) %>% nrow()

Comp <- Comp %>% na.omit()

ggplot(aes(y = prob, x = Job_Zone), data = Comp) + geom_boxplot(fill = "gray") +
  theme_bw() + labs(x = "Job Zone", y = "Probability of Computerisation") +
  theme(text = element_text(size=15))
ggsave('boxplot.pdf', units="in", width=5, height=5)
tapply(Comp$prob, Comp$Job_Zone, summary)


# FO and Job Zones --------------------------------------------------------
Job_Zones <- readxl::read_excel("Data/All_Job_Zones.xls", 
                                skip = 3, col_types = "text") %>% 
  mutate(Code = substr(Code,1,7)) %>% 
  rename(Job_Zone = `Job Zone`) %>% distinct(Job_Zone, Code) 

all <- full_join(FO, Job_Zones, by = c("soc" = "Code"))

write.csv2(all, row.names = FALSE, file = )
write.csv2(cbo2002_soc, row.names = F, file = "Conversions/CBO_OK_r.csv")
