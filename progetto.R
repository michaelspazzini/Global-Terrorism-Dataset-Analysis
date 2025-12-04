#Progetto HIS - Analisi GTD - Spazzini Michael - Matricola 732040

# -- Importazione librerie --
library(tidyverse)
library(readxl)
library(ggplot2)
library(gridExtra)
library(plotrix)
library(scales)
library(reshape2)
library(RColorBrewer)



## Funzione per salvare grafici
save_plot <- function(plot, name, folder = "output_grafici", 
                      width = 8, height = 6, dpi = 300) {
  # Crea cartella se non esiste
  if(!dir.exists(folder)) dir.create(folder)
  
  # Costruisci il percorso file
  filename <- paste0(folder, "/HIS_GTD_SpazziniMichael_", name, ".png")
  
  # Salvataggio grafico (solo ggplot)
  ggsave(filename, plot = plot, width = width, height = height, dpi = dpi)
  
  message("Grafico salvato in: ", filename)
}

# -- Importazione Dataset --
# Da eseguire solo 1 volta, dataset molto grande
#gtd <- read_excel("globalterrorismdb_0522dist.xlsx")

## Filtro dati per nazione
iraq_gtd <- gtd %>% 
  filter(country_txt == "Iraq")


## Pulizia Dataset
gtd_clean = select(gtd, eventid, year = iyear, month = imonth, day=iday,
                   country_code = country, country_name=country_txt,
                   region_code=region, region_name=region_txt, provstate,city,
                   latitude,longitude,location,success, 
                   attacktype1=attacktype1_txt, attacktype2=attacktype2_txt,
                   attacktype3=attacktype3_txt,targtype1=targtype1_txt,
                   targsubtype1=targsubtype1_txt,weaptype1=weaptype1_txt, 
                   weapsubtype1=weapsubtype1_txt, property,propextent_txt,
                   propvalue,gname,nkill,nwound)

iraq_gtd = select(iraq_gtd, eventid, year = iyear, month = imonth, day=iday,
                   country_code = country, country_name=country_txt,
                   region_code=region, region_name=region_txt, provstate,city,
                   latitude,longitude,location,success, 
                   attacktype1=attacktype1_txt, attacktype2=attacktype2_txt,
                   attacktype3=attacktype3_txt,targtype1=targtype1_txt,
                   targsubtype1=targsubtype1_txt,weaptype1=weaptype1_txt, 
                   weapsubtype1=weapsubtype1_txt, property,propextent_txt,
                   propvalue,gname,nkill,nwound)


## Relazione attacchi tra gtd_globale e gtd_iraq
plot1 <- gtd_clean %>% 
  count(year) %>%
  ggplot(aes(x=year, y=n)) +
  geom_line(color="darkgreen", size=1) +
  labs(x="year", y="# Event") +
  theme_minimal()

plot2 <- iraq_gtd %>% 
  count(year) %>%
  ggplot(aes(x=year, y=n)) +
  geom_line(color="darkred", size=1) +
  labs(x="year", y="# Event") +
  theme_minimal()
grid.arrange(plot1,plot2,ncol=2)
#save_plot(plot1,"Attacchi_globali")
#save_plot(plot2,"Attacchi_iraq")


## Top 10 paesi
top_countries <- gtd_clean %>%
  group_by(country_name) %>%
  summarise(Total = n()) %>%
  arrange(desc(Total)) %>%
  slice_max(Total, n = 10) %>%
  pull(country_name)

## Top 10 gruppi
top_groups <- gtd_clean %>%
  group_by(gname) %>%
  summarise(Total = n()) %>%
  arrange(desc(Total)) %>%
  slice_max(Total, n = 10) %>%
  pull(gname)

## Preparazione dati
df_plot <- gtd_clean %>%
  filter(country_name %in% top_countries) %>%
  mutate(gname_plot = ifelse(gname %in% top_groups, gname, "Others")) %>%
  group_by(country_name, gname_plot) %>%
  summarise(Events = n(), .groups = "drop") %>%
  group_by(country_name) %>%
  mutate(TotalEvents = sum(Events)) %>%
  ungroup()

## Grafico impilato orizzontale
ggplot(df_plot, aes(x = reorder(country_name, TotalEvents), y = Events, fill = gname_plot)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Country", y = "Number of Events", fill = "Terrorist Group") +
  theme_minimal() +
  theme(legend.position = "right") +
  scale_fill_brewer(palette = "Set3")


## Analisi città iraq più colpite
iraq_gtd %>% 
  group_by(city) %>% 
  summarise(Events = n(), .groups = "drop") %>% 
  filter(Events>200) %>% 
  ggplot(aes(x = reorder(city, -Events), y = Events, fill = Events > 1000)) +  # ordinamento decrescente
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, size = 8, vjust = 1, hjust = 1)) +
  labs(x = "City", y = "Number of Events") +
  scale_fill_manual(values = c('skyblue','red'), guide = FALSE)

## Analisi per obiettivo dell'attacco
iraq_gtd %>% 
  filter(targtype1 != "Unknown") %>% 
  count(targtype1, sort = TRUE) %>% 
  ggplot(aes(x = reorder(targtype1, n), y = n)) + 
  geom_col(fill = "orange", color = "black") +
  geom_text(aes(label = n), hjust = -0.2, size = 3) +
  coord_flip() +
  labs(
    x = "Target Type",
    y = "Number of Events"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(size = 9),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10)
  )



## successo 1/0 in base al tipo di attacco
iraq_gtd %>%
  group_by(attacktype1, success) %>%
  summarise(Events = n(), .groups = "drop") %>%
  ggplot(aes(x = attacktype1, y = Events, fill = factor(success))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Attack Type", y = "Number of Events", fill = "Success") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
  
## Valutazione del tipo di attacco
iraq_gtd %>% 
  group_by(attacktype1) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
    ggplot(aes(x=reorder(attacktype1,n), y=n)) + 
    geom_bar(stat="identity", fill = 'orange', color='black') +
    theme(axis.text.x = element_text(size = 8, vjust = 1, hjust = 1)) +
    labs(x="Attacks type", y="Events") +
    coord_flip()

## Analisi tipologia arma per anno
ggplot(
  data = subset(iraq_gtd, weaptype1 != "Unknown" & !grepl("^Vehicle", weaptype1)),
  aes(x = year, fill = weaptype1)
) +
  geom_bar() +
  labs(
    x = "Year", 
    y = "Number of Attacks", 
    fill = "Weapon Type"
  ) +
  theme_minimal()

## File R di Spazzini Michael, Matricola: 732040











