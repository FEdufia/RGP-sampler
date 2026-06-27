library(readxl)
library(tidyverse)
library(ggthemes)
library(ggpubr)
# FIGURE 1-----------------------------------------------------

#Figures 1a and 1b were made using the Inkscape software



#### Figure 1d ####

sml_vols = read_excel("data/sml_vols.xlsx")

f1d = sml_vols|>
  mutate(minvol = Volume/`Collection Time`)|>
  select(`Collection Time`, Volume, Daytime, Windspeed, minvol, Plates, Daytime, Dips)|>
  mutate(Plate_dips = ifelse(Plates == "3 Plates", Dips * 3, Dips * 2))|>
  ggplot(aes(Plate_dips, Volume, label = Windspeed))+
  geom_point(aes(fill = `Collection Time`),size = 4.5, shape = 21)+
  scale_fill_distiller(palette = "YlOrBr", direction = 1)+
  geom_smooth(method = "lm", color = "black")+
  stat_cor(size = 5, aes( label = paste(..rr.label.., ..p.label.., sep = "~`,`~")))+
  theme_base()+
  coord_cartesian(ylim = c(0, 3000))+
  scale_y_continuous(sec.axis  = dup_axis())+  
  scale_x_continuous(sec.axis = dup_axis())+
  theme( plot.tag = element_text(size =  14, face = "bold"), plot.tag.location = "panel",
         plot.tag.position = c(0.06,0.95),
         plot.title = element_text(hjust = 0.5, face = "plain", size = 15),
         axis.ticks.length = unit(-0.3, "cm"), 
         axis.text.x.top  = element_blank(), 
         axis.title.x.top  = element_blank(), 
         axis.text.y.right  = element_blank(), 
         axis.title.y.right = element_blank(), 
         plot.background = element_blank())+
  labs(x = "Dips per Plate", y = "Volume of Sample (mL)", shape = "Time of Day", fill = "Sampling Time\n(min)")



#FIGURE 2---------------------------------------------
#Figure 2 was made using Igor Pro with the continuous underway CTD data of the R/V Hugh R. Sharp






#Figure 3---------------------------------------

spall = read_excel("data/spall.xlsx")

#### plots of mean and sd for DOC, CDOM and FDOM across stations and sampling periods


vars = c("DOC_mg", "DOC_uM", "SUVA254", "a254", "BIX", "HIX") # List of variables to calculate EF

#calculating means, sds and efs for all variables to plot
efs <- spall |>
  filter(TYPE %in% c("ML", "SS")) |>
  pivot_longer(cols = all_of(vars), names_to = "Variable", values_to = "Value") |>
  group_by(Station, Season, Year, TYPE, Variable) |>
  summarise(
    mean = mean(Value, na.rm = TRUE),
    sd   = sd(Value, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_wider(
    names_from = TYPE,
    values_from = c(mean, sd),
    names_sep = "_"
  ) |>
  mutate(EF = mean_ML / mean_SS) |>
  select(Station, Season, Year, Variable, EF, mean_ML, sd_ML, mean_SS, sd_SS)|>
  mutate(`Sampling Period` = paste(Season, Year))|>
  filter(!is.na(Station))|>
  filter(!Variable %in% c("DOC_mg", "a254"))|>
  mutate(Variable = factor(Variable, levels = c("DOC_uM", "SUVA254", "HIX", "BIX")))


#color palette
cpalette = c("Summer 2022" = "blue",
             "Fall 2022" = "green",
             "Summer 2024" = "black")


# Left panel: Means with error bars
means_long = efs |>
  pivot_longer(cols = c(mean_ML, mean_SS, sd_ML, sd_SS),
               names_to = c("stat", "TYPE"),
               names_sep = "_") |>
  pivot_wider(names_from = stat, values_from = value) |>
  mutate(`Sampling Period` = paste(Season, Year))

p1 = means_long |>
  ggplot(aes(x = Station, y = mean, color = `Sampling Period`, group = `Sampling Period`,
             shape = TYPE)) +
  geom_point(position = position_dodge(width = 0.6), size = 3) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), position = position_dodge(width = 0.6), width = 0.1) +
  scale_shape_manual(values = c(21, 17)) +  
  scale_color_manual(values = cpalette)+
  facet_wrap(~Variable, scales = "free_y", ncol = 1)+
  labs(y = "Mean ± SD", x = "Station") +
  theme_bw(base_size = 12)+ theme(plot.background = element_blank(),plot.title = element_text(hjust = 0.5, face = "plain", size = 15),strip.background = element_blank(),
                                  axis.ticks.length = unit(-0.25, "cm"), axis.text.x.top  = element_blank(), axis.title.x.top  = element_blank(), axis.text.y.right  = element_blank(), axis.title.y.right  = element_blank())+
  rremove("grid")


# Right panel: EF
p2 <- efs |>
  ggplot(aes(x = Station, y = EF, color = `Sampling Period`)) +
  geom_point(position = position_dodge(width = 0.6),size = 3, shape = 15) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  facet_wrap(~Variable, scales = "fixed", ncol = 1)+
  scale_color_manual(values = cpalette)+
  labs(y = "Enrichment Factor (EF)", x = NULL) +
  theme_bw(base_size = 12)+ theme(plot.background = element_blank(),plot.title = element_text(hjust = 0.5, face = "plain", size = 15),strip.background = element_blank(),
                                  axis.ticks.length = unit(-0.25, "cm"), axis.text.x.top  = element_blank(), axis.title.x.top  = element_blank(), axis.text.y.right  = element_blank(), axis.title.y.right  = element_blank())+
  rremove("grid")


# Combine with patchwork
library(patchwork)
(p1 | p2) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")
#Final edits were made using Inkscape


#Figure 4------------------------------------------------------------------------------------

#### Figures 4a and 4b: SURFACTANTS AND ST MIN ####
surf = data.frame(Type = c("ML", "SS"),
                  Surfactant = c(0.073069, 0.057094),
                  STmin = c(37.63, 53.02))

f4a = surf|>
  ggbarplot(x = "Type", y = "Surfactant", fill = "Type", color = "white",
            palette = c("#afc6e9ff", "#ccccccff"))+
  theme_bw(base_size = 12)+ 
  theme(plot.background = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "plain", size = 15),
        strip.background = element_blank(),
        axis.ticks.length = unit(-0.25, "cm"), 
        axis.text.x.top  = element_blank(), 
        axis.title.x.top  = element_blank(), 
        axis.text.y.right  = element_blank(), 
        axis.title.y.right  = element_blank())+
  scale_y_continuous(sec.axis  = dup_axis())+
  rremove("grid")+ labs(x = "Sample Type", y = "Surfactant (µM)", fill = "Sample Type")
  
  
f4b = surf|>
  ggbarplot(x = "Type", y = c("STmin"), fill = "Type",color = "white", palette = c("#afc6e9ff", "#ccccccff"))+
  theme_bw(base_size = 12)+
  theme(plot.background = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "plain", size = 15),
        strip.background = element_blank(),
        axis.ticks.length = unit(-0.25, "cm"), 
        axis.text.x.top  = element_blank(), 
        axis.title.x.top  = element_blank(), 
        axis.text.y.right  = element_blank(), 
        axis.title.y.right  = element_blank())+
  scale_y_continuous(sec.axis  = dup_axis())+
  rremove("grid")+ labs(x = "Sample Type", y = "Surface Tension Minimum", fill = "Sample Type")


#### Figures 4c and 4d: FTICR-MS ####

LFdata = read_xlsx("data/LCFTdata.xlsx")

lpalette = c("A-Sugar" = "#6c6f81ff",
             "Carbohydrate" = "#48453eff",
             "Isotoplogue" = "lightyellow2",
             "Lipid" = "grey",
             "Organosulfur" = "lightskyblue4",
             "Peptide" = "#e6d59aff",
             "Phytochemical" = "darkseagreen",
             "Unclassified" = "#000000ff"
)

f4c = LFdata|>
  filter(Treatment == "Night")|>
  group_by( Sample_Type, `Stoichiometric classification`)|>
  summarise(Unique_peaks = n_distinct(LCMF))|>
  mutate(Sample_Type = ifelse(Sample_Type == "ML", "SML", "SSW"))|>
  ggbarplot(, x = "Sample_Type", y = "Unique_peaks", 
            fill = "Stoichiometric classification", 
            palette = lpalette, color = "white",
  )+
  theme_bw(base_size = 12)+ theme(plot.background = element_blank(),
                                  plot.title = element_text(hjust = 0.5, 
                                                            face = "plain", size = 15),
                                  strip.background = element_blank(),
                                  axis.ticks.length = unit(-0.25, "cm"), 
                                  axis.text.x.top  = element_blank(),
                                  axis.title.x.top  = element_blank(), 
                                  axis.text.y.right  = element_blank(), 
                                  axis.title.y.right  = element_blank())+
  scale_y_continuous(sec.axis  = dup_axis())+
  rremove("grid")+
  labs(y = "Peak Counts", x = "Sample Type")


SLFdata = LFdata|>  
  filter(Treatment == "Sunset")|>
  filter(`Molecular Class` %in% c("CHO", "CHON", "CHOS", "CHONS")) 

NLFdata = LFdata|>  
  filter(Treatment == "Night")|>
  filter(`Molecular Class` %in% c("CHO", "CHON", "CHOS", "CHONS")) 


sunset_unique_formulas_classified = LFdata|>
  filter(Treatment == "Sunset")|>
  filter(`Molecular Class` %in% c("CHO", "CHON", "CHOS", "CHONS")) |>
  mutate(formula_class = case_when(
    Sample_Type == "ML" & !LCMF %in% SLFdata$LCMF[SLFdata$Sample_Type == "SS"] ~ "Unique to ML",
    Sample_Type == "SS" & !LCMF %in% SLFdata$LCMF[SLFdata$Sample_Type == "ML"] ~ "Unique to SS",
    TRUE ~ "Shared"
  ))

Snunique_formulas <- sunset_unique_formulas_classified |>
  filter(formula_class %in% c("Unique to ML", "Unique to SS"))|>
  mutate(Period = "Sunset")

Night_unique_formulas_classified = LFdata|>
  filter(Treatment == "Night")|>
  filter(`Molecular Class` %in% c("CHO", "CHON", "CHOS", "CHONS")) |>
  mutate(formula_class = case_when(
    Sample_Type == "ML" & !LCMF %in% NLFdata$LCMF[NLFdata$Sample_Type == "SS"] ~ "Unique to ML",
    Sample_Type == "SS" & !LCMF %in% NLFdata$LCMF[NLFdata$Sample_Type == "ML"] ~ "Unique to SS",
    TRUE ~ "Shared"
  ))


Nnunique_formulas <- Night_unique_formulas_classified |>
  filter(formula_class %in% c("Unique to ML", "Unique to SS"))|>
  mutate(Period = "Night")

Pmerged_uniques = rbind(Snunique_formulas, Nnunique_formulas)|>
  mutate(Period = factor(Period, levels = c("Sunset", "Night")))

p = Pmerged_uniques|> 
  group_by(Period, LCMF, formula_class)|>
  summarise(`H/C` = mean(`H/C`), `O/C` = mean(`O/C`)) |>
  ungroup() |>
  filter(Period == "Night")|>
  mutate(formula_class = ifelse(formula_class == "Unique to ML", "Unique to SML", "Unique to SSW"))|>
  ggplot(aes(x = `O/C`, y = `H/C`, group = formula_class)) +
  geom_point(aes(color = formula_class), size = 1) +scale_color_manual(values = c("Unique to SML" = "#5f8dd3",  "Unique to SSW" ="grey"))+
  labs(x = "O/C Ratio", y = "H/C Ratio",  color = " ")+
  theme_bw(base_size = 15)+ theme(plot.background = element_blank(),
                                  plot.title = element_text(hjust = 0.5, face = "plain", size = 15),
                                  strip.background = element_blank(), 
                                  legend.position = "bottom",
                                  axis.ticks.length = unit(-0.25, "cm"), 
                                  axis.text.x.top  = element_blank(), 
                                  axis.title.x.top  = element_blank(), 
                                  axis.text.y.right  = element_blank(), 
                                  axis.title.y.right  = element_blank())+
  scale_y_continuous(sec.axis  = dup_axis())+ 
  rremove("grid")



f4d = ggExtra::ggMarginal(p, type = "histogram" , groupFill = T)




#### Figures 4e and 4f: LC-MS LIPIDOMICS ####

library(readxl)
Lipid_Night_HG <- read_excel("data/Lipid_Night_HG.xlsx", 
                             sheet = "Sheet2")

HG = Lipid_Night_HG|>
  filter(SML != 0 | SSW != 0 )|>
  pivot_longer(names_to = "Sample", values_to = "RelativeIntensity", -Headgroup)|>
  mutate(RelativeIntensity = as.numeric(RelativeIntensity),
         Headgroup = factor(Headgroup, levels = c("SQMG","MGDG",  "CL", "PG",  "PC","PI", "PC O", "PA O","PE O", "PI O","LPI",  "LPI O"  , "LPG O","LPA O")))

lipid_order = c("SQMG","MGDG",  "CL", "PG",  "PC","PI", "PC O", "PA O","PE O", "PI O","LPI",  "LPI O"  , "LPG O","LPA O")

lipid_colors <- colorRampPalette(c("#79abe2ff","#f9d67eff" ,"#ffffc8ff","grey90","#a79fe1ff"))(length(lipid_order))



f4e = ggbarplot(HG, 
          x = "Sample", 
          y = "RelativeIntensity", 
          fill = "Headgroup", 
          color = "white",
          palette = lipid_colors,
          label = FALSE, #alpha = 0.5,
          position = position_stack(),
          ylab = "Percentage Contribution",
          xlab = "Sample Type")+theme_bw(base_size = 12)+ 
  theme(plot.background = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "plain", size = 15),
        strip.background = element_blank(),
        axis.ticks.length = unit(-0.25, "cm"), 
        axis.text.x.top  = element_blank(), 
        axis.title.x.top  = element_blank(), 
        axis.text.y.right  = element_blank(), 
        axis.title.y.right  = element_blank())+
  scale_y_continuous(sec.axis  = dup_axis())+
  rremove("grid")+labs(fill = "Lipid")






#### carbon number #### 
# Original summarized data
df <- data.frame(
  SML = c(0,0,0,0,0,0,0,3,2,1,2,2,2,3,2,2,4,5,2,3,2,4,3,4,4,3,3,3,2,3,4,6,3,1,3,0,2,0,0,1,0,0,3,1,1,0,0,0,0,0,0,0,1,0),
  SSW = c(0,0,1,0,0,0,0,2,1,0,0,1,2,4,4,1,2,3,1,0,0,1,1,2,2,2,1,1,1,3,1,2,0,0,4,0,3,2,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0),
  CN = 1:54
)

# Expand the data
df_long <- do.call(rbind, lapply(1:nrow(df), function(i) {
  data.frame(
    CN = rep(df$CN[i], df$SML[i] + df$SSW[i]),
    Layer = c(rep("SML", df$SML[i]), rep("SSW", df$SSW[i]))
  )
}))



# Plot

f4f = ggplot(df_long, aes(x = Layer, y = CN, fill = Layer)) +
  geom_violin(color = "white") +
  labs( y = "Carbon Length",
    x = "Sample Type") +
  scale_fill_manual(values = c("SML" = "#afc6e9ff",  "SSW" ="#ccccccff"))+
  theme_bw(base_size = 12)+ 
  theme(plot.background = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "plain", size = 15),
        strip.background = element_blank(),
        axis.ticks.length = unit(-0.25, "cm"), 
        axis.text.x.top  = element_blank(), 
        axis.title.x.top  = element_blank(), 
        axis.text.y.right  = element_blank(), 
        axis.title.y.right  = element_blank())+
  scale_y_continuous(sec.axis  = dup_axis())+
  rremove("grid")


#### Figures 4g and 4h: MICROBIAL SEQUENCING ####

rel16 <- read_excel("data/rel16.xlsx")

rel18 <- read_excel("data/rel18.xlsx")


#relative abundance stack bars for 16s--------------
category_colors <- c(
  "Pseudomonadota" = "#1f78b4",          
  "Cyanobacteriota" = "#33a02c",        
  "Bacteroidota" = "#ff7f00",           
  "Actinomycetota" = "#e31a1c",         
  "Verrucomicrobiota" = "#6a3d9a",      
  "Planctomycetota" = "#b15928",        
  "SAR324_clade(Marine_group_B)" = "#a6cee3",  
  "Marinimicrobia_(SAR406_clade)" = "#b2df8a", 
  "Bacillota" = "#fb9a99",              
  "Bdellovibrionota" = "#fdbf6f",       
  "Others" = "#999999"                  
)

f4g = rel16|>
  filter(no == 11)|>
  mutate(Time = factor(Time, levels = c("Sunrise", "Midday", "Sunset", "Night")))|>
  
  group_by(type, Day, station, Time, no, ord, phylum)|>
  summarise(relAb = sum(relAb))|> 
  mutate(phylum = ifelse((relAb>0.5), phylum, "Others"))|>
  mutate(phylum = factor(phylum, levels = c("Pseudomonadota" ,          
                                            "Cyanobacteriota" ,        
                                            "Bacteroidota" ,           
                                            "Actinomycetota" ,         
                                            "Verrucomicrobiota" ,      
                                            "Planctomycetota",        
                                            "SAR324_clade(Marine_group_B)",  
                                            "Marinimicrobia_(SAR406_clade)" , 
                                            "Bacillota" ,              
                                            "Bdellovibrionota" ,       
                                            "Others"
                                            
  )))|> 
  ggbarplot( x = "type", y = "relAb", fill = "phylum" 
            , palette = category_colors ,color = "white")+
  theme_bw(base_size = 15)+ 
  theme(plot.background = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "plain", size = 15),
        strip.background = element_blank(),
        axis.ticks.length = unit(-0.25, "cm"), 
        axis.text.x.top  = element_blank(), axis.title.x.top  = element_blank(), 
        axis.text.y.right  = element_blank(), 
        axis.title.y.right  = element_blank())+
  scale_y_continuous(sec.axis  = dup_axis())+
  rremove("grid")+
  labs(y = "Bacteria Relative Abundance", x = "Sample Type", fill = "Phylum" )
  
  
  
 f4h = rel18|>
  filter(no == 11)|>
  mutate(Time = factor(Time, levels = c("Sunrise", "Midday", "Sunset", "Night")))|>
  group_by(type, Day, station, Time, no, ord, Division)|>
  summarise(relAb = sum(relAb))|> 
  mutate(Division = ifelse((relAb>0.5), Division, "Others"))|>
   ggbarplot( x = "type", y = "relAb", fill = "Division" , palette = "Set2" ,color = "white"
)+
  theme_bw(base_size = 15)+ 
   theme(plot.background = element_blank(),
         plot.title = element_text(hjust = 0.5, face = "plain", size = 15),
         strip.background = element_blank(),
         axis.ticks.length = unit(-0.25, "cm"), 
         axis.text.x.top  = element_blank(), 
         axis.title.x.top  = element_blank(), 
         axis.text.y.right  = element_blank(), 
         axis.title.y.right  = element_blank())+
  scale_y_continuous(sec.axis  = dup_axis())+
  rremove("grid")+
  labs(y = "Protist Relative Abundance", x = "Sample Type", fill = "Division" )

