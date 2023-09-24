# Ag Survey 
library(readr)
library(tidyverse)
#-------------------------------------------------------------------------------
# Importing Data
#-------------------------------------------------------------------------------
Data <- read_csv("~/Simpson Centre/AE Survey/Data.csv")

Survey.Data<-filter(Data, Consent !=3)

Survey.Data[,9:14][is.na(Survey.Data[, 9:14])] <- 0

Survey.Data<-Survey.Data|>
  mutate(Treatment.Group = ifelse(Q10 != 0, "Control",
                                  ifelse(Q15!=0, "60% CF",
                                         ifelse(Q23 !=0, "80% CF",
                                                ifelse(Q26 !=0, "100% CF",
                                                       ifelse(Q29 !=0, "120% CF",
                                                              ifelse(Q32!=0, "140% CF", "NA")))))))|>
  filter(Treatment.Group != "NA")|>
  mutate(CT.Preference = Q10+Q15+Q23+Q26+Q29+Q32)|> 
  select(-8:-14)|>
  rename(Primary.Shopper = 8,
         Shop.Type.1 = 9,
         Shop.Type.2 = 10, 
         Shop.Type.3 = 11,
         Shop.Type.4 = 12, 
         Shop.Type.5 = 13, 
         Shop.Type.6 = 14,
         Label.ENV = 15, 
         Label.Local = 16, 
         Label.FT = 17, 
         Label.COOL = 18,
         Label.Cost = 19, 
         Label.Practice = 20, 
         Spending = 21)|>
  
  mutate(ST.Discount.Retail = ifelse(Shop.Type.1 == 1, 1, 
                                  ifelse(Shop.Type.2 == 1,2, 
                                         ifelse(Shop.Type.3 == 1, 3,
                                                ifelse(Shop.Type.4 == 1, 4,
                                                       ifelse(Shop.Type.5 == 1, 5, 
                                                              ifelse(Shop.Type.6 == 1, 6,0)))))), 
         ST.Farmers.Market = ifelse(Shop.Type.1 == 2, 1, 
                                 ifelse(Shop.Type.2 == 2,2, 
                                        ifelse(Shop.Type.3 == 2, 3,
                                               ifelse(Shop.Type.4 == 2, 4,
                                                      ifelse(Shop.Type.5 == 2, 5, 
                                                             ifelse(Shop.Type.6 == 2, 6,0)))))),
         ST.Supermarket = ifelse(Shop.Type.1 == 3, 1, 
                              ifelse(Shop.Type.2 == 3,2, 
                                     ifelse(Shop.Type.3 == 3, 3,
                                            ifelse(Shop.Type.4 == 3, 4,
                                                   ifelse(Shop.Type.5 == 3, 5, 
                                                          ifelse(Shop.Type.6 == 3, 6,0)))))),
         ST.Specialty = ifelse(Shop.Type.1 == 4, 1, 
                            ifelse(Shop.Type.2 == 4,2, 
                                   ifelse(Shop.Type.3 == 4, 3,
                                          ifelse(Shop.Type.4 == 4, 4,
                                                 ifelse(Shop.Type.5 == 4, 5, 
                                                        ifelse(Shop.Type.6 == 4, 6,0)))))),
         ST.Supercenter = ifelse(Shop.Type.1 == 5, 1, 
                              ifelse(Shop.Type.2 == 5,2, 
                                     ifelse(Shop.Type.3 == 5, 3,
                                            ifelse(Shop.Type.4 == 5, 4,
                                                   ifelse(Shop.Type.5 == 5, 5, 
                                                          ifelse(Shop.Type.6 == 5, 6,0)))))),
         ST.Wholesale.Club = ifelse(Shop.Type.1 == 6, 1, 
                                 ifelse(Shop.Type.2 == 6,2, 
                                        ifelse(Shop.Type.3 == 6, 3,
                                               ifelse(Shop.Type.4 == 6, 4,
                                                      ifelse(Shop.Type.5 == 6, 5, 
                                                             ifelse(Shop.Type.6 == 6, 6,0)))))))|>
  rename(Province = 5,
         Age = 6,
         Sex = 7,
         CC.Meet.Commitments = 28, 
         CC.Too.Ambitious = 29,
         CC.CT.Effective = 30, 
         CC.Do.More = 31,
         CC.Overstated = 32, 
         CC.Benifit = 33,
         AG.Food.Security = 34,
         AG.Food.Production = 35,
         AG.Free.Trade = 36, 
         AG.Small.Farm = 37,
         AG.No.Support = 38, 
         AG.Set.Price = 39, 
         CC.AG.GHG.Ag = 40, 
         CC.AG.GHG.Beef = 41, 
         CC.AG.Production.limits = 42, 
         CC.AG.CT.Exempt = 43, 
         CC.AG.Require.BMP = 44, 
         CC.AG.CFL = 45,
         Gov.Invlovement.Ag = 46, 
         Policy.Issues.CC = 47,
         Policy.Issues.Spending = 48,
         Policy.Issues.International = 49, 
         Policy.Issues.Ag = 50,
         Voting = 51, 
         HH.Size = 52, 
         Dependents = 53, 
         Community.Type = 54, 
         Farming.HH = 56, 
         Income = 57, 
         Education= 58)|>
  select(-22:-27)|>
  mutate(Age = ifelse(Age == 10, 1,
                      ifelse(Age == 11, 2, 3)),
         CC.AG.GHG.Ag = ifelse(CC.AG.GHG.Ag == 13, 1,
                               ifelse(CC.AG.GHG.Ag == 14, 2,
                                      ifelse(CC.AG.GHG.Ag == 15, 3,
                                             ifelse(CC.AG.GHG.Ag == 16, 4, 5)))),
         CC.AG.GHG.Beef = ifelse(CC.AG.GHG.Beef == 13, 1,
                               ifelse(CC.AG.GHG.Beef == 14, 2,
                                      ifelse(CC.AG.GHG.Beef == 15, 3,
                                             ifelse(CC.AG.GHG.Beef == 16, 4, 5)))),
         CC.AG.Production.limits = ifelse(CC.AG.Production.limits == 13, 1,
                                          ifelse(CC.AG.Production.limits == 14, 2,
                                                 ifelse(CC.AG.Production.limits == 15, 3,
                                                        ifelse(CC.AG.Production.limits == 16, 4, 5)))),
         CC.AG.CT.Exempt = ifelse(CC.AG.CT.Exempt == 13, 1,
                                          ifelse(CC.AG.CT.Exempt == 14, 2,
                                                 ifelse(CC.AG.CT.Exempt == 15, 3,
                                                        ifelse(CC.AG.CT.Exempt == 16, 4, 5)))),
         CC.AG.Require.BMP = ifelse(CC.AG.Require.BMP == 13, 1,
                                  ifelse(CC.AG.Require.BMP == 14, 2,
                                         ifelse(CC.AG.Require.BMP == 15, 3,
                                                ifelse(CC.AG.Require.BMP == 16, 4, 5)))),
         CC.AG.CFL = ifelse(CC.AG.CFL == 13, 1,
                                    ifelse(CC.AG.CFL == 14, 2,
                                           ifelse(CC.AG.CFL == 15, 3,
                                                  ifelse(CC.AG.CFL == 16, 4, 5)))),
         Farming.HH = ifelse(Farming.HH==3, 1, 
                             ifelse(Farming.HH==4, 2, 3)))
         
         
write.csv(Survey.Data, file = "~/Simpson Centre/AE Survey/Survey_Data.csv")

# Factors 
         
likely <- c("Extremely unlikely", "Somewhat unlikely", "Neither likely nor unlikely", "Somewhat likely", "Extremely likely")      
Shopping <- c("Me", "Shared", "Other")       
Important <-c("Not important", "Slightly important", "Moderately important", "Very important", "Extremely important")
Spending <-c("< $50","$50-$100", "$100-$150", "$150-$200", "$200-$250", "> $250" )
Agree <-c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree")
Level<-c("No Reg + No Fin", "No Reg + Fin", "Reg + Fin", "Reg + No Fin")
Party<-c("LPC", "CPC", "BQ", "NDP", "PPC", "Other", "PNA")
Yes <-c("Yes", "No", "PNS")
Community.Type <-c("Urban", "Suburban", "Rural", "Other")
Edu <- c("< HS", "HS", "Tech School", "Some university", "Bachelor's degree", "Advanced degree", "PNS")
Prov <-c("AB", "BC", "MB", "NB", "NL", "NT", "NS", "NU", "ON", "PE", "QC", "SK", "YT")
Age <- c("18-34", "35-54", "55+")
Gender<-c("M", "F", "O")


Survey.Data$Screen1 <-factor(Survey.Data$Screen1, levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13),labels = Prov)
Survey.Data$`Screen 2`<-factor(Survey.Data$`Screen 2`, levels = c(10,11,12), labels = Age)
Survey.Data$`Screen 3`<-factor(Survey.Data$`Screen 3`,levels = c(1,2,3), labels = Gender)
Survey.Data$Env.Label<-factor(Survey.Data$Env.Label, levels = c(1,2,3,4,5), labels = Important)
Survey.Data$Local.Label<-factor(Survey.Data$Local.Label, levels = c(1,2,3,4,5), labels = Important)
Survey.Data$Fair.Trade<-factor(Survey.Data$Fair.Trade, levels = c(1,2,3,4,5), labels = Important)
Survey.Data$Country.Label<-factor(Survey.Data$Country.Label, levels = c(1,2,3,4,5), labels = Important)
Survey.Data$Cost<-factor(Survey.Data$Cost, levels = c(1,2,3,4,5), labels = Important)
Survey.Data$Practice<-factor(Survey.Data$Practice, levels = c(1,2,3,4,5), labels = Important)
Survey.Data$Spending<-factor(Survey.Data$Spending, levels = c(1,2,3,4,5,6), labels = Spending)

Survey.Data$CC.Meet.Commitments<-factor(Survey.Data$CC.Meet.Commitments, levels = c(1,2,3,4,5), labels = Agree)
Survey.Data$CC.Too.Ambitious<-factor(Survey.Data$CC.Too.Ambitious, levels = c(1,2,3,4,5), labels = Agree)
Survey.Data$CC.CT.Effective<-factor(Survey.Data$CC.CT.Effective, levels = c(1,2,3,4,5), labels = Agree)
Survey.Data$CC.Do.More<-factor(Survey.Data$CC.Do.More, levels = c(1,2,3,4,5), labels = Agree)
Survey.Data$CC.Overstated<-factor(Survey.Data$CC.Overstated, levels = c(1,2,3,4,5), labels = Agree)
Survey.Data$CC.Benifit<-factor(Survey.Data$CC.Benifit, levels = c(1,2,3,4,5), labels = Agree)

Survey.Data$AG.Food.Security<-factor(Survey.Data$AG.Food.Security, levels = c(1,2,3,4,5), labels = Agree)
Survey.Data$AG.Food.Production<-factor(Survey.Data$AG.Food.Production, levels = c(1,2,3,4,5), labels = Agree)
Survey.Data$AG.Free.Trade<-factor(Survey.Data$AG.Free.Trade, levels = c(1,2,3,4,5), labels = Agree)
Survey.Data$AG.Small.Farm<-factor(Survey.Data$AG.Small.Farm, levels = c(1,2,3,4,5), labels = Agree)
Survey.Data$AG.No.Support<-factor(Survey.Data$AG.No.Support, levels = c(1,2,3,4,5), labels = Agree)
Survey.Data$AG.Set.Price<-factor(Survey.Data$AG.Set.Price, levels = c(1,2,3,4,5), labels = Agree)

Survey.Data$CC.AG.GHG.Ag<-factor(Survey.Data$CC.AG.GHG.Ag, levels = c(13:17), labels = Agree)
Survey.Data$CC.AG.GHG.Beef<-factor(Survey.Data$CC.AG.GHG.Beef, levels = c(13:17), labels = Agree)
Survey.Data$CC.AG.Production.limits<-factor(Survey.Data$CC.AG.Production.limits, levels = c(13:17), labels = Agree)
Survey.Data$CC.AG.CT.Exempt<-factor(Survey.Data$CC.AG.CT.Exempt, levels = c(13:17), labels = Agree)
Survey.Data$CC.AG.Require.BMP<-factor(Survey.Data$CC.AG.Require.BMP, levels = c(13:17), labels = Agree)
Survey.Data$CC.AG.CFL<-factor(Survey.Data$CC.AG.CFL, levels = c(13:17), labels = Agree)

Survey.Data$Gov.Invlovement.Ag<-factor(Survey.Data$Gov.Invlovement.Ag, levels = c(1,2,4,5), labels = Level)

Survey.Data$Policy.Issues.CC<-factor(Survey.Data$Policy.Issues.CC, levels = c(1,2,3,4,5), labels = Important)
Survey.Data$Policy.Issues.Spending<-factor(Survey.Data$Policy.Issues.Spending, levels = c(1,2,3,4,5), labels = Important)
Survey.Data$Policy.Issues.International<-factor(Survey.Data$Policy.Issues.International, levels = c(1,2,3,4,5), labels = Important)
Survey.Data$Policy.Issues.Ag<-factor(Survey.Data$Policy.Issues.Ag, levels = c(1,2,3,4,5), labels = Important)

Survey.Data$Voting<-factor(Survey.Data$Voting, levels = c(1:7), labels = Party)

Survey.Data$Dependents<-factor(Survey.Data$Dependents, levels = c(3:5), labels = Yes )
Survey.Data$Community.Type<-factor(Survey.Data$Community.Type, levels = c(1:4), labels = Community.Type )
Survey.Data$Farming.HH<-factor(Survey.Data$Farming.HH, levels = c(3:5), labels = Yes)
Survey.Data$Education<-factor(Survey.Data$Education, levels = c(1,2,3,4,5,6,8), labels = Edu )

Survey.Data$Preference<-factor(Survey.Data$Preference, levels = c(1:5), labels = likely)


#Figures------------------------------------------------------------------------
setwd("~/Simpson Centre/Ag Survey")
# Province of Residence
ggplot(subset(Survey.Data, !is.na(Survey.Data$Screen1)), aes(Screen1))+
  geom_bar(fill = "dodgerblue4", alpha = 0.75,color = "#333333")+
  geom_text(stat='count', aes(label=..count..), vjust=-1, color = "#333333")+
  labs(caption = "What is your province or territory of residence?")+
  xlab("")+
  ylab("")

ggsave("Survey_EDA.png", plot = last_plot(), width = 13.33, height = 7.5, units = "in", dpi = "retina")
# Age  
ggplot(subset(Survey.Data, !is.na(Survey.Data$`Screen 2`)), aes(`Screen 2`))+
  geom_bar(fill = "dodgerblue4", alpha = 0.75,color = "#333333")+
  geom_text(stat='count', aes(label=..count..), vjust=-1, color = "#333333")+
  labs(caption = "What is your age?")+
  xlab("")+
  ylab("")

ggsave("Survey_EDA.png", plot = last_plot(), width = 13.33, height = 7.5, units = "in", dpi = "retina")

# Gender  
ggplot(subset(Survey.Data, !is.na(Survey.Data$`Screen 3`)), aes(`Screen 3`))+
  geom_bar(fill = "dodgerblue4", alpha = 0.75,color = "#333333")+
  geom_text(stat='count', aes(label=..count..), vjust=-1, color = "#333333")+
  labs(caption = "Which gender do you identify as?")+
  xlab("")+
  ylab("")

ggsave("Survey_EDA.png", plot = last_plot(), width = 13.33, height = 7.5, units = "in", dpi = "retina")

#Food product labeling-------------------------------------------------
# ENV

ggplot(subset(Survey.Data, !is.na(Survey.Data$Env.Label)), aes(`Env.Label`))+
  geom_bar(fill = "dodgerblue4", alpha = 0.75,color = "#333333")+
  geom_text(stat='count', aes(label=..count..), vjust=-1, color = "#333333")+
  labs(caption = "How important are the following factors to you when making food purchasing decisions for your household?\nEnvironmental sustainability certification")+
  xlab("")+
  ylab("")

ggsave("Survey_EDA.png", plot = last_plot(), width = 13.33, height = 7.5, units = "in", dpi = "retina")

#local

ggplot(subset(Survey.Data, !is.na(Survey.Data$Local.Label)), aes(Local.Label))+
  geom_bar(fill = "dodgerblue4", alpha = 0.75,color = "#333333")+
  geom_text(stat='count', aes(label=..count..), vjust=-1, color = "#333333")+
  labs(caption = "How important are the following factors to you when making food purchasing decisions for your household?\nLocally produced certification")+
  xlab("")+
  ylab("")

ggsave("Survey_EDA.png", plot = last_plot(), width = 13.33, height = 7.5, units = "in", dpi = "retina")

#fair trade

ggplot(subset(Survey.Data, !is.na(Survey.Data$Fair.Trade)), aes(Fair.Trade))+
  geom_bar(fill = "dodgerblue4", alpha = 0.75,color = "#333333")+
  geom_text(stat='count', aes(label=..count..), vjust=-1, color = "#333333")+
  labs(caption = "How important are the following factors to you when making food purchasing decisions for your household?\nFair-trade certification")+
  xlab("")+
  ylab("")

ggsave("Survey_EDA.png", plot = last_plot(), width = 13.33, height = 7.5, units = "in", dpi = "retina")

# Country of origin labeling

ggplot(subset(Survey.Data, !is.na(Survey.Data$Country.Label)), aes(Country.Label))+
  geom_bar(fill = "dodgerblue4", alpha = 0.75,color = "#333333")+
  geom_text(stat='count', aes(label=..count..), vjust=-1, color = "#333333")+
  labs(caption = "How important are the following factors to you when making food purchasing decisions for your household?\nCost")+
  xlab("")+
  ylab("")

ggsave("Survey_EDA.png", plot = last_plot(), width = 13.33, height = 7.5, units = "in", dpi = "retina")

# Cost

ggplot(subset(Survey.Data, !is.na(Survey.Data$Cost)), aes(Cost))+
  geom_bar(fill = "dodgerblue4", alpha = 0.75,color = "#333333")+
  geom_text(stat='count', aes(label=..count..), vjust=-1, color = "#333333")+
  labs(caption = "How important are the following factors to you when making food purchasing decisions for your household?\nCost")+
  xlab("")+
  ylab("")

ggsave("Survey_EDA.png", plot = last_plot(), width = 13.33, height = 7.5, units = "in", dpi = "retina")

# Cost

ggplot(subset(Survey.Data, !is.na(Survey.Data$Cost)), aes(Cost))+
  geom_bar(fill = "dodgerblue4", alpha = 0.75,color = "#333333")+
  geom_text(stat='count', aes(label=..count..), vjust=-1, color = "#333333")+
  labs(caption = "How important are the following factors to you when making food purchasing decisions for your household?\nCost")+
  xlab("")+
  ylab("")

ggsave("Survey_EDA.png", plot = last_plot(), width = 13.33, height = 7.5, units = "in", dpi = "retina")

# Farming practices

ggplot(subset(Survey.Data, !is.na(Survey.Data$Practice)), aes(Practice))+
  geom_bar(fill = "dodgerblue4", alpha = 0.75,color = "#333333")+
  geom_text(stat='count', aes(label=..count..), vjust=-1, color = "#333333")+
  labs(caption = "How important are the following factors to you when making food purchasing decisions for your household?\nFarming practices certification (Organic, Biodynamic, Certified Humane, Non-GMO)")+
  xlab("")+
  ylab("")

ggsave("Survey_EDA.png", plot = last_plot(), width = 13.33, height = 7.5, units = "in", dpi = "retina")

#Climate Change--------------------------------------------------------------

# Commitments

ggplot(subset(Survey.Data, !is.na(Survey.Data$CC.Meet.Commitments)), aes(CC.Meet.Commitments))+
  geom_bar(fill = "dodgerblue4", alpha = 0.75,color = "#333333")+
  geom_text(stat='count', aes(label=..count..), vjust=-1, color = "#333333")+
  labs(caption = "Please indicate how strongly you agree with the following statements:\nCanada will meet climate commitments with their current plan.")+
  xlab("")+
  ylab("")

ggsave("Survey_EDA.png", plot = last_plot(), width = 13.33, height = 7.5, units = "in", dpi = "retina")

# Ambitious

ggplot(subset(Survey.Data, !is.na(Survey.Data$CC.Too.Ambitious)), aes(CC.Too.Ambitious))+
  geom_bar(fill = "dodgerblue4", alpha = 0.75,color = "#333333")+
  geom_text(stat='count', aes(label=..count..), vjust=-1, color = "#333333")+
  labs(caption = "Please indicate how strongly you agree with the following statements:\nCanada’s climate commitments are too ambitious.")+
  xlab("")+
  ylab("")

ggsave("Survey_EDA.png", plot = last_plot(), width = 13.33, height = 7.5, units = "in", dpi = "retina")

#CT Effectiveness

ggplot(subset(Survey.Data, !is.na(Survey.Data$CC.CT.Effective)), aes(CC.CT.Effective))+
  geom_bar(fill = "dodgerblue4", alpha = 0.75,color = "#333333")+
  geom_text(stat='count', aes(label=..count..), vjust=-1, color = "#333333")+
  labs(caption = "Please indicate how strongly you agree with the following statements:\nCarbon taxes are effective at reducing greenhouse gas emissions.")+
  xlab("")+
  ylab("")

ggsave("Survey_EDA.png", plot = last_plot(), width = 13.33, height = 7.5, units = "in", dpi = "retina")

#Canada should do more

ggplot(subset(Survey.Data, !is.na(Survey.Data$CC.Do.More)), aes(CC.Do.More))+
  geom_bar(fill = "dodgerblue4", alpha = 0.75,color = "#333333")+
  geom_text(stat='count', aes(label=..count..), vjust=-1, color = "#333333")+
  labs(caption = "Please indicate how strongly you agree with the following statements:\nCanada should do more to meet its international commitments under the Paris Climate Agreement.")+
  xlab("")+
  ylab("")

ggsave("Survey_EDA.png", plot = last_plot(), width = 13.33, height = 7.5, units = "in", dpi = "retina")

# Climate Change effects overstated

ggplot(subset(Survey.Data, !is.na(Survey.Data$CC.Overstated)), aes(CC.Overstated))+
  geom_bar(fill = "dodgerblue4", alpha = 0.75,color = "#333333")+
  geom_text(stat='count', aes(label=..count..), vjust=-1, color = "#333333")+
  labs(caption = "Please indicate how strongly you agree with the following statements:\nThe effects of climate change are overstated.")+
  xlab("")+
  ylab("")

ggsave("Survey_EDA.png", plot = last_plot(), width = 13.33, height = 7.5, units = "in", dpi = "retina")

# Canada's climate commitments benefit the economy

ggplot(subset(Survey.Data, !is.na(Survey.Data$AG.Free.Trade)), aes(AG.Free.Trade))+
  geom_bar(fill = "dodgerblue4", alpha = 0.75,color = "#333333")+
  geom_text(stat='count', aes(label=..count..), vjust=-1, color = "#333333")+
  labs(caption = "Please indicate how strongly you agree with the following statements:\nThe federal government should work to reduce trade barriers for agricultural commodities")+
  xlab("")+
  ylab("")
ggsave("Survey_EDA.png", plot = last_plot(), width = 13.33, height = 7.5, units = "in", dpi = "retina")

#Ag Policy Preferences----------------------------------------------------------
# Free Trade
ggplot(subset(Survey.Data, !is.na(Survey.Data$AG.Free.Trade)), aes(AG.Free.Trade))+
  geom_bar(fill = "dodgerblue4", alpha = 0.75,color = "#333333")+
  geom_text(stat='count', aes(label=..count..), vjust=-1, color = "#333333")+
  labs(caption = "Please indicate how strongly you agree with the following statements:\nThe federal government should work to reduce trade barriers for agricultural commodities")+
  xlab("")+
  ylab("")

ggsave("Survey_EDA.png", plot = last_plot(), width = 13.33, height = 7.5, units = "in", dpi = "retina")

# Prioritizing Small Scale Farms

ggplot(subset(Survey.Data, !is.na(Survey.Data$AG.Small.Farm)), aes(AG.Small.Farm))+
  geom_bar(fill = "dodgerblue4", alpha = 0.75,color = "#333333")+
  geom_text(stat='count', aes(label=..count..), vjust=-1, color = "#333333")+
  labs(caption = "Please indicate how strongly you agree with the following statements:\nThe federal government should prioritize support for small scale family farms.")+
  xlab("")+
  ylab("")

ggsave("Survey_EDA.png", plot = last_plot(), width = 13.33, height = 7.5, units = "in", dpi = "retina")

# No Financial Support

ggplot(subset(Survey.Data, !is.na(Survey.Data$AG.Set.Price)), aes(AG.Set.Price))+
  geom_bar(fill = "dodgerblue4", alpha = 0.75,color = "#333333")+
  geom_text(stat='count', aes(label=..count..), vjust=-1, color = "#333333")+
  labs(caption = "Please indicate how strongly you agree with the following statements:\nThe federal government should provide financial support by setting minimum prices that farmers receive for their products.")+
  xlab("")+
  ylab("")

ggsave("Survey_EDA.png", plot = last_plot(), width = 13.33, height = 7.5, units = "in", dpi = "retina")

#Ag and Climate Change Policy --------------------------------------------------

#Ag Production Limits

ggplot(subset(Survey.Data, !is.na(Survey.Data$CC.AG.Production.limits)), aes(CC.AG.Production.limits))+
  geom_bar(fill = "dodgerblue4", alpha = 0.75,color = "#333333")+
  geom_text(stat='count', aes(label=..count..), vjust=-1, color = "#333333")+
  labs(caption = "Please indicate how strongly you agree with the following statements:\nThe federal government should implement production controls (i.e., by limiting total output) for agricultural products with large carbon footprints.")+
  xlab("")+
  ylab("")

ggsave("Survey_EDA.png", plot = last_plot(), width = 13.33, height = 7.5, units = "in", dpi = "retina")

#Ag Production Limits

ggplot(subset(Survey.Data, !is.na(Survey.Data$CC.AG.Production.limits)), aes(CC.AG.Production.limits))+
  geom_bar(fill = "dodgerblue4", alpha = 0.75,color = "#333333")+
  geom_text(stat='count', aes(label=..count..), vjust=-1, color = "#333333")+
  labs(caption = "Please indicate how strongly you agree with the following statements:\nThe federal government should implement production controls (i.e., by limiting total output) for agricultural products with large carbon footprints.")+
  xlab("")+
  ylab("")

ggsave("Survey_EDA.png", plot = last_plot(), width = 13.33, height = 7.5, units = "in", dpi = "retina")

#Ag Production Limits

ggplot(subset(Survey.Data, !is.na(Survey.Data$CC.AG.Production.limits)), aes(CC.AG.Production.limits))+
  geom_bar(fill = "dodgerblue4", alpha = 0.75,color = "#333333")+
  geom_text(stat='count', aes(label=..count..), vjust=-1, color = "#333333")+
  labs(caption = "Please indicate how strongly you agree with the following statements:\nThe federal government should implement production controls (i.e., by limiting total output) for agricultural products with large carbon footprints.")+
  xlab("")+
  ylab("")

ggsave("Survey_EDA.png", plot = last_plot(), width = 13.33, height = 7.5, units = "in", dpi = "retina")

# Carbon Tax Exemption

ggplot(subset(Survey.Data, !is.na(Survey.Data$CC.AG.CT.Exempt)), aes(CC.AG.CT.Exempt))+
  geom_bar(fill = "dodgerblue4", alpha = 0.75,color = "#333333")+
  geom_text(stat='count', aes(label=..count..), vjust=-1, color = "#333333")+
  labs(caption = "Please indicate how strongly you agree with the following statements:\nEmissions from agricultural production should be exempt from the federal carbon tax.")+
  xlab("")+
  ylab("")

ggsave("Survey_EDA.png", plot = last_plot(), width = 13.33, height = 7.5, units = "in", dpi = "retina")

# BMP Adoption 

ggplot(subset(Survey.Data, !is.na(Survey.Data$CC.AG.Require.BMP)), aes(CC.AG.Require.BMP))+
  geom_bar(fill = "dodgerblue4", alpha = 0.75,color = "#333333")+
  geom_text(stat='count', aes(label=..count..), vjust=-1, color = "#333333")+
  labs(caption = "Please indicate how strongly you agree with the following statements:\nFarmers should be required to adopt measures that reduce greenhouse gas emissions.")+
  xlab("")+
  ylab("")

ggsave("Survey_EDA.png", plot = last_plot(), width = 13.33, height = 7.5, units = "in", dpi = "retina")

# Carbon Footprint Labeling

ggplot(subset(Survey.Data, !is.na(Survey.Data$CC.AG.CFL)), aes(CC.AG.CFL))+
  geom_bar(fill = "dodgerblue4", alpha = 0.75,color = "#333333")+
  geom_text(stat='count', aes(label=..count..), vjust=-1, color = "#333333")+
  labs(caption = "Please indicate how strongly you agree with the following statements:\nCanada should introduce mandatory labelling of food products to show their carbon footprint.")+
  xlab("")+
  ylab("")

ggsave("Survey_EDA.png", plot = last_plot(), width = 13.33, height = 7.5, units = "in", dpi = "retina")

# preferred level of Government Intervention 

ggplot(subset(Survey.Data, !is.na(Survey.Data$Gov.Invlovement.Ag)), aes(Gov.Invlovement.Ag))+
  geom_bar(fill = "dodgerblue4", alpha = 0.75,color = "#333333")+
  geom_text(stat='count', aes(label=..count..), vjust=-1, color = "#333333")+
  labs(caption = "How should the federal government encourage a reduction in agriculture-based greenhouse gas emissions?")+
  xlab("")+
  ylab("")

ggsave("Survey_EDA.png", plot = last_plot(), width = 13.33, height = 7.5, units = "in", dpi = "retina")

# General policy Preferences-----------------------------------------------------
# Spending and taxation
ggplot(subset(Survey.Data, !is.na(Survey.Data$Policy.Issues.Spending)), aes(Policy.Issues.Spending))+
  geom_bar(fill = "dodgerblue4", alpha = 0.75,color = "#333333")+
  geom_text(stat='count', aes(label=..count..), vjust=-1, color = "#333333")+
  labs(caption = "How important are the following issues to you when making voting decisions in a federal election?\nGovernment Spending & Taxation")+
  xlab("")+
  ylab("")

ggsave("Survey_EDA.png", plot = last_plot(), width = 13.33, height = 7.5, units = "in", dpi = "retina")

# International Relations

ggplot(subset(Survey.Data, !is.na(Survey.Data$Policy.Issues.International)), aes(Policy.Issues.International))+
  geom_bar(fill = "dodgerblue4", alpha = 0.75,color = "#333333")+
  geom_text(stat='count', aes(label=..count..), vjust=-1, color = "#333333")+
  labs(caption = "How important are the following issues to you when making voting decisions in a federal election?\nInternational Trade & International Relations")+
  xlab("")+
  ylab("")

ggsave("Survey_EDA.png", plot = last_plot(), width = 13.33, height = 7.5, units = "in", dpi = "retina")

# Agriculture & Food Policy

ggplot(subset(Survey.Data, !is.na(Survey.Data$Policy.Issues.Ag)), aes(Policy.Issues.Ag))+
  geom_bar(fill = "dodgerblue4", alpha = 0.75,color = "#333333")+
  geom_text(stat='count', aes(label=..count..), vjust=-1, color = "#333333")+
  labs(caption = "How important are the following issues to you when making voting decisions in a federal election?\nAgriculture & Food policy")+
  xlab("")+
  ylab("")

ggsave("Survey_EDA.png", plot = last_plot(), width = 13.33, height = 7.5, units = "in", dpi = "retina")

# Party Preference
ggplot(subset(Survey.Data, !is.na(Survey.Data$Voting)), aes(Voting))+
  geom_bar(fill = "dodgerblue4", alpha = 0.75,color = "#333333")+
  geom_text(stat='count', aes(label=..count..), vjust=-1, color = "#333333")+
  labs(caption = "If a federal election were to be held tomorrow, which party would you vote for?")+
  xlab("")+
  ylab("")

ggsave("Survey_EDA.png", plot = last_plot(), width = 13.33, height = 7.5, units = "in", dpi = "retina")

#Demographics----------------------------------------------------------------
# Household size

ggplot(subset(Survey.Data, !is.na(Survey.Data$HH.Size)), aes(factor(HH.Size)))+
  geom_bar(fill = "dodgerblue4", alpha = 0.75,color = "#333333")+
  geom_text(stat='count', aes(label=..count..), vjust=-1, color = "#333333")+
  labs(caption = "Including yourself, how many people are in your household?")+
  xlab("")+
  ylab("")

ggsave("Survey_EDA.png", plot = last_plot(), width = 13.33, height = 7.5, units = "in", dpi = "retina")

# Dependents

ggplot(subset(Survey.Data, !is.na(Survey.Data$Dependents)), aes(factor(Dependents)))+
  geom_bar(fill = "dodgerblue4", alpha = 0.75,color = "#333333")+
  geom_text(stat='count', aes(label=..count..), vjust=-1, color = "#333333")+
  labs(caption = "Are there any members of your household under the age of 18?")+
  xlab("")+
  ylab("")

ggsave("Survey_EDA.png", plot = last_plot(), width = 13.33, height = 7.5, units = "in", dpi = "retina")

# GG location

ggplot(subset(Survey.Data, !is.na(Survey.Data$Community.Type)), aes(factor(Community.Type)))+
  geom_bar(fill = "dodgerblue4", alpha = 0.75,color = "#333333")+
  geom_text(stat='count', aes(label=..count..), vjust=-1, color = "#333333")+
  labs(caption = "What type of community do you live in?")+
  xlab("")+
  ylab("")

ggsave("Survey_EDA.png", plot = last_plot(), width = 13.33, height = 7.5, units = "in", dpi = "retina")

#Farming Household

ggplot(subset(Survey.Data, !is.na(Survey.Data$Farming.HH)), aes(factor(Farming.HH)))+
  geom_bar(fill = "dodgerblue4", alpha = 0.75,color = "#333333")+
  geom_text(stat='count', aes(label=..count..), vjust=-1, color = "#333333")+
  labs(caption = "Are you or a member of your household involved in farming?")+
  xlab("")+
  ylab("")

ggsave("Survey_EDA.png", plot = last_plot(), width = 13.33, height = 7.5, units = "in", dpi = "retina")

#Income

Income<-c("<$10,000", "$10,000 - $19,999", "$20,000 - $29,999", "$30,000 - $39,999", "$40,000 - $49,999",
          "$50,000 - $59,999", "$60,000 - $69,999", "$70,000 - $79,999", "$80,000 - $89,999", "$90,000 - $99,999",
          "$100,000 - $124,999", "$125,000 - $149,999", "$150,000 - $199,999", "$200,000-$250,000", "More than $250,000", 
          "Prefer not to say")
Survey.Data$Income<-factor(Survey.Data$Income, levels = c(1:16), labels = Income)
rm(Income)

ggplot(subset(Survey.Data, !is.na(Survey.Data$Income)), aes(factor(Income)))+
  geom_bar(fill = "dodgerblue4", alpha = 0.75,color = "#333333")+
  geom_text(stat='count', aes(label=..count..), vjust=-1, color = "#333333")+
  labs(caption = "What is your household pre-tax income?")+
  xlab("")+
  ylab("")+
  theme(axis.text.x = element_text(vjust = grid::unit(c(0, 2), "points"))) 
  

ggsave("Survey_EDA.png", plot = last_plot(), width = 13.33, height = 7.5, units = "in", dpi = "retina")

# Education



ggplot(subset(Survey.Data, !is.na(Survey.Data$Education)), aes(Education))+
  geom_bar(fill = "dodgerblue4", alpha = 0.75,color = "#333333")+
  geom_text(stat='count', aes(label=..count..), vjust=-1, color = "#333333")+
  labs(caption = "What is your household pre-tax income?")+
  xlab("")+
  ylab("")


ggsave("Survey_EDA.png", plot = last_plot(), width = 13.33, height = 7.5, units = "in", dpi = "retina")

#-------------------------------------------------------------------------------#
# Explority factor Analysis
#-------------------------------------------------------------------------------#


install.packages("psych")
install.packages("coorplot")

library(corrplot)
library(psych)

#Factors----

##Test 1. Remove NA

EDA<-na.omit(EDA)

EDA<-EDA|>
  mutate(CC.AG.GHG.Ag = ifelse(CC.AG.GHG.Ag==13, 1,
                               ifelse(CC.AG.GHG.Ag==14, 2,
                                      ifelse(CC.AG.GHG.Ag==15, 3,
                                             ifelse(CC.AG.GHG.Ag==16, 4, 5)))),
         
         CC.AG.GHG.Beef = ifelse(CC.AG.GHG.Beef==13, 1,
                                 ifelse(CC.AG.GHG.Beef==14, 2,
                                        ifelse(CC.AG.GHG.Beef==15, 3,
                                               ifelse(CC.AG.GHG.Beef==16, 4, 5)))),
         
         CC.AG.Production.limits = ifelse(CC.AG.Production.limits==13, 1,
                                          ifelse(CC.AG.Production.limits==14, 2,
                                                 ifelse(CC.AG.Production.limits==15, 3,
                                                        ifelse(CC.AG.Production.limits==16, 4, 5)))), 
         
         CC.AG.CT.Exempt = ifelse(CC.AG.CT.Exempt == 13, 1,
                                  ifelse(CC.AG.CT.Exempt == 14, 2,
                                         ifelse(CC.AG.CT.Exempt == 15, 3,
                                                ifelse(CC.AG.CT.Exempt == 16, 4, 5)))), 
         
         CC.AG.Require.BMP = ifelse(CC.AG.Require.BMP == 13, 1,
                                    ifelse(CC.AG.Require.BMP == 14, 2,
                                           ifelse(CC.AG.Require.BMP == 15, 3,
                                                  ifelse(CC.AG.Require.BMP == 16, 4, 5)))), 
         CC.AG.CFL = ifelse( CC.AG.CFL == 13, 1,
                             ifelse( CC.AG.CFL == 14, 2,
                                     ifelse( CC.AG.CFL == 15, 3,
                                             ifelse( CC.AG.CFL == 16, 4, 5)))))


EDA.3<-EDA|>
  mutate(Env.Label = ifelse(Env.Label <= 2, 1,
                            ifelse(Env.Label == 3, 2, 3)), 
         
         Local.Label = ifelse(Local.Label <= 2, 1,
                              ifelse(Local.Label == 3, 2, 3)),
         
         Fair.Trade = ifelse(Fair.Trade <=2, 1,
                             ifelse(Fair.Trade == 3, 2, 3)),
         
         Country.Label = ifelse(Country.Label <= 2, 1,
                                ifelse(Country.Label == 3, 2, 3)),
         
         Cost = ifelse(Cost<= 2, 1,
                       ifelse(Cost == 3, 2, 3)),
         
         Practice = ifelse(Practice<= 2, 1,
                           ifelse(Practice == 3, 2, 3)),
         
         CC.Meet.Commitments = ifelse(CC.Meet.Commitments <= 2, 1,
                                      ifelse(CC.Meet.Commitments == 3, 2, 3)),
         
         CC.Too.Ambitious = ifelse(CC.Too.Ambitious<= 2, 1,
                                   ifelse(CC.Too.Ambitious == 3, 2, 3)),
         
         CC.CT.Effective = ifelse(CC.CT.Effective <= 2, 1,
                                  ifelse(CC.CT.Effective == 3, 2, 3)),
         
         CC.Do.More = ifelse(CC.Do.More <= 2, 1,
                             ifelse(CC.Do.More == 3, 2, 3)),
         
         CC.Overstated = ifelse(CC.Overstated  <= 2, 1,
                                ifelse(CC.Overstated  == 3, 2, 3)),
         
         CC.Benifit = ifelse(CC.Benifit <= 2, 1,
                             ifelse(CC.Benifit == 3, 2, 3)),
         
         AG.Food.Security = ifelse(AG.Food.Security <= 2, 1,
                                   ifelse(AG.Food.Security == 3, 2, 3)),
         
         AG.Food.Production = ifelse(AG.Food.Production <= 2, 1,
                                     ifelse(AG.Food.Production <= 3, 2, 3)),
         
         AG.Free.Trade = ifelse(AG.Free.Trade <= 2, 1,
                                ifelse(AG.Free.Trade == 3, 2, 3)),
         
         AG.Small.Farm = ifelse(AG.Small.Farm <= 2, 1,
                                ifelse(AG.Small.Farm == 3, 2, 3)),
         
         AG.No.Support = ifelse(AG.No.Support <= 2, 1,
                                ifelse(AG.No.Support == 3, 2, 3)),
         
         AG.Set.Price = ifelse(AG.Set.Price <= 2, 1,
                               ifelse(AG.Set.Price == 3, 2, 3)),
         
         CC.AG.GHG.Ag = ifelse(CC.AG.GHG.Ag <= 2, 1,
                               ifelse(CC.AG.GHG.Ag == 3, 2, 3)),
         
         CC.AG.GHG.Beef = ifelse(CC.AG.GHG.Beef <= 2, 1,
                                 ifelse(CC.AG.GHG.Beef == 3, 2, 3)),
         
         CC.AG.Production.limits = ifelse(CC.AG.Production.limits <= 2, 1,
                                          ifelse(CC.AG.Production.limits == 3, 2, 3)),
         
         CC.AG.CT.Exempt = ifelse(CC.AG.CT.Exempt <= 2, 1,
                                  ifelse(CC.AG.CT.Exempt == 3, 2, 3)),
         
         CC.AG.Require.BMP = ifelse(CC.AG.Require.BMP <= 2, 1,
                                    ifelse(CC.AG.Require.BMP == 3, 2, 3)),
         
         CC.AG.CFL = ifelse(CC.AG.CFL <= 2, 1,
                            ifelse(CC.AG.CFL == 3, 2, 3)),
         
         Gov.Invlovement.Ag = ifelse(Gov.Invlovement.Ag <= 2, 1,
                                     ifelse(Gov.Invlovement.Ag == 3, 2, 3)),
         
         Policy.Issues.CC = ifelse(Policy.Issues.CC <= 2, 1,
                                   ifelse(Policy.Issues.CC == 3, 2, 3)),
         
         Policy.Issues.Spending = ifelse(Policy.Issues.Spending <= 2, 1,
                                         ifelse(Policy.Issues.Spending == 3, 2, 3)),
         
         Policy.Issues.International = ifelse(Policy.Issues.International <= 2, 1,
                                              ifelse(Policy.Issues.International == 3, 2, 3)),
         
         Policy.Issues.Ag = ifelse(Policy.Issues.Ag <= 2, 1,
                                   ifelse(Policy.Issues.Ag == 3, 2, 3)))

describe(EDA)

head(EDA)

corrplot(cor(EDA))

KMO(r = cor(EDA))

fafitfree <- fa(EDA,nfactors = ncol(EDA), rotate = "none")
n_factors <- length(fafitfree$e.values)
scree     <- data.frame(
  Factor_n =  as.factor(1:n_factors), 
  Eigenvalue = fafitfree$e.values)
ggplot(scree, aes(x = Factor_n, y = Eigenvalue, group = 1)) + 
  geom_point() + geom_line() +
  xlab("Number of factors") +
  ylab("Initial eigenvalue") +
  labs( title = "Scree Plot", 
        subtitle = "(Based on the unreduced correlation matrix)")

parallel <- fa.parallel(EDA.3)

factanal.none <- factanal(EDA.3, factors=4, scores = c("regression"), rotation = "none")
print(factanal.none)

fa.none <- fa(r=EDA, 
              nfactors = 4, 
              # covar = FALSE, SMC = TRUE,
              fm= "pa", # type of factor analysis we want to use (“pa” is principal axis factoring)
              max.iter=100, # (50 is the default, but we have changed it to 100
              rotate= "") # none rotation
print(fa.none)
fa.diagram(fa.none)


