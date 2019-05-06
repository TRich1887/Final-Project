
EPLData <-read.csv("EPL Seasons Data.csv")
View(EPLData)

library(corrplot)


NumData<-read.csv("Numeric Data.csv")
S <-cor(NumData, use="pairwise.complete.obs")
corrplot(S, method = "number")

S <-cor(NumData, use="pairwise.complete.obs")
corrplot(S, method = "circle")


library(tidyverse)

ggplot(EPLData,aes(x=Total.Goals,y=Total.Wins,color = Team,size = 3))+geom_point()+
geom_text(aes(label=ifelse(Total.Wins>100,as.character(Team),'')),hjust=1, vjust=1)+
ggtitle("Wins Compared To Goals")+xlab("Total Goals")+ylab("Total Wins")
ggplot(EPLData, aes(x = Team, y = Indiv.Years.in.EPL, fill = factor(Indiv.Group)))+ geom_bar(stat = "identity")+ggtitle("Years in Premier League") +ylab("Years in EPL")


goalfit <- lm(goals ~wins+losses+total_yel_card+total_red_card+total_scoring_att+ontarget_scoring_att+hit_woodwork+total_offside+clean_sheet+goals_conceded+saves+outfielder_block+interception+total_tackle+last_man_tackle+total_clearance+own_goals+penalty_conceded+pen_goals_conceded+total_pass+total_long_balls+total_cross+corner_taken+touches+clearance_off_line+penalty_save+total_high_claim+punches, data = EPLData)
summary(goalfit)

winfit <- lm(wins ~goals+total_yel_card+total_red_card+total_scoring_att+ontarget_scoring_att+hit_woodwork+att_hd_goal+att_pen_goal+att_freekick_goal+att_ibox_goal+att_obox_goal+goal_fastbreak+total_offside+clean_sheet+goals_conceded+saves+outfielder_block+interception+total_tackle+last_man_tackle+total_clearance+own_goals+penalty_conceded+pen_goals_conceded+total_pass+total_long_balls+total_cross+corner_taken+touches+clearance_off_line+penalty_save+total_high_claim+punches, data = EPLData)
summary(winfit)
View(winfit)

predgoals<- predict(lm(goals ~wins+losses+total_yel_card+total_red_card+total_scoring_att+ontarget_scoring_att+hit_woodwork+total_offside+clean_sheet+goals_conceded+saves+outfielder_block+interception+total_tackle+last_man_tackle+total_clearance+own_goals+penalty_conceded+pen_goals_conceded+total_pass+total_long_balls+total_cross+corner_taken+touches+clearance_off_line+penalty_save+total_high_claim+punches, data = EPLData))
predwins<- predict(lm(wins ~goals+total_yel_card+total_red_card+total_scoring_att+ontarget_scoring_att+hit_woodwork+att_hd_goal+att_pen_goal+att_freekick_goal+att_ibox_goal+att_obox_goal+goal_fastbreak+total_offside+clean_sheet+goals_conceded+saves+outfielder_block+interception+total_tackle+last_man_tackle+total_clearance+own_goals+penalty_conceded+pen_goals_conceded+total_pass+total_long_balls+total_cross+corner_taken+touches+clearance_off_line+penalty_save+total_high_claim+punches, data = EPLData))
View(predwins)
View(predgoals)

EPLData <- EPLData %>% mutate(exp.tier = case_when(
  Years.in.EPL<=3 ~ 3,
  Years.in.EPL>3 & Years.in.EPL<=9 ~ 2,
  Years.in.EPL>9 ~ 1),
  exp.tier = as.factor(exp.tier)
)

summary(lm(total_scoring_att ~ exp.tier, data=EPLData))
summary(lm(ontarget_scoring_att ~ exp.tier, data=EPLData))
summary(lm(goal_fastbreak~ exp.tier, data=EPLData))
summary(lm(total_offside ~ exp.tier, data=EPLData))
summary(lm(interception ~ exp.tier, data=EPLData))
summary(lm(total_tackle ~ exp.tier, data=EPLData))
summary(lm(outfielder_block ~ exp.tier, data=EPLData))
summary(lm(total_clearance ~ exp.tier, data=EPLData))
summary(lm(total_pass ~ exp.tier, data=EPLData))
summary(lm(total_long_balls ~ exp.tier, data=EPLData))
summary(lm(total_cross ~ exp.tier, data=EPLData))
summary(lm(touches ~ exp.tier, data=EPLData))




