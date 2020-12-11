skaters_full = read.csv("Documents/Stat 420/STAT420-finalproject/Final Project/skater_stats.csv")
head(skaters_full)
names(skaters_full)
tail(skaters_full)
skaters <- subset(skaters_full, Season >= 2000)
View(skaters)
head(skaters)
tail(skaters)

#numeric
View(skaters$Season)
View(skaters$Age)
View(skaters$GP)
View(skaters$G)
View(skaters$GPG)
View(skaters$A)
View(skaters$PTS)
View(skaters$X...)
View(skaters$ATOI)
View(skaters$SHA)
#Variable types that aren't numeric:
#Season = Date
#Player = Character
#TM = Character
#Pos = Character
#ATOI is in colon time form

str(skaters)
skaters$Pos <- as.factor(skaters$Pos)
skaters$Age <- as.numeric(skaters$Age)
skaters$GP <- as.numeric(skaters$GP)
skaters$G <- as.numeric(skaters$G)
skaters$GPG <- as.numeric(skaters$GPG)
skaters$A <- as.numeric(skaters$A)
skaters$PTS <- as.numeric(skaters$PTS)
skaters$X... <- as.numeric(skaters$X...)
skaters$PIM <- as.numeric(skaters$PIM)
skaters$EVG <- as.numeric(skaters$EVG)
skaters$PPG <- as.numeric(skaters$PPG)
skaters$SHG <- as.numeric(skaters$SHG)
skaters$GWG <- as.numeric(skaters$GWG)
skaters$EVA <- as.numeric(skaters$EVA)
skaters$PPA <- as.numeric(skaters$PPA)
skaters$SHA <- as.numeric(skaters$SHA)
skaters$S <- as.numeric(skaters$S)
skaters$S. <- as.numeric(skaters$S.)
skaters$TOI <- as.numeric(skaters$TOI)
skaters$BLK <- as.numeric(skaters$BLK)
skaters$HIT <- as.numeric(skaters$HIT)
skaters$FOwin <- as.numeric(skaters$FOwin)
skaters$FOloss <- as.numeric(skaters$FOloss)
skaters$FO. <- as.numeric(skaters$FO.)
typeof(skaters$Pos)
str(skaters)

skaters[is.na(skaters)] = 0

# skaters$G[is.na(skaters$G)] <- 0
# skaters$A[is.na(skaters$A)] <- 0
# skaters$EVG[is.na(skaters$EVG)] <- 0
# skaters$PPG[is.na(skaters$PPG)] <- 0
# skaters$PTS[is.na(skaters$PTS)] <- 0
# skaters$SHG[is.na(skaters$SHG)] <- 0
# skaters$GWG[is.na(skaters$GWG)] <- 0
# skaters$EVA[is.na(skaters$EVA)] <- 0
# skaters$SHG[is.na(skaters$SHG)] <- 0
# skaters$PPA[is.na(skaters$PPA)] <- 0
# skaters$SHA[is.na(skaters$SHA)] <- 0
# skaters$S[is.na(skaters$S)] <- 0
# skaters$S.[is.na(skaters$S.)] <- 0
# skaters$TOI[is.na(skaters$TOI)] <- 0
# skaters$FO.[is.na(skaters$FO.)] <- 0
# skaters$X..[is.na(skaters$X..)] <- 0
# skaters$PIM[is.na(skaters$PIM)] <- 0
str(skaters)
#all NAs have been changed to 0s

#Remove variables that are calculated using other variables in the data
skaters_cleaned <- skaters[, c(4, 6, 7, 8, 10, 13, 21, 25, 26, 27, 28)]
View(skaters_cleaned)
names(skaters_cleaned)

#full additive model
goals_addfull = lm(G ~ ., data = skaters_cleaned)
summary(goals_addfull)
summary(goals_addfull)$r.sq

#find the "best" additive model using back AIC, starting with the additive model
goals_addfull = lm(G ~ ., data = skaters_cleaned)
plot(goals_addfull)

goals_model_back_aic = step(goals_addfull, direction = "backward", trace = 0)
summary(goals_model_back_aic)$adj.r.sq

n = length(resid(goals_addfull))
goals_model_back_bic = step(goals_addfull, direction = "backward", k = log(n), trace = 0)
summary(goals_model_back_bic)$adj.r.sq

#a squared model:
goals_bigModel = lm(G ~. ^ 2 + I(Age ^ 2)  + I(GP ^ 2) + I(A ^ 2) + I(PIM ^ 2) + I(S ^ 2) + I(BLK ^ 2) + I(FOwin ^ 2) + I(FOloss ^ 2), data = skaters_cleaned)
summary(goals_bigModel)$r.sq
big_model_back_aic = step(goals_bigModel, direction = "backward", trace = 0)
summary(big_model_back_aic)
summary(big_model_back_aic)$r.sq

#interactive model
#goals_int = lm(G ~ Age * Pos * GP * A * PIM, data = skaters_cleaned)
#summary(goals_int)

#Response variable: Goals (G)
#Some Key Variables: Age, Team (Tm), Position (Pos), Games Played (GP), Goals (G), Points (PTS)
#don't use points; points is calculated w/ goals
#don't use: PTS, GPG, EVG, PPG, SHG, GWG, S., X
#additive models
# goals_addfull = lm(G ~ . - PTS - GPG - EVG - PPG - SHG - GWG - S. - X, data = skaters)
# summary(goals_addfull)$r.sq
# #R-sq for addfull model is 0.9244623
# goals_addmod = lm(G ~ . - X - Season - Player - Tm - Pos, data = skaters)
# summary(goals_addmod)$r.sq
# goals_mod1 = lm(G ~ Age + Pos, data = skaters)
# summary(goals_mod1)$r.sq
# goals_mod2 = lm(G ~ Age + Pos + GP, data = skaters)
# summary(goals_mod2)$r.sq
# goals_mod3 = lm(G ~ Age + Pos + GP + A, data = skaters)
# summary(goals_mod3)$r.sq
# goals_mod4 = lm(G ~ Age + Pos + GP + A + PIM, data = skaters)
# summary(goals_mod4)$r.sq
# goals_mod5 = lm(G ~ Age + Pos + GP + A + PIM + EVG, data = skaters)
# summary(goals_mod5)$r.sq
# goals_mod6 = lm(G ~ Age + Pos + GP + A + PIM + EVG + S, data = skaters)
# summary(goals_mod6)$r.sq
# goals_mod7 = lm(G ~ Age + Pos + GP + A + PIM + EVG + S + TOI, data = skaters)
# summary(goals_mod7)$r.sq
# goals_mod8 = lm(G ~ Age + Pos + GP + A + PIM + EVG + S + TOI + BLK, data = skaters)
# summary(goals_mod8)$r.sq
# #R-sq value of goals_mod8 = 0.9573586

#interaction models take too long to run
#don't use: PTS, GPG, EVG, PPG, SHG, GWG, S., X
#goals_intfull = lm(G ~ Season * Player , data = skaters)
#* Age * Tm * Pos * GP * A * X... * PIM * EVA * PPA * SHA * S * TOI * ATOI
#* BLK * HIT * FOwin * FOloss * FO.

#functions to check assumptions:
plot_fitted_resid = function(model, pointcol = "dodgerblue", linecol = "darkorange") {
  plot(fitted(model), resid(model), 
       col = pointcol, pch = 20, cex = 1.5,
       xlab = "Fitted", ylab = "Residuals")
  abline(h = 0, col = linecol, lwd = 2)
}

plot_qq = function(model, pointcol = "dodgerblue", linecol = "darkorange") {
  qqnorm(resid(model), col = pointcol, pch = 20, cex = 1.5)
  qqline(resid(model), col = linecol, lwd = 2)
}

names(skaters_cleaned)
sum(skaters$G < 0)


goals_exper = lm(G ~ I(Age ^ 1/4) + Pos + I(GP ^ 1/4) + I(A ^ 1/4) + I(PIM ^ 1/4) + 
                   I(S ^ 1/4)  + I(BLK ^ 1/4) + I(HIT ^ 1/4) + I(FOwin ^ 1/4) + I(FOloss ^ 1/4), data = skaters_cleaned)
  
plot_fitted_resid(goals_exper)
bptest(goals_exper)
plot_qq(goals_exper)
library(MASS)
boxcox(goals_addfull, plotit = TRUE)
#check assumptions
plot_fitted_resid(goals_addfull)
plot_qq(goals_addfull)
plot_fitted_resid(goals_model_back_aic)
plot_fitted_resid(goals_model_back_bic)
plot_fitted_resid(goals_bigModel)
plot_qq(goals_bigModel)
plot_qq(goals_model_back_aic)
plot_qq(goals_model_back_bic)
library(lmtest)
bptest(goals_addfull)
#constant variance assumption is violated
shapiro.test(resid(goals_addfull))
#can't run shapiro test bc sample size is too large
bptest(goals_model_back_aic)
#constant variance assumption is violated
bptest(goals_model_back_bic)
#constant variance assumption violated
bptest(goals_bigModel)







