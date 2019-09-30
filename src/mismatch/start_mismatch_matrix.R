
skill_ad <- c(1, 1, 1, 1, 1, 1, 1)
skill_res <- rbind(c(1, 0, 1, 1, 0, 1, 1), c(0, 0, 0, 1, 0, 1, 1), c(0, 0, 0, 0, 1, 1, 1),
                        c(1, 1, 1, 1, 1, 1, 1), c(0, 0, 0, 0, 0, 0, 0), c(1, 1, 1, 0, 1, 1, 1))
skill_ad <- rbind(c(1, 1, 1, 1, 1, 1, 1), c(0, 1, 1, 1, 1, 1, 1), c(0, 0, 0, 1, 1, 1, 1),
                  c(0, 0, 0, 1, 0, 0, 0), c(1, 0, 1, 0, 1, 0, 1))


colnames(skill_ad) <- c("skill1", "skill2", "skill3", "skill4", "skill5", "skill6", "skill7")
rownames(skill_ad) <- c("ad1", "ad2", "ad3", "ad4", "ad5")

colnames(skill_res) <- c("skill1", "skill2", "skill3", "skill4", "skill5", "skill6", "skill7")
rownames(skill_res) <- c("res1", "res2", "res3", "res4", "res5", "res6")

skill_res == skill_ad


apply(skill_ad, MARGIN = 1, function(y){apply(skill_res, MARGIN = 1, function(x){sum(x == y)/length(x)})})


skill_res_n <- skill_res
skill_res_n[skill_res_n == 0] <- NA
skill_ad_n <- skill_ad
skill_ad_n[skill_ad_n == 0] <- NA

match <- apply(skill_ad, MARGIN = 1, function(y){apply(skill_res, MARGIN = 1, function(x){sum(x[x == y], na.rm = TRUE)/sum(y, na.rm = TRUE)})})

enough <- sum(match)/(nrow(match)*ncol(match))

skill_ad_n
skill_res_n

match <- apply(skill_ad, MARGIN = 1, function(y){apply(skill_res, MARGIN = 1, function(x){sum(x[x == y], na.rm = TRUE)/
    sum(y, na.rm = TRUE)})})

match_score <- sum(match)/(nrow(match)*ncol(match))
#ad_avg <- apply(match, MARGIN = 2, function(x){sum(x/nrow(match))})
#enough <- sum(match)/nrow(skill_ad_n)

enough <- sum(apply(match, MARGIN = 1, mean))/nrow(skill_ad)

skill_res_n_2 <- rbind(skill_res_n, skill_res_n)

match2 <- apply(skill_ad, MARGIN = 1, function(y){apply(skill_res_n_2, MARGIN = 1, function(x){sum(x[x == y], na.rm = TRUE)/
    sum(y, na.rm = TRUE)})})

enough2 <- sum(apply(match2, MARGIN = 1, mean))/nrow(skill_ad)
#enough2 <- sum(match2)/nrow(skill_ad_n)

skill_ad_2 <- rbind(skill_ad, skill_ad)

match3 <- apply(skill_ad_2, MARGIN = 1, function(y){apply(skill_res, MARGIN = 1, function(x){sum(x[x == y], na.rm = TRUE)/
    sum(y, na.rm = TRUE)})})

enough3 <- sum(apply(match3, MARGIN = 1, mean))/nrow(skill_ad_2)
