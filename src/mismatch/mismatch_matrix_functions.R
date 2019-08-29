mismatch_matrix <- function(ad_skill, res_skill){
  apply(ad_skill, MARGIN = 1, function(y){apply(res_skill, MARGIN = 1,
    function(x){sum(x[x == y], na.rm = TRUE)/sum(y, na.rm = TRUE)})})
}


mismatch_score <- function(ad_skill, res_skill){
match_matrix <-   apply(ad_skill, MARGIN = 1,
      function(y){apply(res_skill, MARGIN = 1,
      function(x){sum(x[x == y], na.rm = TRUE)/sum(y, na.rm = TRUE)})})
  sum(apply(match_matrix, MARGIN = 1, mean))/nrow(ad_skill)
}

make_matrix <- function(njob, nskill, perc){
mat <- matrix(nrow = njob, ncol = nskill)
for(i in 1:njob){
  skill <- round(rnorm(nskill, rnorm(1, perc, .2), .2))
  mat[i,] <- skill
  }
mat
}

###equal size

high_skill_ad <- make_matrix(njob = 100, nskill = 8, perc = .8)
high_skill_res <- make_matrix(njob = 100, nskill = 8, perc = .8)
low_skill_res <- make_matrix(njob = 100, nskill = 8, perc = .3)
mid_skill_res <- make_matrix(njob = 100, nskill = 8, perc = .5)

mismatch_score(high_skill_ad, high_skill_res)
mismatch_score(high_skill_ad, low_skill_res)
mismatch_score(high_skill_ad, mid_skill_res)

hist(mismatch_matrix(high_skill_ad, high_skill_res))
hist(mismatch_matrix(high_skill_ad, low_skill_res))
hist(mismatch_matrix(high_skill_ad, mid_skill_res))

###oversupply of workers

high_skill_ad <- make_matrix(njob = 100, nskill = 8, perc = .8)
high_skill_res <- make_matrix(njob = 200, nskill = 8, perc = .8)
low_skill_res <- make_matrix(njob = 200, nskill = 8, perc = .3)
mid_skill_res <- make_matrix(njob = 200, nskill = 8, perc = .5)

mismatch_score(high_skill_ad, high_skill_res)
mismatch_score(high_skill_ad, low_skill_res)
mismatch_score(high_skill_ad, mid_skill_res)

hist(mismatch_matrix(high_skill_ad, high_skill_res))
hist(mismatch_matrix(high_skill_ad, low_skill_res))
hist(mismatch_matrix(high_skill_ad, mid_skill_res))

##undersupply of workers
high_skill_ad <- make_matrix(njob = 200, nskill = 8, perc = .8)
high_skill_res <- make_matrix(njob = 100, nskill = 8, perc = .8)
low_skill_res <- make_matrix(njob = 100, nskill = 8, perc = .3)
mid_skill_res <- make_matrix(njob = 100, nskill = 8, perc = .5)

mismatch_score(high_skill_ad, high_skill_res)
mismatch_score(high_skill_ad, low_skill_res)
mismatch_score(high_skill_ad, mid_skill_res)

mismatch_matrix(high_skill_ad, high_skill_res)

hist(mismatch_matrix(high_skill_ad, high_skill_res))
hist(mismatch_matrix(high_skill_ad, low_skill_res))
hist(mismatch_matrix(high_skill_ad, mid_skill_res))



###long-form instead of massive match matrix? (which is painfully slow)
#alas, have only succeeded at making it slower.

test_ad <- make_matrix(njob = 500, nskill = 90, perc = .8)
test_res <- make_matrix(njob = 5000, nskill = 90, perc = .4)

rownames(test_ad) <- paste("ad",1:nrow(test_ad),sep="")
rownames(test_res) <- paste("res",1:nrow(test_res),sep="")

mismatch_matrix_long <- function(ad_contingency, res_contingency){
  match <- expand.grid(rownames(ad_contingency), rownames(res_contingency))
  match$score <- apply(match, MARGIN = 1, function(x){
    sum(ad_contingency[x[1],] == 1 & res_contingency[x[2],] == 1, na.rm = FALSE)/sum(ad_contingency[x[1],])
  })
  match
}

mismatch_matrix <- function(ad_skill, res_skill){
  apply(ad_skill, MARGIN = 1, function(y){apply(res_skill, MARGIN = 1,
  function(x){sum(x[x == y], na.rm = TRUE)/sum(y, na.rm = TRUE)})})
}

ltest <- mismatch_matrix_long(test_ad, test_res)

system.time(mismatch_matrix_long(test_ad, test_res))
system.time(mismatch_matrix(test_ad, test_res))
