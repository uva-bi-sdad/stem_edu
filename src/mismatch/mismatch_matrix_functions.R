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
