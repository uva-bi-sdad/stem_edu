##richmond
loc <- "data/stem_edu/working/burning_glass_ad_combine_16_17"
rich_ad_skill <- fread(file.path(loc, "richmond_top5_stw_jobs_all_skills.csv"))
rich_ad_main <- fread(file.path(loc, "richmond_top5_stw_jobs_main.csv"))

nurse_id <- rich_ad_main[onetname == "Critical Care Nurses"]$bgtjobid
nurse_skill <- rich_ad_skill[bgtjobid %in% nurse_id]

nurse_ad_contingency <- as.data.frame.matrix(table(nurse_skill[,c("bgtjobid", "skill")]))
nurse_ad_contingency[nurse_ad_contingency > 1] <- 1

loc_write <- "data/stem_edu/working/mismatch_working"
write.csv(nurse_ad_contingency, file.path(loc_write, "richmond_nurse_skill_contingency.csv"))

maint_id <- rich_ad_main[onetname == "Maintenance and Repair Workers, General"]$bgtjobid
maint_skill <- rich_ad_skill[bgtjobid %in% maint_id]

maint_ad_contingency <- as.data.frame.matrix(table(maint_skill[,c("bgtjobid", "skill")]))
maint_ad_contingency[maint_ad_contingency > 1] <- 1

write.csv(maint_ad_contingency, file.path(loc_write, "richmond_maintenance_skill_contingency.csv"))

comp_id <- rich_ad_main[onetname == "Computer User Support Specialists"]$bgtjobid
comp_skill <- rich_ad_skill[bgtjobid %in% comp_id]

comp_ad_contingency <- as.data.frame.matrix(table(comp_skill[,c("bgtjobid", "skill")]))
comp_ad_contingency[comp_ad_contingency > 1] <- 1

write.csv(comp_ad_contingency, file.path(loc_write, "richmond_compsupport_skill_contingency.csv"))


##blacksburg

blacks_ad_skill <- fread(file.path(loc, "blacksburg_top5_stw_jobs_all_skills.csv"))
blacks_ad_main <- fread(file.path(loc, "blacksburg_top5_stw_jobs_main.csv"))

nurse_id <- blacks_ad_main[onetname == "Critical Care Nurses"]$bgtjobid
nurse_skill <- blacks_ad_skill[bgtjobid %in% nurse_id]

nurse_ad_contingency <- as.data.frame.matrix(table(nurse_skill[,c("bgtjobid", "skill")]))
nurse_ad_contingency[nurse_ad_contingency > 1] <- 1

loc_write <- "data/stem_edu/working/mismatch_working"
write.csv(nurse_ad_contingency, file.path(loc_write, "blacksburg_nurse_skill_contingency.csv"))

maint_id <- blacks_ad_main[onetname == "Maintenance and Repair Workers, General"]$bgtjobid
maint_skill <- blacks_ad_skill[bgtjobid %in% maint_id]

maint_ad_contingency <- as.data.frame.matrix(table(maint_skill[,c("bgtjobid", "skill")]))
maint_ad_contingency[maint_ad_contingency > 1] <- 1

write.csv(maint_ad_contingency, file.path(loc_write, "blacksburg_maintenance_skill_contingency.csv"))

comp_id <- blacks_ad_main[onetname == "Computer User Support Specialists"]$bgtjobid
comp_skill <- blacks_ad_skill[bgtjobid %in% comp_id]

comp_ad_contingency <- as.data.frame.matrix(table(comp_skill[,c("bgtjobid", "skill")]))
comp_ad_contingency[comp_ad_contingency > 1] <- 1

write.csv(comp_ad_contingency, file.path(loc_write, "blacksburg_compsupport_skill_contingency.csv"))
