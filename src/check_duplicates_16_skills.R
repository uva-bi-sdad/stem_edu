library(lubridate)

jobs16 <- fread("data/stem_edu/working/job_ads_2016/job_ads_main_2016.csv")
skill16 <- fread("data/stem_edu/working/job_ads_2016/job_ads_skills_2016.csv")

length(unique(jobs16$BGTJobId))

skill_dupes <- skill16 %>% group_by(BGTJobId, Skill, JobDate) %>% summarise(count = n()) %>% filter(count > 1)
unique(month(skill_dupes$JobDate))


zip_files_path <- "../stem_edu/data/stem_edu/original/Burning_Glass_Data/Skill/2016"
zip_files <- list.files(zip_files_path, full.names = T)
jan_file <- zip_files[1]

unzip(jan_file, exdir = zip_files_path)
unzipped_file <- paste0(tools::file_path_sans_ext(jan_file), ".txt")
jan_skill <- fread(unzipped_file)
jan_skill$BGTJobId <- as.character(jan_skill$BGTJobId)
jobs16$BGTJobId <- as.character(jobs16$BGTJobId)
jan_skill <- as.data.table(jan_skill)
jobs16 <- as.data.table(jobs16)
jan_skill_VA <- jan_skill[BGTJobId %chin% jobs16$BGTJobId]

jan_skill_VA <- mutate(jan_skill_VA, month = month(JobDate))
table(jan_skill_VA$month)

jan_VA_dupes <- jan_skill_VA %>% group_by(BGTJobId, SkillClusterFamily, SkillCluster, Skill, JobDate) %>% summarise(count = n()) %>% filter(count > 1)

length(unique(jan_VA_dupes$BGTJobId))
length(unique(jan_skill_VA$BGTJobId))

unlink(unzipped_file)
