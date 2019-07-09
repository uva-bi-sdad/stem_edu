library(DataExplorer)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(stringr)
library(dplyr)



# Gets the files in the folder Alyssa made
files_path <- "../stem_edu/data/stem_edu/working/skills_by_occupation/top5_rb_separate"
all_files <- list.files(files_path, full.names = T)

# Writes the files to seperate things
b_main <- read.csv(all_files[1])
b_skills <- read.csv(all_files[2])
r_main <- read.csv(all_files[3])
r_skills <- read.csv(all_files[4])

# How many are "na"?
b_skills_na <- b_skills$skillcluster == "na"
n = 0
for (i in b_skills_na) {
  if (i) {
    n = n+1
  }
}
print(paste("Blacksburg has a total of",length(b_skills$skill),"skills and of those",n,"are 'na'."))
r_skills_na <- r_skills$skillcluster == "na"
n = 0
for (i in r_skills_na) {
  if (i) {
    n = n+1
  }
}
print(paste("Richmond has a total of",length(r_skills$skill),"skills and of those",n,"are 'na'."))

plot_bar(r_skills$skillclusterfamily)
plot_bar(b_skills)

# Comparing the skill cluster families from the two communities
w = table(r_skills$skillclusterfamily)
x = table(b_skills$skillclusterfamily)

pie(w)
pie(x)

x$Var1 <- as.character(x$Var1)
w$Var1 <- as.character(w$Var1)

# Standardize the result
w <- as.data.frame(w * 100 /length(r_skills$skill))
x <- as.data.frame(x * 100 / length(b_skills$skill))

# Add missing rows to x
x[nrow(x)+1,] = c("Science and Research",0)
x[nrow(x)+1,] = c("Economics, Policy, and Social Studies",0)

w1 <- w[order(w$Var1),]
x1 <- x[order(x$Var1),]
x1$Freq <- as.numeric(x1$Freq)

# Shows the proportional differences in skills for the two MSAs
differences <- data.frame(w1$Var1,abs(w1$Freq - x1$Freq))
colnames(differences)<- c("Skill","Percentage")

# Selects only those that are fairly different.
maindif <- differences[differences$Percentage > 1.3,]
g <- ggplot(maindif, aes(Skill,Percentage))
g + geom_col()


usa <- map("usa")
states <- map_data("state")

ggplot(data = states) +
  geom_polygon(aes(x = long, y = lat, fill = region, group = group), color = "white") +
  coord_fixed(1.3) +
  guides(fill=FALSE)  # do this to leave off the color legend

virginia <- subset(states,region == "virginia")
counties <- map_data("county")
va_county <- subset(counties, region == "virginia")

# plots virginia really nice with color coded onets
ggplot(data = virginia) +
  geom_polygon(aes(x = long, y = lat, group = group), fill = "skyblue", color = "black") +
  coord_fixed(ratio = 1.3) +
  geom_point(data=b_main,aes(x=lon,y=lat,color = factor(onetname)), size = 1, alpha = 0.5) # + ylim(37,39) + xlim(-79,-82)

# And richmond
ggplot(data = virginia) +
  geom_polygon(aes(x = long, y = lat, group = group), fill = "skyblue", color = "black") +
  coord_fixed(ratio = 1.3) +
  geom_point(data=r_main,aes(x=lon,y=lat,color = factor(onetname)), size = 3, alpha = 0.8) # + ylim(37,39) + xlim(-79,-82)

# Plots virginia with county lines
va_base <- ggplot(data = virginia, mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(color = "black", fill = "gray")

