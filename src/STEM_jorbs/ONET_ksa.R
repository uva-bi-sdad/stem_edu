# Get KSA data for each profession

knowledge = read.table("Knowledge.txt", header = T, sep = "\t")
skills = read.table("Skills.txt", header = T, sep = "\t")
abilities = read.table("Abilities.txt", header = T, sep = "\t")

knowledge %>% transmute(O.NET.SOC.Code, Element.Name, Scale.ID = paste(Scale.ID, "knowledge", sep = "_"), Data.Value) %>%
  cast(O.NET.SOC.Code ~ Scale.ID + Element.Name) -> k1
skills %>% transmute(O.NET.SOC.Code, Element.Name, Scale.ID = paste(Scale.ID, "skills", sep = "_"), Data.Value) %>%
  cast(O.NET.SOC.Code ~ Scale.ID + Element.Name) -> s1
abilities %>% transmute(O.NET.SOC.Code, Element.Name, Scale.ID = paste(Scale.ID, "abilities", sep = "_"), Data.Value) %>%
  cast(O.NET.SOC.Code ~ Scale.ID + Element.Name) -> a1

ksa = merge(merge(k1, s1), a1)

ksaPca = cmdscale(dist(scale(ksa[,-1])))


## Extract the 6 stem knowledge fields for all professions: Biology, Chemistry, Physics, Computers and Electronics, Engineering and Technology, mathematics

knowledge %>% filter(Element.Name %in% c("Biology", "Chemistry", "Physics", "Computers and Electronics", "Engineering and Technology", "Mathematics")) %>%
  filter(Scale.ID == "IM")%>%
  transmute(O.NET.SOC.Code, Element.Name, Data.Value) -> stemKnowledgeLong
#write.csv(stemKnowledgeLong, "stem knowledge importance.csv")

stemKnowledgeWide = cast(stemKnowledgeLong, O.NET.SOC.Code ~ Element.Name)
rownames(stemKnowledgeWide) = stemKnowledgeWide[,1]
stemKnowledgeWide = stemKnowledgeWide[,-1]

hist(scale(rowSums(stemKnowledgeWide)))
centers = sort(kmeans(scale(rowSums(stemKnowledgeWide)), 3)$centers)
abline(v = centers[-3] + diff(centers)/2)

allKnowledge = filter(knowledge, Scale.ID == "IM")


