# Housekeeping -----------------------------------------------------------------

if (!require("tidyverse")) {install.packages("tidyverse"); library("tidyverse")}
if (!require("caret")) {install.packages("caret"); library("caret")}


# Import data ------------------------------------------------------------------

 # THIS PATH NEEDS TO BE CHANGED BEFORE UPLOADING THE REPO!!!
folder = file.path("C:","Users","fabio","Documents","GitHub",
                   "breast-cancer-proteomes","files")

# read files
proteomes <- read_csv(file.path(folder, "77_cancer_proteomes_CPTAC_itraq.csv"))
clinicalData <- read_csv(file.path(folder, "clinical_data_breast_cancer.csv")) 
PAM50proteins <- read_csv(file.path(folder, "PAM50_proteins.csv")) 

rm(folder)

# Explore data -----------------------------------------------------------------

names(proteomes)
names(clinicalData)
names(PAM50proteins)

samples <- 
  proteomes %>% 
  select(-gene_symbol, -gene_name) %>% 
  pivot_longer(cols = -"RefSeq_accession_number",
               names_to = "subject",
               values_to = "expression") %>% 
  pivot_wider(names_from = "RefSeq_accession_number",
              values_from = "expression") %>% 
  mutate(sample = str_sub(subject,1,7))

clinicalData <-
  clinicalData %>% 
  mutate(subject = str_sub(`Complete TCGA ID`,6,12))

samples %>% 
  select(any_of(c("subject", PAM50proteins$RefSeqProteinID))) %>% 
  column_to_rownames("subject") %>% 
  as.matrix() %>% 
  cor()

# proteins without NAs in the set of samples
complete.data <-
  samples %>% 
  select(any_of(c("subject", PAM50proteins$RefSeqProteinID))) %>% 
  column_to_rownames("subject") %>% 
  pivot_longer(cols = everything(),
               names_to = "protein",
               values_to = "expression") %>% 
  group_by(protein) %>% 
  summarise(num.na = sum(is.na(expression)), .groups = 'drop') %>% 
  filter(num.na == 0) %>% 
  .$protein

# samples with only the proteins in the PAM50 set and full data (no NAs)
complete.samples <-
  samples %>% 
  select(any_of(c("subject", complete.data)))

complete.samples %>% 
  pivot_longer(cols = -"subject",
               names_to = "protein",
               values_to = "expression") %>% 
  group_by(protein) %>% 
  summarise(mean = mean(expression), sd = sd(expression)) %>% 
  data.table::as.data.table()

# perform clustering with complete.samples

normalized.samples <-
  complete.samples %>% 
  column_to_rownames("subject") %>% 
  as.matrix()

normalized.samples <- 
  (normalized.samples - colMeans(normalized.samples)) /
  matrixStats::colSds(normalized.samples)

cluster <-
  kmeans(x = select(complete.samples, - subject),
         centers = 4)
