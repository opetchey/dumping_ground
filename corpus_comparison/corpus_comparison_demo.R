## install the following if required
library(bibliometrix)
library(tidyverse)
library(circlize)
library(wordcloud2)
library(cowplot)

readFiles("data/from_zotero.bib") %>%
  isibib2df() %>%
  dplyr::mutate(DOI=stringr::str_trim(DI)) %>%
  filter(!is.na(DOI)) %>%
  pull(DOI) %>%
  paste(collapse=" OR ") %>%
  cat(file="data/dois_for_WoS.txt")

focal_corpus <- readFiles("data/focal_corpus_from_WoS.bib") %>%
  isibib2df() %>%
  mutate(TC=as.numeric(TC),
         Corpus="URPPGCB",
         AUx=gsub(";","",AU),
         num_authors=nchar(AU) - nchar(AUx))

D1 <- readFiles("data/ref-corpus1.bib") %>%
  isibib2df()
D2 <- readFiles("data/ref-corpus2.bib") %>%
  isibib2df()
D3 <- readFiles("data/ref-corpus2.bib") %>%
  isibib2df()
D4 <- readFiles("data/ref-corpus2.bib") %>%
  isibib2df()
D5 <- readFiles("data/ref-corpus2.bib") %>%
  isibib2df()
reference_corpus <- rbind(D1, D2, D3, D4, D5) %>%
  mutate(TC=as.numeric(TC),
         Corpus="Reference",
         AUx=gsub(";","",AU),
         num_authors=nchar(AU) - nchar(AUx))
rm(D1, D2, D3, D4, D5)

focal_corpus_biban <- biblioAnalysis(focal_corpus)
NetMatrix <- biblioNetwork(focal_corpus, analysis = "collaboration",  network = "authors", sep = ";")
MM <- as(NetMatrix, "matrix")
MM <- MM[order(rownames(MM)), order(colnames(MM))]
diag(MM) <- 0
top_authors <- focal_corpus_biban$Authors[1:20]
these_cols <- colnames(MM) %in% names(top_authors)
these_rows <- rownames(MM) %in% names(top_authors)
top_MM <- MM[these_rows,these_cols]
set.seed(999)
chordDiagram(top_MM, symmetric = TRUE)

reference_corpus <- select(reference_corpus, intersect(names(reference_corpus), names(focal_corpus)))
focal_corpus <- select(focal_corpus, intersect(names(reference_corpus), names(focal_corpus)))
corpus <- rbind(focal_corpus, reference_corpus)

summ_stats <- group_by(corpus, Corpus) %>%
  summarise(mean_cites=mean(TC),
            sem_cites=sd(TC)/sqrt(n()),
            mean_num_authors=mean(num_authors),
            sem_num_authors=sd(num_authors)/sqrt(n()))
p1 <- ggplot(summ_stats) +
  geom_point(mapping=aes(x=Corpus, y=mean_cites)) +
  geom_errorbar(mapping=aes(x=Corpus,
                            ymin=mean_cites-sem_cites,
                            ymax=mean_cites+sem_cites),
                width=0.1) +
  ylab("Number of times cited")
p2 <- ggplot(summ_stats) +
  geom_point(mapping=aes(x=Corpus, y=mean_num_authors)) +
  geom_errorbar(mapping=aes(x=Corpus,
                            ymin=mean_num_authors-sem_num_authors,
                            ymax=mean_num_authors+sem_num_authors),
                width=0.1) +
  ylab("Number of authors")
p0 <- plot_grid(p2, p1)
ggsave(filename="corpus_comparison.jpg", plot=p0, width = 6, height = 4)
