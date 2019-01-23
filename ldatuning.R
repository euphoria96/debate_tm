# install.packages("ldatuning")
# install.packages("devtools")
# devtools::install_github("nikita-moor/ldatuning")

library("ldatuning")

library("topicmodels")

result <- FindTopicsNumber(
  cpsdtm,
  topics = seq(from = 2, to = 15, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
)
FindTopicsNumber_plot(result)

dtm2 = Dtm[1:10,]
result <- FindTopicsNumber(
  dtm2,
  topics = seq(from = 2, to = 15, by = 1),
  # metrics = c("text"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE
)
FindTopicsNumber_plot(result)


