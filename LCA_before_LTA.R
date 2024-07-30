library(dplyr)
lca_W1 <- select(lca, HurtOthers_w1, FreqGroup_w1, H1FS3, H1FS6, H1FS16)
lca_W2 <- select(lca, FreqGroup_w2, HurtOthers_w2, H2FS3, H2FS6, H2FS16)
lca_W3 <- select(lca, FreqGroup_w3, HurtOthers_w3, H3SP6, H3SP9, H3SP12)
glimpse(lca_W1)

#########Latent class analysis##############
library(poLCA)
set.seed(1234)
#########Transfrom to numeric############
factor_vars <- c("HurtOthers_w1", "FreqGroup_w1", "H1FS3", "H1FS6", "H1FS16")
lca_W1 <- lca_W1 %>%
  mutate_at(factor_vars, ~ as.numeric(as.character(.)))
glimpse(lca_W1)
lca_W1 <- lca_W1 + 1

factor_vars <- c("HurtOthers_w2", "FreqGroup_w2", "H2FS3", "H2FS6", "H2FS16")
lca_W2 <- lca_W2 %>%
  mutate_at(factor_vars, ~ as.numeric(as.character(.)))
glimpse(lca_W2)
lca_W2 <- lca_W2 + 1

factor_vars <- c("HurtOthers_w3", "FreqGroup_w3", "H3SP6", "H3SP9", "H3SP12")
lca_W3 <- lca_W3 %>%
  mutate_at(factor_vars, ~ as.numeric(as.character(.)))
glimpse(lca_W3)
lca_W3 <- lca_W3 + 1

# start lca in w1, class = 2
lca_W1_2 <- poLCA(formula = cbind(FreqGroup_w1, HurtOthers_w1, H1FS3, H1FS6,
                                      H1FS16)~1, data = lca_W1, nclass = 2, maxiter = 1000, nrep = 5, graph = TRUE)
lca_W1_3 <- poLCA(formula = cbind(FreqGroup_w1, HurtOthers_w1, H1FS3, H1FS6,
                                      H1FS16)~1, data = lca_W1, nclass = 3, maxiter = 1000, nrep = 5, graph = TRUE)
lca_W1_3 <- poLCA(formula = cbind(FreqGroup_w1, HurtOthers_w1, H1FS3, H1FS6,
                                  H1FS16)~1, data = lca_W1, nclass = 4, maxiter = 1000, nrep = 5, graph = TRUE)

lca_W2_2 <- poLCA(formula = cbind(FreqGroup_w2, HurtOthers_w2, H2FS3, H2FS6,
                                  H2FS16)~1, data = lca_W2, nclass = 2, maxiter = 1000, nrep = 5, graph = TRUE)
lca_W2_3 <- poLCA(formula = cbind(FreqGroup_w2, HurtOthers_w2, H2FS3, H2FS6,
                                  H2FS16)~1, data = lca_W2, nclass = 3, maxiter = 1000, nrep = 5, graph = TRUE)
lca_W2_3 <- poLCA(formula = cbind(FreqGroup_w2, HurtOthers_w2, H2FS3, H2FS6,
                                  H2FS16)~1, data = lca_W2, nclass = 4, maxiter = 1000, nrep = 5, graph = TRUE)

lca_W3_2 <- poLCA(formula = cbind(FreqGroup_w3, HurtOthers_w3, H3SP6, H3SP9,
                                  H3SP12)~1, data = lca_W3, nclass = 2, maxiter = 1000, nrep = 5, graph = TRUE)
lca_W3_3 <- poLCA(formula = cbind(FreqGroup_w3, HurtOthers_w3, H3SP6, H3SP9,
                                  H3SP12)~1, data = lca_W3, nclass = 3, maxiter = 1000, nrep = 5, graph = TRUE)
lca_W3_3 <- poLCA(formula = cbind(FreqGroup_w3, HurtOthers_w3, H3SP6, H3SP9,
                                  H3SP12)~1, data = lca_W3, nclass = 4, maxiter = 1000, nrep = 5, graph = TRUE)
