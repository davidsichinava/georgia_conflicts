library(tidyverse)
library(survey)
library(readxl)
library(haven)
library(magrittr)
library(survey)
library(margins)
library(ggeffects)
library(MASS)


d <- read_delim("distance.csv", ",")

cb <- read_dta("http://caucasusbarometer.org/downloads/CB2019_Georgia_response_30Jan2020.dta")

cb <- merge(cb, d, by="PSU")

cb <- cb %>%
  mutate(
    mingrelian = case_when(
      SECNDLAMR == 1 ~ 1,
      SECNDLAMR == 0 ~ 2,
      SECNDLAMR %in% c(-1, -2) ~ 98,
      SECNDLAMR == -5 ~ 97
    ),
    svan = case_when(
      SECNDLASV == 1 ~ 1,
      SECNDLASV == 0 ~ 2,
      SECNDLASV %in% c(-1, -2) ~ 98,
      SECNDLASV == -5 ~ 99      
    ),
    PARTYSUPP = case_when(PARTYSUPO>0~as.numeric(PARTYSUPO),
                          TRUE~as.numeric(PARTYSUPP)),
    PARTYSUPP = case_when(PARTYSUPP %in% c(302, 303, 307, 308) ~ 2,
                          PARTYSUPO %in% c(1, 7, 9) ~ 2,
                          PARTYSUPP == 301 ~ 1,
                          PARTYSUPP %in% c(304, 306, 309) ~ 3,
                          PARTYSUPO %in% c(2, 4, 5) ~ 3,
                          PARTYSUPP %in% c(305, 999) ~ 4,
                          PARTYSUPP == -5 ~ 5,
                          TRUE ~ 6
    ),
    employed = case_when(EMPLSIT==5~1,
                         EMPLSIT==6~1,
                         TRUE ~ 0), #employment var
    wealth = apply(dplyr::select(cb, dplyr::starts_with("OWN")),1, function(x) sum(x,na.rm=T)),
    DISSBAD = if_else(USSRDISS==2,1,0),
    momsed = case_when(RMOEDUC %in% c(6:8)~1,
                       T~0),
    popsed = case_when(RFAEDUC %in% c(6:8)~1,
                       T~0),
    oss_km=DistOsset/1000,
    abk_km=DistAbkh/1000,
    RELCOND=if_else(RELCOND<3,1,0),
    ETHNIC=case_when(ETHNIC %in% c(1, 2, 4, 5, 6, 7) ~ 1,
                     TRUE ~ 2),
    RESPSEX=factor(RESPSEX),
    PARTYSUPP=factor(PARTYSUPP),
    STRATUM=factor(STRATUM),
    EDUYRS = case_when(
      EDUYRS < 0 ~ NA_real_,
      T ~ as.numeric(EDUYRS)
    ),
    ETHNIC=factor(ETHNIC),
    PARTYSUPP=fct_relevel(as.factor(PARTYSUPP), "5")
  )%>%
  mutate_at(c("ABKGNOA", "ABKGHIA", "ABKGCONF", "ABKGIND", "ABKGRUS", "SOGNOA", "SOGHIA", "SOGCONF", "SOGIND", "SOGRUS"), function (x)  replace(x, x < -2, NA))%>%
  mutate_at(c("ABKGNOA", "ABKGHIA", "ABKGCONF", "ABKGIND", "ABKGRUS", "SOGNOA", "SOGHIA", "SOGCONF", "SOGIND", "SOGRUS"), function (x)  replace(x, x==-1, 98))%>%
  mutate_at(c("ABKGNOA", "ABKGHIA", "ABKGCONF", "ABKGIND", "ABKGRUS", "SOGNOA", "SOGHIA", "SOGCONF", "SOGIND", "SOGRUS"), function (x)  replace(x, x==-2, 99))

cb <- cb %>%
  mutate(
    abkh_ind = case_when(
      ABKGIND == 3 | ABKGIND == 2 ~ 1,
      # RESABKH < 1 ~ NA_real_,
      T ~ 0),
    abkh_conf = case_when(
      ABKGCONF == 3 | ABKGCONF == 2 ~ 1,
      # RESABKH < 1 ~ NA_real_,
      T ~ 0),
    abkh_aut = case_when(
      ABKGHIA == 3 | ABKGHIA == 2 ~ 1,
      # RESABKH < 1 ~ NA_real_,
      T ~ 0),
    abkh_non = case_when(
      ABKGNOA == 3 | ABKGNOA == 2 ~ 1,
      # RESABKH < 1 ~ NA_real_,
      T ~ 0),
    oss_ind = case_when(
      SOGIND == 3 | SOGIND == 2 ~ 1,
      # RESRSOC < 1 ~ NA_real_,
      T ~ 0),
    oss_conf = case_when(
      SOGCONF == 3 | SOGCONF == 2 ~ 1,
      # RESRSOC < 1 ~ NA_real_,
      T ~ 0),
    oss_aut = case_when(
      SOGHIA == 3 | SOGHIA == 2 ~ 1,
      # RESRSOC < 1 ~ NA_real_,
      T ~ 0),
    oss_non = case_when(
      SOGNOA == 3 | SOGNOA == 2 ~ 1,
      # RESRSOC < 1 ~ NA_real_,
      T ~ 0),
  )%>%
  mutate(
    ## abkh = abkh_ind + abkh_conf + abkh_aut + abkh_non,
    ## oss = oss_ind + oss_conf + oss_aut + oss_non,
    abkh = case_when(
      abkh_ind == 1 ~ 4,
      abkh_conf == 1 & abkh_ind ==0 ~ 3,
      abkh_aut == 1 & abkh_conf ==0 ~ 2,
      abkh_non == 1 & abkh_aut ==0 ~ 1,
      T ~ NA_real_
    ),
    oss = case_when(
      oss_ind == 1 ~ 4,
      oss_conf == 1 & oss_ind ==0 ~ 3,
      oss_aut == 1 & oss_conf ==0 ~ 2,
      oss_non == 1 & oss_aut ==0 ~ 1,
      T ~ NA_real_
    )
  )

cb_svy <- svydesign(id=~PSU,weights=~INDWT, strat=~SUBSTRATUM, data=cb)

### Visuals

ndi %>%
  filter(item == "Territorial integrity")%>%
  ggplot(aes(wave, value, group=item, color=item))+
  geom_line()+
  geom_point()+
  geom_text(aes(label=paste0(round(value, 0))),
            position = position_dodge(width = 1),
            vjust = -0.5)+
  ylim(0, 100)+
  scale_color_manual(values=c("#084177", "#687466", "#cd8d7b", "#fbc490", "#999999"))+
  guides(color=guide_legend(ncol=2))+
  labs(title="Territorial integrity as the Most Important National Issue (%)",
       subtitle="CRRC/NDI Surveys, 2009-2019")+
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line())


ggplot(historical, aes(wave, value, group=item, color=item))+
  geom_line()+
  geom_point()+
  ylim(0, 100)+
  scale_color_manual(values=c("#084177", "#687466", "#cd8d7b", "#fbc490", "#999999"))+
  guides(color=guide_legend(ncol=2))+
  labs(title="Abkhazia and South Ossetia should be…(%)",
       subtitle="IRI/Baltic Surveys, 2003-2009")+
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title = element_blank())


abkhazia_19 <- svymean(~factor(ABKGNOA), design=cb_svy, na.rm=TRUE)%>%
  data.frame()%>%
bind_rows(as.data.frame(svymean(~factor(ABKGHIA), design=cb_svy, na.rm=TRUE)))%>%
bind_rows(as.data.frame(svymean(~factor(ABKGCONF), design=cb_svy, na.rm=TRUE)))%>%
bind_rows(as.data.frame(svymean(~factor(ABKGIND), design=cb_svy, na.rm=TRUE)))%>%
bind_rows(as.data.frame(svymean(~factor(ABKGRUS), design=cb_svy, na.rm=TRUE)))


#### Models: (a) independence
### Abkhazia independence

abk_ind <- svyglm(abkh_ind ~ RESPSEX+RESPAGE+ETHNIC+STRATUM+EDUYRS+PARTYSUPP+wealth+log(abk_km)+log(oss_km), design = cb_svy)
summary(abk_ind)

abkh_ind_distance <- ggpredict(abk_ind, terms = "abk_km[exp]")
plot(abkh_ind_distance)

abkh_ind_strat <- ggpredict(abk_ind, terms = "STRATUM")
plot(abkh_ind_strat)

abkh_ind_ethn <- ggpredict(abk_ind, terms = "ETHNIC")
plot(abkh_ind_ethn)

#### S. Ossetia independence

oss_ind <- svyglm(oss_ind ~ RESPSEX+RESPAGE+ETHNIC+STRATUM+EDUYRS+PARTYSUPP+wealth+log(abk_km)+log(oss_km), design = cb_svy)
summary(oss_ind)

oss_ind_strat <- ggpredict(oss_ind, terms = "STRATUM")
plot(oss_ind_strat)

oss_ind_sex <- ggpredict(oss_ind, terms = "RESPSEX")
plot(oss_ind_sex)

#### Models: (b) confederacy
### Abkhazia confederacy
abk_conf <- svyglm(abkh_conf ~ RESPSEX+RESPAGE+ETHNIC+STRATUM+EDUYRS+PARTYSUPP+wealth+log(abk_km)+log(oss_km), design = cb_svy)
summary(abk_conf)

abkh_conf_eduyrs <- ggpredict(abk_conf, terms = "EDUYRS[all]")
plot(abkh_conf_eduyrs)

abkh_conf_ethnic <- ggpredict(abk_conf, terms = "ETHNIC")
plot(abkh_conf_ethnic)

abkh_conf_strat <- ggpredict(abk_conf, terms = "STRATUM")
plot(abkh_conf_strat)

abkh_conf_party <- ggpredict(abk_conf, terms = "PARTYSUPP")
plot(abkh_conf_party)

#### S. Ossetia confederacy

oss_conf <- svyglm(oss_conf ~ RESPSEX+RESPAGE+ETHNIC+STRATUM+EDUYRS+PARTYSUPP+wealth+log(abk_km)+log(oss_km), design = cb_svy)
summary(oss_conf)

oss_conf_eduyrs <- ggpredict(oss_conf, terms = "EDUYRS")
plot(oss_conf_eduyrs)

oss_conf_ethnic <- ggpredict(oss_conf, terms = "ETHNIC")
plot(oss_conf_ethnic)

oss_conf_strat <- ggpredict(oss_conf, terms = "STRATUM")
plot(oss_conf_strat)

oss_conf_wealth <- ggpredict(oss_conf, terms = "wealth")
plot(oss_conf_wealth)

#### Models: (c) autonomy
### Abkhazia autonomy
abk_aut <- svyglm(abkh_aut ~ RESPSEX+RESPAGE+ETHNIC+STRATUM+EDUYRS+PARTYSUPP+wealth+log(abk_km)+log(oss_km), design = cb_svy)
summary(abk_aut)

abkh_aut_ethnic <- ggpredict(abk_aut, terms = "ETHNIC")
plot(abkh_aut_ethnic)

abkh_aut_strat <- ggpredict(abk_aut, terms = "STRATUM")
plot(abkh_aut_strat)

abkh_aut_sex <- ggpredict(abk_aut, terms = "RESPSEX")
plot(abkh_aut_sex)

abkh_aut_party <- ggpredict(abk_aut, terms = "PARTYSUPP")
plot(abkh_aut_party)


#### S. Ossetia autonomy

oss_aut <- svyglm(oss_aut ~ RESPSEX+RESPAGE+ETHNIC+STRATUM+EDUYRS+PARTYSUPP+wealth+log(abk_km)+log(oss_km), design = cb_svy)
summary(oss_aut)

oss_aut_ethnic <- ggpredict(oss_aut, terms = "ETHNIC")
plot(oss_aut_ethnic)

oss_aut_strat <- ggpredict(oss_aut, terms = "STRATUM")
plot(oss_aut_strat)

oss_aut_party <- ggpredict(oss_aut, terms = "PARTYSUPP")
plot(oss_aut_party)


#### Models: (d) nothing
### Abkhazia nothing

abk_non <- svyglm(abkh_non ~ RESPSEX+RESPAGE+ETHNIC+STRATUM+EDUYRS+PARTYSUPP+wealth+log(abk_km)+log(oss_km), design = cb_svy)
summary(abk_non)

abkh_non_age <- ggpredict(abk_non, terms = "RESPAGE")
plot(abkh_non_age)

abkh_non_ethnic <- ggpredict(abk_non, terms = "ETHNIC")
plot(abkh_non_ethnic)

abkh_non_strat <- ggpredict(abk_non, terms = "STRATUM")
plot(abkh_non_strat)


oss_non <- svyglm(oss_non ~ RESPSEX+RESPAGE+ETHNIC+STRATUM+EDUYRS+PARTYSUPP+wealth+log(abk_km)+log(oss_km), design = cb_svy)
summary(oss_non)

oss_non_age <- ggpredict(oss_non, terms = "RESPAGE")
plot(oss_non_age)

oss_non_ethnic <- ggpredict(oss_non, terms = "ETHNIC")
plot(oss_non_ethnic)

oss_non_strat <- ggpredict(oss_non, terms = "STRATUM")
plot(oss_non_strat)

oss_non_edu <- ggpredict(oss_non, terms = "EDUYRS")
plot(oss_non_edu)


###
cb$abkh[cb$abkh<1] <- NA
cb$oss[cb$oss<1] <- NA

abk_ord <- polr(factor(abkh) ~ mingrelian+RESPSEX+RESPAGE+ETHNIC+STRATUM+EDUYRS+PARTYSUPP+wealth+log(abk_km)+log(oss_km), data=cb)
summary(abk_ord)

abkh_ord_dist <- ggpredict(abk_ord, terms = "abk_km[exp]")
plot(abkh_ord_dist)

abkh_ord_dist_os <- ggpredict(abk_ord, terms = "oss_km[exp]")
plot(abkh_ord_dist_os)

abkh_ord_stratum <- ggpredict(abk_ord, terms = "STRATUM")
plot(abkh_ord_stratum)

oss_ord <- polr(factor(oss) ~ RESPSEX+RESPAGE+ETHNIC+STRATUM+EDUYRS+PARTYSUPP+wealth+log(abk_km)+log(oss_km), data=cb)
summary(oss_ord)

oss_ord_dist_abk <- ggpredict(oss_ord, terms = "abk_km[exp]")
plot(oss_ord_dist_abk)

oss_ord_dist_os <- ggpredict(oss_ord, terms = "oss_km[exp]")
plot(oss_ord_dist_os)

oss_ord_strat <- ggpredict(oss_ord, terms = "STRATUM")
plot(oss_ord_strat)

oss_ord_age <- ggpredict(oss_ord, terms = "RESPAGE[20, 40, 60]")
plot(oss_ord_age)

oss_ord_party <- ggpredict(oss_ord, terms = "PARTYSUPP")
plot(oss_ord_party)

