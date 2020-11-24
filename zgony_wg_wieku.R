library("ggplot2")
library("dplyr")
library("scales")
library("ggthemes")
library("ggpubr")
library("tidyr")

picWd <- 12
spanV <- 0.5
surl <- "© NI-KW (source: http://www.wsse.gda.pl/)"
mainColor <- "deeppink"
loessColor <- "steelblue"
mainBreaks <- "1 month"
NIKW <- "© NI-KW @ github.com/knsm-psw/GUS_mortality | https://stat.gov.pl/obszary-tematyczne/ludnosc/ludnosc/zgony-wedlug-tygodni,39,2.html"

z <- read.csv("PL-mortality-2015.csv", sep = ';',  header=T, na.string="NA" )
lastO <- last(z$date)
lastT <- last(z$week)

nuts <- c('PL21', 'PL22', 'PL41', 'PL42', 'PL43', 'PL51', 'PL52', 'PL61', 'PL62', 'PL63', 'PL71', 'PL72', 'PL81', 'PL82', 'PL84', 'PL91', 'PL92')

### Ogółem
z00 <- z %>% filter ( sex == 'O'  & geo == 'PL' ) %>% as.data.frame

z0 <- z00 %>% filter ( year >= 2015  & year < 2020 ) %>% as.data.frame
z1 <- z00 %>% filter ( year == 2020 ) %>% as.data.frame

## średnie w okresie 1 -- (n-1)
zz0 <- z0 %>% group_by(age,week) %>% summarise( year = 't19', vv = mean(value, na.rm=TRUE)) %>% as.data.frame
zz1 <- z1 %>% group_by(age,week) %>% summarise( year = 't20', vv = mean(value, na.rm=TRUE)) %>% as.data.frame
### Połącz
zz1 <- bind_rows(zz0, zz1)

farbe19 <- '#F8766D'
farbe20 <- '#00BFC4'

p1 <- ggplot(zz1, aes(x=week, y=vv, color=year)) +
 geom_smooth(method="loess", se=F, span=spanV, size=.4) +
 geom_point(size=.4, alpha=.5) +
 facet_wrap( ~age, scales = "free_y") +
 xlab(label="") +
 ylab(label="") +
 ##theme_nikw()+
 theme(plot.subtitle=element_text(size=9), legend.position="top")+
 scale_color_manual(name="Rok: ", labels = c("średnia 2015--2019", "2020"), values = c("t19"=farbe19, "t20"=farbe20 )) +
 ggtitle("Zgony wg grup wiekowych (PL/Ogółem)", subtitle=sprintf("%s | ostatni tydzień: %s", NIKW, lastO) )

ggsave(plot=p1, "zgony_PL_by_age_O.png", width=picWd)

### M ###
z00 <- z %>% filter ( sex == 'M'  & geo == 'PL' ) %>% as.data.frame

z0 <- z00 %>% filter ( year >= 2015  & year < 2020 ) %>% as.data.frame
z1 <- z00 %>% filter ( year == 2020 ) %>% as.data.frame

## średnie w okresie 1 -- (n-1)
zz0 <- z0 %>% group_by(age,week) %>% summarise( year = 't19', vv = mean(value, na.rm=TRUE)) %>% as.data.frame
zz1 <- z1 %>% group_by(age,week) %>% summarise( year = 't20', vv = mean(value, na.rm=TRUE)) %>% as.data.frame
### Połącz
zz1 <- bind_rows(zz0, zz1)

p2 <- ggplot(zz1, aes(x=week, y=vv, group=year, color=year)) +
 geom_smooth(method="loess", se=F, span=spanV, size=.4) +
 geom_point(size=.4, alpha=.5) +
 facet_wrap( ~age, scales = "free_y") +
 xlab(label="") +
 ylab(label="") +
 ##theme_nikw()+
 ##labs(caption=source) +
 theme(plot.subtitle=element_text(size=9), legend.position="top")+
 scale_color_manual(name="Rok: ", labels = c("średnia 2015--2019", "2020"), values = c("t19"=farbe19, "t20"=farbe20 )) +
 ggtitle("Zgony wg grup wiekowych (PL/Mężczyźni)", subtitle=sprintf("%s | ostatni tydzień: %s", NIKW, lastO) )

ggsave(plot=p2, "zgony_PL_by_age_M.png", width=picWd)


### K #########################################
z00 <- z %>% filter ( sex == 'K'  & geo == 'PL' ) %>% as.data.frame

z0 <- z00 %>% filter ( year >= 2015  & year < 2020 ) %>% as.data.frame
z1 <- z00 %>% filter ( year == 2020 ) %>% as.data.frame

## średnie w okresie 1 -- (n-1)
zz0 <- z0 %>% group_by(age,week) %>% summarise( year = 't19', vv = mean(value, na.rm=TRUE)) %>% as.data.frame
zz1 <- z1 %>% group_by(age,week) %>% summarise( year = 't20', vv = mean(value, na.rm=TRUE)) %>% as.data.frame
### Połącz
zz1 <- bind_rows(zz0, zz1)

p3 <- ggplot(zz1, aes(x=week, y=vv, group=year, color=year)) +
 geom_smooth(method="loess", se=F, span=spanV, size=.4) +
 geom_point(size=.4, alpha=.5) +
 facet_wrap( ~age, scales = "free_y") +
 xlab(label="") +
 ylab(label="") +
 ##theme_nikw()+
 ##labs(caption=source) +
 theme(plot.subtitle=element_text(size=9), legend.position="top")+
 scale_color_manual(name="Rok: ", labels = c("średnia 2015--2019", "2020"), values = c("t19"=farbe19, "t20"=farbe20 )) +
 ggtitle("Zgony wg grup wiekowych (PL/Kobiety)", subtitle=sprintf("%s | ostatni tydzień: %s", NIKW, lastO) )

ggsave(plot=p3, "zgony_PL_by_age_K.png", width= picWd)

###############################################################
### ogółem wg województw #####################################
n <- read.csv("nuts.csv", sep = ';',  header=T, na.string="NA" )
## dodaj nazwy
z <- left_join(z, n, by='geo')

## wiek razem
z00 <- z %>% filter ( sex == 'O'  & geo %in% nuts & age == 'OGÓŁEM') %>% as.data.frame

z0 <- z00 %>% filter ( year >= 2015  & year < 2020 ) %>% as.data.frame
z1 <- z00 %>% filter ( year == 2020 ) %>% as.data.frame

## średnie w okresie 1 -- (n-1)
zz0 <- z0 %>% group_by(name,week) %>% summarise( year = 't19', vv = mean(value, na.rm=TRUE)) %>% as.data.frame
zz1 <- z1 %>% group_by(name,week) %>% summarise( year = 't20', vv = mean(value, na.rm=TRUE)) %>% as.data.frame
### Połącz
zz1 <- bind_rows(zz0, zz1)

lastWeek <- last(zz1$week)
firstWeek <- lastWeek - 6

zz1 <- zz1 %>% filter ( week >= firstWeek  ) %>% as.data.frame
print(zz1)

p4 <- ggplot(zz1, aes(x=week, y=vv, group=year, color=year)) +
 geom_smooth(method="loess", se=F, span=spanV, size=.4) +
 geom_point(size=.4, alpha=.5) +
 facet_wrap( ~name, scales = "free_y") +
 xlab(label="") +
 ylab(label="") +
 ##theme_nikw()+
 ##labs(caption=source) +
 theme(plot.subtitle=element_text(size=9), legend.position="top")+
 scale_color_manual(name="Rok: ", labels = c("średnia 2015--2019", "2020"), values = c("t19"=farbe19, "t20"=farbe20 )) +
 ggtitle("Zgony wg województw* (PL/Ogółem)", 
   subtitle=sprintf("*wg klasyfikacji NUTS stąd mazowieckie/stołeczne | %s | ostatni tydzień: %s", NIKW, lastO))

ggsave(plot=p4, "zgony_PL_by_woj_O.png", width=picWd)

## jako %% w średniej w poprzednich 5 lat

zz1 <- zz1 %>% spread(year, vv)

str(zz1)

zz1$yy <- zz1$t20 / zz1$t19 * 100

p5 <- ggplot(zz1, aes(x=week, y=yy), color=farbe20) +
 geom_smooth(method="loess", se=F, span=spanV, size=.4, color=farbe20) +
 geom_point(size=.4, alpha=.5) +
 facet_wrap( ~name, scales = "fixed") +
 xlab(label="nr tygodnia") +
 ylab(label="%") +
 theme(plot.subtitle=element_text(size=9), legend.position="top")+
 scale_color_manual(name="Rok 2020: ", labels = c("% 2020/(średnia 2015--2015)"), values = c("yy"=farbe20 )  ) +
 ggtitle("Zgony wg województw* (PL/Ogółem)", 
   subtitle=sprintf("*wg klasyfikacji NUTS stąd mazowieckie/stołeczne | %s | ostatni tydzień: %s", NIKW, lastO))

ggsave(plot=p5, "zgony_PL_by_woj_P.png", width=picWd)

zz1 <- zz1 %>% filter ( week >= firstWeek  ) %>% as.data.frame

p6 <- ggplot(zz1, aes(x=week, y=yy), color=farbe20) +
 geom_smooth(method="loess", se=F, span=spanV, size=.4, color=farbe20) +
 geom_point(size=.4, alpha=.5) +
 geom_text(aes(label=sprintf("%.1f", yy)), vjust=-1.25, size=1.5) +
 facet_wrap( ~name, scales = "fixed") +
 xlab(label="nr tygodnia") +
 ylab(label="%") +
 theme(plot.subtitle=element_text(size=9), legend.position="top")+
 scale_color_manual(name="Rok 2020: ", labels = c("% 2020/(średnia 2015--2015)"), values = c("yy"=farbe20 )  ) +
 ggtitle(sprintf("Zgony wg województw* (PL/Ogółem) tygodnie: %i--%i (%i tydzień zaczyna się %s)", firstWeek, lastWeek, lastWeek, lastO), 
   subtitle=sprintf("*wg klasyfikacji NUTS stąd mazowieckie/stołeczne | %s", NIKW))

ggsave(plot=p6, "zgony_PL_by_woj_P6.png", width=picWd)

##
#over60 <- c ('60-64', '65-69', '70-74', '75-79', '80-84', '85-89', '90-99')
over60 <- c ('70-74', '75-79', '80-84', '85-89', '90-99')
over50 <- c ('50-54', '55-59', '60-64', '65-69', '70-74', '75-79', '80-84', '85-89', '90-99')

z6 <- z %>% filter ( sex == 'O'  & geo %in% nuts & age %in% over60) %>% as.data.frame
z5 <- z %>% filter ( sex == 'O'  & geo %in% nuts & age %in% over50) %>% as.data.frame

z60 <- z6 %>% filter ( year >= 2015  & year < 2020 ) %>% as.data.frame
z61 <- z6 %>% filter ( year == 2020 ) %>% as.data.frame
zz60 <- z60 %>% group_by(name,week) %>% summarise( year = 't19', age='60', vv = mean(value, na.rm=TRUE)) %>% as.data.frame
zz61 <- z61 %>% group_by(name,week) %>% summarise( year = 't20', age='60', vv = mean(value, na.rm=TRUE)) %>% as.data.frame


z50 <- z5 %>% filter ( year >= 2015  & year < 2020 ) %>% as.data.frame
z51 <- z5 %>% filter ( year == 2020 ) %>% as.data.frame
zz50 <- z50 %>% group_by(name,week) %>% summarise( year = 't19', age ='50', vv = mean(value, na.rm=TRUE)) %>% as.data.frame
zz51 <- z51 %>% group_by(name,week) %>% summarise( year = 't20', age ='50', vv = mean(value, na.rm=TRUE)) %>% as.data.frame

zz61 <- bind_rows(zz60, zz61, zz50, zz51)
zz61 <- zz61 %>% spread(year, vv)

str(zz61)

zz61$yy <- zz61$t20 / zz61$t19 * 100

zz61 <- zz61 %>% filter ( week >= firstWeek  ) %>% as.data.frame

p7 <- ggplot(zz61, aes(x=week, y=yy, color=age)) +
 geom_smooth(method="loess", se=F, span=spanV, size=.4) +
 geom_point(size=.4, alpha=.5) +
 geom_text(aes(label=sprintf("%.1f", yy)), vjust=-1.25, size=1.5) +
 facet_wrap( ~name, scales = "fixed") +
 xlab(label="nr tygodnia") +
 ylab(label="%") +
 theme(plot.subtitle=element_text(size=9), legend.position="top")+
 #scale_color_manual(name="Rok 2020: ", labels = c("% 2020/(średnia 2015--2015)"), values = c("yy"=farbe20 )  ) +
 scale_color_manual(name="Wiek: ", labels = c("50 i więcej", "70 i więcej"), values = c("50"=farbe19, "60"=farbe20 )) +
 ggtitle(sprintf("Zgony w 2020 (jako %% średniej z lat 2019--2015) tygodnie: %i--%i (%i tydzień zaczyna się %s)",
        firstWeek, lastWeek, lastWeek, lastO), 
   subtitle=sprintf("*wg klasyfikacji NUTS stąd mazowieckie/stołeczne | %s", NIKW))

ggsave(plot=p7, "zgony_PL_by_woj_age_P6.png", width=picWd)
