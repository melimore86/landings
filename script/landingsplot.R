
library("cowplot")
library("devtools")
library("ggplot2")
library("ggpubr")
library("grid")
library("gridExtra")
library("lattice")
library("marelac")
library("scales")
library("ggpubr")
library("tidyverse")




landings <- read.csv("data/landings.csv", header= T)

max<- subset(landings, landings$Year<2013)



stland12<-
  ggplot(data=max, aes(x=Year, y=State_landings/100000))+
  geom_line(size=2)+
  labs(x= "Year", y= "State Landings (thousands)") +
  scale_y_continuous(limits=c(0,35))+
  scale_x_continuous(limits=c(1986,2017), breaks=c(1986, 1988, 1990, 1992, 1994, 1996, 1998, 2000,2002,  2004, 2006, 2008,  2010, 2012,2014,2016)) +
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"),
        plot.title =element_text(size=20, face='bold'),
        axis.text.x = element_text(angle = 90, hjust = 1),
        aspect.ratio = 0.70)

stland17<-
  ggplot(data=landings, aes(x=Year, y=State_landings/100000))+
  geom_line(size=2)+
  labs(x= "Year", y= "State Landings (thousands)") +
  scale_y_continuous(limits=c(0,35))+
  scale_x_continuous(limits=c(1986,2017), breaks=c(1986, 1988, 1990, 1992, 1994, 1996, 1998, 2000,2002,  2004, 2006, 2008,  2010, 2012,2014,2016)) +
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"),
        plot.title =element_text(size=20, face='bold'),
        axis.text.x = element_text(angle = 90, hjust = 1),
        aspect.ratio = 0.70)


stlandred<-
  ggplot()+
  geom_line(data=landings, aes(x=Year, y=State_landings/100000),size=2, color="red")+
  geom_line(data=max, aes(x=Year, y=State_landings/100000),size=2)+
  labs(x= "Year", y= "State Landings (thousands)") +
  scale_y_continuous(limits=c(0,35))+
  scale_x_continuous(limits=c(1986,2017), breaks=c(1986, 1988, 1990, 1992, 1994, 1996, 1998, 2000,2002,  2004, 2006, 2008,  2010, 2012,2014,2016)) +
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"),
        plot.title =element_text(size=20, face='bold'),
        axis.text.x = element_text(angle = 90, hjust = 1),
        aspect.ratio = 0.70)


sttrip12<-
  ggplot(data=max, aes(x=Year, y=State_trips/1000))+
  geom_line(size=2)+
  labs(x= "Year", y= "State Trips (thousands)")+
    scale_y_continuous(limits=c(0,110))+
  scale_x_continuous(limits=c(1986,2017), breaks=c(1986, 1988, 1990, 1992, 1994, 1996, 1998, 2000,2002,  2004, 2006, 2008,  2010, 2012,2014,2016)) +
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"),
        plot.title =element_text(size=20, face='bold'),
        axis.text.x = element_text(angle = 90, hjust = 1),
        aspect.ratio = 0.70)


sttrip17<-
  ggplot(data=landings, aes(x=Year, y=State_trips/1000))+
  geom_line(size=2)+
  labs(x= "Year", y= "State Trips (thousands)")+
  scale_y_continuous(limits=c(0,110))+
  scale_x_continuous(limits=c(1986,2017), breaks=c(1986, 1988, 1990, 1992, 1994, 1996, 1998, 2000,2002,  2004, 2006, 2008,  2010, 2012,2014,2016)) +
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"),
        plot.title =element_text(size=20, face='bold'),
        axis.text.x = element_text(angle = 90, hjust = 1),
        aspect.ratio = 0.70)

sttripred<-
  ggplot()+
  geom_line(data=landings, aes(x=Year, y=State_trips/1000),size=2,color="red")+
  geom_line(data=max, aes(x=Year, y=State_trips/1000),size=2)+
  labs(x= "Year", y= "State Trips (thousands)")+
  scale_y_continuous(limits=c(0,110))+
  scale_x_continuous(limits=c(1986,2017), breaks=c(1986, 1988, 1990, 1992, 1994, 1996, 1998, 2000,2002,  2004, 2006, 2008,  2010, 2012,2014,2016)) +
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"),
        plot.title =element_text(size=20, face='bold'),
        axis.text.x = element_text(angle = 90, hjust = 1),
        aspect.ratio = 0.70)


stpertrip12<-
  ggplot(data=max, aes(x=Year, y=State_per_trip))+
  geom_line(size=2)+
  labs(x= "Year", y= "State Per Trip")+
    scale_y_continuous(limits=c(0,165)) +
  scale_x_continuous(limits=c(1986,2017), breaks=c(1986, 1988, 1990, 1992, 1994, 1996, 1998, 2000,2002,  2004, 2006, 2008,  2010, 2012,2014,2016)) +
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"),
        plot.title =element_text(size=20, face='bold'),
        axis.text.x = element_text(angle = 90, hjust = 1),
        aspect.ratio = 0.70)


stpertrip17<-
  ggplot(data=landings, aes(x=Year, y=State_per_trip))+
  geom_line(size=2)+
  labs(x= "Year", y= "State Per Trip")+
  scale_y_continuous(limits=c(0,165)) +
  scale_x_continuous(limits=c(1986,2017), breaks=c(1986, 1988, 1990, 1992, 1994, 1996, 1998, 2000,2002,  2004, 2006, 2008,  2010, 2012,2014,2016)) +
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"),
        plot.title =element_text(size=20, face='bold'),
        axis.text.x = element_text(angle = 90, hjust = 1),
        aspect.ratio = 0.70)


stpertripred<-
  ggplot()+
  geom_line(data=landings, aes(x=Year, y=State_per_trip),size=2, color="red")+
  geom_line(data=max, aes(x=Year, y=State_per_trip),size=2)+
  labs(x= "Year", y= "State Per Trip")+
  scale_y_continuous(limits=c(0,165)) +
  scale_x_continuous(limits=c(1986,2017), breaks=c(1986, 1988, 1990, 1992, 1994, 1996, 1998, 2000,2002,  2004, 2006, 2008,  2010, 2012,2014,2016)) +
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"),
        plot.title =element_text(size=20, face='bold'),
        axis.text.x = element_text(angle = 90, hjust = 1),
        aspect.ratio = 0.70)



apstland12<-
  ggplot(data=max, aes(x=Year, y=Apalach_landings/100000)) +
  geom_line(size=2) +
  labs(x= "Year", y= "Apalachicola Landings(thousands)") +
    scale_y_continuous(limits=c(0,35))+
  scale_x_continuous(limits=c(1986,2017), breaks=c(1986, 1988, 1990, 1992, 1994, 1996, 1998, 2000,2002,  2004, 2006, 2008,  2010, 2012,2014,2016)) +
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"),
        plot.title =element_text(size=20, face='bold'),
        axis.text.x = element_text(angle = 90, hjust = 1),
        aspect.ratio = 0.70)

apstland17<-
  ggplot(data=landings, aes(x=Year, y=Apalach_landings/100000)) +
  geom_line(size=2) +
  labs(x= "Year", y= "Apalachicola Landings(thousands)") +
  scale_y_continuous(limits=c(0,35))+
  scale_x_continuous(limits=c(1986,2017), breaks=c(1986, 1988, 1990, 1992, 1994, 1996, 1998, 2000,2002,  2004, 2006, 2008,  2010, 2012,2014,2016)) +
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"),
        plot.title =element_text(size=20, face='bold'),
        axis.text.x = element_text(angle = 90, hjust = 1),
        aspect.ratio = 0.70)


apstlandred<-
  ggplot() +
  geom_line(data=landings, aes(x=Year, y=Apalach_landings/100000),size=2, color="red") +
  geom_line(data=max, aes(x=Year, y=Apalach_landings/100000),size=2) +
  labs(x= "Year", y= "Apalachicola Landings(thousands)") +
  scale_y_continuous(limits=c(0,35))+
  scale_x_continuous(limits=c(1986,2017), breaks=c(1986, 1988, 1990, 1992, 1994, 1996, 1998, 2000,2002,  2004, 2006, 2008,  2010, 2012,2014,2016)) +
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"),
        plot.title =element_text(size=20, face='bold'),
        axis.text.x = element_text(angle = 90, hjust = 1),
        aspect.ratio = 0.70)



apsttrip12<-
  ggplot(data=max, aes(x=Year, y=Apalach_trips/1000))+
  geom_line(size=2)+
  labs(x= "Year", y= "Apalachicola Trips (thousands)")+
  scale_y_continuous(limits=c(0,110))+
  scale_x_continuous(limits=c(1986,2017), breaks=c(1986, 1988, 1990, 1992, 1994, 1996, 1998, 2000,2002,  2004, 2006, 2008,  2010, 2012,2014,2016))+
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"),
        plot.title =element_text(size=20, face='bold'),
        axis.text.x = element_text(angle = 90, hjust = 1),
        aspect.ratio = 0.70)

apsttrip17<-
  ggplot(data=landings, aes(x=Year, y=Apalach_trips/1000))+
  geom_line(size=2)+
  labs(x= "Year", y= "Apalachicola Trips (thousands)")+
  scale_y_continuous(limits=c(0,110))+
  scale_x_continuous(limits=c(1986,2017), breaks=c(1986, 1988, 1990, 1992, 1994, 1996, 1998, 2000,2002,  2004, 2006, 2008,  2010, 2012,2014,2016))+
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"),
        plot.title =element_text(size=20, face='bold'),
        axis.text.x = element_text(angle = 90, hjust = 1),
        aspect.ratio = 0.70)


apsttripred<-
  ggplot()+
  geom_line(data=landings, aes(x=Year, y=Apalach_trips/1000),size=2, color="red")+
  geom_line(data=max, aes(x=Year, y=Apalach_trips/1000),size=2)+
  labs(x= "Year", y= "Apalachicola Trips (thousands)")+
  scale_y_continuous(limits=c(0,110))+
  scale_x_continuous(limits=c(1986,2017), breaks=c(1986, 1988, 1990, 1992, 1994, 1996, 1998, 2000,2002,  2004, 2006, 2008,  2010, 2012,2014,2016))+
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"),
        plot.title =element_text(size=20, face='bold'),
        axis.text.x = element_text(angle = 90, hjust = 1),
        aspect.ratio = 0.70)



apstpertrip12<-
  ggplot(data=max, aes(x=Year, y=Apalach_per_trip))+
  geom_line(size=2)+
  labs(x= "Year", y= "Apalachicola Per Trip")+
  scale_y_continuous(limits=c(0,165)) +
  scale_x_continuous(limits=c(1986,2017), breaks=c(1986, 1988, 1990, 1992, 1994, 1996, 1998, 2000,2002,  2004, 2006, 2008,  2010, 2012,2014,2016)) +
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"),
        plot.title =element_text(size=20, face='bold'),
        axis.text.x = element_text(angle = 90, hjust = 1),
        aspect.ratio = 0.70)

apstpertrip17<-
  ggplot(data=landings, aes(x=Year, y=Apalach_per_trip))+
  geom_line(size=2)+
  labs(x= "Year", y= "Apalachicola Per Trip")+
  scale_y_continuous(limits=c(0,165)) +
  scale_x_continuous(limits=c(1986,2017), breaks=c(1986, 1988, 1990, 1992, 1994, 1996, 1998, 2000,2002,  2004, 2006, 2008,  2010, 2012,2014,2016)) +
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"),
        plot.title =element_text(size=20, face='bold'),
        axis.text.x = element_text(angle = 90, hjust = 1),
        aspect.ratio = 0.70)


apstpertripred<-
  ggplot()+
  geom_line(data=landings, aes(x=Year, y=Apalach_per_trip),size=2, color="red")+
  geom_line(data=max, aes(x=Year, y=Apalach_per_trip),size=2)+
  labs(x= "Year", y= "Apalachicola Per Trip")+
  scale_y_continuous(limits=c(0,165)) +
  scale_x_continuous(limits=c(1986,2017), breaks=c(1986, 1988, 1990, 1992, 1994, 1996, 1998, 2000,2002,  2004, 2006, 2008,  2010, 2012,2014,2016)) +
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"),
        plot.title =element_text(size=20, face='bold'),
        axis.text.x = element_text(angle = 90, hjust = 1),
        aspect.ratio = 0.70)


swstland12<-
  ggplot(data=max, aes(x=Year, y=Suw_landings/1000))+
  geom_line(size=2)+
  labs(x= "Year", y= "Suwannee Sound Landings (thousands)")+
    scale_y_continuous(limits=c(0,35))+
  scale_x_continuous(limits=c(1986,2017), breaks=c(1986, 1988, 1990, 1992, 1994, 1996, 1998, 2000,2002,  2004, 2006, 2008,  2010, 2012,2014,2016)) +
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"),
        plot.title =element_text(size=20, face='bold'),
        axis.text.x = element_text(angle = 90, hjust = 1),
        aspect.ratio = 0.70)



swstland17<-
  ggplot(data=landings, aes(x=Year, y=Suw_landings/1000))+
  geom_line(size=2)+
  labs(x= "Year", y= "Suwannee Sound Landings (thousands)")+
  scale_y_continuous(limits=c(0,35))+
  scale_x_continuous(limits=c(1986,2017), breaks=c(1986, 1988, 1990, 1992, 1994, 1996, 1998, 2000,2002,  2004, 2006, 2008,  2010, 2012,2014,2016)) +
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"),
        plot.title =element_text(size=20, face='bold'),
        axis.text.x = element_text(angle = 90, hjust = 1),
        aspect.ratio = 0.70)



swstlandred<-
  ggplot()+
  geom_line(data=landings, aes(x=Year, y=Suw_landings/1000),size=2, color="red")+
  geom_line(data=max, aes(x=Year, y=Suw_landings/1000),size=2)+
  labs(x= "Year", y= "Suwannee Sound Landings (thousands)")+
  scale_y_continuous(limits=c(0,35))+
  scale_x_continuous(limits=c(1986,2017), breaks=c(1986, 1988, 1990, 1992, 1994, 1996, 1998, 2000,2002,  2004, 2006, 2008,  2010, 2012,2014,2016)) +
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"),
        plot.title =element_text(size=20, face='bold'),
        axis.text.x = element_text(angle = 90, hjust = 1),
        aspect.ratio = 0.70)



swsttrip12<-
  ggplot(data=max, aes(x=Year, y=Suw_trips/10000))+
  geom_line(size=2)+
  labs(x= "Year", y= "Suwannee Sound Trips (thousands)") +
    scale_y_continuous(limits=c(0,110))+
  scale_x_continuous(limits=c(1986,2017), breaks=c(1986, 1988, 1990, 1992, 1994, 1996, 1998, 2000,2002,  2004, 2006, 2008,  2010, 2012,2014,2016)) +
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"),
        plot.title =element_text(size=20, face='bold'),
        axis.text.x = element_text(angle = 90, hjust = 1),
        aspect.ratio = 0.70)


swsttrip17<-
  ggplot(data=landings, aes(x=Year, y=Suw_trips/10000))+
  geom_line(size=2)+
  labs(x= "Year", y= "Suwannee Sound Trips (thousands)") +
  scale_y_continuous(limits=c(0,110))+
  scale_x_continuous(limits=c(1986,2017), breaks=c(1986, 1988, 1990, 1992, 1994, 1996, 1998, 2000,2002,  2004, 2006, 2008,  2010, 2012,2014,2016)) +
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"),
        plot.title =element_text(size=20, face='bold'),
        axis.text.x = element_text(angle = 90, hjust = 1),
        aspect.ratio = 0.70)


swsttripred<-
  ggplot()+
  geom_line(data=landings, aes(x=Year, y=Suw_trips/10000),size=2, color="red")+
  geom_line(data=max, aes(x=Year, y=Suw_trips/10000),size=2)+
  labs(x= "Year", y= "Suwannee Sound Trips (thousands)") +
  scale_y_continuous(limits=c(0,110))+
  scale_x_continuous(limits=c(1986,2017), breaks=c(1986, 1988, 1990, 1992, 1994, 1996, 1998, 2000,2002,  2004, 2006, 2008,  2010, 2012,2014,2016)) +
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"),
        plot.title =element_text(size=20, face='bold'),
        axis.text.x = element_text(angle = 90, hjust = 1),
        aspect.ratio = 0.70)


swstpertrip12<-
  ggplot(data=max, aes(x=Year, y=Suw_per_trip))+
  geom_line(size=2)+
  labs(x= "Year", y= "Suwannee Sound Per Trip")+
  scale_y_continuous(limits=c(0,165))+
  scale_x_continuous(limits=c(1986,2017), breaks=c(1986, 1988, 1990, 1992, 1994, 1996, 1998, 2000,2002,  2004, 2006, 2008,  2010, 2012,2014,2016)) +
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"),
        plot.title =element_text(size=20, face='bold'),
        axis.text.x = element_text(angle = 90, hjust = 1),
        aspect.ratio = 0.70)


swstpertrip17<-
  ggplot(data=landings, aes(x=Year, y=Suw_per_trip))+
  geom_line(size=2)+
  labs(x= "Year", y= "Suwannee Sound Per Trip")+
  scale_y_continuous(limits=c(0,165))+
  scale_x_continuous(limits=c(1986,2017), breaks=c(1986, 1988, 1990, 1992, 1994, 1996, 1998, 2000,2002,  2004, 2006, 2008,  2010, 2012,2014,2016)) +
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"),
        plot.title =element_text(size=20, face='bold'),
        axis.text.x = element_text(angle = 90, hjust = 1),
        aspect.ratio = 0.70)


swstpertripred<-
  ggplot()+
  geom_line(data=landings, aes(x=Year, y=Suw_per_trip),size=2, color="red")+
  geom_line(data=max, aes(x=Year, y=Suw_per_trip),size=2)+
  labs(x= "Year", y= "Suwannee Sound Per Trip")+
  scale_y_continuous(limits=c(0,165))+
  scale_x_continuous(limits=c(1986,2017), breaks=c(1986, 1988, 1990, 1992, 1994, 1996, 1998, 2000,2002,  2004, 2006, 2008,  2010, 2012,2014,2016)) +
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.text=element_text(size=15),
        axis.title=element_text(size=15,face="bold"),
        plot.title =element_text(size=20, face='bold'),
        axis.text.x = element_text(angle = 90, hjust = 1),
        aspect.ratio = 0.70)



aplandings12<-
  ggplot(data=max, aes(x=Year, y=Apalach_landings/1000))+
  geom_line(size=2)+
  labs(x= "Year", y= "Apalachicola Landings(thousands)")+
  scale_x_continuous(limits=c(1986,2017), breaks=c(1986, 1988, 1990, 1992, 1994, 1996, 1998, 2000,2002,  2004, 2006, 2008,  2010, 2012,2014,2016)) +
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.text.x = element_text(angle = 90, hjust = 1),
        aspect.ratio = 0.70)


aplandings17<-
  ggplot(data=landings, aes(x=Year, y=Apalach_landings/1000))+
  geom_line(size=2)+
  labs(x= "Year", y= "Apalachicola Landings (thousands)")+
  scale_x_continuous(limits=c(1986,2017), breaks=c(1986, 1988, 1990, 1992, 1994, 1996, 1998, 2000,2002,  2004, 2006, 2008,  2010, 2012,2014,2016)) +
  theme(panel.border = element_rect(color = "black", size = 0.5, fill = NA, linetype="solid"),
        axis.text.x = element_text(angle = 90, hjust = 1),
        aspect.ratio = 0.70)



ggsave("aplandings12.tiff", units="in", width=10, height=10, dpi=300, compression = 'lzw')
ggsave("aplandings17.tiff", units="in", width=10, height=10, dpi=300, compression = 'lzw')




alllandslines12<-
ggdraw() +
  draw_plot(swstland12, x=0.34, y=0, width=0.3, height=0.30 ) +
  draw_plot(swsttrip12, x=0.34, y=0.30, width=0.3, height=0.30 ) +
  draw_plot(swstpertrip12, x=0.34, y=0.60, width=0.3, height=0.30 ) +
  draw_plot(stland12, x=0, y=0, width=0.3, height=0.30 ) +
  draw_plot(sttrip12, x=0, y=0.3, width=0.3, height=0.30 ) +
  draw_plot(stpertrip12, x=0, y=0.6, width=0.3, height=0.30 ) +
  draw_plot(apstland12, x=0.67, y=0, width=0.3, height=0.30 ) +
  draw_plot(apsttrip12, x=0.67, y=0.3, width=0.3, height=0.30 ) +
  draw_plot(apstpertrip12, x=0.67, y=0.6, width=0.3, height=0.30 )

setwd("C:/Users/Mel/Desktop")
ggsave("alllandslines12.tiff", units="in", width=25, height=25, dpi=300, compression = 'lzw')



alllandslines17<-
  ggdraw() +
  draw_plot(swstland12, x=0.34, y=0, width=0.3, height=0.30 ) +
  draw_plot(swsttrip17, x=0.34, y=0.30, width=0.3, height=0.30 ) +
  draw_plot(swstpertrip17, x=0.34, y=0.60, width=0.3, height=0.30 ) +
  draw_plot(stland17, x=0, y=0, width=0.3, height=0.30 ) +
  draw_plot(sttrip17, x=0, y=0.3, width=0.3, height=0.30 ) +
  draw_plot(stpertrip17, x=0, y=0.6, width=0.3, height=0.30 ) +
  draw_plot(apstland17, x=0.67, y=0, width=0.3, height=0.30 ) +
  draw_plot(apsttrip17, x=0.67, y=0.3, width=0.3, height=0.30 ) +
  draw_plot(apstpertrip17, x=0.67, y=0.6, width=0.3, height=0.30 )

ggsave("alllandslines17.tiff", units="in", width=25, height=25, dpi=300, compression = 'lzw')



alllandslinesred<-
  ggdraw() +
  draw_plot(swstlandred, x=0.34, y=0, width=0.3, height=0.30 ) +
  draw_plot(swsttripred, x=0.34, y=0.30, width=0.3, height=0.30 ) +
  draw_plot(swstpertripred, x=0.34, y=0.60, width=0.3, height=0.30 ) +
  draw_plot(stlandred, x=0, y=0, width=0.3, height=0.30 ) +
  draw_plot(sttripred, x=0, y=0.3, width=0.3, height=0.30 ) +
  draw_plot(stpertripred, x=0, y=0.6, width=0.3, height=0.30 ) +
  draw_plot(apstlandred, x=0.67, y=0, width=0.3, height=0.30 ) +
  draw_plot(apsttripred, x=0.67, y=0.3, width=0.3, height=0.30 ) +
  draw_plot(apstpertripred, x=0.67, y=0.6, width=0.3, height=0.30 )

ggsave("alllandslinesred.tiff", units="in", width=25, height=25, dpi=300, compression = 'lzw')



stlandings12<-
ggdraw() +
  draw_plot(stland12, x=0.2, y=0.05, width=0.50, height=0.28 ) +
  draw_plot(sttrip12, x=0.2, y=0.35, width=0.50, height=0.28 ) +
  draw_plot(stpertrip12, x=0.2, y=0.65, width=0.50, height=0.28 ) 
setwd("C:/Users/Mel/Desktop")
ggsave("stlandings12.tiff", units="in", width=25, height=25, dpi=300, compression = 'lzw')


stlandings17<-
  ggdraw() +
  draw_plot(stland17, x=0.2, y=0.05, width=0.50, height=0.28 ) +
  draw_plot(sttrip17, x=0.2, y=0.35, width=0.50, height=0.28 ) +
  draw_plot(stpertrip17, x=0.2, y=0.65, width=0.50, height=0.28 ) 
setwd("C:/Users/Mel/Desktop")
ggsave("stlandings17.tiff", units="in", width=25, height=25, dpi=300, compression = 'lzw')



stlandingsred<-
  ggdraw() +
  draw_plot(stlandred, x=0.2, y=0.05, width=0.50, height=0.28 ) +
  draw_plot(sttripred, x=0.2, y=0.35, width=0.50, height=0.28 ) +
  draw_plot(stpertripred, x=0.2, y=0.65, width=0.50, height=0.28 ) 
setwd("C:/Users/Mel/Desktop")
ggsave("stlandingsred.tiff", units="in", width=25, height=25, dpi=300, compression = 'lzw')


ap12<-
  ggdraw() +
  draw_plot(apstland12, x=0.2, y=0.05, width=0.50, height=0.28 ) +
  draw_plot(apsttrip12, x=0.2, y=0.35, width=0.50, height=0.28 ) +
  draw_plot(apstpertrip12, x=0.2, y=0.65, width=0.50, height=0.28 ) 
setwd("C:/Users/Mel/Desktop")
ggsave("ap12.tiff", units="in", width=25, height=25, dpi=300, compression = 'lzw')



ap17<-
  ggdraw() +
  draw_plot(apstland17, x=0.2, y=0.05, width=0.50, height=0.28 ) +
  draw_plot(apsttrip17, x=0.2, y=0.35, width=0.50, height=0.28 ) +
  draw_plot(apstpertrip17, x=0.2, y=0.65, width=0.50, height=0.28 ) 
setwd("C:/Users/Mel/Desktop")
ggsave("ap17.tiff", units="in", width=25, height=25, dpi=300, compression = 'lzw')


apred<-
  ggdraw() +
  draw_plot(apstlandred, x=0.2, y=0.05, width=0.50, height=0.28 ) +
  draw_plot(apsttripred, x=0.2, y=0.35, width=0.50, height=0.28 ) +
  draw_plot(apstpertripred, x=0.2, y=0.65, width=0.50, height=0.28 ) 
setwd("C:/Users/Mel/Desktop")
ggsave("apred.tiff", units="in", width=25, height=25, dpi=300, compression = 'lzw')




suw12<-
  ggdraw() +
  draw_plot(swstland12, x=0.2, y=0.05, width=0.50, height=0.28 ) +
  draw_plot(swsttrip12, x=0.2, y=0.35, width=0.50, height=0.28 ) +
  draw_plot(swstpertrip12, x=0.2, y=0.65, width=0.50, height=0.28 ) 
setwd("C:/Users/Mel/Desktop")
ggsave("suw12.tiff", units="in", width=25, height=25, dpi=300, compression = 'lzw')

suw17<-
  ggdraw() +
  draw_plot(swstland17, x=0.2, y=0.05, width=0.50, height=0.28 ) +
  draw_plot(swsttrip17, x=0.2, y=0.35, width=0.50, height=0.28 ) +
  draw_plot(swstpertrip17, x=0.2, y=0.65, width=0.50, height=0.28 ) 
setwd("C:/Users/Mel/Desktop")
ggsave("suw17.tiff", units="in", width=25, height=25, dpi=300, compression = 'lzw')


suwred<-
  ggdraw() +
  draw_plot(swstlandred, x=0.2, y=0.05, width=0.50, height=0.28 ) +
  draw_plot(swsttripred, x=0.2, y=0.35, width=0.50, height=0.28 ) +
  draw_plot(swstpertripred, x=0.2, y=0.65, width=0.50, height=0.28 ) 
setwd("C:/Users/Mel/Desktop")
ggsave("suwred.tiff", units="in", width=25, height=25, dpi=300, compression = 'lzw')


