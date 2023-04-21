pacman::p_load(tidyverse, metafor)

#####Functions for plotting#####

#Inputs:
#
# d: data prepared for plotting, data subsetted by factor, each row representing a level of the factor, and perc represent de proportion
#
# pal: desired colour palette in the plot
#
# title: desired plot title
#
# legend: desired plot legend
#
# width: desired bar width
#

customized_barplot<- function(d, pal, title, legend, width=0.35, rate=TRUE){
  
  plt <- ggplot(data=d, aes(x=NA, y=perc, fill=Item)) +
    geom_bar(colour = 'black', stat="identity", width = width) +
    scale_x_discrete(breaks = NULL) +
    scale_fill_manual(values=pal,
                      guide = guide_legend(reverse=T))
  if (rate == TRUE){  
  
    plt<- plt + scale_y_continuous(labels=scales::percent) 
    
  }
    
    else {
      
    plt <- plt + scale_y_continuous(limits = c(0, sum(d$n)))  
    }
    plt <- plt + theme_minimal(base_size = 10) +
    theme(legend.position='bottom', 
          legend.direction='horizontal',
          legend.text = element_text(size = 13),
          legend.key.size = unit(.25,"cm"),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.title.x=element_blank(),
          plot.title = element_text(size = 15, face = "bold")) 
  
  if (rate == TRUE) {  
    plt<-plt +geom_text(aes(label=sprintf ("%i%%", round(perc*100, 0))), 
              size = 4, 
              position = position_stack(vjust = 0.5),
              fontface = "bold") 
    }
  
  else {
    plt<-plt +geom_text(aes(label=paste0(perc)), 
                       size = 4, 
                       position = position_stack(vjust = 0.5),
                       fontface = "bold") 
  }
  
   plt<- plt +labs(fill=legend) +
    ggtitle(sprintf("      %s", title)) +
    coord_flip(clip = 'off') +
    geom_text(
      x = 0.5,
      y = sum(d$perc),
      inherit.aes = FALSE,
      label = sprintf("N = %i", sum(d$n)),
      check_overlap = TRUE,
      hjust = 1,
      fontface = 'bold',
      size = 3.5
    )
  return(plt)
}

#Inputs:
#
# d: data prepared for plotting
#
# x: x-axis variable
#
# y: y-axis variable
#
# shape: shape variable
#
# shape_name: name of the shape variable
#
# shape_labels: levels of the shape variable
#
# size: size of the shape
#
# title: desired plot title
#

scatter_match<- function(d, x, y, shape, shape_name, shape_labels, size, title, size_constant = T){ 
  if (size_constant == TRUE) {
    
  plt<-ggplot(d, aes(x=x, y=y, shape = shape)) +
  geom_point(aes(col = shape), stroke = 1, size = size)  
  } 

else {
  
  plt<-ggplot(d, aes(x=x, y=y, shape = shape, size = size)) +
    geom_point(aes(col = shape), stroke = 1)  
  
}

plt<- plt +  geom_abline(intercept = 0, slope = 1, colour = "#000000", size = 0.65) + 
  scale_shape_manual(name = shape_name, labels = shape_labels, values = c(17, 4)) +
  scale_size_continuous(range = c(1, 6), guide = "none") + 
  scale_color_manual(name = shape_name, labels = shape_labels, values = c("red", "black"))+
  theme_classic(base_size = 10) + 
  theme(legend.text = element_text(size = 13),
        legend.key.size = unit(.5,"cm"),
        legend.title = element_text(size=14, face="bold"),
        plot.title = element_text(size = 13, face = "bold", hjust = .5),
        panel.background = element_rect(colour = "black", size = 1),
        axis.title = element_text(size=14,face="bold"),
        axis.text = element_text(size = 12)) +
  labs(title= title,
       y= "Reproduced value", 
       x= "Original value"
  )

}

scatter_match2<- function(d, x, y, color, color_name, color_labels, size, title)
  
  ggplot(d, aes(x=x, y=y, color = color, size = size)) +
  geom_point(stroke = 1, size = size, shape = 4) + 
  geom_abline(intercept = 0, slope = 1, colour = "#000000", size = 0.65) + 
#  scale_shape_manual(name = shape_name, labels = shape_labels, values = c(18, 8), guide = "none") +
  scale_color_manual(name = color_name, labels = color_labels, values = c("#3444d9", "#b00b13"))+
  scale_size_continuous(range = c(1, 6), guide = "none")+
  theme_classic(base_size = 10) + 
  theme(legend.text = element_text(size = 13),
        legend.key.size = unit(.5,"cm"),
        legend.title = element_text(size=14, face="bold"),
        plot.title = element_text(size = 13, face = "bold", hjust = .5),
        panel.background = element_rect(colour = "black", size = 1),
        axis.title=element_text(size=14,face="bold"),
        axis.text = element_text(size = 12)) +
  labs(title= title,
       y= "Reproduced value", 
       x= "Original value"
  )





#####Function to tidy primary data#####

# Inputs:
#
# dat: collected primary data of one paper (There could be more than one independet meta-analysis dataset)
# type: type of primary data collected
#       #gs_ci= effect sizes an their confindence bouns; #gs_sei= effect sizes and their standard errors; #means_smd: statistics to compute effect sizes
#       #raw_means= statitics to compute raw means differences
# n_mas: number of independent meta-analyses for which data are in the datafile. 
#
# Outputs
#
# mas_dat: a list with one tidy dataset for each independent meta-analysis
# 

ind_ma<-function(dat, type=c("gs_ci","gs_sei", "means_smd", "raw_means"), n_mas) {
  
  #creating the list
  ma_dat<- list()
  
  if (type=="gs_ci") {
    
    if (n_mas==1) {

      
      ma_dat[[1]]<-  dat %>%    
        dplyr::rename(yi=2) %>% 
        dplyr::rename(ll=3) %>% 
        dplyr::rename(ul=4) %>% 
        #Computing standard error using the 95% confidence bounds
        mutate(sei=(ul-ll) / (2*qnorm(0.975)))
    }
    
    else {
      #Cheking that the datafile contains all the needed data
      if (n_mas+1==ncol(dat)/3) {
        
        for (i in 1:n_mas) {
          
          ma_dat[[i]]<- dat %>% 
            dplyr::select(ID, ends_with(paste0("_",i)), ni, nc) %>% 
            drop_na(ends_with(paste(i)))%>%
            dplyr::rename(yi=2) %>% 
            dplyr::rename(ll=3) %>% 
            dplyr::rename(ul=4) %>% 
            mutate(sei=(ul-ll) / (2*qnorm(0.975)))
          
        }
      }
      else {
        
        stop("'n_mas' and cols of data don't match")
      }
      
    }
    
  }
  
  
  else if (type=="gs_sei")  {
    
    if (n_mas==1) {
      
      ma_dat[[1]]<- dat %>% 
        dplyr::rename(yi=2) %>% 
        dplyr::rename(sei=3)
    }
    
    else {
      #Cheking that the datafile contains all the needed data
      if (n_mas==(ncol(dat)-3)/2) {
        
        for (i in 1:n_mas) {
          
          ma_dat[[i]]<- dat %>% 
            dplyr::select(ID, ends_with(paste(i)), ni, nc) %>% 
            drop_na(ends_with(paste(i)))%>%
            dplyr::rename(yi=2) %>% 
            dplyr::rename(sei=3)
          
        }
      }
      else {
        
        stop("'n_mas' and cols of data don't match")
      }
      
    }
    
  }
  
  
  else if (type=="means_smd"){
    
    
    if (n_mas==1){
      
      ma_dat[[1]]<-dat %>% 
        dplyr::rename(mi=2, mc=5, sdi=3, sdc=6, ni=4, nc=7) %>% 
      escalc(measure = "SMD", m1i=mi, m2i=mc, sd1i = sdi, sd2i = sdc, n1i = ni, n2i = nc, data = .)%>% 
        #more computations of d will be added if necessary
        mutate(sei=sqrt(vi)) %>% 
        select(ID, yi, sei, 4, 7)
      
    }
    
    else {
      #Cheking that the datafile contains all the needed data
      if (n_mas==(ncol(dat)-1)/6) {
        
        for (i in 1:n_mas) {
          
          ma_dat[[i]]<- dat %>% 
            dplyr::select(ID, ends_with(paste(i))) %>% 
            drop_na()%>% 
            dplyr::rename(mi=2, mc=5, sdi=3, sdc=6, ni=4, nc=7) %>% 
            escalc(measure = "SMD", m1i=mi, m2i=mc, sd1i = sdi, sd2i = sdc, n1i = ni, n2i = nc, data = .) %>% 
            mutate(sei=sqrt(vi)) %>% 
            select(ID, yi, sei, 4, 7)
        }
      }
      else {
        
        stop("'n_mas' and cols of data don't match")
      }
      
    }
    
  }
  
  else if (type=="raw_means"){
    
    
    if (n_mas==1){
      
      ma_dat[[1]]<-dat %>% 
        dplyr::rename(mi=2, mc=5, sdi=3, sdc=6, ni=4, nc=7) %>% 
        escalc(measure = "MD", m1i=mi, m2i=mc, sd1i = sdi, sd2i = sdc, n1i = ni, n2i = nc, data = .)%>% 
        #more computations of d will be added if necessary
        mutate(sei=sqrt(vi)) %>% 
        select(ID, yi, sei, 4, 7)
      
    }
    
    else {
      #Cheking that the datafile contains all the needed data
      if (n_mas==(ncol(dat)-1)/6) {
        
        for (i in 1:n_mas) {
          
          ma_dat[[i]]<- dat %>% 
            dplyr::select(ID, ends_with(paste(i))) %>% 
            drop_na()%>% 
            dplyr::rename(mi=2, mc=5, sdi=3, sdc=6, ni=4, nc=7) %>% 
            escalc(measure = "MD", m1i=mi, m2i=mc, sd1i = sdi, sd2i = sdc, n1i = ni, n2i = nc, data = .) %>% 
            mutate(sei=sqrt(vi)) %>% 
            select(ID, yi, sei, 4, 7)
        }
      }
      else {
        
        stop("'n_mas' and cols of data don't match")
      }
      
    }
    
  }
  
  
  else {
    
    stop("'type' argument is not valid")
  }
  
  return(ma_dat)
  
}



