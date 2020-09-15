library(ggpubr)
#library(ggsn)
library(reshape2)
library(rJava)
library(xlsxjars)
library(ggplot2)
library(plotly)
library(tidyverse)
library(dplyr)
#library(RMySQL)
library(xlsx)
#library(ragtop)
library(lubridate)
library(gridExtra)
options(java.parameters = "-Xmx4g")
#library(XLConnect)
#library(epitools)
library(raster)
library(lattice)
# install.packages('staplr', dependencies = TRUE)
#devtools::install_github("pridiltal/staplr")
library(staplr)
library(stringr)
library(stringi)
library(sjmisc)
#require(devtools)
#devtools::install_github("Displayr/flipTime")
library(flipTime)
library(ggrepel)
#library(openxlsx)


# --- Import data ---
add_dataset <- function(filename) {
  read.xlsx2(
    filename,
    sheetIndex = 1,
    skipEmptyRows = TRUE,
    skipEmptyCols = TRUE,
    startRow = 3,
    header = TRUE,
    na.strings = TRUE
  )
}
import_all_datasets <- function() {
  master_dataset <- data.frame()
  
  file_list <- list.files('datasets/')
  for (i in file_list) {
    filename <- paste('datasets/', i, sep = '')
    ds <- add_dataset(filename)
    master_dataset <- rbind(master_dataset, ds)
  }
  return(master_dataset)
}

# --- Modify Data ---
averaged_data <- function(df) {
counter = 1
subset_df <-
  df %>% dplyr::select(
    #Reporting.starts,
    Video.engagement.rate.75.,
    Post.engagement.rate,
    audience_name,
    Account.name,
    Ad.set.name,
    Platform,
    reported_date,
    Cost.per.1.000.people.reached,
    Audience_type
  )
anova_results <- data.frame(
  platform = character(10000),
  audience_type=character(10000),
  city = character(10000),
  cost_per_1000=double(10000),
  #Reporting.starts =character(10000),
  reported_date = character(10000),
  audience = character(10000),
  audience_name=character(10000),
  post_mean = double(10000),
  video_mean = double(10000),
  post_size = integer(10000),
  video_size = integer(10000),
  post_sd = double(10000),
  video_sd = double(10000),
  stringsAsFactors = FALSE
)
platforms <- unique(subset_df$Platform)
for (m in platforms) {
  dataset_platform <- subset_df %>% filter(Platform == m)
  if (nrow(dataset_platform) > 0) {
    cities <- unique(subset_df$Account.name)
    for (i in cities) {
      dataset_city <- dataset_platform %>% filter(Account.name == i)
      if (nrow(dataset_city) > 0) {
        months_list <- unique(dataset_city$reported_date)
        for (j in months_list) {
          dataset_month <- dataset_city %>% filter(reported_date == j)
          audiences_list <- unique(dataset_month$Ad.set.name)
          if (nrow(dataset_month) > 0) {
            for (k in audiences_list) {
              dataset_audience <- dataset_month %>% filter(Ad.set.name == k)
              dataset_audience[dataset_audience == ''] <- NA
              if (nrow(dataset_audience) == 0) {
                #do nothing
              }
              else {
                print(counter)
                anova_results$cost_per_1000<-as.numeric(dataset_audience$Cost.per.1.000.people.reached[counter])
                anova_results$city[counter] <- i
                anova_results$audience[counter] <- k
                anova_results$audience_type[counter] <- dataset_audience$Audience_type[counter]
                anova_results$audience_name[counter] <- dataset_audience$audience_name[counter]
                anova_results$platform[counter] <- m
                anova_results$reported_date[counter] <- j
                #anova_results$Reporting.starts[counter] <- dataset_audience$Reporting.starts[counter]
                dataset_audience$Post.engagement.rate <-
                  as.numeric(dataset_audience$Post.engagement.rate)
                dataset_audience$Video.engagement.rate.75. <-
                  as.numeric(dataset_audience$Video.engagement.rate.75.)
                anova_results$post_size[counter] <-
                  length(na.omit(dataset_audience$Post.engagement.rate))
                anova_results$video_size[counter] <-
                  length(na.omit(dataset_audience$Video.engagement.rate))
                if (as.numeric(length(
                  na.omit(dataset_audience$Post.engagement.rate)
                )) > 1) {
                  #print('yes')
                  anova_results[counter, 'post_mean'] <-
                    mean(as.numeric(na.omit(dataset_audience[, 'Post.engagement.rate'])))
                  anova_results[counter, 'post_sd'] <-
                    sd(as.numeric(na.omit(dataset_audience[, 'Post.engagement.rate'])))
                }
                else if (as.numeric(length(
                  na.omit(dataset_audience$Post.engagement.rate)
                )) == 1) {
                  #print('yes')
                  anova_results[counter, 'post_mean'] <-
                    as.numeric(na.omit(dataset_audience[, 'Post.engagement.rate']))
                }
                else {
                  #print('no')
                  anova_results[counter, 'post_mean'] <- NA
                }
                if (as.numeric(length(
                  na.omit(dataset_audience$Video.engagement.rate)
                )) > 1) {
                  anova_results$video_mean[counter] <-
                    mean(as.numeric(
                      na.omit(dataset_audience$Video.engagement.rate)
                    ))
                  anova_results$video_sd[counter] <-
                    sd(as.numeric(
                      na.omit(dataset_audience$Video.engagement.rate)
                    ))
                } else if (as.numeric(length(
                  na.omit(dataset_audience$Video.engagement.rate)
                )) == 1) {
                  anova_results$video_mean[counter] <-
                    as.numeric(na.omit(dataset_audience$Video.engagement.rate))
                }
                else {
                  anova_results$video_mean[counter] <- NA
                }
                counter = counter + 1
              }
            }
          }
        }
      }
    }
  }
}
print(counter)
anova_results <- anova_results[1:counter,]
anova_results <- anova_results[-(anova_results$post_size == 0 & anova_results$video_size == 0),]
#anova_results <- anova_results[anova_results$platform !='',]
anova_results <- anova_results[anova_results$platform !='',]
return(anova_results)
}
#results <- averaged_data(revised_filtered)
#Populate dataset
populate_dataframe<-function() {
  #new_dataframe<-entire_dataset[FALSE,]
  new_dataframe<-data.frame()
  canadian_cities<-read.csv2('canadian_cities_list.csv',header= FALSE, sep='')
  cities_list<-canadian_cities[,1]
  cities_list<-sample(cities_list,15,replace=FALSE)
  platform_list<-c('instagram','facebook', 'youtube','twitter')
  audiences_list<-c('15-25','25-35','35-45','45-55','seniors')
  months_list<-c('January-2020','February-2020','March-2020',
                 'April-2020','May-2020','June-2020')
  counter<-1
  #generate datapoints
  #while(counter < 2000){
    for (c in cities_list){
      for (p in platform_list){
        for (a in audiences_list){
          for (m in months_list) {
            print(counter)
            new_dataframe[counter,'medium']<-'post'
            new_dataframe[counter,'city']<-c
            new_dataframe[counter,'audience']<-a
            new_dataframe[counter,'platform']<-p
            new_dataframe[counter,'reporting_month']<-m
            counter=counter+1
        
            
            if (counter%%3==0){
              new_dataframe[counter,'medium']<-'video'
              new_dataframe[counter,'city']<-c
              new_dataframe[counter,'audience']<-a
              new_dataframe[counter,'platform']<-p
              new_dataframe[counter,'reporting_month']<-m
              counter=counter+1
            }
            if (counter%%5==0 & p=='instagram'){
              new_dataframe[counter,'medium']<-'post'
              new_dataframe[counter,'city']<-c
              new_dataframe[counter,'audience']<-a
              new_dataframe[counter,'platform']<-p
              new_dataframe[counter,'reporting_month']<-m
            }
            if (counter%%9==0 & p=='tik tok'){
              new_dataframe[counter,'medium']<-'post'
              new_dataframe[counter,'city']<-c
              new_dataframe[counter,'audience']<-a
              new_dataframe[counter,'platform']<-p
              new_dataframe[counter,'reporting_month']<-m
              counter=counter+1
            }

           
        } 
      }
      } 
    }
  #}
  #random numbers
  n<-nrow(new_dataframe)
  new_dataframe$amount.spent<-floor(runif(n,min=1000,max=3000))
  new_dataframe$reach<-floor(runif(n,min=500,max=8000))
  new_dataframe$Video.plays.at.75<-floor(runif(n,min=500,max=5000))
  
  for (t in 1:n){
    new_dataframe$reactions<-floor(new_dataframe[t,'reach']/runif(n,min=1.5,max=6.5))  
    new_dataframe[t,'Post.engagement.rate']<-new_dataframe[t,'reactions']/new_dataframe[t,'reach']
    new_dataframe[t,'cost.per.1000.reached']<-new_dataframe[t,'amount.spent']/new_dataframe[t,'reach']
    new_dataframe[t,'Video.engagement.rate.75']<-new_dataframe[t,'Video.plays.at.75']/new_dataframe[t,'reach']
  }
  return(new_dataframe)
}
generate_entire_dataset<-function(new_df){
  write.xlsx2(new_df,
              'revised_filtered.xlsx',
              sheetName ='revised_filtered',
              col.names = TRUE, row.names = FALSE,
              append=FALSE)
  df<-read.xlsx2(
    'revised_filtered.xlsx',
    sheetIndex = 1,
    startRow = 1,
    header = TRUE,
  )
  return(df)
}
#final_df<-populate_dataframe()

# --- Global variables
#audiences<-unique(revised_filtered$audience_name)
#platforms<-unique(revised_filtered$Platform)
#months_list<-unique(revised_filtered$reported_date)
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#999999", "#0072B2", "#D55E00", "#CC79A7")
filename<-paste0("output/Summary_Analytics_",Sys.Date(),'.xlsx')

# --- My plot_options
my_facets <- function(facet_type, df) {
  if (facet_type == 'grid') {
    #facet_grid
    g <-
      ggplot(df,
             aes(
               x = AsDate(reported_date),
               y = post_mean,
               group = platform,
               colour = audience,
             )) + ylim(c(0, 1)) + geom_point()   + facet_grid(
               cols = vars(platform),
               rows = vars(city),
               scales = 'free',
               labeller = label_both
             ) + theme(
               panel.background = element_rect(colour='#000000',
                                               fill='#FFFFFF'),
               plot.title = element_text(
                 size = 20,
                 #face="bold",
                 # family="Georgia",
                 color =
                   "blue",
                 hjust =
                   0.5,
                 lineheight =
                   1.2
               ),plot.subtitle = element_text(size=15,face='bold',color='red'),
               axis.title.x =
                 element_text(
                   # vjust = 10,
                   size = 15),
               axis.title.y =
                 element_text(size = 15),
               axis.text.x =
                 element_text(
                   size = 10,
                   angle = 60,
                   vjust =
                     .5
                 ),
               axis.text.y = element_text(size = 10),
               legend.position = c('bottom'),
               panel.grid.minor = element_blank(),
               #panel.grid.major = element_blank(),
               strip.text = element_text(size = 13)+theme_minimal()
               #  ) +scale_y_continuous(expand = c(0, 1)
             ) + labs(y = "RESPONSE RATE",
                      x = "REPORTED MONTH",
                      title =
                        "SOCIAL MEDIA ANALYSIS OF TARGETED AUDIENCES",
                      subtitle = 'POST'
             ) + scale_x_date(date_labels = "%B-%Y",breaks='1 month')
  }
  else if (facet_type == 'video_grid') {
    #facet_grid
    g <-
      ggplot(df,
             aes(
               x = AsDate(reported_date),
               y = video_mean,
               group = platform,
               colour = audience,
               alpha=1/2,
             )) + ylim(c(0, 1)) + geom_point()   + facet_grid(
               cols = vars(platform),
               rows = vars(city),
               scales = 'free',
               labeller = label_both
             ) + theme(
               panel.background = element_rect(colour='#000000',
                                               fill='#FFFFFF'),
               plot.title = element_text(
                 size = 20,
                 #face="bold",
                 # family="Georgia",
                 color =
                   "blue",
                 hjust =
                   0.5,
                 lineheight =
                   1.2
               ),plot.subtitle = element_text(size=15,face='bold',color='red'),
               axis.title.x =
                 element_text(
                   # vjust = 10,
                   size = 15),
               axis.title.y =
                 element_text(size = 15),
               axis.text.x =
                 element_text(
                   size = 10,
                   angle = 60,
                   vjust =
                     .5
                 ),
               axis.text.y = element_text(size = 10),
               legend.position = c('bottom'),
               panel.grid.minor = element_blank(),
               #panel.grid.major = element_blank(),
               strip.text = element_text(size = 13)+theme_minimal()
               #  ) +scale_y_continuous(expand = c(0, 1)
             ) + labs(y = "RESPONSE RATE",
                      x = "REPORTED MONTH",
                      title =
                        "SOCIAL MEDIA ANALYSIS OF TARGETED AUDIENCES",
                      subtitle = 'VIDEO'
             ) + scale_x_date(date_labels = "%B-%Y",breaks='1 month')
    
  }
  else if (facet_type == 'plotly') {
    #facet_grid
    platforms=unique(df$platform)
    grid_plots<-list()
    for(i in 1:length(platforms)){
      subset_df<-subset(df,platform==platforms[i])
      fig <- plot_ly(subset_df, x = ~reported_date, y = ~post_rate, type = 'scatter', mode = 'lines')
      grid_plots[[i]]<-fig
    } 
    #y = "RESPONSE RATE",
    # x = "REPORTED MONTH"
    # title ="SOCIAL MEDIA ANALYSIS OF TARGETED AUDIENCES")
    g<-subplot(grid_plots,nrows = 1)
  }
  else if (facet_type == 'wrap') {
    g <-
      ggplot(df,
             aes(x = reported_date,
                 y = post_mean)) + geom_line() + geom_point(aes(col = audience)) + facet_wrap(
                   vars(city),
                   ncol = 4,
                   nrow = numberofcities / 3,
                   scales = 'free_y',
                   labeller = label_wrap_gen(multi_line = FALSE)
                 ) + theme(
                   legend.position = 'none',
                   panel.grid.minor = element_blank(),
                   panel.grid.major = element_blank(),
                   strip.text = element_text(size = 13)
                 ) + scale_y_continuous(expand = c(0, 1))
  }
  else{
    g <-
      ggplot(results, aes(x = reported_date, y = post_mean)) + geom_point(aes(col =
                                                                                audience)) + facet_wrap(vars(city),
                                                                                                        ncol = 1) + theme(legend.position = 'none') + scale_y_continuous(expand = c(0, 1))
  }
  return(g)
}
audience_plot<-function(df,medium_type,audience_name){
  y_attribute<-''
  if (medium_type=='post'){
    y_attribute<-df$Post.engagement.rate
  }
  if(medium_type=='video'){
    y_attribute<-df$Video.engagement.rate.75.
  }
  g<-ggplot(df,aes(x=AsDate(reported_date),
                   y=y_attribute,
                   label=y_attribute)
  )+geom_bar(aes(fill=Platform),
             position = 'dodge',
             stat='identity'
  )+ theme(panel.background = element_rect(colour='#000000'
                                           #fill='#FFFFFF'
  ), plot.title = element_text(
    size = 20,
    face="bold",
    #family="Arial",
    color =
      cbPalette[6],
    hjust =
      0.5,
    lineheight =
      1.2
  ),plot.subtitle = element_text(size=14,face='bold',color=cbPalette[7]),
  axis.title.x =
    element_text(
      # vjust = 10,
      size = 12),
  axis.title.y =
    element_text(size = 12),
  axis.text.x =
    element_text(
      size = 10,
      angle = 60,
      vjust =
        .6
    ),
  axis.text.y = element_text(size = 10),
  legend.position = c('right'),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  #strip.text = element_text(size = 13)
  ) + labs(y = "RESPONSE RATE",
           x = "REPORTED MONTH",
           subtitle = paste('Town:', df$Account.name[1],' - ','Publication type: ',medium_type),
           title=audience_name
  ) + scale_x_date(date_labels = "%B-%Y",
                   breaks='1 month')+scale_fill_manual("legend",
                                                       values=cbPalette
                   )
  #)+geom_text(size = 3, position = position_stack(vjust = 0.5))
  
  return(g)
}


#generate output
audience_summary<-function(df){
  #cities<-unique(df$Account.name)
  cities=c('AMIENS','TOULOUSE','NANCY')
  #clear output folder 
  do.call(file.remove, list(list.files("output/", full.names = TRUE)))
  #wb = openxlsx::createWorkbook( creator = "yannLHOR",
  #title = "Social Media Analytics")
  wb <- xlsx::createWorkbook(type = "xlsx")
  for (i in cities){
    #addWorksheet(wb,i,gridLines = FALSE)
    sheet <- xlsx::createSheet(wb, sheetName = i)
    list_pages<-list()
    counter=1
    subset_df<-filter(df,Account.name==i)
    audience_list<-unique(subset_df$Ad.set.name)
    for (a in audience_list){
      #preparing datasets
      subset_df1<-filter(subset_df,Ad.set.name==a)
      subset_post<-filter(subset_df1,medium=='post')
      subset_video<-filter(subset_df1,medium=='video')
      subset_post<-filter(subset_post,!is.na(Post.engagement.rate))
      subset_video<-filter(subset_video,!is.na(Video.engagement.rate.75.))
      #generate datasets and plots
      post_dataset<-NULL
      video_dataset<-NULL
      fig_post<-NULL
      fig_video<-NULL
      if(nrow(subset_post)>0){
        post_dataset<-audience_dataset(subset_post,months_list,'post','AMIENS','AUDIENCE0HE')
        fig_post<-audience_plot(df,'post',a)
      }
      if(nrow(subset_video)>0){
        video_dataset<-audience_dataset(subset_video,months_list,'video','AMIENS','AUDIENCE0HE')
        fig_video<-audience_plot(df,'video',a)
      }
      #Remove null objects
      one_page<-c(fig_post,post_dataset,fig_video,video_dataset)
      plot_list<-list(fig_post,fig_video)
      tables_list=list(post_dataset,video_dataset)
      #one_page[[1]]<-fig_post
      #one_page[[3]]<-post_dataset
      #one_page[[2]]<-fig_video
      #one_page[[4]]<-video_dataset
      #plot_list<-ggarrange(fig_post,fig_video,ncol=2,nrow=1)
      #tables_list[[1]]<-ggtexttable(post_dataset,theme = ttheme("mOrange"))
      #tables_list[[2]]<-ggtexttable(video_dataset,theme = ttheme("mOrange"))
      revised_page<-list()
      page_counter<-1
      object_counter<-1
      tables_counter<-1
      new_plot_list<-list()
      new_tables_list<-list()
      # for (z in 1:length(plot_list)){
      #   if (is.null(plot_list[[z]])){
      #    }
      #    else{
      #      new_plot_list[[object_counter]]<-plot_list[[z]]
      #      object_counter=object_counter+1
      #    }
      #}
      for (z in 1:length(tables_list)){
        if (is.null(tables_list[[z]])){
        }
        else{
          #new_table_list[[tables_counter]]<-ggtexttable(tables_list[[z]],theme = ttheme("mOrange"))
          new_tables_list[[tables_counter]]<-tables_list[[z]]
        }
        tables_counter=tables_counter+1
      }
      for (z in 1:length(one_page)){
        if (is.null(one_page[[z]])){}
        else{
          if(typeof(one_page[[z]])=='character'){
            #revised_page[[page_counter]]<-ggtexttable(one_page[[z]],theme = ttheme("mOrange"))
          }
          if(typeof(one_page[[z]])=='list'){
            #revised_page[[page_counter]]<-one_page[[z]]
          }
          #page_counter=page_counter+1
        } 
      }
      for (z in 1:length(objects_list)){
        if (is.null(objects_list[[z]])){}
        else{
          revised_page[[page_counter]]<-objects_list[[z]]
        }
        page_counter=page_counter+1
      } 
      
      #Generate output document
      # new_output<-list()
      output_counter<-1
      #if(length(plot_list)==1){
      #new_output[[output_counter]]<-new_plot_list[[1]]
      # output_counter<-output_counter+1
      #}
      # else {
      #  new_output[[output_counter]]<-new_plot_list[[1]]
      #  output_counter<-output_counter+1
      #  new_output[[output_counter]]<-new_plot_list[[2]]
      #  output_counter<-output_counter+1
      # }
      #if(length(tables_list)==1){
      #  new_output[[output_counter]]<-new_tables_list[[1]]
      #  }
      #else{
      #new_output[[output_counter]]<-new_tables_list[[1]]
      # output_counter<-output_counter+1
      # new_output[[output_counter]]<-new_tables_list[[2]]
      #}
      
      #output2<-grid.arrange(plot_list,tables_list,nrow=3)
      #numberofiems<-length(revised_page)
      #output_page<-ggarrange(plotlist=revised_page,ncol=1,nrow=numberofiems) %>% ggexport(filename = "test.pdf")
      #output_page<-ggarrange(plotlist=revised_page,ncol=2,nrow=3)
      #output_page<-ggarrange(plot_list,tables_list[[1]], tables_list[[2]],ncol=1,nrow=3)
      
      print(counter)
      list_pages[[counter]]<-revised_page
      #list_pages[[counter]]<-new_output
      #list_pages[[counter]]<-output_page
      #write_to_pdf2(paste(a,'-',i),output_page)
      #write_to_pdf2(paste(a,'-',i),new_output)
      #write_to_pdf2(paste(a,'-',i),output2)
      counter=counter+1
    }
    write_data_excel3(list_pages,i)
    
  }
  
  #saveWorkbook(wb, file =paste0("output/Summary_Analytics_",Sys.Date(),'.xslx'),overwrite = TRUE)
  #staplr::staple_pdf(input_directory='output/',
  #                   output_filepath=paste0('Summary_Analysis_',Sys.Date(),'.pdf'),
  #                   overwrite=TRUE)
  #return(list_pages)
  
}
#audience_results<-audience_summary(revised_filtered)
audience_dataset<-function(df,months_list,medium,city,audience){
  t<-as.data.frame(months_list)
  new_month_order<-order(AsDate(t$months_list))
  #Create matrices
  total_matrix<-matrix()
  monthly_matrix<-matrix()
  post_names<-c('Type',
                'Town',
                'Audience',
                'Month',
                'Post.engagement.rate',
                'Reactions',
                'Engagment',
                'Reach',
                'Cost.per.1000.in.Euros',
                'Amount spent in Euros'
  )
  video_names<-c('Type',
                 'Town',
                 'Audience',
                 'Month',
                 'Video.engagement.rate.75.',
                 'Reactions',
                 'Video.plays.at.75.',
                 'Reach',
                 'Cost.per.1000.in.Euros',
                 'Amount spent in Euros'
  )
  if(medium=='post') {
    total_matrix<-matrix(0,nrow=length(post_names),ncol=0)
  }
  if(medium=='video') {
    total_matrix<-matrix(0,nrow=length(video_names),ncol=0)
  }
  for (m in 1:length(new_month_order)){
    subset_df<-df %>% filter(df$reported_date==months_list[new_month_order[m]])
    temp_post<-matrix(0,nrow=length(post_names),ncol=length(platforms)) 
    colnames(temp_post)<-platforms
    rownames(temp_post)<-post_names
    temp_video<-matrix(0,nrow=length(video_names),ncol=length(platforms)) 
    colnames(temp_video)<-platforms
    rownames(temp_video)<-video_names
    if (medium=='post'){
      subset_post<-filter(subset_df, medium=='post')
      monthly_matrix<-matrix(0,nrow=length(post_names),ncol=0)
      if(nrow(subset_post)>0){
        for (p in platforms){
          subset_df1<-subset_post %>% filter(subset_post$Platform==p)
          temp_post['Type',p]<-medium
          #temp_post['City',p]<-subset_df1$Account.name[1]
          temp_post['Town',p]<-city
          temp_post['Audience',p]<-audience
          temp_post['Month',p]<-months_list[new_month_order[m]]
          #if(all(is.na(subset_df1$Post.engagement))){
          # temp_post['Post.engagement.rate',p]<-0
          #}
          #else{
          temp_post['Post.engagement.rate',p]<-paste0(format(round(100*(sum(as.numeric(subset_df1$Post.engagement.rate))),2)),'%')  
          #temp_post['Post.engagement.rate',p]<-paste0(format(round(100*(sum(as.numeric(subset_df1$Post.engagement))/sum(as.numeric(subset_df1$Reach))),2)),'%')
          # }
          temp_post['Reactions',p]<-sum(as.numeric(subset_df1$Post.reactions))
          temp_post['Engagment',p]<-sum(as.numeric(subset_df1$Post.engagement))
          temp_post['Reach',p]<-sum(as.numeric(subset_df1$Reach))
          temp_post['Cost.per.1000.in.Euros',p]<-format(round(sum(as.numeric(subset_df1$Cost.per.1.000.people.reached)),2))
          temp_post['Amount spent in Euros',p]<-format(round(sum(as.numeric(subset_df1$Amount.spent)),2))
        }
        monthly_matrix<-cbind(monthly_matrix,temp_post)
      }
    } 
    if (medium=='video'){
      #total_matrix<-matrix(0,nrow=length(video_names),ncol=0)
      monthly_matrix<-matrix(0,nrow=length(video_names),ncol=0)
      subset_video<-subset_df %>% filter(medium=='video')
      if(nrow(subset_video)>0){
        for (p in platforms){
          subset_df3<-subset_video %>% filter(Platform==p)
          temp_video['Type',p]<-medium
          temp_video['Town',p]<-city
          temp_video['Audience',p]<-audience
          
          temp_video['Month',p]<-months_list[new_month_order[m]]
          #if(all(is.na(subset_df3$Video.engagement.rate.75.))){
          #temp_post['Video.engagement.rate.75.',p]<-0
          #}
          # else{
          #temp_video['Video.engagement.rate.75.',p]<-paste0(round(sum(as.numeric(subset_df3$Video.engagement.rate.75.))/sum(as.numeric(subset_df3$Reach)),digits=2),'%')
          temp_video['Video.engagement.rate.75.',p]<-paste0(round(100*(sum(subset_df3$Video.engagement.rate.75.)),2),'%')
          #}
          temp_video['Reactions',p]<-sum(as.numeric(subset_df3$Post.reactions))
          temp_video['Video.plays.at.75.',p]<-sum(as.numeric(subset_df3$Video.plays.at.75.))
          temp_video['Reach',p]<-sum(as.numeric(subset_df3$Reach))
          temp_video['Cost.per.1000.in.Euros',p]<-format(round(sum(as.numeric(subset_df3$Cost.per.1.000.people.reached)),2))
          temp_video['Amount spent in Euros',p]<-format(round(sum(as.numeric(subset_df3$Amount.spent)),2))
        }
        monthly_matrix<-cbind(monthly_matrix, temp_video)
      }
    }
    total_matrix<-cbind(total_matrix,monthly_matrix)
  }
  return(total_matrix)
}
output_tables<-function(df){
  #Set number of cities
  #cities<-unique(df$Account.name)
  cities=c('AMIENS','TOULOUSE','NANCY')
  
  # Clear output folder 
  do.call(file.remove, list(list.files("output/", full.names = TRUE)))
  
  # Create excel file
  wb <- xlsx::createWorkbook()
  for (i in cities){
    # Create worksheet
    sheet <- xlsx::createSheet(wb, sheetName = i)
    
    # List of data frames
    list_pages<-list()
    counter=1
    
    # Filter to specific audience
    subset_df<-filter(df,Account.name==i)
    audience_list<-unique(subset_df$Ad.set.name)
    
    for (a in audience_list){
      #preparing datasets
      subset_df1<-filter(subset_df,Ad.set.name==a)
      subset_post<-filter(subset_df1,medium=='post')
      subset_video<-filter(subset_df1,medium=='video')
      subset_post<-filter(subset_post,!is.na(Post.engagement.rate))
      subset_video<-filter(subset_video,!is.na(Video.engagement.rate.75.))
      
      #generate datasets
      post_dataset<-NULL
      video_dataset<-NULL
      if(nrow(subset_post)>0){
        post_dataset<-audience_dataset(subset_post,months_list,'post',i,a)
      }
      if(nrow(subset_video)>0){
        video_dataset<-audience_dataset(subset_video,months_list,'video',i,a)
      }
      
      #Remove null objects
      tables_list=list(post_dataset,video_dataset)
      new_tables_list<-list()
      for (z in 1:length(tables_list)){
        if (is.null(tables_list[[z]])){
        }
        else{
          #list_pages[[counter]]<-ggtexttable(tables_list[[z]],theme = ttheme("mOrange"))
          list_pages[[counter]]<-as.data.frame(tables_list[[z]])
          #new_tables_list[[tables_counter]]<-tables_list[[z]]
          counter=counter+1
        }
      }
      print(counter)
    }
    #Format columns in excel sheet
    
    #Write sheet in workbook -- data from one townname
    write_data_excel5(list_pages,sheet,wb)
    xlsx::autoSizeColumn(sheet=sheet,colIndex = 2:50)
  }
  filename<-paste0("output/Summary_Analytics_",Sys.Date(),'.xlsx')
  xlsx::saveWorkbook(wb, file = filename)
  #return(list_pages)
}
#table_results<-output_tables(revised_filtered)
format_excel<-function(file) {
  wb <- xlsx::loadWorkbook(file)              # load workbook
  fo1 <- Fill(foregroundColor='#f42ae6')   # create fill object # 1
  fo3<- Font(wb, heightInPoints=15, isBold=TRUE, color='#000000')
  cs_insta <- xlsx::CellStyle(wb, fill=fo1, font=fo3)  
  #cs_insta <- xlsx::CellStyle(wb) + Fill(foregroundColor="red")
  # cellStyle1 <- CellStyle(wb) +
  #  Fill(backgroundColor="orange", foregroundColor="orange",
  # pattern="SOLID_FOREGROUND") 
  # create cell style # 1
  fo2 <- Fill(foregroundColor='#4266a5')
  # create fill object # 2
  cs_fb <- CellStyle(wb, fill=fo2, font=fo3) 
  sheets <- xlsx::getSheets(wb)               # get all sheets
  for (s in sheets){
    sheet <- s 
    # get specific sheet
    rows <- getRows(sheet, rowIndex=2:5000)     # get rows
    # 1st row is headers
    cells <- getCells(rows, colIndex = 2:30)         # get cells
    values <- lapply(cells, getCellValue) # extract the values
    # find cells meeting conditional criteria 
    highlight_f <- NULL
    highlight_i <- NULL
    for (i in names(values)) {
      x <- values[i]
      if (x=='instagram') {
        highlight_i <- c(highlight_i, i)
      }
      if (x=='facebook') {
        highlight_f<- c(highlight_f, i)
      }
    }
    #for (t in highlight_i){
    # xlsx::setCellStyle(cells[[t]],cs_insta)
    #}
    #lapply(names(cells[highlight_i]),function(ii) {xlsx::setCellStyle(cells[[ii]],cs_insta)})
    #lapply(names(cells[highlight_f]),function(ii) {xlsx::setCellStyle(cells[[ii]],cs_fb)})
    lapply(names(cells[highlight_i]),
           function(ii) xlsx::setCellStyle(cells[[ii]], cs_insta))
    
    lapply(names(cells[highlight_f]),
           function(ii) xlsx::setCellStyle(cells[[ii]], cs_fb))
  }
  xlsx::saveWorkbook(wb, file = filename)
}
#format_excel(filename)
format_pdf<-function(objects_list){
  numberofiems<-length(objects_list)
  output_page<-list()
  for (i in 1:numberofiems){
    if(class(objects_list[[i]])=='matrix'){
      output_page[[i]]<-ggtexttable(objects_list[[i]])
    }
    if(class(objects_list[[i]])=='gg'){
      output_page[[i]]<-(objects_list[[i]])
    }
  }
  return(ggarrange(output_page,ncol=1,nrow=numberofiems))
}
one_row_per_grid<-function(plot_type,df,medium){
  my_path<-'post'
  subset_df<-data.frame()
  cities=list()
  if(medium=='post'){
    my_path<-'post_output'
    subset_df <- df %>% dplyr::select(audience, post_mean,reported_date,platform,city) 
    do.call(file.remove, list(list.files("post_output/", full.names = TRUE)))
    cities<-unique(subset_df$city)
    subset_df<-subset_df %>% filter(!is.na(post_mean))
    #return(subset_df)
  }
  if(medium=='video'){
    my_path<-'video_output'
    subset_df <- df %>% dplyr::select(audience, video_mean,reported_date,platform,city) 
    subset_df<-subset_df %>% filter(!is.na(video_mean))
    do.call(file.remove, list(list.files("video_output/", full.names = TRUE)))
    cities<-unique(subset_df$city)
    #return(subset_df)
  }
  
  numberofcities<-length(cities)
  platforms<-unique(subset_df$platform)
  #temp_df<-data.frame()
  #last_plot_index<-floor(numberofcities/3)
  for (i in 1:numberofcities){
    # grid_plots<-c()
    subset_df1<- subset(subset_df,city == cities[i])
    #temp_df<-rbind(temp_df,subset_df1)
    g<-my_facets(plot_type,subset_df1)
    #grid_plots[[i]]<-g
    dev.set(dev.next())
    file<-tempfile()
    ggsave(filename=paste0(i,'.pdf'),
           plot=g,
           device='pdf',
           width = 11,
           height = 8,
           units='in',
           dpi=300,
           limitsize = TRUE,
           path=my_path)
    dev.off()
    temp_df<-data.frame()
  }
  staplr::staple_pdf(input_directory=my_path,
                     output_filepath=paste0('Merged_Analysis_',medium,'_',Sys.Date(),'.pdf'),
                     overwrite=TRUE)
}



# platform analysis
analyze_platform<-function(final_df,selected){
  graphs_list<-list()
  if (selected=='All Platforms'){
    platform_post<-subset(final_df,medium=='post')
    platform_video<-subset(final_df,medium=='video')
    dist_platform_post<-ggplot(platform_post,aes(x=AsDate(reporting_month))) + geom_histogram(aes(fill=platform),binwidth = 10,position='dodge2') +scale_x_date()
    dist_platform_video<-ggplot(platform_video,aes(x=AsDate(reporting_month))) + geom_histogram(aes(fill=platform),binwidth = 10,position='dodge2') +scale_x_date()
    dist_platform<-subplot(ggplotly(dist_platform_post),ggplotly(dist_platform_video),nrows = 2, shareX = TRUE)       
    dist_platform
    boxplot_post<-ggplot(platform_post,aes(x=platform,y=Post.engagement.rate)) + geom_boxplot(aes(fill=platform))
    boxplot_video<-ggplot(platform_video,aes(x=platform,y=Video.engagement.rate.75)) + geom_boxplot(aes(fill=platform))
    boxplot_platform<-subplot(ggplotly(boxplot_post),ggplotly(boxplot_video),nrows=2,shareX=TRUE)
    boxplot_platform
    # graphs_list<-c(dist_platform,boxplot_platform) 
    graphs_list[[1]]<-dist_platform
    graphs_list[[2]]<-boxplot_platform
  }
  else {
    platform_post<-subset(final_df,medium=='post' & platform==selected)
    platform_video<-subset(final_df,medium=='video'& platform==selected)
    dist_platform_post<-ggplot(platform_post,aes(x=AsDate(reporting_month))) + geom_histogram(aes(fill=audience),binwidth = 10,position='dodge2'
    )+scale_x_date()
    dist_platform_video<-ggplot(platform_video,aes(x=AsDate(reporting_month))) + geom_histogram(aes(fill=audience),binwidth = 10,position='dodge2')+scale_x_date()
    dist_platform<-subplot(ggplotly(dist_platform_post),ggplotly(dist_platform_video),nrows = 2, shareX = TRUE)       
    boxplot_post<-ggplot(platform_post,aes(x=audience,y=Post.engagement.rate)) + geom_boxplot(aes(fill=audience))
    boxplot_video<-ggplot(platform_video,aes(x=audience,y=Video.engagement.rate.75)) + geom_boxplot(aes(fill=audience))
    boxplot_platform<-subplot(ggplotly(boxplot_post),ggplotly(boxplot_video),nrows=2,shareX=TRUE)
    #graphs_list<-c(dist_platform,boxplot_platform) 
    graphs_list[[1]]<-dist_platform
    graphs_list[[2]]<-boxplot_platform
  }
  return (graphs_list)
 }
# Audience analysis
analyze_audience<-function(final_df,selected) {
  graphs_list<-list()
  if (selected=='All Audiences'){
    platform_post<-subset(final_df,medium=='post')
    platform_video<-subset(final_df,medium=='video')
    #yearly distribution of response rate for all audiences (boxplot)
    boxplot_post<-ggplot(platform_post,aes(x=audience,y=Post.engagement.rate)) + geom_boxplot(aes(fill=audience))
    boxplot_video<-ggplot(platform_video,aes(x=audience,y=Video.engagement.rate.75)) + geom_boxplot(aes(fill=audience))
    boxplot_audience<-subplot(boxplot_post,boxplot_video,nrows=2,shareX=TRUE)
    boxplot_audience
  
    #cumulative response rate v time barplot
    barplot_audience<-ggplot(platform_post,aes(x=AsDate(reporting_month),
                                 y=Post.engagement.rate,fill=audience
    )) + geom_bar( position='dodge',
                   stat='identity'
    )+scale_x_date()+ labs(y = "RESPONSE RATE",
                           x = "REPORTED MONTH"
                           # subtitle = paste('Town:', platform_post[counter,'city'],' - ','Publication type: ',platform_video$medium_type),
                           # title=audience_name
    ) +scale_fill_manual("legend",
                         values=cbPalette
    )
    barplot_audience
    #graphs_list <- c(boxplot_audience,barplot_audience)
    graphs_list[[1]]<-boxplot_audience
    graphs_list[[2]]<-barplot_audience
  }
  else {
    audience_post<-subset(final_df,medium=='post' & audience==selected)
    audience_video<-subset(final_df,medium=='video' & audience==selected)
    # plot facet grid of Rate vs time x every platform x every city
    audience_grid<-ggplot(audience_post,aes(x=AsDate(reporting_month),
                                            y=Post.engagement.rate)
    ) + geom_point()+geom_line()+ facet_grid(city~platform) +scale_x_date()+scale_y_continuous()
    audience_grid
    audience_grid_video<-ggplot(audience_video,aes(x=AsDate(reporting_month),
                                                   y=Video.engagement.rate.75)
    ) + geom_point()+geom_line()+ facet_grid(city~platform) +scale_x_date()+scale_y_continuous()
    audience_grid_video
    graphs_list<-c(audience_grid,audience_grid_video)
    graphs_list[[1]]<-audience_grid
    graphs_list[[2]]<-audience_grid_video
  }
  return(graphs_list)
}
#city analysis
analyze_city<-function(final_df,selected){
  graphs_list<-list()
  if (selected=='All Cities'){ 
    city_post<-subset(final_df,medium=='post')
    city_video<-subset(final_df,medium=='video')
    city_grid<-ggplot(city_post,aes(x=AsDate(reporting_month),
                                            y=Post.engagement.rate)
    ) + geom_point()+ facet_grid(city~platform) +scale_x_date()+scale_y_continuous()
    city_grid
    city_grid_video<-ggplot(city_video,aes(x=AsDate(reporting_month),
                                                   y=Video.engagement.rate.75)
    ) + geom_point()+geom_line()+ facet_grid(city~platform) +scale_x_date()+scale_y_continuous()
    city_grid_video
    
    city_audience_grid<-ggplot(city_post,aes(x=AsDate(reporting_month),
                                    y=Post.engagement.rate,
                                    fill=)
    ) + geom_point()+geom_line()+ facet_grid(city~audience) +scale_x_date()+scale_y_continuous()
    city_audience_grid
    city_audience_video<-ggplot(city_video,aes(x=AsDate(reporting_month),
                                           y=Video.engagement.rate.75)
    ) + geom_point()+geom_line()+ facet_grid(city~platform) +scale_x_date()+scale_y_continuous()
    city_audience_video
    #graphs_list<-c(city_grid, city_grid_video, city_audience_grid,city_audience_video)
    graphs_list[[1]]<-city_grid
    graphs_list[[2]]<-city_grid_video
    graphs_list[[3]]<-city_audience_grid
    graphs_list[[4]]<-city_audience_video
  } else {
    city_post<-subset(final_df,medium=='post' & city==selected)
    city_video<-subset(final_df,medium=='video' & city==selected)
    
    city_grid<-ggplot(city_post,aes(x=AsDate(reporting_month),
                                    y=Post.engagement.rate)
    ) + geom_point()+ geom_line()+facet_grid(audience~platform) +scale_x_date()+scale_y_continuous()
    city_grid
    city_grid_video<-ggplot(city_video,aes(x=AsDate(reporting_month),
                                           y=Video.engagement.rate.75)
    ) + geom_point()+geom_line()+ facet_grid(audience~platform) +scale_x_date()+scale_y_continuous()
    city_grid_video
    #graphs_list<-c(city_grid,city_grid_video)
    graphs_list[[1]]<-city_grid
    graphs_list[[2]]<-city_grid_video
  }
  
  return(graphs_list)
}

