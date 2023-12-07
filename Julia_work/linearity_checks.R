library(ggplot2)
library(glue)
library(dplyr)
library(tidyverse)

data_dir <- '/Users/u1001626/Documents/PhD/Statistical Practice/Projects/Project2/zoib_output'

post_pred <- readRDS(glue("{data_dir}/post_op_pred.rds"))
pre_pred <- readRDS(glue("{data_dir}/pre_op_pred.rds"))
post_res <- readRDS(glue("{data_dir}/post_op_res.rds"))
pre_res <- readRDS(glue("{data_dir}/pre_op_res.rds"))

post_pdf <- rep(0,115)
post_rdf <- rep(0,115)
for (i in 1:20){
    preds <- rep(0,115)
    resids <- rep(0,115)
    
    prd_df1 <- cbind(post_pred[[i]][[1]])
    prd_df2 <- cbind(post_pred[[i]][[2]])
    prd_df3 <- cbind(post_pred[[i]][[3]])
    prd_df4 <- cbind(post_pred[[i]][[4]])
    
    res_df1 <- cbind(post_res[[i]][[1]])
    res_df2 <- cbind(post_res[[i]][[2]])
    res_df3 <- cbind(post_res[[i]][[3]])
    res_df4 <- cbind(post_res[[i]][[4]])
    
    for (j in 1:115){
        prd_col1 <- prd_df1[,j]
        prd_col2 <- prd_df2[,j]
        prd_col3 <- prd_df3[,j]
        prd_col4 <- prd_df4[,j]
        
        res_col1 <- res_df1[,j]
        res_col2 <- res_df2[,j]
        res_col3 <- res_df3[,j]
        res_col4 <- res_df4[,j]
        
        preds[j] <- mean(c(prd_col1,prd_col2,prd_col3,prd_col4))
        #print(i,j,preds[j])
        resids[j] <- mean(c(res_col1,res_col2,res_col3,res_col4))
    }
    post_pdf <- cbind(post_pdf,preds)
    post_rdf <- cbind(post_rdf,resids)
}

# Post operation attention model linearity plots
for (i in 1:20){
    pst_df <- cbind(pred=post_pdf[,i+1],resid=post_rdf[,i+1])
    pst_df <- as.data.frame(pst_df)
    post_plot <- ggplot(pst_df,aes(x=resid,y=pred))+
        geom_point(alpha=0.7) +
        labs(x="Residuals",y='Predicted Outcome')

    disp <- post_plot + 
        stat_smooth(geom='smooth',method='lm',formula=y~splines::ns(x,knots=c())) + 
        ggtitle(glue("Linearity of Residuals Post Attention Model Imputed Dataset {i}"))+ 
      theme(plot.title = element_text(face='bold',hjust = 0.5))
    print(disp)

}

pre_pdf <- rep(0,115)
pre_rdf <- rep(0,115)
for (i in 1:20){
    preds <- rep(0,115)
    resids <- rep(0,115)
    
    prd_df1 <- cbind(pre_pred[[i]][[1]])
    prd_df2 <- cbind(pre_pred[[i]][[2]])
    prd_df3 <- cbind(pre_pred[[i]][[3]])
    prd_df4 <- cbind(pre_pred[[i]][[4]])
    
    res_df1 <- cbind(pre_res[[i]][[1]])
    res_df2 <- cbind(pre_res[[i]][[2]])
    res_df3 <- cbind(pre_res[[i]][[3]])
    res_df4 <- cbind(pre_res[[i]][[4]])
    
    for (j in 1:115){
        prd_col1 <- prd_df1[,j]
        prd_col2 <- prd_df2[,j]
        prd_col3 <- prd_df3[,j]
        prd_col4 <- prd_df4[,j]
        
        res_col1 <- res_df1[,j]
        res_col2 <- res_df2[,j]
        res_col3 <- res_df3[,j]
        res_col4 <- res_df4[,j]
        
        preds[j] <- mean(c(prd_col1,prd_col2,prd_col3,prd_col4))
        #print(i,j,preds[j])
        resids[j] <- mean(c(res_col1,res_col2,res_col3,res_col4))
    }
    pre_pdf <- cbind(pre_pdf,preds)
    pre_rdf <- cbind(pre_rdf,resids)
}

# Pre-operation attention model linearity plots
for (i in 1:20){
    pst_df <- cbind(pred=post_pdf[,i+1],resid=post_rdf[,i+1])
    pst_df <- as.data.frame(pst_df)
    post_plot <- ggplot(pst_df,aes(x=resid,y=pred))+
        geom_point(alpha=0.7) +
        labs(x="Residuals",y='Predicted Outcome')

    disp <- post_plot + 
        stat_smooth(geom='smooth',method='lm',formula=y~splines::ns(x,knots=c())) + 
        ggtitle(glue("Linearity of Residuals Pre Attention Model Imputed Dataset {i}"))+ 
      theme(plot.title = element_text(face='bold',hjust = 0.5))
    print(disp)

}