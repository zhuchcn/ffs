## ---------------------------------------------------------
## ----------------- loading packages ----------------------
library(dplyr);library(plyr);library(stringr);library(reshape2);
library(ggplot2);library(Biostrings);library(RSkittleBrewer)
library(pheatmap);library(RColorBrewer);library(msa);library(rBLAST)
library(plotly);library(DESeq2);library(tibble);library(RPMG)
library(ggthemes); library(ggthemr);library(ggrepel)
## ---------------------------------------------------------
add_taxo_level_to_fdata = function(data){
    edata = data$edata
    fdata = data$fdata
    pdata = data$pdata
    seqs = data$seqs
    
    for(i in 1:7){
        fdata[,i] = str_c(
            tolower(strsplit(names(fdata)[i],'')[[1]][1]),
            '_', fdata[,i]
        )
    }
    
    return(list(edata=edata, fdata=fdata, pdata=pdata, seqs=seqs))
}

## ------------------ Nomralization ------------------------
normalizeRelativeAbundence = function(data){
    edata = data$edata
    pdata = data$pdata
    fdata = data$fdata
    seqs = data$seqs
    
    edata = apply(edata,2, function(xx){
        yy = xx/sum(xx)
        names(yy) = rownames(edata)
        return(yy)
    }) %>% as.data.frame
    
    return(list(edata=edata, pdata=pdata, fdata=fdata, seqs=seqs))
}

## --------------------------------------------------------- 
## ------------------- Taxo barplot ------------------------
## ---------------------------------------------------------
taxa_plot_get_df = function(data, taxa='Phylum'){
    edata = data$edata
    fdata = data$fdata
    pdata = data$pdata
    
    df = edata %>% 
        rownames_to_column(var='FeatureID') %>% 
        mutate(taxa = fdata[,taxa]) %>%
        melt(c('FeatureID','taxa'), 
             variable.name = 'SampleID',
             value.name='Count') %>%
        ddply(.(taxa,SampleID), summarize,
              total_count = sum(Count)) %>%
        ddply(.(SampleID), mutate,
              abundence = total_count/sum(total_count)) %>%
        dcast(SampleID~taxa, value.var='abundence') %>%
        mutate(SubjID = pdata$StudyID,
               TX = pdata$Treatment,
               Day = pdata$Timepoint)%>% 
        melt(c('SampleID','SubjID','TX','Day'),
             variable.name = 'taxa',
             value.name = 'abundence')
    return(df)
}

# taxa_plot_get_labs return the most abundant taxa classifiers of a given 
# taxonomy level to print in legend in taxonomy bar plot
taxa_plot_get_labs = function(data, taxa = 'Phylum', n=10){
    edata = data$edata
    fdata = data$fdata
    pdata = data$pdata
    
    df = taxa_plot_get_df(data, taxa=taxa) %>%
        select(c(SampleID,taxa,abundence)) %>%
        ddply(.(taxa), summarize,
              mean = mean(abundence)) %>% 
        arrange(desc(mean))
    
    breaks = head(df$taxa,n) %>% as.character
    return(breaks)
}

# taxa_plot maks the taxonomy bar plot
taxa_plot = function(data, taxa='Phylum', n=10){
    df = taxa_plot_get_df(data, taxa)
    breaks = taxa_plot_get_labs(data, taxa = taxa, n = n)
    
    legend_title = str_c(taxa, ', top ',n)
    
    g = ggplot(df) +
        geom_bar(aes(x=interaction(Day, TX),y=abundence, fill=taxa),
                 stat='identity') +
        scale_fill_discrete(legend_title, breaks = breaks) +
        facet_grid(.~SubjID) +
        theme(
            text = element_text(size = 15),
            panel.background = element_rect(fill = 'white'),
            axis.ticks =element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank()
        )
    return(g)
}
## --------------------------------------------------------- 
## ---------------- Individual boxplots --------------------
## --------------------------------------------------------- 
ind_bp_get_df = function(data, FeatureID){
    edata = data$edata
    fdata = data$fdata
    pdata = data$pdata
    
    df = edata[FeatureID,] %>% 
        t %>% 
        as.data.frame %>%
        mutate(Subj = factor(pdata$SubjectID)) %>%
        mutate(TX = pdata$Treatment) %>%
        mutate(Day = pdata$Timepoint) %>%
        melt(id.vars=c('Subj','TX','Day'), 
             value.name= 'abundence',
             variable.name = 'FeatureID')
    return(df)
}

get_taxa_bn = function(row,i=7) {
    if(i==0) return('_;_')
    if(is.na(row[i])){
        get_taxa_bn(row,i-1)
    } else{
        return(str_c(row[i-1],'; ',row[i]))
    }
}

ind_bp = function(data, FeatureID){
    edata = data$edata
    pdata = data$pdata
    fdata = data$fdata
    
    taxa_bn = apply(fdata,1,get_taxa_bn)
    taxonomy = taxa_bn[FeatureID]
    df = ind_bp_get_df(data, FeatureID)
    df$FeatureID = taxonomy[df$FeatureID]
    
    g = ggplot(df,aes(x = interaction(Day,TX),y=abundence)) +
        geom_boxplot() +
        geom_point(aes(colour = Subj)) +
        geom_line(data = df[df$TX=='FF',], 
                  aes(group=Subj, colour=Subj)) +
        geom_line(data = df[df$TX=='Med',], 
                  aes(group=Subj, colour=Subj)) + 
        facet_wrap(~FeatureID, scales = 'free_y') +
        theme_hc() +
        theme(
            panel.background = element_blank(),
            panel.grid = element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(size=rel(1.25), color = 'black'),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            legend.position = 'none',
            strip.text = element_text(size=15))
    return(g)
}
## --------------------------------------------------------- 
## ----------------- M S A   Alignment ---------------------
## --------------------------------------------------------- 
sort_data_for_msa = function(data){
    edata = data$edata
    pdata = data$pdata
    fdata = data$fdata
    seqs = data$seqs
    
    fdata = fdata %>%
        rownames_to_column(var = 'FeatureID') %>%
        arrange(Kingdom, Phylum, Class, 
                Order, Family, Genus, Species) %>%
        column_to_rownames(var = 'FeatureID')
    
    edata = edata[rownames(fdata),]
    
    return(list(edata=edata, fdata=fdata, pdata=pdata, seqs=seqs))
}
run_msa = function(data, var_ids, by='taxonomy', taxa_bn, show_consensus = T){
    seqs = data$seqs
    
    if(by == 'taxonomy') features = names(taxa_bn[taxa_bn == var_ids])
    else if(by == 'feature') features = var_ids
    
    if(length(features)==1){
        return(cat('Only 1 feature with this taxonomy label'))
    }
    
    seqs_test = seqs[features]
    distance_ref = sapply(seqs_test, function(xx){
        alignment = pairwiseAlignment(seqs_test[1], xx)
        return(nmismatch(alignment))
    })
    
    for(i in 2:length(seqs_test)){
        if(distance_ref[i] > 50) seqs_test[i] = reverseComplement(seqs_test[i])
    }
    
    msa_alignments = msa(seqs_test)
    print(msa_alignments, show='complete', showConsensus = show_consensus)
}

## --------------------------------------------------------- 
## This function calculates a edit distance matrix for a group of sequences.
## Use it to look at how similar those sequences are, that were assigned to
## the same taxonomy labels.
distance_matrix = function(seqs){
    ndist_ref = sapply(seqs, function(xx){
        alignment = pairwiseAlignment(xx, seqs[1])
        return(nmismatch(alignment))
    })
    
    for(i in 2:length(seqs)){
        if(ndist_ref[i] > 50){
            seqs[i] = reverseComplement(seqs[i])
        }
    }
    
    matrix = sapply(seqs, function(xx){
        sapply(seqs, function(yy){
            alignment = pairwiseAlignment(xx,yy)
            distance_xx = nmismatch(alignment)
            return(distance_xx)
        })
    })
    
    return(matrix)
}

distance_matrix_heatmap = function(seqs){
    
}

## --------------------------------------------------------- 
## Setup the blast enviroment
Sys.setenv(PATH = paste(
    Sys.getenv('PATH'),
    '/Users/chenghaozhu/bio_tools/blast/ncbi-blast-2.7.1+/bin',
    sep = ':'))
bl <- blast(db = '~/bio_tools/blastdb/16SMicrobial')

## -------------------- B L A S T n ------------------------ 
run_blastn = function(data, feature){
    seqs = data$seqs[feature]
    writeXStringSet(seqs, 'input.fasta', format='fasta', width=10000)
    
    system("blastn -db ~/bio_tools/blastdb/16SMicrobial -query input.fasta -out output.txt", intern =T)
#    con = file('output.txt')
#    while (TRUE) {
#        line = readLines(con, n=1)
#        if ( length(line) == 0){
#            break
#        }
#        print(cat(line))
#    }
    
    results = readLines('output.txt')

    file.remove("input.fasta")
    file.remove("output.txt")
    
    return(cat(results, sep='\n'))
}
## ------------------ msa with N C B I ---------------------
getRefGeneFromNCBI = function(gene_id){
    
    recs = entrez_fetch(db='nuccore', id=gene_id, rettype = 'fasta')
    recs = str_split(recs, '>', simplify=T)[-1]
    recs = str_split(recs, '\n', n=2, simplify = T)
    recs = gsub('\n', '', recs)
    ref_seq = recs[2]
    names(ref_seq) = recs[1]
    
    return(DNAStringSet(ref_seq))
}
run_msa_ncbi = function(gene_id, feature, data, rc=F){
    ref_seq = getRefGeneFromNCBI(gene_id)
    
    seq = data$seqs[feature]
    
    if(rc == T) seq = reverseComplement(seq)
    
    msa_alignment = msa(c(seq,ref_seq))
    print(msa_alignment, show='complete', showConsensus = F, halfNrow=30)
}