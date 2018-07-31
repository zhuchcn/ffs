## ------------------- loading librarys ------------------------
pkgs = c('plyr', 'dplyr','stringr','reshape2','tibble', 'DESeq2',
         'limma','data.table')
for(pkg in pkgs){
    library(pkg, quietly=TRUE, verbose=FALSE, warn.conflicts=FALSE, 
            character.only=TRUE)
}
rm(list=ls())

## ------------------- load data ----------------------
load("../../data/food_data.Rdata")

## remove non-baseline values
food_data = food_data[,1:11]
food_data = food_data[apply(food_data[,-1],1,function(row) sum(row)>0),]

## make a food category list
food_category_list = list(
    fruit = c(
        "Apple",
        "Blueberries",
        "Fruit",
        "Grapes",
        "Oranges",
        "Pineapple",
        "Tangerines",
        "Watermelon"
    ),
    vegetable = c(
        "Asparagus",
        "Avocado",
        "Beet",
        "Carrot",
        "Cauliflower",             # i think it should be here
        "Celery Root",             # ?
        "Cucumber",
        "Dish, curry, vegetable green",
        "Mushrooms",
        "Onion",
        "Potatoes",
        "Radishes",
        "Sprouts,",
        "Tomatoes"
    ),
    "leafy/green" = c(
        "Broccoli",                # is it correct?
        "Brussels Sprouts",        # and this?
        "Cabbage",
        "Greens, salad, herb, mix, fresh",
        "Greens, Swiss chard, chopped, boiled, drnd",
        "Lettuce",
        "Salad, chicken Caesar, tossed, w/dressing, lrg",
        "Salad, Mandarin Medley, w/balsamic vinaigrette",
        "Shallots",
        "Spinach"
    ),
    legumes = c(
        "Beans",
        "Falafel",
        "Hummus",
        "Tofu",
        "Soy Milk"
    ),
    meat = c(
        "Bacon",
        "Beef",
        "Cheeseburger",
        "Dish, beef, tips, marinated",
        "Hamburger",
        "Lunchmeat",
        "Meatballs, chicken & turkey, Buffalo, rth",
        "Pork",
        "Salami, beef",
        "Sausage"
    ),
    poultry = c(
        "Chicken,",
        "Meatballs, chicken & turkey, Buffalo, rth",
        "Potstickers, chicken & vegetable, frz",
        "Salad, chicken Caesar, tossed, w/dressing, lrg",
        "Sandwich, chicken club, premium crispy"
    ),
    fish = c(
        "Fish"
    ),
    dairy = c(
        "Butter",
        "Cheese,",
        "Cream Cheese",
        "Cream, whipping,",
        "Milk",
        "Yogurt"
    ),
    "fried food" = c(
        "Chips, potato,",
        "French Fries",
        "Fried Chicken",
        "Nuggets, chicken, McNuggets",
        "Snack, potato, crisps"
    ),
    "bread/flour" = c(
        "Bagel",
        "Baguette",
        "Biscuit",
        "Bread",
        "Cracker",
        "Crepe, plain",
        "Macaroni & Cheese",
        "Noodles",
        "Pancakes",
        "Pasta",
        "Pilaf, rice, herb & butter, dry",
        "Tortilla,"
    ),
    "pasta w/sauce" = c(
        "Macaroni & Cheese"
    ),
    cereals = c(
        "Cereal, Cheerios, honey nut",
        "Cereal, granola, fruit & nut",
        "Cereal, granola, oats honey & raisin",
        "Cereal, Kix"
    ),
    chips = c(
        "Baking Chips",
        "Chips, tortilla,"
    ),
    bar = c(
        "Bar,"
    ),
    grains = c(
        "Barley",
        "Cereal, hot, oatmeal, apple cinnamon, inst",
        "Cereal, hot, oatmeal, quick, dry",
        "Oats,",
        "Popcorn",               # ??
        "Rice",
        "Sushi"
    ),
    "nuts/seeds" = c(
        "Chia Seeds",
        "Nut Butter, peanut, creamy",
        "Peanut Butter, creamy, organic",
        "Seeds"
    ),
    egg = c(
        "Egg"
    ),
    "oil/dressing" = c(
        "Dressing",
        "Oil"
    ),
    pizza = c(
        "Pizza",
        "Crust, pizza, classic, refrig dough"
    ),
    desserts = c(
        "Candy",
        "Cheesecake",
        "Coconut, dried, shredded, sweetened,",
        "Cookie",
        "Frozen Dessert",
        "Honey",
        "Ice Cream",
        "Jelly",
        "Muffin",
        "Syrup, maple",
        "Sugar"
    ),
    "sweetened beverages" = c(
        "Juice",
        "Soda"
    ),
    alcohol = c(
        "Beer",
        "Wine"
    ),
    "coffee/tea" = c(
        "Tea",
        "Coffee"
    )
)

# Items that are unable to categorize:
# saurce

food_data$category = sapply(food_data$`Item Name`, function(food_item){
    if(food_item == "Brussels Sprouts, fresh") return("leafy/green")
    if(grepl("Candy Bar", food_item)) return("desserts")
    if(grepl("Macaroni & Cheese", food_item)) return("bread/flour;pasta w/sauce")
    if(food_item == "Nut Butter, peanut, creamy") return("nuts/seeds")
    if(food_item == "Soy Milk") return("legumes")
    result = NULL
    for(i in 1:length(food_category_list)){
        for(category_item in food_category_list[[i]]){
            if(grepl(category_item, food_item)){
                result = c(result, names(food_category_list)[i])
            }
        }
    }
    if(is.null(result)) return(NA)
    return(paste(unique(result), collapse=";"))
})

food_data_2 = food_data %>%
    melt(id.var = c("Item Name", "category"),
         variable.name = "Subject",
         value.name = "Amt") %>%
    mutate(Subject = gsub("Baseline","",Subject))

blf_data = sapply(unique(food_data_2$Subject),function(Subj){
    sapply(names(food_category_list), function(category){
        return(
            sum(food_data_2$Amt[
                food_data_2$Subject == Subj & 
                    grepl(category,food_data_2$category)])
        )
    })
})

## -------------------- corr function ----------------------
runCorrTests = function(data, covar){
    
    pearson_list = apply(data, 1, function(yy){
        y = yy[which(!is.na(covar))]
        x = covar[!is.na(covar)]
        return(cor.test(x,y,method='pearson'))
    })
    spearman_list = apply(data, 1, function(yy){
        y = yy[which(!is.na(covar))]
        x = covar[!is.na(covar)]
        return(cor.test(x,y,method='spearman'))
    })
    kendall_list = apply(data, 1, function(yy){
        y = yy[which(!is.na(covar))]
        x = covar[!is.na(covar)]
        return(cor.test(x,y,method='kendall'))
    })
    
    pearson.r = sapply(pearson_list, function(x) x[[4]])
    pearson.p = sapply(pearson_list, function(x) x[[3]])
    spearman.rho = sapply(spearman_list, function(x) x[[4]])
    spearman.p = sapply(spearman_list,function(x) x[[3]])
    kendall.tau = sapply(kendall_list, function(x) x[[4]])
    kendall.p = sapply(kendall_list, function(x) x[[3]])
    
    out.mat = data.frame(
        pearson.r = pearson.r,
        pearson.pvalue = pearson.p,
        spearman.rho = spearman.rho,
        spearman.pvalue = spearman.p,
        kendall.tau = kendall.tau,
        kendall.pvalue = kendall.p,
        row.names = rownames(data)
    )
    return(out.mat)
}

runCorrTestsforAllVariable = function(data,covar_mat){
    out = lapply(1:ncol(covar_mat), function(i){
        fit = runCorrTests(data, covar_mat[,i])
        print(str_c(i, "    ", names(covar_mat)[i]))
        return(fit)
    })
    names(out) = colnames(covar_mat)
    return(out)
}

## --------------------- corr mcb --------------------------
# data structure:
#   blf_mcb: 
#       food1:
#           FF:
#           Med:
#           TX:

load("../Rdata/mcb_precalc.Rdata")

# first, calulate the treatment change
mcb_data_tx = lapply(edata_list, function(normal){
    lapply(normal, function(level){
        df = level %>%
            t %>% as.data.frame %>%
            mutate(
                TX = pdata$Treatment,
                Day = pdata$Timepoint,
                Subj = pdata$StudyID
            ) %>%
            melt(id.var = c("Subj","TX","Day"),
                 variable.name = "Feature",
                 value.name = "Value") %>%
            dcast(Subj + TX +Feature ~ Day,
                  value.var = "Value") %>%
            mutate(Change = Post - Pre) %>%
            dcast(Feature + Subj ~ TX, value.var = "Change") %>% 
            mutate(Mix = Med - FF) %>%
            melt(id.var = c("Feature","Subj"),
                 variable.name = "Group",
                 value.name = "Value") 
        return_list = lapply(levels(df$Group), function(TX){
            df[df$Group == TX,] %>%
                dcast(Feature ~ Subj, value.var = "Value") %>%
                column_to_rownames("Feature")
        })
        names(return_list) = levels(df$Group)
        return(return_list)
    })
})

blf_mcb_corr = lapply(mcb_data_tx, function(normal){
    lapply(normal, function(level){
        lapply(level, function(group){
            runCorrTestsforAllVariable(group, as.data.frame(t(blf_data)))
        })
    })
})

## ---------------- do the same thing to bga ---------------
df = biogenic_amines$edata %>%
    t %>% as.data.frame %>%
    mutate(
        TX = pdata$Treatment,
        Day = pdata$Timepoint,
        Subj = pdata$StudyID
    ) %>%
    melt(id.var = c("Subj","TX","Day"),
         variable.name = "Feature",
         value.name = "Value") %>%
    dcast(Subj + TX +Feature ~ Day,
          value.var = "Value") %>%
    mutate(Change = Post - Pre) %>%
    dcast(Feature + Subj ~ TX, value.var = "Change") %>% 
    mutate(Mix = Med - FF) %>%
    melt(id.var = c("Feature","Subj"),
         variable.name = "Group",
         value.name = "Value") 
bga_data_tx = lapply(levels(df$Group), function(TX){
    df[df$Group == TX,] %>%
        dcast(Feature ~ Subj, value.var = "Value") %>%
        column_to_rownames("Feature")
})
names(bga_data_tx) = levels(df$Group)

blf_bga_corr = lapply(bga_data_tx, function(group){
    runCorrTestsforAllVariable(group, as.data.frame(t(blf_data)))
})

## ----------------------- save ----------------------------
setwd("/Users/chenghaozhu/Box Sync/UC Davis/Right Now/Researches/Zivkovic Lab/Fast Food Study/Data/between_assays_analysis/mcb/Rdata/")
save(blf_data, 
     mcb_data_tx, blf_mcb_corr,
     bga_data_tx, blf_bga_corr,
     file = "blf_precalc.Rdata")
