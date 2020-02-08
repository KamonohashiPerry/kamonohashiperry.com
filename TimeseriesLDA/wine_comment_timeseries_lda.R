library(tidyverse)
library(RMeCab)
library(LDATS)

wine_rating <- read_csv(file = "TimeseriesLDA/dataset.csv")

wine_rating <- wine_rating %>% filter(!is.na(sales_catch_copy_text),
                                      !is.na(ratings),
                                      year > 1999,
                                      !is.na(commission_text))

wine_rating$sales_catch_copy_text <- gsub(pattern = "[0-9]",
                                          replacement = "数字",
                                          x = wine_rating$sales_catch_copy_text)
wine_rating$commission_text <- gsub(pattern = "[0-9]",
                                    replacement = "数字",
                                    x = wine_rating$commission_text)

wine_rating$bind_text <- paste0(wine_rating$sales_catch_copy_text ,
                                wine_rating$commission_text) 


# Bag of wordsの生成
res <- docMatrixDF(wine_rating$bind_text,minFreq=3)
res <- data.frame(res)
# View(rownames(res))

# Google Spread Sheetの=GOOGLETRANSLATE(C18,"ja","en")で変換した英語のデータを読み込む
translate_df <- read_csv(file = "TimeseriesLDA/translate.csv",col_names = FALSE)
colnames(translate_df) <- c("word_ja", "word_en")

word_translate <- data.frame(word_ja=rownames(res))
word_translate <- word_translate %>% left_join(translate_df, by="word_ja")

rownames(res) <- word_translate$word_en

word_vector <- row.names(res)
colnames(res) <- wine_rating$year

# LDATSで扱えるデータ構造を作成
for (i in 1:nrow(res)) {
  nam <- paste( word_vector[i], sep = "")
  assign(nam, as.integer(res[i, ]))
}

# ここでの変数名が可視化の際に表示される
document_term_table <- data.frame(list(sa=sa,
                                       fruits=fruits,
                                       Greatness=Greatness,
                                       Can=Can,
                                       workmanship=workmanship,
                                       taste=taste,
                                       quality=quality,
                                       Year=Year,
                                       Thenumbers=Thenumbers,
                                       Highest=Highest,
                                       fruit=fruit,
                                       Great=Great,
                                       delicate=delicate,
                                       complexity=complexity,
                                       rich=rich,
                                       past=past,
                                       fragrance=fragrance
                                       ))

# 共変量データセット
document_covariate_table <- data.frame(list(year=as.integer(colnames(res))),
                                       list(rating=as.integer(wine_rating$ratings)))

test_set <- list(document_term_table=document_term_table,
                 document_covariate_table=document_covariate_table)

# 時系列トピックモデルの実行
r_LDATS <- LDA_TS(test_set,
                  topics = 3:6, 
                  nseeds = 2,
                  formulas = ~1,  
                  nchangepoints = 1:2,
                  timename = "year")
# 対数尤度などの出力
print(r_LDATS)

# 時系列トピックモデルの可視化（先行研究に準拠）
plot(r_LDATS,binwidth = 5, xlab = "Year")

