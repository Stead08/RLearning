#package のインストール
library(RMeCab)
library(tidyverse)
library(quanteda)
library(distrom)
url  <- 'https://s3.amazonaws.com/amazon-reviews-pds/tsv/amazon_reviews_multilingual_JP_v1_00.tsv.gz'
# Amazonのレビューデータをダウンロード
amazon_df <- read_tsv(url)

review_mecabbed <- c()
for (i in 1:nrow(amazon_df)){
  this_review <- str_replace_all(amazon_df$review_body[i], "[^一-龠ぁ-んーァ-ヶーa-zA-Z]", " ")
  mecab_output <- unlist(RMeCabC(this_review, 1))
  review_mecabbed[i] <- str_c(mecab_output[which(names(mecab_output) == "名詞")], collapse = " ")
}

review_dfm <- review_mecabbed %>%
  phrase() %>%
  tokens() %>%
  dfm() %>%
  dfm_trim(min_termfreq = 500, max_termfreq = 100000)


covars <- model.matrix(
  ~ - 1 + product_category + star_rating + helpful_votes, data = amazon_df
)

set.seed(12345)
train_id <- sample(
  1:nrow(amazon_df),
  round(nrow(amazon_df) * 0.7),
  replace = FALSE
)

cl <- makeCluster(16)
#マシンパワー使うよ
fits <- dmr(cl,
            counts = review_dfm[train_id,],
            covars = covars[train_id,],
            varweight = c(
              rep(1/20, sum(str_detect(colnames(covars), "product_category"))),
              rep(1, 2)
            ),
            verb = 1
)



B <- coef(fits)

#高評価レビューを100単語抽出
sort(B["star_rating",], decreasing = TRUE) %>%
  head(100)

#低評価レビューを100単語抽出
sort(B["star_rating",], decreasing = FALSE) %>%
  head(100)
#ユーザーがどんなレビューに役にたったかをつけるか
sort(B["helpful_votes",], decreasing = TRUE) %>%
  head(100)