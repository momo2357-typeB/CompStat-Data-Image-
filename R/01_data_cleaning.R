#Data cleaning and visualisation

plot_fashionimage <- function(pixel_vector, title = "") {
  stopifnot(is.numeric(pixel_vector), length(pixel_vector) == 784)
  
  imag_mat <- matrix(pixel_vector, nrow = 28,byrow = TRUE)
  im_corrected <- t(imag_mat)[28:1, ]
  
  image(
    x = 1:28,
    y = 1:28,
    z = im_corrected,
    col = gray(seq(1,0,lengthout = 256)),
    axes = FALSE,
    xlab = "",
    ylab = "",
    main = title
  )
  box()
}

train_path1 <- "C:/Users/HP/Downloads/archive/fashion-mnist_train1.csv" 
train_path2 <- "C:/Users/HP/Downloads/archive/fashion-mnist_train2.csv"

raw_part1 <- read.csv(train_path1, header = TRUE)
raw_part2 <- read.csv(train_path2, header = TRUE)

stopifnot(identical(colnames(raw_part1), colnames(raw_part2)))

target_classes <- c(7,9)
sub_part1 <- raw_part1[raw_part1$label %in% target_classes, ]
sub_part2 <- raw_part2[raw_part2$label %in% target_classes, ]

rm(raw_part1, raw_part2)
gc()

shoes_train <- rbind(sub_part1, sub_part2)
rm(sub_part1, sub_part2)

stopifnot(sum(!complete.cases(shoes_train_df)) == 0)

y_train <- factor(shoes_train$label, levels = target_classes, labels = c("Sneaker", "Ankle boot"))
y_train <- relevel(y_train, ref = "Sneaker")


pixel_cols <- grep("^pixel", colnames(shoes_train_df))
X_train <- as.matrix(shoes_train_df[, pixel_cols]) / 255.0
rownames(X_train) <- NULL

mean_train_vector <- colMeans(X_train)
X_train_centered  <- scale(X_train, center = mean_train_vector, scale = FALSE)

cat(sprintf("Trainingsmenge erfolgreich vereint: N = %d Beobachtungen, p = %d Pixel\n",
            nrow(X_train), ncol(X_train)))
print(table(y_train))

