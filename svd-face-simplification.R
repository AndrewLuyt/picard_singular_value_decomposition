# Singular value decomposition is used to create successively better
# approximations of a small greyscale image.
#
# Only works with square images.

library(png)
colors = colorRampPalette(c("#000011", "#FFDDDD")) # slightly more interesting than Black&White

# displays a matrix as an image, in the correct orientation and aspect ratio
showImage <- function(img, label="") {
    # asp=1 forces 1x1 aspect ratio for 'pixels'
    image(t(img)[, ncol(img):1], col = colors(255),
          main = label,
          asp = 1,
          )
}

# Display an approximation of the image using n principal components.
# face: the SVD of the image.
# n: # of principal components to use in the approximation.
approxFace <- function(face, n=2) {
    explainedVariance = round(cumsum(face$d**2 / sum(face$d**2))[n], 6) * 100
    lbl = ifelse(n == ncol(face$u),
                 yes = paste("Original", n),
                 no = paste("PC", n, ", ", explainedVariance, "%"))
    # This is where the important matrix operations happen.
    showImage(
        face$u[, 1:n] %*% diag(face$d[1:n]) %*% t(face$v[, 1:n]),
        label = lbl
    )

}

# load("face.rda")  # 32x32, loads into faceData
faceData <- readPNG("picard_128x128.png")
faceData <- faceData[, , 1]

# summary(prcomp(faceData))

# create svd. Scaling seems to make no difference - these are all pixel values,
# after all, and the 'features' are just columns of pixels, not different 'things'
# measured on different scales.
s <- svd(faceData)

# Diagnostic: plot the cumulative variance explained, by principal component.
# Mark off what components break the cumulative 99% and 99.9% barriers.
# NB: tiny face image is 32x32 and has 32 PCs
#     Picard is 128x128 and has 128 PCs.
par(mfrow=c(1,1))
explained <- cumsum(s$d**2 / sum(s$d**2))
plot(explained, type='l',
     xlab = 'Principal Component', main = 'Cumulative Explained Variance')
index = min(which(explained >= .99))
abline(v = index, col='blue', lty=2)
text(index, .99, index, cex=2)
index = min(which(explained >= .999))
abline(v = index, col='red', lty=2)
text(index, .999, index, cex=2)

# using 1st 2 PCs
approxFace(s, 2)

# using 1st 5 PCs
approxFace(s, 5)

# compare across PC: meant for the Picard image.
png("picard_SVD.png", width = 708, height = 970)  # uncomment to save the output to disk
par(mfrow=c(3,2))
for(i in c(2, 10, 21, 48, 96, ncol(s$u))) {
    approxFace(s, i)
}
dev.off()

par(mfrow=c(1,1))
