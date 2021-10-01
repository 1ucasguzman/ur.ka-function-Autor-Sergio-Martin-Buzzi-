ur.ka <- function (y, bp = 1, model = c("intercept", "trend", "both"), lag = NULL)
{
  y <- na.omit(as.vector(y))
  n <- length(y)
  model <- match.arg(model)
  if (is.null(lag))
    lag <- 0
  lag <- as.integer(lag)
  if (length(lag) > 1 || lag < 0) {
    warning("\nPlease, specify maximal number of lags for differenced series as positive integer; lag=1 is now used.")
    lag <- 1
  }
  datmat <- matrix(NA, n, lag + 3)
  if (n < ncol(datmat) + 2) {
    stop("\nInsufficient number of obeservations.")
  }
  idx <- 1:(n - 1)
  trend <- seq(1, n)
  datmat[, 1] <- y
  datmat[, 2] <- c(NA, y)[1:n]
  datmat[, 3] <- trend
  datmat <- as.data.frame(datmat)
  colnames(datmat)[1:3] <- c("y", "y.l1", "trend")
  if (lag > 0) {
    for (i in 1:lag) {
      datmat[, i + 3] <- c(rep(NA, i + 1), diff(y))[1:n]
    }
    colnames(datmat) <- c("y", "y.l1", "trend", paste("y.dl", 1:lag, sep = ""))
  }
  if (model == "intercept") {
    if(bp == 1) {
      test.reg <- lm(datmat)
      roll <- function(z) {
        du <- c(rep(0, z), rep(1, (n - z)))
        rollmat <- cbind(datmat, du)
        roll.reg <- coef(summary(lm(rollmat)))
        (roll.reg[2, 1] - 1)/roll.reg[2, 2]
      }
      roll.stat <- sapply(idx, roll)
      cval <- c(-5.34, -4.94, -4.66)
      bpoint0 <- bpoint <- which.min(roll.stat)
      teststat0 <- teststat <- roll.stat[bpoint]
      du <- c(rep(0, bpoint), rep(1, (n - bpoint)))
      testmat <- cbind(datmat, du)
      test.reg <- lm(testmat)
    }
    else if(bp == 2) {
      test.reg <- lm(datmat)
      roll <- function(z) {
        du <- c(rep(0, z), rep(1, (n - z)))
        rollmat <- cbind(datmat, du)
        roll.reg <- coef(summary(lm(rollmat)))
        (roll.reg[2, 1] - 1)/roll.reg[2, 2]
      }
      roll.stat <- sapply(idx, roll)
      bpoint0 <- bpoint <- which.min(roll.stat)
      teststat0 <- teststat <- roll.stat[bpoint]
      du1 <- du <- c(rep(0, bpoint), rep(1, (n - bpoint)))
      test.reg <- lm(datmat)
      roll <- function(z) {
        du <- c(rep(0, z), rep(1, (n - z)))
        rollmat <- cbind(datmat, du1, du)
        roll.reg <- coef(summary(lm(rollmat)))
        (roll.reg[2, 1] - 1)/roll.reg[2, 2]
      }
      roll.stat <- sapply(idx, roll)
      cval <- c(-6.16, -5.69, -5.47)
      bpoint <- which.min(roll.stat)
      bpoint0 <- c(bpoint0, bpoint)
      teststat <- roll.stat[bpoint]
      teststat0 <- c(teststat0, teststat)
      du2 <- c(rep(0, bpoint), rep(1, (n - bpoint)))
      testmat <- cbind(datmat, du1, du2)
      test.reg <- lm(testmat)
    }
    else if(bp == 3) {
      test.reg <- lm(datmat)
      roll <- function(z) {
        du <- c(rep(0, z), rep(1, (n - z)))
        rollmat <- cbind(datmat, du)
        roll.reg <- coef(summary(lm(rollmat)))
        (roll.reg[2, 1] - 1)/roll.reg[2, 2]
      }
      roll.stat <- sapply(idx, roll)
      bpoint0 <- bpoint <- which.min(roll.stat)
      teststat0 <- teststat <- roll.stat[bpoint]
      du1 <- du <- c(rep(0, bpoint), rep(1, (n - bpoint)))
      test.reg <- lm(datmat)
      roll <- function(z) {
        du <- c(rep(0, z), rep(1, (n - z)))
        rollmat <- cbind(datmat, du1, du)
        roll.reg <- coef(summary(lm(rollmat)))
        (roll.reg[2, 1] - 1)/roll.reg[2, 2]
      }
      roll.stat <- sapply(idx, roll)
      bpoint <- which.min(roll.stat)
      bpoint0 <- c(bpoint0, bpoint)
      teststat <- roll.stat[bpoint]
      teststat0 <- c(teststat0, teststat)
      du2 <- c(rep(0, bpoint), rep(1, (n - bpoint)))
      test.reg <- lm(datmat)
      roll <- function(z) {
        du <- c(rep(0, z), rep(1, (n - z)))
        rollmat <- cbind(datmat, du1, du2, du)
        roll.reg <- coef(summary(lm(rollmat)))
        (roll.reg[2, 1] - 1)/roll.reg[2, 2]
      }
      roll.stat <- sapply(idx, roll)
      cval <- c(-7.00, -6.53, -6.27)
      bpoint <- which.min(roll.stat)
      bpoint0 <- c(bpoint0, bpoint)
      teststat <- roll.stat[bpoint]
      teststat0 <- c(teststat0, teststat)
      du3 <- c(rep(0, bpoint), rep(1, (n - bpoint)))
      testmat <- cbind(datmat, du1, du2, du3)
      test.reg <- lm(testmat)
    }
    else if(bp == 4) {
      test.reg <- lm(datmat)
      roll <- function(z) {
        du <- c(rep(0, z), rep(1, (n - z)))
        rollmat <- cbind(datmat, du)
        roll.reg <- coef(summary(lm(rollmat)))
        (roll.reg[2, 1] - 1)/roll.reg[2, 2]
      }
      roll.stat <- sapply(idx, roll)
      bpoint0 <- bpoint <- which.min(roll.stat)
      teststat0 <- teststat <- roll.stat[bpoint]
      du1 <- du <- c(rep(0, bpoint), rep(1, (n - bpoint)))
      test.reg <- lm(datmat)
      roll <- function(z) {
        du <- c(rep(0, z), rep(1, (n - z)))
        rollmat <- cbind(datmat, du1, du)
        roll.reg <- coef(summary(lm(rollmat)))
        (roll.reg[2, 1] - 1)/roll.reg[2, 2]
      }
      roll.stat <- sapply(idx, roll)
      bpoint <- which.min(roll.stat)
      bpoint0 <- c(bpoint0, bpoint)
      teststat <- roll.stat[bpoint]
      teststat0 <- c(teststat0, teststat)
      du2 <- c(rep(0, bpoint), rep(1, (n - bpoint)))
      test.reg <- lm(datmat)
      roll <- function(z) {
        du <- c(rep(0, z), rep(1, (n - z)))
        rollmat <- cbind(datmat, du1, du2, du)
        roll.reg <- coef(summary(lm(rollmat)))
        (roll.reg[2, 1] - 1)/roll.reg[2, 2]
      }
      roll.stat <- sapply(idx, roll)
      bpoint <- which.min(roll.stat)
      bpoint0 <- c(bpoint0, bpoint)
      teststat <- roll.stat[bpoint]
      teststat0 <- c(teststat0, teststat)
      du3 <- c(rep(0, bpoint), rep(1, (n - bpoint)))
      test.reg <- lm(datmat)
      roll <- function(z) {
        du <- c(rep(0, z), rep(1, (n - z)))
        rollmat <- cbind(datmat, du1, du2, du3, du)
        roll.reg <- coef(summary(lm(rollmat)))
        (roll.reg[2, 1] - 1)/roll.reg[2, 2]
      }
      roll.stat <- sapply(idx, roll)
      cval <- c(-7.56, -7.10, -6.83)
      bpoint <- which.min(roll.stat)
      bpoint0 <- c(bpoint0, bpoint)
      teststat <- roll.stat[bpoint]
      teststat0 <- c(teststat0, teststat)
      du4 <- c(rep(0, bpoint), rep(1, (n - bpoint)))
      testmat <- cbind(datmat, du1, du2, du3, du4)
      test.reg <- lm(testmat)
    }
    else if(bp == 5) {
      test.reg <- lm(datmat)
      roll <- function(z) {
        du <- c(rep(0, z), rep(1, (n - z)))
        rollmat <- cbind(datmat, du)
        roll.reg <- coef(summary(lm(rollmat)))
        (roll.reg[2, 1] - 1)/roll.reg[2, 2]
      }
      roll.stat <- sapply(idx, roll)
      bpoint0 <- bpoint <- which.min(roll.stat)
      teststat0 <- teststat <- roll.stat[bpoint]
      du1 <- du <- c(rep(0, bpoint), rep(1, (n - bpoint)))
      test.reg <- lm(datmat)
      roll <- function(z) {
        du <- c(rep(0, z), rep(1, (n - z)))
        rollmat <- cbind(datmat, du1, du)
        roll.reg <- coef(summary(lm(rollmat)))
        (roll.reg[2, 1] - 1)/roll.reg[2, 2]
      }
      roll.stat <- sapply(idx, roll)
      bpoint <- which.min(roll.stat)
      bpoint0 <- c(bpoint0, bpoint)
      teststat <- roll.stat[bpoint]
      teststat0 <- c(teststat0, teststat)
      du2 <- c(rep(0, bpoint), rep(1, (n - bpoint)))
      test.reg <- lm(datmat)
      roll <- function(z) {
        du <- c(rep(0, z), rep(1, (n - z)))
        rollmat <- cbind(datmat, du1, du2, du)
        roll.reg <- coef(summary(lm(rollmat)))
        (roll.reg[2, 1] - 1)/roll.reg[2, 2]
      }
      roll.stat <- sapply(idx, roll)
      bpoint <- which.min(roll.stat)
      bpoint0 <- c(bpoint0, bpoint)
      teststat <- roll.stat[bpoint]
      teststat0 <- c(teststat0, teststat)
      du3 <- c(rep(0, bpoint), rep(1, (n - bpoint)))
      test.reg <- lm(datmat)
      roll <- function(z) {
        du <- c(rep(0, z), rep(1, (n - z)))
        rollmat <- cbind(datmat, du1, du2, du3, du)
        roll.reg <- coef(summary(lm(rollmat)))
        (roll.reg[2, 1] - 1)/roll.reg[2, 2]
      }
      roll.stat <- sapply(idx, roll)
      bpoint <- which.min(roll.stat)
      bpoint0 <- c(bpoint0, bpoint)
      teststat <- roll.stat[bpoint]
      teststat0 <- c(teststat0, teststat)
      du4 <- c(rep(0, bpoint), rep(1, (n - bpoint)))
      test.reg <- lm(datmat)
      roll <- function(z) {
        du <- c(rep(0, z), rep(1, (n - z)))
        rollmat <- cbind(datmat, du1, du2, du3, du4, du)
        roll.reg <- coef(summary(lm(rollmat)))
        (roll.reg[2, 1] - 1)/roll.reg[2, 2]
      }
      roll.stat <- sapply(idx, roll)
      cval <- c(-8.25, -7.64, -7.40)
      bpoint <- which.min(roll.stat)
      bpoint0 <- c(bpoint0, bpoint)
      teststat <- roll.stat[bpoint]
      teststat0 <- c(teststat0, teststat)
      du5 <- c(rep(0, bpoint), rep(1, (n - bpoint)))
      testmat <- cbind(datmat, du1, du2, du3, du4, du5)
      test.reg <- lm(testmat)
    }
  }
  else if (model == "trend") {
    if(bp == 1) {
      test.reg <- lm(datmat)
      roll <- function(z) {
        dt <- c(rep(0, z), 1:(n - z))
        rollmat <- cbind(datmat, dt)
        roll.reg <- coef(summary(lm(rollmat)))
        (roll.reg[2, 1] - 1)/roll.reg[2, 2]
      }
      roll.stat <- sapply(idx, roll)
      cval <- c(-5.01, -4.50, -4.14)
      bpoint0 <- bpoint <- which.min(roll.stat)
      teststat0 <- teststat <- roll.stat[bpoint]
      dt <- c(rep(0, bpoint), 1:(n - bpoint))
      testmat <- cbind(datmat, dt)
      test.reg <- lm(testmat)
    }
    else if(bp == 2) {
      test.reg <- lm(datmat)
      roll <- function(z) {
        dt <- c(rep(0, z), 1:(n - z))
        rollmat <- cbind(datmat, dt)
        roll.reg <- coef(summary(lm(rollmat)))
        (roll.reg[2, 1] - 1)/roll.reg[2, 2]
      }
      roll.stat <- sapply(idx, roll)
      bpoint0 <- bpoint <- which.min(roll.stat)
      teststat0 <- teststat <- roll.stat[bpoint]
      dt1 <- dt <- c(rep(0, bpoint), 1:(n - bpoint))
      test.reg <- lm(datmat)
      roll <- function(z) {
        dt <- c(rep(0, z), 1:(n - z))
        rollmat <- cbind(datmat, dt1, dt)
        roll.reg <- coef(summary(lm(rollmat)))
        (roll.reg[2, 1] - 1)/roll.reg[2, 2]
      }
      roll.stat <- sapply(idx, roll)
      cval <- c(-5.62, -5.10, -4.78)
      bpoint <- which.min(roll.stat)
      bpoint0 <- c(bpoint0, bpoint)
      teststat <- roll.stat[bpoint]
      teststat0 <- c(teststat0, teststat)
      dt2 <- c(rep(0, bpoint), 1:(n - bpoint))
      testmat <- cbind(datmat, dt1, dt2)
      test.reg <- lm(testmat)
    }
    else if(bp == 3) {
      test.reg <- lm(datmat)
      roll <- function(z) {
        dt <- c(rep(0, z), 1:(n - z))
        rollmat <- cbind(datmat, dt)
        roll.reg <- coef(summary(lm(rollmat)))
        (roll.reg[2, 1] - 1)/roll.reg[2, 2]
      }
      roll.stat <- sapply(idx, roll)
      bpoint0 <- bpoint <- which.min(roll.stat)
      teststat0 <- teststat <- roll.stat[bpoint]
      dt1 <- dt <- c(rep(0, bpoint), 1:(n - bpoint))
      test.reg <- lm(datmat)
      roll <- function(z) {
        dt <- c(rep(0, z), 1:(n - z))
        rollmat <- cbind(datmat, dt1, dt)
        roll.reg <- coef(summary(lm(rollmat)))
        (roll.reg[2, 1] - 1)/roll.reg[2, 2]
      }
      roll.stat <- sapply(idx, roll)
      bpoint <- which.min(roll.stat)
      bpoint0 <- c(bpoint0, bpoint)
      teststat <- roll.stat[bpoint]
      teststat0 <- c(teststat0, teststat)
      dt2 <- c(rep(0, bpoint), 1:(n - bpoint))
      test.reg <- lm(datmat)
      roll <- function(z) {
        dt <- c(rep(0, z), 1:(n - z))
        rollmat <- cbind(datmat, dt1, dt2, dt)
        roll.reg <- coef(summary(lm(rollmat)))
        (roll.reg[2, 1] - 1)/roll.reg[2, 2]
      }
      roll.stat <- sapply(idx, roll)
      cval <- c(-6.29, -5.73, -5.43)
      bpoint <- which.min(roll.stat)
      bpoint0 <- c(bpoint0, bpoint)
      teststat <- roll.stat[bpoint]
      teststat0 <- c(teststat0, teststat)
      dt3 <- c(rep(0, bpoint), 1:(n - bpoint))
      testmat <- cbind(datmat, dt1, dt2, dt3)
      test.reg <- lm(testmat)
    }
    else if(bp == 4) {
      test.reg <- lm(datmat)
      roll <- function(z) {
        dt <- c(rep(0, z), 1:(n - z))
        rollmat <- cbind(datmat, dt)
        roll.reg <- coef(summary(lm(rollmat)))
        (roll.reg[2, 1] - 1)/roll.reg[2, 2]
      }
      roll.stat <- sapply(idx, roll)
      bpoint0 <- bpoint <- which.min(roll.stat)
      teststat0 <- teststat <- roll.stat[bpoint]
      dt1 <- dt <- c(rep(0, bpoint), 1:(n - bpoint))
      test.reg <- lm(datmat)
      roll <- function(z) {
        dt <- c(rep(0, z), 1:(n - z))
        rollmat <- cbind(datmat, dt1, dt)
        roll.reg <- coef(summary(lm(rollmat)))
        (roll.reg[2, 1] - 1)/roll.reg[2, 2]
      }
      roll.stat <- sapply(idx, roll)
      bpoint <- which.min(roll.stat)
      bpoint0 <- c(bpoint0, bpoint)
      teststat <- roll.stat[bpoint]
      teststat0 <- c(teststat0, teststat)
      dt2 <- c(rep(0, bpoint), 1:(n - bpoint))
      test.reg <- lm(datmat)
      roll <- function(z) {
        dt <- c(rep(0, z), 1:(n - z))
        rollmat <- cbind(datmat, dt1, dt2, dt)
        roll.reg <- coef(summary(lm(rollmat)))
        (roll.reg[2, 1] - 1)/roll.reg[2, 2]
      }
      roll.stat <- sapply(idx, roll)
      bpoint <- which.min(roll.stat)
      bpoint0 <- c(bpoint0, bpoint)
      teststat <- roll.stat[bpoint]
      teststat0 <- c(teststat0, teststat)
      dt3 <- c(rep(0, bpoint), 1:(n - bpoint))
      test.reg <- lm(datmat)
      roll <- function(z) {
        dt <- c(rep(0, z), 1:(n - z))
        rollmat <- cbind(datmat, dt1, dt2, dt3, dt)
        roll.reg <- coef(summary(lm(rollmat)))
        (roll.reg[2, 1] - 1)/roll.reg[2, 2]
      }
      roll.stat <- sapply(idx, roll)
      cval <- c(-6.86, -6.31, -6.00)
      bpoint <- which.min(roll.stat)
      bpoint0 <- c(bpoint0, bpoint)
      teststat <- roll.stat[bpoint]
      teststat0 <- c(teststat0, teststat)
      dt4 <- c(rep(0, bpoint), 1:(n - bpoint))
      testmat <- cbind(datmat, dt1, dt2, dt3, dt4)
      test.reg <- lm(testmat)
    }
    else if(bp == 5) {
      test.reg <- lm(datmat)
      roll <- function(z) {
        dt <- c(rep(0, z), 1:(n - z))
        rollmat <- cbind(datmat, dt)
        roll.reg <- coef(summary(lm(rollmat)))
        (roll.reg[2, 1] - 1)/roll.reg[2, 2]
      }
      roll.stat <- sapply(idx, roll)
      bpoint0 <- bpoint <- which.min(roll.stat)
      teststat0 <- teststat <- roll.stat[bpoint]
      dt1 <- dt <- c(rep(0, bpoint), 1:(n - bpoint))
      test.reg <- lm(datmat)
      roll <- function(z) {
        dt <- c(rep(0, z), 1:(n - z))
        rollmat <- cbind(datmat, dt1, dt)
        roll.reg <- coef(summary(lm(rollmat)))
        (roll.reg[2, 1] - 1)/roll.reg[2, 2]
      }
      roll.stat <- sapply(idx, roll)
      bpoint <- which.min(roll.stat)
      bpoint0 <- c(bpoint0, bpoint)
      teststat <- roll.stat[bpoint]
      teststat0 <- c(teststat0, teststat)
      dt2 <- c(rep(0, bpoint), 1:(n - bpoint))
      test.reg <- lm(datmat)
      roll <- function(z) {
        dt <- c(rep(0, z), 1:(n - z))
        rollmat <- cbind(datmat, dt1, dt2, dt)
        roll.reg <- coef(summary(lm(rollmat)))
        (roll.reg[2, 1] - 1)/roll.reg[2, 2]
      }
      roll.stat <- sapply(idx, roll)
      bpoint <- which.min(roll.stat)
      bpoint0 <- c(bpoint0, bpoint)
      teststat <- roll.stat[bpoint]
      teststat0 <- c(teststat0, teststat)
      dt3 <- c(rep(0, bpoint), 1:(n - bpoint))
      test.reg <- lm(datmat)
      roll <- function(z) {
        dt <- c(rep(0, z), 1:(n - z))
        rollmat <- cbind(datmat, dt1, dt2, dt3, dt)
        roll.reg <- coef(summary(lm(rollmat)))
        (roll.reg[2, 1] - 1)/roll.reg[2, 2]
      }
      roll.stat <- sapply(idx, roll)
      bpoint <- which.min(roll.stat)
      bpoint0 <- c(bpoint0, bpoint)
      teststat <- roll.stat[bpoint]
      teststat0 <- c(teststat0, teststat)
      dt4 <- c(rep(0, bpoint), 1:(n - bpoint))
      test.reg <- lm(datmat)
      roll <- function(z) {
        dt <- c(rep(0, z), 1:(n - z))
        rollmat <- cbind(datmat, dt1, dt2, dt3, dt4, dt)
        roll.reg <- coef(summary(lm(rollmat)))
        (roll.reg[2, 1] - 1)/roll.reg[2, 2]
      }
      roll.stat <- sapply(idx, roll)
      cval <- c(-7.40, -6.72, -6.42)
      bpoint <- which.min(roll.stat)
      bpoint0 <- c(bpoint0, bpoint)
      teststat <- roll.stat[bpoint]
      teststat0 <- c(teststat0, teststat)
      dt5 <- c(rep(0, bpoint), 1:(n - bpoint))
      testmat <- cbind(datmat, dt1, dt2, dt3, dt4, dt5)
      test.reg <- lm(testmat)
    }
  }
  else if (model == "both") {
    if(bp == 1) {
      test.reg <- lm(datmat)
      roll <- function(z) {
        du <- c(rep(0, z), rep(1, (n - z)))
        dt <- c(rep(0, z), 1:(n - z))
        rollmat <- cbind(datmat, du, dt)
        roll.reg <- coef(summary(lm(rollmat)))
        (roll.reg[2, 1] - 1)/roll.reg[2, 2]
      }
      roll.stat <- sapply(idx, roll)
      cval <- c(-5.70, -5.08, -4.82)
      bpoint0 <- bpoint <- which.min(roll.stat)
      teststat0 <- teststat <- roll.stat[bpoint]
      du <- c(rep(0, bpoint), rep(1, (n - bpoint)))
      dt <- c(rep(0, bpoint), 1:(n - bpoint))
      testmat <- cbind(datmat, du, dt)
      test.reg <- lm(testmat)
    }
    else if(bp == 2) {
      test.reg <- lm(datmat)
      roll <- function(z) {
        du <- c(rep(0, z), rep(1, (n - z)))
        dt <- c(rep(0, z), 1:(n - z))
        rollmat <- cbind(datmat, du, dt)
        roll.reg <- coef(summary(lm(rollmat)))
        (roll.reg[2, 1] - 1)/roll.reg[2, 2]
      }
      roll.stat <- sapply(idx, roll)
      bpoint0 <- bpoint <- which.min(roll.stat)
      teststat0 <- teststat <- roll.stat[bpoint]
      du1 <- du <- c(rep(0, bpoint), rep(1, (n - bpoint)))
      dt1 <- dt <- c(rep(0, bpoint), 1:(n - bpoint))
      test.reg <- lm(datmat)
      roll <- function(z) {
        du <- c(rep(0, z), rep(1, (n - z)))
        dt <- c(rep(0, z), 1:(n - z))
        rollmat <- cbind(datmat, du1, du, dt1, dt)
        roll.reg <- coef(summary(lm(rollmat)))
        (roll.reg[2, 1] - 1)/roll.reg[2, 2]
      }
      roll.stat <- sapply(idx, roll)
      cval <- c(-6.59, -6.11, -5.85)
      bpoint <- which.min(roll.stat)
      bpoint0 <- c(bpoint0, bpoint)
      teststat <- roll.stat[bpoint]
      teststat0 <- c(teststat0, teststat)
      du2 <- c(rep(0, bpoint), rep(1, (n - bpoint)))
      dt2 <- c(rep(0, bpoint), 1:(n - bpoint))
      testmat <- cbind(datmat, du1, du2, dt1, dt2)
      test.reg <- lm(testmat)
    }
    else if(bp == 3) {
      test.reg <- lm(datmat)
      roll <- function(z) {
        du <- c(rep(0, z), rep(1, (n - z)))
        dt <- c(rep(0, z), 1:(n - z))
        rollmat <- cbind(datmat, du, dt)
        roll.reg <- coef(summary(lm(rollmat)))
        (roll.reg[2, 1] - 1)/roll.reg[2, 2]
      }
      roll.stat <- sapply(idx, roll)
      bpoint0 <- bpoint <- which.min(roll.stat)
      teststat0 <- teststat <- roll.stat[bpoint]
      du1 <- du <- c(rep(0, bpoint), rep(1, (n - bpoint)))
      dt1 <- dt <- c(rep(0, bpoint), 1:(n - bpoint))
      test.reg <- lm(datmat)
      roll <- function(z) {
        du <- c(rep(0, z), rep(1, (n - z)))
        dt <- c(rep(0, z), 1:(n - z))
        rollmat <- cbind(datmat, du1, du, dt1, dt)
        roll.reg <- coef(summary(lm(rollmat)))
        (roll.reg[2, 1] - 1)/roll.reg[2, 2]
      }
      roll.stat <- sapply(idx, roll)
      bpoint <- which.min(roll.stat)
      bpoint0 <- c(bpoint0, bpoint)
      teststat <- roll.stat[bpoint]
      teststat0 <- c(teststat0, teststat)
      du2 <- c(rep(0, bpoint), rep(1, (n - bpoint)))
      dt2 <- c(rep(0, bpoint), 1:(n - bpoint))
      test.reg <- lm(datmat)
      roll <- function(z) {
        du <- c(rep(0, z), rep(1, (n - z)))
        dt <- c(rep(0, z), 1:(n - z))
        rollmat <- cbind(datmat, du1, du2, du, dt1, dt2, dt)
        roll.reg <- coef(summary(lm(rollmat)))
        (roll.reg[2, 1] - 1)/roll.reg[2, 2]
      }
      roll.stat <- sapply(idx, roll)
      cval <- c(-7.40, -7.01, -6.69)
      bpoint <- which.min(roll.stat)
      bpoint0 <- c(bpoint0, bpoint)
      teststat <- roll.stat[bpoint]
      teststat0 <- c(teststat0, teststat)
      du3 <- c(rep(0, bpoint), rep(1, (n - bpoint)))
      dt3 <- c(rep(0, bpoint), 1:(n - bpoint))
      testmat <- cbind(datmat, du1, du2, du3, dt1, dt2, dt3)
      test.reg <- lm(testmat)
    }
    else if(bp == 4) {
      test.reg <- lm(datmat)
      roll <- function(z) {
        du <- c(rep(0, z), rep(1, (n - z)))
        dt <- c(rep(0, z), 1:(n - z))
        rollmat <- cbind(datmat, du, dt)
        roll.reg <- coef(summary(lm(rollmat)))
        (roll.reg[2, 1] - 1)/roll.reg[2, 2]
      }
      roll.stat <- sapply(idx, roll)
      bpoint0 <- bpoint <- which.min(roll.stat)
      teststat0 <- teststat <- roll.stat[bpoint]
      du1 <- du <- c(rep(0, bpoint), rep(1, (n - bpoint)))
      dt1 <- dt <- c(rep(0, bpoint), 1:(n - bpoint))
      test.reg <- lm(datmat)
      roll <- function(z) {
        du <- c(rep(0, z), rep(1, (n - z)))
        dt <- c(rep(0, z), 1:(n - z))
        rollmat <- cbind(datmat, du1, du, dt1, dt)
        roll.reg <- coef(summary(lm(rollmat)))
        (roll.reg[2, 1] - 1)/roll.reg[2, 2]
      }
      roll.stat <- sapply(idx, roll)
      bpoint <- which.min(roll.stat)
      bpoint0 <- c(bpoint0, bpoint)
      teststat <- roll.stat[bpoint]
      teststat0 <- c(teststat0, teststat)
      du2 <- c(rep(0, bpoint), rep(1, (n - bpoint)))
      dt2 <- c(rep(0, bpoint), 1:(n - bpoint))
      test.reg <- lm(datmat)
      roll <- function(z) {
        du <- c(rep(0, z), rep(1, (n - z)))
        dt <- c(rep(0, z), 1:(n - z))
        rollmat <- cbind(datmat, du1, du2, du, dt1, dt2, dt)
        roll.reg <- coef(summary(lm(rollmat)))
        (roll.reg[2, 1] - 1)/roll.reg[2, 2]
      }
      roll.stat <- sapply(idx, roll)
      bpoint <- which.min(roll.stat)
      bpoint0 <- c(bpoint0, bpoint)
      teststat <- roll.stat[bpoint]
      teststat0 <- c(teststat0, teststat)
      du3 <- c(rep(0, bpoint), rep(1, (n - bpoint)))
      dt3 <- c(rep(0, bpoint), 1:(n - bpoint))
      test.reg <- lm(datmat)
      roll <- function(z) {
        du <- c(rep(0, z), rep(1, (n - z)))
        dt <- c(rep(0, z), 1:(n - z))
        rollmat <- cbind(datmat, du1, du2, du3, du, dt1, dt2, dt3, dt)
        roll.reg <- coef(summary(lm(rollmat)))
        (roll.reg[2, 1] - 1)/roll.reg[2, 2]
      }
      roll.stat <- sapply(idx, roll)
      cval <- c(-8.24, -7.74, -7.43)
      bpoint <- which.min(roll.stat)
      bpoint0 <- c(bpoint0, bpoint)
      teststat <- roll.stat[bpoint]
      teststat0 <- c(teststat0, teststat)
      du4 <- c(rep(0, bpoint), rep(1, (n - bpoint)))
      dt4 <- c(rep(0, bpoint), 1:(n - bpoint))
      testmat <- cbind(datmat, du1, du2, du3, du4, dt1, dt2, dt3, dt4)
      test.reg <- lm(testmat)
    }
    else if(bp == 5) {
      test.reg <- lm(datmat)
      roll <- function(z) {
        du <- c(rep(0, z), rep(1, (n - z)))
        dt <- c(rep(0, z), 1:(n - z))
        rollmat <- cbind(datmat, du, dt)
        roll.reg <- coef(summary(lm(rollmat)))
        (roll.reg[2, 1] - 1)/roll.reg[2, 2]
      }
      roll.stat <- sapply(idx, roll)
      bpoint0 <- bpoint <- which.min(roll.stat)
      teststat0 <- teststat <- roll.stat[bpoint]
      du1 <- du <- c(rep(0, bpoint), rep(1, (n - bpoint)))
      dt1 <- dt <- c(rep(0, bpoint), 1:(n - bpoint))
      test.reg <- lm(datmat)
      roll <- function(z) {
        du <- c(rep(0, z), rep(1, (n - z)))
        dt <- c(rep(0, z), 1:(n - z))
        rollmat <- cbind(datmat, du1, du, dt1, dt)
        roll.reg <- coef(summary(lm(rollmat)))
        (roll.reg[2, 1] - 1)/roll.reg[2, 2]
      }
      roll.stat <- sapply(idx, roll)
      bpoint <- which.min(roll.stat)
      bpoint0 <- c(bpoint0, bpoint)
      teststat <- roll.stat[bpoint]
      teststat0 <- c(teststat0, teststat)
      du2 <- c(rep(0, bpoint), rep(1, (n - bpoint)))
      dt2 <- c(rep(0, bpoint), 1:(n - bpoint))
      test.reg <- lm(datmat)
      roll <- function(z) {
        du <- c(rep(0, z), rep(1, (n - z)))
        dt <- c(rep(0, z), 1:(n - z))
        rollmat <- cbind(datmat, du1, du2, du, dt1, dt2, dt)
        roll.reg <- coef(summary(lm(rollmat)))
        (roll.reg[2, 1] - 1)/roll.reg[2, 2]
      }
      roll.stat <- sapply(idx, roll)
      bpoint <- which.min(roll.stat)
      bpoint0 <- c(bpoint0, bpoint)
      teststat <- roll.stat[bpoint]
      teststat0 <- c(teststat0, teststat)
      du3 <- c(rep(0, bpoint), rep(1, (n - bpoint)))
      dt3 <- c(rep(0, bpoint), 1:(n - bpoint))
      test.reg <- lm(datmat)
      roll <- function(z) {
        du <- c(rep(0, z), rep(1, (n - z)))
        dt <- c(rep(0, z), 1:(n - z))
        rollmat <- cbind(datmat, du1, du2, du3, du, dt1, dt2, dt3, dt)
        roll.reg <- coef(summary(lm(rollmat)))
        (roll.reg[2, 1] - 1)/roll.reg[2, 2]
      }
      roll.stat <- sapply(idx, roll)
      bpoint <- which.min(roll.stat)
      bpoint0 <- c(bpoint0, bpoint)
      teststat <- roll.stat[bpoint]
      teststat0 <- c(teststat0, teststat)
      du4 <- c(rep(0, bpoint), rep(1, (n - bpoint)))
      dt4 <- c(rep(0, bpoint), 1:(n - bpoint))
      test.reg <- lm(datmat)
      roll <- function(z) {
        du <- c(rep(0, z), rep(1, (n - z)))
        dt <- c(rep(0, z), 1:(n - z))
        rollmat <- cbind(datmat, du1, du2, du3, du4, du, dt1, dt2, dt3, dt4, dt)
        roll.reg <- coef(summary(lm(rollmat)))
        (roll.reg[2, 1] - 1)/roll.reg[2, 2]
      }
      roll.stat <- sapply(idx, roll)
      cval <- c(-9.04, -8.34, -8.02)
      bpoint <- which.min(roll.stat)
      bpoint0 <- c(bpoint0, bpoint)
      teststat <- roll.stat[bpoint]
      teststat0 <- c(teststat0, teststat)
      du5 <- c(rep(0, bpoint), rep(1, (n - bpoint)))
      dt5 <- c(rep(0, bpoint), 1:(n - bpoint))
      testmat <- cbind(datmat, du1, du2, du3, du4, du5, dt1, dt2, dt3, dt4, dt5)
      test.reg <- lm(testmat)
    }
  }
  list( lag = lag, teststats = teststat0, teststatmin = min(teststat0),
        cval = cval, bpoints = bpoint0,
        testreg = test.reg, test.name = "Kapetanios")
}
