---

# 📘 **DTSA 5900 / 5704: Managing, Describing, and Analyzing Data**

## 🔧 Getting Started with `burrm/lolcat`

To use the course examples, first install and load the `lolcat` package (part of the `burrm` project):

```r
# Install devtools if not already installed
install.packages("devtools")

# Install the lolcat package from GitHub
devtools::install_github("burrm/lolcat")

# Load the package
library(lolcat)
```

> ✅ Use `require(lolcat)` or `library(lolcat)` at the start of each R script.

---

# 📄 **Module\_2\_Summary.R**

```r
# Module 2: Data Management, Analysis and Visualization in R
# Using the burrm/lolcat package

# Load necessary package
require(lolcat)

# General R Options
options(scipen=999) # Prevent scientific notation

# Round output helper
ro <- round.object

# ----------------------------------
# 1. Basic Data Entry and Visualization
# ----------------------------------
cfm <- c(68,72,72,74,72,69,75,75,72,73,70,71,71,72,73,72,70,72,73,74)
fans <- data.frame(cfm)
View(fans)

# Run Chart
spc.run.chart(chart.series = fans$cfm, 
              main = "Run Chart: Computer Fans",
              ylab = "CFM", 
              pch = 19, cex = 1.2, col = "blue", 
              lty = 1, lwd = 2, type = "o")
mean(fans$cfm)
abline(h = 72)  # Mean line

# ----------------------------------
# 2. Frequency Distributions
# ----------------------------------
frequency.dist.ungrouped(fans$cfm) # Frequency table for raw data
# Grouped version requires castings dataset
frequency.dist.grouped(castings$weight)

# ----------------------------------
# 3. Frequency Polygon & Histogram
# ----------------------------------
frequency.polygon.ungrouped(fans$cfm, 
                            main = "Ungrouped Freq Polygon: Fans Data", 
                            xlab = "CFM")

# Grouped versions require a grouped numeric dataset
frequency.polygon.grouped(castings$weight, ...)
hist.grouped(castings$weight, ...)

hist.ungrouped(fans$cfm, main = "Ungrouped Histogram", xlab = "CFM")

# ----------------------------------
# 4. Density Plots
# ----------------------------------
# Density curve and overlay
hist.grouped(castings$weight, freq = FALSE)
lines(density(castings$weight))

dp <- density(castings$weight)
plot(dp)
polygon(dp, col = "red", border = "black")

# ----------------------------------
# 5. Summary Statistics
# ----------------------------------
summary(castings$weight)
boxplot(castings$weight, main = "Boxplot of Casting Weight")
boxplot(weight ~ mold, data = castings3)

# ----------------------------------
# 6. Measures of Central Tendency
# ----------------------------------
weight <- c(65,67,36,37,36,57,53,39,38,58)
preform <- data.frame(weight)
View(preform)

mean(preform$weight)       # Mean
median(preform$weight)     # Median
table(preform$weight)      # Frequency Table for Mode
sample.mode(preform$weight) # Mode using lolcat

# Bimodal example
weight <- c(65,67,36,37,36,57,53,39,38,58,57)
preform <- data.frame(weight)
sample.mode(preform$weight)

# ----------------------------------
# 7. Range, IQR, SD, Variance
# ----------------------------------
range(preform$weight)
rng <- range(preform$weight)
rng[2] - rng[1]             # Range calculation

IQR(preform$weight)        # Interquartile Range
sd(preform$weight)         # Standard Deviation
ro(sd(preform$weight), 2)

var(preform$weight)        # Variance
ro(var(preform$weight), 2)

# ----------------------------------
# 8. Quantiles
# ----------------------------------
quantile(preform$weight, probs = 0.30)
quantile(preform$weight, probs = c(0.25, 0.75))

# ----------------------------------
# 9. Weighted Mean (Grouped Data)
# ----------------------------------
fdcast <- frequency.dist.grouped(castings$weight)
midpts <- fdcast$midpoint
freq <- fdcast$freq
weighted.mean(x = midpts, w = freq)

# Simple weighted mean example
wt <- c(0.2, 0.4, 0.4)
x <- c(88, 85, 92)
weighted.mean(x = x, w = wt)

# ----------------------------------
# 10. Skewness and Kurtosis
# ----------------------------------
skewness(castings$weight)
ro(skewness(castings$weight), 3)

kurtosis(castings$weight)
ro(kurtosis(castings$weight), 3)

# ----------------------------------
# 11. Descriptive Summary
# ----------------------------------
summary.continuous(castings$weight, stat.sd = TRUE)
nqtr <- function(x, d) { noquote(t(ro(x, d))) }
nqtr(summary.continuous(castings$weight), 3)

# ----------------------------------
# 12. Data Transformation and Correlation
# ----------------------------------
castnew <- transform.independent.format.to.dependent.format(fx = weight ~ mold, data = castings3)
colnames(castnew)[1:3] <- c("Mold_1", "Mold_2", "Mold_3")
cor(castnew$Mold_1, castnew$Mold_2)

plot(castnew$Mold_1, castnew$Mold_2, pch = 19, xlab = "Mold 2", ylab = "Mold 1")
abline(lm(castnew$Mold_2 ~ castnew$Mold_1), col = "blue", lwd = 2)
```

---

## Description of Key Functions (lolcat):

* `spc.run.chart()` — Run chart (SPC tool for time series).
* `frequency.dist.ungrouped()` — Tabulates frequency of ungrouped numeric values.
* `frequency.dist.grouped()` — Groups numeric data into intervals and tabulates.
* `frequency.polygon.ungrouped()` — Line graph connecting frequency of each value.
* `hist.ungrouped()` — Histogram for raw values.
* `sample.mode()` — Calculates the mode.
* `round.object()` — Rounds output for better readability.
* `summary.continuous()` — Returns extended descriptive stats (mean, median, SD, etc.).
* `transform.independent.format.to.dependent.format()` — Reshapes wide data (e.g., mold by row) into long format.

---

# 📁 `module_3_summary.R`

```r
# Module 3 Summary: Probability Distributions and Normality Testing

require(lolcat)  # Load lolcat package
nqtr <- function(x,d) { noquote(t(round.object(x, d))) }  # Utility function to round & format

# ---------------------------
# EXPONENTIAL DISTRIBUTIONS
# ---------------------------

# Area under exponential curve (right-tail)
pexp(q = 60, rate = 1/100, lower.tail = FALSE)

# Plot exponential curve and shade upper tail
x <- seq(0, 800, length=200)
y <- dexp(x, rate=1/100)
plot(x, y, type="l", main="Exponential Distribution")
x <- seq(60, 800, length=100)
y <- dexp(x, rate=1/100)
polygon(c(60, x, 800), c(0, y, 0), col="red")

# Left-tail exponential area using offset mean
pexp(q = (20 - 5), rate = 1/(50 - 5), lower.tail = TRUE)
pexp.low(q = 20, low = 5, mean = 50, lower.tail = TRUE)

# Shade left tail
x <- seq(5, 20, length=100)
y <- dexp(x, rate=1/50)
polygon(c(5, x, 20), c(0, y, 0), col="red")

# Test for exponentiality
expdata <- rexp(n = 100, rate = 1/50)
shapiro.wilk.exponentiality.test(expdata)

expdata <- rexp(n = 101, rate = 1/50)
shapetest.exp.epps.pulley.1986(expdata)  # Use this when n > 100

# ---------------------------
# NORMAL DISTRIBUTIONS
# ---------------------------

# Normal probabilities
pnorm(q = -1.6, mean = 0, sd = 1, lower.tail = TRUE)
pnorm(q = 172, mean = 180, sd = 5)

# Two-tailed probabilities
(lower <- pnorm(q = 5.15, mean = 5.20, sd = 0.05))
(upper <- pnorm(q = 5.35, mean = 5.20, sd = 0.05, lower.tail = FALSE))
(total <- lower + upper)
round.object(total * 100, 2)

# Visualize tails on normal curve
x <- seq(5, 5.45, length=200)
y <- dnorm(x, mean=5.22, sd=0.05)
plot(x, y, type="l", main="Normal Distribution with Tails")
polygon(c(5, seq(5, 5.15, length=100), 5.15), c(0, dnorm(seq(5, 5.15, length=100), 5.22, 0.05), 0), col="red")
polygon(c(5.35, seq(5.35, 5.45, length=100), 5.45), c(0, dnorm(seq(5.35, 5.45, length=100), 5.22, 0.05), 0), col="red")
abline(v = 5.22)

# Normality Tests
normdata <- rnorm(n = 24, mean = 10, sd = 2)
anderson.darling.normality.test(normdata)
shapiro.wilk.normality.test(normdata)
summary.continuous(normdata)

normdata <- rnorm(n = 25, mean = 10, sd = 2)
(normout <- dagostino.normality.omnibus.test(normdata))
(skkupvals <- c(normout$estimate[6], normout$estimate[12]))

# ---------------------------
# POISSON DISTRIBUTIONS
# ---------------------------

# Manual and built-in Poisson probability
lambda <- 25
X <- 10
(lambda^X / factorial(X)) * exp(-lambda)
dpois(x = 10, lambda = 25)

# Poisson table and barplot
round.object(table.dist.poisson(lambda = 25), 5)
data <- dpois(x = 6:50, lambda = lambda)
names(data) <- 6:50
barplot(data, xlab="Parts per Hour", ylab="P(X)", ylim=c(0, 0.10))

# Poisson cumulative probability range
(ft20 <- ppois(q = 19, lambda = 25))
(ft30 <- ppois(q = 30, lambda = 25))
ft30 - ft20

# Poisson test
poisdist <- rpois(n = 100, lambda = 25)
poisson.dist.test(poisdist)

# ---------------------------
# BINOMIAL DISTRIBUTIONS
# ---------------------------

# Manual binomial probability
p <- 0.80
q <- 0.20
r <- 45
n <- 50
factorial(n) / (factorial(r) * factorial(n - r)) * (p^r) * (q^(n - r))

# Built-in binomial
dbinom(x = 45, size = 50, prob = 0.8)

# Binomial distribution table
round.object(table.dist.binomial(n = 50, p = 0.80), 5)

# Barplot with highlighted region
data <- dbinom(x = 26:n, size = n, prob = p)
names(data) <- 26:n
cols <- rep("grey", n + 1)
cols[20:25] <- "red"
barplot(data, col=cols, xlab="# of Good Parts", ylab="P(G)", ylim=c(0, 0.15))

# Binomial upper tail
pbinom(q = 44, size = 50, prob = 0.80, lower.tail = FALSE)

# Small binomial example
table.dist.binomial(n = 2, p = 0.2)
n <- 2; P <- 0.2
data <- dbinom(x = 0:n, size = n, prob = P)
names(data) <- 0:n
barplot(data, xlab="# of Defectives", ylab="P(D)", ylim=c(0, 1))

# ---------------------------
# DISCRETE RANDOM VARIABLES
# ---------------------------

# Grouped frequency & probability distribution
(freqdistdp <- round.object(frequency.dist.grouped(Daily.Production$V1), 3))
probdistdp <- freqdistdp[, c("min", "freq", "rel.freq")]
colnames(probdistdp) <- c("Daily Production", "# of Days", "P(DP)")
View(probdistdp)

# Histogram and Expected Value
hist.grouped(Daily.Production$V1, freq=FALSE, anchor.value=50, ylim=c(0, 0.20))
x <- probdistdp$`Daily Production`
y <- probdistdp$`P(DP)`
weighted.mean(x, y)  # Expected value
mean(Daily.Production$V1)  # Alternative
```

---

### 🔍 Summary of Key Concepts Covered

* **Exponential distribution:** Probabilities and visualization with tail shading.
* **Normal distribution:** Area under curve, two-tailed probabilities, and normality testing (`shapiro`, `anderson`, `dagostino`).
* **Poisson distribution:** Manual calculation, built-in functions (`dpois`, `ppois`), probability tables and barplots.
* **Binomial distribution:** Manual and `dbinom`, tail probabilities, probability distribution barplots.
* **Discrete probability distributions:** Expected value via weighted mean, visualized as histograms.

---

# 🧾 **Module 4: Confidence Intervals & Random Sampling Distributions**

---

## 🔹 Confidence Intervals for Proportions and Poisson Counts

### ✅ **One-Sample Proportion (Exact)**

```r
proportion.test.onesample.exact.simple(
  sample.proportion = 0.12,
  sample.size = 100,
  conf.level = 0.95
)
```

* Calculates a confidence interval for a population proportion.
* Use when data follows a binomial distribution and sample size is relatively small.

### 📊 **From Data File (Proportion Example)**

```r
(prop <- mean(Point_Estimates$Proportion))
(n <- length(Point_Estimates$Proportion))
```

Then:

```r
proportion.test.onesample.exact.simple(
  sample.proportion = prop,
  sample.size = n,
  conf.level = 0.90
)
```

### ✅ **Poisson Confidence Interval**

```r
poisson.dist.test(Point_Estimates$Count)
counts <- sum(Point_Estimates$Count)
n <- length(Point_Estimates$Count)

poisson.test.onesample.simple(
  sample.count = counts,
  sample.size = n,
  conf.level = 0.90
)
```

* Use for rate-based data (e.g., counts per unit time).

---

## 🔹 Confidence Intervals for Means and Variance

### ✅ **Mean (σ Known) — Z-test**

```r
z.test.onesample.simple(
  sample.mean = 20,
  known.population.variance = 5^2,
  sample.size = 150,
  conf.level = 0.95
)
```

### ✅ **Mean (σ Unknown) — T-test**

```r
t.test.onesample.simple(
  sample.mean = 15000,
  sample.variance = 500^2,
  sample.size = 14,
  conf.level = 0.90
)
```

### ✅ **Variance and Standard Deviation**

```r
ci.var <- variance.test.onesample.simple(
  sample.variance = 10^2,
  sample.size = 25,
  conf.level = 0.95
)

ci.var$conf.int  # Variance CI
sqrt(ci.var$conf.int)  # SD CI
```

### 📊 **From File — Mean, Variance, SD**

```r
t.test.onesample(Point_Estimates$Weight, conf.level = 0.95)
summary.continuous(Point_Estimates$Weight)
```

---

## 🔹 Point Estimates

```r
mean(Point_Estimates$Weight)
sd(Point_Estimates$Weight)
mean(Point_Estimates$Proportion)
mean(Point_Estimates$Count)
```

---

## 🔹 Normal Probabilities using RSD

### ✅ **Example 1**

```r
pnorm(q = 1.433, mean = 1.325, sd = 0.045/sqrt(25), lower.tail = FALSE)
```

### ✅ **Example 2**

```r
pnorm(q = 55, mean = 50, sd = 14.4/sqrt(25), lower.tail = FALSE)
```

Use `dnorm()` and `polygon()` to draw curves and shade tail areas.

---

## 🔹 Random Sampling Distributions (Simulation)

### ✅ **Exponential Population Example**

```r
popexp <- rexp(50000, rate = 1/10)
samplesexp <- replicate(1000, rexp(100, rate = 1/10))
sampleavgsexp <- colMeans(samplesexp)
```

* Compare histograms of population vs sample means.

### ✅ **Normal Population Example**

```r
pop <- rnorm(50000, mean = 10, sd = 2)
samples <- replicate(5000, rnorm(4, mean = 10, sd = 2))
sampleavgs <- colMeans(samples)
```

* Use `layout(matrix(...))` and `hist.grouped()` for comparison plots.

---

## 🔹 Compare Multiple Samples

### ✅ **Create and Compare 4 Samples**

```r
normdata <- data.frame(
  d1 = rnorm(30, 100, 10),
  d2 = rnorm(30, 100, 10),
  d3 = rnorm(30, 100, 10),
  d4 = rnorm(30, 100, 10)
)
summary.all.variables(normdata, stat.sd = TRUE)
```

* Use `par(mfrow=c(2,2))` to visualize multiple histograms in one window.

---

Here’s a concise summary of **Module 5: Statistical Power and Two-Sample Tests**, structured by topic and tied to your code examples.

---

## 🧠 **Module 5 Summary: Statistical Power and Two-Sample Tests**

### **1. Calculating Power**

Used to estimate the probability of correctly rejecting a false null hypothesis.

#### 🔹 **Mean (One Sample)**

```r
power.mean.t.onesample(sample.size = 9, effect.size = 10, variance.est = 100, alpha = 0.05)
```

* Tests power for detecting a difference in population mean from a hypothesized mean.

#### 🔹 **Variance (One Sample)**

```r
power.variance.onesample(sample.size = 9,
                         null.hypothesis.variance = 100,
                         alternative.hypothesis.variance = 144,
                         alpha = 0.05)
```

* Used to detect changes in variance (either increase or decrease).

---

### **2. Two Independent Sample Tests for Means**

#### 🔹 **Equal Variances**

```r
t.test.twosample.independent.simple(sample.mean.g1 = ..., sample.variance.g1 = ..., ...)
```

* Assumes both groups have the same variance.
* More powerful when assumption is met.

#### 🔹 **Unequal Variances (Welch's t-test)**

```r
t.test.twosample.independent.simple(..., sample.variance.g1 = 400, sample.variance.g2 = 81)
```

* No assumption of equal variances.
* Safer for unequal spread.

---

### **3. Two Dependent Sample Tests (Paired t-test)**

Used when the same subjects are measured twice or naturally paired.

#### 🔹 **Example: Noise Data**

```r
t.test.twosample.dependent(x1 = Noise$Old, x2 = Noise$New)
```

#### 🔹 **Using Summary Data**

```r
t.test.twosample.dependent.simple.dbar(mean(Noise$Diff), var(Noise$Diff), sample.size = 10)
```

#### 🔹 **Using Sample Correlation**

```r
cor.pearson.r.onesample(x = Noise$Old, y = Noise$New)
```

---

### **4. Two Sample Tests for Variances**

Assumes normally distributed populations.

#### 🔹 **Independent Samples (F-test)**

```r
variance.test.twosample.independent.simple(...)
```

#### 🔹 **Dependent Samples (Matched Design)**

```r
variance.test.twosample.dependent.simple(...)
```

* Includes a correlation term for matched samples.

---

### **5. ISO Plot (Visual Check of Agreement)**

```r
plot(g1, g2)
lines(x = min:max, y = min:max, lwd = 2, lty = 2, col = "blue")
```

* Visualizes agreement and differences between paired data.

---

### **6. Two Sample Tests for Proportions**

#### 🔹 **Fisher’s Exact Test (Independent)**

```r
proportion.test.twosample.exact.simple(...)
```

* Accurate for small sample sizes.

#### 🔹 **McNemar’s Test (Dependent)**

```r
proportion.test.mcnemar.simple(b = 4, c = 56)
```

* Used for before/after or paired nominal data.

---

### **7. Poisson Count Tests (Rates)**

Used for comparing count data (events per unit of time, space, etc.).

#### 🔹 **Example: Eddycur Data**

```r
poisson.test.twosample.simple(sample.count.g1 = 112, sample.size.g1 = 130,
                              sample.count.g2 = 260, sample.size.g2 = 130)
```

* Tests for differences in rates (e.g., number of defects before and after a change).

---

