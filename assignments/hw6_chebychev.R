f.exp <- function(k){
    if (k <= 1){
        return (1 + exp(-(1+k)) - exp(k-1))
    }
    else {
        return (exp(-(1+k)))
    }
}

f.unif <- function(k){
    if (k < sqrt(3)){
        return (1 - 2*k/sqrt(12))
    }
    else {
        return (0)
    }
}

f.cheby <- function(k) 1/k^2

k <- seq(0, 5, length.out=100)
plot(k, f.cheby(k), pch=20, col=1, ylim=c(0, 5), typ='b')
points(k, sapply(k, f.unif), pch=2, col=2)
lines(k, sapply(k, f.unif), pch=20, col=2)
points(k, sapply(k, f.exp), pch=4, col=3)
lines(k, sapply(k, f.exp), pch=20, col=3)
legend('topright', c('Chebychev', 'Uniform', 'Exponential'), col=1:3, pch=c(20,2,4))
