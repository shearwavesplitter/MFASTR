#Functions from RSEIS so that JSAC.seis (and others) will work. Details found in JSAC.seis function
#' @export
getmoday <- function (jul, iyear) 
{
    if (length(iyear) < length(jul)) {
        iyear = rep(iyear, length(jul))
    }
    inine = tojul(iyear, 1, 1)
    ijul = inine + jul - 1
    MD = fromjul(ijul, iyear)
    return(list(mo = MD$mo, dom = MD$dom))
}

#' @export
tojul <- function (year, month, day) 
{
    yy = year
    mm = month
    dd = day
    jul = 0
    flg = mm > 2
    yy[flg] = yy[flg]
    yy[!flg] = yy[!flg] - 1
    mm[flg] = mm[flg] - 3
    mm[!flg] = mm[!flg] + 9
    c = trunc(yy/100)
    ya = yy - 100 * c
    jul = trunc((146097 * c)/4) + trunc((1461 * ya)/4) + trunc((153 * 
        mm + 2)/5) + dd + 1721119
    return(jul)
}

#' @export
fromjul <- function (jul, yy) 
{
    j = jul - 1721119
    yy = trunc((4 * j - 1)/146097)
    j = 4 * j - 1 - 146097 * yy
    dd = trunc(j/4)
    j = trunc((4 * dd + 3)/1461)
    dd = 4 * dd + 3 - 1461 * j
    dd = trunc((dd + 4)/4)
    mm = trunc((5 * dd - 3)/153)
    dd = 5 * dd - 3 - 153 * mm
    dd = trunc((dd + 5)/5)
    yy = 100 * yy + j
    yy[mm < 10] = yy[mm < 10]
    yy[mm >= 10] = yy[mm >= 10] + 1
    flg = mm < 10
    mm[flg] = mm[flg] + 3
    mm[!flg] = mm[!flg] - 9
    return(list(mo = mm, dom = dd))
}

#' @export
fixcomps <- function (oldcomps, SEGY = FALSE) 
{
    if (missing(SEGY)) 
        SEGY = FALSE
    UPcomps = toupper(oldcomps)
    comps = UPcomps
    nc = nchar(comps)
    comps = substr(comps, nc, nc)
    comps[comps == "Z"] = "V"
    comps[comps == "U"] = "V"
    comps[UPcomps == "LD"] = "I"
    comps[UPcomps == "MIC"] = "I"
    if (SEGY == 456) {
        comps[comps == "4"] = "V"
        comps[comps == "5"] = "N"
        comps[comps == "6"] = "E"
    }
    if (SEGY == 123) {
        comps[comps == "1"] = "V"
        comps[comps == "2"] = "N"
        comps[comps == "3"] = "E"
    }
    return(comps)
}


#' @export
detrend <- function (x) 
{
    n = length(x)
    t = seq(1, n)
    r = lm(x ~ t)
    b = r$coefficients[1]
    m = r$coefficients[2]
    newf = x - (m * t + b)
    return(newf)
}

#' @export
jadjust.length <- function (inputdata) 
{
    if (is.character(inputdata)) 
        s <- scan(inputdata)
    else s <- inputdata
    np <- length(s)
    pow <- 1
    while (2 * pow < np) pow <- 2 * pow
    new.np <- 2 * pow
    if (np == new.np) 
        list(signal = s, length = np)
    else {
        new.s <- 1:new.np
        new.s[1:new.np] <- 0
        new.s[1:np] <- s
        list(signal = new.s, length = new.np)
    }
}

FRWDft <- function (g, n, tstart, dt) 
{
    N = length(g)
    if (length(tstart) > 1) {
        t = tstart
        tstart = t(1)
        dt = t(2) - t(1)
    }
    else {
        t = tstart + seq(from = 0, by = dt, to = (N - 1) * dt)
    }
    if (length(t) != N) {
    }
    f = makefreq(N, dt)
    i = complex(real = 0, imaginary = 1)
    G = dt * fft(g) * exp(-2 * pi * i * tstart * f)
    return(list(G = G, f = f, t = t))
}

makefreq <- function (n, dt) 
{
    if ((n%%2) == 1) {
        f = c(seq(0, (n - 1)/2), seq(-(n - 1)/2, -1))/(n * dt)
    }
    else {
        f = c(seq(0, n/2), seq(-n/2 + 1, -1))/(n * dt)
    }
    return(f)
}

