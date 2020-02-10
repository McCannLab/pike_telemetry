#' @param x 
#' @param max_speed maximum speed for Norther Pike ref XXX
#' 
fish_speed <- function(x, max_speed = 38.5) {
    # sort 
    x <- x[order(x$datetime), ]
    # distance 
    tmp <- st_coordinates(x)
    dis <- Mod(diff(tmp[,1] + 1i*tmp[,2]))
    # time  difference 
    tim <- difftime(x$datetime, lag(x$datetime), units="min")[-1]
    x$dd = c(NA, dis)
    x$dt = c(NA, as.numeric(tim))
    x$speed = c(NA, dis/as.numeric(tim))
    #
    x$speed[x$dt > 20 | x$speed > max_speed] <- NA 
    x
}





