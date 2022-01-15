library(ggforce)
library(ggplot2)

grid_size <- c(200,200)

single_circle <- function (radius) {
    x = runif(n = 1, max = grid_size[1] - radius -0.01,
              min = 0 + radius - 0.01)
    y = runif(n = 1, max = grid_size[2] - radius  -0.01,
              min = 0 + radius - 0.01)
    r = radius
    
  return(c(x = x,y = y,r = r))
}

generate_circles <- function() {
  for (i in 1:25000){
      # b <- ifelse(val < 7, runif(1), runif(1, min = 5, max = val))
      b <- runif(1, min = 0.1, max = 0.5)
      new_circle <- single_circle(runif(1, min = 1, max = 18))
      
      for (i in 1:length(values)) {
        is_circle_interesecting <- vector()
        is_circle_interesecting <- sqrt(((values[[i]][[1]] - new_circle[[1]]) ^ 2) + ((values[[i]][[2]] - new_circle[[2]]) ^ 2) ) - b > values[[i]][[3]] + new_circle[[3]]

        if (!is_circle_interesecting) {
          break
        }
        
        if (i == length(values)) {
         values[[i+1]] <<- new_circle
        }
      }
  }
}

create_layout <- function() {

values <<- vector("list")
values[[1]] <<- single_circle(1)

generate_circles()
dat <- as.data.frame(do.call(rbind, values))

circlecolors <- c("#e63946", "#f1faee", "#a8dadc", "#457b9d", "#1d3557")
dat$fillcolor <- sample(circlecolors, size = nrow(dat), replace = T)

return(dat)
}

dat <- create_layout()

ggplot(dat)+
  geom_circle(aes(x0= x, y0 =y, r =r, fill = fillcolor),size = 0.02)+
  theme_void()+
  theme(
    plot.background = element_rect(fill = "gray95", color = NA),
    panel.background = element_rect(fill = "gray95", color = NA)
  )+
  scale_fill_identity()

ggsave("odd_colors2.png", width = 12, height =12, dpi = 500)

