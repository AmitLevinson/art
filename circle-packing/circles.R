library(ggforce)
library(ggplot2)
library(magrittr)

grid_size <- c(200,200)

single_circle <- function (radius) {
    x = runif(n = 1, max = grid_size[1] - radius -0.01,
              min = 0 + radius - 0.01)
    y = runif(n = 1, max = grid_size[2] - radius  -0.01,
              min = 0 + radius - 0.01)
    r = radius
    
  return(c(x = x,y = y,r = r))
}


generate_circles <- function(val, attempts) {
  for (i in 1:attempts){
      b <- ifelse(val < 7, runif(1), runif(1, min = 5, max = val))
      new_circle <- single_circle(val)
      
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

create_layout <- function(circle_radius, seq_attempts) {

values <<- vector("list")

values[[1]] <<- single_circle(1)

mapply(FUN = generate_circles, circle_radius, seq_attempts)

dat <- as.data.frame(do.call(rbind, values))

circlecolors <- c("#d4e09b", "#f6f4d2", "#cbdfbd", "#f19c79", "#a44a3f")

dat$fillcolor <- sample(circlecolors, size = nrow(dat), replace = T)

return(dat)
}

dat <- create_layout(c(12:1), seq(10,4000,length.out = 12))

ggplot(dat)+
  geom_circle(aes(x0= x, y0 =y, r =r, fill = fillcolor),color = NA)+
  theme_void()+
  theme(
    plot.background = element_rect(fill = "gray95", color = NA),
    panel.background = element_rect(fill = "gray95", color = NA)
  )+
  scale_fill_identity()

ggsave("plot.png", width = 12, height =12, dpi = 500)
