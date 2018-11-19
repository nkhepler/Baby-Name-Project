state_la <- state %>% 
  filter(Gender == 'F', Name == 'Lindsay')

state_la_sum <- state_la %>% 
  group_by(State) %>%
  select(Name, state=State, Count) %>%
  summarize(total=sum(Count))

## getting total number of people per state
sum_names_state <- state %>%
  group_by(State) %>%
  select(state=State, Count) %>%
  summarize(total=sum(Count))

state_le <- state %>% 
  filter(Gender == 'F', Name == 'Lindsey')

state_le_sum <- state_le %>% 
  group_by(State) %>%
  select(Name, state=State, Count) %>%
  summarize(total=sum(Count))

prop_la <- data.frame(sum_names_state$state, (state_la_sum$total/sum_names_state$total)) %>%
  magrittr::set_colnames(c('state', 'total'))%>% 
  (function(x){
    df <- data_frame(state=x$state, prop = x$total*100)})

prop_le <- data.frame(sum_names_state$state, (state_le_sum$total/sum_names_state$total)) %>%
  magrittr::set_colnames(c('state', 'total'))%>% 
  (function(x){
    df <- data_frame(state=x$state, prop = x$total*100)})

p8 <- plot_usmap(data=prop_la, values = 'prop') + 
  scale_fill_gradient(name = 'Percentage', low='blue', high='red') + 
  labs(title='Percentage of Babies Named Lindsay By State', 
       subtitle = 'US babies born 1880-2014', 
       caption='Source: Kaggle.com') + 
  theme(
    legend.position = 'right',
    legend.title = element_text(size=11, face='bold'),
    legend.text = element_text(size=9),
    plot.title = element_text(size=16, face='bold'),
    plot.subtitle = element_text(size=13),
    plot.caption = element_text(size=9)
  )

p8

p9 <- plot_usmap(data=prop_le, values = 'prop') + 
  scale_fill_gradient(name = 'Percentage', low='blue', high='red') + 
  labs(title='Percentage of Babies Named Lindsay By State', 
       subtitle = 'US babies born 1880-2014', 
       caption='Source: Kaggle.com') + 
  theme(
    legend.position = 'right',
    legend.title = element_text(size=11, face='bold'),
    legend.text = element_text(size=9),
    plot.title = element_text(size=16, face='bold'),
    plot.subtitle = element_text(size=13),
    plot.caption = element_text(size=9)
  )

p9

## combining Nathan and Nathaniel proportions into single data frame
props_l <- data.frame(prop_la$state, prop_la$prop, prop_le$prop)
colnames(props_l) <- c('state', 'Lindsay', 'Lindsey')

## writing function that will determine which is more popular for each state
## uses numeric representation: 1==Nathan, 2==Nathaniel
nathan_or_nathaniel <- function(nathan,nathaniel){
  bigger = c()
  for (val in 1:length(nathan)){
    if (nathan[val] > nathaniel[val]){
      bigger = append(bigger, '1')
    }else{
      bigger = append(bigger, '2')
    }
  }
  return(bigger)
}

## running function and adding as new column to props data frame
linds <- nathan_or_nathaniel(props_l$Lindsay, props_l$Lindsey)
props_l$linds <- linds

## determining which is more popular by state
det_nvn <- function(non){
  one = c()
  two = c()
  for (val in non){
    if (val == '1'){
      one = append(one, 1)
    }else{
      two = append(two, 1)
    }
  }
  if (length(one) > length(two)){
    diff = (length(one) + 1) - length(two)
    return(paste0('Majority of states (including DC) prefer Lindsay (', length(one), ' states)'))
  }else{
    diff = (length(two) + 1) - length(one)
    return(paste0('Majority of states (including DC) prefer Lindsey (', length(two), ' states)'))
  }
}

which_is_more <- det_nvn(props_l$linds)

## plotting by state
p_non_l <- plot_usmap(data=props_l, values = 'linds') + 
  scale_fill_manual(name='Variation', 
                    labels = c('Lindsay', 'Lindsey'), 
                    values=c('Peach Puff', 'Tomato')) + 
  labs(title='Lindsay vs Lindsey By State', 
       subtitle = paste0('US babies born 1880-2014\n\n', which_is_more), 
       caption='Source: Kaggle.com') + 
  theme(
    legend.position = 'right',
    legend.title = element_text(face='bold', size=13),
    legend.text = element_text(size=11),
    plot.title = element_text(size=16, face='bold'),
    plot.subtitle = element_text(size=13),
    plot.caption = element_text(size=9)
  )
p_non_l

ggsave(plot=p_non_l, filename='Lindsay-vs-Lindsey-By-State.png',
       path='Baby-Name-Project/figures/explanatory_figures/',
       height=4, width=6, unit=c('in'))
