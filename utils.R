
data_temp <- function(path){
  header <- c("YEAR","MONTH", "DAY", "HOUR", "MINUTE", "SECOND", "temp")
  list_of_files <- list.files(path, recursive = TRUE, all.files = FALSE, full.names = TRUE)
  DT <- rbindlist(sapply(list_of_files, fread, simplify = FALSE),
                  use.names = TRUE, idcol = "FileName") %>%
    select(2:8)
  names(DT) <- header
  date_t <- select(DT, YEAR, MONTH, DAY, HOUR, MINUTE) %>%
    mutate(datetime = make_datetime(YEAR, MONTH, DAY, HOUR, MINUTE)) %>% 
    select(datetime)
  final_temp <- cbind(DT, date_t) %>% 
    select(datetime,temp) %>% 
    as_tibble()
  
  return(final_temp)
}

parsivelData <- function(path){
  data_parsivel <- readMat(path)
  header <- c("YEAR","MONTH", "DAY", "HOUR", "MINUTE", "SECOND", "RI", "DBZ", "SYNOP4680")
  data_parsivel <- data_parsivel$matrix.pb%>% as_tibble()
  names(data_parsivel) <- header
  final_data <- data_parsivel %>% 
    filter(YEAR != 0) %>% 
    mutate(datetime = make_datetime(YEAR, MONTH, DAY, HOUR, MINUTE)) %>% 
    select("datetime", "RI", "DBZ", "SYNOP4680") %>% 
    arrange(datetime)
  return(final_data)
}

pp_fraction <- function(ppdata){
  tempmean <- ppdata %>% 
    group_by(date_day) %>% 
    summarise(temp = mean(temp))
  
  data_rain_frac <- data_rain %>% 
    group_by(date_day, typepp) %>% 
    summarise(total = sum(RI)) %>% 
    pivot_wider(names_from = typepp, values_from = total, values_fill = 0) %>% 
    as_tibble() %>% 
    left_join(RI) %>% 
    mutate(liquid_fracc = LIQUID/total_RI, temp = tempmean$temp)
  
  return(data_rain_frac)
}

pptypes <- function(dataf){
  typespp <- dataf %>%
    mutate(date_day = as.Date(datetime, format("%Y/%m/%d"))) %>% 
    mutate(typepp = case_when(
      SYNOP4680 %in% liquidpp ~ "LIQUID", 
      SYNOP4680 %in% solidpp ~ "SOLID"))
  return(typespp)
}
