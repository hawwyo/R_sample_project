getMean <- function(folder, type, id=1:332){
  # folder - символьный вектор длины 1, который указывает расположение CSV-файлов
  # type - символьный вектор длины 1, который указывает тип загрязнителя воздуха "sulfate" или "nitrate"
  # id - целочисленный вектор, указывающий идентификаторы мониторов, которые нужно обработать
  
  # Функция должна вычислить среднее значение по указанному загрязнителю по всем мониторам указанным в векторе id, игнорируя значения NA.
  # Не используйте округление для результата	
  
  cur_sum = 0;
  cur_cnt = 0;
  for (i in id) {
    data;
    if (i < 10) {
      data = read.csv(paste(folder, "/00", i, ".csv", sep = ""));
    }
    else if (i < 100) {
      data = read.csv(paste(folder, "/0", i, ".csv", sep = ""));
    }
    else {
      data = read.csv(paste(folder, "/", i, ".csv", sep = ""));
    }
    
    cur_sum = cur_sum + sum(data[type], na.rm = TRUE);
    cur_cnt = cur_cnt + sum(!is.na(data[type]));
  }
  
  return (cur_sum / cur_cnt);
}



getCompleteObservation <- function(folder, id = 1:332) {
  # folder - символьный вектор длины 1, который указывает расположение CSV-файлов
  # id - целочисленный вектор, указывающий идентификаторы мониротов, которые нужно обработать
  
  # Функция возвращает дата фрейм вида
  #   id count
  #    1   117
  #    2   1041
  #   ...
  # где id - идентификатор монитора, count - количество полных наблюдаемых случаев для этого монитора.
  count = c();
  
  for (i in id) {
    data;
    if (i < 10) {
      data = read.csv(paste(folder, "/00", i, ".csv", sep = ""));
    }
    else if (i < 100) {
      data = read.csv(paste(folder, "/0", i, ".csv", sep = ""));
    }
    else {
      data = read.csv(paste(folder, "/", i, ".csv", sep = ""));
    }
    
    count = c(count, sum(!is.na(data["sulfate"]) & !is.na(data["nitrate"])))
  }
  
  return( data.frame(id, count) );
}






getCorrelation <- function(folder, limen = 0) {
  # folder - символьный вектор длины 1, который указывает расположение CSV-файлов
  # limen - целочисленный вектор длины 1, указывающий пороговое значение количества полных случаев, при котором нужно вычислять корреляцию.
  
  # Функция возвращает числовой вектор корреляций
  
  
  
  id <- getCompleteObservation(folder);
  
  id <- id[ id['count'] > limen, ]['id'];
  
  res <- c();
  
  for (i in id$id) {
    data;
    if (i < 10) {
      data = read.csv(paste(folder, "/00", i, ".csv", sep = ""));
    }
    else if (i < 100) {
      data = read.csv(paste(folder, "/0", i, ".csv", sep = ""));
    }
    else {
      data = read.csv(paste(folder, "/", i, ".csv", sep = ""));
    }
    
    sulf = data[!is.na(data["sulfate"]) & !is.na(data["nitrate"]),]['sulfate'];
    nitr = data[!is.na(data["sulfate"]) & !is.na(data["nitrate"]),]['nitrate'];
    
    
    
    if (nrow(sulf) > 0) {
      res <- c(res, cor(sulf, nitr));
    }
  }
  
  return(res);
}



res <- getCorrelation("data", 150)
head(res)

## [1] -0.01895754 -0.14051254 -0.04389737 -0.06815956 -0.12350667 -0.07588814

summary(res)
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## -0.21057 -0.04999  0.09463  0.12525  0.26844  0.76313

res <- getCorrelation("data", 400)
head(res)

## [1] -0.01895754 -0.04389737 -0.06815956 -0.07588814  0.76312884 -0.15782860

summary(res)

##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## -0.17623 -0.03109  0.10021  0.13969  0.26849  0.76313

res <- getCorrelation("data", 5000)
summary(res)

##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 

length(res)

## [1] 0

res <- getCorrelation("data")
summary(res)

##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## -1.00000 -0.05282  0.10718  0.13684  0.27831  1.00000

length(res)