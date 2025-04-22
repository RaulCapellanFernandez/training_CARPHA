

# 3. Aggregating the data
aggregate_data <- function(infile,  gender=NULL, age=NULL, itime, ftime, timetype = "NOTI", tunit = "week",  sunit = "muni", sloc = "all", spacetype= "N",crit="both", amazon_file){
  
  #check data requirements
  if (missing(ftime) | missing (itime) | missing(infile)){
    stop("You are missing a required variable")
  } 
  stopifnot("`gender` must be a 'F','M', 'I' or 'all'." = (gender %in% c('F','M', 'I','all')))
  stopifnot("`age` must be a vector of numbers." = is.numeric(age) | missing(age))   
  stopifnot("`itime` must be a year." = is.numeric(itime)) 
  stopifnot("`ftime` must be a year." = is.numeric(ftime)) 
  stopifnot("`tunit` must be either 'week', 'month', 'year'." = (tunit %in% c("week","month","year")))
  stopifnot("`sunit` must be either 'MUNI', 'UF'." = (sunit %in% c("MUNI", "UF")))
  stopifnot("`spacetype` must be a year." = (spacetype %in% c("N", "R", "H", "I")))
  
  #load file
  data <- infile
  
  #get only desired gender
  if (!is.null(gender)) {
    if (!("SEX" %in% colnames(data))){
      print("WARNING: The data will not be aggregated by gender because column 'SEX' is missing.")
    } else if (gender %in% c("F","M","I")) {
      data <- data[data["SEX"] == gender,]
    } else if (gender != "all"){
      print("WARNING: The data will not be aggregated by gender because the provided gender is incorrect.")
    }
  }
  
  #get moment of time
  # ttype <- hash(keys=c("sym", "noti"), values=c("SYM","NOTI"))
  ttype <- setNames(c("SYM","NOTI"), c("sym", "noti"))
  
  #select temporal boundaries
  data <- data[which(data[,paste0("DT_",ttype[[timetype]])] >= paste0(itime,"-01-01") & 
                       data[,paste0("DT_",ttype[[timetype]])] <= paste0(ftime, "-12-31")),]
  
  #get type of data
  # stype <- hash(keys=c("N", "R", "H", "I"), values=c("NOTI","RESI","HOSP","INF"))
  stype <- setNames(c("NOTI","RESI","HOSP","INF"), c("N", "R", "H", "I"))
  
  # select spatial boundaries
  if (sloc == "amazon") {
    am_muni <- read.csv(amazon_file)
    if (sunit == "MUNI") {data <- data[which(data[,paste0("MUNI_",stype[[spacetype]])] %in% am_muni[,1]),]
    } else if (sunit == "UF") {print("The legal amazonian can only be used for municipalities (for now)")}
  } else if (sloc != "all") {data <- data[which(data[,paste0(sunit, "_",stype[[spacetype]])] %in% sloc),]}
  
  # select confirmation criteria
  if (!is.null(crit)){
    data <- data[data$CRIT2 == "Confirmed" | data$CRIT2 == "Suspected" | data$CRIT2 == "Negative",]
  }
  
  # add respective timesteps, depending on choosen time unit
  if (tunit == "week") {data$T_STEP <- format(as.Date(data[[paste0("DT_",ttype[[timetype]])]]), "%V-%G") 
  } else if (tunit == "month") {data$T_STEP <- format(as.Date(data[[paste0("DT_",ttype[[timetype]])]]), "%m-%Y") 
  } else if (tunit == "year") {data$T_STEP <- format(as.Date(data[[paste0("DT_",ttype[[timetype]])]]), "%Y") }
  
  # add age group
  if (!is.null(age)) {
    if (!("AGE" %in% colnames(data))){
      print("WARNING: The data will not be aggregated by age because column 'AGE' is missing.")
    } else if (is.numeric(age)) {
      age_groups <- c()
      for (i in 1:length(age)-1) {age_groups[i] <- paste0(age[i],"-",age[i+1])}
      data$AGE_GRP <- cut(data$AGE, breaks=age, labels = age_groups)
    } else if (age != "all"){
      print("WARNING: The data will not be aggregated by age because the provided ages are incorrect.")
    }
  }
  
  #prepare labels for output
  collabels <- c("area_code","t_step")
  if (!(is.null(age))) {collabels <- append(collabels, "age_grp")}
  if (!(is.null(gender))) {collabels <- append(collabels, "gender")}
  if (!(is.null(crit))) {collabels <- append(collabels, "crit")}
  
  #group together?  
  data <- as.data.frame(do.call(table, c(list(data[[paste0(sunit,"_",stype[[spacetype]])]]),
                                         list(data[["T_STEP"]]),
                                         list(data[["AGE_GRP"]])[!(is.null(age))],
                                         list(data[["SEX"]] )[!(is.null(gender))],
                                         list(data[["CRIT2"]] )[!(is.null(crit))],
                                         list(dnn= collabels))),
                        responseName = "freq")

  return(data)
}

# 4. Plot a map
plot_map <- function(infile, popdata, UF_shp, muni_shp, type = "abs", incidence=1000, age=NULL, gender=NULL, timestep){
  
  # check data requirements
  if ( missing(infile) | missing(UF_shp) & missing(muni_shp)){
    stop("You are missing a required variable")
  }   
  stopifnot("`type` must be either 'abs' (absolute values) or 'inc'(incidence)." = (type %in% c("abs","inc"))) 
  stopifnot("`incidence` must be a number." = is.numeric(incidence)) 
  
  
  #load data
  #data <- read.dbf(infile)
  data <- infile
  
  #select correct age group
  if (!is.null(age)) {
    if ("age_grp" %in% colnames(data)) {
      if (age %in% unique(data$age_grp)){
        data <- data[data["age_grp"] == age,]
      } else {
        stop(paste0("The provided age group is not contained in the dataset, try: ", paste(unique(data$age_grp),collapse=" ")))
      }
    } else {
      stop("There are no age groups contained in the data.")
    }
  }
  
  #select correct sex
  if (!is.null(gender)) {
    if ("gender" %in% colnames(data)) {
      if  (all(gender %in% unique(data$gender))){
        data <- data[data["gender"] %in% gender,]
      } else{
        stop(paste0("The provided gender is not contained in the dataset, try: ", paste(unique(data$gender),collapse=" ")))
      }
    } else {
      stop("The provided gender is not contained in the data.")
    }
  }
  
  if (all(timestep %in% unique(data$t_step))){
    data<- data[data$t_step %in% timestep,]
    data <- aggregate(list(freq=data$freq), by=list(area_code=data$area_code), FUN=sum) #############!!!!!!!!!!!!!!!!!!
  }
  
  #add incidence
  if (type == "inc"){
    pop <- popdata
    data <-  left_join(data, pop)
    data$inci <- as.integer(data$freq / data$pop*incidence)
  }

  #add spatial aspect
  shp <- st_as_sf(muni_shp)
  shp <- shp[,"NAME_1"]
  names(shp)[1] <- 'area_code'
  data <- left_join(shp,data)

  #set colors for plot
  cols <- c("green", "yellow","orange","red","darkred", "black")

  #plot
  if (type == "abs") {
    plot <- ggplot(data) +
      geom_sf( aes(fill=freq), linewidth=.15, show.legend = TRUE) +
      scale_fill_stepsn(breaks = seq(0, 100, length=6),#max(data$freq, na.rm=T), length=6),
                        colours = cols,
                        values= scales::rescale(seq(0,  100, length=6)))+#max(data$freq, na.rm=T), length=6)))+
      labs(fill="Number of cases")
  } else {
    plot <- ggplot(data) +
      geom_sf( aes(fill=inci), linewidth=.15, show.legend = TRUE) +
      scale_fill_stepsn(breaks = seq(0,  max(data$inci, na.rm=T), length=6),
                        colours = cols,
                        values= scales::rescale(seq(0,  max(data$inci, na.rm=T), length=6)))+
      labs(fill="Incidence of cases")
  }
  # if (type == "abs") {
  #   plot <- ggplot(data) +
  #     geom_sf( aes(fill=freq), linewidth=.15, show.legend = TRUE) +
  #     scale_fill_stepsn(breaks = seq(0, max(data$freq[is.finite(data$freq)]), length=6),
  #                       colours = cols,
  #                       values= scales::rescale(seq(0, max(data$freq[is.finite(data$freq)]), length=6)))+
  #     labs(fill="Number of cases")
  # } else {
  #   plot <- ggplot(data) +
  #     geom_sf( aes(fill=inci), linewidth=.15, show.legend = TRUE) +
  #     scale_fill_stepsn(breaks = seq(0, max(data$inci[is.finite(data$inci)]), length=6),
  #                       colours = cols,
  #                       values= scales::rescale(seq(0, max(data$inci[is.finite(data$inci)]), length=6)))+
  #     labs(fill="Incidence of cases")
  # }
  
  return(plot)
}

# 5. Plot a timeseries
plot_ts <- function(infile, comparison, area=NULL, timestep=NULL, age=NULL, gender=NULL, title=NULL, case=NULL){
  
  #load data
  data <- infile
  
  # select area
  if (!is.null(area)){
    if (all(area %in% unique(data$area_code ))){
      data<- data[data$area_code %in% area,]
    } else {
      print(paste0("Your defined area codes (",paste(area, collapse=", "),") do not match with the area codes in the data (",paste(unique(data$area_code), collapse = ", "),")"))
    }
  }
  
  # select timestep
  if (!is.null(timestep)){
    if (all(timestep %in% unique(data$t_step))){
      data<- data[data$t_step %in% timestep,]
    } else {
      print(paste0("Your defined timesteps (",paste(timestep, collapse=", "),") do not match with the time steps in the data (",paste(unique(data$t_step), collapse = ", "),")"))
    }
  }
  
  #select age group
  if (!is.null(age)){
    if (!("age_grp" %in% colnames(data))){
      print("WARNING: The data will not be aggregated by age because column 'age_grp' is missing.")
    } else if (all(age %in% unique(data$age_grp ))){
      data<- data[data$age_grp %in% age,]
    } else {
      print(paste0("Your defined age groups (",paste(area, collapse=", "),") do not match with the age groups in the data (",paste(unique(data$age_grp), collapse = ", "),")"))
    }
  }
  
  
  # select gender
  if (!is.null(gender)){
    if (!("gender" %in% colnames(data))){
      print("WARNING: The data will not be aggregated by gender because column 'gender' is missing.")
    } else if (all(gender %in% unique(data$gender ))){
      data<- data[data$gender %in% gender,]
    } else {
      print(paste0("Your defined genders (",paste(gender, collapse=", "),") do not match with the genders in the data (",paste(unique(data$gender), collapse = ", "),")"))
    }
  }
  
  # select correct "disease case"
  if (!is.null(case)){
    if (!("crit" %in% colnames(data))){
      print("WARNING: The data will not be aggregated by disease case because column 'crit' is missing.")
    } else if (all(case %in% unique(data$crit ))){
      data<- data[data$crit %in% case,]
    } else {
      print(paste0("Your defined cases (",paste(case, collapse=", "),") do not match with the cases in the data (",paste(unique(data$crit), collapse = ", "),")"))
    }
  }
  
  
  #aggregate
  # ctype <- hash(keys=c("L", "A", "G", "C"), values=c("area_code","age_grp","gender","crit"))
  ctype <- setNames(c("area_code","age_grp","gender","crit"), c("L", "A", "G", "C"))
  # ctype2 <- hash(keys=c("L", "A", "G", "C"), values=c("","Age groups","Gender","Case"))
  ctype2 <- setNames(c("","Age groups","Gender","Case"), c("L", "A", "G", "C"))
  
  if (comparison %in% c("L","A","G","N","C")){
    by <- list(t_step=data$t_step)
    if(comparison %in% c("L","A","G", "C")){
      by <- append(by, list(comp=data[[ctype[[comparison]]]]))
    } else if (comparison== "N"){
      comp <- 1
    }
  } else {
    print(paste0("Your input for comparison (", comparison,") is not valid. It has to be 'L','A','G', 'C' or 'N'."))
  }
  data <- aggregate(list(freq=data$freq), 
                    by=by, 
                    FUN=sum)
  
  #plot
  plot <- ggplot(data) +
    geom_line(aes(x=t_step, y=freq, group=comp, color=comp), linewidth=1.5) +
    xlab("Time")+
    ylab("Number of cases")
  labs(title=title, color=ctype2[[comparison]])

  return(plot)
}

