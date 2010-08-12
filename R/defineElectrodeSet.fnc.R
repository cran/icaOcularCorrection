defineElectrodeSet.fnc <-
function(
                which.cap="biosemi.32", # easy.cap.28, easy.cap.64.partial, 
                                        # easy.cap.64.full, biosemi.32
                bottom.eye="BE"         # or NA
                ) {

  if(!which.cap%in%c("easy.cap.28","easy.cap.64.partial","easy.cap.64.full","biosemi.32"))
    stop("Error: Unknown cap\n")

  ########################### easy.cap.28 ##################################################
  if (which.cap == "easy.cap.28") {
    electrodes = c("F7",  "F3",  "Fz",  "F4",  "F8",
                   "FT7", "FC3", "FCz", "FC4", "FT8",
                   "T7",  "C3",  "Cz",  "C4",  "T8",
                   "TP7", "CP3", "CPz", "CP4", "TP8",
                   "P7",  "P3",  "Pz",  "P4",  "P8",
                          "O1",  "Oz",  "O2")
    xticks = c("n", "n", "n", "n", "n",
             "n", "n", "n", "n", "n",
             "n", "n", "n", "n", "n",
             "n", "n", "n", "n", "n",
             "s", "n", "n", "n", "s",
                "s", "s", "s")
    if(!is.na(bottom.eye)) {
      xticks[25]="n"
      xticks=c(xticks,"s")
    }
    yticks = c("s", "n", "n", "n", "n",
             "s", "n", "n", "n", "n",
             "s", "n", "n", "n", "n",
             "s", "n", "n", "n", "n",
             "s", "n", "n", "n", "n",
                "s", "n", "n")
    if(!is.na(bottom.eye)) {
      yticks=c(yticks,"n")
    }
    
    skips = rep(0, length(electrodes))
    names(skips) = electrodes
    skips["O1"] = 1
    pmatrix = matrix(NA, 6, 5)
    xcoords = c(1, 2, 3, 4, 5,
                1, 2, 3, 4, 5,
                1, 2, 3, 4, 5,
                1, 2, 3, 4, 5,
                1, 2, 3, 4, 5,
                   2, 3, 4)                
    names(xcoords)=electrodes
    ycoords = c(1, 1, 1, 1, 1,
                2, 2, 2, 2, 2,
                3, 3, 3, 3, 3,
                4, 4, 4, 4, 4,
                5, 5, 5, 5, 5,
                   6, 6, 6)                
    names(ycoords)=electrodes
  }

  ########################### easy.cap.64.partial ##################################################

  if (which.cap == "easy.cap.64.partial") {
    electrodes = paste("C", 
                 c(                 "53", "60", "21",
                                    "46", "59", "14",
                    "51",           "39",       "7",             "19",
                          "44",           "31",            "12",
                    "50",       "37",     "30",      "5",        "18",
                                          "29",
                    "49", "42",     "35",       "3",       "10", "17",
                                    "40",       "8", 
                                "47",     "27",      "15", 
                          "54",           "26",            "22"), sep="")

    xticks =     c(                  "n",  "n",  "n",
                                     "n",  "s",  "n",
                    "s",             "s",        "s",             "s",
                          "s",             "n",             "s",
                    "s",        "s",       "n",       "s",        "s",
                                           "s",
                    "s",  "s",       "n",        "n",       "s",  "s",
                                     "s",        "s", 
                                "s",       "n",       "s", 
                          "s",             "s",             "s")     
    if(!is.na(bottom.eye)) {
      xticks=c(xticks,"s")
    }

    yticks =     c(                 "s",  "n",  "n",
                                    "s",  "n",  "n",
                    "s",            "s",        "s",             "s",
                          "s",            "s",             "s",
                    "s",        "s",      "s",      "s",         "s",
                                          "s",
                    "s",  "n",      "s",        "s",       "s",  "n",
                                    "s",        "s", 
                                "s",      "s",      "s", 
                          "s",            "s",             "s")
    if(!is.na(bottom.eye)) {
      yticks=c(yticks,"n")
    }

    xcoords = c(         4, 5, 6, 
                         4, 5, 6, 
                1,       4,    6,       9,
                   2,       5,       8,
                1,    3,    5,    7,    9,
                            5,
                1, 2,    4,    6,    8, 9,
                         4,    6,
                      3,    5,    7,
                   2,       6,       8)                
    names(xcoords)=electrodes
    ycoords = c(         1, 1, 1, 
                         2, 2, 2, 
                3,       3,    3,       3,
                   4,       4,       4,
                5,    5,    5,    5,    5,
                            6,
                7, 7,    7,    7,    7, 7,
                         8,    8,
                      9,    9,    9,
                  10,      10,      10)                
    names(ycoords)=electrodes

         
    skips = rep(0, length(electrodes))
    names(skips) = electrodes
    skips["C53"] = 3
    skips["C46"] = 6
    skips["C51"] = 3
    skips["C39"] = 2
    skips["C7"]  = 1
    skips["C19"] = 2
    skips["C44"] = 1
    skips["C31"] = 2
    skips["C12"] = 2
    skips["C50"] = 1
    skips["C37"] = 1
    skips["C30"] = 1
    skips["C5"]  = 1
    skips["C18"] = 1
    skips["C29"] = 4
    skips["C49"] = 4
    skips["C35"] = 1
    skips["C3"]  = 1
    skips["C10"] = 1
    skips["C40"] = 3
    skips["C8"]  = 1
    skips["C47"] = 5
    skips["C27"] = 1
    skips["C15"] = 1
    skips["C54"] = 3
    skips["C26"] = 2
    skips["C22"] = 2
    pmatrix = matrix(NA, 10, 9)
  }

  ########################### easy.cap.64.full ##################################################

  if (which.cap == "easy.cap.64.full") {
    electrodes = paste("C", 
               c(                   "53", "60", "21",
                  "57", "52",       "46", "59", "14",       "20", "25",
                  "51",       "45", "39", "58",  "7", "13",       "19",
                        "44", "38", "34", "31",  "2",  "6", "12",
                  "50",       "37",       "30",        "5",       "18",
                        "43",       "33", "29",  "1",       "11",
                  "49", "42", "36", "35", "28",  "3",  "4", "10", "17",
                              "41", "40",        "8",  "9",
                  "55", "48", "47",       "27",       "15", "16", "23",
                        "54",             "26",             "22"), sep="")

    xticks =     c(                  "n",  "n",  "n",
                    "n",  "s",       "n",  "s",  "n",       "s",  "n",
                    "s",        "n", "n",  "n",  "n", "n",        "s",
                          "s",  "n", "s",  "n",  "s", "n",  "s",  
                    "s",        "s",       "n",       "s",        "s",
                          "n",       "n",  "n",  "n",       "n",
                    "s",  "s",  "n", "n",  "s",  "n", "n",  "s",  "s",
                                "n", "s",        "s", "n",
                    "s",  "n",  "s",       "n",       "s",  "n",  "s", 
                          "s",             "s",             "s") 
    if(!is.na(bottom.eye)) {
      xticks[54]="n"
      xticks=c(xticks,"s")
    }

    yticks =     c(                  "s",  "n",  "n",
                    "s",  "n",       "s",  "n",  "n",       "s",  "n",
                    "s",        "s", "n",  "n",  "n", "n",        "s",
                          "s",  "n", "n",  "n",  "n", "n",  "n",  
                    "s",        "s",       "s",       "s",        "s",
                          "s",       "s",  "n",  "n",       "s",
                    "s",  "n",  "n", "n",  "n",  "n", "n",  "n",  "n",
                                "s", "n",        "s", "n",
                    "s",  "n",  "n",       "s",       "s",  "n",  "n", 
                          "s",             "s",             "s")
    if(!is.na(bottom.eye)) {
      yticks=c(yticks,"n")
    }
              
    xcoords = c(         4, 5, 6, 
                1, 2,    4, 5, 6,    8, 9,
                1,    3, 4, 5, 6, 7,    9,
                   2, 3, 4, 5, 6, 7, 8,
                1,    3,    5,    7,    9,
                   2,    4, 5, 6,    8,
                1, 2, 3, 4, 5, 6, 7, 8, 9,
                      3, 4,    6, 7,
                1, 2, 3,    5,    7, 8, 9,
                   2,       6,       8)                
    names(xcoords)=electrodes
    ycoords = c(         1, 1, 1, 
                2, 2,    2, 2, 2,    2, 2,
                3,    3, 3, 3, 3, 3,    3,
                   4, 4, 4, 4, 4, 4, 4,
                5,    5,    5,    5,    5,
                   6,    6, 6, 6,    6,
                7, 7, 7, 7, 7, 7, 7, 7, 7,
                      8, 8,    8, 8,
                9, 9, 9,    9,    9, 9, 9,
                  10,      10,      10)
    names(ycoords)=electrodes

    skips = rep(0, length(electrodes))
    names(skips) = electrodes
    skips["C53"] = 3
    skips["C57"] = 3
    skips["C46"] = 1
    skips["C20"] = 1
    skips["C45"] = 1
    skips["C19"]  = 1
    skips["C44"] = 1
    skips["C50"] = 1
    skips["C37"] = 1
    skips["C30"] = 1
    skips["C5"] = 1
    skips["C18"] = 1
    skips["C43"] = 1
    skips["C33"]  = 1
    skips["C11"] = 1
    skips["C49"] = 1
    skips["C41"] = 2
    skips["C8"] = 1
    skips["C55"]  = 2
    skips["C27"] = 1
    skips["C15"] = 1
    skips["C54"]  = 1
    skips["C26"] = 2
    skips["C22"] = 2
    pmatrix = matrix(NA, 10, 9)   
  }

  ########################### biosemi.32 ##################################################

  if (which.cap == "biosemi.32") {
    electrodes = c(       "Fp1",        "Fp2",
                          "AF3",        "AF4",
                   "F7",  "F3",  "Fz",  "F4",  "F8",
                   "FC5", "FC1",        "FC2", "FC6",
                   "T7",  "C3",  "Cz",  "C4",  "T8",
                   "CP5", "CP1",        "CP2", "CP6",
                   "P7",  "P3",  "Pz",  "P4",  "P8",
                          "PO3",        "PO4",
                          "O1",  "Oz",  "O2")

    xticks     = c(       "n",        "n",
                          "n",        "n",
                   "n",   "n",  "s",  "n",  "n",
                   "n",   "n",        "n",  "n",
                   "n",   "n",  "s",  "n",  "n",
                   "n",   "n",        "n",  "n",
                   "s",   "n",  "s",  "n",  "s",
                          "n",        "n",
                          "s",  "s",  "s")
    if(!is.na(bottom.eye)) {
      xticks=c(xticks,"s")
    }

    yticks     = c(       "s",        "s",
                          "s",        "s",
                   "s",   "n",  "n",  "n",  "n",
                   "s",   "n",        "s",  "n",
                   "s",   "n",  "n",  "n",  "n",
                   "s",   "n",        "s",  "n",
                   "s",   "n",  "n",  "n",  "n",
                          "s",        "s",
                          "s",  "n",  "n")
    if(!is.na(bottom.eye)) {
      yticks=c(yticks,"n")
    }
    xcoords = c(   2,    4,   
                   2,    4,   
                1, 2, 3, 4, 5,
                1, 2,    4, 5,
                1, 2, 3, 4, 5,
                1, 2,    4, 5,
                1, 2, 3, 4, 5,
                   2,    4,                
                   2, 3, 4)                
    names(xcoords)=electrodes
    ycoords = c(   1,    1,   
                   2,    2,   
                3, 3, 3, 3, 3,
                4, 4,    4, 4,
                5, 5, 5, 5, 5,
                6, 6,    6, 6,
                7, 7, 7, 7, 7,
                   8,    8,                
                   9, 9, 9)
    names(ycoords)=electrodes

    skips = rep(0, length(electrodes))
    names(skips) = electrodes
    skips["Fp1"] = 1
    skips["Fp2"] = 1
    skips["AF3"] = 2
    skips["AF4"] = 1
    skips["F7"] = 1
    skips["FC2"] = 1
    skips["CP2"] = 1
    skips["PO3"] = 1
    skips["PO4"] = 1
    skips["O1"] = 2  
    pmatrix = matrix(NA, 9, 5)
  } 


  if(!is.na(bottom.eye)){
    electrodes = c(electrodes,bottom.eye)
    skips = c(skips,0)
    names(skips)[length(skips)]="BE"
    xcoords = c(xcoords, ncol(pmatrix))
    ycoords = c(ycoords, nrow(pmatrix))
  }

  return(list(electrodes=electrodes, xticks=xticks, yticks=yticks, 
       skips=skips, pmatrix=pmatrix, xcoords=xcoords, ycoords=ycoords, cap.name = which.cap))

}

