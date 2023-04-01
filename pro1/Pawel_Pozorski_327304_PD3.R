### Przetwarzanie Danych Ustrukturyzowanych 2023L
### Praca domowa nr. 3
###
### UWAGA:
### nazwy funkcji oraz ich parametrow powinny pozostac niezmienione.
###
### Wskazane fragmenty kodu przed wyslaniem rozwiazania powinny zostac
### zakomentowane
###

# -----------------------------------------------------------------------------#
# Wczytanie danych oraz pakietow.
# !!! Przed wyslaniem zakomentuj ten fragment
# -----------------------------------------------------------------------------#

# library("sqldf")
# library("dplyr")
# library("data.table")
# library("microbenchmark")

# Posts <- read.csv("travel_stackexchange_com/Posts.csv")
# Comments <- read.csv("travel_stackexchange_com/Comments.csv")
# Users <- read.csv("travel_stackexchange_com/Users.csv")

# -----------------------------------------------------------------------------#
# Zadanie 1
# -----------------------------------------------------------------------------#

sql_1 <- function(Users) {
  # wykonujemy zapytanie sql przez sqldf i zwracamy
  sqldf("
      SELECT Location, SUM(UpVotes) as TotalUpVotes FROM Users
      WHERE Location != ''
      GROUP BY Location
      ORDER BY TotalUpVotes DESC LIMIT 10
    ")
}

base_1 <- function(Users) {
  # bierzemy tylko te gdzie lokalizacja nie jest pusta
  idxs <- Users$Location != ""
  # grupujemy ilosc UpVotes po Lokalizacji
  s <- aggregate(
    Users$UpVotes[idxs],
    Users[idxs, c("Location"),
      drop = FALSE
    ],
    sum
  )
  colnames(s)[2] <- "TotalUpVotes"
  # sortujemy malejaco po TotalUpVotes, bierzemy najwieksze 10
  s <- s[order(s[, c("TotalUpVotes")], decreasing = TRUE)[1:10], ]
  # resetujemy indeks
  rownames(s) <- NULL
  # zwracamy wynik
  return(s)
}

dplyr_1 <- function(Users) {
  s <- Users %>%
    # bierzemy tylko te gdzie lokalizacja nie jest pusta
    filter(Location != "") %>%
    # interesuja nas tylko te 2 kolumny
    select(Location, UpVotes) %>%
    # grupujemy po lokalizacji
    group_by(Location) %>%
    # dla kazdej ze znalezionych lokalizacji zliczamy sume UpVotes
    reframe(TotalUpVotes = sum(UpVotes, na.rm = TRUE)) %>%
    # sortujemy calosc po TotalUpVotes malejaco
    arrange(-TotalUpVotes) %>%
    # zwracamy pierwsze 10 wejsc
    slice_head(n = 10)
  # zmieniamy s na data.frame i zwracamy
  return(as.data.frame(s))
}

table_1 <- function(Users) {
  # tworzymy z Users data.table
  s <- data.table(Users)
  # bierzemy tylko te gdzie lokalizacja nie jest pusta,
  # jedynie interesujące nas kolumny Location i UpVotes
  s <- s[!(Location == ""), .(Location, UpVotes)]
  # grupujemy po lokalizacji i liczymy sumę UpVotes
  s <- s[, .(TotalUpVotes = sum(UpVotes, na.rm = TRUE)), by = Location]
  # sortujemy calosc po TotalUpVotes malejaco
  setorder(s, cols = -"TotalUpVotes")
  s <- head(s, 10)
  # aby kazda funkcja zwracala to samo zmieniamy s na data.frame
  return(as.data.frame(s))
}

# Sprawdzenie rownowaznosci wynikow - zakomentuj te czesc przed wyslaniem

# zrobimy w tym celu bardziej uniwersalna funkcje
# ktora potem bedzie uzywana w sprawdzaniu
# rownowaznosci wynikow pozostalych zadan
# compare_calls <- function(sql_call_score, base_call_score,
#                           dplyr_call_score, table_call_score) {
#   c1 <- all_equal(sql_call_score, base_call_score)
#   c2 <- all_equal(base_call_score, dplyr_call_score)
#   c3 <- all_equal(dplyr_call_score, table_call_score)
#   # zwraca TRUE tylko jak wszystkie TRUE
#   return (isTRUE(c1) && isTRUE(c2) && isTRUE(c3))
# }

# compare_calls(
#   sql_1(Users),
#   base_1(Users),
#   dplyr_1(Users),
#   table_1(Users)
# )

# Porowanie czasow wykonania - zakomentuj te czesc przed wyslaniem
# microbenchmark(
#   sql_call_score=sql_1(Users),
#   base_call_score=base_1(Users),
#   dplyr_call_score=dplyr_1(Users),
#   table_call_score=table_1(Users),
#   times=10
# )

# -----------------------------------------------------------------------------#
# Zadanie 2
# -----------------------------------------------------------------------------#

sql_2 <- function(Posts) {
  # wykonujemy zapytanie sql przez sqldf i zwracamy
  sqldf("
      SELECT
        STRFTIME('%Y', CreationDate) AS Year,
        STRFTIME('%m', CreationDate) AS Month,
        COUNT(*) AS PostsNumber,
        MAX(Score) AS MaxScore
      FROM Posts
      WHERE PostTypeId IN (1, 2)
      GROUP BY Year, Month HAVING PostsNumber > 1000
    ")
}

base_2 <- function(Posts) {
  # interesuje nas tylko PostTypeId 1 lub 2
  idxs <- Posts$PostTypeId == 1 | Posts$PostTypeId == 2
  # tworzymy data.frame s zawierajaca 3 dalej nas
  # interesujace kolumny - Score, Year, Month
  s <- Posts[idxs, "Score", drop = FALSE]
  # zaczynamy od pobrania z daty roku i miesiaca
  # konwertujemy tekstowa date na objekt R
  date <- as.POSIXct(Posts$CreationDate[idxs])
  # bierzemy rok z daty i zapisujemy jako nowa kolumne do s
  s["Year"] <- format(date, format = "%Y")
  # bierzemy miesiac z daty i zapisujemy jako nowa kolumne do s
  s["Month"] <- format(date, format = "%m")
  # grupujemy po roku, nastepnie miesiacu i zbieramy liczbe wejsc
  s1 <- aggregate(s$Score, s[, c("Year", "Month")], length)
  colnames(s1)[3] <- "PostsNumber"
  # potem nas interesuja tylko te co maja PostsNumber > 1000
  idxs <- s1$PostsNumber > 1000
  # grupujemy po roku, nastepnie miesiacu i bierzemy max ze Score
  s2 <- aggregate(s$Score, s[, c("Year", "Month")], max)
  colnames(s2)[3] <- "MaxScore"
  # dodajemy MaxScore do s1)
  s1["MaxScore"] <- s2$MaxScore
  # wybieramy tylko te wejscia co maja PostsNumber > 1000
  s1 <- s1[idxs, ]
  # resetujemy indeks
  rownames(s1) <- NULL
  # zwracamy wynik
  return(s1)
}

dplyr_2 <- function(Posts) {
  s <- Posts %>%
    # interesuje nas tylko PostTypeId 1 lub 2
    filter(PostTypeId == 1 | PostTypeId == 2) %>%
    # dołączamy date
    mutate(
      Year = format(as.POSIXct(CreationDate), format = "%Y"),
      Month = format(as.POSIXct(CreationDate), format = "%m")
    ) %>%
    # dalej nas interesują juz tylko Year, Month, Score
    select(Year, Month, Score) %>%
    # grupujemy po roku, nastepnie miesiacu
    group_by(Year, Month) %>%
    # zbieramy liczbe wejsc i max ze Score
    reframe(PostsNumber = length(Score), MaxScore = max(Score, na.rm = TRUE)) %>%
    # nas interesuja tylko te co maja PostsNumber > 1000
    filter(PostsNumber > 1000)
  # zmieniamy s na data.frame i zwracamy
  return(as.data.frame(s))
}

table_2 <- function(Posts) {
  # tworzymy z Posts data.table
  s <- data.table(Posts)
  # interesuje nas tylko PostTypeId 1 lub 2, pracujemy z CreationDate i Score
  s <- s[PostTypeId == 1 | PostTypeId == 2, .(CreationDate, Score)]
  # dolaczamy date podobnie jak wczesniej i usuwamy CreationDate
  date <- as.POSIXct(s$CreationDate)
  yr <- format(date, format = "%Y") # bierzemy rok z daty
  mm <- format(date, format = "%m") # bierzemy miesiac z daty
  s <- s[, ":="(Year = yr, Month = mm, CreationDate = NULL)]
  # grupujemy po roku, nastepnie miesiacu i zbieramy liczbe wejsc i max ze Score
  s <- s[,
    .(PostsNumber = length(Score), MaxScore = max(Score, na.rm = TRUE)),
    by = .(Year, Month)
  ]
  # nas interesuja tylko te co maja PostsNumber > 1000
  s <- s[PostsNumber > 1000]
  # zmieniamy s na data.frame
  return(as.data.frame(s))
}

# Sprawdzenie rownowaznosci wynikow - zakomentuj te czesc przed wyslaniem
# compare_calls(
#   sql_2(Posts),
#   base_2(Posts),
#   dplyr_2(Posts),
#   table_2(Posts)
# )

# Porowanie czasow wykonania - zakomentuj te czesc przed wyslaniem
# microbenchmark(
#   sql_call_score=sql_2(Posts),
#   base_call_score=base_2(Posts),
#   dplyr_call_score=dplyr_2(Posts),
#   table_call_score=table_2(Posts),
#   times=10
# )

# -----------------------------------------------------------------------------#
# Zadanie 3
# -----------------------------------------------------------------------------#

sql_3 <- function(Posts, Users) {
  # wykonujemy zapytanie sql przez sqldf i zwracamy
  sqldf("
      SELECT Id, DisplayName, TotalViews FROM (
        SELECT OwnerUserId, SUM(ViewCount) as TotalViews FROM Posts
        WHERE PostTypeId = 1
        GROUP BY OwnerUserId
      ) AS Questions JOIN Users
      ON Users.Id = Questions.OwnerUserId ORDER BY TotalViews DESC
      LIMIT 10
    ")
}

base_3 <- function(Posts, Users) {
  #------ wewnetrzna kwarenda
  # interesują nas wejscia gdzie PostTypeId==1
  # dalej pracujemy tylko z OwnerUserId i ViewCount
  s1 <- Posts[Posts$PostTypeId == 1, c("OwnerUserId", "ViewCount")]
  # sumujemy ViewCount po OwnerUserId
  s1 <- aggregate(
    s1$ViewCount, s1[, c("OwnerUserId"), drop = FALSE],
    sum,
    na.rm = TRUE
  )
  colnames(s1)[2] <- "TotalViews"
  #------ zewnetrzna kwarenda
  # laczymy tabele
  s2 <- merge(Users, s1, by.x = "Id", by.y = "OwnerUserId")
  # sortujemy po TotalViews i bierzemy 10 najmniejszych
  s2 <- s2[order(s2[, c("TotalViews")], decreasing = TRUE)[1:10], ]
  # resetujemy indeks
  rownames(s2) <- NULL
  # zwracamy interesujące nas kolumny
  return(s2[, c("Id", "DisplayName", "TotalViews")])
}

dplyr_3 <- function(Posts, Users) {
  #------ wewnetrzna kwarenda
  s1 <- Posts %>%
    # interesują nas wejscia gdzie PostTypeId==1
    filter(PostTypeId == 1) %>%
    # dalej pracujemy  tylko z OwnerUserId, ViewCount
    select(OwnerUserId, ViewCount) %>%
    # grupujemy po OwnerUserId
    group_by(OwnerUserId) %>%
    # sumujemy ViewCount jako TotalViews
    reframe(TotalViews = sum(ViewCount, na.rm = TRUE))
  # wrzucamy wynik pierwszej kwarendy w data.frame
  s1 <- as.data.frame(s1)
  #------ zewnetrzna kwarenda
  s2 <- Users %>%
    # laczymy tabele Users z s1
    inner_join(s1, join_by(Id == OwnerUserId), na_matches = "never") %>%
    # sortujemy po TotalViews
    arrange(-TotalViews) %>%
    # bierzemy pierwsze 10
    slice_head(n = 10) %>%
    # zostawiamy tylko Id, DisplayName, TotalViews
    select(Id, DisplayName, TotalViews)
  # zmieniamy s na data.frame i zwracamy
  return(as.data.frame(s2))
}

table_3 <- function(Posts, Users) {
  #------ wewnetrzna kwarenda
  s1 <- data.table(Posts)
  # interesują nas wejscia gdzie PostTypeId==1
  # dalej pracujemy tylko z OwnerUserId i ViewCount
  s1 <- s1[PostTypeId == 1, .(OwnerUserId, ViewCount)]
  # sumujemy ViewCount po OwnerUserId
  s1 <- s1[, .(TotalViews = sum(ViewCount, na.rm = TRUE)), by = OwnerUserId]
  #------ zewnetrzna kwarenda
  # laczymy tabele
  s2 <- na.omit(data.table(Users)[s1, on = .(Id = OwnerUserId)])
  # sortujemy po TotalViews i bierzemy 10 najmniejszych
  setorder(s2, -TotalViews)
  # zwracamy interesujące nas kolumny jako data.frame
  return(as.data.frame(s2[1:10, .(Id, DisplayName, TotalViews)]))
}

# Sprawdzenie rownowaznosci wynikow - zakomentuj te czesc przed wyslaniem
# compare_calls(
#   sql_3(Posts, Users),
#   base_3(Posts, Users),
#   dplyr_3(Posts, Users),
#   table_3(Posts, Users)
# )

# Porowanie czasow wykonania - zakomentuj te czesc przed wyslaniem
# microbenchmark(
#   sql_call_score=sql_3(Posts, Users),
#   base_call_score=base_3(Posts, Users),
#   dplyr_call_score=dplyr_3(Posts, Users),
#   table_call_score=table_3(Posts, Users),
#   times=1
# )

# -----------------------------------------------------------------------------#
# Zadanie  4
# -----------------------------------------------------------------------------#

sql_4 <- function(Posts, Users) {
  # wykonujemy zapytanie sql przez sqldf i zwracamy
  sqldf("
      SELECT
        DisplayName,
        QuestionsNumber,
        AnswersNumber,
        Location,
        Reputation,
        UpVotes,
        DownVotes
      FROM (
        SELECT * FROM
        (
          SELECT COUNT(*) as AnswersNumber, OwnerUserId FROM Posts
          WHERE PostTypeId = 2
          GROUP BY OwnerUserId
        )
        AS Answers JOIN
        (
          SELECT COUNT(*) as QuestionsNumber, OwnerUserId FROM Posts
          WHERE PostTypeId = 1
          GROUP BY OwnerUserId
        )
        AS Questions
        ON Answers.OwnerUserId = Questions.OwnerUserId
        WHERE AnswersNumber > QuestionsNumber
        ORDER BY AnswersNumber DESC
        LIMIT 5
      ) AS PostsCounts JOIN Users
      ON PostsCounts.OwnerUserId = Users.Id
    ")
}

base_4 <- function(Posts, Users) {
  #------ pierwsza z najbardziej wewnetrznych kawrend
  # wybieramy tylko PostTypeId=2
  s1 <- Posts[Posts$PostTypeId == 2, ]
  # grupujemy po OwnerUserId i zliczamy ile wystapien
  s1 <- aggregate(
    s1$OwnerUserId,
    s1[, c("OwnerUserId"),
      drop = FALSE
    ],
    length
  )
  colnames(s1)[2] <- "AnswersNumber"
  #------ druga z najbardziej wewnetrznych kwarend
  # wybieramy tylko PostTypeId=1
  s2 <- Posts[Posts$PostTypeId == 1, ]
  # grupujemy po OwnerUserId i zliczamy ile wystapien
  s2 <- aggregate(
    s2$OwnerUserId,
    s2[, c("OwnerUserId"),
      drop = FALSE
    ],
    length
  )
  colnames(s2)[2] <- "QuestionsNumber"
  #------ wewnetrzna kwarende ze srodka
  # laczymy wyniki dwoch poprzednich kwarend
  s3 <- merge(s1, s2, by.x = "OwnerUserId", by.y = "OwnerUserId")
  # zostawiamy tylko te gdzie AnswersNumber > QuestionsNumber
  s3 <- s3[s3$AnswersNumber > s3$QuestionsNumber, ]
  # sortujemy po AnswersNumber i zostawiamy tylko 5 najwiekszych
  s3 <- s3[order(s3[, c("AnswersNumber")], decreasing = TRUE), ][1:5, ]
  # resetujemy indeks
  rownames(s3) <- NULL
  #------ najbardziej zewnetrzna kwarenda
  # laczymy wyniki poprzedniej kwarendy z tabela Users
  s4 <- merge(s3, Users, by.x = "OwnerUserId", by.y = "Id")
  # zwracamy to co nas interesuje
  return(s4[, c(
    "DisplayName",
    "QuestionsNumber",
    "AnswersNumber",
    "Location",
    "Reputation",
    "UpVotes",
    "DownVotes"
  )])
}

dplyr_4 <- function(Posts, Users) {
  #------ pierwsza z najbardziej wewnetrznych kawrend
  s1 <- Posts %>%
    # wybieramy tylko PostTypeId==2
    filter(PostTypeId == 2) %>%
    # grupujemy po OwnerUserId
    group_by(OwnerUserId) %>%
    # zliczamy ile wystapien
    reframe(AnswersNumber = length(OwnerUserId))
  #------ druga z najbardziej wewnetrznych kwarend
  s2 <- Posts %>%
    # wybieramy tylko PostTypeId==1
    filter(PostTypeId == 1) %>%
    # grupujemy po OwnerUserId
    group_by(OwnerUserId) %>%
    # zliczamy ile wystapien
    reframe(QuestionsNumber = length(OwnerUserId))
  #------ wewnetrzna kwarende ze srodka
  # laczymy wyniki dwoch poprzednich kwarend, pomijamy na
  s3 <- inner_join(
    s1, s2,
    join_by(OwnerUserId == OwnerUserId),
    na_matches = "never"
  ) %>%
    # zostawiamy tylko te gdzie AnswersNumber > QuestionsNumber
    filter(AnswersNumber > QuestionsNumber) %>%
    # sortujemy po AnswersNumber
    arrange(-AnswersNumber) %>%
    # zostawiamy tylko 5
    slice_head(n = 5)
  #------ najbardziej zewnetrzna kwarenda
  # laczymy wyniki poprzedniej kwarendy z tabela Users
  s4 <- inner_join(
    s3, Users,
    join_by(OwnerUserId == Id),
    na_matches = "never"
  ) %>%
    # zostawiamy to co nas interesuje
    select(
      DisplayName,
      QuestionsNumber,
      AnswersNumber,
      Location,
      Reputation,
      UpVotes,
      DownVotes
    )
  # zmieniamy s na data.frame i zwracamy
  return(as.data.frame(s4))
}

table_4 <- function(Posts, Users) {
  #------ pierwsza z najbardziej wewnetrznych kawrend
  # wybieramy tylko PostTypeId==2
  # grupujemy po OwnerUserId i zliczamy ile wystapien
  s1 <- data.table(Posts)[PostTypeId == 2, ][,
    .(AnswersNumber = length(PostTypeId)),
    by = OwnerUserId
  ]
  #------ druga z najbardziej wewnetrznych kwarend
  # wybieramy tylko PostTypeId==1
  # grupujemy po OwnerUserId i zliczamy ile wystapien
  s2 <- data.table(Posts)[PostTypeId == 1, ][,
    .(QuestionsNumber = length(PostTypeId)),
    by = OwnerUserId
  ]
  #------ wewnetrzna kwarende ze srodka
  # laczymy wyniki dwoch poprzednich kwarend
  s3 <- na.omit(s1[s2, on = .(OwnerUserId = OwnerUserId)])
  # zostawiamy tylko te gdzie AnswersNumber > QuestionsNumber
  s3 <- s3[AnswersNumber > QuestionsNumber, 
  # sortujemy po AnswersNumber i zostawiamy tylko 5 najwiekszych
  setorder(s3, -AnswersNumber)
  s3 <- head(s3, 5)
  #------ najbardziej zewnetrzna kwarenda
  # laczymy wyniki poprzedniej kwarendy z tabela Users
  s4 <- data.table(Users)[s3, on = .(Id = OwnerUserId)]
  # zwracamy to co nas interesuje jako data.frame
  return(as.data.frame(s4[, .(
    DisplayName,
    QuestionsNumber,
    AnswersNumber,
    Location,
    Reputation,
    UpVotes,
    DownVotes
  )]))
}

# Sprawdzenie rownowaznosci wynikow - zakomentuj te czesc przed wyslaniem
# compare_calls(
#   sql_4(Posts, Users),
#   base_4(Posts, Users),
#   dplyr_4(Posts, Users),
#   table_4(Posts, Users)
# )

# Porowanie czasow wykonania - zakomentuj te czesc przed wyslaniem
# microbenchmark(
#   sql_call_score=sql_4(Posts, Users),
#   base_call_score=base_4(Posts, Users),
#   dplyr_call_score=dplyr_4(Posts, Users),
#   table_call_score=table_4(Posts, Users),
#   times=1
# )

# -----------------------------------------------------------------------------#
# Zadanie 5
# -----------------------------------------------------------------------------#

sql_5 <- function(Posts, Comments, Users) {
  # wykonujemy zapytanie sql przez sqldf i zwracamy
  sqldf("
      SELECT
        Title,
        CommentCount,
        ViewCount,
        CommentsTotalScore,
        DisplayName,
        Reputation,
        Location
      FROM (
        SELECT
          Posts.OwnerUserId,
          Posts.Title,
          Posts.CommentCount,
          Posts.ViewCount,
          CmtTotScr.CommentsTotalScore
        FROM (
          SELECT PostId, SUM(Score) AS CommentsTotalScore
          FROM Comments
          GROUP BY PostId
        ) AS CmtTotScr
        JOIN Posts ON Posts.Id = CmtTotScr.PostId WHERE Posts.PostTypeId=1
      ) AS PostsBestComments
      JOIN Users ON PostsBestComments.OwnerUserId = Users.Id
      ORDER BY CommentsTotalScore DESC
      LIMIT 10
    ")
}

base_5 <- function(Posts, Comments, Users) {
  #------ najbardziej wewnetrzna kawrenda
  # grupujemy po PostId i sumujemy po score
  s1 <- aggregate(
    Comments$Score, Comments[, c("PostId"), drop = FALSE],
    sum,
    na.rm = TRUE
  )
  colnames(s1)[2] <- "CommentsTotalScore"
  #------ kolejna kwarenda w hierarchii
  # przeprowadzamy join pomiedzy postami a wynikami poprzedniej
  s2 <- merge(s1, Posts, by.x = "PostId", by.y = "Id")
  # zostawiamy tylko PostTypeId==1
  s2 <- s2[s2$PostTypeId == 1, ]
  # zostawiamy tylko to co nas interesuje
  s2 <- s2[, c(
    "OwnerUserId",
    "Title",
    "CommentCount",
    "ViewCount",
    "CommentsTotalScore"
  )]
  #------ najbardziej zewnetrzna kwarenda
  # przeprowadzamy join miedzy wynikami poprzedniej a Users
  s3 <- merge(s2, Users, by.x = "OwnerUserId", by.y = "Id")
  # sortujemy po CommentsTotalScore i zostawiamy tylko 10 najwiekszych
  s3 <- s3[order(s3$CommentsTotalScore, decreasing = TRUE)[1:10], ]
  # resetujemy indeks
  rownames(s3) <- NULL
  # zwracamy tylko to co nas interesuje
  return(s3[, c(
    "Title",
    "CommentCount",
    "ViewCount",
    "CommentsTotalScore",
    "DisplayName",
    "Reputation",
    "Location"
  )])
}

dplyr_5 <- function(Posts, Comments, Users) {
  #------ najbardziej wewnetrzna kawrenda
  s1 <- Comments %>%
    # grupujemy po PostId
    group_by(PostId) %>%
    # sumujemy po score
    reframe(CommentsTotalScore = sum(Score, na.rm = TRUE))
  #------ kolejna kwarenda w hierarchii
  # przeprowadzamy join pomiedzy postami a wynikami poprzedniej
  s2 <- inner_join(
    s1, Posts,
    join_by(PostId == Id),
    na_matches = "never"
  ) %>%
    # zostawiamy tylko PostTypeId==1
    filter(PostTypeId == 1) %>%
    # zostawiamy tylko to co nas interesuje
    select(
      OwnerUserId,
      Title,
      CommentCount,
      ViewCount,
      CommentsTotalScore
    )
  #------ najbardziej zewnetrzna kwarenda
  # przeprowadzamy join miedzy wynikami poprzedniej a Users
  s3 <- inner_join(
    s2, Users,
    join_by(OwnerUserId == Id),
    na_matches = "never"
  ) %>%
    # sortujemy po CommentsTotalScore
    arrange(-CommentsTotalScore) %>%
    # zostawiamy tylko 10 najwiekszych
    slice_head(n = 10) %>%
    # interesujace nas kolumny
    select(
      Title,
      CommentCount,
      ViewCount,
      CommentsTotalScore,
      DisplayName,
      Reputation,
      Location
    )
  return(as.data.frame(s3))
}

table_5 <- function(Posts, Comments, Users) {
  #------ najbardziej wewnetrzna kawrenda
  # grupujemy po PostId i sumujemy po score
  s1 <- data.table(Comments)[,
    .(CommentsTotalScore = sum(Score, na.rm = TRUE)),
    by = PostId
  ]
  #------ kolejna kwarenda w hierarchii
  # przeprowadzamy join pomiedzy postami a wynikami poprzedniej
  s2 <- data.table(Posts)[s1, on = .(Id = PostId)]
  # zostawiamy tylko PostTypeId==1 i interesujące nas kolumny
  s2 <- s2[PostTypeId == 1, .(
    OwnerUserId,
    Title,
    CommentCount,
    ViewCount,
    CommentsTotalScore
  )]
  #------ najbardziej zewnetrzna kwarenda
  # przeprowadzamy join miedzy wynikami poprzedniej a Users
  s3 <- na.omit(data.table(Users)[s2, on = .(Id = OwnerUserId)])
  s3
  # sortujemy po CommentsTotalScore i zostawiamy tylko 10 najwiekszych
  setorder(s3, -CommentsTotalScore)
  s3 <- head(s3, 10)
  # zwracamy tylko to co nas interesuje
  return(as.data.frame(s3[, .(
    Title,
    CommentCount,
    ViewCount,
    CommentsTotalScore,
    DisplayName,
    Reputation,
    Location
  )]))
}

# Sprawdzenie rownowaznosci wynikow - zakomentuj te czesc przed wyslaniem
# compare_calls(
#   sql_5(Posts, Comments, Users),
#   base_5(Posts, Comments, Users),
#   dplyr_5(Posts, Comments, Users),
#   table_5(Posts, Comments, Users)
# )

# Porowanie czasow wykonania - zakomentuj te czesc przed wyslaniem
# microbenchmark(
#   sql_call_score=sql_5(Posts, Comments, Users),
#   base_call_score=base_5(Posts, Comments, Users),
#   dplyr_call_score=dplyr_5(Posts, Comments, Users),
#   table_call_score=table_5(Posts, Comments, Users),
#   times=1
# )
