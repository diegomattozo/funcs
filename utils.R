#' Separate train and test database.
#'
#' @param db Dabatase to separate between trainning and test sets (data.frame)
#' @param percentual percentual dedicated to test set (numeric)
#' @param seed A seed (numeric)
#' @return list containing the databases (list)
#' @export
train_test_split = function(db, percentual, seed = 999) {
    set.seed(seed)
    lenDB = length(db[,1])
    aux = rep(F, lenDB)
    n = floor(lenDB * percentual)
    aux[sample(1:lenDB, n)] = T
    train = db[!aux,]
    test = db[aux,]
    
    list('train'=train, 'test'=test)
}


#' Categorize a database from the cutpoints returned from Univariate Methods.
#'
#' @param db: database (data.frame)
#' @parm cutpoints: vector of cutpoints (numeric)
#'
#' @return list containing discretized database (list)
#' @export
#'
#' @examples
disc_from_cuts = function (db, cutpoints) {
    len = length(db)
    Names = names(db)[-length(db)]; respName = names(db)[len]
    resp = db[, len]
    ChList = list()
    k=1
    for (i in Names) {
        cuts = unlist(cutpoints[i])
        #if (k==3) { print(findInterval(testDB[[i]], cuts, rightmost.closed = TRUE)) }
        covaux = findInterval(db[[i]], cuts, rightmost.closed = TRUE)
        ChList[[i]] = covaux
        k=k+1
    }
    ChList[[respName]] = resp
    data =  as.data.frame(ChList)
    
}
