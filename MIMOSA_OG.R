pkgs <- c("MIMOSA","Biobase","ggplot2","MASS", "plyr","reshape")
    # see what packages are currently installed
installed_pacakges <- row.names(installed.packages()) # loop over the needed packages
for(p in pkgs){
        # check if package is installed
        already_installed <- p %in% installed_pacakges
        # if not already installed, install it
        if(!already_installed){ install.packages(p)
        }
        # and load package
        library(p, character.only = TRUE)}    
    
data(ICS)
E<-ConstructMIMOSAExpressionSet(ICS,
                                reference=ANTIGEN%in%'negctrl',measure.columns=c('CYTNUM','NSUB'),
                                other.annotations=c('CYTOKINE','TCELLSUBSET','ANTIGEN','UID'),
                                default.cast.formula=component~UID+ANTIGEN+CYTOKINE+TCELLSUBSET,
                                .variables=.(TCELLSUBSET,CYTOKINE,UID),
                                featureCols=1,ref.append.replace='_REF')

result<-MIMOSA(NSUB+CYTNUM~UID+TCELLSUBSET+CYTOKINE|ANTIGEN,
               data=E, method='mcmc',
               subset=RefTreat%in%'Treatment'&ANTIGEN%in%'ENV',
               ref=ANTIGEN%in%'ENV'&RefTreat%in%'Reference')

head(getZ(result))

#When I first tried to sub in a new value for EXPRATE, I couldn't find the actual function
#That .fitMCMC executes so I left the brackets empty
my.fitMCMC<-function (data, inits = NULL, iter = 250000, burn = 50000, thin = 1, 
          tune = 100, outfile = basename(tempfile(tmpdir = ".", fileext = ".dat")), 
          alternative = "greater", UPPER = 0.5, LOWER = 0.15, FAST = TRUE, 
          EXPRATE = .01, pXi = c(1, 1), seed = 10)
{
    set.seed(seed)
    alternative <- match.arg(alternative, c("greater", "not equal"))
    data <- MIMOSA:::icsdata2mvicsdata(data)
    if (is.null(inits)) {
        r <- MDMix(data)
        inits <- list(alpha.s = r@par.stim, alpha.u = r@par.unstim, 
                      q = r@w[1], z = round(r@z))
    }
    if (alternative == "greater") {
        ps <- t(do.call(cbind, apply(data$n.stim, 1, function(x) (data.frame(prop.table(x))[-1L, 
                                                                                            , drop = FALSE]))))
        pu <- t(do.call(cbind, apply(data$n.unstim, 1, function(x) (data.frame(prop.table(x))[-1L, 
                                                                                              , drop = FALSE]))))
        filter <- sapply(1:nrow(ps), function(i) all(ps[i, ] <= 
                                                         pu[i, ]))
        FILTER = TRUE
    }
    else {
        filter <- rep(FALSE, nrow(data$n.stim))
        FILTER <- FALSE
    }
    result <- .Call("C_fitMCMC", as.matrix(data$n.stim), as.matrix(data$n.unstim), 
                    as.vector(inits$alpha.s), as.vector(inits$alpha.u), as.vector(inits$q), 
                    as.matrix(inits$z), as.vector(iter), as.vector(burn), 
                    as.vector(thin), as.numeric(tune), as.character(outfile), 
                    as.vector(filter), as.numeric(UPPER), as.numeric(LOWER), 
                    FILTER, FAST, as.numeric(EXPRATE), as.numeric(pXi))
    if (inherits(result, "character")) {
        return(result)
    }
    result$z <- cbind(result$z, 1 - result$z)
    result$getmcmc <- function(x = outfile) {
        coda:::mcmc(read.table(x, sep = "\t", header = TRUE))
    }
    result$getP <- function(x = paste(outfile, "P", sep = ""), 
                            thin = 1) {
        if (thin > 1) {
            nc <- length(strsplit(readLines(x, 1), "\t")[[1]])
            thins <- paste("p", paste(rep(";n", thin - 1), collapse = ""), 
                           sep = "")
            s <- sprintf("sed -n '%s' %s|cut -f %s-%s", thins, 
                         x, (nc/3 + 1), nc)
            con <- pipe(s)
            d <- do.call(rbind, lapply(strsplit(readLines(con), 
                                                "\t")[-1L], as.numeric))
            colnames(d) <- strsplit(readLines(x, 1), "\t")[[1]][(nc/3 + 
                                                                     1):nc]
            d <- split(as.list(data.frame(d)), gl(nc/3, 2))
            close(con)
        }
        else {
            d <- coda:::mcmc(read.table(x, sep = "\t", header = TRUE))
            nc <- ncol(d)
            d <- split(as.list(data.frame(d[, (nc/3 + 1):nc])), 
                       gl(nc/3, 2))
        }
        d
    }
    attr(result, "class") <- c(attr(result, "class"), "MDMixResult")
    attr(result, "pData") <- attr(data, "pData")
    result$n.stim <- data$n.stim
    result$n.unstim <- data$n.unstim
    result
}


#The function reads in fine and I can both assign and fix the new function into MIMOSA's name space
assignInNamespace(".fitMCMC", my.fitMCMC, ns="MIMOSA")

#I can even run the MIMOSA call just fine
result_2<-MIMOSA(NSUB+CYTNUM~UID+TCELLSUBSET+CYTOKINE|ANTIGEN,
               data=E, method='mcmc',
               subset=RefTreat%in%'Treatment'&ANTIGEN%in%'ENV',
               ref=ANTIGEN%in%'ENV'&RefTreat%in%'Reference')

