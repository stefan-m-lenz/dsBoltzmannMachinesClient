asDSVectorArg <- function (x) {
   if (!is.null(x)) {
      x <- paste0(as.character(x), collapse = ",")
   }
   return(x)
}


print.monitoringresult <- function(x) {
   print("Monitoring output ... (not printed)")
}


#' Fit an RBM model
#'
#' Fits an RBM model using Stochastic Gradient Descent (SGD) on the \code{data}
#' with Contrastive Divergence (CD).
#' During the training, monitoring data is collected by default.
#' The monitoring data is returned to the user.
#' The trained model is stored on the server side (see parameter \code{newobj}).
#'
#' If the option \code{datashield.BoltzmannMachines.shareModels} is set to \code{TRUE}
#' by an administratorat the server side, the models themselves are returned in addition.
#'
#' @param datasources A list of Opal object(s) as a handle to the server-side session
#' @param data The name of the variable that holds the data on the server-side.
#'    Defaults to \code{"D"}.
#' @param newobj The name for the variable, in which the trained RBM will be stored.
#'    Defaults to \code{"rbm"}
#' @param monitoring Name for monitoring options used for RBM training.
#'    Possible options:
#'    \itemize{
#'    \item \code{"reconstructionerror"}: Calculates the reconstruction error (Default)
#'    \item \code{"loglikelihood"}: Estimates the loglikelihood via annealed importance sampling (AIS)
#'    \item \code{"exactloglikelihood"}: Exact calculation of log-likelihood.
#'    This is only feasible for very small models.
#'    \item \code{NULL}: No monitoring
#'    }
#' @param monitoringdata A vector of names for server-side data sets that are to be used for
#'    monitoring
#' @param rbmtype the type of the RBM that is to be trained
#'    This must be a subtype of \code{AbstractRBM} and defaults to \code{BernoulliRBM}.
#' @param nhidden number of hidden units for the returned RBM
#' @param epochs number of training epochs
#' @param learningrate The learning rate for the weights and biases
#'    can be specified as single value, used throughout all epochs. Defaults to 0.005.
#' @param learningrates The learning rate for the weights and biases
#'    can also be specified as a vector that contains a value for each epoch.
#' @param batchsize number of samples that are used for making one step in the
#'    stochastic gradient descent optimizer algorithm. Default is 1.
#' @param pcd indicating whether Persistent Contrastive Divergence (PCD) is to
#'    be used (true, default) or simple CD that initializes the Gibbs Chain with
#'    the training sample (false)
#' @param cdsteps number of Gibbs sampling steps for (persistent)
#'    contrastive divergence, defaults to 1
#' @param categories only relevant if \code{rbmtype} is \code{"Softmax0BernoulliRBM"}.
#'   The number of categories, if all variables have the same number
#'   of categories, or as vector that contains the number of categories
#'   of the i'th categorical variable in the i'th entry.
#' @param upfactor If this function is used e.g. for pretraining a part of
#'    a DBM, it is necessary to multiply the weights of the RBM with factors.
#' @param downfactor If this function is used e.g. for pretraining a part of
#'    a DBM, it is necessary to multiply the weights of the RBM with factors.
#' @param startrbm a name for an RBM object at the server side that
#'    is used as starting value for training.
#'    If this argument is specified, \code{nhidden} and \code{rbmtype} are ignored.
ds.monitored_fitrbm <- function(datasources, data = "D", newobj = 'rbm',
                                monitoring = "reconstructionerror",
                                monitoringdata = NULL,
                                # keyword arguments for fitrbm
                                nhidden = NULL,
                                epochs = NULL,
                                upfactor = NULL,
                                downfactor = NULL,
                                learningrate = NULL,
                                learningrates = NULL,
                                pcd = NULL,
                                cdsteps = NULL,
                                categories = NULL,
                                batchsize = NULL,
                                rbmtype = NULL,
                                startrbm = NULL) {

   cally <- call('monitored_fitrbmDS', newobj = newobj, data = data,
                 monitoring = asDSVectorArg(monitoring),
                 monitoringdata = asDSVectorArg(monitoringdata),
                 # keyword arguments for fitrbm
                 # (The  order is important (must be same as in server package)
                 # -  Bug in Opal?)
                 nhidden = nhidden,
                 epochs = epochs,
                 upfactor = upfactor,
                 downfactor = downfactor,
                 learningrate = learningrate,
                 learningrates = asDSVectorArg(learningrates),
                 pcd = pcd,
                 cdsteps = cdsteps,
                 categories = asDSVectorArg(categories),
                 batchsize = batchsize,
                 rbmtype = rbmtype,
                 startrbm = startrbm)

   datashield.aggregate(datasources, cally)
}


#' Train a stack of RBMs
#'
#' Performs greedy layerwise training for Deep Belief Networks or greedy layerwise
#' pretraining for Deep Boltzmann Machines.
#' During the training, monitoring data is collected by default.
#' The monitoring data is returned to the user.
#' The trained model is stored on the server side (see parameter \code{newobj}).
#'
#' If the option \code{datashield.BoltzmannMachines.shareModels} is set to \code{TRUE}
#' by an administrator at the server side, the model itself is returned in addition.
#'
#' @param datasources A list of Opal object(s) as a handle to the server-side session
#' @param data The name of the variable that holds the data on the server-side.
#'    Defaults to \code{"D"}.
#' @param newobj The name for the variable, in which the trained RBM will be stored.
#'    Defaults to \code{"rbmstack"}
#' @param monitoring Name for monitoring options used for RBM training.
#'    For possible options, see \code{\link{ds.monitored_fitrbm}}
#' @param monitoringdata A vector of names for server-side data sets that are to be used for
#'    monitoring. The data is propagated forward through the
#'    network to monitor higher levels.
#' @param predbm logical value indicating that the greedy layerwise training is
#'    pre-training for a DBM.
#'    If its value is \code{FALSE} (default), a DBN is trained.
#' @param nhiddens vector containing the number of nodes of the i'th hidden layer in
#'    the i'th entry
#' @param epochs number of training epochs
#' @param learningrate learningrate, default 0.005
#' @param batchsize size of minibatches, defaults to 1
#' @param trainlayers a vector of names for \code{TrainLayer} objects.
#'    With this argument it is possible
#'    to specify the training parameters for each layer/RBM individually.
#'    If the number of training epochs and the learning rate are not specified
#'    explicitly for a layer, the values of \code{epochs}, \code{learningrate}
#'    and \code{batchsize} are used.
#'    For more information see help of \code{\link{ds.bm.defineLayer}}.
#' @param samplehidden logical value indicating that consequent layers are to be trained
#'    with sampled values instead of the deterministic potential.
#'    Using the deterministic potential (\code{FALSE}) is the default.
ds.monitored_stackrbms <- function(datasources, data = "D", newobj = 'rbmstack',
                                   monitoring = "reconstructionerror",
                                   monitoringdata = NULL,
                                   # keyword arguments for stackrbms
                                   nhiddens = NULL,
                                   epochs = NULL,
                                   predbm = NULL,
                                   samplehidden = NULL,
                                   learningrate = NULL,
                                   batchsize = NULL,
                                   trainlayers = NULL) {

   cally <- call('monitored_stackrbmsDS', newobj = newobj, data = data,
                 monitoring = asDSVectorArg(monitoring),
                 monitoringdata = asDSVectorArg(monitoringdata),
                 # keyword arguments for stackrbms
                 nhiddens = asDSVectorArg(nhiddens),
                 epochs = epochs,
                 predbm = predbm,
                 samplehidden = samplehidden,
                 learningrate = learningrate,
                 batchsize = batchsize,
                 trainlayers = asDSVectorArg(trainlayers))

   datashield.aggregate(datasources, cally)
}


#' Fine-Tuning of a DBM
#'
#' This functions performs monitored fine-tuning of a given DBM model.
#' For the complete training, including the pre-training, see \code{\link{ds.monitored_fitdbm}}.
#' During the training, monitoring data is collected by default.
#' The monitoring data is returned to the user.
#' The trained model is stored on the server side (see parameter \code{newobj}).
#'
#' If the option \code{datashield.BoltzmannMachines.shareModels} is set to \code{TRUE}
#' by an administrator at the server side, the model itself is returned in addition.
#'
#' @param dbm The name of DBM model that is to be fine-tuned. Defaults to \code{"dbm"}.
#' @param newobj The name of the variable to store the new DBM model.
#'    Defaults to \code{"dbm"}, such that the previous model is overwritten.
#' @param monitoring Name for monitoring options used for DBM training.
#'    For possible options, see \code{\link{ds.monitored_fitdbm}}
#' @param monitoringdata A vector of names of server-side data sets that are to be used for
#'    monitoring
#' @param epochs Number of training epochs for fine-tuning, defaults to 10
#' @param nparticles Number of particles used for sampling during fine-tuning of the
#'    DBM, defaults to 100
#' @param learningrate Learning rate for fine-tuning,
#'    decaying by default decaying with the number of epochs,
#'    starting with the given value for the \code{learningrate}.
#'    By default, the learning rate decreases with the factor \eqn{11 / (10 + epoch)}.
#' @param learningrates a vector of learning rates for each epoch of fine-tuning
ds.monitored_traindbm <- function(datasources, dbm = "dbm",
                                  newobj = "dbm", data = "D",
                                  monitoring = "logproblowerbound",
                                  monitoringdata = data,
                                  epochs = NULL,
                                  nparticles = NULL,
                                  learningrate = NULL,
                                  learningrates = NULL) {

   cally <- call('monitored_traindbmDS', startdbm = dbm, newobj = newobj, data = data,
                 monitoring = asDSVectorArg(monitoring),
                 monitoringdata = asDSVectorArg(monitoringdata),
                 # keyword arguments for traindbm!
                 epochs = epochs,
                 nparticles = nparticles,
                 learningrate = learningrate,
                 learningrates = asDSVectorArg(learningrates))

   datashield.aggregate(datasources, cally)
}


#' Fits a (multimodal) DBM model
#'
#' The procedure for DBM fitting consists of two parts:
#' First a stack of RBMs is pretrained in a greedy layerwise manner
#' (see \code{\link{ds.monitored_stackrbms}}). Then the weights of all layers are jointly
#' trained using the general Boltzmann Machine learning procedure.
#' During pre-training and fine-tuning, monitoring data is collected by default.
#' The monitoring data is returned to the user.
#' The trained model is stored on the server side (see parameter \code{newobj}).
#'
#' If the option \code{datashield.BoltzmannMachines.shareModels} is set to \code{TRUE}
#' by an administratorat the server side, the models themselves are returned in addition.
#'
#' @param datasources A list of Opal object(s) as a handle to the server-side session
#' @param newobj The name for the variable, in which the trained DBM will be stored.
#'    Defaults to \code{"dbm"}
#' @param data The name of the variable that holds the data on the server-side.
#'    Defaults to \code{"D"}.
#' @param nhiddens A vector that defines the number of nodes in the hidden layers of
#'    the DBM. The default value specifies two hidden layers with the same size
#'    as the visible layer.
#' @param epochs Number of training epochs for fine-tuning, defaults to 10
#' @param epochspretraining Number of training epochs for pretraining,
#'    defaults to \code{epochs}
#' @param learningrate
#'    Learning rate for joint training of layers (= fine-tuning)
#'    using the learning algorithm for a general Boltzmann Machine
#'    with mean-field approximation.
#'    The learning rate for fine tuning is by default decaying with the number of epochs,
#'    starting with the given value for the \code{learningrate}.
#'    By default, the learning rate decreases with the factor \eqn{11 / (10 + epoch)}.
#' @param learningrates a vector of learning rates for each epoch of fine-tuning
#' @param learningratepretraining Learning rate for pretraining,
#'    defaults to \code{learningrate}
#' @param batchsizepretraining Batchsize for pretraining, defaults to 1
#' @param nparticles Number of particles used for sampling during fine-tuning of the
#'    DBM, defaults to 100
#' @param pretraining The arguments for layerwise pretraining
#'    can be specified for each layer individually.
#'    This is done via a vector of names for objects that have previously been defined
#'    by \code{\link{ds.bm.defineLayer}} or \code{\link{ds.bm.definePartitionedLayer}}.
#'    (For a detailed description of the possible parameters,
#'    see the help there).
#'    If the number of training epochs and the learning rate are not specified
#'    explicitly for a layer, the values of \code{epochspretraining},
#'    \code{learningratepretraining} and \code{batchsizepretraining} are used.
#' @param monitoring Name(s) for the monitoring options used for monitoring the fine-tuning.
#'    Possible options:
#'    \itemize{
#'    \item \code{"logproblowerbound"}: Variational lower bound of log probability (Default)
#'    \item \code{"exactloglikelihood"}: Exact calculation of log-likelihood.
#'    This is only feasible for very small models.
#'    \item \code{NULL}: No monitoring
#'    }
#' @param monitoringpretraining Name for monitoring options used for monitoring the pre-training.
#'    The options are the same as for
#'    training an RBM (see \code{\link{ds.monitored_fitrbm}}).
#'    By default, the reconstruction error is monitored.
#' @param monitoringdata A vector of names for server-side data sets that are to be used for
#'    monitoring
#' @param monitoringdatapretraining A vector of names for data sets that are to be used for
#'    monitoring the pretraining. By default, this is the same as the \code{monitoringdata}.
ds.monitored_fitdbm <- function(datasources, newobj = "dbm", data = "D",
                                monitoring = "logproblowerbound",
                                monitoringdata = data,
                                monitoringpretraining = "reconstructionerror",
                                monitoringdatapretraining = monitoringdata,
                                nhiddens = NULL,
                                epochs = NULL,
                                nparticles = NULL,
                                learningrate = NULL,
                                learningrates = NULL,
                                learningratepretraining = NULL,
                                epochspretraining = NULL,
                                batchsizepretraining = NULL,
                                pretraining = NULL) {

   cally <- call('monitored_fitdbmDS', newobj = newobj, data = data,
                 monitoring = asDSVectorArg(monitoring),
                 monitoringdata = asDSVectorArg(monitoringdata),
                 monitoringpretraining = asDSVectorArg(monitoringpretraining),
                 monitoringdatapretraining = asDSVectorArg(monitoringdatapretraining),
                 # keyword arguments for fitdbm
                 nhiddens = asDSVectorArg(nhiddens),
                 epochs = epochs,
                 nparticles = nparticles,
                 learningrate = learningrate,
                 learningrates = asDSVectorArg(learningrates),
                 learningratepretraining = learningratepretraining,
                 epochspretraining = epochspretraining,
                 batchsizepretraining = batchsizepretraining,
                 pretraining = asDSVectorArg(pretraining))

   datashield.aggregate(datasources, cally)
}


#' Split samples of a data set
#'
#' Splits a data set randomly into two data sets \eqn{x_1} and \eqn{x_2}, such that
#' the fraction of samples in \eqn{x_2} is equal to (or as close as possible to) the
#' given \code{ratio}.
#'
#' @param datasources A list of Opal object(s) as a handle to the server-side session
#' @param data The name of the variable that holds the data on the server-side. Defaults to \code{"D"}.
#' @param ratio The ratio of samples
#' @param newobj1 Name for variable, where \eqn{x_1} is stored
#' @param newobj2 Name for variable, where \eqn{x_2} is stored
#' @examples
#'     ds.splitdata(o, "D", 0.1, "D.Train", "D.Test")
ds.splitdata <- function(datasources, data, ratio, newobj1, newobj2) {
   cally <- call("splitdataDS", data, ratio, newobj1, newobj2)
   datashield.aggregate(datasources, cally)
   invisible()
}


#' Set a random seed in Julia
#'
#' Set a seed for the random generator in Julia.
#' This makes calls to the non-deterministic algorithms in this package reproducible.
#'
#' @param seed the seed to set
ds.setJuliaSeed <- function(datasources, seed) {
   cally <- call("setJuliaSeedDS", seed)
   datashield.aggregate(datasources, cally)
   invisible()
}

#' Generate samples from a restricted Boltzmann machine
#'
#' Same as \code{\link{ds.bm.samples}}, only with parameter \code{bm} changed to \code{rbm}.
#'
#' @param rbm The name of the model to sample from on the server-side. Defaults to \code{"rbm"}.
ds.rbm.samples <- function(datasources, rbm = "rbm", ...) {
   ds.bm.samples(datasources = datasources, bm = rbm, ...)
}

#' Generate samples from a deep Boltzmann machine
#'
#' Same as \code{\link{ds.bm.samples}}, only with parameter \code{bm} changed to \code{dbm}.
#'
#' @param dbm The name of the model to sample from on the server-side. Defaults to \code{"dbm"}.
ds.dbm.samples <- function(datasources, dbm = "dbm", ...) {
   ds.bm.samples(datasources = datasources, bm = dbm, ...)
}


#' Generates samples from a Boltzmann machine model
#'
#' A Gibbs sampler is run in the Boltzmann machine model to sample from the learnt
#' distribution. This can also be used for sampling from a
#' \emph{conditional distribution}
#' (see argument `conditionIndex` and `conditionValue` below.)
#'
#' @param datasources A list of Opal object(s) as a handle to the server-side session
#' @param bm The name of the model to sample from on the server-side
#' @param nsamples Number of samples to generate
#' @param burnin Number of Gibbs sampling steps, defaults to 50.
#' @param conditionIndex A vector containing indices of variables that are to be conditioned on
#' @param conditionValue A vector containing the values for the variables that are to be conditioned on.
#'     (must be of same length as \code{conditionIndex})
#' @param samplelast boolean to indicate whether to sample in last step (\code{TRUE}, default)
#'   or whether to use the activation potential.
ds.bm.samples <- function(datasources, bm, nsamples,
                       burnin = NULL,
                       conditionIndex = NULL,
                       conditionValue = NULL,
                       samplelast = NULL) {

   cally <- call("samplesDS", bm, nsamples,
                 burnin = burnin,
                 conditionIndex = asDSVectorArg(conditionIndex),
                 conditionValue = asDSVectorArg(conditionValue),
                 samplelast = samplelast)
   datashield.aggregate(datasources, cally)
}


#' Parameters for training one RBM-layer in a DBM or DBN
#'
#' The call stores the parameters for training one RBM-layer in a DBM or DBN
#' on the server side in a Julia \code{TrainLayer} object.
#' The parameters \code{rbmtype}, \code{nhidden}, \code{epochs},
#'   \code{learningrate}/\code{learningrates}, \code{categories},
#'   \code{batchsize}, \code{pcd}, \code{cdsteps}, \code{startrbm} and \code{monitoring}
#'   of this object are passed to \code{\link{ds.monitored_fitrbm}}.
#'   For a detailed description, see there.
#'   Values of \code{NULL} indicate that a corresponding default value should be used.
#' @param datasources A list of Opal object(s) as a handle to the server-side session
#' @param newobj The name of the server-side object where the parameters are stored
#' @param nvisible Number of visible units in the RBM. Only relevant for partitioning.
#'    This parameter is derived as much as possible by \code{\link{ds.monitored_stackrbms}}.
#'    For multimodal DBMs with a partitioned first layer, it is necessary to specify
#'    the number of visible nodes for all but at most one partition in the input layer.
ds.bm.defineLayer <- function(datasources, newobj,
                           epochs = NULL,
                           learningrate = NULL,
                           learningrates = NULL,
                           sdlearningrate = NULL,
                           sdlearningrates = NULL,
                           categories = NULL,
                           monitoring = NULL,
                           rbmtype = NULL,
                           nhidden = NULL,
                           nvisible = NULL,
                           batchsize = NULL,
                           pcd = NULL,
                           cdsteps = NULL,
                           startrbm = NULL) {

   cally <- call("bm.defineLayerDS", newobj,
                 epochs = epochs,
                 learningrate = learningrate,
                 learningrates = asDSVectorArg(learningrates),
                 sdlearningrate = sdlearningrate,
                 sdlearningrates = asDSVectorArg(sdlearningrates),
                 categories = asDSVectorArg(categories),
                 rbmtype = rbmtype,
                 nhidden = asDSVectorArg(nhidden),
                 nvisible = asDSVectorArg(nvisible),
                 batchsize = batchsize,
                 pcd = pcd,
                 cdsteps = cdsteps,
                 startrbm = startrbm)

   datashield.aggregate(datasources, cally)
   invisible()
}


#' Parameters for training a partitioned RBM-layer in a DBM or DBN
#'
#' Creates an object at the server-side that encapsulates parameters for training
#' a partitioned layer.
#'
#' @param datasources A list of Opal object(s) as a handle to the server-side session
#' @param newobj The name of the server-side object where the parameters are stored
#' @param parts A vector with names for \code{TrainLayer} objects which have been created
#'   by \code{\link{ds.bm.defineLayer}} before.
ds.bm.definePartitionedLayer <- function (datasources, newobj, parts) {
   parts <- asDSVectorArg(parts)
   cally <- call("bm.definePartitionedLayerDS", newobj, parts)
   datashield.aggregate(datasources, cally)
   invisible()
}

#' Two-dimensional representation of latent features
#'
#' Calculates the mean-field approximation of the activation of the top hidden nodes in
#' the DBM. If there are more than two nodes in the top layer, the activation is further
#' reduced in dimensionality using a PCA.
#'
#' @param datasources A list of Opal object(s) as a handle to the server-side session
#' @param dbm The name of the DBM model on the server-side. Defaults to \code{"dbm"}.
#' @param data The name of the variable that holds the data on the server-side.
#'    Defaults to \code{"D"}.
#' @return A matrix with two columns for each of the sites,
#'   containing the two-dimensional representation for
#'   each of the samples in the rows. The samples are shuffled at random.
ds.dbm.top2LatentDims <- function(datasources, dbm = "dbm", data = "D") {
   cally <- call("dbm.top2LatentDimsDS", dbm, data)
   datashield.aggregate(datasources, cally)
}


#' Likelihood  estimation for an RBM model
#'
#' Estimates the log-likelihood for an RBM model using annealed importance sampling (AIS)
#' for estimating the partition function.
#'
#' @param datasources A list of Opal object(s) as a handle to the server-side session
#' @param rbm The name of the RBM model on the server. Defaults to \code{"rbm"}.
#' @param data The name of the variable that holds the data on the server-side.
#'    Defaults to \code{"D"}.
#' @param ntemperatures Number of temperatures for annealing from the starting model
#'    to the target model, defaults to 100
#' @param nparticles Number of parallel chains and calculated weights in AIS, defaults to 100
#' @param burnin Number of steps to sample for the Gibbs transition between models in AIS
ds.rbm.loglikelihood <- function(datasources, rbm = "rbm", data = "D",
                                 parallelized = NULL,
                                 ntemperatures = NULL,
                                 nparticles = NULL,
                                 burnin = NULL) {
   cally <- call("rbm.loglikelihoodDS", rbm, data,
                 parallelized = parallelized,
                 ntemperatures = ntemperatures,
                 nparticles = nparticles,
                 burnin = burnin)
   datashield.aggregate(datasources, cally)
}


#' Likelihood estimation for a DBM model
#'
#' Performs a separate run of annealed importance sampling (AIS)
#' for each of the samples to estimate the log-likelihood of a DBM in addition to
#' estimating the partition function.
#'
#' @param datasources A list of Opal object(s) as a handle to the server-side session
#' @param dbm The name of the DBM model on the server-side. Defaults to \code{"dbm"}.
#' @param data The name of the variable that holds the data on the server-side.
#'    Defaults to \code{"D"}.
#' @param ntemperatures Number of temperatures for annealing from the starting model
#'    to the target model, defaults to 100
#' @param nparticles Number of parallel chains and calculated weights in AIS, defaults to 100
#' @param burnin Number of steps to sample for the Gibbs transition between models in AIS
ds.dbm.loglikelihood <- function(datasources, dbm = "dbm", data = "D",
                                 parallelized = NULL,
                                 ntemperatures = NULL,
                                 nparticles = NULL,
                                 burnin = NULL) {
   cally <- call("dbm.loglikelihoodDS", dbm, data,
                 parallelized = parallelized,
                 ntemperatures = ntemperatures,
                 nparticles = nparticles,
                 burnin = burnin)
   datashield.aggregate(datasources, cally)
}


#' Estimation of Variational lower bound of log probability for a DBM model
#'
#' Estimates the variational lower bound of the likelihood of a DBM using
#' annealed importance sampling (AIS).
#'
#' @param datasources A list of Opal object(s) as a handle to the server-side session
#' @param rbm The name of the DBM model on the server-side. Defaults to \code{"dbm"}.
#' @param data The name of the variable that holds the data on the server-side.
#'    Defaults to \code{"D"}.
#' @param ntemperatures Number of temperatures for annealing from the starting model
#'    to the target model, defaults to 100
#' @param nparticles Number of parallel chains and calculated weights in AIS, defaults to 100
#' @param burnin Number of steps to sample for the Gibbs transition between models in AIS
ds.dbm.logproblowerbound <- function(datasources, dbm = "dbm", data = "D",
                                     parallelized = NULL,
                                     ntemperatures = NULL,
                                     nparticles = NULL,
                                     burnin = NULL) {
   cally <- call("dbm.logproblowerboundDS", dbm, data,
                 parallelized = parallelized,
                 ntemperatures = ntemperatures,
                 nparticles = nparticles,
                 burnin = burnin)
   datashield.aggregate(datasources, cally)
}


#' Exact calculation of the log-likelihood for a Boltzmann machine
#'
#' Calculates the log-likelihood of a Boltzmann machine model.
#'
#' \emph{This is only feasible for very small models, as the runtime grows exponentially with
#' the number of hidden nodes.}
#'
#' @param datasources A list of Opal object(s) as a handle to the server-side session
#' @param bm The name of the Boltzmann machine model on the server-side.
#' @param data The name of the variable that holds the data on the server-side.
#'    Defaults to \code{"D"}.
ds.bm.exactloglikelihood <- function(datasources, bm, data = "D") {
   cally <- call("bm.exactloglikelihoodDS", bm, data)
   datashield.aggregate(datasources, cally)
}


#' Exact calculation of the log-likelihood for an RBM
#'
#' Same as \code{\link{ds.bm.exactloglikelihood}}, only with parameter \code{bm} changed to \code{rbm}.
#'
#' @param rbm The name of the model to sample from on the server-side. Defaults to \code{"rbm"}.
ds.rbm.exactloglikelihood <- function(datasources, rbm = "rbm", data = "D") {
   ds.bm.exactloglikelihood(datasources, bm = rbm, data)
}


#' Exact calculation of the log-likelihood for a DBM
#'
#' Same as \code{\link{ds.bm.exactloglikelihood}}, only with parameter \code{bm} changed to \code{dbm}.
#'
#' @param dbm The name of the model to sample from on the server-side. Defaults to \code{"dbm"}.
ds.dbm.exactloglikelihood <- function(datasources, dbm = "dbm", data = "D") {
   ds.bm.exactloglikelihood(datasources, bm = dbm, data)
}
