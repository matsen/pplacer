
####### 2D Rotations ###################

v1Rot <- function(vec1, vec2, theta)  vec1*cos(theta) + vec2*sin(theta)
v2Rot <- function(vec1, vec2, theta) -vec1*sin(theta) + vec2*cos(theta)

overlap <- function(vec1, vec2, theta) {
  sum(abs(v1Rot(vec1, vec2, theta) * v2Rot(vec1, vec2, theta)))
}

rotatedVar <- function(var1, var2, theta) {
  var1.rot <- cos(theta)^2 * var1 + sin(theta)^2 * var2
  var2.rot <- sin(theta)^2 * var1 + cos(theta)^2 * var2
  c(var1.rot, var2.rot)
}

minOverlap <- function(vec1, vec2, points=50) {
  # Simple minimization routine returning the theta for which the overlap
  # is minimized
  thetas <- ((0:points)/points) * (pi / 2) - (pi / 4)
  overlaps <- sapply(thetas, function(theta) overlap(vec1, vec2, theta))
  theta <- thetas[ which.min(overlaps) ]
  list(theta=theta, plot.data=data.frame(theta=thetas, overlap=overlaps))
}

# This is bad - row.ids is actually row.ids here, because we are working with transposes
# (uhh... did I mean col.ids here    ^    ?)
rotatedTrans <- function(trans, theta, points=50, vars=NULL, row.ids=1:2) {
  row.ids <- row.ids[1:2]
  col1 <- row.ids[1]
  col2 <- row.ids[2]
  vec1 <- as.numeric(trans[col1,])
  vec2 <- as.numeric(trans[col2,])
  min.overlap <- minOverlap(vec1, vec2, points)
  theta <- min.overlap$theta
  plot.data <- min.overlap$plot.data
  rot.vec1 <- v1Rot(vec1, vec2, theta)
  rot.vec2 <- v2Rot(vec1, vec2, theta)
  rot.trans <- trans
  rot.trans[col1,] <- rot.vec1
  rot.trans[col2,] <- rot.vec2
  rot.names <- sapply(row.ids, function(i) paste('v', i, '.rot', sep=''))
  rot.vars <- vars
  if (!is.null(vars)) {
    rot.vars[row.ids] <- rotatedVar(vars[col1], vars[col2], theta)
    rot.names <- paste(rot.names, sapply(rot.vars, function(var) paste('var =', var)))
  }
  rownames(rot.trans)[row.ids] <- rot.names
  list(theta=theta, rot.trans=rot.trans, rot.vars=rot.vars, plot.data=plot.data)
}





##### 3D Rotations ###############

crossProdMat <- function(vec) {
  row1 <- c(0,      -vec[3], vec[2])
  row2 <- c(vec[3],  0,     -vec[1])
  row3 <- c(-vec[2], vec[1], 0     )
  matrix(c(row1,row2,row3), byrow=T, nrow=3)
}

# We need the rotational axisection here for pluging into the formula properly
rotMatrix3D <- function(axis, theta) {
  diag(3)*cos(theta) + sin(theta)*crossProdMat(axis) + (1 - cos(theta))*crossprod(t(axis))
}

rotateTrans3D <- function(trans, thetas) {
  rot.mat <- diag(3)
  for (i in 1:length(thetas)) {
    axis <- rep(0,3)
    axis[i] <- 1
    rot.mat <- rot.mat %*% rotMatrix3D(axis, thetas[i])
  }
  t(t(trans) %*% rot.mat)
}

overlap3D <- function(trans, thetas) {
  print(thetas)
  rot.trans <- rotateTrans3D(trans, thetas)
  sum( abs(rot.trans[1,]*rot.trans[2,]) + abs(rot.trans[1,]*rot.trans[3,]) + abs(rot.trans[2,]*rot.trans[3,]) )
}

minOverlap3D <- function(trans, start=c(0,0,0)) {
  print('starting overlap minimization')
  upper <- rep(pi/2, 3)
  lower <- -1 * upper
  obj.fn <- function(thetas) overlap3D(trans, thetas)
  optim(start, obj.fn, method='L-BFGS-B', lower=lower, upper=upper)
}

# the start input determines how many dimensions you want to rotate in
# it does not however give you much control of the direction of rotation
# for 2d case.
rotatedTrans3D <- function(trans, start=c(0,0,0), vars=NULL) {
  part.trans <- trans[1:3,]
  opt <- minOverlap3D(part.trans, start=start)
  thetas <- opt$par
  overlap <- opt$value
  rot.trans <- trans
  rot.trans[1:3,] <- rotateTrans3D(part.trans, thetas)
  list(rot.trans=rot.trans, thetas=thetas, rot.vars=NULL, overlap=overlap)
}



##### Generalization...

crossProdMat <- function(vec) {
  row1 <- c(0,      -vec[3], vec[2])
  row2 <- c(vec[3],  0,     -vec[1])
  row3 <- c(-vec[2], vec[1], 0     )
  matrix(c(row1,row2,row3), byrow=T, nrow=3)
}

# We need the rotational axisection here for pluging into the formula properly
rotMatrix <- function(axis, theta) {
  diag(3)*cos(theta) + sin(theta)*crossProdMat(axis) + (1 - cos(theta))*crossprod(t(axis))
}

rotateTrans <- function(trans, thetas) {
  rot.mat <- diag(3)
  for (i in 1:length(thetas)) {
    axis <- rep(0,3)
    axis[4-i] <- 1
    rot.mat <- rot.mat %*% rotMatrix(axis, thetas[i])
  }
  t(t(trans) %*% rot.mat)
}

overlap <- function(trans, dimensions, thetas) {
  print(thetas)
  rot.trans <- rotateTrans(trans, thetas)
  # could do over 3 choose length(dimensions)
  if (dimensions == 3) {
    sum( abs(rot.trans[1,]*rot.trans[2,]) + abs(rot.trans[1,]*rot.trans[3,]) + abs(rot.trans[2,]*rot.trans[3,]) )
  } else {
    sum( abs(rot.trans[1,]*rot.trans[2,]) )
  }
}

minOverlap <- function(trans, dimensions, opt.start) {
  print('starting overlap minimization')
  upper <- rep(pi/2, 3)
  lower <- -1 * upper
  obj.fn <- function(thetas) overlap(trans, dimensions, thetas)
  optim(opt.start, obj.fn, method='L-BFGS-B', lower=lower, upper=upper)
}

# the start input determines how many dimensions you want to rotate in
# it does not however give you much control of the direction of rotation
# for 2d case.
rotatedTrans <- function(trans, dimensions, opt.start=NULL, vars=NULL) {
  part.trans <- trans[1:3,]
  if (is.null(opt.start)) {
    n <- dimensions
    opt.start <- rep(0, n*(n-1)/2)
  }
  opt <- minOverlap(part.trans, dimensions, opt.start=opt.start)
  thetas <- opt$par
  overlap <- opt$value
  rot.trans <- trans
  rot.trans[1:3,] <- rotateTrans3D(part.trans, thetas)
  list(rot.trans=rot.trans, thetas=thetas, rot.vars=NULL, overlap=overlap)
}

