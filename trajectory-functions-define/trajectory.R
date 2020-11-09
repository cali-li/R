#Q3
#data transformation at first
my_data =function(my_mx){
  #input trajectory(x,y,t)
  #output: #initialize data, s.t. at time 0, mouse is at the origin point.
  n = nrow(my_mx)
  i = 1
  my_mx2 = matrix(nrow = n, ncol = 3)
  while (i <= n) {
    my_mx2[i,] = my_mx[i,] - my_mx[1,]
    i = i+1
  }
  return(my_mx2)
}

#Q3.a
my_traj = function(my_mx,t){
  #input: trajectory (x, y, t), 
  #(x, y) is the position of a mouse cursor in the monitor,
  #t is the time at specific position.
  #output: a function combine two info together. Given time(t), get position(x,y).
  my_mx = my_data(my_mx)
  n = nrow(my_mx)
  x = my_mx[,1:2]       #x is the position of the mouse, that is x = (x,y)
  y = my_mx[,3]         #y is the time respectively,that is y = t
  for (i in 1:n) {
    if (y[i] == t) return(x[i,1:2])  #return x according to y
  }
}

#compute using data package
#TODO

#Q3.b
my_angle = function(my_mx){
  #input: trajectory (x, y, t),
  #(x, y) is the position of a mouse cursor in the monitor.
  #output: the angle ?? connecting the origin and the final position in the trajectory.
  my_mx = my_data(my_mx)                 #data initializing
  n = nrow(my_mx)
  theta = atan2(my_mx[n,2],my_mx[n,1])   #compute the angle
  #theta = theta * 100
  #if (theta < -180) theta = theta + 180   #output angle theta
  return(theta)
}

#Q3.c
my_rotate = function(my_mx){
  #input: trajectory (x, y, t),
  #(x, y) is the position of a mouse cursor in the monitor.
  #output: If the final position is at the negetive x-axis part, 
  #rotate the coordinates to make it positive
  my_mx = my_data(my_mx)
  n = nrow(my_mx)
  my_mx2 = matrix(nrow = n, ncol = 3)    #generate new matrix
  for (i in 1:n) {                       #output in new matrix
    my_mx2[i,1] = sqrt((my_mx[i,1])^2 + (my_mx[i,2])^2)*sin((atan2(my_mx[i,2],my_mx[i,1]))-(atan2(my_mx[n,2],my_mx[n,1])-pi/2))
    my_mx2[i,2] = sqrt((my_mx[i,1])^2 + (my_mx[i,2])^2)*cos((atan2(my_mx[i,2],my_mx[i,1]))-(atan2(my_mx[n,2],my_mx[n,1])-pi/2))
    my_mx2[i,3] = my_mx[i,3]
  }
  return(my_mx2)
}

#Q3.d
my_comb = function(my_mx) {
  #input: trajectory (x, y, t),
  #(x, y) is the position of a mouse cursor in the monitor.
  #output: similar to question c
  my_mx = my_data(my_mx)
  my_mx = my_rotate(my_mx)
  return(my_mx)
}

#Q3.e
my_curv = function(my_mx) {
  #input: same
  #output: put above metrics together describing curvature
  y = c("dis" = my_curv1(my_mx),
        "max_ad" = my_curv2(my_mx),
        "mean_ad" = my_curv3(my_mx),
        "area" = my_curv4(my_mx))
  return(y)
}

my_curv1 = function(my_mx) {
  #input: the same as foreward
  #output: total distance
  n = nrow(my_mx)
  dis = 0
  i = 1
  while (i < n) {           #compute the total (Euclidean) distance
  dis = dis + sqrt((my_mx[i,1]-my_mx[i+1,1])^2 + (my_mx[i,2]-my_mx[i+1,2])^2)
  i = i + 1
  }
  return(dis)               #return the value
}

my_curv2 = function(my_mx) {
  #input: same
  #output: maximum absolute deviation from the secant connecting the starting and final positions,
  n = nrow(my_mx)
  my_mx = my_rotate(my_mx)
  y = abs(my_mx[,2])                 #y is a vector of absolute deviation
  i = 1
  while (i <= n) {
    if (y[i] == max(y)) return(y[i]) #find the maximun absolute deviation
    i = i + 1
  }
}

my_curv3 = function(my_mx) {
  #input: same
  #output: average absolute deviation
  n = nrow(my_mx)
  my_mx = my_rotate(my_mx)
  y = abs(my_mx[,2])                 #y is a vector of absolute deviation
  mean(y)                            #average absolute deviation
}

my_curv4 = function(my_mx) {
  #input: same as above
  #output:  (absolute) area under the curve
  n = nrow(my_mx)
  my_mx = my_rotate(my_mx)
  area = 0
  i = 1
  while (i < n) {             #compute absolute area using the trapezoidal rule
    area = area + (my_mx[i,1]-my_mx[i+1,1])*(abs(my_mx[i,2])+abs(my_mx[i+1,2]))/2
    i = i + 1
  }
  area = abs(area)
  return(area)
}