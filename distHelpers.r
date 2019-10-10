library(dplyr)

e = exp(1)

# Exponential ----

expParams = function(data) {
  list(l=1/mean(data))
}

expPdfGen = function(params) {
  function(t) params$l * e^(-params$l * t)
}

expCdfFnGen = function(params) {
  return(function(t) 1 - e^(-params$l * t))
}

expCdf = function(params, left, rights) {
  fn = expCdfFnGen(params)
  fn(rights) - fn(left)
}


# Inter-Delay ----
interDelayParams = function(data) {
  mu = mean(data)
  s2 = var(data)
  
  l_re = 2 * 
    sqrt(2*s2 - mu^2)/
    (mu^2 - s2)
  
  l_fd = 
    (mu - sqrt(2*s2 - mu^2))/
    (mu^2 - s2)
  
  if(l_re < 0) l_re=-1*l_re;
  
  S = l_fd/l_re * (l_re + l_fd)
  
  return(
    list(l_re=l_re, l_fd=l_fd, S=S)
  )
}

interDelayPdfGen = function(params) { 
  function(t) params$S * exp(-t * params$l_fd) * 
    (1 - exp(-t * params$l_re))
}

interDelayCdfFnGen = function(params) {
  l1 = params$l_re
  l2 = params$l_fd
  
  fn = function(t) 1 - e^(-t * l2) + (l2/l1)*e^(-t*l2)*(e^(-t*l1) - 1)
  
  return(fn)
}

interDelayCdf = function(params, left, rights) {
  fn = interDelayCdfFnGen(params)
  fn(rights) - fn(left)
}


# Combine ----
combineExpWithDelayParamsInitialGuess = function(data, params) {
  p0_1_exp = expCdf(params, 0, 1)
  p0_1_delay = interDelayCdf(params, 0, 1)
  
  #p0_exp = exp_pdf[min(which(deltaTimes == 0))]
  n_data = length(data)
  p0_1_data = sum(data < 1)/n_data
  
  if(p0_1_data > p0_1_exp) {
    warning("p(data) > p(exp), setting p(regroup) = 0")
    p_regroup = 0
  }
  else if(p0_1_data < p0_1_delay) {
    warning("p(data) < p(delay), setting p(regroup) = 1")
    p_regroup = 1
  }
  else {
    dist_delay = p0_1_data - p0_1_delay
    dist = p0_1_exp - p0_1_delay
    dist_exp = p0_1_exp - p0_1_data
    
    p_regroup = dist_exp/dist
  }
  
  return(p_regroup)
}


combineExpWithDelayParams = function(data) {
  id_params = interDelayParams(data)
  exp_params = expParams(data)
  
  params = append(id_params, exp_params)
  
  p_regroup_start = combineExpWithDelayParamsInitialGuess(data, params)
  
  genNewPdf = function(p_regroup) {
    params$R = p_regroup
    combineExpWithDelayPdfGen(params)
  }
  
  localLL = function(p_regroup) {
    pdf_fn = genNewPdf(p_regroup)
    calcLL(data, pdf_fn)
  }
  
  n = 20
  past_n = seq(0, (n-1)*n, n)
  span = function(arr) range(arr)[2] - range(arr)[1]
  
  curr_p_regroup = p_regroup_start
  while(span(past_n) > .01) {
    cand_p_regroup = curr_p_regroup + rnorm(1, sd = .04)
    cand_p_regroup = ifelse(cand_p_regroup > 1, 1, cand_p_regroup)
    cand_p_regroup = ifelse(cand_p_regroup < 0, 0, cand_p_regroup)
    
    curr_ll = localLL(curr_p_regroup)
    cand_ll = localLL(cand_p_regroup)
    if(cand_ll > curr_ll)
      curr_p_regroup = cand_p_regroup
    
    past_n = append(curr_p_regroup, head(past_n, (n-1)))
  }
  
  params$R = curr_p_regroup
  
  return(params)
}

combineExpWithDelayPdfGen = function(params) {
  interDelayPdf = interDelayPdfGen(params)
  expPdf = expPdfGen(params)
  return (
    function (t) interDelayPdf(t) * params$R +
      expPdf(t) * (1 - params$R)
  )
}

combineExpWithDelayCdfFnGen = function(params) {
  p_regroup = params$R
  
  delay_fn = interDelayCdfFnGen(params)
  exp_fn = expCdfFnGen(params)
  
  fn = function(t) 
    delay_fn(t) * p_regroup + exp_fn(t) * (1 - p_regroup)
  
  return(fn)
}

combineExpWithDelayCdf = function(params, left, rights) {
  fn = combineExpWithDelayCdfFnGen(params)
  fn(rights) - fn(left)
}


#Emperical ----
empericalCdf = function(data) {
  ecdf(data)(data)
}



#Sampling ----



#Log Likelihood ----
calcLL = function(data, p_fn) {
  data %>% p_fn %>% log %>% sum
}
