#Aqui se crean las funciones para valor futuro conocido de anualidades vencidas:

VFVenc = function(A, T, r){
  VF = A*((1+r)^T-1)/r
  return(VF)
}

PagoVFVenc = function(VF, T, r){
  A = VF/(((1+r)^T-1)/r)
  return(A)
}

TasaVFVenc = function(VF, A, T){
  aprox = VF + 1
  VF2 = 0
  i = 1
  while(VF/10000 < aprox){
    r = i/1000000
    VF2 = A*((1+r)^T-1)/r
    aprox = abs(VF-VF2)
    i = i + 1
  }
  return(r*100*24)
}

PeriodosVFVenc = function(VF, A, r){
  T = log(((VF/A)*r)+1)/log(1+r)
  return(T/24)
}

#Aqui se crean las funciones para valor actual conocido de anualidades vencidas:

VAVenc = function(A, T, r){
  VA = A*(1-(1+r)^-T)/r
  return(VA)
}

PagoVAvenc = function(VA, T, r){
  A = VA/((1-(1+r)^-T)/r)
  return(A)
}

TasaVAVenc = function(VA, A, T){
  aprox = VA + 1
  VA2 = 0
  i = 1
  while (VA/10000 < aprox) {
    r = i/1000000
    VA2 = A*(1-(1+r)^-T)/r
    aprox = abs(VA-VA2)
    i = i + 1
  }
  return(r*100*24)
}

PeriodosVAVenc = function(VA, A, r){
  T = -log(1-((VA/A)*r))/log(1+r)
  return(T/24)
}

#Aqui se crean las funciones para valor futuro conocido de anualidades anticipadas:

VFAnt = function(A, T, r){
  VF = (1+r)*A*((1+r)^T-1)/r
  return(VF)
}

PagoVFAnt = function(VF, T, r){
  A = r*VF/(((1+r)^T-1)*(1+r))
  return(A)
}

TasaVFAnt = function(VF, A, T){
  aprox = VF + 1
  VF2 = 0
  i = 1
  while(VF/10000 < aprox){
    r = i/1000000
    VF2 = (1+r)*A*((1+r)^T-1)/r
    aprox = abs(VF-VF2)
    i = i + 1
  }
  return(r*100*24)
}

PeriodosVFAnt = function(VF, A, r){
  T = log(1+r*VF/(A*(1+r)))/log(1+r)
  return(T/24)
}

#Aqui se crean las funciones para valor actual conocido de anualidades anticipadas:

VAAnt = function(A, T, r){
  VA = A*((1-(1+r)^-T)/r)*(1+r)
  return(VA)
}

PagoVAAnt = function(VA, T, r){
  A = r*VA/((1-(1+r)^-T)*(1+r))
  return(A)
}

TasaVAAnt = function(VA, A, T){
  aprox = VA + 1
  VA2 = 0
  i = 1
  while (VA/10000 < aprox) {
    r = i/1000000000
    VA2 = A*((1-(1+r)^-T)/r)*(1+r)
    aprox = abs(VA-VA2)
    i = i + 1
  }
  return(r*100*24)
}

PeriodosVAAnt = function(VA, A, r){
  T = -log(1-r*VA/(A*(1+r)))/log(1+r)
  return(T/24)
}

#Aqui se crean las funciones para valor actual y monto del pago A conocido de anualidades 
#diferidas con pagos vencidos:

VADif = function(A, T, r, G){
  VA = (A*(1-(1+r)^-T)/r)/(1+r)^G
  return(VA)
}

PagoVADif = function(VA, T, r, G){
  VA = VA*(1+r)^G
  A = r*VA/(1-(1+r)^-T)
  return(A)
}

