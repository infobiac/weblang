; ModuleID = 'my cool jit'
source_filename = "<string>"

define double @main() {
entry:
  ret double 9.000000e+00
}

define double @foo(double %a, double %b) {
entry:
  %0 = alloca double
  store double %a, double* %0
  %1 = alloca double
  store double %b, double* %1
  %2 = load double, double* %0
  %3 = load double, double* %0
  %4 = fmul double %2, %3
  %5 = load double, double* %0
  %6 = fmul double 2.000000e+00, %5
  %7 = load double, double* %1
  %8 = fmul double %6, %7
  %9 = fadd double %4, %8
  %10 = load double, double* %1
  %11 = load double, double* %1
  %12 = fmul double %10, %11
  %13 = fadd double %9, %12
  ret double %13
}

define double @bar(double %a) {
entry:
  %0 = alloca double
  store double %a, double* %0
  %1 = load double, double* %0
  %2 = call double @foo(double %1, double 4.000000e+00)
  %3 = call double @bar(double 3.133700e+04)
  %4 = fadd double %2, %3
  ret double %4
}

declare double @cos(double)

define double @main.1() {
entry:
  %0 = call double @cos(double 1.234000e+00)
  ret double %0
}
