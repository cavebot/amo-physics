 E04DGF Example Program Results
 
 *** E04DGF
 *** Start of NAG Library implementation details ***
 
 Implementation title: IBM RISC System/6000 AIX
            Precision: Double
         Product Code: FLIB617DA
                 Mark: 17A
 
 *** End of NAG Library implementation details ***
 
 Parameters
 ----------
 
 Variables..............         2
 
 Maximum step length....  1.00D+20       EPS (machine precision)  1.11D-16
 Optimality tolerance...  3.26D-12       Linesearch tolerance...  9.00D-01
 
 Est. opt. function val.      None       Function precision.....  4.37D-15
 Verify level...........         0
 
 Iteration limit........        50       Print level............        10
 
 
 Verification of the objective gradients.
 ----------------------------------------
 
 The objective gradients seem to be ok.
 
 Directional derivative of the objective   -1.47151776D-01
 Difference approximation                  -1.47151794D-01
 
 
  Itn      Step  Nfun      Objective    Norm G    Norm X   Norm (X(k-1)-X(k))
    0               1   1.839397D+00   8.2D-01   1.4D+00
    1   3.7D-01     3   1.724275D+00   2.8D-01   1.3D+00         3.0D-01
    2   1.6D+01     8   6.083488D-02   9.2D-01   9.3D-01         2.2D+00
    3   1.6D-03    14   5.367978D-02   1.0D+00   9.6D-01         3.7D-02
    4   4.8D-01    16   1.783392D-04   5.8D-02   1.1D+00         1.6D-01
    5   1.0D+00    17   1.671122D-05   2.0D-02   1.1D+00         6.7D-03
    6   1.0D+00    18   1.101991D-07   1.7D-03   1.1D+00         2.4D-03
    7   1.0D+00    19   2.332133D-09   1.8D-04   1.1D+00         1.5D-04
    8   1.0D+00    20   9.130942D-11   3.3D-05   1.1D+00         3.0D-05
    9   1.0D+00    21   1.085272D-12   4.7D-06   1.1D+00         7.0D-06
   10   1.0D+00    22   5.308300D-14   1.2D-06   1.1D+00         6.4D-07
 
 Exit from E04DGF after    10 iterations.
 
 
 Variable          Value      Gradient value
 Varbl    1      .500000          9.1D-07
 Varbl    2    -1.000000          8.3D-07
 
 Exit E04DGF - Optimal solution found.
 
 Final objective value =    .5308300E-13
