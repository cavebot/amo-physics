 E04UCF Example Program Results
 
 *** E04UCF
 *** Start of NAG Library implementation details ***
 
 Implementation title: IBM RISC System/6000 AIX
            Precision: Double
         Product Code: FLIB617DA
                 Mark: 17A
 
 *** End of NAG Library implementation details ***
 
 Parameters
 ----------
 
 Linear constraints.....         1       Variables..............         4
 Nonlinear constraints..         2
 
 Infinite bound size....  1.00D+20       COLD start.............
 Infinite step size.....  1.00D+20       EPS (machine precision)  1.11D-16
 Step limit.............  2.00D+00       Hessian................        NO
 
 Linear feasibility.....  1.05D-08       Crash tolerance........  1.00D-02
 Nonlinear feasibility..  1.05D-08       Optimality tolerance...  3.26D-12
 Line search tolerance..  9.00D-01       Function precision.....  4.37D-15
 
 Derivative level.......         3       Monitoring file........        -1
 Verify level...........         0
 
 Major iterations limit.        50       Major print level......        10
 Minor iterations limit.        50       Minor print level......         0
 
 Workspace provided is     IWORK(     100),  WORK(    1000).
 To solve problem we need  IWORK(      17),  WORK(     185).
 
 
 Verification of the constraint gradients.
 -----------------------------------------
 
 The constraint Jacobian seems to be ok.
 
 The largest relative error was    2.29D-07  in constraint    2
 
 
 
 Verification of the objective gradients.
 ----------------------------------------
 
 The objective gradients seem to be ok.
 
 Directional derivative of the objective    8.15250000D-01
 Difference approximation                   8.15249734D-01
 
 
  Maj  Mnr    Step Merit Function Norm Gz  Violtn Cond Hz
    0    4 0.0D+00   1.738281D+01 7.1D-01 1.2D+01 1.0D+00
    1    1 1.0D+00   1.703169D+01 4.6D-02 1.9D+00 1.0D+00
    2    1 1.0D+00   1.701442D+01 2.1D-02 8.8D-02 1.0D+00
    3    1 1.0D+00   1.701402D+01 3.1D-04 5.4D-04 1.0D+00
    4    1 1.0D+00   1.701402D+01 7.0D-06 9.9D-08 1.0D+00
    5    1 1.0D+00   1.701402D+01 1.1D-08 4.6D-11 1.0D+00
 
 Exit from NP problem after     5 major iterations,
                                9 minor iterations.
 
 
 Varbl State     Value       Lower Bound   Upper Bound    Lagr Mult   Slack
 
 V   1    LL    1.00000       1.00000       5.00000       1.088         .
 V   2    FR    4.74300       1.00000       5.00000           .       .2570
 V   3    FR    3.82115       1.00000       5.00000           .       1.179
 V   4    FR    1.37941       1.00000       5.00000           .       .3794
 
 
 L Con State     Value       Lower Bound   Upper Bound    Lagr Mult   Slack
 
 L   1    FR    10.9436          None       20.0000           .       9.056
 
 
 N Con State     Value       Lower Bound   Upper Bound    Lagr Mult   Slack
 
 N   1    UL    40.0000          None       40.0000      -.1615     -3.5264E-11
 N   2    LL    25.0000       25.0000          None       .5523     -2.8791E-11
 
 Exit E04UCF - Optimal solution found.
 
 Final objective value =    17.01402
