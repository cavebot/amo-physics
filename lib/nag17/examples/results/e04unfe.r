 E04UNF Example Program Results
 
 *** E04UNF
 *** Start of NAG Library implementation details ***
 
 Implementation title: IBM RISC System/6000 AIX
            Precision: Double
         Product Code: FLIB617DA
                 Mark: 17A
 
 *** End of NAG Library implementation details ***
 
 Parameters
 ----------
 
 Linear constraints.....         1       Variables..............         2
 Nonlinear constraints..         1       Subfunctions...........        44
 
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
 
 J'J initial Hessian....                 Reset frequency........         2
 
 Workspace provided is     IWORK(     100),  WORK(    1000).
 To solve problem we need  IWORK(       9),  WORK(     306).
 
 
 Verification of the constraint gradients.
 -----------------------------------------
 
 The constraint Jacobian seems to be ok.
 
 The largest relative error was    1.92D-08  in constraint    1
 
 
 
 Verification of the objective gradients.
 ----------------------------------------
 
 The objective Jacobian seems to be ok.
 
 The largest relative error was    1.04D-08  in subfunction    3
 
 
 
  Maj  Mnr    Step Merit Function Norm Gz  Violtn Cond Hz
    0    2 0.0D+00   2.224070D-02 8.5D-02 3.6D-02 1.0D+00
    1    1 1.0D+00   1.455402D-02 1.5D-03 9.8D-03 1.0D+00
    2    1 1.0D+00   1.436491D-02 4.9D-03 7.2D-04 1.0D+00
    3    1 1.0D+00   1.427013D-02 2.9D-03 9.2D-06 1.0D+00
    4    1 1.0D+00   1.422989D-02 1.6D-04 3.6D-05 1.0D+00
    5    1 1.0D+00   1.422983D-02 5.4D-07 6.4D-08 1.0D+00
    6    1 1.0D+00   1.422983D-02 3.4D-09 9.8D-13 1.0D+00
 
 Exit from NP problem after     6 major iterations,
                                8 minor iterations.
 
 
 Varbl State     Value       Lower Bound   Upper Bound    Lagr Mult   Slack
 
 V   1    FR    .419953       .400000          None           .      1.9953E-02
 V   2    FR    1.28485      -4.00000          None           .       5.285
 
 
 L Con State     Value       Lower Bound   Upper Bound    Lagr Mult   Slack
 
 L   1    FR    1.70480       1.00000          None           .       .7048
 
 
 N Con State     Value       Lower Bound   Upper Bound    Lagr Mult   Slack
 
 N   1    LL  -9.768456E-13       .            None      3.3358E-02 -9.7685E-13
 
 Exit E04UNF - Optimal solution found.
 
 Final objective value =    .1422983E-01
