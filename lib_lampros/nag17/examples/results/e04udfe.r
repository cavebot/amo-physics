 E04UDF Example Program Results
 
 
 Calls to E04UEF
 ---------------
 
       Infinite Bound Size = 1.0D+25
       Print Level = 1
       Verify Level = -1
 
 
 OPTIONS file
 ------------
 
      Begin   Example options file for  E04UDF
         Major Iteration Limit   =  15      *  (Default =  50)
         Minor Iteration Limit   =  10      *  (Default =  50)
      End
 
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
 
 Infinite bound size....  1.00D+25       COLD start.............
 Infinite step size.....  1.00D+25       EPS (machine precision)  1.11D-16
 Step limit.............  2.00D+00       Hessian................        NO
 
 Linear feasibility.....  1.05D-08       Crash tolerance........  1.00D-02
 Nonlinear feasibility..  1.05D-08       Optimality tolerance...  3.26D-12
 Line search tolerance..  9.00D-01       Function precision.....  4.37D-15
 
 Derivative level.......         3       Monitoring file........        -1
 Verify level...........        -1
 
 Major iterations limit.        15       Major print level......         1
 Minor iterations limit.        10       Minor print level......         0
 
 Workspace provided is     IWORK(     100),  WORK(    1000).
 To solve problem we need  IWORK(      17),  WORK(     185).
 
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
