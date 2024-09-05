 E04UQF Example Program Results
 
 
 Calls to E04URF
 ---------------
 
       Infinite Bound Size = 1.0D+25
       Print Level = 1
       Verify Level = -1
 
 
 OPTIONS file
 ------------
 
      Begin   Example options file for  E04UQF
         Major Iteration Limit   =  15      *  (Default =  50)
         Minor Iteration Limit   =  10      *  (Default =  50)
      End
 
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
 
 J'J initial Hessian....                 Reset frequency........         2
 
 Workspace provided is     IWORK(     100),  WORK(    1000).
 To solve problem we need  IWORK(       9),  WORK(     306).
 
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
