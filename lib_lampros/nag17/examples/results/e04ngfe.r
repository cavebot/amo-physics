 E04NGF Example Program Results
 
 
 Calls to E04NHF
 ---------------
 
       Print Level = 1
       Check Frequency = 10
       Crash Tolerance = 0.05
       Infinite Bound Size = 1.0D+25
 
 
 OPTIONS file
 ------------
 
      Begin   Example options file for  E04NGF
         Feasibility Phase Iteration Limit  =   5    *  (Default = 70)
         Optimality  Phase Iteration Limit  =  10    *  (Default = 70)
      End
 
 *** E04NFF
 *** Start of NAG Library implementation details ***
 
 Implementation title: IBM RISC System/6000 AIX
            Precision: Double
         Product Code: FLIB617DA
                 Mark: 17A
 
 *** End of NAG Library implementation details ***
 
 Parameters
 ----------
 
 Problem type...........       QP2
 
 Linear constraints.....         7       Feasibility tolerance..  1.05D-08
 Variables..............         7       Crash tolerance........  5.00D-02
 Hessian rows...........         7       Rank tolerance.........  1.11D-14
 
 Infinite bound size....  1.00D+25       COLD start.............
 Infinite step size.....  1.00D+25       EPS (machine precision)  1.11D-16
 
 Check frequency........        10       Expand frequency.......         5
 Minimum sum of infeas..        NO
 
 Max degrees of freedom.         7       Print level............         1
 Feasibility phase itns.         5       Monitoring file........        -1
 Optimality  phase itns.        10
 
 Workspace provided is     IWORK(    1000),  WORK(   10000).
 To solve problem we need  IWORK(      17),  WORK(     189).
 
 
 Varbl State     Value       Lower Bound   Upper Bound    Lagr Mult   Slack
 
 V   1    LL  -1.000000E-02 -1.000000E-02  1.000000E-02   .4700         .
 V   2    FR  -6.986465E-02  -.100000       .150000           .      3.0135E-02
 V   3    FR   1.825915E-02 -1.000000E-02  3.000000E-02       .      1.1741E-02
 V   4    FR  -2.426081E-02 -4.000000E-02  2.000000E-02       .      1.5739E-02
 V   5    FR  -6.200564E-02  -.100000      5.000000E-02       .      3.7994E-02
 V   6    FR   1.380544E-02 -1.000000E-02      None           .      2.3805E-02
 V   7    FR   4.066496E-03 -1.000000E-02      None           .      1.4066E-02
 
 
 L Con State     Value       Lower Bound   Upper Bound    Lagr Mult   Slack
 
 L   1    EQ   -.130000      -.130000      -.130000      -1.908         .
 L   2    FR  -5.879898E-03      None     -4.900000E-03       .      9.7990E-04
 L   3    UL  -6.400000E-03      None     -6.400000E-03  -.3144         .
 L   4    FR  -4.537323E-03      None     -3.700000E-03       .      8.3732E-04
 L   5    FR  -2.915996E-03      None     -1.200000E-03       .      1.7160E-03
 L   6    LL  -9.920000E-02 -9.920000E-02      None       1.955         .
 L   7    LL  -3.000000E-03 -3.000000E-03  2.000000E-03   1.972      8.6736E-19
 
 Exit E04NFF - Optimal QP solution.
 
 Final QP objective value =    .3703165E-01
 
 Exit from QP problem after     8 iterations.
