 E04NDF Example Program Results
 
 
 Calls to E04NEF
 ---------------
 
       Infinite Bound Size = 1.0D+25
       Problem Type = QP2
 
 
 OPTIONS file
 ------------
 
      Begin   Example options file for  E04NDF
         Iteration Limit   =  30      *  (Default =  90)
      End
 
 *** E04NCF
 *** Start of NAG Library implementation details ***
 
 Implementation title: IBM RISC System/6000 AIX
            Precision: Double
         Product Code: FLIB617DA
                 Mark: 17A
 
 *** End of NAG Library implementation details ***
 
 Parameters
 ----------
 
 Problem type...........       QP2       Hessian................        NO
 
 Linear constraints.....         3       Feasibility tolerance..  1.05D-08
 Variables..............         9       Crash tolerance........  1.00D-02
 Objective matrix rows..         9       Rank tolerance.........  1.05D-07
 
 Infinite bound size....  1.00D+25       COLD start.............
 Infinite step size.....  1.00D+25       EPS (machine precision)  1.11D-16
 
 Print level............        10       Feasibility phase itns.        60
 Monitoring file........        -1       Optimality  phase itns.        30
 
 Workspace provided is     IWORK(     100),  WORK(    1000).
 To solve problem we need  IWORK(       9),  WORK(     270).
 
 Rank of the objective function data matrix =     5
 
 
 Itn     Step Ninf Sinf/Objective  Norm Gz
   0  0.0D+00    0   0.000000D+00  4.5D+00
   1  7.5D-01    0  -4.375000D+00  5.0D-01
   2  1.0D+00    0  -4.400000D+00  2.8D-17
   3  3.0D-01    0  -4.700000D+00  8.9D-01
   4  1.0D+00    0  -5.100000D+00  2.4D-17
   5  5.4D-01    0  -6.055714D+00  1.7D+00
   6  1.1D-02    0  -6.113326D+00  1.6D+00
   7  1.1D-01    0  -6.215049D+00  1.2D+00
   8  1.0D+00    0  -6.538008D+00  4.8D-17
   9  6.5D-01    0  -7.428704D+00  7.2D-02
  10  1.0D+00    0  -7.429717D+00  1.8D-17
  11  1.0D+00    0  -8.067718D+00  5.3D-17
  12  1.0D+00    0  -8.067778D+00  5.3D-17
 
 Exit from QP problem after    12 iterations.
 
 
 Varbl State     Value       Lower Bound   Upper Bound    Lagr Mult   Slack
 
 V   1    UL    2.00000      -2.00000       2.00000      -.8000         .
 V   2    FR   -.233333      -2.00000       2.00000           .       1.767
 V   3    FR   -.266667      -2.00000       2.00000           .       1.733
 V   4    FR   -.300000      -2.00000       2.00000           .       1.700
 V   5    FR   -.100000      -2.00000       2.00000           .       1.900
 V   6    UL    2.00000      -2.00000       2.00000      -.9000         .
 V   7    UL    2.00000      -2.00000       2.00000      -.9000         .
 V   8    FR   -1.77778      -2.00000       2.00000           .       .2222
 V   9    FR   -.455556      -2.00000       2.00000           .       1.544
 
 
 L Con State     Value       Lower Bound   Upper Bound    Lagr Mult   Slack
 
 L   1    UL    1.50000      -2.00000       1.50000     -6.6667E-02  8.8818E-16
 L   2    UL    1.50000      -2.00000       1.50000     -3.3333E-02 -4.4409E-16
 L   3    FR    3.93333      -2.00000       4.00000           .      6.6667E-02
 
 Exit E04NCF - Optimal QP solution.
 
 Final QP objective value =   -8.067778
