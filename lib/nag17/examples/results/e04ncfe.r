 E04NCF Example Program Results
 
 *** E04NCF
 *** Start of NAG Library implementation details ***
 
 Implementation title: IBM RISC System/6000 AIX
            Precision: Double
         Product Code: FLIB617DA
                 Mark: 17A
 
 *** End of NAG Library implementation details ***
 
 Parameters
 ----------
 
 Problem type...........       LS1       Hessian................        NO
 
 Linear constraints.....         3       Feasibility tolerance..  1.05D-08
 Variables..............         9       Crash tolerance........  1.00D-02
 Objective matrix rows..        10       Rank tolerance.........  1.11D-14
 
 Infinite bound size....  1.00D+20       COLD start.............
 Infinite step size.....  1.00D+20       EPS (machine precision)  1.11D-16
 
 Print level............        10       Feasibility phase itns.        60
 Monitoring file........        -1       Optimality  phase itns.        60
 
 Workspace provided is     IWORK(     100),  WORK(    1000).
 To solve problem we need  IWORK(       9),  WORK(     261).
 
 Rank of the objective function data matrix =     6
 
 
 Itn     Step Ninf Sinf/Objective  Norm Gz
   0  0.0D+00    1   2.145500D+00  0.0D+00
   1  2.5D-01    1   1.145500D+00  0.0D+00
   2  3.8D-01    0   6.595685D+00  2.2D+01
   3  1.8D-02    0   6.365562D+00  1.6D+01
   4  3.3D-01    0   2.869136D+00  8.5D+00
   5  1.4D-01    0   2.256269D+00  7.1D+00
   6  1.2D-01    0   1.899743D+00  5.4D+00
   7  1.1D-01    0   1.770819D+00  0.0D+00
   8  1.0D+00    0   1.390865D+00  5.3D-15
   9  1.9D-01    0   9.246136D-01  3.6D-01
  10  1.0D+00    0   9.225418D-01  5.3D-15
  11  6.1D-01    0   2.204146D-01  6.0D-01
  12  1.0D+00    0   1.652065D-01  9.0D-16
  13  1.0D+00    0   9.605160D-02  3.9D-15
  14  3.0D-02    0   9.236999D-02  4.5D-01
  15  1.0D+00    0   8.134082D-02  3.9D-15
 
 Exit from LS problem after    15 iterations.
 
 
 Varbl State     Value       Lower Bound   Upper Bound    Lagr Mult   Slack
 
 V   1    LL   0.000000E+00       .         2.00000       .1572         .
 V   2    FR   4.152607E-02       .         2.00000           .      4.1526E-02
 V   3    FR    .587176          None       2.00000           .       1.413
 V   4    LL   0.000000E+00       .         2.00000       .8782         .
 V   5    FR   9.964323E-02       .         2.00000           .      9.9643E-02
 V   6    LL   0.000000E+00       .         2.00000       .1473         .
 V   7    FR   4.905781E-02       .         2.00000           .      4.9058E-02
 V   8    LL   0.000000E+00       .         2.00000       .8603         .
 V   9    FR    .305649           .         2.00000           .       .3056
 
 
 L Con State     Value       Lower Bound   Upper Bound    Lagr Mult   Slack
 
 L   1    LL    2.00000       2.00000          None       .3777         .
 L   2    UL    2.00000          None       2.00000     -5.7914E-02     .
 L   3    LL    1.00000       1.00000       4.00000       .1075      4.4409E-16
 
 Exit E04NCF - Optimal LS solution.
 
 Final LS objective value =    .8134082E-01
