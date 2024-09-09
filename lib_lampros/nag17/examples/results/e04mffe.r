 E04MFF Example Program Results
 
 *** E04MFF
 *** Start of NAG Library implementation details ***
 
 Implementation title: IBM RISC System/6000 AIX
            Precision: Double
         Product Code: FLIB617DA
                 Mark: 17A
 
 *** End of NAG Library implementation details ***
 
 Parameters
 ----------
 
 Problem type...........        LP
 
 Linear constraints.....         7       Feasibility tolerance..  1.05D-08
 Variables..............         7       Crash tolerance........  1.00D-02
 
 Infinite bound size....  1.00D+20       COLD start.............
 Infinite step size.....  1.00D+20       EPS (machine precision)  1.11D-16
 
 Check frequency........        50       Expand frequency.......         5
 Minimum sum of infeas..        NO
 
 Print level............        10       Iteration limit........        70
 Monitoring file........        -1
 
 Workspace provided is     IWORK(    1000),  WORK(   10000).
 To solve problem we need  IWORK(      17),  WORK(     182).
 
 
  Itn     Step Ninf Sinf/Objective  Norm Gz
    0  0.0D+00    3   1.038000D-01  0.0D+00
    1  4.1D-02    1   3.000000D-02  0.0D+00
    2  4.2D-02    0   3.500000D-02  0.0D+00
    3  1.9D-01    0   3.090243D-02  0.0D+00
    4  1.5D-01    0   2.989710D-02  0.0D+00
    5  3.7D-01    0   2.725725D-02  0.0D+00
    6  6.5D-01    0   2.403774D-02  0.0D+00
    7  4.6D+00    0   2.359648D-02  0.0D+00
 
 
 Varbl State     Value       Lower Bound   Upper Bound    Lagr Mult   Slack
 
 V   1    LL  -1.000000E-02 -1.000000E-02  1.000000E-02   .3301         .
 V   2    LL   -.100000      -.100000       .150000      1.4384E-02     .
 V   3    UL   3.000000E-02 -1.000000E-02  3.000000E-02 -9.0997E-02     .
 V   4    UL   2.000000E-02 -4.000000E-02  2.000000E-02 -7.6612E-02     .
 V   5    FR  -6.748534E-02  -.100000      5.000000E-02       .      3.2515E-02
 V   6    FR  -2.280130E-03 -1.000000E-02      None           .      7.7199E-03
 V   7    FR  -2.345277E-04 -1.000000E-02      None           .      9.7655E-03
 
 
 L Con State     Value       Lower Bound   Upper Bound    Lagr Mult   Slack
 
 L   1    EQ   -.130000      -.130000      -.130000      -1.431     -2.7756E-17
 L   2    FR  -5.479544E-03      None     -4.900000E-03       .      5.7954E-04
 L   3    FR  -6.571922E-03      None     -6.400000E-03       .      1.7192E-04
 L   4    FR  -4.849707E-03      None     -3.700000E-03       .      1.1497E-03
 L   5    FR  -3.874853E-03      None     -1.200000E-03       .      2.6749E-03
 L   6    LL  -9.920000E-02 -9.920000E-02      None       1.501     -4.1633E-17
 L   7    LL  -3.000000E-03 -3.000000E-03  2.000000E-03   1.517         .
 
 Exit E04MFF - Optimal LP solution.
 
 Final LP objective value =    .2359648E-01
 
 Exit from LP problem after     7 iterations.
