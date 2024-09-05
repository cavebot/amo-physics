 H02BUF Example Program Results
 
 
 Calls to E04MHF
 ---------------
 
      Print Level = 5
 
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
 
 Linear constraints.....         3       Feasibility tolerance..  1.05D-08
 Variables..............         6       Crash tolerance........  1.00D-02
 
 Infinite bound size....  1.00D+20       COLD start.............
 Infinite step size.....  1.00D+20       EPS (machine precision)  1.11D-16
 
 Check frequency........        50       Expand frequency.......         5
 Minimum sum of infeas..        NO
 
 Print level............         5       Iteration limit........        50
 Monitoring file........        -1
 
 Workspace provided is     IWORK(     103),  WORK(    5802).
 To solve problem we need  IWORK(      15),  WORK(      89).
 
 
  Itn     Step Ninf Sinf/Objective  Norm Gz
    0  0.0D+00    3   1.799000D+03  0.0D+00
    1  1.5D-02    1   2.550000D+02  0.0D+00
    2  1.4D-03    0   1.271429D+02  0.0D+00
    3  8.7D-02    0   1.129048D+02  0.0D+00
    4  2.1D-01    0   1.062857D+02  0.0D+00
    5  1.9D+00    0   9.733333D+01  0.0D+00
    6  2.9D+00    0   9.250000D+01  0.0D+00
 
 Exit E04MFF - Optimal LP solution.
 
 Final LP objective value =    92.50000
 
 Exit from LP problem after     6 iterations.
 
 
 Varbl   State     Value     Lower Bound   Upper Bound    Lagr Mult   Residual
 
 OATMEAL  UL    4.00000      0.000000E+00   4.00000      -3.188      0.0000E+00
 CHICKEN  LL   0.000000E+00  0.000000E+00   3.00000       12.47      0.0000E+00
 EGGS     LL   0.000000E+00  0.000000E+00   2.00000       4.000      0.0000E+00
 MILK     FR    4.50000      0.000000E+00   8.00000      0.0000E+00   3.500
 PIE      UL    2.00000      0.000000E+00   2.00000      -3.625      0.0000E+00
 BACON    LL   0.000000E+00  0.000000E+00   2.00000       4.375      0.0000E+00
 
 
 L Con   State     Value     Lower Bound   Upper Bound    Lagr Mult   Residual
 
 ENERGY   LL    2000.00       2000.00         None       5.6250E-02 -2.2737E-13
 PROTEIN  FR    60.0000       55.0000         None       0.0000E+00   5.000
 CALCIUM  FR    1334.50       800.000         None       0.0000E+00   534.5
