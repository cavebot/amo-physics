 H02BFF Example Program Results
 
 *** H02BBF
 *** Start of NAG Library implementation details ***
 
 Implementation title: IBM RISC System/6000 AIX
            Precision: Double
         Product Code: FLIB617DA
                 Mark: 17A
 
 *** End of NAG Library implementation details ***
 
 Parameters
 ----------
 
 Linear constraints......         3        First integer solution..       OFF
 Variables...............         6        Max depth of the tree...        75
 
 Feasibility tolerance...  1.05D-08        Print level.............         5
 Infinite bound size.....  1.00D+20        EPS (machine precision).  1.11D-16
 
 Integer feasibility tol.  1.00D-05        Iteration limit.........        50
 Max number of nodes.....      NONE
 
 ** Workspace provided with MAXDPT =    75: LRWORK = 10100  LIWORK =  9679
 ** Workspace required with MAXDPT =    75: LRWORK =   746  LIWORK =  2587
 
 
   *** Optimum LP solution ***    92.50000
 
 
   *** Start of tree search ***
 
 
  Node  Parent    Obj       Varbl  Value      Lower     Upper     Value    Depth
   No    Node    Value      Chosen Before     Bound     Bound     After
    2     1      93.2          4    4.50      5.00       8.00      5.00       1
    3     1      93.8          4    4.50      .000E+00   4.00      4.00       1
    4     2      94.8          5    1.81      2.00       2.00      2.00       2
    5     2      96.1          5    1.81      .000E+00   1.00      1.00       2
    6     3      96.9          6    .308      1.00       2.00      1.00       2
    7     3      94.5          6    .308      .000E+00   .000E+00  .000E+00   2
    8     7      96.5          3    .500      1.00       2.00      1.00       3
    9     7      97.4          3    .500      .000E+00   .000E+00  .000E+00   3
   10     4      97.0          1    3.27      4.00       4.00      4.00       3
      *** Integer solution ***
 
 
  Node  Parent    Obj       Varbl  Value      Lower     Upper     Value    Depth
   No    Node    Value      Chosen Before     Bound     Bound     After
   11     4      95.7          1    3.27      .000E+00   3.00      3.00       3
   12    11      99.5     CO   4    5.19      6.00       8.00      6.00       4
   13    11      96.2          4    5.19      5.00       5.00      5.00       4
   14     5      97.3     CO   4    7.12      8.00       8.00      8.00       3
   15     5      96.5          4    7.12      5.00       7.00      7.00       3
   16    13      107.     CO   6    .115      1.00       2.00      1.00       5
   17    13      96.4          6    .115      .000E+00   .000E+00  .000E+00   5
   18    17      103.     CO   3    .188      1.00       2.00      1.00       6
   19    17      97.5     CO   3    .188      .000E+00   .000E+00  .000E+00   6
   20    15      100.     CO   6    .769E-01  1.00       2.00      1.00       4
   21    15      96.6          6    .769E-01  .000E+00   .000E+00  .000E+00   4
   22     8      97.2     CO   4    3.50      4.00       4.00      4.00       4
   23     8      98.5     CO   4    3.50      .000E+00   3.00      3.00       4
   24    21      100.     CO   3    .125      1.00       2.00      1.00       5
   25    21      97.3     CO   3    .125      .000E+00   .000E+00  .000E+00   5
   26     6      97.0     CO   4    2.88      3.00       4.00      3.00       3
   27     6      105.     CO   4    2.88      .000E+00   2.00      2.00       3
     *** End of tree search ***
 
 
 Total of    27 nodes investigated.
 
 Exit H02BBF - Optimum IP solution found.
 
 Final IP objective value =    97.00000
 
 
 
 Varbl   State     Value     Lower Bound   Upper Bound    Lagr Mult   Residual
 
 OATMEAL  EQ    4.00000       4.00000       4.00000       3.000      0.0000E+00
 CHICKEN  LL   0.000000E+00  0.000000E+00   3.00000       24.00      0.0000E+00
 EGGS     LL   0.000000E+00  0.000000E+00   2.00000       13.00      0.0000E+00
 MILK     LL    5.00000       5.00000       8.00000       9.000      0.0000E+00
 PIE      EQ    2.00000       2.00000       2.00000       20.00      0.0000E+00
 BACON    LL   0.000000E+00  0.000000E+00   2.00000       19.00      0.0000E+00
 
 
 L Con   State     Value     Lower Bound   Upper Bound    Lagr Mult   Residual
 
 ENERGY   FR    2080.00       2000.00         None       0.0000E+00   80.00
 PROTEIN  FR    64.0000       55.0000         None       0.0000E+00   9.000
 CALCIUM  FR    1477.00       800.000         None       0.0000E+00   677.0
