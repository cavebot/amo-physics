 H02BBF Example Program Results
 
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
 Variables...............         2        Max depth of the tree...         4
 
 Feasibility tolerance...  1.05D-08        Print level.............        10
 Infinite bound size.....  1.00D+20        EPS (machine precision).  1.11D-16
 
 Integer feasibility tol.  1.00D-05        Iteration limit.........        50
 Max number of nodes.....      NONE
 
 ** Workspace provided with MAXDPT =     4: LRWORK =  1000  LIWORK =  1000
 ** Workspace required with MAXDPT =     4: LRWORK =    86  LIWORK =   137
 
 
   *** Optimum LP solution ***   -17.50000
 
 
 Varbl State     Value     Lower Bound   Upper Bound    Lagr Mult   Residual
 
 V  1    FR    3.92857      0.000000E+00     None       0.0000E+00   3.929
 V  2    FR    1.42857      0.000000E+00     None       0.0000E+00   1.429
 
 
 L Con State     Value     Lower Bound   Upper Bound    Lagr Mult   Residual
 
 L  1    UL    15.0000         None        15.0000      -1.000      0.0000E+00
 L  2    UL    5.00000         None        5.00000      -.5000     -8.8818E-16
 L  3    FR    14.6429       5.00000         None       0.0000E+00   9.643
 
 
   *** Start of tree search ***
 
 
  Node  Parent    Obj       Varbl  Value      Lower     Upper     Value    Depth
   No    Node    Value      Chosen Before     Bound     Bound     After
    2     1    No Feas Soln    1    3.93      4.00       None      4.00       1
    3     1     -16.2          1    3.93      .000E+00   3.00      3.00       1
    4     3     -15.5          2    1.80      2.00       None      2.00       2
    5     3     -13.0          2    1.80      .000E+00   1.00      1.00       2
      *** Integer solution ***
 
 
  Node  Parent    Obj       Varbl  Value      Lower     Upper     Value    Depth
   No    Node    Value      Chosen Before     Bound     Bound     After
    6     4    No Feas Soln    1    2.50      3.00       3.00      3.00       3
    7     4     -14.8          1    2.50      .000E+00   2.00      2.00       3
    8     7     -12.0     CO   2    2.20      3.00       None      3.00       4
    9     7     -14.0          2    2.20      2.00       2.00      2.00       4
      *** Integer solution ***
 
     *** End of tree search ***
 
 
 Total of     9 nodes investigated.
 
 Exit H02BBF - Optimum IP solution found.
 
 Final IP objective value =   -14.00000
 
 
 
 Varbl State     Value     Lower Bound   Upper Bound    Lagr Mult   Residual
 
 V  1    UL    2.00000      0.000000E+00   2.00000      -3.000      0.0000E+00
 V  2    EQ    2.00000       2.00000       2.00000      -4.000      0.0000E+00
 
 
 L Con State     Value     Lower Bound   Upper Bound    Lagr Mult   Residual
 
 L  1    FR    14.0000         None        15.0000      0.0000E+00   1.000
 L  2    FR   0.000000E+00     None        5.00000      0.0000E+00   5.000
 L  3    FR    10.0000       5.00000         None       0.0000E+00   5.000
