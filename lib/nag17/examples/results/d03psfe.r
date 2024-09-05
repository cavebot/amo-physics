 D03PSF Example Program Results
 
 
 Example 1
 

  NPTS =   61 ATOL =   .100D-03 RTOL =   .100D-03

 T =   .100
 X            .2000    .3000    .4000    .5000    .6000    .7000    .8000
 Approx U     .0000    .1198    .9461    .1182    .0000    .0000    .0000

 T =   .200
 X            .2000    .3000    .4000    .5000    .6000    .7000    .8000
 Approx U     .0000    .0007    .1631    .9015    .1629    .0001    .0000

 T =   .300
 X            .2000    .3000    .4000    .5000    .6000    .7000    .8000
 Approx U     .0000    .0000    .0025    .1924    .8596    .1946    .0002

 Number of integration steps in time =     92
 Number of function evaluations =    443
 Number of Jacobian evaluations =    39
 Number of iterations =    231

 
 
 Example 2
 

 NPTS =   61 ATOL =   .500D-03 RTOL =   .500D-01

 T =   .200
        X        Approx U    Exact U

       .0000      1.0000      1.0000
       .3000       .9585      1.0000
       .4000       .0000       .0000
       .5000       .0000       .0000
       .6000       .0000       .0000
       .7000       .0000       .0000
      1.0000       .0000       .0000
 T =   .400
        X        Approx U    Exact U

       .0000      1.0000      1.0000
       .3000      1.0000      1.0000
       .4000      1.0000      1.0000
       .5000       .9767      1.0000
       .6000       .0000       .0000
       .7000       .0000       .0000
      1.0000       .0000       .0000

 Number of integration steps in time =    652
 Number of function evaluations =   1469
 Number of Jacobian evaluations =     1
 Number of iterations =      2

