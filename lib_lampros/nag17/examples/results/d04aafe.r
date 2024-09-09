 D04AAF Example Program Results
 
 Four separate runs to calculate the first four odd order derivatives of
    FUN(X) = 0.5*exp(2.0*X-1.0) at X = 0.5.
 The exact results are 1, 4, 16 and 64
 
 Input parameters common to all four runs
    XVAL = 0.5    NDER = -7    IFAIL = 0
 
 
 with step length    .5000  the results are
 Order        Derivative       Error estimate
  1            .1392D+04           -.1073D+06
  3           -.3139D+04           -.1438D+06
  5            .8762D+04           -.2479D+06
  7           -.2475D+05           -.4484D+06
 
 with step length    .0500  the results are
 Order        Derivative       Error estimate
  1            .1000D+01            .1530D-10
  3            .4000D+01            .2113D-08
  5            .1600D+02            .3815D-06
  7            .6400D+02            .7385D-04
 
 with step length    .0050  the results are
 Order        Derivative       Error estimate
  1            .1000D+01            .1221D-13
  3            .4000D+01            .4208D-09
  5            .1600D+02            .1450D-04
  7            .6404D+02            .2973D+00
 
 with step length    .0005  the results are
 Order        Derivative       Error estimate
  1            .1000D+01            .1422D-12
  3            .4000D+01            .3087D-06
  5            .1599D+02            .6331D+00
  7            .3825D+05           -.1964D+07
