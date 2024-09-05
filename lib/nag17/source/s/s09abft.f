      DOUBLE PRECISION FUNCTION S09ABF(X,IFAIL)
C     MARK 5A REVISED - NAG COPYRIGHT 1976
C     MARK 11.5(F77) REVISED. (SEPT 1985.)
C     ARCCOS(X)
C
C     .. Parameters ..
      CHARACTER*6                      SRNAME
      PARAMETER                        (SRNAME='S09ABF')
C     .. Scalar Arguments ..
      DOUBLE PRECISION                 X
      INTEGER                          IFAIL
C     .. Local Scalars ..
      DOUBLE PRECISION                 PI, PIB2, T, U, Y
      LOGICAL                          UPRANG
C     .. Local Arrays ..
      CHARACTER*1                      P01REC(1)
C     .. External Functions ..
      INTEGER                          P01ABF
      EXTERNAL                         P01ABF
C     .. Intrinsic Functions ..
      INTRINSIC                        ABS, SQRT
C     .. Data statements ..
C     PRECISION DEPENDENT CONSTANTS
C08   DATA PIB2,PI/1.5707963D0,3.1415927D0/
C12   DATA PIB2,PI/1.57079632679D0,3.14159265359D0/
C14   DATA PIB2,PI/1.5707963267949D0,3.1415926535898D0/
      DATA PIB2,PI/1.570796326794897D0,3.141592653589793D0/
C18   DATA PIB2,PI/1.57079632679489662D0,3.14159265358979324D0/
C     .. Executable Statements ..
C
C     ERROR TEST
      IF (ABS(X).GT.1.0D0) GO TO 40
C
C     RANGE REDUCTION
      U = X*X
      UPRANG = U .GT. 0.5D0
      IF (UPRANG) U = 1.0D0 - U
C
C     EXPANSION ARGUMENT
      T = 4.0D0*U - 1.0D0
C
C      * EXPANSION (0011) *
C
C     EXPANSION (0011) EVALUATED AS Y(T)  --PRECISION 08E
C08   Y = ((((((((+1.9318271D-6)*T+6.8335509D-6)*T+2.1010686D-5)
C08  *    *T+8.2136899D-5)*T+3.4098621D-4)*T+1.5199201D-3)
C08  *    *T+7.7988867D-3)*T+5.3751474D-2)*T + 1.0471976D+0
C
C     EXPANSION (0011) EVALUATED AS Y(T)  --PRECISION 12E
C12   Y = (((((((((((((+4.51310991043D-9)*T+1.47970636519D-8)
C12  *    *T+3.43362913094D-8)*T+1.19837848470D-7)
C12  *    *T+4.41902724535D-7)*T+1.57119463187D-6)
C12  *    *T+5.70107084616D-6)*T+2.13440418692D-5)
C12  *    *T+8.30233296866D-5)*T+3.40863978572D-4)
C12  *    *T+1.51966852540D-3)*T+7.79890222287D-3)
C12  *    *T+5.37514935917D-2)*T + 1.04719755120D+0
C
C     EXPANSION (0011) EVALUATED AS Y(T)  --PRECISION 14E
C14   Y = (((((((((((((((+4.3026937720591D-10)*T+1.3883500576116D-9)
C14  *    *T+2.8995997459059D-9)*T+9.9378384502835D-9)
C14  *    *T+3.6756556556196D-8)*T+1.2651928312197D-7)
C14  *    *T+4.4005391080501D-7)*T+1.5666391082459D-6)
C14  *    *T+5.7018271790496D-6)*T+2.1345636302489D-5)
C14  *    *T+8.3023170856665D-5)*T+3.4086371283347D-4)
C14  *    *T+1.5196685401067D-3)*T+7.7989022394820D-3)
C14  *    *T+5.3751493591316D-2)*T + 1.0471975511966D+0
C
C     EXPANSION (0011) EVALUATED AS Y(T)  --PRECISION 16E
      Y = ((((((((((((((((+4.210346761190271D-11
     *    *T+1.342121568282535D-10)*T+2.513296398553196D-10)
     *    *T+8.515014302985799D-10)*T+3.212744286269388D-9)
     *    *T+1.081021746966715D-8)*T+3.646577948300129D-8)
     *    *T+1.257811162594110D-7)*T+4.402076871418002D-7)
     *    *T+1.566985123962741D-6)*T+5.701781046148566D-6)
     *    *T+2.134554822576075D-5)*T+8.302317819598986D-5)
     *    *T+3.408637238430600D-4)*T+1.519668539582420D-3)
     *    *T+7.798902238957732D-3)*T+5.375149359132719D-2)*T +
     *     1.047197551196598D+0
C
C     EXPANSION (0011) EVALUATED AS Y(T)  --PRECISION 18E
C18   Y = (((((((((((((((+1.33711797207485645D-12
C18  *    *T+4.20458578523455488D-12)*T+6.58956868182959718D-12)
C18  *    *T+2.21316851320385700D-11)*T+8.86808218416313467D-11)
C18  *    *T+2.91273204815047852D-10)*T+9.46796605847489446D-10)
C18  *    *T+3.16905601209468502D-9)*T+1.07088463647073773D-8)
C18  *    *T+3.64941768612148445D-8)*T+1.25842626704542897D-7)
C18  *    *T+4.40196531028930638D-7)*T+1.56696342252377888D-6)
C18  *    *T+5.70178362063615142D-6)*T+2.13455525023407982D-5)
C18  *    *T+8.30231778741789102D-5)*T+3.40863723422356588D-4
C18   Y = (((Y*T+1.51966853960070431D-3)*T+7.79890223897362983D-3)
C18  *    *T+5.37514935913268831D-2)*T + 1.04719755119659775D+0
C
C
      IF (UPRANG) GO TO 20
C
      S09ABF = PIB2 - X*Y
      IFAIL = 0
      RETURN
C
   20 S09ABF = SQRT(U)*Y
      IF (X.LT.0.0D0) S09ABF = PI - S09ABF
      IFAIL = 0
      RETURN
C
C     ERROR EXIT
   40 IFAIL = P01ABF(IFAIL,1,SRNAME,0,P01REC)
      S09ABF = 0.0D0
      RETURN
C
      END
