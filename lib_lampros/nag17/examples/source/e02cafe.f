*     E02CAF Example Program Text.
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, MTMAX, NA, NWORK
      PARAMETER        (NMAX=20,MTMAX=400,NA=100,NWORK=1500)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION YMAX
      INTEGER          I, IFAIL, J, K, L, MI, MJ, N, R, T
*     .. Local Arrays ..
      DOUBLE PRECISION A(NA), F(MTMAX), FF(MTMAX), W(MTMAX),
     +                 WORK(NWORK), X(MTMAX), XMAX(NMAX), XMIN(NMAX),
     +                 Y(NMAX)
      INTEGER          M(20)
*     .. External Subroutines ..
      EXTERNAL         E02CAF, E02CBF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'E02CAF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
*     Input the number of lines Y = Y(I) on which data is given,
*     and the required degree of fit in the X and Y directions
   20 READ (NIN,*,END=120) N, K, L
      WRITE (NOUT,*)
      IF (N.GT.0 .AND. N.LE.NMAX) THEN
         MJ = 0
*        Input Y(I), the number of data points on Y = Y(I) and the
*        range of X-values on this line, for I = 1,2,...N
         DO 40 I = 1, N
            READ (NIN,*) Y(I), MI, XMIN(I), XMAX(I)
            M(I) = MI
            MJ = MJ + MI
   40    CONTINUE
*        Terminate program if the arrays have not been declared
*        large enough to contain the data
         IF (MTMAX.LT.MJ) THEN
            WRITE (NOUT,99999)
     +        'MTOT is too small. It should be at least ', MJ
            STOP
         END IF
*        Input the X-values and function values, F, together with
*        their weights, W.
         READ (NIN,*) (X(I),F(I),W(I),I=1,MJ)
*        Evaluate the coefficients, A, of the fit to this set of data
         IFAIL = 0
*
         CALL E02CAF(M,N,K,L,X,Y,F,W,MTMAX,A,NA,XMIN,XMAX,Y,1,Y,1,WORK,
     +               NWORK,IFAIL)
*
         MI = 0
         WRITE (NOUT,*)
     +     '     Data Y     Data X     Data F   Fitted F   Residual'
         WRITE (NOUT,*)
         DO 80 R = 1, N
            T = MI + 1
            MI = MI + M(R)
            YMAX = Y(N)
            IF (N.EQ.1) YMAX = YMAX + 1.0D0
*           Evaluate the fitted polynomial at each of the data points
*           on the line Y = Y(R)
            IFAIL = 0
*
            CALL E02CBF(T,MI,K,L,X,XMIN(R),XMAX(R),Y(R),Y(1),YMAX,FF,A,
     +                  NA,WORK,NWORK,IFAIL)
*
*           Output the data and fitted values on the line Y = Y(R)
            DO 60 I = T, MI
               WRITE (NOUT,99998) Y(R), X(I), F(I), FF(I), FF(I) - F(I)
   60       CONTINUE
            WRITE (NOUT,*)
   80    CONTINUE
*        Output the Chebyshev coefficients of the fit
         WRITE (NOUT,*) 'Chebyshev coefficients of the fit'
         WRITE (NOUT,*)
         DO 100 J = 1, K + 1
            WRITE (NOUT,99997) (A(I),I=1+(J-1)*(L+1),J*(L+1))
  100    CONTINUE
         GO TO 20
      END IF
  120 STOP
*
99999 FORMAT (1X,A,I5)
99998 FORMAT (1X,4F11.4,D11.2)
99997 FORMAT (1X,6F11.4)
      END
