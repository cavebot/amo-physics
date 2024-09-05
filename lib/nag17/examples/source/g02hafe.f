*     G02HAF Example Program Text
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, MMAX
      PARAMETER        (NMAX=8,MMAX=3)
*     .. Local Scalars ..
      DOUBLE PRECISION CPSI, CUCV, DCHI, H1, H2, H3, SIGMA, TOL
      INTEGER          I, IC, IFAIL, INDC, INDW, IPSI, ISIGMA, IX, J, M,
     +                 MAXIT, N, NITMON
*     .. Local Arrays ..
      DOUBLE PRECISION C(MMAX,MMAX), RS(NMAX), THETA(MMAX), WGT(NMAX),
     +                 WORK(4*NMAX+MMAX*(NMAX+MMAX)), X(NMAX,MMAX),
     +                 Y(NMAX)
*     .. External Subroutines ..
      EXTERNAL         G02HAF, X04ABF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G02HAF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      CALL X04ABF(1,NOUT)
*     Read in number of observations and number of X variables
      READ (NIN,*) N, M
      WRITE (NOUT,*)
      IF (N.GT.0 .AND. N.LE.NMAX .AND. M.GT.0 .AND. M.LE.MMAX) THEN
*        Read in X and Y
         DO 20 I = 1, N
            READ (NIN,*) (X(I,J),J=1,M), Y(I)
   20    CONTINUE
*        Read in control parameters
         READ (NIN,*) INDW, IPSI, ISIGMA
*        Read in appropriate weight function parameters.
         IF (INDW.NE.0) READ (NIN,*) CUCV, INDC
         IF (IPSI.GT.0) THEN
            IF (IPSI.EQ.1) READ (NIN,*) CPSI
            IF (IPSI.EQ.2) READ (NIN,*) H1, H2, H3
            IF (ISIGMA.GT.0) READ (NIN,*) DCHI
         END IF
*        Set values of remaining parameters
         IX = NMAX
         IC = MMAX
         TOL = 0.5D-4
         MAXIT = 50
*        * Change NITMON to a positive value if monitoring information
*          is required *
         NITMON = 0
         SIGMA = 1.0D0
         DO 40 I = 1, M
            THETA(I) = 0.0D0
   40    CONTINUE
         IFAIL = -1
*
         CALL G02HAF(INDW,IPSI,ISIGMA,INDC,N,M,X,IX,Y,CPSI,H1,H2,H3,
     +               CUCV,DCHI,THETA,SIGMA,C,IC,RS,WGT,TOL,MAXIT,NITMON,
     +               WORK,IFAIL)
*
         IF ((IFAIL.NE.0) .AND. (IFAIL.LT.7)) THEN
            WRITE (NOUT,99999) 'G02HAF fails, IFAIL = ', IFAIL
         ELSE
            IF (IFAIL.GE.7) THEN
               WRITE (NOUT,99999) 'G02HAF returned IFAIL = ', IFAIL
               WRITE (NOUT,*)
     +          '       Some of the following results may be unreliable'
            END IF
            WRITE (NOUT,99998) 'Sigma = ', SIGMA
            WRITE (NOUT,*)
            WRITE (NOUT,*) '       THETA      Standard'
            WRITE (NOUT,*) '                   errors'
            DO 60 J = 1, M
               WRITE (NOUT,99997) THETA(J), C(J,J)
   60       CONTINUE
            WRITE (NOUT,*)
            WRITE (NOUT,*) '     Weights     Residuals'
            DO 80 I = 1, N
               WRITE (NOUT,99997) WGT(I), RS(I)
   80       CONTINUE
         END IF
      END IF
      STOP
*
99999 FORMAT (1X,A,I2)
99998 FORMAT (1X,A,F10.4)
99997 FORMAT (1X,F12.4,F13.4)
      END
