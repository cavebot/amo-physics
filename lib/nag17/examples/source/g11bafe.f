*     G11BAF Example Program Text
*     Mark 17 Release. NAG Copyright 1995.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          NMAX, MMAX, LTMAX
      PARAMETER        (NMAX=54,MMAX=3,LTMAX=18)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J, K, LDF, MAXT, N, NCELLS, NCOL, NDIM,
     +                 NFAC, NROW
      CHARACTER        STAT, WEIGHT
*     .. Local Arrays ..
      DOUBLE PRECISION AUXT(2*LTMAX), TABLE(LTMAX), WT(NMAX), Y(NMAX)
      INTEGER          ICOUNT(LTMAX), IDIM(MMAX), IFAC(NMAX,MMAX),
     +                 ISF(MMAX), IWK(2*MMAX), LFAC(MMAX)
*     .. External Subroutines ..
      EXTERNAL         G11BAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G11BAF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) STAT, WEIGHT, N, NFAC
      IF (N.LE.NMAX .AND. NFAC.LE.MMAX) THEN
         IF (WEIGHT.EQ.'W' .OR. WEIGHT.EQ.'w' .OR. WEIGHT.EQ.'V' .OR.
     +       WEIGHT.EQ.'v') THEN
            DO 20 I = 1, N
               READ (NIN,*) (IFAC(I,J),J=1,NFAC), Y(I), WT(I)
   20       CONTINUE
         ELSE
            DO 40 I = 1, N
               READ (NIN,*) (IFAC(I,J),J=1,NFAC), Y(I)
   40       CONTINUE
         END IF
         READ (NIN,*) (LFAC(J),J=1,NFAC)
         READ (NIN,*) (ISF(J),J=1,NFAC)
         LDF = NMAX
         MAXT = LTMAX
         IFAIL = 0
*
         CALL G11BAF(STAT,'I',WEIGHT,N,NFAC,ISF,LFAC,IFAC,LDF,Y,WT,
     +               TABLE,MAXT,NCELLS,NDIM,IDIM,ICOUNT,AUXT,IWK,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) ' TABLE'
         WRITE (NOUT,*)
         NCOL = IDIM(NDIM)
         NROW = NCELLS/NCOL
         K = 1
         DO 60 I = 1, NROW
            WRITE (NOUT,99999) (TABLE(J),'(',ICOUNT(J),')',J=K,
     +        K+NCOL-1)
            K = K + NCOL
   60    CONTINUE
      END IF
      STOP
*
99999 FORMAT (1X,6(F8.2,A,I2,A))
      END
