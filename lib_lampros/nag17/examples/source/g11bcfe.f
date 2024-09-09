*     G11BCF Example Program Text
*     Mark 17 Release. NAG Copyright 1995.
*     .. Parameters ..
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
      INTEGER          MMAX, LTMAX
      PARAMETER        (MMAX=5,LTMAX=54)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, J, K, MAXST, MCELLS, MDIM, NCELLS,
     +                 NCOL, NDIM, NROW
      CHARACTER        STAT
*     .. Local Arrays ..
      DOUBLE PRECISION AUXT(LTMAX), STABLE(LTMAX), TABLE(LTMAX),
     +                 WK(LTMAX)
      INTEGER          IDIM(MMAX), ISDIM(MMAX), IWK(3*MMAX),
     +                 MLEVEL(MMAX)
*     .. External Subroutines ..
      EXTERNAL         G11BCF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G11BCF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) STAT, NCELLS, NDIM
      IF (NCELLS.LE.LTMAX .AND. NDIM.LT.MMAX) THEN
         READ (NIN,*) (TABLE(I),I=1,NCELLS)
         READ (NIN,*) (IDIM(J),J=1,NDIM)
         READ (NIN,*) (ISDIM(J),J=1,NDIM)
         MAXST = LTMAX
         IFAIL = 0
*
         CALL G11BCF(STAT,TABLE,NCELLS,NDIM,IDIM,ISDIM,STABLE,MAXST,
     +               MCELLS,MDIM,MLEVEL,AUXT,IWK,WK,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) ' Marginal Table'
         WRITE (NOUT,*)
         NCOL = MLEVEL(MDIM)
         NROW = MCELLS/NCOL
         K = 1
         DO 20 I = 1, NROW
            WRITE (NOUT,99999) (STABLE(J),J=K,K+NCOL-1)
            K = K + NCOL
   20    CONTINUE
      END IF
      STOP
*
99999 FORMAT (10F8.2)
      END
