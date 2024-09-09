*     G02ECF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, MMAX, LMAX
      PARAMETER        (NMAX=20,MMAX=6,LMAX=32)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION SIGSQ, TSS
      INTEGER          I, IFAIL, II, J, M, N, NMOD
*     .. Local Arrays ..
      DOUBLE PRECISION CP(LMAX), RSQ(LMAX), RSS(LMAX),
     +                 WK(NMAX*(MMAX+1)), WT(NMAX), X(NMAX,MMAX),
     +                 Y(NMAX)
      INTEGER          ISX(MMAX), MRANK(LMAX), NTERMS(LMAX)
      CHARACTER*3      MODEL(LMAX,MMAX), NAME(MMAX)
*     .. External Subroutines ..
      EXTERNAL         G02EAF, G02ECF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G02ECF Example Program Results'
*     Skip heading in data file
      READ (NIN,*)
      READ (NIN,*) N, M
      IF (M.LE.MMAX .AND. N.LE.NMAX) THEN
         DO 20 I = 1, N
            READ (NIN,*) (X(I,J),J=1,M), Y(I)
   20    CONTINUE
         READ (NIN,*) (ISX(J),J=1,M)
         READ (NIN,*) (NAME(J),J=1,M)
         IFAIL = 0
*
*        Calculate residual sums of squares using G02EAF
         CALL G02EAF('M','U',N,M,X,NMAX,NAME,ISX,Y,WT,NMOD,MODEL,LMAX,
     +               RSS,NTERMS,MRANK,WK,IFAIL)
*
         TSS = RSS(1)
         SIGSQ = RSS(NMOD)/(N-NTERMS(NMOD)-1)
         IFAIL = 0
*
         CALL G02ECF('M',N,SIGSQ,TSS,NMOD,NTERMS,RSS,RSQ,CP,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Number of     CP      RSQ         MODEL'
         WRITE (NOUT,*) 'parameters'
         WRITE (NOUT,*)
         DO 40 I = 1, NMOD
            II = NTERMS(I)
            WRITE (NOUT,99999) II, CP(I), RSQ(I), (MODEL(I,J),J=1,II)
   40    CONTINUE
      END IF
      STOP
*
99999 FORMAT (1X,I7,F11.2,F8.4,1X,5(1X,A))
      END
