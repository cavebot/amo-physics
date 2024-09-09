*     G02EAF Example Program Text
*     Mark 14 Release.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          NMAX, MMAX, LMAX
      PARAMETER        (NMAX=20,MMAX=6,LMAX=32)
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      INTEGER          I, IFAIL, II, J, M, N, NMOD
*     .. Local Arrays ..
      DOUBLE PRECISION RSS(LMAX), WK(NMAX*(MMAX+1)), WT(NMAX),
     +                 X(NMAX,MMAX), Y(NMAX)
      INTEGER          ISX(MMAX), MRANK(LMAX), NTERMS(LMAX)
      CHARACTER*3      MODEL(LMAX,MMAX), NAME(MMAX)
*     .. External Subroutines ..
      EXTERNAL         G02EAF
*     .. Executable Statements ..
      WRITE (NOUT,*) 'G02EAF Example Program Results'
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
         CALL G02EAF('M','U',N,M,X,NMAX,NAME,ISX,Y,WT,NMOD,MODEL,LMAX,
     +               RSS,NTERMS,MRANK,WK,IFAIL)
*
         WRITE (NOUT,*)
         WRITE (NOUT,*) 'Number of     RSS    RANK         MODEL'
         WRITE (NOUT,*) 'parameters'
         DO 40 I = 1, NMOD
            II = NTERMS(I)
            WRITE (NOUT,99999) II, RSS(I), MRANK(I), (MODEL(I,J),J=1,II)
   40    CONTINUE
      END IF
      STOP
*
99999 FORMAT (1X,I8,F11.4,I4,3X,5(1X,A))
      END
