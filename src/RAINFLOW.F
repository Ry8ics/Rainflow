      PROGRAM RAINFLOW
C
C	  RAINFLOW.F    VER 1.2    SEPTEMBER 23, 2013
C	  BY TOM IRVINE
C	  EMAIL:  TOM@VIBRATIONDATA.COM
C
C     ASTM E 1049-85 (2005) RAINFLOW COUNTING METHOD
C
C23456789
C
      PARAMETER(MAX=10000000)
C
      INTEGER, ALLOCATABLE :: T(:)
      INTEGER, ALLOCATABLE :: ST(:)
C
      DOUBLE PRECISION, ALLOCATABLE :: Y(:),A(:),TEMP(:)
      DOUBLE PRECISION, ALLOCATABLE :: C(:)
      DOUBLE PRECISION, ALLOCATABLE :: CC(:)
      DOUBLE PRECISION, ALLOCATABLE :: AA(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: A_MEAN(:,:),B(:,:)
C
      DOUBLE PRECISION, ALLOCATABLE :: AVERAGEMEAN(:)
      DOUBLE PRECISION, ALLOCATABLE :: MAXMEAN(:)
      DOUBLE PRECISION, ALLOCATABLE :: MINMEAN(:)
      DOUBLE PRECISION, ALLOCATABLE :: MAXPEAK(:)
      DOUBLE PRECISION, ALLOCATABLE :: MINVALLEY(:)
      DOUBLE PRECISION, ALLOCATABLE :: MAXAMP(:)
      DOUBLE PRECISION, ALLOCATABLE :: AVERAGEAMP(:)
C
      DOUBLE PRECISION AMP(MAX)
      DOUBLE PRECISION L(14)
      DOUBLE PRECISION AMAX,AAMAX
      DOUBLE PRECISION SLOPE1,SLOPE2
      DOUBLE PRECISION BX,D
C
      DOUBLE PRECISION P1,P2,TP1,TP2
      DOUBLE PRECISION X,YY,CUM
C
      INTEGER IFLAG,KFLAG
      INTEGER I,IP1,J,JP1,K,M,NUM
      INTEGER MSA,MSA_ORIG
      INTEGER TC
      INTEGER IOstatus
      INTEGER stat_alloc
C
      CHARACTER :: B_file*20, F_COMP*20,CLINE*34
C
C     Corrections by B. Melhus
CBM_CORR      PRINT'(" ")'
CBM_CORR      PRINT'(" ASTM E 1049-85 (2005) RAINFLOW COUNTING METHOD")'
CCC      WRITE(*,*) 'ASTM E 1049-85 (2005) RAINFLOW COUNTING METHOD'
C     Corrections end      
C
      CALL DATA_READ(AMP,NUM,F_COMP)
C
CBM_CORR      PRINT'(" ")'
CBM_CORR      PRINT'(" Enter name of output file:  ")'
CBM_CORR      READ(*,*)B_file
CBM_CORR      OPEN(UNIT=11,FILE=B_file,ACTION='WRITE')
C
      ALLOCATE(T(NUM))
      ALLOCATE(Y(NUM))
      ALLOCATE(A(NUM))
C
      DO I=1,NUM
        Y(I)=AMP(I)
        A(I)=0.
        T(I)=0
      ENDDO
C
      A(1)=Y(1)
      T(1)=1
      K=2
C
      M=NUM
CCC      WRITE(*,133)M
133   FORMAT(/,' Total number of input points = ',I10,/)
C
CBM_CORR      PRINT'(" Begin slope calculation ")'
C
      DO I=2,NUM-1
         SLOPE1=(  Y(I)-Y(I-1))
         SLOPE2=(Y(I+1)-Y(I))
         IF((SLOPE1*SLOPE2).LE.0)THEN
           A(K)=Y(I)
           T(K)=I
           K=K+1
         ENDIF
      ENDDO
C
      A(K)=Y(NUM)
      T(K)=T(K-1)+1
CCC      WRITE(*,512)K
512   FORMAT(/,' Number of Peaks = ',I10,/)
C
      ALLOCATE(TEMP(K))
      TEMP(1:K)=A(1:K)
      DEALLOCATE(A,stat=stat_alloc)
      ALLOCATE(A(K))
      A=TEMP
C
      TEMP(1:K)=T(1:K)
      DEALLOCATE(T,stat=stat_alloc)
      ALLOCATE(T(K))
      T=TEMP
C
      ALLOCATE(CC(K))
      CC=A
      ALLOCATE(AA(K,2))
C
      DO I=1,K
        AA(I,1)=T(I)
        AA(I,2)=A(I)
C        WRITE(*,199)A(I)
C199     FORMAT(G10.4)
      ENDDO
      DEALLOCATE(T,stat=stat_alloc)
      DEALLOCATE(A,stat=stat_alloc)
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C   RULES FOR THIS METHOD ARE AS FOLLOWS: LET X DENOTE
C   RANGE UNDER CONSIDERATION Y, PREVIOUS RANGE ADJACENT TO X AND
C   S, STARTING POINT IN THE HISTORY.
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C
      AAMAX=0.
      ALLOCATE(B(NUM,4))
      ALLOCATE(A_MEAN(NUM,4))
C
      A_MEAN=0.
      B=0.
C
C  ST is the index status.
C
C    ST(I)=0  array entry still active
C    ST(I)=1  array entry deleted
C
      ALLOCATE(ST(K))
      ST=0
C
C     Find AI
C
      KV=1
      I1=1
C
      I=1
      J=2
      JP1=3
      IFLAG=1
      IB=0
C
C     Correction next line by B. Melhus
C     I have no clue what's going on. 
C     However for some instances of time series an infinite loop occurs.
C     By updating to K-4 the loop seems always to terminate.      
C     DO WHILE(I.LE.K-3)
C      DO WHILE(I.LE.K-4)  
      DO WHILE(I.LE.K-5)  
          
         IP1=J
C
         AI=AA(I,2)
       AIP1=AA(J,2)
         AJ=AA(J,2)
       AJP1=AA(JP1,2)
C
         YY=(ABS(AIP1-AI))
          X=(ABS(AJP1-AJ))
C
C           write(*,543)AI,AJ,AJP1
543        format(3(1x,g10.4))
          IF(X.GE.YY .AND. YY.GT.0.)THEN
              IF(IFLAG.EQ.1)THEN
                B(KV,1)=YY
                B(KV,2)=0.5
                B(KV,3)=AI
                B(KV,4)=AJ
                ST(I)=1
C                WRITE(*,*)'p1a ',B(KV,1),B(KV,2),B(KV,3),B(KV,4)
                I=J
                ISTART=J+1
                IEND=K-1
                CALL FIND_NEXT(ISTART,IEND,ST,J,K)
                ISTART=J+1
                IEND=K
                CALL FIND_NEXT(ISTART,IEND,ST,JP1,K)
              ELSE
                B(KV,1)=YY
                B(KV,2)=1
                B(KV,3)=AI
                B(KV,4)=AJ
                ST(I)=1
                ST(IP1)=1
C                WRITE(*,112)'p1b ',B(KV,1),B(KV,2),B(KV,3),B(KV,4)
                ISTART=1
                IEND=K-2
                CALL FIND_NEXT(ISTART,IEND,ST,I,K)
                ISTART=I+1
                IEND=K-1
                CALL FIND_NEXT(ISTART,IEND,ST,J,K)
                ISTART=J+1
                IEND=K
                CALL FIND_NEXT(ISTART,IEND,ST,JP1,K)
             ENDIF
             IF(YY.GT.AAMAX)THEN
                    P1=AA(I,2)
                    P2=AA(I+1,2)
                    TP1=AA(I,1)
                    TP2=AA(I+1,1)
                    AAMAX=YY
              ENDIF
              KV=KV+1
C
              I1=I
              IFLAG=1
C
          ELSE
C
              IFLAG=IFLAG+1
              I=J
              ISTART=J+1
              IEND=K-1
              CALL FIND_NEXT(ISTART,IEND,ST,J,K)
              ISTART=J+1
              IEND=K
              CALL FIND_NEXT(ISTART,IEND,ST,JP1,K)
C
          ENDIF
C
      ENDDO
C      write(*,*)' ref 890 '
C
C23456789
C
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C  COUNT EACH RANGE THAT HAS NOT BEEN PREVIOUSLY COUNTED
C  AS ONE-HALF CYCLE.
C
      PRINT'(" ")'
C
      I1=1
      ISTART=I1
      IEND=K-1
      CALL FIND_NEXT(ISTART,IEND,ST,I,K)
C
      DO WHILE(I1.LE.K)
        ISTART=I+1
        IEND=K
        CALL FIND_NEXT(ISTART,IEND,ST,J,K)
C
        AI=AA(I,2)
        AJ=AA(J,2)
C
        YY=ABS(AJ-AI)
C
        ST(I)=1
        IF(YY.GT.0.)THEN
           B(KV,1)=YY
           B(KV,2)=0.5
           B(KV,3)=AI
           B(KV,4)=AJ
C           WRITE(*,112)'p2 ',B(KV,1),B(KV,2),B(KV,3),B(KV,4)
C112        FORMAT(4(1X,G12.5))
        ENDIF
C
        IF(YY.GT.AAMAX)THEN
            P1=AI
            P2=AIP1
            TP1=AA(I,1)
            TP2=AA(IP1,1)
            AAMAX=YY
        ENDIF
        KV=KV+1
        I=J
        I1=I+1
      ENDDO
C      write(*,*)' ref 900 '
C23456789
C
      DEALLOCATE(ST,stat=stat_alloc)
C
C AMAX=MAX(Y)-MIN(Y)
C
CC      PRINT'(" BEGIN BIN SORTING ")'
C
      AMAX=MAXVAL(B(1:M,1))
      L(1)=0
      L(2)=2.5
      L(3)=5
      L(4)=10
      L(5)=15
      L(6)=20
      L(7)=30
      L(8)=40
      L(9)=50
      L(10)=60
      L(11)=70
      L(12)=80
      L(13)=90
      L(14)=100
      L=L*AMAX/100
C
C      write(*,*)' AMAX= ',AMAX
C
      NUM=SIZE(L)-1
C
      ALLOCATE(AVERAGEMEAN(NUM))
      ALLOCATE(MAXMEAN(NUM))
      ALLOCATE(MINMEAN(NUM))
      ALLOCATE(MAXPEAK(NUM))
      ALLOCATE(MINVALLEY(NUM))
      ALLOCATE(MAXAMP(NUM))
      ALLOCATE(AVERAGEAMP(NUM))
      ALLOCATE(C(NUM))
C
      DO I=1,NUM
         C(I)=0.
         AVERAGEMEAN(I)=0
         MAXMEAN(I)=-1.0E+09
         MINMEAN(I)= 1.0E+09
C
         MAXPEAK(I)=-1.0E+09
         MINVALLEY(I)= 1.0E+09
C
         MAXAMP(I)=0
         AVERAGEAMP(I)=0
      ENDDO
C
C      print'(" ref 5 ")'
C
      DO I=1,KV-1
           YY=B(I,1)
           DO IJK=1,NUM
              IF((YY.GE.L(IJK)).AND.(YY.LE.L(IJK+1)))THEN
                   C(IJK)=C(IJK)+B(I,2)
                   BM=(B(I,3)+B(I,4))/2
                   IF(B(I,3).GT.MAXPEAK(IJK))THEN
                      MAXPEAK(IJK)=B(I,3)
                   ENDIF
                   IF(B(I,4).GT.MAXPEAK(IJK))THEN
                      MAXPEAK(IJK)=B(I,4)
                   ENDIF
                   IF(B(I,3).LT.MINVALLEY(IJK))THEN
                      MINVALLEY(IJK)=B(I,3)
                   ENDIF
                   IF(B(I,4).LT.MINVALLEY(IJK))THEN
                      MINVALLEY(IJK)=B(I,4)
                   ENDIF
C
                   AVERAGEAMP(IJK)=AVERAGEAMP(IJK)+B(I,1)*B(I,2)
                   AVERAGEMEAN(IJK)=AVERAGEMEAN(IJK)+BM*B(I,2)
C
                   IF( BM .GT. MAXMEAN(IJK))THEN
                      MAXMEAN(IJK)=BM
                   ENDIF
                   IF( BM .LT. MINMEAN(IJK))THEN
                      MINMEAN(IJK)=BM
                   ENDIF
C
                   IF(B(I,1).GT.MAXAMP(IJK))THEN
                      MAXAMP(IJK)=B(I,1)
                   ENDIF
                   EXIT
              ENDIF
          ENDDO
      ENDDO
C
      DO IJK=1,NUM
         IF( C(IJK).GT.0.)THEN
            AVERAGEAMP(IJK)=AVERAGEAMP(IJK)/C(IJK)
            AVERAGEMEAN(IJK)=AVERAGEMEAN(IJK)/C(IJK)
         ENDIF
      ENDDO
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C
C23456789
C     Corrections by B. Melhus
C      WRITE(11,555)
C      WRITE(11,556)
C      WRITE(11,557)
CC      WRITE(*,555)
CC      WRITE(*,556)
CC      WRITE(*,557)
C      
555   FORMAT('  AMPLITUDE = (PEAK-VALLEY)/2  ',/)
556   FORMAT('    RANGE LIMITS         CYCLE        AVERAGE      MAX
     1  MIN       AVE       MAX        MIN        MAX')
557   FORMAT('     (UNITS)             COUNTS         AMP        AMP
     1  MEAN      MEAN      MEAN      VALLEY      PEAK',/)
C
cc      PRINT'(" ")'
cc      PRINT'("  AMPLITUDE = (PEAK-VALLEY)/2 ")'
cc      PRINT'(" ")'
cc      PRINT'("     RANGE LIMITS        CYCLE      AVERAGE     MAX")'
cc      PRINT'("      (UNITS)            COUNTS       AMP       AMP")'
C
      MAXAMP=MAXAMP/2
      AVERAGEAMP=AVERAGEAMP/2
C
C     New code inserted by B. Melhus:
      CLINE=ADJUSTR(F_COMP)//'         Range'
      CLINE=ADJUSTL(CLINE)
      WRITE(*,203) CLINE,'Ncum','Amplitude','Mean'
C     New code end      
C
      CUM=0.0D0
      DO I=1,NUM
          J=NUM+1-I
C
          IF(C(J).LT.0.5)THEN
             AVERAGEAMP(J)=0.
             MAXAMP(J)=0.
             MINMEAN(J)=0.
             AVERAGEMEAN(J)=0.
             MAXMEAN(J)=0.
             MINVALLEY(J)=0.
             MAXPEAK(J)=0.
          ENDIF
C
C23456789
cc          WRITE(*,207)L(J),L(J+1),C(J),AVERAGEAMP(J),MAXAMP(J)
cc207       FORMAT(1X,G10.4,' to ',G10.4,1X,F8.1,2X,G10.4,2X,G10.4)
C

          
          
C          WRITE(*,201)L(J),L(J+1),C(J),AVERAGEAMP(J),MAXAMP(J),
C     1    MINMEAN(J),AVERAGEMEAN(J),MAXMEAN(J),MINVALLEY(J),MAXPEAK(J)
C
C         New code inserted by B. Melhus
          CUM=CUM+C(J)
c          print*,L(J),' ',CUM
          
      WRITE(*,202) L(J),CUM,AVERAGEAMP(J),AVERAGEMEAN(J)
           
 202  FORMAT (1x,F20.1,3(2X,F12.1))   
 203  FORMAT (1x,A20,  3(2X,A12))    
C
201   FORMAT(1X,G10.4,' to ',G10.4,2X,F8.1,1X,7(1X,G10.4))
C
      ENDDO
C      CLOSE(11)
C
CC      WRITE(*,301)AAMAX
301   FORMAT(/,'  MAX RANGE = ',G10.4,/)
C
      TC=SUM(C)
CC     WRITE(*,401)TC
401   FORMAT(/,'  TOTAL CYCLES = ',I10,/)
C
C Corrections by B. Melhus
CCC      PRINT'(" ")'
CCC      PRINT '(" Calculate Relative Damage Index?  1=yes 2=no ")'
CCC      READ(*,*)IDI
      IDI=2
C
      IF(IDI.EQ.1)THEN
        PRINT'(" ")'
        PRINT '(" Enter fatigue exponent ")'
        READ(*,*)BX
C
        D=0.
        DO I=1,KV-1
           D=D+B(I,2)*(0.5*B(I,1))**BX
        ENDDO
C
        WRITE(*,717)D
717     FORMAT(/,' Damage Index = ',G10.4,/)
C
      ENDIF
C
C
      DEALLOCATE(Y,stat=stat_alloc)
      DEALLOCATE(TEMP,stat=stat_alloc)
      DEALLOCATE(CC,stat=stat_alloc)
      DEALLOCATE(A_MEAN,stat=stat_alloc)
      DEALLOCATE(B,stat=stat_alloc)
      DEALLOCATE(C,stat=stat_alloc)
C
      DEALLOCATE(AVERAGEMEAN,stat=stat_alloc)
      DEALLOCATE(MAXMEAN,stat=stat_alloc)
      DEALLOCATE(MINMEAN,stat=stat_alloc)
      DEALLOCATE(MAXPEAK,stat=stat_alloc)
      DEALLOCATE(MINVALLEY,stat=stat_alloc)
      DEALLOCATE(MAXAMP,stat=stat_alloc)
      DEALLOCATE(AVERAGEAMP,stat=stat_alloc)
C_
CCCC      WRITE(*,*)
CCCC      WRITE(*,*)' Output file:  ',B_file
C
      STOP
      END