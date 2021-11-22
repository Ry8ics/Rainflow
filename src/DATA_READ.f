      SUBROUTINE DATA_READ(AMP,NUM,F_COMP)
      PARAMETER(MAX=10000000)
      DOUBLE PRECISION AMP(MAX)
      DOUBLE PRECISION TT,A
      INTEGER I,IC,NUM
      INTEGER ios,IOstatus,stat_alloc
      CHARACTER :: A_file*20, F_COMP*20
C
C     Corrections/simplifications by B. Melhus on 28.6.2019
C
C     Read first line of input that contains the number of lines
      READ(*,*) NUM
C     Read force component (not yet in use...)
      READ(*,'(A20)') F_COMP
      DO I=1,NUM
         READ(*,*) TT,AMP(I)    
      ENDDO
      RETURN
C     Corrections/simplifications END (and return from routine)
c      
      PRINT'(" The input file must be a time history. ")'
      PRINT'(" Select format: ")'
      PRINT'("   1=amplitude ")'
      PRINT'("   2=time & amplitude ")'
      READ(*,*)IC
C
      ios=1
      DO WHILE(ios.NE.0)
        PRINT '( " Enter the input filename:")'
        READ(*,*)A_file
C
      OPEN(UNIT=10,FILE=A_file,STATUS='OLD',ACTION='READ',IOSTAT=ios)
C
        IF(ios.EQ.0)THEN
            WRITE(*,720)
720         FORMAT(/,'File opened.',/)
        ELSE
            WRITE(*,721)
721         FORMAT(/,' Error: File does not exist.',/)
        ENDIF
C
      ENDDO
C
      DO I=1,MAX
            IF(IC.EQ.1)THEN
               READ(10,*,IOSTAT=IOstatus) A
            ELSE
               READ(10,*,IOSTAT=IOstatus) TT,A
            ENDIF
            IF (IOstatus.EQ.0) THEN
                NUM=I
            ELSE
                EXIT
            ENDIF
      ENDDO
C
      REWIND(10)
C
      DO I=1,NUM
            IF(IC.EQ.1)THEN
               READ(10,*,IOSTAT=IOstatus) AMP(I)
            ELSE
               READ(10,*,IOSTAT=IOstatus) TT,AMP(I)
            ENDIF
      ENDDO
C
C      WRITE(*,404)NUM
C404   FORMAT(/,' NUM =  ',I8,/)
C
      CLOSE(10)
      RETURN
      END