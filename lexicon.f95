module lexicon
    
    implicit none
    !function to build lexicon
    contains      
    subroutine buildlexicon(alphabet_index,x) 
        implicit none
        !declaring parameters
        !looping through to store in a dictionary
        integer, dimension(28), intent(out) :: alphabet_index
        !dimension chosen to accomodate for the number of lines in the word file
        character (len=8), dimension(88759), intent(out) :: x
        integer :: i
        character (len=20) :: fname = 'dict2.txt'
        character (len=20) :: line
        
        open(unit=1,file=fname,status='old',action='read')
        alphabet_index(1) = 1
      !88759 chosen because of the number of lines in the file
        do i=1, 88759
            read (1,*) line
            x(i) = trim(line)
            ! print *, x(i)
            
            !  PRINT *, line
             IF (x(i) == 'A') THEN  
                ! PRINT *, i
                ! PRINT *, x(i)
                alphabet_index(1) = i

            END IF
            IF (x(i) == 'B') THEN  
                ! PRINT *, i
                ! PRINT *, line
                alphabet_index(2) = i

            END IF
            IF (x(i) == 'C') THEN  
                ! PRINT *, i
                ! PRINT *, line
                alphabet_index(3) = i
            END IF
            IF (x(i) == 'D') THEN  
                ! PRINT *, i
                ! PRINT *, line
                alphabet_index(4) = i

            END IF
            IF (x(i) == 'E') THEN  
                ! PRINT *, i
                ! PRINT *, line
                alphabet_index(5) = i
            END IF
            IF (x(i) == 'F') THEN  
                ! PRINT *, i
                ! PRINT *, line
                alphabet_index(6) = i

            END IF
            IF (x(i) == 'G') THEN  
                ! PRINT *, i
                ! PRINT *, line
                alphabet_index(7) = i
            END IF
            IF (x(i) == 'H') THEN  
                ! PRINT *, i
                ! PRINT *, line
                alphabet_index(8) = i

            END IF
            IF (x(i) == 'I') THEN  
                ! PRINT *, i
                ! PRINT *, line
                alphabet_index(9) = i
            END IF
            IF (x(i) == 'J') THEN  
                ! PRINT *, i
                ! PRINT *, line
                alphabet_index(10) = i

            END IF
            IF (x(i) == 'K') THEN  
                ! PRINT *, i
                ! PRINT *, line
                alphabet_index(11) = i
            END IF
            IF (x(i) == 'L') THEN  
                ! PRINT *, i
                ! PRINT *, line
                alphabet_index(12) = i

            END IF
            IF (x(i) == 'M') THEN  
                ! PRINT *, i
                ! PRINT *, line
                alphabet_index(13) = i
            END IF
            IF (x(i) == 'N') THEN  
                ! PRINT *, i
                ! PRINT *, line
                alphabet_index(14) = i

            END IF
            IF (x(i) == 'O') THEN  
                ! PRINT *, i
                ! PRINT *, line
                alphabet_index(15) = i
            END IF
            IF (x(i) == 'P') THEN  
                ! PRINT *, i
                ! PRINT *, line
                alphabet_index(16) = i

            END IF
            IF (x(i) == 'Q') THEN  
                ! PRINT *, i
                ! PRINT *, line
                alphabet_index(17) = i
            END IF
            IF (x(i) == 'R') THEN  
                ! PRINT *, i
                ! PRINT *, line
                alphabet_index(18) = i

            END IF
            IF (x(i) == 'S') THEN  
                ! PRINT *, i
                ! PRINT *, line
                alphabet_index(19) = i
            END IF
            IF (x(i) == 'T') THEN  
                ! PRINT *, i
                ! PRINT *, line
                alphabet_index(20) = i

            END IF
            IF (x(i) == 'U') THEN  
                ! PRINT *, i
                ! PRINT *, line
                alphabet_index(21) = i
            END IF
            IF (x(i) == 'V') THEN  
                ! PRINT *, i
                ! PRINT *, line
                alphabet_index(22) = i

            END IF
            IF (x(i) == 'W') THEN  
                ! PRINT *, i
                ! PRINT *, line
                alphabet_index(23) = i
            END IF
            IF (x(i) == 'X') THEN  
                ! PRINT *, i
                ! PRINT *, line
                alphabet_index(24) = i

            END IF
            IF (x(i) == 'Y') THEN  
                ! PRINT *, i
                ! PRINT *, line
                alphabet_index(25) = i
            END IF
            IF (x(i) == 'Z') THEN  
                ! PRINT *, i
                ! PRINT *, line
                alphabet_index(26) = i
            END IF
            
            
        end do
        
            close(1) 
          
       
        
      
    end subroutine buildlexicon 
    !function for findlex with these parameters
     integer function findlex(anagram, alphabet_index,dictionary)  
     character (len=8), dimension(88759), intent(in) :: dictionary
     integer, dimension(28), intent(in) :: alphabet_index
     !dimension 28 chosen because of size of the alphabet
     character (len=8), intent(in) :: anagram 
     integer :: j = 1 
    ! intent(in) is used because we are inputting the dictionary
    !  PRINT *, anagram
     do j=1 , 88759 
      IF (trim(anagram) == dictionary(j)) THEN
         findlex = 1
         exit
      END IF
    end do 
    ! IF (findlex /= 1 ) THEN
    !     findlex = 0
        
    !  END IF
       
     end function findlex  
 end module lexicon
