module lexicon
    
    implicit none
    
    contains      
    subroutine buildlexicon(alphabet_index) 
        implicit none
        !declaring parameters
        integer, dimension(28), intent(out) :: alphabet_index
        integer :: i
        character (len=20) :: fname = 'dict2.txt'
        character (len=20) :: line
        PRINT *, 'in build lexicon function'
        open(unit=1,file=fname)
        alphabet_index(1) = 1
        do i=1, 88759
            read (1,*) line
            ! PRINT *, line
            IF (line == 'B') THEN  
                ! PRINT *, i
                ! PRINT *, line
                alphabet_index(2) = i

            END IF
            IF (line == 'C') THEN  
                ! PRINT *, i
                ! PRINT *, line
                alphabet_index(3) = i
            END IF
            IF (line == 'D') THEN  
                ! PRINT *, i
                ! PRINT *, line
                alphabet_index(4) = i

            END IF
            IF (line == 'E') THEN  
                ! PRINT *, i
                ! PRINT *, line
                alphabet_index(5) = i
            END IF
            IF (line == 'F') THEN  
                ! PRINT *, i
                ! PRINT *, line
                alphabet_index(6) = i

            END IF
            IF (line == 'G') THEN  
                ! PRINT *, i
                ! PRINT *, line
                alphabet_index(7) = i
            END IF
            IF (line == 'H') THEN  
                ! PRINT *, i
                ! PRINT *, line
                alphabet_index(8) = i

            END IF
            IF (line == 'I') THEN  
                ! PRINT *, i
                ! PRINT *, line
                alphabet_index(9) = i
            END IF
            IF (line == 'J') THEN  
                ! PRINT *, i
                ! PRINT *, line
                alphabet_index(10) = i

            END IF
            IF (line == 'K') THEN  
                ! PRINT *, i
                ! PRINT *, line
                alphabet_index(11) = i
            END IF
            IF (line == 'L') THEN  
                ! PRINT *, i
                ! PRINT *, line
                alphabet_index(12) = i

            END IF
            IF (line == 'M') THEN  
                ! PRINT *, i
                ! PRINT *, line
                alphabet_index(13) = i
            END IF
            IF (line == 'N') THEN  
                ! PRINT *, i
                ! PRINT *, line
                alphabet_index(14) = i

            END IF
            IF (line == 'O') THEN  
                ! PRINT *, i
                ! PRINT *, line
                alphabet_index(15) = i
            END IF
            IF (line == 'P') THEN  
                ! PRINT *, i
                ! PRINT *, line
                alphabet_index(16) = i

            END IF
            IF (line == 'Q') THEN  
                ! PRINT *, i
                ! PRINT *, line
                alphabet_index(17) = i
            END IF
            IF (line == 'R') THEN  
                ! PRINT *, i
                ! PRINT *, line
                alphabet_index(18) = i

            END IF
            IF (line == 'S') THEN  
                ! PRINT *, i
                ! PRINT *, line
                alphabet_index(19) = i
            END IF
            IF (line == 'T') THEN  
                ! PRINT *, i
                ! PRINT *, line
                alphabet_index(20) = i

            END IF
            IF (line == 'U') THEN  
                ! PRINT *, i
                ! PRINT *, line
                alphabet_index(21) = i
            END IF
            IF (line == 'V') THEN  
                ! PRINT *, i
                ! PRINT *, line
                alphabet_index(22) = i

            END IF
            IF (line == 'W') THEN  
                ! PRINT *, i
                ! PRINT *, line
                alphabet_index(23) = i
            END IF

            IF (line == 'X') THEN  
                ! PRINT *, i
                ! PRINT *, line
                alphabet_index(26) = i

            END IF
            IF (line == 'Y') THEN  
                ! PRINT *, i
                ! PRINT *, line
                alphabet_index(27) = i
            END IF
            IF (line == 'Z') THEN  
                ! PRINT *, i
                ! PRINT *, line
                alphabet_index(28) = i
            END IF
            
            
        end do
        close(1) 
       
        
      
    end subroutine buildlexicon 
     integer function findlex(anagram, alphabet_index)  
     character (len=20) :: fname = 'dict2.txt'
     integer, dimension(28), intent(in) :: alphabet_index
     character (len=8), intent(in) :: anagram 
     integer :: j
     character (len=20) :: line
     open(unit=1,file=fname)
     do j=1, 88800 
        read (1,*) line
        IF (anagram(1:1) == 'a') THEN
            IF (j>=alphabet_index(1) .and. j<alphabet_index(2)) THEN 
                IF (anagram == line) THEN
                    PRINT *, anagram
                    findlex = 1
                    exit
                END IF 
            END IF 
        END IF
        IF (anagram(1:1) == 'b') THEN
            IF (j>=alphabet_index(2) .and. j<alphabet_index(3)) THEN 
                IF (anagram == line) THEN
                    PRINT *, anagram
                    findlex = 1
                    exit
                END IF 
            END IF 
        END IF
        IF (anagram(1:1) == 'c') THEN
            IF (j>=alphabet_index(3) .and. j<alphabet_index(4)) THEN 
                IF (anagram == line) THEN
                    PRINT *, anagram
                    findlex = 1
                    exit
                END IF 
            END IF 
        END IF
        IF (anagram(1:1) == 'd') THEN
            IF (j>=alphabet_index(4) .and. j<alphabet_index(5)) THEN 
                IF (anagram == line) THEN
                    PRINT *, anagram
                    findlex = 1
                    exit
                END IF 
            END IF 
        END IF
        IF (anagram(1:1) == 'e') THEN
            IF (j>=alphabet_index(5) .and. j<alphabet_index(6)) THEN 
                IF (anagram == line) THEN
                    PRINT *, anagram
                    findlex = 1
                    exit
                END IF 
            END IF 
        END IF
        IF (anagram(1:1) == 'f') THEN
            IF (j>=alphabet_index(6) .and. j<alphabet_index(7)) THEN 
                IF (anagram == line) THEN
                    PRINT *, anagram
                    findlex = 1
                    exit
                END IF 
            END IF 
        END IF
        IF (anagram(1:1) == 'g') THEN
            IF (j>=alphabet_index(7) .and. j<alphabet_index(8)) THEN 
                IF (anagram == line) THEN
                    PRINT *, anagram
                    findlex = 1
                    exit
                END IF 
            END IF 
        END IF
        IF (anagram(1:1) == 'h') THEN
            IF (j>=alphabet_index(8) .and. j<alphabet_index(9)) THEN 
                IF (anagram == line) THEN
                    PRINT *, anagram
                    findlex = 1
                    exit
                END IF 
            END IF 
        END IF
        IF (anagram(1:1) == 'i') THEN
            IF (j>=alphabet_index(9) .and. j<alphabet_index(10)) THEN 
                IF (anagram == line) THEN
                    PRINT *, anagram
                    findlex = 1
                    exit
                END IF 
            END IF 
        END IF
        IF (anagram(1:1) == 'j') THEN
            IF (j>=alphabet_index(10) .and. j<alphabet_index(11)) THEN 
                IF (anagram == line) THEN
                    PRINT *, anagram
                    findlex = 1
                    exit
                END IF 
            END IF 
        END IF
        IF (anagram(1:1) == 'k') THEN
            IF (j>=alphabet_index(11) .and. j<alphabet_index(12)) THEN 
                IF (anagram == line) THEN
                    PRINT *, anagram
                    findlex = 1
                    exit
                END IF 
            END IF 
        END IF
        IF (anagram(1:1) == 'l') THEN
            IF (j>=alphabet_index(12) .and. j<alphabet_index(13)) THEN 
                IF (anagram == line) THEN
                    PRINT *, anagram
                    findlex = 1
                    exit
                END IF 
            END IF 
        END IF
        IF (anagram(1:1) == 'm') THEN
            IF (j>=alphabet_index(13) .and. j<alphabet_index(14)) THEN 
                IF (anagram == line) THEN
                    PRINT *, anagram
                    findlex = 1
                    exit
                END IF 
            END IF 
        END IF
        IF (anagram(1:1) == 'n') THEN
            IF (j>=alphabet_index(14) .and. j<alphabet_index(15)) THEN 
                IF (anagram == line) THEN
                    PRINT *, anagram
                    findlex = 1
                    exit
                END IF 
            END IF 
        END IF
        IF (anagram(1:1) == 'o') THEN
            IF (j>=alphabet_index(15) .and. j<alphabet_index(16)) THEN 
                IF (anagram == line) THEN
                    PRINT *, anagram
                    findlex = 1
                    exit
                END IF 
            END IF 
        END IF
        IF (anagram(1:1) == 'p') THEN
            IF (j>=alphabet_index(16) .and. j<alphabet_index(17)) THEN 
                IF (anagram == line) THEN
                    PRINT *, anagram
                    findlex = 1
                    exit
                END IF 
            END IF 
        END IF
        IF (anagram(1:1) == 'q') THEN
            IF (j>=alphabet_index(17) .and. j<alphabet_index(18)) THEN 
                IF (anagram == line) THEN
                    PRINT *, anagram
                    findlex = 1
                    exit
                END IF 
            END IF 
        END IF
        IF (anagram(1:1) == 'r') THEN
            IF (j>=alphabet_index(18) .and. j<alphabet_index(19)) THEN 
                IF (anagram == line) THEN
                    PRINT *, anagram
                    findlex = 1
                    exit
                END IF 
            END IF 
        END IF
        IF (anagram(1:1) == 's') THEN
            IF (j>=alphabet_index(19) .and. j<alphabet_index(20)) THEN 
                IF (anagram == line) THEN
                    PRINT *, anagram
                    findlex = 1
                    exit
                END IF 
            END IF 
        END IF
        IF (anagram(1:1) == 't') THEN
            IF (j>=alphabet_index(20) .and. j<alphabet_index(21)) THEN 
                IF (anagram == line) THEN
                    PRINT *, anagram
                    findlex = 1
                    exit
                END IF 
            END IF 
        END IF
        IF (anagram(1:1) == 'u') THEN
            IF (j>=alphabet_index(21) .and. j<alphabet_index(22)) THEN 
                IF (anagram == line) THEN
                    PRINT *, anagram
                    findlex = 1
                    exit
                END IF 
            END IF 
        END IF
        IF (anagram(1:1) == 'v') THEN
            IF (j>=alphabet_index(22) .and. j<alphabet_index(23)) THEN 
                IF (anagram == line) THEN
                    PRINT *, anagram
                    findlex = 1
                    exit
                END IF 
            END IF 
        END IF
        IF (anagram(1:1) == 'w') THEN
            IF (j>=alphabet_index(23) .and. j<alphabet_index(24)) THEN 
                IF (anagram == line) THEN
                    PRINT *, anagram
                    findlex = 1
                    exit
                END IF 
            END IF 
        END IF
        IF (anagram(1:1) == 'x') THEN
            IF (j>=alphabet_index(24) .and. j<alphabet_index(25)) THEN 
                IF (anagram == line) THEN
                    PRINT *, anagram
                    findlex = 1
                    exit
                END IF 
            END IF 
        END IF
        IF (anagram(1:1) == 'y') THEN
            IF (j>=alphabet_index(25) .and. j<alphabet_index(26)) THEN 
                IF (anagram == line) THEN
                    PRINT *, anagram
                    findlex = 1
                    exit
                END IF 
            END IF 
        END IF
        IF (anagram(1:1) == 'z') THEN
            IF (j>=alphabet_index(26) .and. j<alphabet_index(27)) THEN 
                IF (anagram == line) THEN
                    PRINT *, anagram
                    findlex = 1
                    exit
                END IF 
            END IF 
        END IF
    end do 
    close(1) 
        findlex = 0 
     end function findlex  
 end module lexicon
