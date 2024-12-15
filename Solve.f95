PROGRAM MAIN
    USE lexicon 
implicit none
    integer :: n
    integer :: left = 0
    integer :: right
    integer :: index = 0
    integer :: anagram_number
    character (len = 8) :: chosen_anagram
    
    character(len=8), dimension(4320) :: array_string
    character (len = 8) :: jumbled_names
    integer, dimension(28) :: alphabet_index
   
    PRINT *,'enter the number of jumbles'
    read *, n
    call inputJumble(jumbled_names)
    right = len_trim(jumbled_names)
    ! PRINT *,jumbled_names

    call generateAnagram(jumbled_names, left+1, right,array_string,index)
    anagram_number = index - 1 
    PRINT *,array_string
    !  call buildlexicon(alphabet_index) 
    !  PRINT *,alphabet_index
    call findAnagram(array_string,anagram_number,alphabet_index,chosen_anagram)
    PRINT*, chosen_anagram
    
   END PROGRAM MAIN 

   

subroutine inputJumble(first_name)
    character (len = 8), intent(inout) :: first_name
    PRINT *,'enter the jumbled words'
    read *, first_name 

    END

recursive subroutine generateAnagram(first_name, left, right, array_string,index)

    integer :: i
    integer, intent(in) :: left, right
    integer, intent(inout) :: index
    character (len = 15) :: temp
    !array string
    character(len=8), dimension(4320), intent(out) :: array_string
    character (len=8), intent(inout) :: first_name
    strL = len_trim(first_name)
    IF (index == 0) THEN
        array_string(index) = first_name
        index = index + 1
    END IF
    IF (left == right) THEN  
        array_string(index) = first_name
        index = index + 1
    ELSE 
        do i=left, right
        temp = first_name(left:left)
        first_name(left:left) = first_name(i:i)
        first_name(i:i) = temp
        call generateAnagram(first_name,left+1,right, array_string,index)
        temp = first_name(left:left)
        first_name(left:left) = first_name(i:i)
        first_name(i:i) = temp
        
        
       
        
        end do   
        !returning anagrams as an array
    END IF 
END subroutine generateAnagram
    
subroutine findAnagram(array_string,anagram_number,alphabet_index,chosen_anagram)
    USE lexicon 
    character(len=8), dimension(4320), intent(in) :: array_string
    integer, dimension(28), intent(in) :: alphabet_index
    character (len = 8), intent(out) :: chosen_anagram
    integer, intent(in) ::  anagram_number
    integer :: i
    do i=0, anagram_number
        
        IF (findlex(array_string(i),alphabet_index) == 1) THEN 
            chosen_anagram = array_string(i)
        END IF 
         
     
    end do 

END subroutine findAnagram


