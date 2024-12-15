
   

PROGRAM MAIN
    USE lexicon 
implicit none
    integer :: n
    integer :: start_of_loop
    integer :: left = 0
    integer :: right
    integer :: index = 0
    ! Main function has the identifiers and calls functions to run program
    ! 
    character (len=8), dimension(88759) :: dictionary
    integer :: number_of_anagrams
    character (len = 8) :: chosen_anagram
    
    character(len=8), dimension(40320) :: anagrams
    character (len = 8) :: jumbled_names
    integer, dimension(28) :: alphabet_index
   
    PRINT *,'enter the number of jumbles'
    read *, n
    do start_of_loop =1, n
        left = 0
        index = 0
        call input_jumble(jumbled_names)
        right = len_trim(jumbled_names)
    

        call generate_anagram(jumbled_names, left+1, right,anagrams,index)
        number_of_anagrams = index - 1 
        
        call buildlexicon(alphabet_index,dictionary) 
        
        call find_anagram(anagrams,number_of_anagrams,alphabet_index,chosen_anagram,dictionary)
        PRINT*, chosen_anagram
    end do 

   

    
   END PROGRAM MAIN 

   
!function to get input from the user
subroutine input_jumble(first_name)
    character (len = 8), intent(inout) :: first_name
    PRINT *,'enter the jumbled word'
    read *, first_name 

    END

recursive subroutine generate_anagram(first_name, left, right, anagrams,index)
!function to generate anagrams
    integer :: i
    integer, intent(in) :: left, right
    integer, intent(inout) :: index
    character (len = 15) :: temp
    
    character(len=8), dimension(40320), intent(out) :: anagrams
    character (len=8), intent(inout) :: first_name
    strL = len_trim(first_name)
    IF (index == 0) THEN
        anagrams(index) = first_name
        index = index + 1
    END IF
    IF (left == right) THEN  
        anagrams(index) = first_name
        index = index + 1
    ELSE 
        do i=left, right
        temp = first_name(left:left)
        first_name(left:left) = first_name(i:i)
        first_name(i:i) = temp
        call generate_anagram(first_name,left+1,right, anagrams,index)
        temp = first_name(left:left)
        first_name(left:left) = first_name(i:i)
        first_name(i:i) = temp
        
        
       
        
        end do   
        
    END IF 
END subroutine generate_anagram
    !function to find the anagrams using these parameters
    
subroutine find_anagram(anagrams,number_of_anagrams,alphabet_index,chosen_anagram,dictionary)
    USE lexicon 
    character(len=8), dimension(4320), intent(in) :: anagrams
    integer, dimension(28), intent(in) :: alphabet_index
    character (len = 8), intent(out) :: chosen_anagram
    character (len=8), dimension(88759), intent(in) :: dictionary
    integer, intent(in) ::  number_of_anagrams
    integer :: i
   
    do i=0, number_of_anagrams
       
        IF (findlex(anagrams(i),alphabet_index,dictionary) == 1) THEN 
            chosen_anagram = anagrams(i)
            exit
        END IF 
         
     
    end do 

END subroutine find_anagram


