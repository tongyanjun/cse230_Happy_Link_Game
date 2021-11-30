# cse230_Happy_Link_Game

## Group Members
Yanjun Tong

Yiyang Qiu

Shanbin Ke

Er Zhuo

## Project Description
In a limited time, as long as you find all the same letters that can be connected in pairs, each time you find a pair, they will automatically disappear, as long as all the letters are eliminated to win. The so-called being able to connect means: whether horizontally or vertically, the line from one letter to another letter cannot exceed two bends, and the line cannot pass through the letters that have not been eliminated.

               1 2 3 4 5 6 7 8 9 10
               A B Y K L A M K L Z  1
               K A Y A O P N P L Z  2
               F O N L M R F B X X  3
 
## Architecture
#### print_the_game:
This API is the outer monad to print the current game status to the terminal. It will include the board with existing letters and eliminated ones replaced by specific symbols. It will also print the remain time and some hint information such as the “Invalid” message will you choose two letters not able to be linked. It will also contain some interactive functions.

#### check_linkable:
This is one of the most significant logic in our project that will decide whether two remaining letters on the board can be linked or not. 

#### hint:
This API will randomly give a pair of letters which can be linked on current board. The function will be mainly built based on the check_linkable function. The API will be called by other functions or by the player when they click the “hint” bottom.

#### shuffle:
This API is aimed to shuffle the remaining letters on the board when no two letters on the board can be linked. The function will be implemented based on “hint” function and will be automatically triggered when the game is “dead”. Player cannot shuffle the board intentionally. The shuffle will only change the relative position of different letters, the “dead place” will not be moved.

#### change_board:
This API is an intermediate function that will change our inner date structures. It will bridge our inner decision making logic and our print out API.

## Challege:
+ The implementation of the game logic is a little bit complicated. We searched some reference materials to design a relatively efficient algorithm and then implemented the functions in Haskell step by step.
+ Usually the game will show the linked path when the player finds a valid pair of letters. But printing the path is not that easy in this terminal based game. We are still trying some more presenting methods to make the linked line more vivid and natural. But we cannot promise that we will hold this presenting function at the end.

## Expectation:
We expect to meet our goals until the deadline.

The playing pattern of this game can be very flexible. After implementing the basic pattern, we can still try to build other patterns, like “all letters are aligned to the left after each elimination”.
