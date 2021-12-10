# cse230_Happy_Link_Game
![](https://img.ams1.imgbed.xyz/2021/12/10/oM3Wq.png)
## Group Members
Yanjun Tong

Yiyang Qiu

Shanbin Ke

Er Zhuo

## Project Description
Happy link game is a terminal based small game with a straightforward goal to eliminate all the cells on the board. You need to find all the same letters that can be connected in pairs, each time you find a pair, they will automatically disappear, as long as all the letters are eliminated, you win. The so-called being able to connect means: whether horizontally or vertically, the line from one letter to another letter cannot exceed two bends, and the line cannot pass through the letters that have not been eliminated.

               1 2 3 4 5 6 7 8 9 10
               A B Y K L A M K L Z  1
               K A Y A O P N P L Z  2
               F O N L M R F B X X  3
 
## Architecture
#### print_the_game:
This API is the outer monad to print the current game status to the terminal. It will include the board with existing letters and eliminated ones replaced by an empty position. It will also print some interactive information, like the coordinate of the cell you choose, and one important hint, that is once you choose the wrong letter paired with your former chosen one, it will simply print "nothing" to the screen. The cells will always be filled with different colors according to their letter value to simplify the seach process. It will also provide some basic operation instructions to help for a quick start.

#### check_linkable:
This is one of the most significant logic in our project that will decide whether two remaining letters on the board can be linked or not, that is if there exists a line that can linked the chosen pair together with no more than 2 bends and does not passing throught any existing cell.

#### shuffle:
This API is aimed to shuffle the remaining letters on the board when no two letters on the board can be linked further. Most likely, a player will repeat the elimination process normally until the board is empty. But sometimes with a bad initial states and a even worse order of pair choice, a player may encounter the situation that no pairs can will eliminated further, that the game is dead. To make life easier, we introduce the shuffle function the shuffle the entire board. It will re-locate the surviving cells on the board to another random position within the board. And most often, it will introduce new pairs valid to remove.

#### change_board:
This APIs are the inner functions that will change our inner data structures. It will bridge our inner decision making logic and our UI functions.

## Challege:
+ The implementation of the game logic is a little bit complicated. We searched some reference materials to design a relatively efficient algorithm and then implemented the functions in Haskell step by step.

## Expectation:
We expect to meet our goals until the deadline.

The playing pattern of this game can be very flexible. After implementing the basic pattern, we can still try to build other patterns, like “all letters are aligned to the left after each elimination”.
