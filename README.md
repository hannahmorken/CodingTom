# CodingTom
#### How to build, run and play Coding Tom
1. Clone repository CodingTom from GitHub.
2. In a terminal, navigate to CodingTom.
3. Run the command "cabal run" to run the game (make sure you have Cabal installed on your computer) in a terminal.
4. You have now started the game, and are looking at the menu for Coding Tom.
5. Press the “start game” button with your mouse.
6. Press the “ok!” button to start the first level.
7. You will now reach the first level. At the right side of the window is where you will write you code. Press the ‘X’ button to remove the “Hello!” text in the code editor. Typing is as normal, but note there is no text cursor. Do not use backspace to delete characters in the text. Instead, use the left arrow button. 
8. Write “walk” 5 times with either space or new line between each command and press the “run code” button to make the character, Tom, walk and finish the level. 
9. Your are now at the introduction text to the next level where turning is introduced. Start this level, and write code as told in the text.
10. To close the game, press the “Q” button at the top right corner. N.B., closing the game can be slow when far into it.

#### Solutions to each level
(A whitespace is interpreted the same as a newline.)
- Level 1: walk walk walk walk walk
- Level 2: walk walk walk walk walk turnLeft walk walk
- Level 3: while notFinished (walk)
- Level 4: while notFinished (if blocked (turnRight) else (walk))
