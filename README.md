# DINO RUNNN!
We plan to develop an arcade game using brick library in Haskell named DINO RUNNN! DINO RUNNN! is an endless runner game  which is inpired by Chrome Dino developed by Google. In this game, our hero dino runs at full speed to avoid his predators. Players will be using keyboards to help the character avoid all obstacles and rush to the end. The dinosaur needs to jump over bushes and Cactaceae without getting hurt and it can reach some bonus food to get score bonus. It also needs to cope with other challenges on the road. The difficulty of this adventure will gradually increase along this journey. Watch out, before your fail the game by letting dino get hurt.
Once the game is finished(the player fails the game), the player will receive their scores and ranking.

## Goals
- [ ] Use the keyboard to control the dinosaur's vertical movement
- [ ] Automatically generate views, obstacles and bonus food along the road
- [ ] Gradually increase the game difficulty (speed, obstacle amount) over time
- [ ] Display scoreboard at the end

## Architecture

- Dino: Inspired by Samy's tutorial of the Snake. This component is designed to deal with all the events that occur in the game stage. Currently, the code file includes functions that deal with 3 major tasks: 
1. Check whether Dino dies or collides with the barrier. 
2. Determine the height and velocity of Dino. 
3. Generate bush/barriers during the game.
- UI: Built by Brick. Shows the game start/game over user interfaces, with a scoreboard indicating the player's score during the game.Contains the main logic of the game


## Challenges
1. Jumping Height 
Dino accidentally gets the ability to control jumping height. The group needs to deal with the surprise.
-> Sol1: Find out the code snippets that make this trouble and tackle this problem with a total prohibition of "jumping in the air"
-> Sol2: We can use it as a special feature of the game, making it more fun for players to use this trick coping the challenges they met.

2. Avatar Size
Dino's avatar size is way too large for the display. It either takes up 1/4 of the screen or cannot be properly displayed in a small window.
->Sol1: Increase the resolution of the gaming window and enlarge the barrier to make them compatible with Dino
->Sol2: Modify the array that defines Dino's shape to make a smaller version of Dino.

3. Time Conflicts 
Most team members need to prepare for the final exams and interviews next 2 weeks. There are still a few issues that need to be discussed and settled.
-> Sol1: Confirm a time slot for future discussion. Survey each team member for their schedule in the next week. Meanwhile use WeChat to share ideas.


## Progress
- Successfully create the game start page and jump from it to the game playing page.
- Implemented the gravity function, and fixed a bug that allows Dino to start jumping in the air.
- Added UIs for the Start and End of this game.
- Implemented collision detection and death check functions.
- Implemented character/bushes animation.
- Refactored the Dino avatar with a pixel-style sketch of high quality.


## Team Members
- Shiting Ding (s1ding@ucsd.edu)
- Zhiyuan Chai (zhchai@ucsd.edu)
- Le Yang (ley004@ucsd.edu)
- Xinyu Zhou (xiz143@ucsd.edu)

## Reference
- https://en.wikipedia.org/wiki/Dinosaur_Game
- https://github.com/jtdaugherty/brick/
