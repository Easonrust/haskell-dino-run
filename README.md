# DINO RUNNN!
We plan to develop an arcade game using brick library in Haskell named DINO RUNNN! DINO RUNNN! is an endless runner game  which is inpired by Chrome Dino developed by Google. In this game, our hero dino runs at full speed to avoid his predators. Players will be using keyboards to help the character avoid all obstacles and rush to the end. The dinosaur needs to jump over bushes and Cactaceae without getting hurt and it can reach some bonus food to get score bonus. It also needs to cope with other challenges on the road. The difficulty of this adventure will gradually increase along this journey. Watch out, before your fail the game by letting dino get hurt.
Once the game is finished(the player fails the game), the player will receive their scores and ranking.

## Goals
- [ ] Use the keyboard to control the dinosaur's vertical movement
- [ ] Automatically generate views, obstacles and bonus food along the road
- [ ] Gradually increase the game difficulty (speed, obstacle amount) over time
- [ ] Display scoreboard at the end

## Architecture

- UI: Built by Brick. Shows the game start page, score board and game over page. Also contains keyboard input and tick event handler.
- Dino: Built by Brick. Contains the main logic of the game

## Challenges

- Team Collaboration: It is difficult to distribute the tasks reasonably. Also most of team members have different deadlines from other course, so we use WeChat to share ideas.

## Progress

- Successfully creates the game start page and jump from it to game playing page.
- Successfully sets the basic game logic including dino moving, bushes moving.
- Successfully implements the collision detection for the dino and bushes.
- Successfully Implements character animation.

## Team Members
- Shiting Ding (s1ding@ucsd.edu)
- Zhiyuan Chai (zhchai@ucsd.edu)
- Le Yang (ley004@ucsd.edu)
- Xinyu Zhou (xiz143@ucsd.edu)

## Reference
- https://en.wikipedia.org/wiki/Dinosaur_Game
- https://github.com/jtdaugherty/brick/
