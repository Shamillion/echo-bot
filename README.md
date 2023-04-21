# echo-bot
------

This is a bot for Telegram and Vkontakte, which returns the received message to the sender with a configurable number of repetitions.

The project is a task from Metalamp's internship (link: https://rizzoma.com/topic/c27faf1cfa188c1120f59af4c35e6099/0_b_9n8n_8jl2p/). 

## Project deployment

You should have installed the Stack.

To deploy the project, you need to perform the following steps:
1. Clone this repository.
2. Open terminal and go to the root folder of the project.
3. Compile the project with the 
   ```haskell
   stack build
   ```
   command.
4. Open the **config.json** file in the root folder of the project and replace the parameters for one or both connections with your parameters.
    - "messenger" : "TG"                         - the messenger that you are going to use at the moment (TG or VK),
    - "hostTG"    : "api.telegram.org"           - telegram host issued when creating a bot in telegram,
    - "hostVK"    : "api.vk.com"                 - vkontakte host,
    - "tokenTG"   : "23654???????:??????????"    - telegram token issued when creating a bot in telegram,
    - "tokenVK"   : "23654???????:??????????"    - vkontakte group token,
    - "groupIdVK" : 000000000                    - id of the vkontakte group.
   and save the file.
   You also may change other parameters. The values of the rest parameters will be given below.
   
   NOTE! For the bot to work, it is necessary that the values of ALL parameters are filled in! 
   If you use only one connection, do not delete the parameter values of the other connection.
5. Run the bot with the 
   ```haskell
   stack run
   ```
   command from the root folder of the project.
   After running, the bot will output a line in the terminal: "Started Telegram echobot." or "Started VK echobot."
   
That is all. Your bot is ready to work!

You can stop the bot by pressing **Ctrl + C** in the terminal

## Starting and stopping the bot

To start the bot, open the terminal, go to the root of the project and run the following command 
```haskell
stack run
```
To stop the bot operation, press **Ctrl + C** in the terminal where the bot was started.

## Settings in config.json

. . . . . . . 
