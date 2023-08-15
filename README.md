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
4. Rename the **_config.json** file in the root folder of the project in **config.json**.  
5. Open the **config.json** file and replace the parameters for one or both connections with your parameters.
    - "messenger" : "TG"                         - the messenger that you are going to use at the moment (TG or VK),
    - "hostTG"    : "api.telegram.org"           - telegram host issued when creating a bot in telegram,
    - "hostVK"    : "api.vk.com"                 - vkontakte host,
    - "tokenTG"   : "23654???????:??????????"    - telegram token issued when creating a bot in telegram,
    - "tokenVK"   : "23654???????:??????????"    - vkontakte group token,
    - "groupIdVK" : 123456789                    - id of the vkontakte group.
   and save the file.
   You also may change other parameters. The values of the rest parameters will be given below.
   
   NOTE! For the bot to work, it is necessary that the values of ALL parameters are filled in! 
   If you use only one connection, do not delete the parameter values of the other connection.
6. Run the bot with the 
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

## Commands for the bot

The bot understands the following commands:
- **/help**       - informational message,
- **/repeat**     - calling the keyboard to change the number of repetitions. 

## Settings in config.json

```haskell
{
  "messenger" : "TG",                  -- the messenger that you are going to use at the moment,
  "comment_messenger" : "TG || VK",    -- possible values of the "messenger" field (Telegram or Vkontakte),         
  "hostTG"    : "api.telegram.org",    -- Telegram host issued when creating a bot in Telegram,
  "hostVK"    : "api.vk.com",          -- Vkontakte host,
  "tokenTG"   : "23654???????:??????????"    -- Telegram token issued when creating a bot in Telegram,
  "tokenVK"   : "23654???????:??????????"    -- Vkontakte group token,
  "groupIdVK" : 123456789              -- id of the Vkontakte group,
  "apiVKVersion" : "5.130",            -- the version of the api for Vkontakte (the bot has been tested with version 5.130),
  "helpMess"  : [ "This is an echobot.\n",
                  "It repeats your messages a certain number of times.\n",
                  "The number of repetitions can be changed by entering", 
                  " command /repeat."
                ],                      -- the message displayed when the bot receives the "/help" command,
  "repeatMess"     : "How many reps do you want to set?",    -- the message displayed when the bot receives the "/repeat" command,
  "defaultRepeats" : 2,                 -- default number of repetitions,
  "priorityLevel"  : "DEBUG",           -- logging level.
  "comment_priorityLevel" : "DEBUG || INFO || WARNING || ERROR",    -- possible logging levels.
  "logOutput"      : "cons",            -- the place where the logs will be output.
  "comment_logOutput" : "file || cons"  -- possible log output locations (file or console).
}
```  

## Basic project structure

```haskell
echo-bot                       -- the root folder of the project.
   ├── app
   │   └── Main.hs
   ├── config.json             -- bot settings file.
   ├── log.log                 -- file for bot logs. 
   ├── src
   │   ├── Config.hs           -- functions for getting settings from a file.
   │   ├── Connect.hs          -- function for connecting to the server.
   │   ├── Data.hs             -- data types for the server response.
   │   ├── Environment.hs      -- data types and function for environment.
   │   ├── Lib.hs              -- the pure logic for the bot work.
   │   ├── Logger              -- folder with modules for the logger.
   │   │   ├── Data.hs
   │   │   └── Functions.hs
   │   ├── RequestBuilding.hs  -- functions for creating a request to the server.
   │   ├── Telegram            -- folder with modules for the Telegram.
   │   │   ├── Engine.hs       -- main program cycle for Telegram.
   │   │   ├── Functions.hs    -- functions for work of Telegram's echobot.
   │   │   ├── Handler.hs      -- handle for work of Telegram's echobot.
   │   │   └── KeyboardData.hs -- creating keyboard for Telegram.
   │   └── Vk                  -- folder with modules for the Vkontakte.
   │       ├── Data.hs         -- data types for the response from the Vkontakte server.
   │       ├── Engine.hs       -- main program cycle for Vkontakte.
   │       ├── Functions.hs    -- functions for work of Vkontakte's echobot.  
   │       ├── Handler.hs      -- Handle for work of Vkontakte's echobot.
   │       └── KeyboardData.hs -- creating keyboard for Vkontakte.
   └── test
      └── Spec.hs              -- tests for bot.
```
