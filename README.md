# Tomodoro BOT
[![Uptime Robot status](https://img.shields.io/uptimerobot/status/m782472842-da84bea2eff2fb2b088a87ff.svg)](https://stats.uptimerobot.com/xlwJ1i8DW)
[![CircleCI](https://circleci.com/gh/chepiov/tomodoro-bot.svg?style=shield)](https://circleci.com/gh/chepiov/tomodoro-bot)
[![Codacy Badge](https://api.codacy.com/project/badge/Grade/14350b5814124e1db12cad1869c4cd95)](https://app.codacy.com/app/chepiov/tomodoro-bot?utm_source=github.com&utm_medium=referral&utm_content=chepiov/tomodoro-bot&utm_campaign=Badge_Grade_Dashboard)
[![Codacy Badge](https://api.codacy.com/project/badge/Coverage/a6c863b3a4fa4f1291a3e9d3db8df246)](https://www.codacy.com/app/chepiov/tomodoro-bot?utm_source=github.com&utm_medium=referral&utm_content=chepiov/tomodoro-bot&utm_campaign=Badge_Coverage)
[![Telegram](https://img.shields.io/badge/telegram-join-green.svg)](https://telegram.me/tomodoroBot)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://raw.githubusercontent.com/chepiov/tomodoro-bot/master/LICENSE)
[![Release](https://img.shields.io/github/release/chepiov/tomodoro-bot.svg)](https://github.com/chepiov/tomodoro-bot/releases)

[![Deploy](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy)


**Telegram bot for the [Pomodoro Technique](https://en.wikipedia.org/wiki/Pomodoro_Technique).**

[Click here](https://telegram.me/tomodoroBot) to join. 

### Available commands:
* `/help`      - show help information
* `/state`     - show current state
* `/continue`  - start tomodoro or start a break 
* `/pause`     - pause tomodoro or pause a break
* `/skip`      - skip current tomodoro or current break
* `/reset`     - reset the whole tomodoro cycle
* `/settings`  - change settings
* `/stats`     - show stats


### What it is ###
* Just study project
* An effort to apply FP techniques (`cats`/`cats-effect`) in the `Akka` ecosystem

### Stack ###
* `Cats` and `Cats Effect` for FP awesomeness
* `Akka Http` as the web server
* `Akka Persistence` as the CQRS implementation
* `Doobie` for database access
* `spray-json` for json serialization
* `ScalaCheck` for property based testing
* `Tagless Final` for the core domain


