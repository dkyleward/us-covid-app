Full documentation on how to deploy the dashr app:
https://dashr.plotly.com/deployment

Setup:
After adding the yml/docker/etc files:
Download/install Heroku CLI and then:
    $ heroku create --stack container us-covid-app # this tells heroku to use docker
    $ git add . # add all files to git
    $ git commit -m 'Initial app boilerplate'
    $ git push heroku master # deploy code to Heroku
    $ heroku ps:scale web=1  # run the app with one Heroku 'dyno'

Updating the add:
    $ git status # view the changes
    $ git add .  # add all the changes
    $ git commit -m 'a description of the changes'
    $ git push heroku master