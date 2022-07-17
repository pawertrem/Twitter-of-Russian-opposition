import tweepy
import configparser
import pandas as pd
import datetime
import pytz

api_key = api_key
api_key_secret = api_key_secret
access_token = access_token
access_token_secret = access_token_secret

# authentication
auth = tweepy.OAuthHandler(api_key, api_key_secret)
auth.set_access_token(access_token, access_token_secret)
api = tweepy.API(auth)

# LET'S GET TWEETS

username = "max_katz" #code is subsequenty executed for every Twitter account from a prepared list (~20 accounts)
tweets=tweepy.Cursor(api.user_timeline,
                     screen_name=username,
                     include_rts=False,
                     tweet_mode='extended').items()

# DataFrame
columns = ['Time', 'User', 'Tweet']
data = []
for tweet in tweets:
    data.append([tweet.created_at, tweet.user.screen_name, tweet.full_text])

df = pd.DataFrame(data, columns=columns)

df.to_csv('katz.csv') #after collection of tweets from all necesarry accounts, they are binded in a single dataframe ('rbind' in R) 
