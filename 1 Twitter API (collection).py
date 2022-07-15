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

startDate = datetime.datetime(2021, 1, 1, 00, 00, 00)
endDate = datetime.datetime(2021, 12, 31, 00, 00, 00)
username = "max_katz" #code is subsequenty executed for every Twitter account from a prepared list (~20 accounts)

tweets=tweepy.Cursor(api.user_timeline,
                     screen_name=username,
                     include_rts=False,
                     tweet_mode='extended').items()

# create dataframe
columns = ['Time', 'User', 'Tweet']
data = []
for tweet in tweets:
    data.append([tweet.created_at, tweet.user.screen_name, tweet.full_text])

df = pd.DataFrame(data, columns=columns)

df.to_csv('katz.csv')
