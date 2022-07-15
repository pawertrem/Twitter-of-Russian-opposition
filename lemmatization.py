import pandas as pd
df = pd.read_csv (r'C:\Users\User\Desktop\after.csv', sep=',', encoding='utf-8')

print(df.iloc[:, 2])

import re

from pymorphy2 import MorphAnalyzer
from nltk.corpus import stopwords

patterns = "[A-Za-z0-9!#$%&'()*+,./:;<=>?@[\]^_`{|}~—\"\-]+"
stopwords_ru = stopwords.words("russian")
morph = MorphAnalyzer()


def lemmatize(doc):
    doc = re.sub(patterns, ' ', str(doc))
    tokens = []
    for token in doc.split():
        if token and token not in stopwords_ru:
            token = token.strip()
            token = morph.normal_forms(token)[0]

            tokens.append(token)
    if len(tokens) > 2:
        return tokens
    return None

data = df.iloc[:, 2].apply(lemmatize)

data.to_csv(r'C:\Users\User\Desktop\after.csv', sep=',', encoding='utf-8')
