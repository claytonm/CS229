import requests

url = ('https://newsapi.org/v2/top-headlines?'
       'country=us&'
       'apiKey=bc5d9fce8f2f4679a555f95517fe0442')
response = requests.get(url)
print response.json()



