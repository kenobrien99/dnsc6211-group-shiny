# -*- coding: utf-8 -*-


def getTwitterFeeds(searchStr):
    import twitter
    CONSUMER_KEY = ''
    CONSUMER_SECRET =''
    OAUTH_TOKEN = ''
    OAUTH_TOKEN_SECRET = ''


    auth = twitter.oauth.OAuth(OAUTH_TOKEN,
                               OAUTH_TOKEN_SECRET,
                               CONSUMER_KEY,
                               CONSUMER_SECRET)
    
    api = twitter.Twitter(auth=auth)
    #api = twitter.Api(CONSUMER_KEY,CONSUMER_SECRET,OAUTH_TOKEN,OAUTH_TOKEN_SECRET)
    #searchStr= 'chicago,IL'
    search = api.search.tweets(q=searchStr, count=5)    
    #search = api.GetSearch(term=searchStr, lang='en', result_type='recent', count=10, max_id='')
    resultSet=''
    l=[]
    #print(search)
    l.append('<html>')
    for t in search['statuses']:
        
        l.append((t['user']['name'] + ' (' + t['created_at'] + ')\n').encode('unicode-escape'))
        #Add the .encode to force encoding
        #l.append(t.text.encode('utf-8'))
        l.append(t['text'].encode('unicode-escape'))
        l.append(' \n')
        l.append('=======================================================================================')
    l.append('</html>\n\r\n')    
    resultSet=''.join(l)
    return resultSet

   
#myResults=getTwitterFeeds('washington,dc')
#print myResults
   