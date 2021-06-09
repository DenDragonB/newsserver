# newsserver

This is a training project.
News web-server.
This product allows you to record and retrieve information from the database.
To use it you must install Postgresql and create a new database

CONFIGURATION
Please fill in the config.toml file

Section [server] contains the server settings
PORT - the port number to which requests from the network will be sent

Section [database] contains the settings for postgresql and your database
host - host name where your database is work
port - port number of database
user - user name to access to the database
pass - password to access to the database
name - name of database in postgresql

Section [logger] contains logging settings
logPath - log file path
logMinLevel - inimum level of logging data.
    can be : Debug | Info | Warning | Error
logTo - Where to output log
    can be : LogToFile | LogToConsole

API description
Only registered users can receive responses from the server.
Some of the requests are available only to administrators
When registering, the user receives a token
When requesting data, you must specify the token as one of the request parameters

To get data from the server, you must send a request like:

http://<server_web_addres>:<port>/<command>?<arg1>=<val1>&<arg2>=<val2>...

for example

http://localhost:8080/tag.get?token=abc123&id=5

in response, you will receive a message in JSON format

{"result":"Ok","object":[{"name":"Computer","id":5}]}


Posts API.

Command: 
posts.get - Outputs all records that match the search parameters

for example: http://localhost:8080/posts.get?token=ab12&author=Pall

Available parameters:
token - key token for user identification
author - optional. search for posts by the specified author.
            supports search by part of the name
header - optional. find posts by header name
            supports search by part of the name
content - optional. search for a string that can be found in text content 
            supports search by part of string
created_at - optional. date when post is created
created_at__lt - optional. news created earlier than the specified date
created_at__gt - optional. news created after the specified date
/posts.get?created_at=2018-05-21
/posts.get?created_at__lt=2018-05-21
/posts.get?created_at__gt=2018-05-21

tag - optional. tag id number
tags__all - optional. tag numbers that ALL should be present in the post
tags__in - optional. find posts that have at least one tag from the list
/posts.get?tag=123
/posts.get?tags__in=[123,124,125]
/posts.get?tags__all=[123,124,125]

sort_by - optional. sorting posts. possible values:
    author - sort by author name
    date - sort ba date
    category - sort by category name
    photos - sort by number of photos

You can ask for a paginated result. 
limit - optional. number of posts per page
page - optional. page number in the output

Drafts API

Command: 

draft.add - Add new draft to database
for example: 
    http://localhost:8080/draft.add?token=author
    &header=Phone of 2021&category_id=5&tags_id=[1,2]
    &content=there are same info about cell phones
    &main_photo=photo.jpg&photos=['photo1.png','photo2.jpg']

Available parameters:
token - key token for user identification. Only authors can add drafts.
header - header of post
category_id - id number of the category in which the news is posted
tags_id - optional. Id numbers of tags in square brackets separated by commas: [5] [1,4]
content - optional. The text of content
main_photo - optional. Link to the main photo of the news
photos - optional. Links to additional photos in square brackets separated by commas.
    Each link must be enclosed in apostrophes: ['photo1.png','photo2.jpg']




        "/database.migrate" -> do 
            liftIO $ DataBase.migrateDB (DataBase.dbConn env)
            return $ A.String "DataBase updated"
        -- Users API    
        "/user.add"    -> DataBase.userAdd $ (parseQuery . rawQueryString) request
        "/user.get"    -> DataBase.userGet $ (parseQuery . rawQueryString) request
        "/user.delete" -> DataBase.userDel $ (parseQuery . rawQueryString) request
        "/user.change_pass" -> DataBase.userNewPass $ (parseQuery . rawQueryString) request
        -- Author API
        "/author.add"    -> DataBase.authorAdd $ (parseQuery . rawQueryString) request
        "/author.edit"   -> DataBase.authorEdit $ (parseQuery . rawQueryString) request
        "/author.get"    -> DataBase.authorGet $ (parseQuery . rawQueryString) request
        "/author.delete" -> DataBase.authorDelete $ (parseQuery . rawQueryString) request
        -- Category API
        "/category.add"    -> DataBase.categoryAdd $ (parseQuery . rawQueryString) request
        "/category.edit"   -> DataBase.categoryEdit $ (parseQuery . rawQueryString) request
        "/category.get"    -> DataBase.categoryGet $ (parseQuery . rawQueryString) request
        "/category.delete" -> DataBase.categoryDelete $ (parseQuery . rawQueryString) request
        -- Tags API
        "/tag.add"    -> DataBase.tagAdd $ (parseQuery . rawQueryString) request
        "/tag.edit"   -> DataBase.tagEdit $ (parseQuery . rawQueryString) request
        "/tag.get"    -> DataBase.tagGet $ (parseQuery . rawQueryString) request
        "/tag.delete" -> DataBase.tagDelete $ (parseQuery . rawQueryString) request
        -- Drafts API
        "/draft.add"    -> DataBase.draftAdd $ (parseQuery . rawQueryString) request
        "/draft.edit"   -> DataBase.draftEdit $ (parseQuery . rawQueryString) request
        "/draft.get"    -> DataBase.draftGet $ (parseQuery . rawQueryString) request
        "/draft.delete" -> DataBase.draftDelete $ (parseQuery . rawQueryString) request
        "/draft.publish" -> DataBase.draftPublish $ (parseQuery . rawQueryString) request

