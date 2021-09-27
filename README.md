# newsserver

This is a training project.
News web-server.  
This product allows you to record and retrieve information from the database.
To use it you must install Postgresql and create a new database
Copy files to work directory.

## CONFIGURATION
Please fill in the `config.toml` file

Section **[server]** contains the server settings.

**PORT** - the port number to which requests from the network will be sent

Section **[database]** contains the settings for postgresql and your database

**host** - host name where your database is work  
**port** - port number of database  
**user** - user name to access to the database  
**pass** - password to access to the database  
**name** - name of database in postgresql  

Section **[logger]** contains logging settings

**logPath** - log file path  
**logMinLevel** - minimum level of logging data.  
    can be : *Debug* | *Info* | *Warning* | *Error*  
**logTo** - Where to output log  
    can be : *LogToFile* | *LogToConsole*

Before use start server, then send request to setup tables in database:

http://\<server_web_addres\>\:\<port\>/database.migrate

for example:
http://localhost:8080/database.migrate


## API description
Only registered users can receive responses from the server.  
Some of the requests are available only to administrators.  
When registering, the user receives a token.  
When requesting data, you must specify the token as one of the request parameters.

To get data from the server, you must send a request like:

http://\<server_web_addres\>\:\<port\>\/\<command\>\?\<arg1\>\=\<val1\>\&\<arg2\>\=\<val2\>...

for example

http://localhost:8080/tag.get?token=abc123&id=5

in response, you will receive a message in JSON format

\{"result":"Ok","object":\[\{\"name\"\:\"Computer\",\"id\"\:5\}\]\}


### Posts API.

Command: 

`posts.get` - Outputs all records that match the search parameters

for example: http://localhost:8080/posts.get?token=ab12&author=Pall

Available parameters:

**token** - key token for user identification  
**author** - optional. search for posts by the specified author.  
supports search by part of the name  
**header** - optional. find posts by header name  
supports search by part of the name  
**content** - optional. search for a string that can be found in text content  
supports search by part of string  
**created_at** - optional. date when post is created  
**created_at__lt** - optional. news created earlier than the specified date  
**created_at__gt** - optional. news created after the specified date  
    </posts.get?created_at=2018-05-21>  
    </posts.get?created_at__lt=2018-05-21>  
    </posts.get?created_at__gt=2018-05-21>  
**tag** - optional. tag id number  
**tags__all** - optional. tag numbers that ALL should be present in the post  
**tags__in** - optional. find posts that have at least one tag from the list  
    </posts.get?tag=123>  
    </posts.get?tags__in=[123,124,125]>  
    </posts.get?tags__all=[123,124,125]>  
**search** - optional. search for a string that can be found in text content, 
header, tag name, category name or author name. case-insensitive.  
**sort_by** - optional. sorting posts. possible values:  
- *author* - sort by author name  
- *date* - sort ba date  
- *category* - sort by category name  
- *photos* - sort by number of photos  

You can ask for a paginated result.  
**limit** - optional. number of posts per page  
**page** - optional. page number in the output

### Drafts API

Command: 

`draft.add` - Add new draft to database

**PLEASE Remember the id to access the draft in future**

for example:   
    http://localhost:8080/draft.add?token=author
    &header=Phone of 2021&category_id=5&tags_id=[1,2]
    &content=there are same info about cell phones
    &main_photo=http://localhost:8080/photos/photo.jpg
    &photos=['http://localhost:8080/photos/photo1.png','http://localhost:8080/photos/photo2.jpg']

Available parameters:
token - key token for user identification. Only authors can add drafts.
header - header of post
category_id - id number of the category in which the news is posted
tags_id - optional. Id numbers of tags in square brackets separated by commas: [5] [1,4]
content - optional. The text of content
main_photo - optional. Link to the main photo of the news
photos - optional. Links to additional photos in square brackets separated by commas.
    Each link must be enclosed in apostrophes: ['photo1.png','photo2.jpg']

Command: 

`draft.edit` - Edit an existing draft.

Available parameters:
token - key token for user identification. Only author can edit draft.
id - draft identification number

Other parameters are optional. See their description in the command draft.add
If the parameter is included in the request, its new value will be saved

Commands: 

`draft.get` - Returns the author's draft by its id
`draft.delete` - Delete an existing draft.
`draft.publish` - Publish a new post or update the data in the post from the draft

Available parameters:
token - key token for user identification. Only author can get or delete draft.
id - draft identification number


Users API

Command: 

user.add - register new user into database

for example:
http://localhost:8080/user.add?last_name=Ivanov&first_name=Ivan
    &name=Cheburashka&pass=GenaTheBest
    &avatar=http://localhost:8080/photos/uhi.jpg

Available parameters:
last_name - Last name of new user
first_name - First name of new user
name -username
avatar - Link to avatar for new user
pass - password of new user

Command: 

user.get - get list of users into database

for example:
http://localhost:8080/user.get?token=aa33&last_name=Bychkov&limit=2&page=2

Available parameters:
token - key token for user identification
last_name - optional. search users by last name user
first_name - optional. search users by first name user
name - optional. search users by username

Command: 

user.delete - delete user from database

for example:
http://localhost:8080/user.delene?token=aa33&id=3

Available parameters:
token - key token for user identification. Only administrators can delete users
id - id number of user

Command: 

user.change_pass - change pass for user and get new token

for example:
http://localhost:8080/user.change_pass?name=Cheburashka
    &pass=GenaTheBest&new_pass=CheburashkaBetter

Available parameters:
name -username
pass - password of user
new_pass - new password


Authors API

Command:

author.add - add user to authors

for example
http://localhost:8080/author.add?token=amin45&user_id=11&about=Thisisbestauthor

Available parameters:
token - key token for user identification. Only administrators can add authors
user_id - id number of the user to make the author 
about - description about author

Command:

author.edit - edit authors information

for example
http://localhost:8080/author.edit?token=amin45&id=2&user_id=5&about=Edit_Author

Available parameters:
token - key token for user identification. Only administrators can edit authors
user_id - id number of the user to make the author 
about - description about author

Command:

author.get - get list of authors into database

for example
http://localhost:8080/author.get?token=amin45&user_id=11

Available parameters:
token - key token for user identification. Only administrators can get authors
user_id - optional. id number of the user to make the author 
id - optional. id number of author

Command:

author.delete - delete author from database

for example
http://localhost:8080/author.delete?token=amin45&id=2

Available parameters:
token - key token for user identification. Only administrators can delete authors
id - id number of author to delete


Category API

Command:

category.add - add categoy to database

for example
http://localhost:8080/category.add?token=admin&name=Computers

Available parameters:
token - key token for user identification. Only administrators can add authors
name - name of category 
parent - id number of parent category

Command:

category.edit - edit an existing category

for example
http://localhost:8080/category.edit?token=admin&id=2&parent=1

Available parameters:
token - key token for user identification. Only administrators can edit authors
id - id number of category
name - optional. name of category 
parent - optional. id number of parent category

Command:

category.get - get list of authors into database

for example
http://localhost:8080/category.get?token=user&id=1

Available parameters:
token - key token for user identification. Only administrators can get authors
name - optional. name of category 
id - optional. id number of category
parent - optional. id number of parent category

Command:

category.delete - delete category from database

for example
http://localhost:8080/category.delete?token=admin&id=2

Available parameters:
token - key token for user identification. Only administrators can delete authors
id - id number of category to delete


Tags API

Command:

tag.add - add tag to database

for example
http://localhost:8080/tag.add?token=admin&name=Computer

Available parameters:
token - key token for user identification. Only administrators can add tags
name - name of tag 

Command:

tag.edit - edit an existing tag

for example
http://localhost:8080/tag.edit?token=admin&id=5&name=Computer

Available parameters:
token - key token for user identification. Only administrators can edit tags
id - id number of tag
name - optional. name of tag 

Command:

tag.get - get list of tags into database

for example
http://localhost:8080/tag.get?token=admin

Available parameters:
token - key token for user identification
name - optional. name of tags 
id - optional. id number of tag


Command:

tag.delete - delete tag from database

for example
http://localhost:8080/tag.delete?token=admin&id=4

Available parameters:
token - key token for user identification. Only administrators can delete tags
id - id number of tag to delete

