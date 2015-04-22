select uf.UserFavouriteId
,uf.UserId
,uf.ArtistId
,convert(varchar(10),uf.AddedDate, 120)
,uf.usergroupid
,uf.isVisible
,de.DisplayName
,ug.GroupName
from UserFavourite_GROUP uf
inner join dimentity de on uf.artistid=de.entityid
inner join UserGroup ug on uf.UserGroupID=ug.UserGroupID
