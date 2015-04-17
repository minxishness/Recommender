/****** Script for SelectTopNRows command from SSMS  ******/
SELECT [UserId]
      ,[ArtistId]
      ,[AddedDate]
	  ,de.DisplayName
  FROM [ArtistPortal3].[dbo].[UserRecentlyViewed] urw
  inner join dimentity de on urw.artistid=de.EntityID
  where artistid is not null