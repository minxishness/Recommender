/****** Script for SelectTopNRows command from SSMS  ******/
SELECT  distinct([Genre])
, de.DisplayName
,de.entityid
  FROM [ArtistPortal3].[dbo].[DimResource] DP
  inner join dimentity de on dp.primaryentityId=de.entityId
  inner join userfavourite_group uf on dp.primaryentityid=uf.ArtistId
  where genre is not null
  order by displayname
