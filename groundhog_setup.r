groundhog.day               <-  "2025-06-15"
foo                         <-  "groundhog"%in%installed.packages()
if(!foo)                    {   install.packages("groundhog") }
library                     (   groundhog)
foo                         <-  packageVersion("groundhog")=="3.2.3"
if(!foo)                    {   meta.groundhog(groundhog.day) }
rm                          (   foo)

groundhog.library           (   languageserver  ,groundhog.day)
groundhog.library           (   httpgd          ,groundhog.day)

groundhog.library           (   here            ,groundhog.day,force.source=TRUE)
groundhog.library           (   tidyverse       ,groundhog.day,force.source=TRUE)
groundhog.library           (   data.table      ,groundhog.day,force.source=TRUE)
groundhog.library           (   leaflet         ,groundhog.day,force.source=TRUE)
groundhog.library           (   sf              ,groundhog.day,force.source=TRUE)
groundhog.library           (   sp              ,groundhog.day,force.source=TRUE)
groundhog.library           (   ggrepel         ,groundhog.day,force.source=TRUE)
groundhog.library           (   plotly          ,groundhog.day,force.source=TRUE)
groundhog.library           (   leafgl          ,groundhog.day,force.source=TRUE)
groundhog.library           (   htmlwidgets     ,groundhog.day,force.source=TRUE)
groundhog.library           (   ragg            ,groundhog.day,force.source=TRUE)

