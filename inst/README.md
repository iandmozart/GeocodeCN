GeocodeCN
================

About
-----

`GeocodeCN` is a simple wrapper around common APIs for geocoding Chinese location. Currently supports [Amap](%22http://lbs.amap.com/%22) and [Baidu Map](%22http://lbsyun.baidu.com/%22).

Geocoding
---------

``` r
library(GeocodeCN)
# default using amap
geocode("北京市朝阳区朝阳公园", key = "Your_Api_Key_Here")
```

These APIs require application key to work. It will be more convenient if you store your application keys in your environment as variables.

``` r
# amap_key = "amap_api_key_put_here"
# baidu_key = "baidu_api_key_put_here"
# ...
file.edit("~/.Renviron")

##--- Restart R session to take effect ---##
amap_ak = Sys.getenv("amap_key")
baidu_ak = Sys.getenv("baidu_key")
geocode("北京市海淀区双清路30号", key = amap_ak)
geocode("上海市长宁区武夷花园", api = "baidu", key = baidu_ak, output = "XML")
```

Geocode Conversion
------------------

Non-Baidu standard geocode can be converted to Baidu standard geocode with the following method.

``` r
geoconv("116.329315", "40.004472", key = "Baidu_Map_Api_Key")
```

Note
----

This package is in primitive stage of development. More features are coming soon and welcome to file issue for help.