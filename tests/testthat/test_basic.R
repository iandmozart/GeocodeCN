library(GeocodeCN)
context("Basic")

test_that(
    "Simple geocode an address",
    {
        # Test wrapper
        expect_error(geocode("北京市朝阳区朝阳首府"))
        expect_equal(geocode("北京市朝阳区朝阳首府", key = Sys.getenv("amap_key"))$status, "1")
    }
)
