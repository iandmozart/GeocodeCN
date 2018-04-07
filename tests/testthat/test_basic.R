library(GeocodeCN)
context("Basic")

skip_if_no_auth <- function() {
    if (identical(Sys.getenv("amap_key"), "")) {
        skip("No authentication available")
    }
}

test_that(
    "Simple geocode an address",
    {
        skip_if_no_auth()
        # Test wrapper
        expect_error(geocode("北京市朝阳区朝阳首府"))
        expect_equal(geocode("北京市朝阳区朝阳首府", key = Sys.getenv("amap_key"))$status, "1")
    }
)
