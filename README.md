# Cowboy access log
[![wercker status](https://app.wercker.com/status/19c24f5d1bbda8df6e14b642af6d20d6/s/master "wercker status")](https://app.wercker.com/project/byKey/19c24f5d1bbda8df6e14b642af6d20d6)
## Description
This app implements apache-styled access logging for cowboy >= 2.0.  
It works as a `stream handler` and logs every request received by server via Logger.  
Following information is being logged:
* `status` - HTTP status code
* `remote_addr` - IP address of request origin if it was forwarded
* `peer_addr` - IP address of request origin
* `request_method` - Request method
* `request_path` -  Request path
* `request_length` - Request length
* `response_length` - Responce length
* `request_time` - Time taken by requset proccessing in µs
* `http_x-request-id` - Unique request id  

`cowboy_access_log` also supports custom meta, that can be added by calling `set_extra_info_fun/2` on your Cowboy options.  
`extra_info_fun` must take `Req` as it's only argument and return `#{atom() => term()}`.
## Usage
To use this stream handler just put it in stream handler chain when configuring cowboy server.
All log events, created by the handler are being taged with `cowboy_access_log` domain, so they can be easily filtered with built-in functions.
Check examples for better understanding.
## Examples
### Add handler
```
cowboy:start_clear(http, [{port, 8080}], #{
    stream_handlers => [cowboy_access_log, cowboy_stream_h],
    env => #{dispatch => Dispatch}
}).
```
### Filter only cowboy_access_log events
```
add_handler_filter(
    HandlerId,
    FilterId,
    {fun logger_filters:domain/2, {stop, not_equal, [cowboy_access_log]}}
).
```
### Add function to tag each log event with random Id
```
CowboyOpts0 = #{
    stream_handlers => [cowboy_access_log, cowboy_stream_h],
    env => #{dispatch => Dispatch}
},
CowboyOpts = cowboy_access_log_h:set_extra_info_fun(fun(_Req) -> #{my_random_id => base64:encode(crypto:strong_rand_bytes(20))} end, CowboyOpts0),
cowboy:start_clear(http, [{port, 8080}], CowboyOpts).
```
