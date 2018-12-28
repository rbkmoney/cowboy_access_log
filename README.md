# access-lib
## Описание
Данное приложение служит в качестве [stream handler'a](https://ninenines.eu/docs/en/cowboy/2.0/guide/streams/), логгирующего запросы, обрабатываемые сервером.  
**Механизм stream handler'ов был добавлен в cowboy 2.0 и не поддерживается более старыми версиями**
## Использование
Для использования хэндлера просто добавьте его в список хэндлеров в настройках ковбоя ( аргумент `ProtoOpts`).  
Для работы хэндлера необходимо специфицировать Sink логгирования, поместите его в env с ключем `sink`.  
Обратите внимание, что если это первый используемый хэндлер, то нужно явно указать `cowboy_stream_h` как первый используемый хэндлер.

### Пример
```
Env = #{dispatch => Dispatch, sink => SinkName},
ranch:child_spec({?MODULE, Id}, Transport, TransportOpts, cowboy_clear, #{env => Env, stream_handlers => [cowboy_stream_h, cowboy_access_log_h]}).
```
