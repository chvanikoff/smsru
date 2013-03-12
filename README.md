smsru
=====

sms.ru API implementation in Erlang
-

Russian:

Для использования нужны только логин и пароль от sms.ru - либо в конфиг-файле priv/smsru.config, либо переданные явно в `smsru:start(Config)` (можно вызвать `smsru:start()` и передать данные позже посредством `smsru:config(Config)`).

Config - proplist с ключами login и password. Например: `[{login, "79871234567"}, {password, "qweqweasdasd"}]`

В данный момент эта обертка вокруг API умеет выполнять только следующие методы (будет обновляться по мере добавления):

> [sms/send](http://sms.ru/?panel=api&subpanel=method&show=sms/send) (Отправка смс со всеми возможными параметрами)

```erlang
smsru:send("79871234567", "Wazzuppp"),
smsru:send("79871234567", "this one will be sent from myregisteredsender", [{from, "myregisteredsender"}, {test, "1"}]),
```
