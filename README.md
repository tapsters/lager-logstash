# lager-logstash
[Lager][lager] backend for sending logs to [Logstash][logstash] with udp support

## Configuration

Add `lager_logstash` to your `rebar.config` deps.

And configure `lager` app with something like this:

``` erlang
[
 {lager,
  [
   {handlers,
    [
     {lager_logstash,
      [
       {level, info},
       {host, "localhost"},
       {port, 5000}]}
      ]}
    ]}
  ]}
].
```

Sample logstash config:

```
input {
  udp {
    host => "localhost"
    port => 5000
    type => "erlang"
  }
}

output {
  elasticsearch {
    hosts => ["localhost:9200"]
    index => "logstash"
  }
}
```

