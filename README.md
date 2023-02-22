# StructStruck

Ever had a deeply nested JSON struct
```json
{
    "outer": {
        "middle": {
            "inner": {
                "foo": "bar"
            }
        }
    }
}
```
and wanted to write the Rust structs to handle that data just in the same nested way?
```rust
struct Parent {
    outer: struct {
        middle: struct {
            inner: struct {
                foo: String,
            }
        }
    }
}
```
This proc macro crate allows exactly that.
Check the [docs](https://docs.rs/structstruck) on how exaclty.

For illustration, some more usecases:

* an enum where every variant has its own struct, named exactly the same as the variant.
```rust
structstruck::strike! {
    enum Token {
        Identifier(struct {
            name: String,
        }),
        Punctuation(struct {
            character: char,
        }),
    }
}
```

* my original use case: conveniently write kubernetes custom resources with `kube`.
```rust
structstruck::strike! {
    #[strikethrough[derive(Deserialize, Serialize, Clone, Debug, Validate, JsonSchema)]]
    #[strikethrough[serde(rename_all = "camelCase")]]
    #[derive(CustomResource)]
    #[kube(
        group = "kafka.strimzi.io",
        version = "v1beta2",
        kind = "Kafka",
        namespaced
    )]
    struct KafkaSpec {
        kafka: struct KafkaCluster {
            #[validate(length(min = 1))]
            version: String,
            #[validate(range(min = 1))]
            replicas: u32,
            listeners: Vec<struct KafkaListener {
                name: String,
                port: u16,
                r#type: String,
                tls: bool,
            }>,
            config: HashMap<String, JsonValue>,
            storage: struct {
                r#type: String,
                volumes: Vec<struct Volume {
                    id: Option<u64>,
                    r#type: String,
                    size: String,
                    delete_claim: bool,
                }>,
            },
        },
        zookeeper: struct {
            #[validate(range(min = 1))]
            replicas: u32,
            storage: Volume,
        },
        entity_operator: struct {
            topic_operator: Option<HashMap<String, JsonValue>>,
            user_operator: Option<HashMap<String, JsonValue>>,
        },
    }
}
```

