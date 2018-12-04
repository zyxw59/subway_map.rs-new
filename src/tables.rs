use std::borrow::Borrow;
use std::cmp::Eq;
use std::collections::HashMap;
use std::hash::Hash;

pub trait Table<Key: ?Sized, Value> {
    fn get(&self, key: &Key) -> Option<&Value>;
}

pub struct Scoped<'a, T: 'a, K, V> {
    local: HashMap<K, V>,
    global: Option<&'a T>,
}

impl<'a, T, K, V> Scoped<'a, T, K, V>
where
    K: Hash + Eq,
{
    pub fn new() -> Scoped<'static, T, K, V> {
        Scoped {
            local: HashMap::new(),
            global: None,
        }
    }

    pub fn with_parent(parent: &'a T) -> Scoped<'a, T, K, V> {
        Scoped {
            local: HashMap::new(),
            global: Some(parent),
        }
    }

    pub fn insert(&mut self, key: K, value: V) {
        self.local.insert(key, value);
    }
}

impl<'a, T, K, Q, V> Table<Q, V> for Scoped<'a, T, K, V>
where
    T: Table<Q, V>,
    K: Borrow<Q> + Hash + Eq,
    Q: ?Sized + Hash + Eq,
{
    fn get(&self, key: &Q) -> Option<&V> {
        self.local
            .get(key)
            .or_else(|| self.global.and_then(|g| g.get(key)))
    }
}
