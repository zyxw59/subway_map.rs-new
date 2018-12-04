use std::borrow::Borrow;
use std::cmp::Eq;
use std::collections::HashMap;
use std::hash::Hash;

pub trait Table<Key: ?Sized, Value> {
    fn get(&self, key: &Key) -> Option<&Value>;
}

pub struct Scoped<'a, T: 'a, K, V> {
    local: HashMap<K, V>,
    global: &'a T,
}

impl<'a, T, K, V> Scoped<'a, T, K, V>
where
    K: Hash + Eq,
{
    pub fn new() -> Scoped<'static, (), K, V> {
        Scoped {
            local: HashMap::new(),
            global: &(),
        }
    }

    pub fn with_parent(parent: &'a T) -> Scoped<'a, T, K, V> {
        Scoped {
            local: HashMap::new(),
            global: parent,
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
        self.local.get(key).or_else(|| self.global.get(key))
    }
}

impl<K, V> Table<K, V> for () {
    fn get(&self, _key: &K) -> Option<&V> {
        None
    }
}
